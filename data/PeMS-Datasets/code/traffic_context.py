#!/usr/bin/env python3
"""Incident, weather, and congestion helpers for the April 2026 LA map."""

from __future__ import annotations

import csv
import gzip
import math
from pathlib import Path
from typing import Iterable, Sequence

import numpy as np
import pandas as pd


INCIDENT_COLUMNS = [
    "incident_id",
    "cc_code",
    "incident_number",
    "timestamp",
    "description",
    "location",
    "area",
    "zoom_map",
    "tb_xy",
    "latitude",
    "longitude",
    "district",
    "county_fips",
    "city_fips",
    "freeway",
    "direction",
    "state_postmile",
    "abs_pm",
    "severity",
    "duration_min",
]

WEATHER_COLUMNS = [
    "STATION",
    "Station_name",
    "DATE",
    "LATITUDE",
    "LONGITUDE",
    "temperature",
    "wind_speed",
    "visibility",
    "precipitation",
    "precipitation_5_minute",
]


def _float_or_none(value: object) -> float | None:
    try:
        number = float(value)
    except (TypeError, ValueError):
        return None
    return number if math.isfinite(number) else None


def _normalize_freeway(value: object) -> str:
    text = str(value or "").strip().upper()
    text = text.removeprefix("INTERSTATE ").removeprefix("STATE ROUTE ")
    text = text.removeprefix("US ").removeprefix("SR ").removeprefix("I-").removeprefix("I")
    try:
        return str(int(float(text)))
    except ValueError:
        return text


def _incident_category(description: str) -> str:
    text = description.lower()
    if "collision" in text or "accident" in text:
        return "Collision"
    if "stalled" in text or "disabled" in text:
        return "Disabled vehicle"
    if "hazard" in text or "debris" in text or "animal" in text:
        return "Traffic hazard"
    if "construction" in text or "closure" in text:
        return "Construction / closure"
    if "fire" in text:
        return "Fire"
    return "Other"


def parse_incident_row(row: Sequence[object]) -> dict[str, object]:
    padded = list(row[: len(INCIDENT_COLUMNS)])
    padded.extend([""] * (len(INCIDENT_COLUMNS) - len(padded)))
    raw = dict(zip(INCIDENT_COLUMNS, padded))

    timestamp = pd.to_datetime(raw["timestamp"], format="%m/%d/%Y %H:%M:%S", errors="coerce")
    duration = _float_or_none(raw["duration_min"])
    active_duration = duration if duration is not None and duration > 0 else 30.0
    description = str(raw["description"] or "").strip()

    return {
        "incident_id": str(raw["incident_id"] or "").strip(),
        "timestamp": timestamp,
        "end_time": timestamp + pd.Timedelta(minutes=active_duration) if pd.notna(timestamp) else pd.NaT,
        "description": description,
        "category": _incident_category(description),
        "location": str(raw["location"] or "").strip(),
        "area": str(raw["area"] or "").strip(),
        "latitude": _float_or_none(raw["latitude"]),
        "longitude": _float_or_none(raw["longitude"]),
        "district": str(raw["district"] or "").strip().zfill(2),
        "county_fips": str(raw["county_fips"] or "").strip(),
        "city_fips": str(raw["city_fips"] or "").strip(),
        "freeway": _normalize_freeway(raw["freeway"]),
        "direction": str(raw["direction"] or "").strip().upper(),
        "state_postmile": str(raw["state_postmile"] or "").strip(),
        "abs_pm": _float_or_none(raw["abs_pm"]),
        "severity": _float_or_none(raw["severity"]),
        "duration_min": duration,
    }


def read_incidents(
    root: Path,
    districts: Iterable[str] = ("07", "08", "12"),
    start: pd.Timestamp = pd.Timestamp("2026-04-01 00:00:00"),
    end: pd.Timestamp = pd.Timestamp("2026-04-30 23:59:59"),
) -> pd.DataFrame:
    allowed = {str(value).zfill(2) for value in districts}
    records: list[dict[str, object]] = []
    pattern = "all_text_chp_incident_day_*.txt.gz"
    for path in sorted(root.glob(f"all_text_chp_incidents_day_*/{pattern}")):
        with gzip.open(path, "rt", encoding="utf-8", errors="replace", newline="") as handle:
            for row in csv.reader(handle):
                incident = parse_incident_row(row)
                timestamp = incident["timestamp"]
                if incident["district"] not in allowed or pd.isna(timestamp):
                    continue
                if timestamp < start or timestamp > end:
                    continue
                incident["source_file"] = path.name
                incident["source_incomplete"] = timestamp.date() == pd.Timestamp("2026-04-03").date()
                records.append(incident)

    if not records:
        raise FileNotFoundError(f"No CHP incident rows found below {root}")

    frame = pd.DataFrame(records)
    frame = frame.sort_values(["timestamp", "incident_id"]).drop_duplicates("incident_id", keep="last")
    return frame.reset_index(drop=True)


def _distance_m(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    radius = 6_371_000.0
    phi1 = math.radians(lat1)
    phi2 = math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlambda = math.radians(lon2 - lon1)
    a = math.sin(dphi / 2) ** 2 + math.cos(phi1) * math.cos(phi2) * math.sin(dlambda / 2) ** 2
    return radius * 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))


def _postmile_gap(postmile: float, start: float, end: float) -> float:
    low, high = sorted((start, end))
    if low <= postmile <= high:
        return 0.0
    return min(abs(postmile - low), abs(postmile - high))


def match_incident_to_link(
    incident: dict[str, object] | pd.Series,
    links: Sequence[dict[str, object]],
    max_postmile_gap: float = 5.0,
    max_coordinate_gap_m: float = 12_000.0,
) -> dict[str, object] | None:
    district = str(incident.get("district", "")).zfill(2)
    freeway = _normalize_freeway(incident.get("freeway", ""))
    direction = str(incident.get("direction", "")).upper()
    route_candidates = [
        link
        for link in links
        if str(link.get("district", "")).zfill(2) == district
        and _normalize_freeway(link.get("freeway", "")) == freeway
        and (not direction or str(link.get("direction", "")).upper() == direction)
    ]
    if not route_candidates:
        return None

    postmile = _float_or_none(incident.get("abs_pm"))
    if postmile is not None:
        scored: list[tuple[float, dict[str, object]]] = []
        for link in route_candidates:
            start = _float_or_none(link.get("from_abs_pm"))
            end = _float_or_none(link.get("to_abs_pm"))
            if start is None or end is None:
                continue
            scored.append((_postmile_gap(postmile, start, end), link))
        if scored:
            gap, link = min(scored, key=lambda item: (item[0], int(item[1]["id"])))
            if gap <= max_postmile_gap:
                return {
                    "link_id": int(link["id"]),
                    "match_method": "route_direction_postmile",
                    "postmile_gap": round(float(gap), 3),
                    "coordinate_gap_m": None,
                }

    latitude = _float_or_none(incident.get("latitude"))
    longitude = _float_or_none(incident.get("longitude"))
    if latitude is None or longitude is None:
        return None

    coordinate_scores: list[tuple[float, dict[str, object]]] = []
    for link in route_candidates:
        coords = link.get("coords") or []
        if not coords:
            continue
        distances = [_distance_m(latitude, longitude, float(lat), float(lon)) for lat, lon in coords]
        coordinate_scores.append((min(distances), link))
    if not coordinate_scores:
        return None

    gap_m, link = min(coordinate_scores, key=lambda item: (item[0], int(item[1]["id"])))
    if gap_m > max_coordinate_gap_m:
        return None
    return {
        "link_id": int(link["id"]),
        "match_method": "route_direction_coordinate",
        "postmile_gap": None,
        "coordinate_gap_m": round(float(gap_m), 1),
    }


def match_incidents_to_links(incidents: pd.DataFrame, links: Sequence[dict[str, object]]) -> pd.DataFrame:
    groups: dict[tuple[str, str, str], list[dict[str, object]]] = {}
    for link in links:
        key = (
            str(link["district"]).zfill(2),
            _normalize_freeway(link["freeway"]),
            str(link["direction"]).upper(),
        )
        groups.setdefault(key, []).append(link)

    rows: list[dict[str, object]] = []
    for incident in incidents.to_dict("records"):
        key = (
            str(incident["district"]).zfill(2),
            _normalize_freeway(incident["freeway"]),
            str(incident["direction"]).upper(),
        )
        match = match_incident_to_link(incident, groups.get(key, []))
        combined = dict(incident)
        combined.update(
            match
            or {
                "link_id": None,
                "match_method": "unmatched",
                "postmile_gap": None,
                "coordinate_gap_m": None,
            }
        )
        rows.append(combined)
    return pd.DataFrame(rows)


def congestion_score(baseline: object, current: object) -> float | None:
    normal = _float_or_none(baseline)
    speed = _float_or_none(current)
    if normal is None or speed is None or speed <= 0:
        return None
    return max(0.0, normal - speed)


def compute_weekslot_baseline(values: np.ndarray, times: Sequence[pd.Timestamp]) -> np.ndarray:
    matrix = np.asarray(values, dtype=np.float32)
    if matrix.ndim == 1:
        matrix = matrix[:, None]
    index = pd.DatetimeIndex(times)
    if len(index) != matrix.shape[0]:
        raise ValueError("The number of timestamps must match the speed rows.")

    weekslots = index.dayofweek * 288 + index.hour * 12 + index.minute // 5
    baseline = np.full((7 * 288, matrix.shape[1]), np.nan, dtype=np.float32)
    for slot in np.unique(weekslots):
        with np.errstate(all="ignore"):
            baseline[int(slot)] = np.nanmedian(matrix[weekslots == slot], axis=0)
    return baseline


def normalize_noaa_weather(raw: pd.DataFrame) -> pd.DataFrame:
    frame = raw.copy()
    for column in WEATHER_COLUMNS:
        if column not in frame:
            frame[column] = np.nan

    utc = pd.to_datetime(frame["DATE"], errors="coerce", utc=True)
    frame["time"] = utc.dt.tz_convert("America/Los_Angeles").dt.tz_localize(None)
    frame["station"] = frame["STATION"].astype(str)
    frame["station_name"] = frame["Station_name"].astype(str).str.strip()
    frame["latitude"] = pd.to_numeric(frame["LATITUDE"], errors="coerce")
    frame["longitude"] = pd.to_numeric(frame["LONGITUDE"], errors="coerce")
    frame["temperature_c"] = pd.to_numeric(frame["temperature"], errors="coerce")
    frame["wind_speed_ms"] = pd.to_numeric(frame["wind_speed"], errors="coerce")
    frame["visibility_km"] = pd.to_numeric(frame["visibility"], errors="coerce")
    five_min = pd.to_numeric(frame["precipitation_5_minute"], errors="coerce")
    hourly = pd.to_numeric(frame["precipitation"], errors="coerce")
    frame["precipitation_mm"] = five_min.combine_first(hourly)
    frame["precipitation_period"] = np.where(five_min.notna(), "5min", np.where(hourly.notna(), "reported", ""))

    columns = [
        "station",
        "station_name",
        "time",
        "latitude",
        "longitude",
        "temperature_c",
        "wind_speed_ms",
        "visibility_km",
        "precipitation_mm",
        "precipitation_period",
    ]
    frame = frame[columns].dropna(subset=["time", "latitude", "longitude"])
    return frame.sort_values(["station", "time"]).reset_index(drop=True)


def assign_links_to_weather_stations(
    links: Sequence[dict[str, object]], weather_stations: pd.DataFrame
) -> dict[int, str]:
    stations = weather_stations.drop_duplicates("station")
    station_rows = list(stations.itertuples(index=False))
    assignments: dict[int, str] = {}
    for link in links:
        coords = link.get("coords") or []
        if not coords or not station_rows:
            continue
        latitude = float(sum(float(coord[0]) for coord in coords) / len(coords))
        longitude = float(sum(float(coord[1]) for coord in coords) / len(coords))
        closest = min(
            station_rows,
            key=lambda station: _distance_m(
                latitude,
                longitude,
                float(station.latitude),
                float(station.longitude),
            ),
        )
        assignments[int(link["id"])] = str(closest.station)
    return assignments


def build_incident_buckets(
    incidents: pd.DataFrame, times: Sequence[pd.Timestamp]
) -> list[list[int]]:
    index = pd.DatetimeIndex(times)
    buckets: list[list[int]] = [[] for _ in range(len(index))]
    if index.empty:
        return buckets

    start = index[0]
    interval_ns = pd.Timedelta(minutes=5).value
    for incident_idx, incident in incidents.reset_index(drop=True).iterrows():
        if pd.isna(incident.get("link_id")):
            continue
        incident_start = pd.Timestamp(incident["timestamp"])
        incident_end = pd.Timestamp(incident["end_time"])
        first = int(math.ceil((incident_start.value - start.value) / interval_ns))
        stop = int(math.ceil((incident_end.value - start.value) / interval_ns))
        first = max(0, first)
        stop = min(len(index), max(first, stop))
        for time_idx in range(first, stop):
            buckets[time_idx].append(int(incident_idx))
    return buckets
