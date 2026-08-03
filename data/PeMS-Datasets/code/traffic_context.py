#!/usr/bin/env python3
"""Incident, weather, and congestion helpers for the April 2026 LA map."""

from __future__ import annotations

import csv
import gzip
import math
import re
import warnings
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


_CHP_DESCRIPTION_PREFIXES = (
    ("1125A", "Animal reported on or near the roadway."),
    ("1125", "Traffic hazard reported."),
    ("1182", "Traffic collision with no reported injuries."),
    ("1183", "Traffic collision; injury status unknown."),
    ("1179", "Traffic collision; ambulance en route."),
    ("20002", "Hit-and-run collision with no reported injuries."),
    ("20001", "Hit-and-run collision with reported injuries."),
    ("CFIRE", "Vehicle fire reported."),
    ("FIRE", "Fire reported."),
    ("CZP", "Construction assistance or work-zone activity."),
    ("BREAK", "Traffic break requested or in progress."),
    ("SIG ALERT", "SigAlert issued for a significant traffic disruption."),
    ("WW", "Wrong-way driver reported."),
    ("ANIMAL", "Live or dead animal reported on or near the roadway."),
    ("CLOSURE", "Road closure reported."),
    ("23114", "Object or debris reported falling from a vehicle."),
    ("MZP", "Caltrans maintenance assistance."),
    ("JUMPER", "Possible jumper reported near a roadway or bridge."),
    ("SPINOUT", "Vehicle spinout reported."),
    ("1166", "Defective traffic signal reported."),
    ("DOT", "Caltrans notification requested."),
    ("FLOOD", "Roadway flooding reported."),
    ("1184", "Traffic control requested."),
    ("1013", "Road or weather condition report."),
    ("WIND", "Wind advisory."),
    ("1181", "Traffic collision with minor injuries."),
    ("1144", "Fatality reported."),
    ("TADV", "Traffic advisory."),
    ("SPILL", "Spilled material reported on the roadway."),
    ("1180", "Traffic collision with major injuries."),
    ("SLIDE", "Mud, dirt, or rock slide reported."),
    ("ESCORT", "Traffic escort requested for road conditions."),
    ("HAZMAT", "Hazardous materials incident."),
    ("MAYDAY", "Aircraft emergency reported."),
    ("FOG", "Foggy roadway conditions."),
    ("CHAINS", "Chain controls reported."),
    ("AMBER", "Missing or abducted child alert."),
    ("SILVER", "Missing elderly person alert."),
    ("FEATHER", "Missing Indigenous person alert."),
    ("SNOFL", "Joint weather operations report."),
)


_CHP_NOTE_REPLACEMENTS = (
    (r"\b(\d+)\s+VEHS?\s+TC\b", r"\1 vehicles involved in a traffic collision"),
    (r"\bVEHS?\s+TC\b", "vehicle involved in a traffic collision"),
    (r"\bTC\b", "traffic collision"),
    (r"\bVEHS\b", "vehicles"),
    (r"\bVEH\b", "vehicle"),
    (r"\bBLK(?:ING|G|NG)\b", "blocking"),
    (r"\bBLK\b", "black"),
    (r"\bWHI\b", "white"),
    (r"\bGRY\b", "gray"),
    (r"\bSIL\b", "silver"),
    (r"\bBLU\b", "blue"),
    (r"\bTOYT\b", "Toyota"),
    (r"\bHOND\b", "Honda"),
    (r"\bNISS\b", "Nissan"),
    (r"\bCHEV\b", "Chevrolet"),
    (r"\bINFI\b", "Infiniti"),
    (r"\bCOA\b", "Corolla"),
    (r"\bSD\b", "sedan"),
    (r"\b(?:TK|TRK)\b", "truck"),
    (r"\bMC\b", "motorcycle"),
    (r"\bPED\b", "pedestrian"),
    (r"\bVS\b", "versus"),
    (r"\bADDTL\b", "additional"),
    (r"\bSLO(?:W)?\s+LN\b", "slow lane"),
    (r"\bMIDDLE\s+LNS\b", "middle lanes"),
    (r"#(\d+)\s+LNS?\b", r"lane \1"),
    (r"\bRT\s+LN\b", "right lane"),
    (r"\bLNS\b", "lanes"),
    (r"\bLN\b", "lane"),
    (r"\bON\s+(?:RHS|RS)\b", "on the right shoulder"),
    (r"\bIN\s+(?:RHS|RS)\b", "on the right shoulder"),
    (r"\bON\s+(?:LHS|LS)\b", "on the left shoulder"),
    (r"\bIN\s+(?:LHS|LS)\b", "on the left shoulder"),
    (r"\bRHS\b|\bRS\b", "right shoulder"),
    (r"\bLHS\b|\bLS\b", "left shoulder"),
    (r"\bCD\b", "center divider"),
    (r"\bGP\b", "gore point"),
    (r"\bFWY\b", "freeway"),
    (r"\bOFR\b", "off-ramp"),
    (r"\bONR\b", "on-ramp"),
    (r"\bNB\b", "northbound"),
    (r"\bSB\b", "southbound"),
    (r"\bEB\b", "eastbound"),
    (r"\bWB\b", "westbound"),
    (r"\bJNO\b", "just north of"),
    (r"\bJSO\b", "just south of"),
    (r"\bJEO\b", "just east of"),
    (r"\bJWO\b", "just west of"),
    (r"\bUNK\b", "unknown"),
    (r"\bINJ\b", "injuries"),
    (r"\bPOSS\b", "possible"),
    (r"\bTRFC\b", "traffic"),
    (r"\bENRT\b", "en route"),
    (r"\bETA\b", "estimated arrival"),
    (r"\bFRM\b", "from"),
    (r"\bPTY\b", "party"),
    (r"\bPLS\b", "please"),
    (r"\bRP\b", "reporting party"),
    (r"\bREQ\b", "requested"),
    (r"\bADVS\b", "advises"),
    (r"\bADDT?L\b", "additional"),
    (r"\bFIRE\s+WENT\s+97\b", "Fire Department arrived on scene"),
    (r"\b(?:IS|AT)\s+97\b", "is on scene"),
    (r"\bST\b", "street"),
    (r"\bNEG\s+RESP\b", "no response"),
    (r"\bWILL\s+ADV\b", "will advise"),
    (r"\bCLR\b", "clear"),
    (r"\bFSP\b", "Freeway Service Patrol"),
    (r"\bCT\b", "Caltrans"),
    (r"\bCHP\b", "California Highway Patrol"),
    (r"\b1125\b", "traffic hazard"),
    (r"\b1126\b", "occupied disabled vehicle"),
    (r"\b1182\b", "collision with no reported injuries"),
    (r"\b1183\b", "collision with unknown injuries"),
)


def _readable_sentence(value: str) -> str:
    text = re.sub(r"\s+", " ", value).strip(" -;,.")
    if not text:
        return ""
    text = text.lower()
    proper = {
        "california highway patrol": "California Highway Patrol",
        "freeway service patrol": "Freeway Service Patrol",
        "caltrans": "Caltrans",
        "toyota": "Toyota",
        "honda": "Honda",
        "nissan": "Nissan",
        "chevrolet": "Chevrolet",
        "infiniti": "Infiniti",
        "corolla": "Corolla",
        "fire department": "Fire Department",
        "hov": "HOV",
        "suv": "SUV",
    }
    for source, replacement in proper.items():
        text = re.sub(rf"\b{re.escape(source)}\b", replacement, text, flags=re.IGNORECASE)
    text = re.sub(r"\bi-(\d+)\b", lambda match: f"I-{match.group(1)}", text)
    text = text[0].upper() + text[1:]
    return text if text.endswith((".", "!", "?")) else text + "."


def expand_chp_note(value: object) -> str:
    """Expand common CHP shorthand while retaining the reported road details."""

    text = clean_incident_message(value) or ""
    text = re.sub(r"\s+(?:-|//)\s+", "; ", text)
    for pattern, replacement in _CHP_NOTE_REPLACEMENTS:
        text = re.sub(pattern, replacement, text, flags=re.IGNORECASE)
    return _readable_sentence(text)


def describe_chp_incident(value: object) -> str:
    """Return a short plain-language description for a CHP incident code."""

    raw = re.sub(r"\s+", " ", str(value or "")).strip()
    normalized = raw.upper()
    for prefix, description in _CHP_DESCRIPTION_PREFIXES:
        if normalized == prefix or normalized.startswith(prefix + "-") or normalized.startswith(prefix + " "):
            return description
    fallback = re.sub(r"^[A-Z]*\d+[A-Z]*[- ]*", "", raw).strip()
    return expand_chp_note(fallback or raw) or "Incident reported."


def parse_incident_row(row: Sequence[object]) -> dict[str, object]:
    padded = list(row[: len(INCIDENT_COLUMNS)])
    padded.extend([""] * (len(INCIDENT_COLUMNS) - len(padded)))
    raw = dict(zip(INCIDENT_COLUMNS, padded))

    timestamp = pd.to_datetime(raw["timestamp"], format="%m/%d/%Y %H:%M:%S", errors="coerce")
    duration = _float_or_none(raw["duration_min"])
    duration_default = duration is None or duration <= 0
    active_duration = 30.0 if duration_default else duration
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
        "duration_min": active_duration,
        "duration_default": duration_default,
    }


_SYSTEM_INCIDENT_MESSAGES = {
    "unit assigned",
    "unit enroute",
    "unit at scene",
    "unit cleared",
    "incident opened",
    "incident closed",
}


def clean_incident_message(value: object) -> str | None:
    text = str(value or "").strip()
    if not text:
        return None
    text = re.sub(r"^(?:\s*\[[^\]]+\])+\s*", "", text)
    text = re.sub(r"\[Shared\]", "", text, flags=re.IGNORECASE)
    text = re.sub(r"\s+", " ", text).strip(" -")
    if not text or text.casefold() in _SYSTEM_INCIDENT_MESSAGES:
        return None
    return text


def select_incident_notes(messages: Iterable[object], limit: int = 3) -> list[str]:
    notes: list[str] = []
    seen: set[str] = set()
    for message in messages:
        cleaned = clean_incident_message(message)
        if not cleaned:
            continue
        key = re.sub(r"[^a-z0-9]+", " ", cleaned.casefold()).strip()
        if not key or key in seen:
            continue
        seen.add(key)
        notes.append(cleaned)
        if len(notes) >= limit:
            break
    return notes


def read_incidents(
    root: Path,
    districts: Iterable[str] = ("07", "08", "12"),
    start: pd.Timestamp = pd.Timestamp("2026-04-01 00:00:00"),
    end: pd.Timestamp = pd.Timestamp("2026-04-30 23:59:59"),
) -> pd.DataFrame:
    allowed = {str(value).zfill(2) for value in districts}
    records: list[dict[str, object]] = []
    monthly_paths = sorted(
        (root.parent / "incident_month").glob(
            "all_text_chp_incidents_month_*/all_text_chp_incidents_month_*.txt.gz"
        )
    )
    daily_paths = sorted(
        root.glob("all_text_chp_incidents_day_*/all_text_chp_incident_day_*.txt.gz")
    )
    for path in monthly_paths or daily_paths:
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
    max_postmile_gap: float = 1.0,
    max_coordinate_gap_m: float = 3_000.0,
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
        with warnings.catch_warnings(), np.errstate(all="ignore"):
            warnings.simplefilter("ignore", category=RuntimeWarning)
            baseline[int(slot)] = np.nanmedian(matrix[weekslots == slot], axis=0)
    return baseline


def compute_leave_date_out_normal(
    speed_values: Sequence[object],
    times: Sequence[pd.Timestamp],
    incident_start: pd.Timestamp,
    incident_end: pd.Timestamp,
    min_reference_days: int = 2,
) -> float | None:
    index = pd.DatetimeIndex(times)
    speeds = pd.to_numeric(pd.Series(speed_values), errors="coerce").to_numpy(dtype=float)
    if len(index) != len(speeds):
        raise ValueError("The number of timestamps must match the speed values.")

    start = pd.Timestamp(incident_start)
    end = pd.Timestamp(incident_end)
    if pd.isna(start) or pd.isna(end) or end <= start:
        end = start + pd.Timedelta(minutes=30)

    during = (index >= start) & (index < end)
    if not during.any():
        return None
    event_dates = pd.date_range(start.normalize(), end.normalize(), freq="D")
    reference = ~index.normalize().isin(event_dates)
    weekslots = index.dayofweek * 288 + index.hour * 12 + index.minute // 5

    slot_medians: list[float] = []
    for slot in np.unique(weekslots[during]):
        candidates = speeds[reference & (weekslots == slot)]
        valid = candidates[np.isfinite(candidates) & (candidates > 0)]
        if valid.size >= min_reference_days:
            slot_medians.append(float(np.median(valid)))
    return round(float(np.median(slot_medians)), 1) if slot_medians else None


def compute_incident_speed_metrics(
    speed_values: Sequence[object],
    times: Sequence[pd.Timestamp],
    incident_start: pd.Timestamp,
    incident_end: pd.Timestamp,
    normal_values: Sequence[object] | None = None,
    min_samples: int = 2,
) -> dict[str, float | int | None]:
    index = pd.DatetimeIndex(times)
    speeds = pd.to_numeric(pd.Series(speed_values), errors="coerce").to_numpy(dtype=float)
    if len(index) != len(speeds):
        raise ValueError("The number of timestamps must match the speed values.")

    start = pd.Timestamp(incident_start)
    end = pd.Timestamp(incident_end)
    if pd.isna(start) or pd.isna(end) or end <= start:
        end = start + pd.Timedelta(minutes=30)

    masks = {
        "before": (index >= start - pd.Timedelta(minutes=30)) & (index < start),
        "during": (index >= start) & (index < end),
        "after": (index >= end) & (index < end + pd.Timedelta(minutes=30)),
    }

    medians: dict[str, float | None] = {}
    counts: dict[str, int] = {}
    for name, mask in masks.items():
        window = speeds[mask]
        valid = window[np.isfinite(window) & (window > 0)]
        counts[name] = int(valid.size)
        medians[name] = round(float(np.median(valid)), 1) if valid.size >= min_samples else None

    normal_during: float | None = None
    if normal_values is not None:
        normal = pd.to_numeric(pd.Series(normal_values), errors="coerce").to_numpy(dtype=float)
        if len(normal) != len(index):
            raise ValueError("The number of normal values must match the timestamps.")
        window = normal[masks["during"]]
        valid_normal = window[np.isfinite(window) & (window > 0)]
        if valid_normal.size:
            normal_during = round(float(np.median(valid_normal)), 1)

    before = medians["before"]
    during = medians["during"]
    drop = round(before - during, 1) if before is not None and during is not None else None
    drop_pct = round(drop / before * 100, 1) if drop is not None and before else None
    deficit = (
        round(normal_during - during, 1)
        if normal_during is not None and during is not None
        else None
    )

    return {
        "speed_before_mph": before,
        "speed_during_mph": during,
        "speed_after_mph": medians["after"],
        "speed_drop_mph": drop,
        "speed_drop_pct": drop_pct,
        "normal_during_mph": normal_during,
        "deficit_vs_normal_mph": deficit,
        "samples_before": counts["before"],
        "samples_during": counts["during"],
        "samples_after": counts["after"],
    }


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
        incident_start = pd.Timestamp(incident["timestamp"])
        incident_end = pd.Timestamp(incident["end_time"])
        first = int(math.floor((incident_start.value - start.value) / interval_ns))
        stop = int(math.ceil((incident_end.value - start.value) / interval_ns))
        first = max(0, first)
        stop = min(len(index), max(first, stop))
        for time_idx in range(first, stop):
            buckets[time_idx].append(int(incident_idx))
    return buckets
