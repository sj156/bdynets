#!/usr/bin/env python3
"""Core helpers for the two-year monthly PeMS processing pipeline."""

from __future__ import annotations

import calendar
import csv
import gzip
import math
import os
import re
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Sequence

import numpy as np
import pandas as pd


DISTRICTS = ("07", "08", "12")
FIVE_MIN_COLUMNS = [
    "timestamp",
    "station",
    "district",
    "freeway",
    "direction",
    "lane_type",
    "station_length",
    "samples",
    "percentage_observed",
    "total_flow",
    "average_occupancy",
    "average_speed",
]
for lane in range(1, 9):
    FIVE_MIN_COLUMNS.extend(
        [
            f"lane{lane}_samples",
            f"lane{lane}_flow",
            f"lane{lane}_average_occupancy",
            f"lane{lane}_average_speed",
            f"lane{lane}_observed",
        ]
    )
TRAFFIC_USE_COLUMNS = [
    "timestamp",
    "station",
    "lane_type",
    "samples",
    "percentage_observed",
    "average_speed",
]
SOURCE_PATTERN = re.compile(
    r"^d(?P<district>07|08|12)_text_station_5min_"
    r"(?P<year>\d{4})_(?P<month>\d{2})_(?P<day>\d{2})\s*"
    r"\.(?P<format>txt(?:\.gz)?)$",
    re.IGNORECASE,
)
UF_DATALESS = 0x40000000
METADATA_PATTERN = re.compile(
    r"^d(?P<district>07|08|12)_text_meta_(?P<date>\d{4}_\d{2}_\d{2})\.txt$",
    re.IGNORECASE,
)


@dataclass(frozen=True)
class SourceRecord:
    path: Path
    district: str
    date: str
    file_format: str


@dataclass(frozen=True)
class InventoryResult:
    records: tuple[SourceRecord, ...]
    missing: tuple[tuple[str, str], ...]
    duplicates: dict[tuple[str, str], tuple[SourceRecord, ...]]


def parse_source_name(name: str) -> tuple[str, str, str] | None:
    match = SOURCE_PATTERN.match(name)
    if not match:
        return None
    parts = match.groupdict()
    date = f"{parts['year']}-{parts['month']}-{parts['day']}"
    return parts["district"], date, parts["format"].lower()


def read_metadata_directory(root: Path) -> pd.DataFrame:
    frames: list[pd.DataFrame] = []
    for path in sorted(Path(root).glob("d*_text_meta_*.txt")):
        match = METADATA_PATTERN.match(path.name)
        if not match:
            continue
        with path.open(newline="", encoding="utf-8", errors="replace") as handle:
            reader = csv.reader(handle, delimiter="\t")
            header = next(reader, None)
            if not header:
                continue
            rows: list[list[str]] = []
            for raw in reader:
                if len(raw) < len(header):
                    raw = raw + [""] * (len(header) - len(raw))
                elif len(raw) > len(header):
                    raw = raw[:13] + [" ".join(part for part in raw[13:-4] if part)] + raw[-4:]
                rows.append(raw)
        frame = pd.DataFrame(rows, columns=header, dtype="string")
        frame["metadata_date"] = match.group("date").replace("_", "-")
        frame["source_file"] = path.name
        frames.append(frame)
    if not frames:
        raise FileNotFoundError(f"No D7/D8/D12 metadata files found below {root}")

    metadata = pd.concat(frames, ignore_index=True)
    metadata = metadata.rename(
        columns={
            "ID": "station",
            "Fwy": "freeway",
            "Dir": "direction",
            "District": "district",
            "County": "county",
            "City": "city",
            "State_PM": "state_pm",
            "Abs_PM": "abs_pm",
            "Latitude": "latitude",
            "Longitude": "longitude",
            "Length": "length",
            "Type": "type",
            "Lanes": "lanes",
            "Name": "name",
        }
    )
    required = {
        "station",
        "freeway",
        "direction",
        "district",
        "abs_pm",
        "latitude",
        "longitude",
        "type",
    }
    missing = required.difference(metadata.columns)
    if missing:
        raise ValueError(f"Metadata is missing columns: {sorted(missing)}")

    metadata["station"] = metadata["station"].astype("string").str.strip().str.removesuffix(".0")
    metadata["district"] = (
        metadata["district"].astype("string").str.strip().str.removesuffix(".0").str.zfill(2)
    )
    metadata["freeway"] = metadata["freeway"].astype("string").str.strip().str.removesuffix(".0")
    metadata["direction"] = metadata["direction"].astype("string").str.strip().str.upper()
    metadata["type"] = metadata["type"].astype("string").str.strip().str.upper()
    for column in ("abs_pm", "latitude", "longitude", "lanes", "length"):
        if column in metadata:
            metadata[column] = pd.to_numeric(metadata[column], errors="coerce")
    metadata = metadata[
        metadata["district"].isin(DISTRICTS)
    ].sort_values(["station", "metadata_date"], kind="stable")
    return metadata.drop_duplicates("station", keep="last").reset_index(drop=True)


def inventory_month(root: Path, year: int, month: int) -> InventoryResult:
    prefix = f"{year:04d}-{month:02d}-"
    records: list[SourceRecord] = []
    by_key: dict[tuple[str, str], list[SourceRecord]] = {}
    for path in sorted(root.rglob("*")):
        if not path.is_file():
            continue
        parsed = parse_source_name(path.name)
        if not parsed:
            continue
        district, date, file_format = parsed
        if not date.startswith(prefix):
            continue
        record = SourceRecord(path, district, date, file_format)
        records.append(record)
        by_key.setdefault((district, date), []).append(record)

    expected: list[tuple[str, str]] = []
    for day in range(1, calendar.monthrange(year, month)[1] + 1):
        date = f"{year:04d}-{month:02d}-{day:02d}"
        expected.extend((district, date) for district in DISTRICTS)
    missing = tuple(key for key in expected if key not in by_key)
    duplicates = {
        key: tuple(value)
        for key, value in sorted(by_key.items())
        if len(value) > 1
    }
    return InventoryResult(tuple(records), missing, duplicates)


def clean_speed_values(
    values: Sequence[object],
    samples: Sequence[object] | None = None,
    percentage_observed: Sequence[object] | None = None,
) -> tuple[np.ndarray, dict[str, int]]:
    numeric = pd.to_numeric(pd.Series(values), errors="coerce").to_numpy(dtype=np.float32)
    valid = np.isfinite(numeric) & (numeric >= 1) & (numeric <= 100)
    observed = np.ones(len(numeric), dtype=bool)
    if samples is not None:
        sample_values = pd.to_numeric(pd.Series(samples), errors="coerce").to_numpy(dtype=float)
        observed &= np.isfinite(sample_values) & (sample_values > 0)
    if percentage_observed is not None:
        observed_values = pd.to_numeric(
            pd.Series(percentage_observed), errors="coerce"
        ).to_numpy(dtype=float)
        observed &= np.isfinite(observed_values) & (observed_values > 0)

    cleaned = numeric.copy()
    cleaned[~valid] = np.nan
    return cleaned, {
        "rows": int(len(cleaned)),
        "valid": int(valid.sum()),
        "invalid": int((~valid).sum()),
        "unobserved_valid": int((valid & ~observed).sum()),
    }


def month_time_index(year: int, month: int) -> pd.DatetimeIndex:
    start = pd.Timestamp(year=year, month=month, day=1)
    if month == 12:
        end = pd.Timestamp(year=year + 1, month=1, day=1)
    else:
        end = pd.Timestamp(year=year, month=month + 1, day=1)
    return pd.date_range(start, end - pd.Timedelta(minutes=5), freq="5min")


def distance_m(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    radius = 6_371_000.0
    phi1 = math.radians(lat1)
    phi2 = math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlambda = math.radians(lon2 - lon1)
    value = (
        math.sin(dphi / 2) ** 2
        + math.cos(phi1) * math.cos(phi2) * math.sin(dlambda / 2) ** 2
    )
    return radius * 2 * math.atan2(math.sqrt(value), math.sqrt(1 - value))


def build_fixed_links(
    metadata: pd.DataFrame,
    max_gap_m: float = 8_500.0,
    max_postmile_gap: float = 8.0,
) -> pd.DataFrame:
    required = {
        "station",
        "district",
        "freeway",
        "direction",
        "abs_pm",
        "latitude",
        "longitude",
        "type",
    }
    missing = required.difference(metadata.columns)
    if missing:
        raise ValueError(f"Metadata is missing columns: {sorted(missing)}")

    frame = metadata.copy()
    for column in ("abs_pm", "latitude", "longitude"):
        frame[column] = pd.to_numeric(frame[column], errors="coerce")
    frame["station"] = frame["station"].astype(str).str.removesuffix(".0")
    frame["district"] = frame["district"].astype(str).str.removesuffix(".0").str.zfill(2)
    frame["freeway"] = frame["freeway"].astype(str).str.removesuffix(".0")
    frame["direction"] = frame["direction"].astype(str).str.strip().str.upper()
    frame["type"] = frame["type"].astype(str).str.strip().str.upper()
    frame = frame[
        (frame["type"] == "ML")
        & frame["district"].isin(DISTRICTS)
    ].dropna(subset=["abs_pm", "latitude", "longitude"])
    frame = frame.sort_values(
        ["district", "freeway", "direction", "abs_pm", "station"],
        kind="stable",
    ).drop_duplicates("station", keep="last")

    rows: list[dict[str, object]] = []
    for (district, freeway, direction), group in frame.groupby(
        ["district", "freeway", "direction"], sort=True
    ):
        ordered = group.sort_values(["abs_pm", "station"], kind="stable").drop_duplicates(
            "abs_pm", keep="first"
        )
        records = list(ordered.itertuples(index=False))
        for first, second in zip(records, records[1:]):
            gap_m = distance_m(
                float(first.latitude),
                float(first.longitude),
                float(second.latitude),
                float(second.longitude),
            )
            pm_gap = abs(float(second.abs_pm) - float(first.abs_pm))
            if gap_m > max_gap_m or pm_gap > max_postmile_gap:
                continue
            rows.append(
                {
                    "link_id": len(rows),
                    "district": str(district),
                    "freeway": str(freeway),
                    "direction": str(direction),
                    "from_station": str(first.station),
                    "to_station": str(second.station),
                    "from_abs_pm": round(float(first.abs_pm), 3),
                    "to_abs_pm": round(float(second.abs_pm), 3),
                    "from_lat": round(float(first.latitude), 7),
                    "from_lon": round(float(first.longitude), 7),
                    "to_lat": round(float(second.latitude), 7),
                    "to_lon": round(float(second.longitude), 7),
                    "gap_m": round(gap_m, 1),
                    "pm_gap": round(pm_gap, 3),
                }
            )
    return pd.DataFrame(rows)


def aggregate_link_speeds(
    station_speed: np.ndarray,
    station_ids: Sequence[str],
    links: pd.DataFrame,
) -> np.ndarray:
    matrix = np.asarray(station_speed, dtype=np.float32)
    if matrix.ndim != 2 or matrix.shape[1] != len(station_ids):
        raise ValueError("Station speed matrix does not match station IDs")
    station_pos = {str(station): index for index, station in enumerate(station_ids)}
    result = np.full((matrix.shape[0], len(links)), np.nan, dtype=np.float32)
    for column, link in enumerate(links.itertuples(index=False)):
        first = station_pos.get(str(link.from_station))
        second = station_pos.get(str(link.to_station))
        endpoint_indices = [index for index in (first, second) if index is not None]
        if not endpoint_indices:
            continue
        values = matrix[:, endpoint_indices]
        valid_count = np.isfinite(values).sum(axis=1)
        np.divide(
            np.nansum(values, axis=1),
            valid_count,
            out=result[:, column],
            where=valid_count > 0,
        )
    return result


def build_station_speed_matrix(
    records: Sequence[SourceRecord],
    station_ids: Sequence[str],
    time_index: pd.DatetimeIndex,
    chunksize: int = 300_000,
    max_source_attempts: int = 4,
    before_date: Callable[[Sequence[SourceRecord]], None] | None = None,
    queue_date: Callable[[Sequence[SourceRecord]], None] | None = None,
    date_ready: Callable[[Sequence[SourceRecord]], bool] | None = None,
    date_lookahead: int = 1,
    after_source: Callable[[SourceRecord], None] | None = None,
) -> tuple[np.ndarray, dict[str, int]]:
    normalized_stations = [str(value).removesuffix(".0") for value in station_ids]
    station_pos = {station: index for index, station in enumerate(normalized_stations)}
    time_pos = {
        label: index
        for index, label in enumerate(time_index.strftime("%m/%d/%Y %H:%M:%S"))
    }
    matrix = np.full(
        (len(time_index), len(normalized_stations)), np.nan, dtype=np.float32
    )
    stats = {
        "files": 0,
        "raw_rows": 0,
        "mainline_rows": 0,
        "valid_speed_rows": 0,
        "invalid_speed_rows": 0,
        "unobserved_valid_rows": 0,
        "unknown_station_rows": 0,
        "invalid_timestamp_rows": 0,
    }

    counter_keys = [key for key in stats if key != "files"]
    ordered = sorted(records, key=lambda record: (record.date, record.district, str(record.path)))
    date_groups: list[list[SourceRecord]] = []
    for record in ordered:
        if not date_groups or date_groups[-1][0].date != record.date:
            date_groups.append([])
        date_groups[-1].append(record)

    window_size = max(1, date_lookahead)
    queued_groups = list(date_groups[:window_size])
    next_group_index = len(queued_groups)
    if queue_date is not None:
        for date_records in queued_groups:
            queue_date(date_records)

    while queued_groups:
        ready_index = 0
        if date_ready is not None:
            wait_deadline = time.monotonic() + 20 * 60
            last_request = time.monotonic()
            while True:
                selected = next(
                    (
                        index
                        for index, group in enumerate(queued_groups)
                        if date_ready(group)
                    ),
                    None,
                )
                if selected is not None:
                    ready_index = selected
                    break
                if time.monotonic() >= wait_deadline:
                    raise TimeoutError("Timed out waiting for a complete source date")
                time.sleep(2)
                if queue_date is not None and time.monotonic() - last_request >= 60:
                    for group in queued_groups:
                        queue_date(group)
                    last_request = time.monotonic()
        date_records = queued_groups.pop(ready_index)
        if before_date is not None:
            before_date(date_records)
        for record in date_records:
            for attempt in range(1, max_source_attempts + 1):
                file_stats = {key: 0 for key in counter_keys}
                try:
                    for chunk in pd.read_csv(
                        record.path,
                        header=None,
                        names=FIVE_MIN_COLUMNS,
                        usecols=TRAFFIC_USE_COLUMNS,
                        dtype={
                            "timestamp": "string",
                            "station": "string",
                            "lane_type": "string",
                        },
                        compression="infer",
                        chunksize=chunksize,
                        low_memory=False,
                    ):
                        file_stats["raw_rows"] += len(chunk)
                        chunk["station"] = chunk["station"].astype("string").str.strip().str.removesuffix(".0")
                        chunk = chunk[
                            chunk["lane_type"].astype("string").str.strip().str.upper() == "ML"
                        ].copy()
                        file_stats["mainline_rows"] += len(chunk)
                        if chunk.empty:
                            continue

                        cleaned, counts = clean_speed_values(
                            chunk["average_speed"],
                            chunk["samples"],
                            chunk["percentage_observed"],
                        )
                        chunk["average_speed"] = cleaned
                        file_stats["valid_speed_rows"] += counts["valid"]
                        file_stats["invalid_speed_rows"] += counts["invalid"]
                        file_stats["unobserved_valid_rows"] += counts["unobserved_valid"]
                        chunk = chunk[np.isfinite(chunk["average_speed"])].copy()
                        if chunk.empty:
                            continue

                        station_indices = chunk["station"].map(station_pos)
                        timestamp_indices = chunk["timestamp"].map(time_pos)
                        file_stats["unknown_station_rows"] += int(station_indices.isna().sum())
                        file_stats["invalid_timestamp_rows"] += int(timestamp_indices.isna().sum())
                        keep = station_indices.notna() & timestamp_indices.notna()
                        if not keep.any():
                            continue
                        matrix[
                            timestamp_indices.loc[keep].astype(int).to_numpy(),
                            station_indices.loc[keep].astype(int).to_numpy(),
                        ] = chunk.loc[keep, "average_speed"].to_numpy(dtype=np.float32)
                    stats["files"] += 1
                    for key in counter_keys:
                        stats[key] += file_stats[key]
                    break
                except OSError as error:
                    is_timeout = isinstance(error, TimeoutError) or getattr(error, "errno", None) in {60, 110}
                    if not is_timeout or attempt >= max_source_attempts:
                        raise
                    print(
                        f"RETRY {record.path.name} after source timeout "
                        f"({attempt}/{max_source_attempts})",
                        flush=True,
                    )
                    subprocess.run(
                        ["brctl", "download", str(record.path)],
                        check=False,
                        capture_output=True,
                        text=True,
                    )
                    time.sleep(min(30, attempt * 5))
            if after_source is not None:
                after_source(record)
        if next_group_index < len(date_groups):
            next_group = date_groups[next_group_index]
            if queue_date is not None:
                queue_date(next_group)
            queued_groups.append(next_group)
            next_group_index += 1
    return matrix, stats


def write_month_speed_csv(
    output: Path,
    time_index: pd.DatetimeIndex,
    link_speed: np.ndarray,
    link_ids: Sequence[int],
) -> None:
    values = np.asarray(link_speed, dtype=np.float32)
    if values.shape != (len(time_index), len(link_ids)):
        raise ValueError("Link speed matrix dimensions do not match output labels")
    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(output.name + ".part")
    frame = pd.DataFrame(values, columns=[f"link_{int(value)}" for value in link_ids])
    frame.insert(0, "time", time_index.strftime("%Y-%m-%d %H:%M:%S"))
    temporary.unlink(missing_ok=True)
    try:
        with temporary.open("wb") as raw_handle:
            with gzip.GzipFile(
                filename="",
                mode="wb",
                fileobj=raw_handle,
                compresslevel=6,
                mtime=0,
            ) as gzip_handle:
                frame.to_csv(
                    gzip_handle,
                    index=False,
                    na_rep="",
                    float_format="%.1f",
                )
            raw_handle.flush()
            os.fsync(raw_handle.fileno())
        os.replace(temporary, output)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise


def verify_month_speed_csv(
    output: Path,
    year: int,
    month: int,
    link_ids: Sequence[int],
) -> dict[str, int | float | str | None]:
    expected_index = month_time_index(year, month)
    expected_columns = ["time"] + [f"link_{int(value)}" for value in link_ids]
    header = pd.read_csv(output, nrows=0).columns.tolist()
    if header != expected_columns:
        raise ValueError(
            f"Monthly speed columns differ from fixed links: expected {len(expected_columns)}, "
            f"found {len(header)}"
        )

    rows = 0
    values = 0
    minimum: float | None = None
    maximum: float | None = None
    first_time: str | None = None
    last_time: str | None = None
    for chunk in pd.read_csv(output, chunksize=256):
        if chunk.empty:
            continue
        rows += len(chunk)
        first_time = first_time or str(chunk["time"].iloc[0])
        last_time = str(chunk["time"].iloc[-1])
        numeric = chunk.iloc[:, 1:].to_numpy(dtype=float)
        valid = numeric[np.isfinite(numeric)]
        values += int(valid.size)
        if valid.size:
            chunk_min = float(valid.min())
            chunk_max = float(valid.max())
            minimum = chunk_min if minimum is None else min(minimum, chunk_min)
            maximum = chunk_max if maximum is None else max(maximum, chunk_max)

    if rows != len(expected_index):
        raise ValueError(f"Monthly speed row count is {rows}, expected {len(expected_index)}")
    expected_first = expected_index[0].strftime("%Y-%m-%d %H:%M:%S")
    expected_last = expected_index[-1].strftime("%Y-%m-%d %H:%M:%S")
    if first_time != expected_first or last_time != expected_last:
        raise ValueError(
            f"Monthly speed time range is {first_time} to {last_time}, "
            f"expected {expected_first} to {expected_last}"
        )
    if minimum is not None and (minimum < 1 or maximum is None or maximum > 100):
        raise ValueError(f"Monthly speed values are outside 1..100: {minimum}..{maximum}")
    return {
        "rows": rows,
        "link_columns": len(link_ids),
        "speed_values": values,
        "minimum_speed": minimum,
        "maximum_speed": maximum,
        "first_time": first_time,
        "last_time": last_time,
    }
