#!/usr/bin/env python3
"""Build lightweight processed outputs for D11 Station Hour 2020 downloads."""

from __future__ import annotations

import csv
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
RAW_DIR = ROOT / "data" / "raw"
PROCESSED_DIR = ROOT / "data" / "processed"
METADATA_DIR = ROOT / "metadata"

HOUR_PATTERN = re.compile(r"^d11_text_station_hour_(?P<year>\d{4})_(?P<month>\d{2})\.txt(?:\.gz)?$")
META_PATTERN = re.compile(
    r"^d11_text_meta_(?P<year>\d{4})_(?P<month>\d{2})_(?P<day>\d{2})\.txt(?:\.gz)?$"
)

METADATA_COLUMNS = [
    "metadata_date",
    "source_file",
    "station",
    "freeway",
    "direction",
    "district",
    "county",
    "city",
    "state_postmile",
    "absolute_postmile",
    "latitude",
    "longitude",
    "length",
    "type",
    "lanes",
    "name",
    "user_id_1",
    "user_id_2",
    "user_id_3",
    "user_id_4",
]


def count_lines(path: Path) -> int:
    with path.open("rb") as f:
        return sum(1 for _ in f)


def first_line(path: Path) -> str:
    with path.open("rb") as f:
        return f.readline().decode("utf-8", errors="replace").rstrip("\r\n")


def last_nonempty_line(path: Path) -> str:
    with path.open("rb") as f:
        f.seek(0, 2)
        pos = f.tell()
        buf = bytearray()
        while pos > 0:
            pos -= 1
            f.seek(pos)
            b = f.read(1)
            if b in {b"\n", b"\r"}:
                if buf:
                    break
                continue
            buf.extend(b)
        return bytes(reversed(buf)).decode("utf-8", errors="replace")


def normalize_metadata_row(row: list[str]) -> list[str]:
    """PeMS metadata names sometimes contain embedded tabs; fold them into name."""
    if len(row) < 18:
        return row + [""] * (18 - len(row))
    if len(row) == 18:
        return row
    fixed = row[:13]
    fixed.append(" ".join(part for part in row[13:-4] if part))
    fixed.extend(row[-4:])
    return fixed


def metadata_date_from_name(path: Path) -> str:
    match = META_PATTERN.match(path.name)
    if match is None:
        raise ValueError(f"Unexpected metadata filename: {path.name}")
    return f"{match.group('year')}-{match.group('month')}-{match.group('day')}"


def build_download_manifest(hour_files: list[Path], metadata_files: list[Path]) -> None:
    out = METADATA_DIR / "downloaded_files.csv"
    rows = []

    for path in hour_files:
        match = HOUR_PATTERN.match(path.name)
        assert match is not None
        rows.append(
            {
                "file": str(path.relative_to(ROOT)),
                "dataset_id": "station_hour",
                "year": match.group("year"),
                "month": match.group("month"),
                "day": "",
                "bytes": path.stat().st_size,
                "rows": count_lines(path),
            }
        )

    for path in metadata_files:
        match = META_PATTERN.match(path.name)
        assert match is not None
        rows.append(
            {
                "file": str(path.relative_to(ROOT)),
                "dataset_id": "station_metadata",
                "year": match.group("year"),
                "month": match.group("month"),
                "day": match.group("day"),
                "bytes": path.stat().st_size,
                "rows": count_lines(path),
            }
        )

    with out.open("w", newline="") as f:
        writer = csv.DictWriter(
            f, fieldnames=["file", "dataset_id", "year", "month", "day", "bytes", "rows"]
        )
        writer.writeheader()
        writer.writerows(rows)


def build_hour_monthly_summary(hour_files: list[Path]) -> None:
    out = PROCESSED_DIR / "station_hour_2020_monthly_summary.csv"
    rows = []
    for path in hour_files:
        match = HOUR_PATTERN.match(path.name)
        assert match is not None
        rows.append(
            {
                "file": path.name,
                "year": match.group("year"),
                "month": match.group("month"),
                "rows": count_lines(path),
                "bytes": path.stat().st_size,
                "first_timestamp": first_line(path).split(",", 1)[0],
                "last_timestamp": last_nonempty_line(path).split(",", 1)[0],
            }
        )

    with out.open("w", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "file",
                "year",
                "month",
                "rows",
                "bytes",
                "first_timestamp",
                "last_timestamp",
            ],
        )
        writer.writeheader()
        writer.writerows(rows)


def build_metadata_outputs(metadata_files: list[Path]) -> dict[str, list[str]]:
    snapshots_out = PROCESSED_DIR / "station_metadata_2020_snapshots.csv"
    latest_out = PROCESSED_DIR / "station_metadata_2020_latest_by_station.csv"
    single_out = PROCESSED_DIR / "station_metadata_2020.csv"

    rows: list[list[str]] = []
    extra_tab_rows: list[tuple[str, int]] = []
    for path in metadata_files:
        metadata_date = metadata_date_from_name(path)
        with path.open(newline="") as f:
            reader = csv.reader(f, delimiter="\t")
            next(reader)
            for line_no, row in enumerate(reader, start=2):
                if len(row) > 18:
                    extra_tab_rows.append((path.name, line_no))
                rows.append([metadata_date, path.name, *normalize_metadata_row(row)])

    with snapshots_out.open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(METADATA_COLUMNS)
        writer.writerows(rows)

    latest_by_station: dict[str, list[str]] = {}
    for row in rows:
        station = row[2]
        if station not in latest_by_station or row[0] > latest_by_station[station][0]:
            latest_by_station[station] = row

    latest_rows = [latest_by_station[s] for s in sorted(latest_by_station, key=int)]
    for out in (latest_out, single_out):
        with out.open("w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(METADATA_COLUMNS)
            writer.writerows(latest_rows)

    extra_out = PROCESSED_DIR / "station_metadata_2020_extra_tab_rows.csv"
    with extra_out.open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["file", "line"])
        writer.writerows(extra_tab_rows)

    return latest_by_station


def parse_int(value: str) -> int | None:
    if value == "":
        return None
    try:
        return int(value)
    except ValueError:
        return None


def parse_float(value: str) -> float | None:
    if value == "":
        return None
    try:
        return float(value)
    except ValueError:
        return None


def safe_divide(numerator: float, denominator: float) -> float | str:
    if denominator == 0:
        return ""
    return numerator / denominator


def build_station_outputs(hour_files: list[Path], latest_metadata: dict[str, list[str]]) -> None:
    metadata_stations = set(latest_metadata)
    hour_stations: set[str] = set()
    station_stats: dict[str, dict[str, object]] = {}
    monthly_stats: dict[tuple[str, str], dict[str, float | int]] = {}

    for path in hour_files:
        match = HOUR_PATTERN.match(path.name)
        month = match.group("month") if match else ""
        with path.open(newline="") as f:
            reader = csv.reader(f)
            for row in reader:
                if len(row) > 1:
                    timestamp, station = row[0], row[1]
                    hour_stations.add(station)
                    stats = station_stats.setdefault(
                        station,
                        {
                            "rows": 0,
                            "first_timestamp": timestamp,
                            "last_timestamp": timestamp,
                            "months": set(),
                        },
                    )
                    stats["rows"] = int(stats["rows"]) + 1
                    if timestamp < str(stats["first_timestamp"]):
                        stats["first_timestamp"] = timestamp
                    if timestamp > str(stats["last_timestamp"]):
                        stats["last_timestamp"] = timestamp
                    stats["months"].add(month)

                    samples = parse_int(row[7]) if len(row) > 7 else None
                    total_flow = parse_float(row[9]) if len(row) > 9 else None
                    average_speed = parse_float(row[11]) if len(row) > 11 else None
                    group = monthly_stats.setdefault(
                        (station, month),
                        {
                            "records": 0,
                            "samples_sum": 0,
                            "total_flow_count": 0,
                            "total_flow_sum": 0.0,
                            "total_flow_weighted_sum": 0.0,
                            "total_flow_weight_samples": 0,
                            "average_speed_count": 0,
                            "average_speed_sum": 0.0,
                            "average_speed_weighted_sum": 0.0,
                            "average_speed_weight_samples": 0,
                        },
                    )
                    group["records"] = int(group["records"]) + 1
                    if samples is not None:
                        group["samples_sum"] = int(group["samples_sum"]) + samples
                    if total_flow is not None:
                        group["total_flow_count"] = int(group["total_flow_count"]) + 1
                        group["total_flow_sum"] = float(group["total_flow_sum"]) + total_flow
                        if samples is not None and samples > 0:
                            group["total_flow_weighted_sum"] = (
                                float(group["total_flow_weighted_sum"]) + total_flow * samples
                            )
                            group["total_flow_weight_samples"] = (
                                int(group["total_flow_weight_samples"]) + samples
                            )
                    if average_speed is not None:
                        group["average_speed_count"] = int(group["average_speed_count"]) + 1
                        group["average_speed_sum"] = float(group["average_speed_sum"]) + average_speed
                        if samples is not None and samples > 0:
                            group["average_speed_weighted_sum"] = (
                                float(group["average_speed_weighted_sum"]) + average_speed * samples
                            )
                            group["average_speed_weight_samples"] = (
                                int(group["average_speed_weight_samples"]) + samples
                            )

    missing = sorted(hour_stations - metadata_stations, key=int)
    matched = sorted(hour_stations & metadata_stations, key=int)

    missing_out = PROCESSED_DIR / "station_hour_2020_missing_metadata_stations.txt"
    missing_out.write_text("\n".join(missing) + ("\n" if missing else ""))

    coverage_out = PROCESSED_DIR / "station_hour_2020_metadata_coverage.csv"
    with coverage_out.open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["metric", "value"])
        writer.writerow(["hour_stations", len(hour_stations)])
        writer.writerow(["metadata_stations", len(metadata_stations)])
        writer.writerow(["hour_matched_metadata", len(matched)])
        writer.writerow(["hour_missing_metadata", len(missing)])

    missing_summary_out = PROCESSED_DIR / "station_hour_2020_missing_metadata_station_summary.csv"
    with missing_summary_out.open("w", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "station",
                "rows",
                "first_timestamp",
                "last_timestamp",
                "months",
            ],
        )
        writer.writeheader()
        for station in missing:
            stats = station_stats[station]
            writer.writerow(
                {
                    "station": station,
                    "rows": stats["rows"],
                    "first_timestamp": stats["first_timestamp"],
                    "last_timestamp": stats["last_timestamp"],
                    "months": ";".join(sorted(stats["months"])),
                }
            )

    basic_summary_out = PROCESSED_DIR / "station_hour_2020_station_month_basic_summary.csv"
    with basic_summary_out.open("w", newline="") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=[
                "station",
                "month",
                "metadata_matched",
                "records",
                "samples_sum",
                "total_flow_count",
                "total_flow_mean",
                "total_flow_weighted_by_samples",
                "average_speed_count",
                "average_speed_mean",
                "average_speed_weighted_by_samples",
            ],
        )
        writer.writeheader()
        for station, month in sorted(monthly_stats, key=lambda item: (int(item[0]), item[1])):
            stats = monthly_stats[(station, month)]
            writer.writerow(
                {
                    "station": station,
                    "month": month,
                    "metadata_matched": "yes" if station in metadata_stations else "no",
                    "records": stats["records"],
                    "samples_sum": stats["samples_sum"],
                    "total_flow_count": stats["total_flow_count"],
                    "total_flow_mean": safe_divide(
                        float(stats["total_flow_sum"]), int(stats["total_flow_count"])
                    ),
                    "total_flow_weighted_by_samples": safe_divide(
                        float(stats["total_flow_weighted_sum"]),
                        int(stats["total_flow_weight_samples"]),
                    ),
                    "average_speed_count": stats["average_speed_count"],
                    "average_speed_mean": safe_divide(
                        float(stats["average_speed_sum"]), int(stats["average_speed_count"])
                    ),
                    "average_speed_weighted_by_samples": safe_divide(
                        float(stats["average_speed_weighted_sum"]),
                        int(stats["average_speed_weight_samples"]),
                    ),
                }
            )


def main() -> int:
    PROCESSED_DIR.mkdir(parents=True, exist_ok=True)
    METADATA_DIR.mkdir(parents=True, exist_ok=True)

    hour_files = sorted(
        p for p in RAW_DIR.glob("d11_text_station_hour_2020_*.txt") if HOUR_PATTERN.match(p.name)
    )
    metadata_files = sorted(
        p for p in RAW_DIR.glob("d11_text_meta_2020_*.txt") if META_PATTERN.match(p.name)
    )

    if not hour_files:
        raise SystemExit("No Station Hour 2020 files found.")
    if not metadata_files:
        raise SystemExit("No Station Metadata 2020 files found.")

    build_download_manifest(hour_files, metadata_files)
    build_hour_monthly_summary(hour_files)
    latest_metadata = build_metadata_outputs(metadata_files)
    build_station_outputs(hour_files, latest_metadata)

    print(f"Station Hour files: {len(hour_files)}")
    print(f"Metadata files: {len(metadata_files)}")
    print("Wrote metadata/downloaded_files.csv and data/processed outputs.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
