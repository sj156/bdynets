#!/usr/bin/env python3
"""Check downloaded PeMS raw files for recognizable names and obvious issues."""

from __future__ import annotations

import argparse
import collections
import re
from pathlib import Path


PATTERNS = {
    "station_5min": re.compile(
        r"^d11_text_station_5min_(?P<year>\d{4})_(?P<month>\d{2})_(?P<day>\d{2})\.txt(?:\.gz)?$"
    ),
    "station_hour": re.compile(
        r"^d11_text_station_hour_(?P<year>\d{4})_(?P<month>\d{2})\.txt(?:\.gz)?$"
    ),
    "station_day": re.compile(
        r"^d11_text_station_day_(?P<year>\d{4})_(?P<month>\d{2})\.txt(?:\.gz)?$"
    ),
    "station_aadt": re.compile(
        r"^d11_text_station_aadt_(?:\d{4}_\d{2}\.txt\.zip|month_hours_\d{4}_\d{2}\.txt(?:\.gz)?)$"
    ),
    "station_aadt_aux": re.compile(
        r"^d11_text_station_aadt_(?:annual_dow|flow_monthly|hour_monthly|top30_monthly)_\d{4}_\d{2}\.txt(?:\.gz)?$"
    ),
    "census_vclass_hour": re.compile(
        r"^all_text_tmg_vclass_hour.*\.txt(?:\.gz)?$"
    ),
    "station_metadata": re.compile(
        r"^d11_text_meta_(?P<year>\d{4})_(?P<month>\d{2})_(?P<day>\d{2})\.txt(?:\.gz)?$"
    ),
    "holiday": re.compile(r"^pems_holiday_insert_.*\.txt$"),
}


def classify(path: Path) -> tuple[str | None, dict[str, str]]:
    for dataset_id, pattern in PATTERNS.items():
        match = pattern.match(path.name)
        if match:
            return dataset_id, match.groupdict()
    return None, {}


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Scan PeMS raw download files for naming, duplicate, and empty-file issues."
    )
    parser.add_argument(
        "raw_dir",
        nargs="?",
        default="data/raw",
        help="Directory containing downloaded raw PeMS files. Default: data/raw",
    )
    args = parser.parse_args()

    raw_dir = Path(args.raw_dir)
    if not raw_dir.exists():
        print(f"ERROR: raw directory does not exist: {raw_dir}")
        return 2

    ignored_names = {"README.md", ".DS_Store"}
    files = [p for p in raw_dir.rglob("*") if p.is_file() and p.name not in ignored_names]
    by_name: dict[str, list[Path]] = collections.defaultdict(list)
    by_dataset: dict[str, list[tuple[Path, dict[str, str]]]] = collections.defaultdict(list)
    unknown: list[Path] = []
    empty: list[Path] = []

    for path in files:
        by_name[path.name].append(path)
        if path.stat().st_size == 0:
            empty.append(path)
        dataset_id, parts = classify(path)
        if dataset_id is None:
            unknown.append(path)
        else:
            by_dataset[dataset_id].append((path, parts))

    duplicates = {name: paths for name, paths in by_name.items() if len(paths) > 1}

    print(f"Scanned directory: {raw_dir}")
    print(f"Total files: {len(files)}")
    print(f"Recognized files: {sum(len(v) for v in by_dataset.values())}")
    print(f"Unknown files: {len(unknown)}")
    print(f"Empty files: {len(empty)}")
    print(f"Duplicate filenames: {len(duplicates)}")
    print()

    if by_dataset:
        print("Recognized by dataset:")
        for dataset_id in sorted(by_dataset):
            records = by_dataset[dataset_id]
            years = sorted({parts.get("year") for _, parts in records if parts.get("year")})
            months = sorted({parts.get("month") for _, parts in records if parts.get("month")})
            days = sorted({parts.get("day") for _, parts in records if parts.get("day")})
            summary = [f"{len(records)} file(s)"]
            if years:
                summary.append(f"years={','.join(years)}")
            if months:
                summary.append(f"months={','.join(months)}")
            if days and dataset_id in {"station_5min", "station_metadata"}:
                summary.append(f"days={len(days)} distinct day values")
            print(f"- {dataset_id}: {'; '.join(summary)}")
        print()

    if unknown:
        print("Unknown files:")
        for path in unknown[:100]:
            print(f"- {path}")
        if len(unknown) > 100:
            print(f"- ... {len(unknown) - 100} more")
        print()

    if empty:
        print("Empty files:")
        for path in empty:
            print(f"- {path}")
        print()

    if duplicates:
        print("Duplicate filenames:")
        for name, paths in duplicates.items():
            print(f"- {name}")
            for path in paths:
                print(f"  {path}")
        print()

    if unknown or empty or duplicates:
        print("Result: review issues above before loading data.")
        return 1

    print("Result: no obvious filename, duplicate, or empty-file issues found.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
