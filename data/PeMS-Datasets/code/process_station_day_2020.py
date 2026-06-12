#!/usr/bin/env python3
"""Process D11 Station Day 2020 files into lightweight analysis outputs."""

from __future__ import annotations

import csv
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "data" / "raw"
PROCESSED = ROOT / "data" / "processed"

DAY_COLUMNS = [
    "timestamp",
    "station",
    "district",
    "route",
    "direction_of_travel",
    "lane_type",
    "station_length",
    "samples",
    "percentage_observed",
    "total_flow",
    "delay_35",
    "delay_40",
    "delay_45",
    "delay_50",
    "delay_55",
    "delay_60",
]


def main() -> int:
    PROCESSED.mkdir(parents=True, exist_ok=True)
    files = sorted(RAW.glob("d11_text_station_day_2020_*.txt"))
    if not files:
        raise SystemExit("No Station Day 2020 files found in data/raw.")

    metadata = pd.read_csv(PROCESSED / "station_metadata_2020_latest_by_station.csv", dtype={"station": str})
    metadata_stations = set(metadata["station"])

    frames = []
    monthly_rows = []
    for path in files:
        month = path.stem.rsplit("_", 1)[-1]
        df = pd.read_csv(path, header=None, names=DAY_COLUMNS)
        df["station"] = df["station"].astype(str)
        df["date"] = pd.to_datetime(df["timestamp"], format="%m/%d/%Y %H:%M:%S").dt.strftime("%Y-%m-%d")
        df["month"] = month
        df["metadata_matched"] = df["station"].isin(metadata_stations).map({True: "yes", False: "no"})
        frames.append(
            df[
                [
                    "date",
                    "month",
                    "station",
                    "metadata_matched",
                    "route",
                    "direction_of_travel",
                    "lane_type",
                    "samples",
                    "percentage_observed",
                    "total_flow",
                ]
            ]
        )
        monthly_rows.append(
            {
                "file": path.name,
                "month": month,
                "rows": len(df),
                "first_date": df["date"].min(),
                "last_date": df["date"].max(),
                "stations": df["station"].nunique(),
            }
        )

    out = pd.concat(frames, ignore_index=True)
    out.to_csv(PROCESSED / "station_day_2020_daily_flow.csv", index=False)

    pd.DataFrame(monthly_rows).to_csv(PROCESSED / "station_day_2020_monthly_file_summary.csv", index=False)

    hour_stations = set(out["station"])
    missing = sorted(hour_stations - metadata_stations, key=int)
    (PROCESSED / "station_day_2020_missing_metadata_stations.txt").write_text(
        "\n".join(missing) + ("\n" if missing else "")
    )

    with (PROCESSED / "station_day_2020_metadata_coverage.csv").open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["metric", "value"])
        writer.writerow(["day_stations", len(hour_stations)])
        writer.writerow(["metadata_stations", len(metadata_stations)])
        writer.writerow(["day_matched_metadata", len(hour_stations & metadata_stations)])
        writer.writerow(["day_missing_metadata", len(missing)])

    print(f"Station Day files: {len(files)}")
    print(f"Rows: {len(out)}")
    print(f"Stations: {out['station'].nunique()}")
    print("Wrote Station Day processed outputs.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
