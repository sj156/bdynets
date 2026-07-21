#!/usr/bin/env python3
"""Download and subset NOAA GHCNh observations for the April 2026 map."""

from __future__ import annotations

import tempfile
import urllib.request
from pathlib import Path

import pandas as pd

from traffic_context import WEATHER_COLUMNS, normalize_noaa_weather


ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "data" / "April" / "weather" / "noaa_ghcnh_april_2026.csv"
BASE_URL = (
    "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/"
    "hourly/access/by-year/2026/psv/GHCNh_{station}_2026.psv"
)

STATIONS = (
    "USW00023174",  # Los Angeles International Airport
    "USW00023129",  # Long Beach Airport
    "USW00023152",  # Hollywood Burbank Airport
    "USW00093184",  # John Wayne Airport
    "USW00003102",  # Ontario International Airport
    "USW00003171",  # Riverside Municipal Airport
    "USW00093138",  # Palm Springs International Airport
)


def download_station(station: str) -> pd.DataFrame:
    url = BASE_URL.format(station=station)
    print(f"Downloading NOAA GHCNh {station}")
    with tempfile.NamedTemporaryFile(suffix=".psv") as temporary:
        urllib.request.urlretrieve(url, temporary.name)
        raw = pd.read_csv(
            temporary.name,
            sep="|",
            usecols=WEATHER_COLUMNS,
            low_memory=False,
        )

    frame = normalize_noaa_weather(raw)
    frame = frame[
        (frame["time"] >= pd.Timestamp("2026-04-01 00:00:00"))
        & (frame["time"] <= pd.Timestamp("2026-04-30 23:59:59"))
    ].copy()
    frame["source_url"] = url
    return frame


def download_weather(output: Path = OUTPUT) -> pd.DataFrame:
    frames = [download_station(station) for station in STATIONS]
    weather = pd.concat(frames, ignore_index=True)
    weather = weather.sort_values(["station", "time"]).reset_index(drop=True)
    output.parent.mkdir(parents=True, exist_ok=True)
    weather.to_csv(output, index=False)
    return weather


def main() -> int:
    weather = download_weather()
    coverage = weather.groupby("station").agg(
        station_name=("station_name", "first"),
        first_time=("time", "min"),
        last_time=("time", "max"),
        observations=("time", "size"),
        precipitation_reports=("precipitation_mm", "count"),
    )
    print(coverage.to_string())
    print(f"Wrote {OUTPUT}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
