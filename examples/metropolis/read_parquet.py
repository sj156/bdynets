"""Read the supplied day of five-minute volumes and the corresponding static roads.

Arguments: raw directory, road-ID CSV, output JSON, YYYY-MM-DD.
The caller supplies a temporary destination; this program never updates its inputs.
"""
import csv
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq


def read_source_clock(volume_file, day):
    field_type = volume_file.schema_arrow.field('timestamp').type
    if getattr(field_type, 'tz', None) is not None:
        raise ValueError('This source reader expects timezone-unspecified release clocks.')
    raw = volume_file.read(columns=['timestamp']).column('timestamp').to_numpy()
    clock = raw.astype('datetime64[s]')
    if np.any(np.isnat(clock)) or np.any(raw != clock):
        raise ValueError('Volume clocks contain missing times or unsupported fractional seconds.')
    start = np.datetime64(day, 'D').astype('datetime64[s]')
    if not np.array_equal(clock, start + np.arange(288) * np.timedelta64(300, 's')):
        raise ValueError(f'The supplied day {day} needs 288 consecutive five-minute timestamps from midnight.')

    return clock


def read_native(directory, identifiers, day):
    paths = {name: directory / ('city-traffic-M-raw-' + name + '.parquet')
             for name in ('static-features', 'volume')}
    files = {key: pq.ParquetFile(path) for key, path in paths.items()}
    ids = [int(value) for value in identifiers]
    if not ids or len(set(ids)) != len(ids) or min(ids) < 0:
        raise ValueError('Road IDs must be distinct zero-based static-row identifiers.')
    static_fields = ['x_coordinate_start', 'y_coordinate_start', 'x_coordinate_end',
                     'y_coordinate_end', 'length', 'region_id', 'category', 'edge_type', 'speed_limit']
    static = files['static-features'].read(columns=static_fields).take(ids).to_pydict()
    volume_file = files['volume']
    day_clock = read_source_clock(volume_file, day)

    native = []
    for offset in range(0, len(ids), 48):
        columns = ['node_' + str(i) for i in ids[offset:offset + 48]]
        table = volume_file.read(columns=columns)
        for column in columns:
            values = table.column(column).to_numpy().astype(float)
            if np.any(~np.isfinite(values) | (values < 0)):
                raise ValueError(f'{column} has missing, infinite or negative volume on {day}.')
            native.append(values.tolist())
    for name in static:
        static[name] = [None if x is None else float(x) for x in static[name]]
    facts = {key: {'file': paths[key].name, 'bytes': paths[key].stat().st_size,
                  'rows': file.metadata.num_rows, 'columns': file.metadata.num_columns}
             for key, file in files.items()}
    data = pd.DataFrame(np.asarray(native, dtype=float).T, index=pd.to_datetime(day_clock))
    volume = data.resample('15min').mean().to_numpy(dtype=float)
    return {
        'schema': 'metropolis-flow-native-v1', 'ids': identifiers, 'static': static,
        'times': data.index.to_numpy(dtype='datetime64[s]').astype('int64').tolist(),
        'volume': native, 'source_files': facts,
        'clock': 'Source wall clock. No timezone is declared in the Parquets.',
        'source_summaries': {'flow': [round(float(row.mean()), 2) for row in volume]},
        'frame_values': {'volume': volume.T.tolist()},
        'summary_definition': 'Fifteen-minute means, then means across the selected roads before road-level rounding.'
    }


if __name__ == '__main__':
    directory, ids_file, destination, day = sys.argv[1:]
    with open(ids_file, encoding='utf-8', newline='') as handle:
        identifiers = [row['road_id'] for row in csv.DictReader(handle)]
    result = read_native(Path(directory), identifiers, day)
    with open(destination, 'x', encoding='utf-8') as handle:
        json.dump(result, handle, allow_nan=False, separators=(',', ':'))
