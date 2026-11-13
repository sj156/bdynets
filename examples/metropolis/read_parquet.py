"""Read selected raw five-minute roads and their unrounded speed calibration.

Arguments: raw directory, one-column road-ID CSV, output JSON, YYYY-MM-DD.
The caller supplies a temporary destination; this program never updates its inputs.
"""
import csv
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import math
import pyarrow.parquet as pq


def read_native(directory, identifiers, day, minutes=None, calibration_file=None):
    paths = {name: directory / ('city-traffic-M-raw-' + name + '.parquet')
             for name in ('static-features', 'volume', 'speed')}
    files = {key: pq.ParquetFile(path) for key, path in paths.items()}
    ids = [int(value) for value in identifiers]
    if not ids or len(set(ids)) != len(ids) or min(ids) < 0:
        raise ValueError('Road IDs must be distinct zero-based static-row identifiers.')
    static_fields = ['x_coordinate_start', 'y_coordinate_start', 'x_coordinate_end',
                     'y_coordinate_end', 'length', 'region_id', 'category', 'edge_type', 'speed_limit']
    static = files['static-features'].read(columns=static_fields).take(ids).to_pydict()
    clocks = {}
    for key in ('volume', 'speed'):
        field_type = files[key].schema_arrow.field('timestamp').type
        if getattr(field_type, 'tz', None) is not None:
            raise ValueError('This source reader expects timezone-unspecified release clocks.')
        raw = files[key].read(columns=['timestamp']).column('timestamp').to_numpy()
        clock = raw.astype('datetime64[s]')
        if np.any(np.isnat(clock)) or np.any(raw != clock):
            raise ValueError(key + ' clocks contain missing times or unsupported fractional seconds.')
        if len(np.unique(clock)) != len(clock) or np.any(np.diff(clock).astype('timedelta64[s]').astype(int) != 300):
            raise ValueError(key + ' has duplicate or nonconsecutive five-minute clocks.')
        if np.any(clock.astype(int) % 300):
            raise ValueError(key + ' clocks are off the five-minute grid.')
        clocks[key] = clock
    if not np.array_equal(clocks['volume'], clocks['speed']):
        raise ValueError('Speed and volume source timestamps differ.')
    selected = clocks['volume'].astype('datetime64[D]') == np.datetime64(day)
    if selected.sum() != 288 or str(clocks['volume'][selected][0]) != day + 'T00:00:00':
        raise ValueError('The selected date must contain one complete day.')
    if minutes is not None:
        minute = np.asarray(minutes, dtype=int)
        if (not len(minute) or np.any(np.diff(minute) != 5) or minute[0] % 15 or
                len(minute) % 3 or minute[0] < 0 or minute[-1] > 1435):
            raise ValueError('The source excerpt needs complete aligned groups of three five-minute slots.')
        source_minutes = (clocks['volume'].astype(int) % 86400) // 60
        selected &= np.isin(source_minutes, minute)
        if selected.sum() != len(minute):
            raise ValueError('The requested source excerpt is incomplete.')
    native = {'volume': [], 'speed': []}
    baseline = []
    peaks = []
    for offset in range(0, len(ids), 48):
        columns = ['node_' + str(i) for i in ids[offset:offset + 48]]
        for metric in ('volume', 'speed'):
            table = files[metric].read(columns=columns)
            for column in columns:
                values = table.column(column).to_numpy().astype(float)
                if np.any(np.isinf(values)) or np.any(values < 0):
                    raise ValueError(metric + ' contains a negative or infinite observation.')
                native[metric].append([None if np.isnan(x) else float(x) for x in values[selected]])
                observed = values[np.isfinite(values)]
                if metric == 'speed':
                    positive = observed[observed > 0]
                    baseline.append(float(np.quantile(positive, .9)) if len(positive) else 0.)
                else:
                    peaks.append(float(np.max(observed)) if len(observed) else None)
    for name in static:
        static[name] = [None if x is None else float(x) for x in static[name]]
    facts = {key: {'file': paths[key].name, 'bytes': paths[key].stat().st_size,
                  'rows': file.metadata.num_rows, 'columns': file.metadata.num_columns}
             for key, file in files.items()}
    result = {'schema': 'metropolis-native-v1', 'ids': identifiers, 'static': static,
              'labels': [str(x).replace('T', ' ')[:16] for x in clocks['volume'][selected]],
              'volume': native['volume'], 'speed': native['speed'], 'free_flow': baseline,
              'max_volume': peaks, 'source_files': facts,
              'clock': 'Source wall clock. No timezone is declared in the Parquets.'}
    if calibration_file is not None:
        with open(calibration_file, encoding='utf-8', newline='') as handle:
            rows = list(csv.DictReader(handle))
        if [row['road_id'] for row in rows] != list(identifiers):
            raise ValueError('Reference calibration IDs do not match the selected source order.')
        reference = np.asarray([float(row['full_history_nonzero_speed_p90']) for row in rows])
        if not np.allclose(reference, baseline, rtol=2e-15, atol=0.):
            raise ValueError('Reference speed calibration does not match the original full history.')
        p95 = {float(row['selected_day_frame_volume_p95']) for row in rows}
        if len(p95) != 1 or not np.isfinite(next(iter(p95))) or next(iter(p95)) < 1:
            raise ValueError('The full-reference frame-volume calibration is invalid.')
        result['calibration_p95'] = next(iter(p95))
    if minutes is not None:
        return result
    # The original map uses pandas quarter means/fill and NumPy summaries before
    # rounding individual road values. Keep those supplied source summaries.
    frames = {}
    for metric in ('volume', 'speed'):
        data = pd.DataFrame(np.asarray(native[metric], dtype=float).T,
                            index=pd.to_datetime(result['labels']))
        frames[metric] = data.resample('15min').mean().interpolate(limit_direction='both').fillna(0.)
    volume = frames['volume'].to_numpy(dtype=float)
    speed = frames['speed'].to_numpy(dtype=float)
    p95 = result.get('calibration_p95', max(1., float(np.percentile(volume, 95))))
    with np.errstate(divide='ignore', invalid='ignore'):
        slowdown = np.nan_to_num(1. - speed / np.asarray(baseline), nan=0., posinf=0., neginf=0.)
    congestion = np.clip(slowdown, 0., 1.) * np.clip(np.log1p(volume) / math.log1p(p95), 0., 1.)
    result['source_summaries'] = {
        'flow': [round(float(row.mean()), 2) for row in volume],
        'speed': [round(float(row[v > 0].mean()), 2) if (v > 0).any() else 0.
                  for row, v in zip(speed, volume)],
        'congestion': [round(float(row.mean()), 3) for row in congestion]}
    result['summary_definition'] = 'Original fifteen-minute pandas means and fills, followed by NumPy regional means before road-frame rounding.'
    return result



if __name__ == '__main__':
    directory, ids_file, destination, day = sys.argv[1:5]
    minutes = None if len(sys.argv) < 6 or sys.argv[5] == '-' else [int(x) for x in sys.argv[5].split(',')]
    calibration_file = None if len(sys.argv) < 7 or sys.argv[6] == '-' else sys.argv[6]
    with open(ids_file, encoding='utf-8', newline='') as handle:
        identifiers = [row['road_id'] for row in csv.DictReader(handle)]
    result = read_native(Path(directory), identifiers, day, minutes, calibration_file)
    with open(destination, 'x', encoding='utf-8') as handle:
        json.dump(result, handle, allow_nan=False, separators=(',', ':'))
