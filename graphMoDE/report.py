#!/usr/bin/env python3
"""Read-only result assembly; never fits, retries, filters by precision, or reads chains."""
from pathlib import Path
import argparse, csv, hashlib, json, math, time

METHODS = ('Exp-W', 'SE-W', 'C', 'EucMoDE', 'MoDE', 'PottsMoDE', 'STGNN', 'PNAR')
METRICS = ('ARI', 'Khat', 'Kocc_mean', 'RMSE', 'boundary_errors', 'internal_cuts',
           'boundary_merge_probability', 'internal_cut_probability', 'max_psm_rms')

def read_json(path):
    return json.loads(path.read_text()) if path.exists() else {}

def read_csv(path):
    if not path.exists(): return []
    with path.open(newline='') as stream: return list(csv.DictReader(stream))

def truth(value):
    return str(value).lower() == 'true'

def finite(value):
    try: return math.isfinite(float(value))
    except (TypeError, ValueError): return False

def merge_rows(entries):
    """Select a whole result row; an unavailable later source cannot erase metrics.

    Completeness is independent of precision: a completed, nonconverged fit is
    retained. All source rows and read failures remain auditable in the manifest.
    """
    def score(entry):
        row = entry['row']
        required = ('RMSE',) if row['method'] == 'PNAR' else ('RMSE', 'ARI', 'Khat', 'Kocc_mean')
        usable = row.get('status') not in ('failed', 'unavailable')
        return (usable and all(finite(row.get(k)) for k in required),
                usable and finite(row.get('RMSE')),
                sum(finite(row.get(k)) for k in METRICS) if usable else -1)
    # Stable ties retain the earlier complete result rather than replacing it.
    selected = max(entries, key=score)
    row = dict(selected['row'], source_table=selected['table'])
    diagnostic_rows = [e['row'] for e in entries
                       if e['row'].get('status') not in ('failed', 'unavailable')
                       and str(e['row'].get('valid')).lower() in ('true', 'false')]
    row['diagnostic_failure_recorded'] = any(not truth(r['valid']) for r in diagnostic_rows)
    row['diagnostic_observed'] = bool(diagnostic_rows)
    row['diagnostic_conflict'] = len({truth(r['valid']) for r in diagnostic_rows}) > 1
    if diagnostic_rows:
        # Contradictory later flags must never turn an observed failed gate into a pass.
        row['valid'] = 'FALSE' if row['diagnostic_failure_recorded'] else 'TRUE'
        if row.get('status') in ('precision_passed', 'precision_failed'):
            row['status'] = 'precision_failed' if row['diagnostic_failure_recorded'] else 'precision_passed'
    issues = [dict(table=e['table'], status=e['row'].get('status'),
                   failed_jobs=e['row'].get('failed_jobs'), error_file=e.get('error_file'))
              for e in entries if e['row'].get('status') in ('failed', 'unavailable') or e.get('error_file')]
    row['source_error'] = bool(issues)
    row['source_issues'] = json.dumps(issues, separators=(',', ':'))
    row['source_tables'] = json.dumps([e['table'] for e in entries], separators=(',', ':'))
    return row, dict(selected_table=selected['table'], sources=entries,
                     policy='whole completed row preferred; stable ties; observed precision failures retained')

def timing(directory, dataset, method, continuation=False):
    """Sum monotonic process durations, including failed tuning cells and retained prefixes.

    This is aggregate process time, not parallel wall time or accepted-draw-only time.
    Continuation prefix attempts include startup and any uncheckpointed tail before stop.
    """
    reg = read_json(directory/'registration.json')
    jobs = reg.get('plan', {}).get('jobs', reg.get('jobs', []))
    if not jobs: jobs = read_csv(directory/'jobs.csv')
    totals = dict(final=0., validation=0., selection=0.)
    counts = dict(final=0, validation=0, selection=0)
    missing = []; evidence = []
    def add(folder, job_id, kind):
        paths = [folder/'jobs'/job_id/name for name in ('process.json', 'process-4000.json')]
        path = next((p for p in paths if p.exists()), paths[0])
        value = read_json(path).get('seconds')
        if isinstance(value, (int, float)) and math.isfinite(value) and value >= 0:
            totals[kind] += value; counts[kind] += 1
            evidence.append(str(path))
        else: missing.append(str(path))
    for job in jobs:
        if (job['dataset'], job['method']) != (dataset, method): continue
        kind = job.get('kind', 'bayes')
        group = 'validation' if kind.endswith('_validation') else 'selection' if kind == 'selection' else 'final'
        add(directory, job['id'], group)
        anchor = reg.get('anchors', {}).get(job['id'], {})
        if continuation and anchor.get('iteration', 0) > 0:
            add(Path(reg['parent']), job['id'], 'final')
    return dict(final_process_seconds=totals['final'] if counts['final'] else '',
                validation_process_seconds=totals['validation'], selection_process_seconds=totals['selection'],
                total_recorded_process_seconds=sum(totals.values()) if sum(counts.values()) else '',
                timing_complete=bool(counts['final']) and not missing, timing_missing_records=len(missing),
                timing_basis='sum_monotonic_process_seconds_including_failed_attempts_and_retained_prefixes'), evidence, missing

def assemble(supplement, output, core=None):
    supplement = Path(supplement); output = Path(output)
    reg = read_json(supplement/'registration.json')
    roots = {m: supplement for m in METHODS}
    tables = []
    if core is not None:
        core = Path(core)
        roots.update({m: core for m in METHODS[:3]})
        tables.append(core/'summary-4000/fits.csv')
    for key, folder in (reg.get('core_results') or {}).items():
        folder = Path(folder)
        roots.update({m: folder for m in (('C',) if key == 'C' else ('Exp-W', 'SE-W'))})
        tables.append(folder/'summary-4000/fits.csv')
    # The supplementary evaluator contains full metrics for split core continuations.
    tables.append(supplement/'summary/fits.csv')
    indexed = {}; sources = {}; merge_evidence = {}
    for path in dict.fromkeys(tables):
        if path.exists(): sources[str(path)] = hashlib.sha256(path.read_bytes()).hexdigest()
        for row in read_csv(path):
            key = (row['dataset'], row['method'])
            entry = dict(table=str(path), row=row)
            error_file = path.parent/(row['dataset']+'-'+row['method']+'-unavailable.json')
            if error_file.exists():
                entry.update(error_file=str(error_file), error=read_json(error_file))
            indexed.setdefault(key, []).append(entry)
    rows = []; time_sources = {}; unavailable = []
    for dataset in ('D01', 'D02'):
        for method in METHODS:
            entries = indexed.get((dataset, method), [])
            row, evidence = merge_rows(entries) if entries else ({}, dict(sources=[]))
            merge_evidence[dataset+'-'+method] = evidence
            row.update(dataset=dataset, method=method)
            for name in METRICS: row.setdefault(name, '')
            available = finite(row['RMSE'])
            bayes = method not in ('STGNN', 'PNAR')
            diagnostic = ('failed' if row['diagnostic_failure_recorded'] else 'passed') if bayes and row.get('diagnostic_observed') else 'not_available' if bayes else 'not_applicable'
            row['diagnostic_status'] = diagnostic
            row['result_status'] = 'metrics_unavailable' if not available else 'provisional_precision_failed' if diagnostic == 'failed' else 'provisional_exploratory'
            row['provisional'] = True
            row['interpretation'] = 'Two-dataset exploratory result; no confirmed superiority. Failed precision gates remain binding.'
            row.setdefault('status', 'precision_'+diagnostic if bayes and diagnostic in ('passed','failed') else 'unavailable')
            row['clustering_status'] = 'not_applicable_PNAR_has_no_partition' if method == 'PNAR' else 'available' if row['ARI'] not in ('', 'NA', 'NaN') else 'unavailable'
            if method == 'PNAR':
                for name in ('ARI', 'Khat', 'Kocc_mean'): row[name] = 'NA'
            timing_values, evidence, missing = timing(roots[method], dataset, method, method in ('Exp-W', 'SE-W'))
            row.update(timing_values); time_sources[dataset+'-'+method] = dict(records=evidence, missing=missing)
            if not available: unavailable.append([dataset, method])
            rows.append(row)
    output.mkdir(parents=True, exist_ok=True)
    fields = list(dict.fromkeys(key for row in rows for key in row))
    with (output/'all-methods.csv').open('x', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields); writer.writeheader(); writer.writerows(rows)
    manifest = dict(created=time.time(), rows=len(rows), expected_rows=16, metrics_unavailable=unavailable,
                    input_tables_sha256=sources, merge_evidence=merge_evidence,
                    timing_sources=time_sources, automatic8000=False,
                    precision_filter_applied=False, fitting_or_diagnostic_rerun=False,
                    reporter_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest())
    with (output/'report-manifest.json').open('x') as stream: json.dump(manifest, stream, indent=2)
    return manifest

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--supplement', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True, help='Report files must not already exist')
    parser.add_argument('--core', type=Path)
    args = parser.parse_args()
    assemble(args.supplement, args.output, args.core)
