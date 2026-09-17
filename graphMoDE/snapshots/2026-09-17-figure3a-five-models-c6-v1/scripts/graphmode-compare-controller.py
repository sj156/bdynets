#!/usr/bin/env python3
"""One-shot supervised science + diagnosis; wall-clock caps and live terminal log."""
import json
import os
from pathlib import Path
import selectors
import shutil
import signal
import subprocess
import sys
import time


def publish(path, value):
    temporary = path.with_suffix(path.suffix + '.writing')
    temporary.write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n')
    temporary.replace(path)


def phase(command, seconds, log):
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               start_new_session=True, bufsize=0)
    watch = selectors.DefaultSelector()
    watch.register(process.stdout, selectors.EVENT_READ)
    start = time.monotonic()
    try:
        while watch.get_map():
            if time.monotonic() - start > seconds:
                raise TimeoutError(f'Phase exceeded registered {seconds}s wall-clock cap')
            for key, _ in watch.select(timeout=1):
                data = os.read(key.fd, 65536)
                if not data:
                    watch.unregister(key.fileobj)
                else:
                    log.write(data); log.flush()
                    sys.stdout.buffer.write(data); sys.stdout.buffer.flush()
        code = process.wait(timeout=max(1, seconds - (time.monotonic() - start)))
        if code:
            raise RuntimeError(f'R process exited {code}')
        return time.monotonic() - start
    finally:
        watch.close()
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()


def main():
    if len(sys.argv) != 3 or sys.argv[2] != '--authorized':
        raise SystemExit('Usage: graphmode-compare-controller.py REGISTRATION.rds --authorized')
    registration = Path(sys.argv[1]).resolve(strict=True)
    directory = registration.parent
    root = Path(__file__).resolve().parents[1]
    with (directory / 'controller-request.json').open('x') as handle:
        json.dump({'started': time.time(), 'pid': os.getpid(), 'science_cap': 14400,
                   'diagnostic_cap': 1800, 'registration': str(registration)}, handle)
    os.environ.update({name: '1' for name in ('OMP_NUM_THREADS', 'OPENBLAS_NUM_THREADS',
                                             'MKL_NUM_THREADS', 'VECLIB_MAXIMUM_THREADS')})
    os.environ['LC_ALL'] = 'C'
    keeper = subprocess.Popen(['/usr/bin/caffeinate', '-is', '-w', str(os.getpid())])
    current = 'science'
    try:
        with (directory / 'controller.log').open('xb') as log:
            durations = {}
            for current, cap in [('science', 14400), ('diagnose', 1800)]:
                command = [shutil.which('Rscript'), '--vanilla', str(root / 'scripts/graphmode-compare-run.R'),
                           current, str(registration), '--authorized']
                durations[current] = phase(command, cap, log)
            if not (directory / 'completion.json').is_file():
                raise RuntimeError('Process ended without completion receipt')
            publish(directory / 'controller-complete.json', {'completed': time.time(), 'seconds': durations})
    except BaseException as error:
        if not (directory / 'failure.json').exists():
            publish(directory / 'failure.json', {'status': 'failed', 'phase': current,
                                                'error': str(error), 'time': time.time()})
        raise
    finally:
        keeper.terminate()
        keeper.wait()


if __name__ == '__main__':
    main()
