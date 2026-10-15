#!/usr/bin/env python3
"""Portable core-pilot controller. All output is external and append-only by stage."""
import argparse, concurrent.futures, csv, hashlib, json, os, pathlib, shutil, subprocess, sys, time
ROOT = pathlib.Path(__file__).resolve().parent
ENV = dict(os.environ, LC_ALL='C', OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1', MKL_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='1', PYTHONDONTWRITEBYTECODE='1')
R = shutil.which('Rscript')
def cmd(*args):
    if not R: raise RuntimeError('Rscript must be on PATH; no packages are installed automatically.')
    return [R, '--vanilla', str(ROOT/'run.R'), *map(str,args)]
def publish(path,data):
    tmp=path.with_suffix(path.suffix+'.writing')
    tmp.write_text(json.dumps(data,indent=2)+'\n'); os.replace(tmp,path)
def resources(out):
    if shutil.disk_usage(out).free < 12*1024**3: raise RuntimeError('Free disk below frozen 12 GiB reserve.')
    size=sum(f.stat().st_size for f in out.rglob('*') if f.is_file())
    if size>30*1024**3: raise RuntimeError('Output exceeds frozen 30 GiB cap.')
def pilot(out,smoke):
    out=out.resolve()
    if out.exists(): raise RuntimeError('Use a new absent output directory; never retry a registered run.')
    if shutil.disk_usage(out.parent).free<42*1024**3: raise RuntimeError('Need 42 GiB free at startup (30 output + 12 reserve).')
    start=time.monotonic();keep=None
    result=subprocess.run(cmd('register',out,'smoke' if smoke else 'core'),env=ENV,text=True,capture_output=True)
    logpath=out/'registration.log' if out.exists() else out.parent/(out.name+'-registration-failure.log')
    logpath.write_text(result.stdout+result.stderr)
    result.check_returncode()
    reg=json.loads((out/'registration.json').read_text());jobs=list(csv.DictReader((out/'jobs.csv').open()))
    if sys.platform=='darwin': keep=subprocess.Popen(['/usr/bin/caffeinate','-is','-w',str(os.getpid())])
    publish(out/'controller.json',dict(pid=os.getpid(),command=sys.argv,started=time.time(),registration=reg['signature'],max_workers=6))
    try:
        targets=[4] if smoke else [4000,8000]
        for target in targets:
            failed=[];active={};queue=list(enumerate(jobs,1));done=0
            try:
                while queue or active:
                    resources(out)
                    while queue and len(active)<6:
                        index,job=queue.pop(0);path=out/'jobs'/job['id'];path.mkdir(exist_ok=True)
                        stream=(path/f'stage-{target}.log').open('xb')
                        process=subprocess.Popen(cmd('worker',out,index,target),env=ENV,stdout=stream,stderr=subprocess.STDOUT)
                        active[process.pid]=(process,stream,path,time.monotonic())
                    for pid,(process,stream,path,t0) in list(active.items()):
                        rc=process.poll()
                        if rc is None: continue
                        stream.close();publish(path/f'process-{target}.json',dict(exit_code=rc,seconds=time.monotonic()-t0,pid=pid))
                        done+=1;del active[pid]
                        if rc: failed.append(path.name)
                    publish(out/'progress.json',dict(phase='sampling',target=target,completed=done,total=len(jobs),active=len(active),failed=failed,seconds=time.monotonic()-start))
                    if failed: raise RuntimeError('Execution failure; retain evidence and stop without retry: '+','.join(failed))
                    if time.monotonic()-start>72*3600: raise RuntimeError('72-hour operational ceiling reached; no retry.')
                    if active: time.sleep(2)
            finally:
                for process,stream,path,t0 in active.values():
                    process.terminate()
                for process,stream,path,t0 in active.values():
                    try: process.wait(timeout=30)
                    except subprocess.TimeoutExpired: process.kill();process.wait()
                    stream.close();publish(path/f'process-{target}.json',dict(exit_code=process.returncode,status='stopped_after_peer_failure',seconds=time.monotonic()-t0))
            if smoke:
                decision=dict(status='smoke_completed',chains=len(jobs),iterations=4,convergence_certified=False)
                break
            publish(out/'progress.json',dict(phase='diagnosing',target=target,seconds=time.monotonic()-start))
            with (out/f'summary-{target}.log').open('xb') as log:
                subprocess.run(cmd('summary',out,target),env=ENV,stdout=log,stderr=subprocess.STDOUT,check=True)
            decision=json.loads((out/f'summary-{target}'/'decision.json').read_text())
            if decision['all_passed'] or target==8000: break
        publish(out/'completion.json',dict(**decision,seconds=time.monotonic()-start,finished=time.time(),full_eight_method_batch_started=False))
        print(json.dumps(decision),flush=True)
    except BaseException as exc:
        publish(out/'failure.json',dict(error=repr(exc),seconds=time.monotonic()-start,full_eight_method_batch_started=False))
        raise
    finally:
        if keep: keep.terminate();keep.wait()
def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action',choices=['test','example','smoke','core'])
    parser.add_argument('--output',type=pathlib.Path)
    a=parser.parse_args()
    if a.action=='test': subprocess.run(cmd('test'),env=ENV,check=True)
    elif a.action=='example':
        if not a.output: parser.error('--output is required')
        subprocess.run(cmd('example',a.output.resolve()),env=ENV,check=True)
    else:
        if not a.output: parser.error('--output is required')
        pilot(a.output,a.action=='smoke')
if __name__=='__main__': main()
