#!/usr/bin/env python3
"""Portable core-pilot controller. All output is external and append-only by stage."""
import argparse, concurrent.futures, csv, hashlib, json, os, pathlib, shutil, signal, subprocess, sys, time
ROOT = pathlib.Path(__file__).resolve().parent
ENV = dict(os.environ, LC_ALL='C', OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1', MKL_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='1', PYTHONDONTWRITEBYTECODE='1')
R = shutil.which('Rscript')
class ResourceLimit(RuntimeError):
    """A resource or elapsed-time stop; writers must not continue draining."""
def cmd(*args):
    if not R: raise RuntimeError('Rscript must be on PATH; no packages are installed automatically.')
    return [R, '--vanilla', str(ROOT/'run.R'), *map(str,args)]
def publish(path,data):
    tmp=path.with_suffix(path.suffix+'.writing')
    tmp.write_text(json.dumps(data,indent=2)+'\n'); os.replace(tmp,path)
def resources(out):
    if shutil.disk_usage(out).free < 12*1024**3: raise ResourceLimit('Free disk below frozen 12 GiB reserve.')
    size=0
    for f in out.rglob('*'):
        try:
            if f.is_file():size+=f.stat().st_size
        except FileNotFoundError:pass # Atomic publication may rename temporary files.
    if size>30*1024**3: raise ResourceLimit('Output exceeds frozen 30 GiB cap.')
def check_deadline(start):
    if time.monotonic()-start>72*3600: raise ResourceLimit('72-hour operational ceiling reached; no retry.')
def pause_writers(active,out,target,exc):
    """Pause owned writers, preserving memory and accepted disk checkpoints.

    Do not wait for a whole chain after a supervisor/resource failure. A partial
    file without its acceptance checksum is not a checkpoint. Resume requires
    inspection and an explicit decision; this function never sends SIGCONT.
    """
    reason='resource_or_time_limit' if isinstance(exc,ResourceLimit) else 'supervisor_exception'
    intent=dict(status='pausing_writers',reason=reason,error=repr(exc),pids=list(active),target=target,
                automatic_resume=False)
    try: publish(out/f'emergency-{target}.json',intent)
    except OSError: pass # Failure to write must not leave writers running.
    records=[]
    for pid,(process,stream,path,t0) in list(active.items()):
        record=dict(pid=pid,job=path.name,reason=reason)
        try:
            if process.poll() is None:
                os.kill(pid,signal.SIGSTOP);record['status']='suspended_in_memory'
            else: record.update(status='already_exited',exit_code=process.returncode)
        except ProcessLookupError: record['status']='exited_during_pause'
        except OSError as error: record.update(status='pause_failed',error=repr(error))
        finally:
            stream.close()
        checkpoints=sorted(path.glob('chunk-*.rds.sha256'))
        record['latest_accepted_checkpoint']=checkpoints[-1].name[:-7] if checkpoints else None
        records.append(record)
    active.clear()
    publish(out/f'suspended-{target}.json',dict(intent,status='writers_pause_attempted',workers=records))
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
        targets=[4] if smoke else [4000]
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
                    check_deadline(start)
                    if active: time.sleep(2)
            except BaseException as exc:
                pause_writers(active,out,target,exc)
                raise
            if failed: raise RuntimeError('Execution failures retained; valid peers were allowed to finish: '+','.join(failed))
            if smoke:
                decision=dict(status='smoke_completed',chains=len(jobs),iterations=4,convergence_certified=False)
                break
            publish(out/'progress.json',dict(phase='diagnosing',target=target,seconds=time.monotonic()-start))
            with (out/f'summary-{target}.log').open('xb') as log:
                process=subprocess.Popen(cmd('summary',out,target),env=ENV,stdout=log,stderr=subprocess.STDOUT)
                diagnostic={process.pid:(process,log,out/f'summary-{target}',time.monotonic())}
                try:
                    while process.poll() is None:
                        resources(out);check_deadline(start);time.sleep(2)
                    if process.returncode: raise subprocess.CalledProcessError(process.returncode,cmd('summary',out,target))
                except BaseException as exc:
                    pause_writers(diagnostic,out,target,exc)
                    raise
            decision=json.loads((out/f'summary-{target}'/'decision.json').read_text())
            decision['automatic_extension']=False
            decision['decision']='4000 stage completed; any extension to8000 requires an explicit decision.'
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
