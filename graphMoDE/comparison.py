#!/usr/bin/env python3
"""Register and queue five supplementary methods on two existing blinded datasets."""
from pathlib import Path
import argparse, csv, fcntl, hashlib, importlib.metadata, importlib.util, json, os, shutil, signal, subprocess, sys, time

ROOT = Path(__file__).resolve().parent
ENV = dict(os.environ, LC_ALL='C', OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1', MKL_NUM_THREADS='1', VECLIB_MAXIMUM_THREADS='1', PYTHONDONTWRITEBYTECODE='1')

def publish(path, value):
    tmp=path.with_name(path.name+'.writing');tmp.write_text(json.dumps(value,indent=2,allow_nan=False)+'\n');tmp.replace(path)

def sha(path): return hashlib.sha256(Path(path).read_bytes()).hexdigest()

def runtime():
    return dict(python=sys.version,packages=sorted([d.metadata['Name'],d.version] for d in importlib.metadata.distributions()))

def accept(path): path.with_name(path.name+'.sha256').write_text(sha(path)+'\n')

def read(path):
    if sha(path)!=path.with_name(path.name+'.sha256').read_text().strip(): raise RuntimeError('Changed accepted artifact: '+str(path))
    return json.loads(path.read_text())

def command(*args): return ['Rscript','--vanilla',str(ROOT/'comparison.R'),*map(str,args)]

def plan():
    seeds={};jobs=[];value=2031104500
    def seed(key):
        nonlocal value
        value+=1;seeds[key]=value;return value
    def add(d,kind,method,chain=0,candidate='',origin=0,restart=0,dependencies=None):
        identity=f'{d}-{kind}-{method}-{candidate}-o{origin}-c{chain}-r{restart}'
        jobs.append(dict(id=identity,dataset=d,kind=kind,method=method,chain=chain,candidate=candidate,origin=origin,restart=restart,
                         seed=seed(identity) if kind in ('bayes','potts_validation','stgnn_validation','stgnn_final') else 0,
                         dependencies=','.join(dependencies or [])))
        return identity
    for d in ('D01','D02'):
        for origin in (96,120):
            for chain in (1,3):
                for component in ('Z','theta'):seed(f'{d}-potts-{origin}-{component}_{chain:02}')
        deps={m:[] for m in ('PottsMoDE','STGNN','PNAR')}
        for beta in (.25,1,4):
            for origin in (96,120):
                for chain in (1,3): deps['PottsMoDE'].append(add(d,'potts_validation','PottsMoDE',chain=chain,candidate=f'beta{beta}',origin=origin))
        for p in (1,2,6,24):
            for origin in (96,120):deps['PNAR'].append(add(d,'pnar_validation','PNAR',candidate=f'p{p}',origin=origin,restart=1))
        for k in (3,5,7,10):
            for pool in (.1,1):
                for origin in (96,120):
                    for restart in (1,2):deps['STGNN'].append(add(d,'stgnn_validation','STGNN',candidate=f'K{k}-pool{pool}',origin=origin,restart=restart))
        selected={m:add(d,'selection',m,dependencies=dep) for m,dep in deps.items()}
        for chain in range(1,5):
            for method in ('EucMoDE','MoDE','PottsMoDE'):
                add(d,'bayes',method,chain=chain,dependencies=[selected[method]] if method=='PottsMoDE' else [])
        add(d,'pnar_final','PNAR',dependencies=[selected['PNAR']])
        for restart in range(1,5):add(d,'stgnn_final','STGNN',restart=restart,dependencies=[selected['STGNN']])
    return dict(version='two-data-eight-method-supplement-20261015',seeds=seeds,jobs=jobs,
                iterations=4000,warmup=2000,automatic8000=False,max_workers=6,numeric_threads=1,
                potts_validation=dict(beta=[.25,1,4],origins=[96,120],chains=[1,3],iterations=600,warmup=300,seconds_cap=1800),
                STGNN=dict(validation_epochs=300,validation_every=25,tuning_restarts=2,final_restarts=4,training_seconds_cap=600),
                PNAR=dict(p=[1,2,6,24],maxeval=1000,xtol_rel=1e-8,seconds_cap=300),
                operational_limits=dict(min_free_GiB=12,combined_output_GiB=30,hours=72),
                role='Two frozen new data sets; all eight methods. No new DGP, no discarded failed fits, no automatic extension.')

def verify(reg):
    for name,h in reg['source_sha256'].items():
        if sha(ROOT/name)!=h:raise RuntimeError('Frozen source changed: '+name)
    if sha(reg['external']['pnar'])!=reg['external_lock']['PNAR']['sha256']:raise RuntimeError('PNAR changed')
    for name,h in reg['external_lock']['STGNN']['files'].items():
        if sha(Path(reg['external']['stgnn'])/name)!=h:raise RuntimeError('STGNN changed')

def neural(out,index):
    import numpy as np
    reg=read(out/'registration.json');verify(reg)
    if runtime()!=reg['python_runtime']:raise RuntimeError('Registered Python runtime changed')
    job=reg['plan']['jobs'][index-1];path=out/'jobs'/job['id']
    data=read(out/job['dataset']/'training.json')
    if data['signature']!=reg['signature']:raise RuntimeError('Wrong training identity')
    Y=np.asarray(data['Y'],dtype='float64');W=np.asarray(data['weight'],dtype='float64')
    if Y.shape!=(168,121) or W.shape!=(121,121):raise RuntimeError('Wrong input shape')
    if job['kind']=='stgnn_validation':
        a,b=job['candidate'].split('-pool');K=int(a[1:]);pool=float(b);epochs=300
        train=Y[:job['origin']];validation=Y[job['origin']:job['origin']+48]
    else:
        selected=read(out/job['dataset']/'selection-STGNN.json')['selected']
        if not selected:raise RuntimeError('No complete valid STGNN candidate; no fallback')
        selected=selected[0];K=int(selected['K']);pool=float(selected['pool']);epochs=int(selected['epoch'])
        train=Y;validation=None
    spec=importlib.util.spec_from_file_location('stgnn_adapter',ROOT/'scripts/stgnn_adapter.py')
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
    scores=[]
    def checkpoint(epoch,prediction,loss):
        if validation is not None:
            row=dict(epoch=epoch,sse=float(np.square(prediction.astype('float64')-validation).sum()),n=5808,valid=True);scores.append(row)
            with (path/f'validation-epoch-{epoch:03}.npz').open('xb') as f:np.savez_compressed(f,prediction=prediction)
            publish(path/f'validation-epoch-{epoch:03}.json',row)
        publish(path/'progress.json',dict(epoch=epoch,epochs=epochs,loss=loss));print(job['id'],epoch,'/',epochs,flush=True)
    try:
        result=module.train(train,W,author_directory=reg['external']['stgnn'],author_hashes=reg['external_lock']['STGNN']['files'],
                            clusters=K,pool=pool,seed=job['seed'],epochs=epochs,seconds_cap=600,checkpoint_callback=checkpoint)
        import torch
        with (path/'model.pt').open('xb') as f:torch.save(result.pop('state'),f)
        accept(path/'model.pt');result['prediction']=result['prediction'].tolist()
        result.update(scores=scores,signature=reg['signature'],job_id=job['id'],selected_K=K,selected_pool=pool,epochs=epochs)
        publish(path/'result.json',result);accept(path/'result.json')
    except BaseException as error:
        if hasattr(error,'c8_failure'):
            import torch
            with (path/'failure-state.pt').open('xb') as f:torch.save(error.c8_failure,f)
        publish(path/'failure.json',dict(error=repr(error),scores=scores,signature=reg['signature']));raise

def processes():
    rows=[]
    for line in subprocess.check_output(['/bin/ps','-axo','pid=,stat=,pcpu=,command='],text=True).splitlines():
        fields=line.strip().split(None,3)
        if len(fields)==4:rows.append(dict(pid=int(fields[0]),state=fields[1],cpu=float(fields[2]),command=fields[3]))
    return rows

def gate(reg,rows):
    for item in reg.get('queue_after',[]):
        if any(r['pid']==item['pid'] and item['command_contains'] in r['command'] and 'Z' not in r['state'] for r in rows):return False
    return True

def capacity(rows,active):
    others=[r for r in rows if r['pid'] not in active and r['pid']!=os.getpid() and 'Z' not in r['state'] and
            (Path(r['command'].split()[0]).name in ('R','Rscript') or ('python' in Path(r['command'].split()[0]).name.lower() and r['cpu']>=50))]
    return max(0,6-len(active)-len(others))

def resources(reg,out,started):
    if shutil.disk_usage(out).free<12*2**30:raise RuntimeError('Free disk below 12 GiB reserve')
    total=0
    for folder in [out,*map(Path,reg.get('combined_outputs',[]))]:
        for f in folder.rglob('*'):
            try:
                if f.is_file():total+=f.stat().st_size
            except FileNotFoundError:pass
    if total>30*2**30:raise RuntimeError('Combined output exceeded 30 GiB')
    if time.monotonic()-started>72*3600:raise RuntimeError('72-hour monotonic ceiling')
    return dict(free_GiB=shutil.disk_usage(out).free/2**30,output_GiB=total/2**30)

def pause(active,out,error):
    records=[]
    for pid,e in active.items():
        status='already_exited'
        if e['process'].poll() is None:
            try:os.kill(pid,signal.SIGSTOP);status='suspended_in_memory'
            except ProcessLookupError:status='exited_during_pause'
            except OSError as exc:status='pause_failed: '+repr(exc)
        e['stream'].close();records.append(dict(pid=pid,job=e['job']['id'],status=status))
    publish(out/'suspended.json',dict(error=repr(error),workers=records,automatic_resume=False))

def queued_launch(out,configuration):
    """Freeze intent now; perform bounded preflight and R registration when a slot is free."""
    cfg=json.loads(configuration.read_text());side=out.with_name(out.name+'.queue')
    if out.exists():raise RuntimeError('Output already exists; no retry')
    side.mkdir();lock=(side/'lock').open('a+');fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
    files={str(f.relative_to(ROOT)):sha(f) for f in ROOT.rglob('*') if f.is_file() and '.git' not in f.relative_to(ROOT).parts and '__pycache__' not in f.parts and f.suffix!='.pyc'}
    manifest=dict(pid=os.getpid(),created=time.time(),source_sha256=files,config_sha256=sha(configuration),plan_sha256=sha(cfg['plan_file']),
                  output=str(out),configuration=str(configuration),queue_after=cfg['queue_after'],stage='preflight_pending',automatic8000=False)
    publish(side/'registration.json',manifest)
    keeper=subprocess.Popen(['/usr/bin/caffeinate','-is','-w',str(os.getpid())]) if sys.platform=='darwin' else None
    active={};started=time.monotonic()
    try:
        while True:
            if sha(configuration)!=manifest['config_sha256'] or sha(cfg['plan_file'])!=manifest['plan_sha256']:raise RuntimeError('Queue configuration changed')
            for name,h in files.items():
                if sha(ROOT/name)!=h:raise RuntimeError('Queued source changed: '+name)
            rows=processes();stats=resources(cfg,side,started)
            if gate(cfg,rows) and capacity(rows,{})>=1:break
            publish(side/'progress.json',dict(phase='queued_before_preflight',elapsed=time.monotonic()-started,resources=stats,checked=time.time()))
            time.sleep(10)
        checks=[['Rscript','--vanilla',str(ROOT/'tests/comparison.R')],
                [cfg['external']['python'],str(ROOT/'tests/external_import.py'),str(configuration)],
                command('register',out,configuration)]
        for i,args in enumerate(checks,1):
            while capacity(processes(),{})<1:resources(cfg,side,started);time.sleep(5)
            stream=(side/f'preflight-{i}.log').open('xb');p=subprocess.Popen(args,env=ENV,stdout=stream,stderr=subprocess.STDOUT)
            active[p.pid]=dict(process=p,stream=stream,job={'id':f'preflight-{i}'},path=side,started=time.monotonic())
            while p.poll() is None:resources(cfg,side,started);time.sleep(2)
            stream.close();active.clear();publish(side/f'preflight-{i}.json',dict(command=args,exit_code=p.returncode,finished=time.time()))
            if p.returncode:raise RuntimeError(f'Preflight/registration {i} failed; no supplementary science launched')
        publish(side/'ready.json',dict(output=str(out),finished=time.time(),preflight_passed=True))
        if keeper:keeper.terminate();keeper.wait();keeper=None
        supervise(out)
    except BaseException as error:
        if active:pause(active,side,error)
        publish(side/'failure.json',dict(error=repr(error),time=time.time(),scientific_output_exists=out.exists()));raise
    finally:
        if keeper:keeper.terminate();keeper.wait()

def supervise(out):
    reg=read(out/'registration.json');verify(reg)
    lock=(out/'supervisor.lock').open('a+');fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
    with (out/'controller.json').open('x') as f:json.dump(dict(pid=os.getpid(),started=time.time(),command=sys.argv,registration=reg['signature']),f,indent=2)
    keeper=subprocess.Popen(['/usr/bin/caffeinate','-is','-w',str(os.getpid())]) if sys.platform=='darwin' else None
    active={};done={};pending=list(enumerate(reg['plan']['jobs'],1));started=time.monotonic();events=(out/'events.jsonl').open('x',buffering=1)
    try:
        while pending or active:
            stats=resources(reg,out,started);verify(reg)
            for pid,e in list(active.items()):
                rc=e['process'].poll()
                if rc is None:continue
                e['stream'].close();publish(e['path']/'process.json',dict(exit_code=rc,seconds=time.monotonic()-e['started'],pid=pid))
                done[e['job']['id']]=rc;del active[pid]
            rows=processes();released=gate(reg,rows);slots=capacity(rows,active) if released else 0
            while slots>0:
                ready=next(((i,j) for i,j in pending if all(x in done for x in j['dependencies'].split(',') if x)),None)
                if ready is None:break
                index,job=ready;pending.remove(ready);path=out/'jobs'/job['id'];path.mkdir()
                stream=(path/'worker.log').open('xb')
                args=[reg['external']['python'],str(ROOT/'comparison.py'),'neural',str(out),'--index',str(index)] if job['kind'].startswith('stgnn_') else command('worker',out,index)
                process=subprocess.Popen(args,env=ENV,stdout=stream,stderr=subprocess.STDOUT)
                active[process.pid]=dict(process=process,stream=stream,job=job,path=path,started=time.monotonic())
                events.write(json.dumps(dict(event='launch',job=job['id'],pid=process.pid,time=time.time(),command=args))+'\n')
                slots=capacity(processes(),active)
            publish(out/'progress.json',dict(phase='running' if released else 'queued_behind_current_C_W',completed=len(done),failed=[k for k,v in done.items() if v],
                    active={str(k):v['job']['id'] for k,v in active.items()},queued=len(pending),target=4000,automatic8000=False,resources=stats,seconds=time.monotonic()-started))
            if pending or active:time.sleep(5)
        # Summaries retain explicit failed-method rows; they never launch a retry.
        while capacity(processes(),{})<1:resources(reg,out,started);time.sleep(5)
        path=out/'summary';path.mkdir();stream=(path/'summary.log').open('xb')
        p=subprocess.Popen(command('summary',out),env=ENV,stdout=stream,stderr=subprocess.STDOUT)
        active[p.pid]=dict(process=p,stream=stream,job={'id':'summary'},path=path,started=time.monotonic())
        while p.poll() is None:resources(reg,out,started);time.sleep(5)
        stream.close();active.clear()
        # Reporting only: retain precision failures, timings and unavailable rows.
        report=subprocess.run([sys.executable,str(ROOT/'report.py'),'--supplement',str(out),'--output',str(out/'reporting')],env=ENV)
        publish(out/'completion.json',dict(scientific_jobs_terminated=True,failed=[k for k,v in done.items() if v],summary_exit_code=p.returncode,report_exit_code=report.returncode,
                target=4000,automatic8000=False,seconds=time.monotonic()-started,finished=time.time()))
    except BaseException as error:
        pause(active,out,error);raise
    finally:
        events.close()
        if keeper:keeper.terminate();keeper.wait()

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('action',choices=['plan','register','queue','run','neural','runtime'])
    p.add_argument('path',nargs='?',type=Path);p.add_argument('--config',type=Path);p.add_argument('--index',type=int);a=p.parse_args()
    if a.action=='plan':
        with a.path.open('x') as f:json.dump(plan(),f,indent=2)
    elif a.action=='runtime':print(json.dumps(runtime()))
    elif a.action=='register':subprocess.run(command('register',a.path.resolve(),a.config.resolve()),env=ENV,check=True)
    elif a.action=='queue':queued_launch(a.path.resolve(),a.config.resolve())
    elif a.action=='run':supervise(a.path.resolve())
    elif a.action=='neural':neural(a.path.resolve(),a.index)

if __name__=='__main__':main()
