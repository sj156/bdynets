#!/usr/bin/env python3
"""Fresh eight-method experiment: register, execute dependencies, retain failures."""
from pathlib import Path
import argparse, concurrent.futures, datetime, hashlib, importlib.util, json, os, re, shutil, subprocess, sys, time
ENV=dict(os.environ,LC_ALL='C',OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1',PYTHONDONTWRITEBYTECODE='1')
def sha(p):return hashlib.sha256(Path(p).read_bytes()).hexdigest()
def put(p,x):
 p=Path(p);q=p.with_suffix(p.suffix+'.writing');q.write_text(json.dumps(x,indent=2,allow_nan=False)+'\n');q.replace(p)
def read(p):return json.loads(Path(p).read_text())
def guard(repo,r):
 if subprocess.check_output(['git','rev-parse','HEAD'],cwd=repo,text=True).strip()!=r['code_commit']:raise RuntimeError('Commit changed')
 for x in r['sources']:
  if sha(repo/x['path'])!=x['sha256']:raise RuntimeError('Source changed: '+x['path'])
def prepare(repo,archive,out,build):
 if out.exists():raise RuntimeError('Fresh external output required')
 if subprocess.check_output(['git','status','--porcelain'],cwd=repo,text=True).strip():raise RuntimeError('Commit source before registration')
 old=read(os.environ['GMDE_CORE_REGISTRATION'])
 ext=read(os.environ['GMDE_COMPARATOR_REGISTRATION'])['external']
 lock=read(repo/'graphMoDE/external-lock.json')
 assert sha(ext['pnar'])==lock['PNAR']['sha256']
 for p,h in lock['STGNN']['files'].items():assert sha(Path(ext['stgnn'])/p)==h
 seeds={};value=2036111700
 def seed(k):
  nonlocal value
  value+=1;seeds[k]=value;return value
 init={(ds,c):seed(f'{ds}-initial-{c}') for ds in ['D01','D02'] for c in range(1,5)}
 vi={(ds,o,c):seed(f'{ds}-validation-initial-{o}-{c}') for ds in ['D01','D02'] for o in [96,120] for c in [1,3]}
 tasks={}
 def add(ds,kind,method,c=0,**kw):
  key=f'{ds}-{kind}-{method}'+(f'-c{c}' if c else '')+''.join(f'-{k}{v}' for k,v in kw.items())
  t=dict(dataset=ds,kind=kind,method=method,chain=c,**kw)
  if kind not in ['pnar_validation','pnar_final']:t['seed']=seed(key)
  if kind in ['smoke','bayes','potts_validation']:
   base=old['tasks'][f'development-{ds}-'+(method if method in ['Exp-W','SE-W','C'] else 'C')+f'-chain{c:02d}']['config'].copy()
   base.update(save_predictive_state=True,dirichlet_alpha=.1,initial_seed=init[(ds,c)])
   if method in ['EucMoDE','MoDE','PottsMoDE']:base.update(u0=0,initial_log_kappa=[-.25,0,.25,.5][c-1])
   if method=='PottsMoDE':base['potts_beta']=kw.get('beta')
   if kind=='potts_validation':base.update(iterations=600,warmup=300,initial_seed=vi[(ds,kw['origin'],c)])
   if kind=='smoke':base.update(iterations=6,warmup=3,potts_beta=1)
   t['config']=base;t['forecast_seed']=seed(key+'-forecast')
  tasks[key]=t
 for m in ['Exp-W','SE-W','C','EucMoDE','MoDE','PottsMoDE']:add('D01','smoke',m,2)
 # Queue validation ahead of final tasks to unlock all methods early.
 for ds in ['D01','D02']:
  for o in [96,120]:
   for b in [.25,1,4]:
    for c in [1,3]:add(ds,'potts_validation','PottsMoDE',c,origin=o,beta=b)
   for p in [1,2,6,24]:add(ds,'pnar_validation','PNAR',origin=o,p=p)
   for k in [3,5,7,10]:
    for pool in [.1,1]:
     for restart in [1,2]:add(ds,'stgnn_validation','STGNN',origin=o,K=k,pool=pool,restart=restart)
 for c in range(1,5):
  for ds in ['D01','D02']:
   for m in ['Exp-W','SE-W','C','EucMoDE','MoDE','PottsMoDE']:add(ds,'bayes',m,c)
 for ds in ['D01','D02']:
  add(ds,'pnar_final','PNAR')
  for restart in range(1,5):add(ds,'stgnn_final','STGNN',restart=restart)
 hits=[];files_checked=0
 pattern=re.compile(r'(?<!\d)(?:'+ '|'.join(str(s) for s in seeds.values())+r')(?!\d)')
 for folder in os.environ['GMDE_SEED_AUDIT_ROOTS'].split(os.pathsep):
  for name in subprocess.check_output(['rg','--files',folder,'-g','*registration*.json','-g','*seed*.json','-g','*plan*.json'],text=True).splitlines():
   files_checked+=1
   try:match=pattern.search(Path(name).read_text())
   except (OSError,UnicodeError):continue
   if match:hits.append([name,match.group()])
 if hits:raise RuntimeError('Seed collision: '+repr(hits))
 out.mkdir();(out/'tasks').mkdir();(out/'logs').mkdir();shutil.copytree(build,out/'build')
 identities=read(repo/'graphMoDE/preparation/2026-11-13/data-verification.json')['datasets']
 source_inputs={}
 for ds in ['D01','D02']:
  source_inputs[ds]={}
  for filename in ['blind.rds','design.rds','evaluation-only.rds']:
   path=archive/ds/filename;h=sha(path)
   assert h==identities[ds]['files'][filename],str(path)
   source_inputs[ds][filename]=h
 # Prepare blinded data and geometry only; no historic fits, states or truth loaded.
 subprocess.run(['Rscript','--vanilla',str(repo/'graphMoDE/scripts/prepare-section41-eight.R'),str(repo),str(archive),str(out)],env=ENV,check=True)
 prepared=read(out/'prepared-inputs.json')
 for task in tasks.values():
  if task['method']=='EucMoDE':
   u=prepared[task['dataset']]['euclidean_center']['u0'];task['config']['u0']=u;task['config']['initial_log_kappa']+=u
 files=subprocess.check_output(['git','ls-files','bdynets/R','bdynets/src','bdynets/inst','graphMoDE/scripts/run-section41-eight.R','graphMoDE/scripts/run-section41-eight.py','graphMoDE/scripts/prepare-section41-eight.R','graphMoDE/scripts/summarize-section41-eight.R','graphMoDE/studies/section41-eight-20261113'],cwd=repo,text=True).splitlines()
 r=dict(schema='graphmode-eight-current-discount-v1',created_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),code_commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=repo,text=True).strip(),
  repository=str(repo),source_archive=str(archive),output_directory=str(out),source_inputs=source_inputs,prepared_inputs=prepared,
  sources=[dict(path=p,sha256=sha(repo/p)) for p in files],binary_sha256=sha(out/'build/bdynets.so'),tasks=tasks,seeds=seeds,seed_audit=dict(files_checked=files_checked,collisions=[]),
  external=ext,external_lock=lock,plan=dict(role='Fresh complete eight-method comparison on D01 and D02; all finite-run diagnostics retained',
   authorization='User explicitly requires all eight methods using new runs and the latest model, including MoDE and PottsMoDE.',
   methods=['Exp-W','SE-W','C','EucMoDE','MoDE','PottsMoDE','STGNN','PNAR'],K=10,iterations=4000,warmup=2000,chains=4,initial_occupied=[1,3,7,10],workers=6,
   forecast_histories=4,forecast_horizon=48,common_fixed_graph='Raw Exp weights at h=1, the current bandwidth-prior mean; no normalization before Potts/STGNN. PNAR row-normalizes internally.',
   potts_validation=dict(beta=[.25,1,4],origins=[96,120],chains=[1,3],iterations=600,warmup=300,criterion='RMSE after averaging two chain predictive means at each origin'),
   stgnn_validation=dict(slots=[3,5,7,10],pool=[.1,1],origins=[96,120],restarts=2,epochs=300,checkpoints_every=25),
   pnar_validation=dict(lags=[1,2,6,24],origins=[96,120],maxeval=1000,xtol_rel=1e-8),
   diagnostic_thresholds=dict(rhat=1.01,ess_bulk=400,ess_tail=400,psm_rms=.1),automatic_extension=False,automatic_retry=False,manuscript_unchanged=True,
   no_old_fits_or_checkpoints=True,all_final_forecasts_fresh=True))
 put(out/'registration.json',r);print('REGISTERED',len(tasks),'tasks',r['code_commit'],out,flush=True)

def neural(repo,out,key,r):
 import numpy as np
 t=r['tasks'][key];p=out/'tasks'/key;p.mkdir()
 put(p/'task.json',t)
 data=read(out/t['dataset']/'training.json')
 if sha(out/t['dataset']/'training.json')!=r['prepared_inputs'][t['dataset']]['training_sha256']:raise RuntimeError('Changed neural training input')
 Y=np.asarray(data['Y'],dtype='float64');W=np.asarray(data['weight'],dtype='float64')
 valid=t['kind']=='stgnn_validation'
 if valid:k,pool,epochs=t['K'],t['pool'],300;train=Y[:t['origin']]
 else:
  s=read(out/t['dataset']/'selection-STGNN.json')['selected'];k,pool,epochs=s['K'],s['pool'],s['epoch'];train=Y
 spec=importlib.util.spec_from_file_location('gmde_stgnn',repo/'bdynets/inst/python/gmde_stgnn.py');module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
 scores=[]
 def checkpoint(epoch,prediction,loss):
  if valid:
   scores.append(dict(epoch=epoch,sse=float(np.square(prediction-Y[t['origin']:t['origin']+48]).sum()),n=int(prediction.size)))
  put(p/'progress.json',dict(epoch=epoch,epochs=epochs,loss=loss))
  print(key,epoch,'/',epochs,flush=True)
 try:
  result=module.train(train,W,author_directory=r['external']['stgnn'],author_hashes=r['external_lock']['STGNN']['files'],clusters=int(k),pool=float(pool),seed=t['seed'],epochs=int(epochs),seconds_cap=1800,checkpoint_callback=checkpoint)
  import torch
  torch.save(result.pop('state'),p/'model.pt');result['prediction']=result['prediction'].tolist();result.update(scores=scores,selected_K=int(k),selected_pool=pool,epochs=int(epochs))
  put(p/'result.json',result);put(p/'status.json',dict(status='completed',seconds=result['seconds']))
 except BaseException as e:
  if hasattr(e,'c8_failure'):
   import torch;torch.save(e.c8_failure,p/'failure-state.pt')
  put(p/'status.json',dict(status='failed',message=repr(e),scores=scores));raise

def select(out,r,ds,method):
 import numpy as np
 data=read(out/ds/'training.json');Y=np.asarray(data['Y'],dtype=float).T
 jobs=[(k,t) for k,t in r['tasks'].items() if t['dataset']==ds and t['method']==method and t['kind'].endswith('_validation')]
 if not all((out/'tasks'/k/'status.json').exists() for k,t in jobs):return False
 rows=[]
 if method=='PottsMoDE':
  for b in [.25,1,4]:
   total=0;valid=True
   for o in [96,120]:
    jj=[k for k,t in jobs if t['beta']==b and t['origin']==o]
    if any(read(out/'tasks'/k/'status.json')['status']!='completed' for k in jj):valid=False;continue
    pred=np.mean([read(out/'tasks'/k/'prediction.json')['prediction'] for k in jj],axis=0)
    total+=float(np.square(pred-Y[:,o:o+48]).sum())
   rows.append(dict(beta=b,rmse=(total/(2*121*48))**.5 if valid else None,eligible=valid))
 elif method=='PNAR':
  for p in [1,2,6,24]:
   jj=[(k,t) for k,t in jobs if t['p']==p];valid=all(read(out/'tasks'/k/'status.json')['status']=='completed' for k,t in jj)
   total=sum(float(np.square(np.asarray(read(out/'tasks'/k/'prediction.json')['prediction'])-Y[:,t['origin']:t['origin']+48]).sum()) for k,t in jj) if valid else 0
   rows.append(dict(p=p,rmse=(total/(2*121*48))**.5 if valid else None,eligible=valid))
 else:
  for K in [3,5,7,10]:
   for pool in [.1,1]:
    jj=[k for k,t in jobs if t['K']==K and t['pool']==pool];valid=all(read(out/'tasks'/k/'status.json')['status']=='completed' for k in jj)
    results=[read(out/'tasks'/k/'result.json') for k in jj] if valid else []
    for epoch in range(25,301,25):
     vals=[next(s for s in x['scores'] if s['epoch']==epoch) for x in results]
     rows.append(dict(K=K,pool=pool,epoch=epoch,rmse=(sum(x['sse'] for x in vals)/sum(x['n'] for x in vals))**.5 if valid else None,eligible=valid))
 eligible=[x for x in rows if x['eligible']];selected=None
 if eligible:
  best=min(x['rmse'] for x in eligible);ties=[x for x in eligible if x['rmse']<=best+1e-10*max(1,abs(best))]
  selected=min(ties,key=(lambda x:x['beta']) if method=='PottsMoDE' else (lambda x:x['p']) if method=='PNAR' else (lambda x:(x['K'],-x['pool'],x['epoch'])))
 put(out/ds/f'selection-{method}.json',dict(selected=selected,candidates=rows,training_only=True));return True

def launch(repo,out):
 r=read(out/'registration.json');guard(repo,r);active={};done={};pending=list(r['tasks']);start=time.monotonic()
 def runnable(k):
  t=r['tasks'][k]
  if t['kind']=='smoke':return True
  smoke=[j for j,v in r['tasks'].items() if v['kind']=='smoke']
  if not all(j in done for j in smoke):return False
  if any(done[j]!=0 for j in smoke):raise RuntimeError('Integration task failed; no production launch')
  if t['kind'] in ['pnar_final','stgnn_final'] or t['kind']=='bayes' and t['method']=='PottsMoDE':
   path=out/t['dataset']/f"selection-{t['method']}.json"
   if not path.exists():return False
   if read(path)['selected'] is None:
    p=out/'tasks'/k;p.mkdir();put(p/'status.json',dict(status='unavailable',message='No complete validation candidate'));done[k]=1;return False
  return True
 while pending or active:
  if shutil.disk_usage(out).free<12*2**30:raise RuntimeError('Free space reserve reached')
  for k,(process,stream) in list(active.items()):
   code=process.poll()
   if code is not None:
    stream.close();done[k]=code;del active[k];print('FINISHED',k,code,flush=True)
  for ds in ['D01','D02']:
   for m in ['PottsMoDE','STGNN','PNAR']:
    if not (out/ds/f'selection-{m}.json').exists():select(out,r,ds,m)
  for k in list(pending):
   if len(active)>=r['plan']['workers']:break
   if k in done:pending.remove(k);continue
   if not runnable(k):continue
   t=r['tasks'][k];guard(repo,r)
   cmd=[r['external']['python'],str(repo/'graphMoDE/scripts/run-section41-eight.py'),'neural',str(repo),str(out),k] if t['method']=='STGNN' else ['Rscript','--vanilla',str(repo/'graphMoDE/scripts/run-section41-eight.R'),str(repo),str(out),k]
   stream=(out/'logs'/f'{k}.log').open('x');process=subprocess.Popen(cmd,env=ENV,stdout=stream,stderr=subprocess.STDOUT)
   active[k]=(process,stream);pending.remove(k);print('STARTED',k,process.pid,flush=True)
  put(out/'batch-progress.json',dict(completed=len(done),failed=[k for k,v in done.items() if v],running={k:p.pid for k,(p,s) in active.items()},pending=len(pending),seconds=time.monotonic()-start,checked_utc=datetime.datetime.now(datetime.timezone.utc).isoformat()))
  if pending and not active and not any(runnable(k) for k in pending):raise RuntimeError('Unresolved task dependencies')
  time.sleep(5)
 put(out/'batch-completed.json',dict(completed=len(done),failed=[k for k,v in done.items() if v],seconds=time.monotonic()-start,ended_utc=datetime.datetime.now(datetime.timezone.utc).isoformat()))
 subprocess.run(['Rscript','--vanilla',str(repo/'graphMoDE/scripts/summarize-section41-eight.R'),str(repo),str(out)],env=ENV,check=True)

if __name__=='__main__':
 mode=sys.argv[1];repo=Path(sys.argv[2]).resolve()
 if mode=='prepare':prepare(repo,Path(sys.argv[3]),Path(sys.argv[4]),Path(sys.argv[5]))
 elif mode=='launch':launch(repo,Path(sys.argv[3]))
 elif mode=='neural':
  out=Path(sys.argv[3]);r=read(out/'registration.json');guard(repo,r);neural(repo,out,sys.argv[4],r)
 else:raise SystemExit('Unknown command')
