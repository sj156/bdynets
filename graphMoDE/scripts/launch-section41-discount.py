# Launch only the registered study phases; preserve every failure and output.
from pathlib import Path
import subprocess,json,concurrent.futures,os,datetime,time,sys,hashlib,shutil
if len(sys.argv)!=3: raise SystemExit('Usage: REPOSITORY REGISTERED_OUTPUT')
repo=Path(sys.argv[1]).resolve();out=Path(sys.argv[2]).resolve()
if (out/'batch-status.json').exists() or any(out.glob('smoke-*')): raise SystemExit('Run already started; no retry or resume.')
source=out/'launcher-source.py'
if source.exists(): raise SystemExit('Launcher snapshot already exists.')
shutil.copy2(__file__,source)
reg=json.loads((out/'registration.json').read_text())
reg['launcher_sha256']=hashlib.sha256(source.read_bytes()).hexdigest()
(out/'registration.json').write_text(json.dumps(reg,indent=2)+'\n')
archive=reg['source_archive']
env=os.environ.copy()
for k in ['OMP_NUM_THREADS','OPENBLAS_NUM_THREADS','MKL_NUM_THREADS','VECLIB_MAXIMUM_THREADS','NUMEXPR_NUM_THREADS']:env[k]='1'
script=repo/'graphMoDE/scripts/run-section41-discount.R'
def run(task_id):
 with (out/(task_id+'.log')).open('x') as f:
  result=subprocess.run(['Rscript',str(script),str(repo),archive,str(out/'registration.json'),task_id,str(out)],env=env,stdout=f,stderr=subprocess.STDOUT)
 status_path=out/task_id/'status.json'
 status=json.loads(status_path.read_text()) if status_path.exists() else {'status':'process_failure','returncode':result.returncode}
 print(task_id,status,flush=True)
 return task_id,status
results={}
for phase in ['smoke','development']:
 if phase=='development':
  smoke=[v for k,v in results.items() if k.startswith('smoke-')]
  complete=len(smoke)==6 and all(s.get('status')=='completed' for s in smoke)
  projection=max([s.get('seconds',float('inf'))/reg['plan']['smoke']['iterations'] for s in smoke])*reg['plan']['launch_gate']['development_sweeps']*reg['plan']['launch_gate']['projection_multiplier']
  gate={'all_smoke_complete':complete,'projected_serial_seconds_with_factor2':projection,'threshold':reg['plan']['launch_gate']['maximum_projected_serial_seconds'],'passed':complete and projection<=reg['plan']['launch_gate']['maximum_projected_serial_seconds']}
  (out/'launch-gate.json').write_text(json.dumps(gate,indent=2)+'\n')
  print('Development launch gate:',gate,flush=True)
  if not gate['passed']:break
 ids=[k for k,v in reg['tasks'].items() if v['phase']==phase]
 with concurrent.futures.ThreadPoolExecutor(max_workers=reg['plan']['launch_gate']['workers']) as pool:
  for task_id,status in pool.map(run,ids):
   results[task_id]=status
   (out/'batch-status.json').write_text(json.dumps(results,indent=2)+'\n')
(out/'batch-completed.json').write_text(json.dumps({'ended_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'tasks_finished':len(results),'complete':all(v.get('status')=='completed' for v in results.values()),'development_started':any(k.startswith('development-') for k in results)},indent=2)+'\n')
print('Batch finished; no retries or extensions.',flush=True)
