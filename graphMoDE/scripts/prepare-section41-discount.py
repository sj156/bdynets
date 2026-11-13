# Prepare a fresh, externally built and source-hashed development registration.
# Explicit run authorization is still required; this script does not launch chains.
from pathlib import Path
import subprocess,json,hashlib,shutil,csv,os,datetime,sys
if len(sys.argv)!=4: raise SystemExit('Usage: REPOSITORY ARCHIVE NEW_OUTPUT')
repo=Path(sys.argv[1]).resolve();archive=Path(sys.argv[2]).resolve();out=Path(sys.argv[3]).resolve()
assert not out.exists(),'Fresh output required'
assert not out.is_relative_to(repo),'Outputs must be external to the repository'
assert archive.is_dir(),'Input archive is missing'
assert subprocess.check_output(['git','status','--porcelain'],cwd=repo,text=True)==''
commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=repo,text=True).strip()
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
plan=json.loads((repo/'graphMoDE/studies/section41-discount-20261113/plan.json').read_text())
identities=json.loads((repo/'graphMoDE/preparation/2026-11-13/data-verification.json').read_text())['datasets']
cal=list(csv.DictReader((repo/'graphMoDE/preparation/2026-11-13/geometry-calibration.csv').open()))
files=subprocess.check_output(['git','ls-files','bdynets/R','bdynets/src','bdynets/inst/include','graphMoDE/scripts/run-section41-discount.R','graphMoDE/studies/section41-discount-20261113/plan.json'],cwd=repo,text=True).splitlines()
sources=[{'path':p,'sha256':sha(repo/p)} for p in files]
out.mkdir(parents=True)
shutil.copytree(repo/'bdynets',out/'build',ignore=shutil.ignore_patterns('*.o','*.so','*.dSYM'))
inc=subprocess.check_output(['Rscript','-e',"cat(system.file('include',package='Rcpp'))"],text=True).strip()
env=os.environ.copy();env['PKG_CPPFLAGS']='-I'+inc;env['PKG_CXXFLAGS']='-std=gnu++17'
with (out/'build.log').open('w') as f:
 subprocess.run(['R','CMD','SHLIB','-o','bdynets.so','gls-reference.c','gls-kernels.cpp','scalar-state-space.cpp','gmde-discount.cpp','gmde-dynamic.cpp','RcppExports.cpp'],cwd=out/'build/src',env=env,stdout=f,stderr=subprocess.STDOUT,check=True)
tasks={}
for phase in ['smoke','development']:
 k=0;p=plan[phase]
 for dataset in plan['datasets']:
  for method in plan['methods']:
   ref='Connectivity' if method=='C' else 'Exp-reference'
   u0=float(next(x for x in cal if x['dataset']==dataset and x['reference']==ref)['u0'])
   for c,ngroups in enumerate(p['initial_groups']):
    cfg={**plan['scientific_config'],**plan['computation'],'u0':u0,'initial_groups':ngroups,
     'iterations':p['iterations'],'warmup':p['warmup'],'initial_h':p['initial_h'][c],
     'initial_log_kappa':u0+p['log_kappa_offset'][c]}
    task_id=f'{phase}-{dataset}-{method}-chain{c+1:02}'
    tasks[task_id]={'phase':phase,'dataset':dataset,'method':method,'chain':c+1,'seed':p['seeds'][k],
     'input_sha256':identities[dataset]['files']['blind.rds'],'config':cfg}
    k+=1
registration={'schema':'graphmode-discount-run-registration-v1','created_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),
 'code_commit':commit,'upstream_commit':'3f2df3907cd3898f0d657ace862a34ea98e31f9d',
 'output_directory':str(out),'source_archive':str(archive),
 'role':plan['role'],'plan':plan,'sources':sources,'binary_sha256':sha(out/'build/src/bdynets.so'),'tasks':tasks,
 'no_old_fits_or_checkpoints':True,'manuscript_unchanged':True}
(out/'registration.json').write_text(json.dumps(registration,indent=2)+'\n')
print('Registered',len(tasks),'tasks and',len(sources),'source files at',commit)
print('Output:',out)
print('Registration SHA256:',sha(out/'registration.json'))
