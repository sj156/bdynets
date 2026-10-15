#!/usr/bin/env python3
"""Portable two-data eight-method entry. External author dependencies are explicit."""
from pathlib import Path
import argparse,csv,hashlib,importlib.util,json,os,subprocess,sys,time
ROOT=Path(__file__).resolve().parent
ENV=dict(os.environ,LC_ALL='C',OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',MKL_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1',PYTHONDONTWRITEBYTECODE='1')

def write(path,value):
    with path.open('x') as f:json.dump(value,f,indent=2)

def sha(path):return hashlib.sha256(path.read_bytes()).hexdigest()

def scheduler():
    spec=importlib.util.spec_from_file_location('comparison_entry',ROOT/'comparison.py')
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module

def dependencies(pnar,stgnn,python):
    lock=json.loads((ROOT/'external-lock.json').read_text())
    pnar=pnar.resolve(strict=True);stgnn=stgnn.resolve(strict=True);python=python.absolute()
    if sha(pnar)!=lock['PNAR']['sha256']:raise ValueError('PNAR source does not match external-lock.json')
    for name,h in lock['STGNN']['files'].items():
        if sha(stgnn/name)!=h:raise ValueError('STGNN source mismatch: '+name)
    runtime=json.loads(subprocess.check_output([str(python),str(ROOT/'comparison.py'),'runtime'],env=ENV,text=True))
    return dict(pnar=str(pnar),stgnn=str(stgnn),python=str(python)),runtime

def configure(output,core,external,runtime):
    # This file is private local configuration; no built-in user directory.
    plan=output/'comparison-plan.json'
    subprocess.run([sys.executable,str(ROOT/'comparison.py'),'plan',str(plan)],env=ENV,check=True)
    cfg=dict(plan_file=str(plan),core_input=str(core),external=external,python_runtime=runtime,
             queue_after=[],combined_outputs=[str(core)],core_results=None)
    path=output/'comparison-config.json';write(path,cfg);return path

def merge_tables(core,supplement,out):
    spec=importlib.util.spec_from_file_location('result_report',ROOT/'report.py')
    report=importlib.util.module_from_spec(spec);spec.loader.exec_module(report)
    result=report.assemble(supplement,out,core)
    write(out/'completion.json',dict(finished=time.time(),rows=result['rows'],
          all_method_rows_present=result['rows']==16,metrics_unavailable=result['metrics_unavailable'],
          automatic8000=False,interpretation='All rows are provisional; failed diagnostics never suppress metrics.'))

def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--output',type=Path,required=True,help='New external directory; never overwritten')
    p.add_argument('--pnar-source',type=Path,required=True,help='Pinned PNAR/R/lin_estimnarpq.R')
    p.add_argument('--stgnn-source',type=Path,required=True,help='Pinned author checkout containing source/')
    p.add_argument('--stgnn-python',type=Path,required=True,help='Existing compatible Python environment executable')
    p.add_argument('--core-output',type=Path,help='Reuse an already terminated portable run.py core output; never restart it')
    p.add_argument('--configure-only',action='store_true',help='Write a plan/configuration for an existing core output without fitting')
    a=p.parse_args();out=a.output.absolute()
    if out.exists():p.error('Output must not exist; scientific retries are not automatic')
    if a.configure_only and not a.core_output:p.error('--configure-only requires --core-output')
    if a.core_output:
        core=a.core_output.resolve(strict=True)
        if not a.configure_only and not any((core/name).exists() for name in ('completion.json','failure.json')):
            p.error('Existing core must have terminated; use the explicit queue interface for running controllers')
        if not (core/'generation-complete.rds.sha256').exists():p.error('Core inputs were not completely generated')
    else:core=out/'core'
    external,runtime=dependencies(a.pnar_source,a.stgnn_source,a.stgnn_python)
    control=scheduler()
    if not a.configure_only and control.capacity(control.processes(),{})!=6:
        p.error('This full-batch entry requires six free numeric slots at startup. Use the explicit queued interface when other work is running.')
    out.mkdir();config=configure(out,core,external,runtime)
    write(out/'request.json',dict(command=sys.argv,created=time.time(),existing_core=a.core_output is not None,
                               source_sha256=sha(Path(__file__)),automatic8000=False))
    if a.configure_only:
        print(str(config));return
    # Both preflight checks precede any new data or fitting.
    preflight=[['Rscript','--vanilla',str(ROOT/'tests/comparison.R')],
               [external['python'],str(ROOT/'tests/external_import.py'),str(config)]]
    for i,args in enumerate(preflight,1):
        with (out/f'preflight-{i}.log').open('xb') as log:
            subprocess.run(args,env=ENV,stdout=log,stderr=subprocess.STDOUT,check=True)
    if a.core_output is None:
        with (out/'core.log').open('xb') as log:
            rc=subprocess.run([sys.executable,str(ROOT/'run.py'),'core','--output',str(core)],env=ENV,stdout=log,stderr=subprocess.STDOUT).returncode
        write(out/'core-process.json',dict(exit_code=rc))
        if not (core/'generation-complete.rds.sha256').exists():raise RuntimeError('Core input generation failed; supplementary fits not launched')
    supplemental=out/'supplement'
    with (out/'supplement.log').open('xb') as log:
        result=subprocess.run([sys.executable,str(ROOT/'comparison.py'),'queue',str(supplemental),'--config',str(config)],env=ENV,stdout=log,stderr=subprocess.STDOUT)
    write(out/'supplement-process.json',dict(exit_code=result.returncode))
    merge_tables(core,supplemental,out)
    if result.returncode:raise SystemExit(result.returncode)

if __name__=='__main__':main()
