"""Fast mocked controller checks; no R process or simulation is started."""
from pathlib import Path
from unittest.mock import patch
import importlib.util,json,signal,tempfile,types

ROOT=Path(__file__).resolve().parents[1]

def exercise(mode):
    spec=importlib.util.spec_from_file_location('pilot',ROOT/'run.py')
    m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
    instances=[];signals=[];resource_calls=0
    class Worker:
        def __init__(self,command,**kwargs):
            self.command=command;self.pid=100+len(instances);self.returncode=None;self.polls=0
            self.summary='summary' in command;instances.append(self)
            if self.summary:
                folder=Path(command[-2])/('summary-'+str(command[-1]));folder.mkdir()
                (folder/'decision.json').write_text(json.dumps({'all_passed':False,'target':4000}))
        def poll(self):
            self.polls+=1
            if mode=='peer' and self.pid==100:self.returncode=1
            elif mode not in ('resource','record_failure') and (self.summary or self.polls>=3):self.returncode=0
            return self.returncode
        def wait(self,**kwargs):raise AssertionError('Do not block waiting for a suspended writer')
        def terminate(self):raise AssertionError('Do not terminate a peer')
        def kill(self):raise AssertionError('Do not kill a peer')
    def register(command,**kwargs):
        out=Path(command[-2]);out.mkdir();(out/'jobs').mkdir()
        (out/'registration.json').write_text(json.dumps({'signature':'fixture'}))
        (out/'jobs.csv').write_text('id\nfirst\nsecond\n')
        return types.SimpleNamespace(stdout='',stderr='',returncode=0,check_returncode=lambda:None)
    def check_resources(out):
        nonlocal resource_calls
        resource_calls+=1
        if mode in ('resource','record_failure') and resource_calls==2:raise m.ResourceLimit('Injected resource limit')
    original_publish=m.publish
    def publish(path,data):
        if mode=='record_failure' and path.name.startswith('emergency-'):raise OSError('Injected record-write failure')
        original_publish(path,data)
    with tempfile.TemporaryDirectory() as d, patch.object(m.subprocess,'run',register), \
         patch.object(m.subprocess,'Popen',Worker),patch.object(m.time,'sleep',lambda _:None), \
         patch.object(m.sys,'platform','linux'),patch.object(m,'resources',check_resources), \
         patch.object(m.os,'kill',lambda pid,sig:signals.append((pid,sig))),patch.object(m,'publish',publish):
        out=Path(d)/'pilot';error=None
        try:m.pilot(out,mode!='hold4000')
        except RuntimeError as e:error=e
        if mode=='peer':
            assert error and 'valid peers were allowed to finish' in str(error)
            assert instances[1].returncode==0 and not signals
        elif mode in ('resource','record_failure'):
            assert isinstance(error,m.ResourceLimit)
            assert signals==[(100,signal.SIGSTOP),(101,signal.SIGSTOP)]
            record=json.loads((out/'suspended-4.json').read_text())
            assert all(x['status']=='suspended_in_memory' for x in record['workers'])
            assert record['automatic_resume'] is False
        else:
            assert error is None
            assert all(int(p.command[-1])==4000 for p in instances)
            assert len(instances)==3 and not signals
            assert json.loads((out/'completion.json').read_text())['automatic_extension'] is False
    with patch.object(m.time,'monotonic',return_value=72*3600+1):
        try:m.check_deadline(0)
        except m.ResourceLimit:pass
        else:raise AssertionError('Time limit must use emergency path')
    print('PASS',mode)

for case in ('peer','resource','record_failure','hold4000'):exercise(case)
