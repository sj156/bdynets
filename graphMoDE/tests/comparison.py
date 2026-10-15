"""Lightweight scheduling tests; no scientific simulation or external imports."""
import importlib.util
from pathlib import Path
import collections, unittest
spec=importlib.util.spec_from_file_location('comparison',Path(__file__).resolve().parents[1]/'comparison.py')
c=importlib.util.module_from_spec(spec);spec.loader.exec_module(c)

class Contract(unittest.TestCase):
    def test_counts_seeds_dependencies(self):
        p=c.plan();counts=collections.Counter(j['kind'] for j in p['jobs'])
        self.assertEqual(dict(counts),dict(potts_validation=24,pnar_validation=16,stgnn_validation=64,selection=6,bayes=24,pnar_final=2,stgnn_final=8))
        self.assertEqual(len(p['seeds']),len(set(p['seeds'].values())))
        ids={j['id'] for j in p['jobs']};self.assertEqual(len(ids),144)
        for j in p['jobs']:self.assertTrue(all(d in ids for d in j['dependencies'].split(',') if d))
    def test_phase_gate_and_capacity(self):
        reg={'queue_after':[dict(pid=10,command_contains='prior-controller')]}
        rows=[dict(pid=10,state='S',cpu=0,command='python prior-controller')]
        self.assertFalse(c.gate(reg,rows));self.assertTrue(c.gate(reg,[]))
        rows += [dict(pid=i,state='R',cpu=99,command='R --vanilla') for i in range(20,26)]
        self.assertEqual(c.capacity(rows,{}),0)
        self.assertEqual(c.capacity(rows,{20:None,21:None}),0)

if __name__=='__main__':unittest.main()
