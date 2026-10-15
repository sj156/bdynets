"""Synthetic report fixtures only; no numerical worker or sampling."""
from pathlib import Path
import csv, importlib.util, json, tempfile, unittest
ROOT=Path(__file__).resolve().parents[1]
spec=importlib.util.spec_from_file_location('result_report',ROOT/'report.py')
r=importlib.util.module_from_spec(spec);spec.loader.exec_module(r)

def put(path,value):
    path.parent.mkdir(parents=True,exist_ok=True);path.write_text(json.dumps(value))

class Reporting(unittest.TestCase):
    def test_failed_precision_retains_metrics_and_missing_rows(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);supp=root/'supp';(supp/'summary').mkdir(parents=True)
            rows=[dict(dataset='D01',method='Exp-W',valid='FALSE',ARI='.61',Khat='7',Kocc_mean='6.8',RMSE='5.4',status='precision_failed'),
                  dict(dataset='D01',method='PNAR',valid='TRUE',ARI='NA',Khat='NA',Kocc_mean='NA',RMSE='5.7',status='completed_no_MCMC_precision_claim')]
            with (supp/'summary/fits.csv').open('w') as f:
                w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
            manifest=r.assemble(supp,root/'report');out=r.read_csv(root/'report/all-methods.csv')
            self.assertEqual(len(out),16)
            self.assertEqual((out[0]['ARI'],out[0]['RMSE'],out[0]['Kocc_mean']),('.61','5.4','6.8'))
            self.assertEqual(out[0]['result_status'],'provisional_precision_failed')
            self.assertEqual(out[7]['diagnostic_status'],'not_applicable')
            self.assertEqual(out[7]['Kocc_mean'],'NA')
            self.assertEqual(out[8]['result_status'],'metrics_unavailable')
            self.assertFalse(manifest['precision_filter_applied'])
            self.assertEqual(len(manifest['metrics_unavailable']),14)
            with self.assertRaises(FileExistsError):r.assemble(supp,root/'report')

    def test_monotonic_timing_includes_prefix_and_failed_validation(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);out=root/'out';parent=root/'parent'
            jobs=[dict(id='chain',dataset='D01',method='Exp-W',kind='bayes'),
                  dict(id='tune',dataset='D01',method='Exp-W',kind='potts_validation')]
            put(out/'registration.json',dict(plan=dict(jobs=jobs),parent=str(parent),anchors=dict(chain=dict(iteration=1000))))
            put(out/'jobs/chain/process-4000.json',dict(seconds=30,exit_code=0))
            put(parent/'jobs/chain/process-4000.json',dict(seconds=10,exit_code=-15))
            put(out/'jobs/tune/process.json',dict(seconds=4,exit_code=1))
            values,_,_=r.timing(out,'D01','Exp-W',True)
            self.assertEqual(values['final_process_seconds'],40)
            self.assertEqual(values['total_recorded_process_seconds'],44)
            self.assertTrue(values['timing_complete'])
            (parent/'jobs/chain/process-4000.json').write_text('{"seconds": -1}')
            values,_,_=r.timing(out,'D01','Exp-W',True)
            self.assertFalse(values['timing_complete'])
            self.assertEqual(values['timing_missing_records'],1)

if __name__=='__main__':unittest.main()
