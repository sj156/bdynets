"""Import/shape check only; does not initialize a model or fit author code."""
from pathlib import Path
import importlib.util, inspect, json, sys
root=Path(__file__).resolve().parents[1]
config=json.loads(Path(sys.argv[1]).read_text());author=Path(config['external']['stgnn']).resolve()
sys.path.insert(0,str(author));from source.modules import TTSModel
assert Path(inspect.getfile(TTSModel)).resolve().is_relative_to(author)
spec=importlib.util.spec_from_file_location('adapter',root/'scripts/stgnn_adapter.py');m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
import numpy as np
Y=np.asarray([[20+(i+t)%7 for i in range(12)] for t in range(96)],dtype='float64')
W=np.eye(12,k=1)+np.eye(12,k=-1);p=m.prepare(Y,W)
assert p['X'].shape==(48,48,12,1)
print('Pinned author import and training-prefix input shape passed; no model initialization/training.')
