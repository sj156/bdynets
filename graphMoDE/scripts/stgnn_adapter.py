#!/usr/bin/env python3
"""Opt-in C8 author-model adapter. No execution CLI and no training on import."""
import hashlib
from pathlib import Path
import sys


def verify_author(directory, expected):
    directory = Path(directory).resolve(strict=True)
    if not expected:
        raise ValueError("Pinned author source hashes required")
    for relative, sha in expected.items():
        path = (directory / relative).resolve(strict=True)
        if not path.is_relative_to(directory):
            raise ValueError("Source escapes author directory")
        if hashlib.sha256(path.read_bytes()).hexdigest() != sha:
            raise ValueError(f"Changed author source: {relative}")


def build_model(TTSModel, nodes, clusters, pool):
    if clusters not in (3, 5, 7, 10) or pool not in (.1, 1.):
        raise ValueError("Unregistered candidate")
    return TTSModel(input_size=1, exog_size=2, hidden_size=16, temporal_layers=2,
                    kernel_size=3, dilation=2, exponential_dilation=True, skip_connection=True,
                    gnn_layers=2, n_nodes=nodes, n_clusters=clusters, topo_w=pool, qual_w=pool,
                    horizon=1, temporal_enc_type='tcn', temporal_aggr_type='attention',
                    mp_method='gcs', pool_method='mincut', lift_softmax='temperature', softmax_temp=1.)


def covariates(periods):
    import numpy as np
    t = np.arange(1, periods + 1)
    return np.column_stack([np.sin(2*np.pi*t/24), np.cos(2*np.pi*t/24)]).astype('float32')


def prepare(training, weight, window=48):
    import numpy as np
    y = np.asarray(training, dtype='float32')  # time x sites; training only
    w = np.asarray(weight, dtype='float32')
    if y.ndim != 2 or not window < len(y) <= 168 or not np.isfinite(y).all():
        raise ValueError("Supply only a complete training prefix")
    if np.any(y < 0) or np.any(y != np.round(y)):
        raise ValueError("Expected counts")
    if (w.shape != (y.shape[1], y.shape[1]) or not np.isfinite(w).all()
            or np.any(w < 0) or np.any(w.sum(1) <= 0)
            or np.any(w.diagonal() != 0) or not np.allclose(w, w.T, rtol=0, atol=1e-7)):
        raise ValueError("Invalid common road graph")
    mean, scale = float(y.mean()), float(y.std())
    if scale <= 0:
        raise ValueError("Degenerate training scale")
    z = (y - mean) / scale
    u = covariates(len(y))
    edges = np.array(np.nonzero(w), dtype='int64')
    return dict(mean=mean, scale=scale, standardized=z, window=window,
                X=np.stack([z[t-window:t, :, None] for t in range(window, len(y))]),
                target=np.stack([z[t:t+1, :, None] for t in range(window, len(y))]),
                U=np.stack([u[t-window:t] for t in range(window, len(y))]),
                edge_index=edges, edge_weight=w[tuple(edges)])


def forecast(prepared, one_step, horizon=48):
    """one_step receives past standardized values, graph and known time features."""
    import numpy as np
    if not isinstance(horizon, int) or not 1 <= horizon <= 48:
        raise ValueError("Unregistered forecast horizon")
    history = prepared['standardized'].copy()
    window = prepared['window']
    u = covariates(len(history) + horizon)
    predictions = []
    for _ in range(horizon):
        t = len(history)
        z = np.asarray(one_step(history[-window:, :, None], prepared['edge_index'],
                               prepared['edge_weight'], u[t-window:t]))
        if z.shape != (history.shape[1],) or not np.isfinite(z).all():
            raise ValueError("Invalid one-step prediction")
        value = np.maximum(0., z * prepared['scale'] + prepared['mean'])
        if not np.isfinite(value).all():
            raise ValueError("Invalid inverse-scaled prediction")
        predictions.append(value)
        history = np.concatenate([history, ((value - prepared['mean']) / prepared['scale'])[None]])
    return np.stack(predictions)


def train(training, weight, *, author_directory, author_hashes, clusters, pool,
          seed, epochs=300, seconds_cap=600, checkpoint_callback=None):
    """Future registered worker entry. Caller owns authorization and failure receipts.

    Validation callers request 300 epochs and score EVERY callback prediction.
    Refit callers use the globally selected epoch and keep all four restarts.
    No validation response or true label is accepted by this fitting function.
    """
    import time
    import random
    import numpy as np
    import torch
    from torch.utils.data import DataLoader, TensorDataset
    if epochs not in range(25, 301, 25) or seconds_cap <= 0:
        raise ValueError("Unregistered training length/budget")
    verify_author(author_directory, author_hashes)
    sys.path.insert(0, str(Path(author_directory).resolve()))
    from source.modules import TTSModel
    # Prevent an already-imported same-named package from another checkout.
    import inspect
    if not Path(inspect.getfile(TTSModel)).resolve().is_relative_to(Path(author_directory).resolve()):
        raise ValueError("Author module came from a different checkout")
    data = prepare(training, weight)
    random.seed(seed); np.random.seed(seed); torch.manual_seed(seed)
    torch.set_num_threads(1); torch.use_deterministic_algorithms(True)
    model = build_model(TTSModel, data['standardized'].shape[1], clusters, pool)
    optimizer = torch.optim.Adam(model.parameters(), lr=.001, weight_decay=.0001)
    scheduler = torch.optim.lr_scheduler.MultiStepLR(optimizer, milestones=[50,100,150,200,250], gamma=.5)
    tensors = [torch.as_tensor(data[k], dtype=torch.float32) for k in ('X', 'target', 'U')]
    loader = DataLoader(TensorDataset(*tensors), batch_size=16, shuffle=True, num_workers=0)
    edges = torch.as_tensor(data['edge_index'], dtype=torch.long)
    weights = torch.as_tensor(data['edge_weight'], dtype=torch.float32)
    started = time.monotonic(); history = []

    def predict(x, _edges, _weight, u):
        with torch.no_grad():
            pred, *_ = model(torch.as_tensor(x, dtype=torch.float32)[None], edges, weights,
                             torch.as_tensor(u, dtype=torch.float32)[None])
        return pred[0, 0, :, 0].numpy()

    def check_budget():
        if time.monotonic() - started > seconds_cap:
            raise TimeoutError("STGNN fit budget exhausted; no automatic restart")

    try:
        for epoch in range(1, epochs + 1):
            model.train(); total = 0.; observations = 0
            for x, y, u in loader:
                check_budget(); optimizer.zero_grad()
                pred, *aux = model(x, edges, weights, u)
                loss = (pred-y).abs().mean() + sum(aux)
                if not torch.isfinite(loss):
                    raise FloatingPointError("Nonfinite author training loss")
                loss.backward()
                torch.nn.utils.clip_grad_norm_(model.parameters(), 5., error_if_nonfinite=True)
                optimizer.step(); total += float(loss.detach()) * len(x); observations += len(x)
            scheduler.step(); model.softmax_temp = max(.01, model.softmax_temp - .0099)
            history.append(total / observations)
            if epoch % 25 == 0:
                model.eval(); prediction = forecast(data, predict)
                check_budget()
                if checkpoint_callback is not None:
                    checkpoint_callback(epoch, prediction, history[-1])
        labels = (model.pooling_layer.assignments().detach().argmax(-1) + 1).tolist()
        return dict(labels=labels, prediction=prediction, history=history, seed=seed,
                    seconds=time.monotonic()-started, state=model.state_dict(),
                    temperature=model.softmax_temp, mean=data['mean'], scale=data['scale'])
    except BaseException as error:
        # Let the future worker persist an explicit failure without rerunning.
        error.c8_failure = dict(epoch=epoch, history=history, state=model.state_dict(),
                                optimizer=optimizer.state_dict(), rng=torch.get_rng_state())
        raise


if __name__ == '__main__':
    raise SystemExit('Preparation module only; no simulation CLI. Use the deterministic test script.')
