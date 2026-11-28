"""One-step count benchmark using Wu et al.'s pinned Graph WaveNet network."""
import argparse
import copy
import hashlib
import importlib.util
import json
import platform
import sys
from pathlib import Path

import numpy as np
import torch
from torch import nn

COMMIT = "6b162e80c59a1d494809252eca055cff93dc66b1"
MODEL_SHA256 = "f357319260657716c18916d691d31179e2380dcb882b9c41556c9a55912f1059"
HISTORY = 12


def upstream_network(model_path, nodes, channels, supports, smoke):
    if hashlib.sha256(model_path.read_bytes()).hexdigest() != MODEL_SHA256:
        raise ValueError("Unexpected upstream Graph WaveNet model.py digest")
    spec = importlib.util.spec_from_file_location("upstream_graph_wavenet", model_path)
    module = importlib.util.module_from_spec(spec)
    sys.dont_write_bytecode = True
    spec.loader.exec_module(module)
    width = dict(residual_channels=8, dilation_channels=8, skip_channels=16,
                 end_channels=32) if smoke else {}
    net = module.gwnet("cpu", nodes, in_dim=channels, out_dim=1, supports=supports,
                       gcn_bool=True, addaptadj=True, **width)
    # The authors' Conv1d layers use 2D kernel tuples and receive 4D inputs.
    # Modern PyTorch requires Conv2d for that same convolution. Preserve the
    # original initialized weights, biases, and temporal dilation exactly.
    for group in (net.gate_convs, net.residual_convs, net.skip_convs):
        for i, layer in enumerate(group):
            compatible = nn.Conv2d(layer.in_channels, layer.out_channels,
                                   kernel_size=layer.weight.shape[2:],
                                   dilation=(1, layer.dilation[0]))
            compatible.weight, compatible.bias = layer.weight, layer.bias
            group[i] = compatible
    return net


def run(args):
    torch.set_num_threads(1)
    torch.manual_seed(args.seed)
    torch.use_deterministic_algorithms(True)
    rng = np.random.default_rng(args.seed)
    read = lambda name: np.loadtxt(args.input / name, delimiter=",", ndmin=2).astype(np.float32)
    y = read("Y.csv")
    a = read("A.csv")
    q = read("Q.csv") if (args.input / "Q.csv").exists() else np.empty((len(y), 0), np.float32)
    n, k = y.shape
    end_val = args.train + args.validation
    if not HISTORY < args.train < end_val < n or a.shape != (k, k) or len(q) != n:
        raise ValueError("Invalid panel or chronological training and validation boundaries")
    # The R graph is target by source. nconv aggregates on its left index.
    def transition(x):
        total = x.sum(axis=1, keepdims=True)
        return torch.from_numpy(x / np.where(total > 0, total, 1))
    supports = [transition(a.T), transition(a)]
    center = y[:args.train].mean(axis=0)
    scale = y[:args.train].std(axis=0)
    scale[scale == 0] = 1.0
    scaled = (y - center) / scale
    smoke = args.profile == "smoke"
    model = upstream_network(args.model, k, 1 + q.shape[1], supports, smoke)
    optimizer = torch.optim.Adam(model.parameters(), lr=.001, weight_decay=.0001)
    # Time windows are materialized one minibatch at a time.
    batch = 8 if smoke else 16
    def predict(rows):
        lag = rows[:, None] + np.arange(-HISTORY, 0)
        count = scaled[lag][:, None, :, :].transpose(0, 1, 3, 2)
        calendar = np.broadcast_to(q[lag].transpose(0, 2, 1)[:, :, None, :],
                                   (len(rows), q.shape[1], k, HISTORY))
        data = torch.from_numpy(np.concatenate((count, calendar), axis=1))
        return model(data)[:, 0, :, -1]
    def forecast(rows):
        model.eval()
        with torch.no_grad():
            z = np.concatenate([predict(rows[i:i+batch]).numpy() for i in range(0,len(rows),batch)])
        return np.maximum(1e-6, center + scale*z)
    epochs = 2 if smoke else 200
    best = float("inf")
    selected_epoch = 0
    history = []
    for epoch in range(1, epochs+1):
        model.train()
        order = rng.permutation(np.arange(HISTORY, args.train))
        for i in range(0, len(order), batch):
            rows = order[i:i+batch]
            optimizer.zero_grad()
            loss = nn.functional.l1_loss(predict(rows), torch.from_numpy(scaled[rows]))
            loss.backward()
            nn.utils.clip_grad_norm_(model.parameters(), 5.0)
            optimizer.step()
        val = forecast(np.arange(args.train, end_val))
        score = float(np.abs(val - y[args.train:end_val]).mean())
        history.append(score)
        if score < best:
            best, selected_epoch = score, epoch
            best_state = copy.deepcopy(model.state_dict())
    if not selected_epoch:
        raise FloatingPointError("No finite validation MAE")
    model.load_state_dict(best_state)
    pred = forecast(np.arange(end_val, n))
    np.savetxt(args.output / "graph-wavenet-mean.csv", pred, delimiter=",", fmt="%.9g")
    torch.save(best_state, args.output / "graph-wavenet-weights.pt")
    with (args.output / "graph-wavenet.json").open("w") as f:
        json.dump(dict(upstream="nnzhan/Graph-WaveNet", commit=COMMIT, model_sha256=MODEL_SHA256,
                       adapter_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                       compatibility="Conv2d for upstream 2D-kernel Conv1d modules",
                       python=platform.python_version(), torch=torch.__version__, numpy=np.__version__,
                       profile=args.profile, seed=args.seed, history=HISTORY, cpu_threads=1,
                       train_rows=args.train, validation_rows=args.validation, test_rows=n-end_val,
                       supports="forward and reverse row-standardized adjacency plus learned adjacency",
                       training_loss="MAE of train-standardized counts; per-series scales",
                       count_forecast_floor=1e-6, selected_epoch=selected_epoch,
                       validation_mae=history, batch_size=batch, epochs=epochs), f, indent=2)


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    for key in ("model", "input", "output"):
        p.add_argument("--"+key, required=True, type=Path)
    for key in ("train", "validation", "seed"):
        p.add_argument("--"+key, required=True, type=int)
    p.add_argument("--profile", choices=["smoke","quick","pilot","full"], required=True)
    run(p.parse_args())
