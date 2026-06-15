import math
import heapq
from pathlib import Path
from collections import defaultdict, Counter

import numpy as np
import pandas as pd

try:
    from tqdm import tqdm
except Exception:
    def tqdm(x, **kwargs):
        return x


# ============================================================
# Config
# ============================================================

TIME_BIN_MIN = 5

# "entry" means a vehicle is counted only when it enters an edge.
# "occupancy" means a vehicle is counted in every time bin it stays on the edge.
EDGE_FLOW_MODE = "occupancy"

FULL_DAY_MINUTES = 1440
FORCE_FULL_DAY = True

# If FORCE_FULL_DAY = True and this is True, only [0, 1440) is kept.
# A trip segment crossing 1440 will be clipped to the last valid bin.
CLIP_FLOW_TO_TIME_PANEL = True

KEEP_ONLY_COMPLETED_AGENTS = True
USE_PCE_AS_WEIGHT = False

COMPLETE_EDGE_TIME_PANEL = True
WRITE_WIDE_TABLES = True

USE_MOE_FEATURES = False

K_NEAREST_NODES = 10
NODE_DISTANCE_RADIUS = None

MAKE_NETWORK_PREVIEW = True

OUTPUT_DIR = "processed_real_network"

EPS = 1e-9

# The official node file stores local projected x/y coordinates and does not
# include CRS metadata. These coefficients convert that local coordinate system
# to WGS84 for this Beijing network.
LOCAL_TO_WGS84_X_CENTER = 514717.3157977706
LOCAL_TO_WGS84_Y_CENTER = 291482.63714945794
LOCAL_TO_WGS84_SCALE = 10000.0
LOCAL_TO_WGS84_LON_COEFF = np.array([
    116.52269041899139,
    0.11674355596300813,
    0.0002240372945873046,
    -6.601902629810713e-07,
    0.00015207751767337745,
    5.792739454636199e-07,
])
LOCAL_TO_WGS84_LAT_COEFF = np.array([
    39.78920600957576,
    -0.00017282817403346755,
    0.09006307780008098,
    -5.8705286307285146e-05,
    -7.273031568614425e-07,
    -6.919121057481031e-07,
])


# ============================================================
# IO helpers
# ============================================================

def find_file(base_dir, candidates):
    base = Path(base_dir)
    lower_map = {p.name.lower(): p for p in base.iterdir() if p.is_file()}

    for name in candidates:
        p = base / name
        if p.exists():
            return p

    for name in candidates:
        p = lower_map.get(name.lower())
        if p is not None:
            return p

    return None


def read_csv_auto(path):
    encodings = ["utf-8-sig", "utf-8", "gbk", "latin1"]

    last_error = None
    for enc in encodings:
        try:
            df = pd.read_csv(
                path,
                encoding=enc,
                skipinitialspace=True,
                engine="python",
                on_bad_lines="warn"
            )
            df.columns = [str(c).strip() for c in df.columns]
            df = df.loc[:, ~pd.Index(df.columns).str.startswith("Unnamed")]
            df = df.loc[:, [str(c).strip() != "" for c in df.columns]]
            return df
        except Exception as e:
            last_error = e

    raise RuntimeError(f"Could not read {path}. Last error: {last_error}")


def to_num(s):
    return pd.to_numeric(s, errors="coerce")


def estimate_wgs84_from_local_xy(x_values, y_values):
    x = pd.to_numeric(x_values, errors="coerce").to_numpy(dtype=float)
    y = pd.to_numeric(y_values, errors="coerce").to_numpy(dtype=float)
    valid = np.isfinite(x) & np.isfinite(y)

    lon = np.full(len(x), np.nan, dtype=float)
    lat = np.full(len(x), np.nan, dtype=float)
    if valid.any():
        u = (x[valid] - LOCAL_TO_WGS84_X_CENTER) / LOCAL_TO_WGS84_SCALE
        v = (y[valid] - LOCAL_TO_WGS84_Y_CENTER) / LOCAL_TO_WGS84_SCALE
        features = np.column_stack([np.ones(len(u)), u, v, u * u, u * v, v * v])
        lon[valid] = features @ LOCAL_TO_WGS84_LON_COEFF
        lat[valid] = features @ LOCAL_TO_WGS84_LAT_COEFF

    return pd.Series(lon, index=getattr(x_values, "index", None)), pd.Series(lat, index=getattr(y_values, "index", None))


def clean_colnames(df):
    df = df.copy()
    df.columns = [str(c).strip() for c in df.columns]
    return df


def parse_semicolon_ints(value):
    if pd.isna(value):
        return []

    parts = str(value).replace('"', "").strip().split(";")
    out = []

    for p in parts:
        p = p.strip()
        if p == "":
            continue
        try:
            out.append(int(float(p)))
        except Exception:
            pass

    return out


def parse_semicolon_floats(value):
    if pd.isna(value):
        return []

    parts = str(value).replace('"', "").strip().split(";")
    out = []

    for p in parts:
        p = p.strip()
        if p == "":
            continue
        try:
            out.append(float(p))
        except Exception:
            pass

    return out


def safe_int(x):
    try:
        if pd.isna(x):
            return None
        return int(float(x))
    except Exception:
        return None


def get_attr(row, col):
    return getattr(row, col) if hasattr(row, col) else np.nan


# ============================================================
# Nodes
# ============================================================

def build_nodes(base_dir, out_dir):
    node_path = find_file(base_dir, ["input_node.csv"])

    if node_path is None:
        raise FileNotFoundError("Missing input_node.csv")

    nodes = read_csv_auto(node_path)
    nodes = clean_colnames(nodes)

    required = ["node_id", "x", "y"]
    for col in required:
        if col not in nodes.columns:
            raise ValueError(f"input_node.csv is missing required column: {col}")

    nodes["node_id"] = to_num(nodes["node_id"]).astype("Int64")
    nodes["x"] = to_num(nodes["x"])
    nodes["y"] = to_num(nodes["y"])

    keep_cols = [
        c for c in [
            "name",
            "node_id",
            "control_type",
            "control_type_name",
            "cycle_length_in_second",
            "x",
            "y",
            "geometry"
        ]
        if c in nodes.columns
    ]

    nodes = nodes[keep_cols].dropna(subset=["node_id"])
    nodes["node_id"] = nodes["node_id"].astype(int)
    nodes = nodes.drop_duplicates("node_id")

    estimated_lon, estimated_lat = estimate_wgs84_from_local_xy(nodes["x"], nodes["y"])
    nodes["x_84"] = estimated_lon
    nodes["y_84"] = estimated_lat

    nodes.to_csv(out_dir / "nodes_clean.csv", index=False)
    return nodes


# ============================================================
# Links
# ============================================================

def estimate_length_from_coords(from_node, to_node, coord_map):
    if from_node not in coord_map or to_node not in coord_map:
        return np.nan

    x1, y1 = coord_map[from_node]
    x2, y2 = coord_map[to_node]

    dist_meter = math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)
    return dist_meter / 1000.0


def build_links(base_dir, out_dir, nodes):
    link_path = find_file(base_dir, ["input_link.csv"])

    if link_path is None:
        raise FileNotFoundError("Missing input_link.csv")

    raw = read_csv_auto(link_path)
    raw = clean_colnames(raw)

    required = ["from_node_id", "to_node_id"]
    for col in required:
        if col not in raw.columns:
            raise ValueError(f"input_link.csv is missing required column: {col}")

    coord_map = {
        int(r.node_id): (float(r.x), float(r.y))
        for r in nodes[["node_id", "x", "y"]].dropna().itertuples(index=False)
    }

    rows = []

    for r in tqdm(raw.itertuples(index=False), total=len(raw), desc="Cleaning links"):
        from_node = safe_int(get_attr(r, "from_node_id"))
        to_node = safe_int(get_attr(r, "to_node_id"))

        if from_node is None or to_node is None:
            continue

        direction = safe_int(get_attr(r, "direction"))
        if direction is None:
            direction = 1

        if direction == -1:
            directed_pairs = [(to_node, from_node, "reversed")]
        elif direction in [0, 2]:
            directed_pairs = [
                (from_node, to_node, "forward"),
                (to_node, from_node, "backward")
            ]
        else:
            directed_pairs = [(from_node, to_node, "forward")]

        for u, v, direction_flag in directed_pairs:
            length = pd.to_numeric(get_attr(r, "length"), errors="coerce")

            if pd.isna(length) or length <= 0:
                length = estimate_length_from_coords(u, v, coord_map)

            rows.append({
                "from_node_id": u,
                "to_node_id": v,
                "direction_flag": direction_flag,
                "original_link_id": get_attr(r, "link_id"),
                "original_link_key": get_attr(r, "link_key"),
                "name": get_attr(r, "name"),
                "link_type_name": get_attr(r, "link_type_name"),
                "length": length,
                "number_of_lanes": get_attr(r, "number_of_lanes"),
                "speed_limit": get_attr(r, "speed_limit"),
                "lane_capacity_in_vhc_per_hour": get_attr(r, "lane_capacity_in_vhc_per_hour"),
                "link_type": get_attr(r, "link_type"),
                "jam_density": get_attr(r, "jam_density"),
                "wave_speed": get_attr(r, "wave_speed"),
                "geometry": get_attr(r, "geometry"),
                "original_geometry": get_attr(r, "original_geometry")
            })

    directed = pd.DataFrame(rows)

    if len(directed) == 0:
        raise ValueError("No valid links were created from input_link.csv")

    numeric_cols = [
        "length",
        "number_of_lanes",
        "speed_limit",
        "lane_capacity_in_vhc_per_hour",
        "link_type",
        "jam_density",
        "wave_speed"
    ]

    for c in numeric_cols:
        if c in directed.columns:
            directed[c] = to_num(directed[c])

    agg_dict = {
        "length": "min",
        "number_of_lanes": "sum",
        "speed_limit": "mean",
        "lane_capacity_in_vhc_per_hour": "sum",
        "link_type": "first",
        "jam_density": "mean",
        "wave_speed": "mean",
        "name": "first",
        "link_type_name": "first",
        "geometry": "first",
        "original_geometry": "first",
        "direction_flag": lambda x: ";".join(sorted(set(map(str, x)))),
        "original_link_id": lambda x: ";".join(map(str, x)),
        "original_link_key": lambda x: ";".join(map(str, x))
    }

    links = (
        directed
        .groupby(["from_node_id", "to_node_id"], as_index=False)
        .agg(agg_dict)
    )

    links = links.sort_values(["from_node_id", "to_node_id"]).reset_index(drop=True)
    links.insert(0, "edge_index", np.arange(len(links)))
    links["edge_id"] = links["from_node_id"].astype(str) + "->" + links["to_node_id"].astype(str)

    links.to_csv(out_dir / "links_clean.csv", index=False)
    return links


# ============================================================
# x_ijt from output_agent.csv
# ============================================================

def get_time_bins_for_segment(t_enter, t_exit):
    """
    For EDGE_FLOW_MODE = "occupancy":
    Count all bins overlapped by [t_enter, t_exit).

    Example if TIME_BIN_MIN = 1:
        [1.8, 2.2) -> bins 1 and 2
        [1.8, 2.0) -> bin 1 only
        [2.0, 2.2) -> bin 2 only
    """

    if not np.isfinite(t_enter) or not np.isfinite(t_exit):
        return []

    if t_enter < 0 and t_exit < 0:
        return []

    if t_exit < t_enter:
        return []

    panel_start = 0.0
    panel_end = FULL_DAY_MINUTES if FORCE_FULL_DAY and CLIP_FLOW_TO_TIME_PANEL else None

    start = max(t_enter, panel_start)
    end = t_exit

    if panel_end is not None:
        end = min(end, panel_end)

    if end < start:
        return []

    if abs(end - start) <= EPS:
        if panel_end is not None and start >= panel_end:
            return []
        return [int(math.floor(start / TIME_BIN_MIN))]

    start_bin = int(math.floor(start / TIME_BIN_MIN))
    end_bin = int(math.floor((end - EPS) / TIME_BIN_MIN))

    if end_bin < start_bin:
        return []

    return list(range(start_bin, end_bin + 1))


def get_time_bin_for_entry(t_enter):
    if not np.isfinite(t_enter) or t_enter < 0:
        return None

    if FORCE_FULL_DAY and CLIP_FLOW_TO_TIME_PANEL:
        if t_enter >= FULL_DAY_MINUTES:
            return None

    return int(math.floor(t_enter / TIME_BIN_MIN))


def build_edge_flow(base_dir, out_dir, links):
    agent_path = find_file(base_dir, ["output_agent.csv", "Output_agent.csv"])

    if agent_path is None:
        raise FileNotFoundError("Missing output_agent.csv")

    if EDGE_FLOW_MODE not in ["entry", "occupancy"]:
        raise ValueError("EDGE_FLOW_MODE must be either 'entry' or 'occupancy'")

    agents = read_csv_auto(agent_path)
    agents = clean_colnames(agents)

    required = ["path_node_sequence", "path_time_sequence"]
    for col in required:
        if col not in agents.columns:
            raise ValueError(f"output_agent.csv is missing required column: {col}")

    pair_to_edge_id = {
        (int(r.from_node_id), int(r.to_node_id)): r.edge_id
        for r in links[["from_node_id", "to_node_id", "edge_id"]].itertuples(index=False)
    }

    pair_to_edge_index = {
        (int(r.from_node_id), int(r.to_node_id)): int(r.edge_index)
        for r in links[["from_node_id", "to_node_id", "edge_index"]].itertuples(index=False)
    }

    valid_pairs = set(pair_to_edge_id.keys())

    flow_counter = defaultdict(float)
    missing_counter = Counter()

    total_agents = 0
    used_agents = 0
    bad_agents = 0

    total_edge_traversals = 0
    used_edge_traversals = 0
    missing_edge_traversals = 0

    counted_edge_segments = 0
    skipped_time_segments = 0
    bad_time_segments = 0
    occupancy_bin_updates = 0

    max_time_seen = 0.0

    has_complete_flag = "complete_flag" in agents.columns
    has_pce = "PCE" in agents.columns

    for r in tqdm(agents.itertuples(index=False), total=len(agents), desc="Parsing agent paths"):
        total_agents += 1

        if KEEP_ONLY_COMPLETED_AGENTS and has_complete_flag:
            flag = str(get_attr(r, "complete_flag")).strip().lower()
            if flag != "c":
                continue

        nodes_seq = parse_semicolon_ints(get_attr(r, "path_node_sequence"))
        times_seq = parse_semicolon_floats(get_attr(r, "path_time_sequence"))

        m = min(len(nodes_seq), len(times_seq))

        if m < 2:
            bad_agents += 1
            continue

        used_agents += 1

        if len(times_seq) > 0:
            max_time_seen = max(max_time_seen, max(times_seq))

        if USE_PCE_AS_WEIGHT and has_pce:
            try:
                weight = float(get_attr(r, "PCE"))
            except Exception:
                weight = 1.0
        else:
            weight = 1.0

        for k in range(m - 1):
            u = nodes_seq[k]
            v = nodes_seq[k + 1]
            t_enter = times_seq[k]
            t_exit = times_seq[k + 1]

            if not np.isfinite(t_enter) or not np.isfinite(t_exit):
                bad_time_segments += 1
                continue

            if t_exit < t_enter:
                bad_time_segments += 1
                continue

            total_edge_traversals += 1

            if (u, v) not in valid_pairs:
                missing_counter[(u, v)] += 1
                missing_edge_traversals += 1
                continue

            used_edge_traversals += 1

            if EDGE_FLOW_MODE == "entry":
                time_bin = get_time_bin_for_entry(t_enter)

                if time_bin is None:
                    skipped_time_segments += 1
                    continue

                flow_counter[(time_bin, u, v)] += weight
                counted_edge_segments += 1
                occupancy_bin_updates += 1

            else:
                bins = get_time_bins_for_segment(t_enter, t_exit)

                if len(bins) == 0:
                    skipped_time_segments += 1
                    continue

                counted_edge_segments += 1

                for time_bin in bins:
                    flow_counter[(time_bin, u, v)] += weight
                    occupancy_bin_updates += 1

    rows = []

    for (time_bin, u, v), val in tqdm(flow_counter.items(), total=len(flow_counter), desc="Writing nonzero x_ijt"):
        rows.append({
            "time_bin": time_bin,
            "start_min": time_bin * TIME_BIN_MIN,
            "end_min": (time_bin + 1) * TIME_BIN_MIN,
            "edge_index": pair_to_edge_index[(u, v)],
            "edge_id": pair_to_edge_id[(u, v)],
            "from_node_id": u,
            "to_node_id": v,
            "x_ijt": val
        })

    flow_nz = pd.DataFrame(rows)

    if len(flow_nz) == 0:
        flow_nz = pd.DataFrame(
            columns=[
                "time_bin",
                "start_min",
                "end_min",
                "edge_index",
                "edge_id",
                "from_node_id",
                "to_node_id",
                "x_ijt"
            ]
        )

    flow_nz = flow_nz.sort_values(["time_bin", "from_node_id", "to_node_id"]).reset_index(drop=True)
    flow_nz.to_csv(out_dir / "edge_flow_xijt_nonzero.csv", index=False)

    if FORCE_FULL_DAY:
        n_bins = int(math.ceil(FULL_DAY_MINUTES / TIME_BIN_MIN))
    else:
        max_bin = int(math.floor(max_time_seen / TIME_BIN_MIN)) if max_time_seen > 0 else 0
        max_bin_from_flow = int(flow_nz["time_bin"].max()) if len(flow_nz) > 0 else 0
        n_bins = max(max_bin, max_bin_from_flow) + 1

    time_df = pd.DataFrame({"time_bin": np.arange(n_bins, dtype=int)})
    time_df["start_min"] = time_df["time_bin"] * TIME_BIN_MIN
    time_df["end_min"] = (time_df["time_bin"] + 1) * TIME_BIN_MIN

    if COMPLETE_EDGE_TIME_PANEL:
        print("Building complete edge-time panel...")

        time_idx = pd.DataFrame({
            "time_bin": np.repeat(time_df["time_bin"].values, len(links)),
            "start_min": np.repeat(time_df["start_min"].values, len(links)),
            "end_min": np.repeat(time_df["end_min"].values, len(links))
        })

        link_idx = pd.concat(
            [links[["edge_index", "edge_id", "from_node_id", "to_node_id"]]] * len(time_df),
            ignore_index=True
        )

        panel = pd.concat([time_idx, link_idx], axis=1)

        panel = panel.merge(
            flow_nz[["time_bin", "from_node_id", "to_node_id", "x_ijt"]],
            on=["time_bin", "from_node_id", "to_node_id"],
            how="left"
        )

        panel["x_ijt"] = panel["x_ijt"].fillna(0.0)

        panel = panel[
            [
                "time_bin",
                "start_min",
                "end_min",
                "edge_index",
                "edge_id",
                "from_node_id",
                "to_node_id",
                "x_ijt"
            ]
        ]

        panel.to_csv(out_dir / "edge_flow_xijt.csv", index=False)
        flow_for_downstream = panel
    else:
        flow_nz.to_csv(out_dir / "edge_flow_xijt.csv", index=False)
        flow_for_downstream = flow_nz

    missing_rows = [
        {
            "from_node_id": u,
            "to_node_id": v,
            "count": cnt
        }
        for (u, v), cnt in missing_counter.items()
    ]

    missing_df = pd.DataFrame(missing_rows)

    if len(missing_df) > 0:
        missing_df = missing_df.sort_values("count", ascending=False)

    missing_df.to_csv(out_dir / "missing_path_edges.csv", index=False)

    summary = {
        "edge_flow_mode": EDGE_FLOW_MODE,
        "total_agents": total_agents,
        "used_completed_agents": used_agents,
        "bad_agents": bad_agents,

        "total_edge_traversals": total_edge_traversals,
        "used_edge_traversals": used_edge_traversals,
        "missing_edge_traversals": missing_edge_traversals,

        "counted_edge_segments_after_time_filter": counted_edge_segments,
        "skipped_time_segments": skipped_time_segments,
        "bad_time_segments": bad_time_segments,
        "occupancy_bin_updates": occupancy_bin_updates,

        "time_bin_minutes": TIME_BIN_MIN,
        "number_of_time_bins": n_bins,
        "max_time_seen_in_agent_paths": max_time_seen
    }

    return flow_for_downstream, flow_nz, time_df, summary


# ============================================================
# x_it
# ============================================================

def build_node_flow(out_dir, nodes, flow_nz, time_df):
    node_ids = nodes["node_id"].dropna().astype(int).sort_values().unique()

    out_flow = (
        flow_nz
        .groupby(["time_bin", "from_node_id"], as_index=False)["x_ijt"]
        .sum()
        .rename(columns={"from_node_id": "node_id", "x_ijt": "out_flow"})
    )

    in_flow = (
        flow_nz
        .groupby(["time_bin", "to_node_id"], as_index=False)["x_ijt"]
        .sum()
        .rename(columns={"to_node_id": "node_id", "x_ijt": "in_flow"})
    )

    print("Building complete node-time panel...")

    time_idx = pd.DataFrame({
        "time_bin": np.repeat(time_df["time_bin"].values, len(node_ids)),
        "start_min": np.repeat(time_df["start_min"].values, len(node_ids)),
        "end_min": np.repeat(time_df["end_min"].values, len(node_ids))
    })

    node_idx = pd.DataFrame({
        "node_id": np.tile(node_ids, len(time_df))
    })

    panel = pd.concat([time_idx, node_idx], axis=1)

    panel = panel.merge(out_flow, on=["time_bin", "node_id"], how="left")
    panel = panel.merge(in_flow, on=["time_bin", "node_id"], how="left")

    panel["out_flow"] = panel["out_flow"].fillna(0.0)
    panel["in_flow"] = panel["in_flow"].fillna(0.0)
    panel["net_flow"] = panel["out_flow"] - panel["in_flow"]

    panel = panel[
        [
            "time_bin",
            "start_min",
            "end_min",
            "node_id",
            "in_flow",
            "out_flow",
            "net_flow"
        ]
    ]

    panel.to_csv(out_dir / "node_flow_xit.csv", index=False)
    return panel


# ============================================================
# Optional Output_LinkTDMOE.csv processing
# Disabled by default
# ============================================================

def build_moe_features(base_dir, out_dir, flow_nz):
    moe_path = find_file(base_dir, ["output_LinkTDMOE.csv", "Output_LinkTDMOE.csv", "output_linkTDMOE.csv"])

    if moe_path is None:
        return None, None

    moe = read_csv_auto(moe_path)
    moe = clean_colnames(moe)

    required = ["from_node_id", "to_node_id", "timestamp_in_min"]
    for col in required:
        if col not in moe.columns:
            raise ValueError(f"Output_LinkTDMOE.csv is missing required column: {col}")

    moe["from_node_id"] = to_num(moe["from_node_id"]).astype("Int64")
    moe["to_node_id"] = to_num(moe["to_node_id"]).astype("Int64")
    moe["timestamp_in_min"] = to_num(moe["timestamp_in_min"])

    moe = moe.dropna(subset=["from_node_id", "to_node_id", "timestamp_in_min"])
    moe["from_node_id"] = moe["from_node_id"].astype(int)
    moe["to_node_id"] = moe["to_node_id"].astype(int)

    moe["time_bin"] = np.floor(moe["timestamp_in_min"] / TIME_BIN_MIN).astype(int)
    moe["start_min"] = moe["time_bin"] * TIME_BIN_MIN
    moe["end_min"] = (moe["time_bin"] + 1) * TIME_BIN_MIN
    moe["edge_id"] = moe["from_node_id"].astype(str) + "->" + moe["to_node_id"].astype(str)

    numeric_cols = [
        c for c in moe.columns
        if c not in [
            "from_node_id",
            "to_node_id",
            "link_id_from_to",
            "edge_id",
            "time_bin",
            "start_min",
            "end_min"
        ]
    ]

    for c in tqdm(numeric_cols, desc="Converting MOE columns"):
        moe[c] = to_num(moe[c])

    sum_cols = [
        "link_in_volume_number_of_veh",
        "link_out_volume_number_of_veh",
        "link_volume_in_veh_per_hour_per_lane",
        "link_volume_in_veh_per_hour_for_all_lanes",
        "total_energy",
        "total_CO2",
        "total_NOX",
        "total_CO",
        "total_HC"
    ]

    mean_cols = [
        "travel_time_in_min",
        "delay_in_min",
        "density_in_veh_per_distance_per_lane",
        "speed",
        "queue_length_percentage",
        "number_of_queued_vehicles"
    ]

    max_cols = [
        "cumulative_arrival_count",
        "cumulative_departure_count"
    ]

    agg = {}

    for c in sum_cols:
        if c in moe.columns:
            agg[c] = "sum"

    for c in mean_cols:
        if c in moe.columns:
            agg[c] = "mean"

    for c in max_cols:
        if c in moe.columns:
            agg[c] = "max"

    if "day_no" in moe.columns:
        agg["day_no"] = "first"

    print("Aggregating MOE features...")

    grouped = (
        moe
        .groupby(["time_bin", "start_min", "end_min", "from_node_id", "to_node_id", "edge_id"], as_index=False)
        .agg(agg)
    )

    grouped.to_csv(out_dir / "edge_moe_features.csv", index=False)

    if "link_in_volume_number_of_veh" in grouped.columns:
        cmp_df = grouped[
            [
                "time_bin",
                "start_min",
                "end_min",
                "from_node_id",
                "to_node_id",
                "edge_id",
                "link_in_volume_number_of_veh"
            ]
        ].merge(
            flow_nz[["time_bin", "from_node_id", "to_node_id", "x_ijt"]],
            on=["time_bin", "from_node_id", "to_node_id"],
            how="left"
        )

        cmp_df["x_ijt"] = cmp_df["x_ijt"].fillna(0.0)
        cmp_df["agent_minus_moe_link_in"] = cmp_df["x_ijt"] - cmp_df["link_in_volume_number_of_veh"]
        cmp_df["abs_diff"] = cmp_df["agent_minus_moe_link_in"].abs()

        cmp_df.to_csv(out_dir / "edge_flow_vs_moe_check.csv", index=False)
    else:
        cmp_df = None

    return grouped, cmp_df


# ============================================================
# Neighbors
# ============================================================

def dijkstra_k_nearest(source, adjacency, k_nearest=10, max_radius=None):
    dist = {source: 0.0}
    heap = [(0.0, source)]
    result = []

    while heap:
        d, u = heapq.heappop(heap)

        if d > dist.get(u, float("inf")):
            continue

        if max_radius is not None and d > max_radius:
            continue

        if u != source:
            result.append((u, d))

            if max_radius is None and k_nearest is not None and len(result) >= k_nearest:
                break

        for v, w in adjacency.get(u, []):
            nd = d + w

            if max_radius is not None and nd > max_radius:
                continue

            if nd < dist.get(v, float("inf")):
                dist[v] = nd
                heapq.heappush(heap, (nd, v))

    return result


def build_node_neighbors(out_dir, nodes, links):
    adjacency = defaultdict(list)

    for r in links[["from_node_id", "to_node_id", "length"]].itertuples(index=False):
        u = int(r.from_node_id)
        v = int(r.to_node_id)
        length = float(r.length) if pd.notna(r.length) else 1.0
        adjacency[u].append((v, length))

    rows = []

    for r in links[["from_node_id", "to_node_id", "length"]].itertuples(index=False):
        rows.append({
            "node_id": int(r.from_node_id),
            "neighbor_node_id": int(r.to_node_id),
            "neighbor_type": "direct_out",
            "hop_distance": 1,
            "network_distance": float(r.length) if pd.notna(r.length) else np.nan,
            "rank": np.nan
        })

    node_ids = nodes["node_id"].dropna().astype(int).sort_values().unique()

    for src in tqdm(node_ids, desc="Node shortest paths"):
        nearest = dijkstra_k_nearest(
            source=int(src),
            adjacency=adjacency,
            k_nearest=K_NEAREST_NODES,
            max_radius=NODE_DISTANCE_RADIUS
        )

        for rank, (dst, d) in enumerate(nearest, start=1):
            rows.append({
                "node_id": int(src),
                "neighbor_node_id": int(dst),
                "neighbor_type": f"shortest_path_top_{K_NEAREST_NODES}",
                "hop_distance": np.nan,
                "network_distance": d,
                "rank": rank
            })

    neighbors = pd.DataFrame(rows)

    neighbors = neighbors.drop_duplicates(
        ["node_id", "neighbor_node_id", "neighbor_type"]
    )

    neighbors = neighbors.sort_values(
        ["node_id", "neighbor_type", "network_distance", "neighbor_node_id"]
    )

    neighbors.to_csv(out_dir / "node_neighbors.csv", index=False)
    return neighbors


def build_edge_neighbors(out_dir, links):
    incoming = defaultdict(list)
    outgoing = defaultdict(list)
    edge_info = {}

    for r in links[["edge_index", "edge_id", "from_node_id", "to_node_id", "length"]].itertuples(index=False):
        edge_index = int(r.edge_index)
        u = int(r.from_node_id)
        v = int(r.to_node_id)
        length = float(r.length) if pd.notna(r.length) else np.nan

        edge_info[edge_index] = {
            "edge_id": r.edge_id,
            "from_node_id": u,
            "to_node_id": v,
            "length": length
        }

        outgoing[u].append(edge_index)
        incoming[v].append(edge_index)

    rows = []

    def is_reverse_edge(e, n):
        return (
            e["from_node_id"] == n["to_node_id"]
            and e["to_node_id"] == n["from_node_id"]
        )

    def center_distance(e, n):
        if pd.isna(e["length"]) or pd.isna(n["length"]):
            return np.nan
        return 0.5 * e["length"] + 0.5 * n["length"]

    def add_relation(edge_index, neighbor_index, relation_type, shared_node_id):
        if edge_index == neighbor_index:
            return

        e = edge_info[edge_index]
        n = edge_info[neighbor_index]

        rows.append({
            "edge_index": edge_index,
            "edge_id": e["edge_id"],
            "from_node_id": e["from_node_id"],
            "to_node_id": e["to_node_id"],
            "edge_length": e["length"],
            "neighbor_edge_index": neighbor_index,
            "neighbor_edge_id": n["edge_id"],
            "neighbor_from_node_id": n["from_node_id"],
            "neighbor_to_node_id": n["to_node_id"],
            "neighbor_edge_length": n["length"],
            "relation_type": relation_type,
            "shared_node_id": shared_node_id,
            "network_distance": center_distance(e, n),
            "distance_type": "local_connected_edge_center_distance",
            "is_reverse_edge": is_reverse_edge(e, n)
        })

    for edge_index, e in tqdm(edge_info.items(), total=len(edge_info), desc="Edge neighbors"):
        u = e["from_node_id"]
        v = e["to_node_id"]

        for nb in incoming[u]:
            add_relation(edge_index, nb, "upstream", u)

        for nb in outgoing[v]:
            add_relation(edge_index, nb, "downstream", v)

        for nb in outgoing[u]:
            add_relation(edge_index, nb, "share_from_node", u)

        for nb in incoming[v]:
            add_relation(edge_index, nb, "share_to_node", v)

    edge_neighbors = pd.DataFrame(rows)

    if len(edge_neighbors) > 0:
        edge_neighbors = edge_neighbors.drop_duplicates(
            ["edge_index", "neighbor_edge_index", "relation_type"]
        )

        edge_neighbors = edge_neighbors.sort_values(
            ["edge_index", "relation_type", "network_distance", "neighbor_edge_index"]
        )

    edge_neighbors.to_csv(out_dir / "edge_neighbors.csv", index=False)
    return edge_neighbors


# ============================================================
# Wide tables
# ============================================================

def write_wide_tables(out_dir, edge_flow, node_flow):
    if not WRITE_WIDE_TABLES:
        return

    print("Writing edge_flow_xijt_wide.csv...")

    ef = edge_flow.copy()
    ef["var_name"] = "x" + ef["from_node_id"].astype(str) + "-" + ef["to_node_id"].astype(str) + "t"

    edge_wide = (
        ef
        .pivot_table(
            index=["time_bin", "start_min", "end_min"],
            columns="var_name",
            values="x_ijt",
            aggfunc="sum",
            fill_value=0.0
        )
        .reset_index()
    )

    edge_wide.columns.name = None
    edge_wide.to_csv(out_dir / "edge_flow_xijt_wide.csv", index=False)

    print("Writing node wide tables...")

    nf = node_flow.copy()
    nf["var_name"] = "x" + nf["node_id"].astype(str) + "t"

    for value_col, file_name in [
        ("net_flow", "node_flow_xit_net_wide.csv"),
        ("in_flow", "node_flow_xit_in_wide.csv"),
        ("out_flow", "node_flow_xit_out_wide.csv")
    ]:
        wide = (
            nf
            .pivot_table(
                index=["time_bin", "start_min", "end_min"],
                columns="var_name",
                values=value_col,
                aggfunc="sum",
                fill_value=0.0
            )
            .reset_index()
        )

        wide.columns.name = None
        wide.to_csv(out_dir / file_name, index=False)


# ============================================================
# Preview plot
# ============================================================

def write_network_preview(out_dir, nodes, links):
    if not MAKE_NETWORK_PREVIEW:
        return

    try:
        import matplotlib.pyplot as plt
        from matplotlib.collections import LineCollection
    except Exception:
        print("matplotlib is not available. Skipped road_network_preview.png.")
        return

    coord = {
        int(r.node_id): (float(r.x), float(r.y))
        for r in nodes[["node_id", "x", "y"]].dropna().itertuples(index=False)
    }

    segments = []

    for r in tqdm(links[["from_node_id", "to_node_id"]].itertuples(index=False), total=len(links), desc="Preparing network plot"):
        u = int(r.from_node_id)
        v = int(r.to_node_id)

        if u in coord and v in coord:
            segments.append([coord[u], coord[v]])

    if len(segments) == 0:
        return

    fig, ax = plt.subplots(figsize=(10, 10))

    lc = LineCollection(segments, linewidths=0.25, alpha=0.45)
    ax.add_collection(lc)

    xy = nodes[["x", "y"]].dropna()

    if len(xy) <= 20000:
        ax.scatter(xy["x"], xy["y"], s=1, alpha=0.4)

    ax.autoscale()
    ax.set_aspect("equal", adjustable="box")
    ax.set_title("Road Network Preview")
    ax.set_xlabel("x")
    ax.set_ylabel("y")

    fig.tight_layout()
    fig.savefig(out_dir / "road_network_preview.png", dpi=300)
    plt.close(fig)


# ============================================================
# Summary
# ============================================================

def write_summary(out_dir, summary_dict):
    lines = []

    for k, v in summary_dict.items():
        lines.append(f"{k}: {v}")

    with open(out_dir / "processing_summary.txt", "w", encoding="utf-8") as f:
        f.write("\n".join(lines))


# ============================================================
# Main
# ============================================================

def main():
    base_dir = Path(".").resolve()
    out_dir = base_dir / OUTPUT_DIR
    out_dir.mkdir(parents=True, exist_ok=True)

    print("\nStep 1/9: Reading and cleaning nodes...")
    nodes = build_nodes(base_dir, out_dir)
    print(f"Saved nodes_clean.csv. Nodes: {len(nodes)}")

    print("\nStep 2/9: Reading and cleaning directed links...")
    links = build_links(base_dir, out_dir, nodes)
    print(f"Saved links_clean.csv. Directed edges: {len(links)}")

    print(f"\nStep 3/9: Building edge flow x_ijt from output_agent.csv...")
    print(f"Current EDGE_FLOW_MODE = {EDGE_FLOW_MODE}")
    edge_flow, edge_flow_nz, time_df, flow_summary = build_edge_flow(base_dir, out_dir, links)
    print(f"Saved edge_flow_xijt.csv. Rows: {len(edge_flow)}")
    print(f"Saved edge_flow_xijt_nonzero.csv. Nonzero rows: {len(edge_flow_nz)}")

    print("\nStep 4/9: Building node flow x_it...")
    node_flow = build_node_flow(out_dir, nodes, edge_flow_nz, time_df)
    print(f"Saved node_flow_xit.csv. Rows: {len(node_flow)}")

    print("\nStep 5/9: Aggregating Output_LinkTDMOE.csv if available...")

    if USE_MOE_FEATURES:
        moe_features, moe_check = build_moe_features(base_dir, out_dir, edge_flow_nz)

        if moe_features is not None:
            print(f"Saved edge_moe_features.csv. Rows: {len(moe_features)}")

            if moe_check is not None:
                print(f"Saved edge_flow_vs_moe_check.csv. Rows: {len(moe_check)}")
        else:
            print("Output_LinkTDMOE.csv not found. Skipped MOE features.")
    else:
        print("Skipped Output_LinkTDMOE.csv because USE_MOE_FEATURES = False.")

    print("\nStep 6/9: Building node neighbors...")
    node_neighbors = build_node_neighbors(out_dir, nodes, links)
    print(f"Saved node_neighbors.csv. Rows: {len(node_neighbors)}")

    print("\nStep 7/9: Building edge neighbors...")
    edge_neighbors = build_edge_neighbors(out_dir, links)
    print(f"Saved edge_neighbors.csv. Rows: {len(edge_neighbors)}")

    print("\nStep 8/9: Writing wide tables...")
    write_wide_tables(out_dir, edge_flow, node_flow)

    print("\nStep 9/9: Writing network preview...")
    write_network_preview(out_dir, nodes, links)

    summary = {
        "base_dir": str(base_dir),
        "output_dir": str(out_dir),
        "TIME_BIN_MIN": TIME_BIN_MIN,
        "EDGE_FLOW_MODE": EDGE_FLOW_MODE,
        "FORCE_FULL_DAY": FORCE_FULL_DAY,
        "FULL_DAY_MINUTES": FULL_DAY_MINUTES,
        "CLIP_FLOW_TO_TIME_PANEL": CLIP_FLOW_TO_TIME_PANEL,
        "USE_MOE_FEATURES": USE_MOE_FEATURES,
        "number_of_nodes": len(nodes),
        "number_of_directed_edges": len(links),
        "edge_flow_rows": len(edge_flow),
        "edge_flow_nonzero_rows": len(edge_flow_nz),
        "node_flow_rows": len(node_flow),
        "node_neighbor_rows": len(node_neighbors),
        "edge_neighbor_rows": len(edge_neighbors),
    }

    summary.update(flow_summary)
    write_summary(out_dir, summary)

    print("\nDone.")
    print(f"All outputs are in: {out_dir}")


if __name__ == "__main__":
    main()
