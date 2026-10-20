"""Regenerate the full multi-scale Divvy map.

The map includes nine spatial aggregation levels and 1/2/3-hour time windows.
The 1.5–4 km files are rebuilt from the existing 1 km nodes and flows.
"""

from pathlib import Path
import json

import numpy as np
import pandas as pd


BASE_DIR = Path(__file__).resolve().parent
OUTPUT_FILE = BASE_DIR / "divvy_multi_scale.html"

NODE_FILES = {
    "micro": "nodes_raw.csv",
    "macro_500m": "nodes_r500.csv",
    "macro_1km": "nodes_r1000.csv",
    "macro_1_5km": "nodes_r1500.csv",
    "macro_2km": "nodes_r2000.csv",
    "macro_2_5km": "nodes_r2500.csv",
    "macro_3km": "nodes_r3000.csv",
    "macro_3_5km": "nodes_r3500.csv",
    "macro_4km": "nodes_r4000.csv",
}

FLOW_FILES = {
    "micro": "od_flow_2026_01_dt01h_raw.csv",
    "macro_500m": "od_flow_2026_01_dt01h_r500.csv",
    "macro_1km": "od_flow_2026_01_dt01h_r1000.csv",
    "macro_1_5km": "od_flow_2026_01_dt01h_r1500.csv",
    "macro_2km": "od_flow_2026_01_dt01h_r2000.csv",
    "macro_2_5km": "od_flow_2026_01_dt01h_r2500.csv",
    "macro_3km": "od_flow_2026_01_dt01h_r3000.csv",
    "macro_3_5km": "od_flow_2026_01_dt01h_r3500.csv",
    "macro_4km": "od_flow_2026_01_dt01h_r4000.csv",
}

LARGE_SCALES = {
    1500: (NODE_FILES["macro_1_5km"], FLOW_FILES["macro_1_5km"]),
    2000: (NODE_FILES["macro_2km"], FLOW_FILES["macro_2km"]),
    2500: (NODE_FILES["macro_2_5km"], FLOW_FILES["macro_2_5km"]),
    3000: (NODE_FILES["macro_3km"], FLOW_FILES["macro_3km"]),
    3500: (NODE_FILES["macro_3_5km"], FLOW_FILES["macro_3_5km"]),
    4000: (NODE_FILES["macro_4km"], FLOW_FILES["macro_4km"]),
}


def haversine_distance_matrix(frame):
    """Return pairwise great-circle distances in metres."""
    latitude = np.radians(frame["latitude"].to_numpy(dtype=float))
    longitude = np.radians(frame["longitude"].to_numpy(dtype=float))
    latitude_delta = latitude[:, None] - latitude[None, :]
    longitude_delta = longitude[:, None] - longitude[None, :]
    haversine = (
        np.sin(latitude_delta / 2) ** 2
        + np.cos(latitude[:, None])
        * np.cos(latitude[None, :])
        * np.sin(longitude_delta / 2) ** 2
    )
    return 2 * 6_371_008.8 * np.arcsin(np.minimum(1, np.sqrt(haversine)))


def build_large_scale_files():
    """Build deterministic 1.5–4 km radius aggregates from the 1 km data."""
    base_nodes = pd.read_csv(BASE_DIR / NODE_FILES["macro_1km"])
    base_nodes = base_nodes[base_nodes["coordinate_available"] == True].copy()
    base_nodes = base_nodes.sort_values("node_idx").reset_index(drop=True)
    base_flows = pd.read_csv(BASE_DIR / FLOW_FILES["macro_1km"])
    distances = haversine_distance_matrix(base_nodes)

    # Dense 1 km nodes become representatives first. This keeps major activity
    # centres visible while ensuring every source node is within the radius.
    candidate_order = sorted(
        range(len(base_nodes)),
        key=lambda index: (
            -int(base_nodes.iloc[index]["n_raw_stations"]),
            int(base_nodes.iloc[index]["node_idx"]),
        ),
    )

    for radius_m, (node_filename, flow_filename) in LARGE_SCALES.items():
        representative_positions = []
        for position in candidate_order:
            if not representative_positions:
                representative_positions.append(position)
                continue
            if distances[position, representative_positions].min() > radius_m:
                representative_positions.append(position)

        nearest_representative = np.argmin(
            distances[:, representative_positions], axis=1
        )
        old_to_new = {
            int(base_nodes.iloc[position]["node_idx"]): int(new_index)
            for position, new_index in enumerate(nearest_representative)
        }

        node_records = []
        for new_index, representative_position in enumerate(
            representative_positions
        ):
            representative = base_nodes.iloc[representative_position]
            members = base_nodes[nearest_representative == new_index]
            node_records.append(
                {
                    "node_idx": new_index,
                    "node_id": representative["node_id"],
                    "representative_station_id": representative[
                        "representative_station_id"
                    ],
                    "latitude": representative["latitude"],
                    "longitude": representative["longitude"],
                    "n_raw_stations": int(members["n_raw_stations"].sum()),
                    "display_name": representative["display_name"],
                    "coordinate_available": True,
                }
            )

        aggregated_flows = base_flows.copy()
        aggregated_flows["origin_node_idx"] = aggregated_flows[
            "origin_node_idx"
        ].map(old_to_new)
        aggregated_flows["destination_node_idx"] = aggregated_flows[
            "destination_node_idx"
        ].map(old_to_new)
        if aggregated_flows[
            ["origin_node_idx", "destination_node_idx"]
        ].isna().any().any():
            raise ValueError(f"Unmapped 1 km node while building {radius_m} m data")
        aggregated_flows = (
            aggregated_flows.groupby(
                ["timestamp", "origin_node_idx", "destination_node_idx"],
                as_index=False,
                sort=True,
            )["trip_count"]
            .sum()
            .sort_values(
                ["timestamp", "origin_node_idx", "destination_node_idx"]
            )
        )
        aggregated_flows[["origin_node_idx", "destination_node_idx"]] = (
            aggregated_flows[
                ["origin_node_idx", "destination_node_idx"]
            ].astype(int)
        )

        if int(aggregated_flows["trip_count"].sum()) != int(
            base_flows["trip_count"].sum()
        ):
            raise ValueError(f"Trip total changed while building {radius_m} m data")

        pd.DataFrame(node_records).to_csv(
            BASE_DIR / node_filename, index=False
        )
        aggregated_flows.to_csv(BASE_DIR / flow_filename, index=False)
        radius_label = f"{radius_m / 1000:g}"
        print(
            f"   {radius_label} km: {len(node_records)} nodes, "
            f"{len(aggregated_flows)} flow rows"
        )


def read_nodes(filename):
    frame = pd.read_csv(BASE_DIR / filename)
    frame = frame[frame["coordinate_available"] == True].copy()
    return [
        {
            "id": int(row.node_idx),
            "name": row.display_name
            if pd.notna(row.display_name)
            else str(row.node_id),
            "lat": float(row.latitude),
            "lng": float(row.longitude),
            "st_count": int(row.n_raw_stations),
        }
        for row in frame.itertuples(index=False)
    ]


def read_flows(filename, valid_node_ids):
    frame = pd.read_csv(BASE_DIR / filename)
    frame = frame[
        frame["origin_node_idx"].isin(valid_node_ids)
        & frame["destination_node_idx"].isin(valid_node_ids)
    ].copy()
    frame["hour"] = pd.to_datetime(frame["timestamp"]).dt.hour
    return frame


def aggregate_routes(frame, start_hour, interval_hours):
    subset = frame[
        (frame["hour"] >= start_hour)
        & (frame["hour"] < start_hour + interval_hours)
        & (frame["origin_node_idx"] != frame["destination_node_idx"])
    ]
    grouped = (
        subset.groupby(
            ["origin_node_idx", "destination_node_idx"], sort=True
        )["trip_count"]
        .sum()
        .reset_index()
    )
    return (
        grouped[
            ["origin_node_idx", "destination_node_idx", "trip_count"]
        ]
        .astype(int)
        .values.tolist()
    )


def build_data():
    nodes = {mode: read_nodes(filename) for mode, filename in NODE_FILES.items()}
    valid_node_ids = {
        mode: {node["id"] for node in mode_nodes}
        for mode, mode_nodes in nodes.items()
    }
    flows = {
        mode: read_flows(FLOW_FILES[mode], valid_node_ids[mode])
        for mode in NODE_FILES
    }

    intervals = {}
    for interval_hours in (1, 2, 3):
        time_labels = []
        od_data = []
        for start_hour in range(0, 24, interval_hours):
            end_hour = start_hour + interval_hours
            time_labels.append(f"{start_hour:02}:00 - {end_hour:02}:00")
            od_data.append(
                {
                    mode: {
                        "dir": aggregate_routes(
                            flows[mode], start_hour, interval_hours
                        )
                    }
                    for mode in NODE_FILES
                }
            )
        intervals[str(interval_hours)] = {
            "timeLabels": time_labels,
            "odData": od_data,
        }

    micro_nodes = nodes["micro"]
    center = [
        sum(node["lat"] for node in micro_nodes) / len(micro_nodes),
        sum(node["lng"] for node in micro_nodes) / len(micro_nodes),
    ]
    return {
        "center": center,
        "zoom": 11,
        "nodes": nodes,
        "intervals": intervals,
    }


def load_original_html_shell():
    if not OUTPUT_FILE.exists():
        raise FileNotFoundError(f"Map template not found: {OUTPUT_FILE.name}")
    return OUTPUT_FILE.read_text(encoding="utf-8")


def replace_embedded_data(html, data):
    start_marker = "  const data = "
    data_start = html.index(start_marker) + len(start_marker)
    data_end = html.index(";\n", data_start)
    return html[:data_start] + json.dumps(data) + html[data_end:]


def add_abstract_mode_control(html):
    """Restore the original white-background / basemap toggle."""
    if 'id="abstractMode"' in html:
        return html

    control_html = '''
    <div class="field">
      <span class="field-title">Layout Control</span>
      <label class="radio-row">
        <input type="checkbox" id="abstractMode">
        <span class="option-copy">
          <strong>Abstract Mode</strong>
          <small>White background · hide map tiles</small>
        </span>
      </label>
    </div>
'''
    html = html.replace("  </aside>", control_html + "  </aside>", 1)

    control_js = '''
  const abstractModeControl = document.getElementById("abstractMode");
  abstractModeControl.addEventListener("change", () => {
      if (abstractModeControl.checked) {
          if (map.hasLayer(baseMapLayer)) map.removeLayer(baseMapLayer);
          map.getContainer().style.backgroundColor = '#ffffff';
      } else {
          if (!map.hasLayer(baseMapLayer)) baseMapLayer.addTo(map);
          map.getContainer().style.backgroundColor = '#eef2f6';
      }
  });

'''
    final_update = "  updateTime();\n\n</script>"
    if final_update not in html:
        raise ValueError("Could not locate the final map initialization block")
    return html.replace(final_update, control_js + final_update, 1)


def main():
    print("1. Building 1.5–4 km aggregates...")
    build_large_scale_files()
    print("2. Rebuilding the multi-scale map data...")
    data = build_data()
    print("3. Preserving the map interface and controls...")
    html = replace_embedded_data(load_original_html_shell(), data)
    html = add_abstract_mode_control(html)
    OUTPUT_FILE.write_text(html, encoding="utf-8")
    print(f"Done: {OUTPUT_FILE.name}")


if __name__ == "__main__":
    main()
