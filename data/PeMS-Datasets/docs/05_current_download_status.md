# 当前下载状态

检查时间：2026-06-08

## 已下载

当前 `data/raw/` 已包含：

- `Station Hour`, District 11, 2020 年 1-12 月，共 12 个 `.txt` 文件。
- `Station Metadata`, District 11, 2020 年 4 个 `.txt` 文件：
  - `d11_text_meta_2020_03_26.txt`
  - `d11_text_meta_2020_05_29.txt`
  - `d11_text_meta_2020_10_01.txt`
  - `d11_text_meta_2020_10_02.txt`

检查结果：

- 原始数据文件总量约 1.5 GB。
- `Station Hour 2020` 共 12,909,493 行。
- 4 份 `Station Metadata 2020` 快照合并后共 5,768 行。
- 按 station 保留最新快照后共 1,521 个 station。
- 没有空文件。
- 没有重复文件。
- 文件名均已被检查脚本识别。

## 已生成

- `metadata/downloaded_files.csv`: 当前已下载文件清单。
- `data/processed/station_hour_2020_monthly_summary.csv`: 2020 年 Station Hour 每月行数与首尾时间。
- `data/processed/station_metadata_2020_snapshots.csv`: 4 份 metadata 快照的标准化合并表。
- `data/processed/station_metadata_2020_latest_by_station.csv`: 按 station 保留最新快照的 metadata 表。
- `data/processed/station_metadata_2020.csv`: 与 latest-by-station 相同，作为默认 2020 metadata 输出。
- `data/processed/station_metadata_2020_extra_tab_rows.csv`: 原始 metadata 中 `name` 字段含 tab 的行，已在输出中修正。
- `data/processed/station_hour_2020_metadata_coverage.csv`: 小时数据 station 与 metadata 的覆盖统计。
- `data/processed/station_hour_2020_missing_metadata_stations.txt`: 小时数据中未被当前 metadata 覆盖的 station ID。
- `data/processed/station_hour_2020_missing_metadata_station_summary.csv`: 未覆盖 station 的行数、首尾时间和出现月份。
- `data/processed/station_hour_2020_station_month_basic_summary.csv`: 2020 小时数据按 station + month 的轻量基础汇总，共 17,812 条 station-month 记录。
- `metadata/station_hour_columns.csv`: Station Hour 原始文件 42 列字段说明。
- `scripts/process_station_hour_2020.py`: 可复跑的轻量处理脚本。

## 注意

当前 `2020-10-02` metadata 可以作为 wiki 所说的“每年最后一个 metadata 文件”使用。为了提高全年 station 属性覆盖率，这里也合并了 2020 年内另外三份 metadata 快照。

合并 4 份 2020 metadata 后，覆盖情况如下：

- 2020 Station Hour 中出现的 station 数：1,535
- 当前 2020 metadata 覆盖的 station 数：1,521
- 未匹配到 metadata 的 station 数：14

剩余 14 个 station 只出现在 2020 年 1-3 月，并且最后出现时间都是 `03/26/2020 00:00:00`。这说明它们大概率需要 2020-03-26 之前的旧 metadata 快照，通常是 2019 年最后一份 metadata。

## 建议补下载

如果要追求 2020 全年 station 属性 100% 覆盖，请再下载 2019 年最后一份 District 11 Station Metadata，选择 `text`，不要选 `tmdd`。

如果只是先做 Station Hour 2020 分析，可以暂时不补。当前未覆盖缺口为 14 个 station、28,560 行小时记录，约占 2020 Station Hour 全部记录的 0.22%。
