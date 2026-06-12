# 数据集总览

本整理基于 SANDAG PeMS-Datasets wiki。它关注 Caltrans PeMS Data Clearinghouse 中的若干交通检测器数据集，并把数据加载到 SQL Server 的 `[pems]` schema 后进行汇总。

## 核心数据集

| 数据集 | PeMS 类型/文件 | 范围 | SQL 表 | 主要指标 | Wiki 已说明加载范围 |
|---|---|---:|---|---|---|
| Station 5-Minute | Station 5-Minute | District 11 | `[pems].[station_five_minute]` | `total_flow`, `average_speed`, lane-level flow/speed/occupancy | 2016, 2018-2020, 2022 Jan-Mar |
| Station Hour | Station Hour | District 11 | `[pems].[station_hour]` | `total_flow`, `average_speed`, occupancy, delay, lane-level values | 2009-2020, 2022 Jan-Mar |
| Station Day | Station Day | District 11 | `[pems].[station_day]` | daily `total_flow`, delay bands | 2014-2020, 2022 Jan-Mar |
| Station AADT Month Hour | Station AADT | District 11 | `[pems].[station_aadt]` | monthly average hourly weekday flow `mahw`, `number_of_days` | 2015-2019 |
| Census V-Class Hour | Census V-Class Hour | All Districts | `[pems].[census_vclass_hour]` | hourly vehicle-class flow fields | 2008-2010, 2012-2014, 2016 |
| Station Metadata | Station Metadata | District 11 | `[pems].[station_metadata]` | station location, freeway, direction, lane/type metadata | last metadata file per year, 2008-2020 |
| Holiday | SANDAG-created table | California holidays | `[pems].[holiday]` | actual/observed/residual holiday dates | 2000-2025 in repo objects plus inserts |

## 汇总逻辑

SANDAG SQL 对象会在汇总时尽量排除不适合做典型工作日统计的记录：

- 排除周末。
- 排除 `[pems].[holiday]` 表中的节假日。
- 排除 `samples = 0` 的插补值。
- 排除主要指标为空的记录。
- 对 flow/speed 汇总时按 `samples` 或 `number_of_days` 加权。
- 对 5 分钟和小时数据，只有某个日内时间段完整覆盖时才纳入部分汇总。

## 时间粒度交叉表

SQL 脚本提供两个时间映射表：

- `[pems].[time_min5_xref]`: 把 5 分钟粒度映射到 15 分钟、30 分钟、小时、ABM half-hour、ABM 5 TOD、全天。
- `[pems].[time_min60_xref]`: 把小时粒度映射到 ABM 5 TOD 和全天。

可用于汇总的时间列包括：

- 5 分钟源表：`min5`, `min15`, `min30`, `abm_half_hour`, `min60`, `abm_5_tod`, `day`
- 小时源表：`min60`, `abm_5_tod`, `day`

## 样本量检查

`[pems].[fn_sample_size]()` 会返回每个数据集、年份、站点的观测数量 `n` 和年度覆盖百分比 `pct`。wiki 特别强调，PeMS 任意时期的数据都不保证完整，做汇总前必须看样本量。

## 站点匹配

`source/PeMS-Datasets/matching/matchStations.py` 用于把某一年的 PeMS station metadata 与 SANDAG highway network `hwycov.e00` 文件做一对一匹配。匹配依据包括：

- freeway name/number
- direction
- HOV / non-HOV
- station 到 highway link 的距离，脚本中默认最大距离为 120

输出文件为 `match.csv`，包含 `station` 和 `hwyCovId`。
