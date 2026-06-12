# 原始 Station Hour 与 Station Metadata 文件说明

本文说明当前已下载的两类 PeMS 原始文件分别代表什么、字段含义是什么，以及它们之间如何关联。

## 当前涉及的原始文件

当前原始数据位于：

```text
data/raw/
```

主要包括两类：

1. `Station Hour`
2. `Station Metadata`

当前下载的数据范围是：

```text
District 11
2020 年
Station Hour + Station Metadata
```

## Station Hour 文件

文件示例：

```text
d11_text_station_hour_2020_01.txt
d11_text_station_hour_2020_02.txt
...
d11_text_station_hour_2020_12.txt
```

这些文件是 PeMS 的小时级交通观测数据。

每一行代表：

```text
某一个 station 在某一个小时的交通观测记录
```

例如一行类似：

```text
01/01/2020 00:00:00,1100313,11,5,N,FR,,119,100,233,...
```

含义是：

```text
2020-01-01 00:00:00 这个小时，station 1100313 的一条小时级交通记录。
```

### Station Hour 主要字段

| 字段 | 含义 |
|---|---|
| `timestamp` | 小时开始时间 |
| `station` | PeMS 站点 ID |
| `district` | PeMS District 编号，当前数据为 11 |
| `route` | 公路编号，例如 5、15、805 |
| `direction_of_travel` | 行驶方向，通常为 `N`、`S`、`E`、`W` |
| `lane_type` | 车道或检测器类型 |
| `station_length` | 站点覆盖路段长度 |
| `samples` | 该小时收到的样本数量 |
| `percentage_observed` | 观测覆盖百分比 |
| `total_flow` | 该小时总交通流量 |
| `average_occupancy` | 平均占有率 |
| `average_speed` | 平均速度 |
| `delay_35` 到 `delay_60` | 不同速度阈值下的延误估计 |
| `lane1_flow` 到 `lane8_flow` | 各车道流量 |
| `lane1_average_occupancy` 到 `lane8_average_occupancy` | 各车道平均占有率 |
| `lane1_average_speed` 到 `lane8_average_speed` | 各车道平均速度 |

完整的 42 列字段说明见：

```text
metadata/station_hour_columns.csv
```

### Station Hour 的用途

`Station Hour` 是实际交通观测数据，可以用于分析：

- 每个站点每小时的交通流量
- 每个站点每月的平均流量
- 不同道路、方向、站点类型的交通差异
- 流量随月份或时间变化的趋势
- 平均速度或拥堵情况

当前已生成的轻量分析表：

```text
data/processed/station_hour_2020_station_month_basic_summary.csv
```

这个表已经把 2020 年 12,909,493 行小时数据汇总成按 `station + month` 的基础统计结果。

## Station Metadata 文件

文件示例：

```text
d11_text_meta_2020_03_26.txt
d11_text_meta_2020_05_29.txt
d11_text_meta_2020_10_01.txt
d11_text_meta_2020_10_02.txt
```

这些文件不是交通流量数据，而是站点属性数据。

每一行代表：

```text
某一个 station 在某个 metadata 快照日期下的站点属性
```

也就是说，metadata 是 station 的说明表或字典表。

### Station Metadata 主要字段

| 原始字段 | 整理后字段 | 含义 |
|---|---|---|
| `ID` | `station` | PeMS 站点 ID |
| `Fwy` | `freeway` | 高速公路编号 |
| `Dir` | `direction` | 方向，通常为 `N`、`S`、`E`、`W` |
| `District` | `district` | PeMS District 编号 |
| `County` | `county` | 县代码 |
| `City` | `city` | 城市代码 |
| `State_PM` | `state_postmile` | 州里程桩 |
| `Abs_PM` | `absolute_postmile` | 绝对里程桩 |
| `Latitude` | `latitude` | 纬度 |
| `Longitude` | `longitude` | 经度 |
| `Length` | `length` | 站点覆盖长度 |
| `Type` | `type` | station 类型 |
| `Lanes` | `lanes` | 车道数 |
| `Name` | `name` | 站点名称 |
| `User_ID_1` 到 `User_ID_4` | `user_id_1` 到 `user_id_4` | PeMS 额外 ID 字段 |

### 常见 station type

| `type` | 含义 |
|---|---|
| `ML` | Mainline，主线 |
| `HV` | HOV 车道 |
| `FR` | Off Ramp，下匝道 |
| `OR` | On Ramp，上匝道 |
| `FF` | Freeway-to-Freeway connector，高速连接匝道 |

### Station Metadata 的用途

`Station Metadata` 用来解释 `Station Hour` 中的 station：

- station 在哪条高速上
- station 的方向是什么
- station 的经纬度在哪里
- station 是主线、匝道还是 HOV
- station 有多少车道
- station 名称是什么

当前已生成的 metadata 处理结果包括：

```text
data/processed/station_metadata_2020_snapshots.csv
data/processed/station_metadata_2020_latest_by_station.csv
data/processed/station_metadata_2020.csv
```

其中：

- `station_metadata_2020_snapshots.csv` 保留 4 份 metadata 快照的全部记录。
- `station_metadata_2020_latest_by_station.csv` 对每个 station 只保留最新一条记录。
- `station_metadata_2020.csv` 是默认使用版本，内容与 latest-by-station 表相同。

## 两类文件如何关联

`Station Hour` 和 `Station Metadata` 通过 `station` 字段关联：

```text
Station Hour.station = Station Metadata.station
```

简单说：

```text
Station Hour     = 每小时交通观测数据
Station Metadata = station 的位置和属性说明
```

如果只看 `Station Hour`，可以知道某个 station 每小时的流量、速度、样本数。

如果再连接 `Station Metadata`，就能知道这个 station 在哪条路、哪个方向、什么类型、经纬度在哪里。

## 当前匹配情况

当前 2020 年数据的匹配结果为：

| 指标 | 数值 |
|---|---:|
| Station Hour 中出现的 station 数 | 1,535 |
| Metadata 中覆盖的 station 数 | 1,521 |
| 成功匹配 metadata 的 station 数 | 1,521 |
| 未匹配 metadata 的 station 数 | 14 |

剩余 14 个未匹配 station 都只出现在 2020 年 1-3 月，缺口很小。若要完全覆盖，可能需要补充 2019 年最后一份 District 11 Station Metadata。

## 当前最适合分析的表

当前最适合直接分析的表是：

```text
data/processed/station_hour_2020_station_month_basic_summary.csv
```

它把原始小时数据按：

```text
station + month
```

汇总，每一行代表某个 station 在某个月的基础交通统计。

主要字段包括：

| 字段 | 含义 |
|---|---|
| `station` | 站点 ID |
| `month` | 月份 |
| `metadata_matched` | 是否匹配到 metadata |
| `records` | 该 station 该月小时记录数 |
| `samples_sum` | 样本数总和 |
| `total_flow_count` | 有效流量记录数 |
| `total_flow_mean` | 普通平均小时流量 |
| `total_flow_weighted_by_samples` | 按 `samples` 加权的平均小时流量 |
| `average_speed_count` | 有效速度记录数 |
| `average_speed_mean` | 普通平均速度 |
| `average_speed_weighted_by_samples` | 按 `samples` 加权的平均速度 |

其中两个加权字段的计算思想来自 SANDAG wiki/SQL 中的说明：

```text
加权平均 = SUM(指标 * samples) / SUM(samples)
```

需要注意：当前这个表是基础整理版，还没有完全复刻 SANDAG SQL 中排除周末、节假日、插补值和完整性检查的正式汇总流程。
