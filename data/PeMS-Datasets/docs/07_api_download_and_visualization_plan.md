# API 下载与分数据集可视化方案

## 1. 关于 PeMS API 的实际情况

老师说“找 API 下载数据”是合理方向，但就目前公开资料和 SANDAG 仓库来看，需要先区分两种情况：

1. **正式公开 API**
   - 类似 REST API，有文档、endpoint、参数、token、返回 JSON/CSV。
   - 目前我没有在公开网页中找到稳定、公开的 PeMS Clearinghouse REST API 文档。

2. **登录后的 Clearinghouse 下载接口**
   - PeMS Data Clearinghouse 本质上是网页下载系统。
   - 需要 PeMS 账号登录。
   - 页面可以按 `Type`、`District`、年份、月份选择文件下载。
   - SANDAG 仓库中的下载脚本也是用 Selenium 模拟浏览器登录和点击下载，不是调用正式 REST API。

因此，比较严谨的表述是：

```text
PeMS 原始数据可以通过账号登录后的 Clearinghouse 下载系统获取。
目前没有确认到公开稳定的官方 REST API；若要自动化，只能使用登录态网页下载或 Selenium 自动化。
```

## 2. 如果必须“用 API/自动化下载”，建议这样做

不要把账号密码写进代码或发给别人。建议使用本机 `.env` 文件：

```text
PEMS_USERNAME=你的用户名
PEMS_PASSWORD=你的密码
```

然后用 Selenium 控制浏览器：

1. 打开 `https://pems.dot.ca.gov/?dnode=Clearinghouse`
2. 自动填写账号密码并登录
3. 选择数据类型，例如：
   - `Station Hour`
   - `Station Day`
   - `Station 5-Minute`
   - `Station Metadata`
   - `Station AADT`
   - `Census V-Class Hour`
4. 选择 District，例如 District 11
5. 点击对应年份/月份
6. 顺序下载文件
7. 下载后运行检查脚本：

```bash
python3 scripts/check_raw_files.py data/raw
```

注意：SANDAG README 说明 PeMS 会限制自动化下载，所以如果使用自动化，应该顺序下载、降低频率，不要并发请求。

## 3. 当前已经完成的数据

当前已经完成的是：

```text
District 11
2020 年
Station Hour + Station Metadata
```

已处理结果位于：

```text
data/processed/
```

当前最适合直接分析和可视化的表是：

```text
data/processed/station_hour_2020_station_month_basic_summary.csv
```

它把 2020 年 12,909,493 行小时级数据汇总成按 `station + month` 的基础统计表。

## 4. 不同数据集分别适合做什么可视化

### 4.1 Station Metadata

用途：说明 station 的位置和属性。

适合可视化：

- station 空间分布图，经纬度散点图
- 不同 station type 的数量柱状图
- 不同 freeway 上的 station 数量
- 主线、HOV、上匝道、下匝道的分布比较

当前可用文件：

```text
data/processed/station_metadata_2020_latest_by_station.csv
```

### 4.2 Station Hour

用途：每个 station 每小时的流量、速度、样本数。

适合可视化：

- 2020 年各月份平均小时流量变化
- 每个 station 的月度流量变化
- 流量最高的 station 排名
- 按 freeway 或 direction 比较流量
- 如果进一步按小时处理，可以做日内 24 小时变化曲线

当前可用文件：

```text
data/processed/station_hour_2020_station_month_basic_summary.csv
```

### 4.3 Station Day

用途：每天的 station 总流量。

适合可视化：

- 日流量时间序列
- 工作日 vs 周末流量对比
- 月度日均流量变化
- 异常日期识别

需要下载：

```text
d11_text_station_day_YYYY_MM.txt
```

### 4.4 Station 5-Minute

用途：5 分钟级别的高频交通数据。

适合可视化：

- 一天内 5 分钟流量曲线
- 早晚高峰识别
- 高峰时段速度下降
- 同一 station 不同日期的日内曲线对比

注意：这个数据很大，建议只选少量月份或少量 station 做示例。

### 4.5 Station AADT

用途：按月份、星期几、小时统计的平均交通流量。

适合可视化：

- 月份-小时热力图
- 星期几-小时热力图
- 不同 station 的 AADT 比较

需要下载：

```text
d11_text_station_aadt_month_hours_...
```

### 4.6 Census V-Class Hour

用途：按车辆类别统计的小时流量。

适合可视化：

- 各车辆类型占比
- 卡车/非卡车流量对比
- 不同年份车辆类型结构变化

需要下载：

```text
all_text_tmg_vclass_hour_*.txt
```

## 5. 建议的后续操作顺序

建议不要一次性下载所有数据。按可视化需要分批做：

1. **已完成：Station Hour + Metadata**
   - 先完成月度流量、站点空间分布、station type 分布。

2. **下一步：Station Day**
   - 下载 2020 年 District 11 的 Station Day。
   - 做日流量趋势、工作日/周末对比。

3. **再下一步：Station 5-Minute 小样本**
   - 只下载 2020 年某 1 个月或某几天。
   - 做早晚高峰日内曲线。

4. **可选：Station AADT**
   - 做月份-小时热力图。

5. **可选：Census V-Class Hour**
   - 做车辆类型组成图。

## 6. 当前 OSM 地图可视化

已删除普通 PNG 统计图，当前保留的是 OSM/Leaflet HTML 交互地图：

```text
outputs/maps/station_hour_2020_osm_map.html
```

可复跑脚本：

```text
scripts/build_osm_station_hour_map.py
```

这个 HTML 使用 OpenStreetMap 作为底图，把 `Station Metadata` 的经纬度点位、`Station Hour` 的 station-month 流量汇总，以及从 OSM/Overpass 获取的附近道路几何结合起来展示。

地图支持：

- 选择全年或某个月份的流量。
- 按平均小时流量给 OSM 道路线段和 station 圆点上色。
- station 圆点仍然保留，便于查看具体检测站位置。
- 通过最近邻匹配把 station 关联到附近 OSM 高速/主干道路段。
- 按 station type 上色。
- 按 station type 筛选。
- 按 freeway 筛选。
- 按 station ID 搜索。
- 点击 station 查看 station 名称、freeway、方向、类型、车道数、流量和 metadata 日期。
- 点击彩色路段查看对应 station、OSM 道路名、匹配距离和流量。

注意：HTML 文件中的 PeMS 数据已经内嵌，不需要连接 PeMS；但底图和 Leaflet 库来自网络 CDN，所以打开时需要联网。

OSM 道路缓存位于：

```text
data/osm/d11_highways_overpass.json
```

当前匹配结果：

```text
Station 点位：1518
成功匹配 OSM 路段：1509
```
