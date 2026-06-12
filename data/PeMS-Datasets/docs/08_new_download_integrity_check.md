# 新增数据完整性检查

检查时间：2026-06-12

## 总体结果

当前 `data/raw/` 中可识别的真实数据文件共 50 个。

检查结果：

- 未发现空文件。
- 未发现重复文件。
- 所有保留在 `data/raw/` 中的数据文件均可识别。
- 失败下载的 PeMS HTML 页面已经不再保留在 `data/raw/` 中。

## 已完整的数据

### Station Day 2020

状态：完整。

文件范围：

```text
d11_text_station_day_2020_01.txt
...
d11_text_station_day_2020_12.txt
```

说明：

- District 11。
- 2020 年 1-12 月齐全。
- 每个文件为一个月份的 daily station 数据。
- 原始字段数为 16。

用途：

- 可做 daily flow 地图。
- 可做每日流量趋势。
- 可做工作日/周末对比。

### Station 5-Minute 2020-10-05 到 2020-10-11

状态：完整。

文件范围：

```text
d11_text_station_5min_2020_10_05.txt
d11_text_station_5min_2020_10_06.txt
d11_text_station_5min_2020_10_07.txt
d11_text_station_5min_2020_10_08.txt
d11_text_station_5min_2020_10_09.txt
d11_text_station_5min_2020_10_10.txt
d11_text_station_5min_2020_10_11.txt
```

说明：

- District 11。
- 一周数据齐全。
- 每天 396,864 行。
- 原始字段数为 52。
- 当前文件是已解压 `.txt`，不是 gzip 文件，这是正常的。

用途：

- 可做一天内 5 分钟流量变化地图。
- 可做早晚高峰动态变化。
- 可做工作日和周末高频交通模式对比。

## AADT 2020 检查结果

状态：不完整。

当前成功下载并解压的月份：

```text
2020-01
2020-04
2020-06
```

每个成功月份下都有 5 个 gzip 子文件：

```text
d11_text_station_aadt_annual_dow_YYYY_MM.txt.gz
d11_text_station_aadt_flow_monthly_YYYY_MM.txt.gz
d11_text_station_aadt_hour_monthly_YYYY_MM.txt.gz
d11_text_station_aadt_month_hours_YYYY_MM.txt.gz
d11_text_station_aadt_top30_monthly_YYYY_MM.txt.gz
```

其中后续最适合做地图可视化的是：

```text
d11_text_station_aadt_month_hours_YYYY_MM.txt.gz
```

原因：它包含 month + day-of-week + hour 维度，适合做“月份 + 小时”的典型流量地图。

### AADT 缺失月份

根据下载页面中非 22 bytes 的有效文件判断，2020 AADT 还缺：

```text
2020-07
2020-09
2020-12
```

页面中这些月份应该重新下载：

```text
d11_text_station_aadt_2020_07.txt.zip
d11_text_station_aadt_2020_09.txt.zip
d11_text_station_aadt_2020_12.txt.zip
```

不要下载或保留：

```text
2020-02  22 bytes
2020-03  22 bytes
2020-05  22 bytes
```

这些基本表示无数据或空下载。

### AADT 失败原因判断

之前出现的 `pems.dot.ca.gov*.html` 是 PeMS 网页 HTML，不是 zip 数据文件。HTML 中出现了 `login`、`Clearinghouse` 等内容，说明下载时拿到的是网页/登录态页面，而不是实际数据 zip。

判断下载是否成功：

- 成功文件名应类似：`d11_text_station_aadt_2020_07.txt.zip`
- 成功大小应约为 MB 级，例如 2 MB 左右。
- 失败文件名常见为：`pems.dot.ca.gov.html`
- 失败大小通常是几十 KB 的 HTML 页面。

## 下一步操作建议

1. 先不要处理 AADT 可视化。
2. 重新下载 AADT 的 2020-07、2020-09、2020-12。
3. 下载完成后放入：

```text
data/raw/
```

4. 再运行：

```bash
python3 scripts/check_raw_files.py data/raw
```

5. 确认 AADT 有效月份达到：

```text
01, 04, 06, 07, 09, 12
```

后，再进行 AADT 的地图可视化处理。

当前可以先继续处理已经完整的：

- `Station Day 2020`
- `Station 5-Minute 2020-10-05 到 2020-10-11`
