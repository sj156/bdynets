# SQL 与处理脚本说明

## 公开源码位置

SANDAG 主仓库已经保存在：

```text
source/PeMS-Datasets/
```

关键文件：

- `source/PeMS-Datasets/sql/pemsObjects.sql`
- `source/PeMS-Datasets/python/main.py`
- `source/PeMS-Datasets/python/download-PeMS-data.py`
- `source/PeMS-Datasets/matching/matchStations.py`
- `source/PeMS-Datasets/holiday_table/`

## SQL 对象

`pemsObjects.sql` 会创建 `[pems]` schema 下的表、时间映射表、样本量函数和汇总过程。

主要表：

- `[pems].[census_vclass_hour]`
- `[pems].[holiday]`
- `[pems].[station_aadt]`
- `[pems].[station_day]`
- `[pems].[station_five_minute]`
- `[pems].[station_hour]`
- `[pems].[station_metadata]`
- `[pems].[time_min5_xref]`
- `[pems].[time_min60_xref]`

主要函数/过程：

- `[pems].[fn_sample_size]()`
- `[pems].[fn_agg_station_day]()`
- `[pems].[sp_agg_station_aadt]`
- `[pems].[sp_agg_station_five_minute_flow]`
- `[pems].[sp_agg_station_five_minute_speed]`
- `[pems].[sp_agg_station_hour_flow]`
- `[pems].[sp_agg_station_hour_speed]`

## 加载脚本

`source/PeMS-Datasets/python/main.py` 的作用：

- 扫描 `../data/` 中的 `.gz`, `.zip`, `.txt` 文件。
- 对 `.gz` 解压出 `.txt`。
- 对 AADT `.zip` 只提取包含 `d11_text_station_aadt_month_hours_` 的文件。
- 根据文件名前缀映射到 SQL 表。
- Station Metadata 会用 pandas 读取并添加 `metadata_date`。
- 其他数据用 SQL Server `BULK INSERT` 导入。

使用前必须修改脚本中的：

- `server`
- `database`
- `pems_data_folder`

并且要保证 SQL Server 中已经运行过 `pemsObjects.sql`。

## 下载脚本

`source/PeMS-Datasets/python/download-PeMS-data.py` 是 Selenium 下载脚本，支持：

- `station_5min`
- `station_day`
- `station_hour`

注意：

- 需要 PeMS 账号密码。
- 需要 Chrome / ChromeDriver。
- 脚本默认路径偏 Windows。
- SANDAG README 推荐浏览器批量下载扩展，因为 PeMS 对程序化下载有限制。

## 节假日表

SQL 脚本内含 2000-2022 的 holiday 数据，`source/PeMS-Datasets/holiday_table/` 里还有后续补充插入文件：

- `pems_holiday_insert_2021-2022.txt`
- `pems_holiday_insert_2023.txt`
- `pems_holiday_insert_2024.txt`
- `pems_holiday_insert_2025.txt`

汇总过程会利用 `[pems].[holiday]` 排除节假日。

## 后续建议

如果你不使用 SQL Server，可以在下载原始文件后改用 Python / DuckDB / Polars 读取压缩文本，再按 `pemsObjects.sql` 中的字段顺序建立本地表。这个整理包目前保留 SANDAG 原始 SQL Server 工作流，避免过早改写处理逻辑。
