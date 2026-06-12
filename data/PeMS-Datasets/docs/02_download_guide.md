# 下载指南

## 为什么需要手动下载

PeMS 原始数据不在 GitHub 上。SANDAG README 指出，数据来自 PeMS Data Clearinghouse，访问需要用户名和密码；Caltrans 有意限制程序化访问，推荐用浏览器批量下载扩展。因此，本整理包只自动保存了公开文档、代码、SQL 和清单，原始交通数据需要你登录 PeMS 后下载。

登录入口：

- https://pems.dot.ca.gov/
- 登录后进入 Data Clearinghouse。

## 下载后放哪里

请把下载得到的原始文件直接放入：

```text
data/raw/
```

可以按年份或类型建立子目录，例如：

```text
data/raw/station_hour/2020/
data/raw/station_day/2020/
data/raw/station_5min/2020/
data/raw/station_metadata/
data/raw/station_aadt/
data/raw/census_vclass_hour/
```

检查脚本会递归扫描 `data/raw/`，所以是否分子目录不影响识别。

## 优先下载清单

如果你的目标是复现 wiki 中 SANDAG 已整理的数据，按下面下载。

### District 11 Station 数据

District 11 对应 San Diego / Imperial 区域。

| 类型 | PeMS 选择 | 文件模式 | 年份范围 |
|---|---|---|---|
| Station Hour | `station_hour`, District `11` | `d11_text_station_hour_YYYY_MM.txt.gz` | 2009-2020, 2022 Jan-Mar |
| Station Day | `station_day`, District `11` | `d11_text_station_day_YYYY_MM.txt.gz` | 2014-2020, 2022 Jan-Mar |
| Station 5-Minute | `station_5min`, District `11` | `d11_text_station_5min_YYYY_MM_DD.txt.gz` | 2016, 2018-2020, 2022 Jan-Mar |
| Station AADT Month Hour | Station AADT, District `11` | zip 内含 `d11_text_station_aadt_month_hours_...` | 2015-2019 |
| Station Metadata | Station Metadata, District `11` | `d11_text_meta_YYYY_MM_DD.txt` 或 `.txt.gz` | 每年最后一个 metadata 文件，2008-2020 |

### Census V-Class Hour

| 类型 | PeMS 选择 | 文件模式 | 年份范围 |
|---|---|---|---|
| Census V-Class Hour | Census / TMG vehicle class hour | `all_text_tmg_vclass_hour_...txt.gz` | 2008-2010, 2012-2014, 2016 |

wiki 写的是 All Districts，不只 District 11。

## 下载后检查

下载结束后运行：

```bash
python3 scripts/check_raw_files.py data/raw
```

它会输出：

- 可识别文件数量。
- 未识别文件名。
- 空文件。
- 重复文件名。
- 每类数据的年份/月份/天数覆盖概览。

## 不建议上传或发送账号密码

PeMS 账号密码请你自己在浏览器里输入。不要把账号密码发到聊天里；如果后续要自动化下载，更合适的做法是在你本机 `.env` 里配置，并由你自己确认运行。
