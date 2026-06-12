# SQL 表结构摘要

字段来自 `source/PeMS-Datasets/sql/pemsObjects.sql`。这里是快速查阅版；精确定义、类型和约束以原 SQL 为准。

## `[pems].[station_five_minute]`

主键约束：`timestamp`, `station`

字段：

- 基础：`timestamp`, `station`, `district`, `freeway`, `direction_of_travel`, `lane_type`, `station_length`
- 观测质量：`samples`, `percentage_observed`
- 汇总指标：`total_flow`, `average_occupancy`, `average_speed`
- 分车道指标：`lane1_samples` 到 `lane8_samples`, `lane1_flow` 到 `lane8_flow`, `lane1_average_occupancy` 到 `lane8_average_occupancy`, `lane1_average_speed` 到 `lane8_average_speed`, `lane1_observed` 到 `lane8_observed`

## `[pems].[station_hour]`

主键约束：`timestamp`, `station`

字段：

- 基础：`timestamp`, `station`, `district`, `route`, `direction_of_travel`, `lane_type`, `station_length`
- 观测质量：`samples`, `percentage_observed`
- 汇总指标：`total_flow`, `average_occupancy`, `average_speed`
- 延误指标：`delay_35`, `delay_40`, `delay_45`, `delay_50`, `delay_55`, `delay_60`
- 分车道指标：`lane1_flow` 到 `lane8_flow`, `lane1_average_occupancy` 到 `lane8_average_occupancy`, `lane1_average_speed` 到 `lane8_average_speed`

## `[pems].[station_day]`

主键约束：`timestamp`, `station`

字段：

- 基础：`timestamp`, `station`, `district`, `route`, `direction_of_travel`, `lane_type`, `station_length`
- 观测质量：`samples`, `percentage_observed`
- 指标：`total_flow`, `delay_35`, `delay_40`, `delay_45`, `delay_50`, `delay_55`, `delay_60`

## `[pems].[station_aadt]`

主键约束：`timestamp`, `station`, `day_number`, `hour_of_day`

字段：

- 基础：`timestamp`, `station`, `freeway_identifier`, `freeway_direction`, `city_identifier`, `county_identifier`, `district_identifier`, `station_type`, `param_set`, `absolute_postmile`
- 时间：`hour_of_day`, `day_number`
- 指标：`mahw`, `number_of_days`

## `[pems].[census_vclass_hour]`

主键约束：`timestamp`, `census_station_identifier`, `census_substation_identifier`

字段：

- 基础：`timestamp`, `census_station_identifier`, `census_substation_identifier`, `freeway_identifier`, `freeway_direction`, `fips_city_code`, `fips_county_code`, `district_identifier`, `absolute_postmile`, `station_type`, `census_station_set_id`
- 指标：`flow`, `samples`, `flow_vehicle_class_0` 到 `flow_vehicle_class_14`

## `[pems].[station_metadata]`

主键约束：`metadata_date`, `station`

字段：

- 时间/站点：`metadata_date`, `station`
- 道路位置：`freeway`, `direction`, `district`, `county`, `city`, `state_postmile`, `absolute_postmile`, `latitude`, `longitude`, `shape`
- 站点属性：`length`, `type`, `lanes`, `name`, `user_id_1`, `user_id_2`, `user_id_3`, `user_id_4`

## `[pems].[holiday]`

主键约束：`date`, `holiday`

字段：

- `date`
- `holiday`
- `type`

`type` 在 SQL 数据中包括 `Actual`, `Observed`, `Residual` 等。

## 时间映射表

`[pems].[time_min5_xref]`:

- `min5`, `min5_period_start`, `min5_period_end`
- `min15`, `min15_period_start`, `min15_period_end`
- `min30`, `min30_period_start`, `min30_period_end`
- `abm_half_hour`, `abm_half_hour_period_start`, `abm_half_hour_period_end`
- `min60`, `min60_period_start`, `min60_period_end`
- `abm_5_tod`, `abm_5_tod_period_start`, `abm_5_tod_period_end`
- `day`, `day_period_start`, `day_period_end`

`[pems].[time_min60_xref]`:

- `min60`, `min60_period_start`, `min60_period_end`
- `abm_5_tod`, `abm_5_tod_period_start`, `abm_5_tod_period_end`
- `day`, `day_period_start`, `day_period_end`
