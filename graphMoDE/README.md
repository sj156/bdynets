# graphMoDE：当前可运行核心代码

本目录提供 Exp-direct W、SE-direct W（均学习带宽 h）和 C 的可复现核心试跑，以及自定义图与计数序列接口。短示例不是收敛证据，也不是论文结论。完整八方法批量尚未启动。初轮 SE 边权下溢现采用经用户批准、有误差诊断的 double 数值近似；完整核心模拟与统计诊断尚未完成，详见 [KNOWN_ISSUES.md](KNOWN_ISSUES.md)。

## 快速开始

需要 R、Python 3（仅标准库），以及已安装的 R 包 `BayesLogit`、`posterior`、`digest`、`jsonlite`、`microbenchmark`。程序不会自动安装或升级依赖。Linux/macOS 均可用；macOS 在核心运行期间自动使用进程绑定的 `caffeinate`。

在仓库根目录运行：

```sh
python3 graphMoDE/run.py test
python3 graphMoDE/run.py example --output "$HOME/graphmode-small-example"
```

第二条命令实际生成 12 点、48 个训练时刻的合成数据，对三个方法各执行 8 步并保存输入与状态。使用新的、不存在的输出目录。它只验证输入与采样入口，不估计精度。

核心试跑的一条命令：

```sh
python3 graphMoDE/run.py core --output "$HOME/graphmode-core-pilot"
```

该命令执行已冻结的 2 份数据 × 3 方法 × 4 链；耗时可能为数小时。最多 6 个 R 工作进程，每个进程设置单数值线程。运行前确认机器没有其他大计算任务，并至少留出 42 GiB 磁盘空间（输出上限 30 GiB，保留 12 GiB）。数据、日志与链写入外部目录；请勿提交它们。

另有 `smoke --output NEW_DIRECTORY`：使用与正式试跑不同的种子，对全部 24 条链各运行 4 步，只检查完整入口，不执行收敛诊断。

## 冻结的核心试跑

- Figure 2 连续双螺旋道路，121 点、120 条物理直接边；五区域大小 `(30,20,21,20,30)`。区域内间距生成式不变，边界因子为 `(3,4,3,2)`。
- 静态季节 Poisson DGP，振幅 `.05`，周期 24，每个相位完整周期的强度均值严格为 25。五相位随机映射区域；训练 168，测试 48。两份数据独立生成，三个方法配对使用同一份数据。
- 真值 5 组，拟合 `K=10`。四链初始占用数为 `1/3/7/10`，初始化仅用训练输入与独立种子；不向拟合传入真实标签，不强制最终占用 5。
- W 使用全物理边的 `exp(-d/h)` 或 `exp(-(d/h)^2)`，每次 h 提议都把正边权归一为均值 1。`h=d_ref*eta`，`eta~Gamma(shape=4,rate=4)`，`d_ref` 为固定物理边长中位数。不使用 NN 删除边、低秩近似、权重裁剪或固定 h 的 W。
- κ、q、类特异指导强度 a 均继续学习。两个 W 的 κ 对数先验中心共用固定的 Exp-reference（`h=d_ref`），不会随 h 重新校准；C 保留按自身 0/1 邻接的边上相关中位数 .5 校准，本轮道路对应 κ₀=2.0000。保留原 `q~Gamma(2,rate=20000)`、κ 对数标准差 1 和既有采样核、提议尺度、4 次 gate scan、42 时刻 expert block、contrast ESS 与 factor cache。
- 每链先运行总 4000 步，前 2000 为预热，不适应或重调。控制器在 4000 步诊断后停止，**不自动启动 8000 步**。诊断可建议延长，但须另有明确决定；若决定延长，所有六个拟合从原状态及 RNG 继续到总 8000 步，仍仅丢弃原前 2000 步。原轨迹、诊断与 receipt 保留。
- rank/folded Rhat ≤1.01、bulk/tail ESS ≥400，所有链对 PSM RMS ≤.05。诊断含占用数、排序组比例、排序指导强度、节点均值、似然、log q、log κ，以及 W 的 log h。与旧标准一致，同值离散常数标记为不可提供混合信息；不同链常数或连续常数不能通过。
- 总 8000 步仍不达标则停止，不扩批、不换种子、不删除失败、不调整先验。诊断门槛与哪个方法胜出无关。完整 Exp-W、SE-W、C、EucMoDE、MoDE、PottsMoDE、STGNN、PNAR 的五份新数据批量需另作决定。

种子及全部配置在生成任何模拟数据前写入 `registration.rds/json`、`seeds.csv`、`jobs.csv`。正式种子范围 `2031101501–2031101564`；入口 smoke 为 `2031102501–2031102564`。已有五份登记及退休种子账本检查无冲突。

## 自定义实例接口与维度

```r
source("graphMoDE/R/api.R")
gm <- graphmode_load("graphMoDE")
x <- gm$graphmode_input(Y, adjacency, edge_lengths, Fmat, Ffuture, ids)
fit <- gm$graphmode_make_fit(x, "Exp-W") # 或 "SE-W", "C"
```

| 输入 | 维度与约定 |
|---|---|
| `Y` | `n × T`，行是节点，列是训练时间；有限非负整数，不接受转置或缺失值 |
| `adjacency` | `n × n`，对称 0/1 物理邻接，零对角；所有给定物理边保留 |
| `edge_lengths` | `n × n`，对称；邻接边上严格正，非边与对角为 0；表示直接物理边长度，不填最短路全距离 |
| `Fmat` | `T × p`，与 Y 列严格对齐；默认示例为截距、sin、cos 三列 |
| `Ffuture` | `H × p`，未来设计；列顺序与 Fmat 相同 |
| `ids` | 长度 n，唯一节点 ID；所有矩阵共用同一节点行序 |

当前公开构造器固定 `K=10`，要求 `n≥10`，默认随机游走动态 `G=I_p`、`W=qI_p`。默认 `m0=(log(max(.1,mean(Y))),0,...)`，`C0=diag(1,.5,...)`；可向 `graphmode_make_fit` 显式传入 `m0` 与 `C0`，特别是设计的第一列不是截距时。无须提供真实分组或坐标。图可以是任意给定物理图；本次核心试跑特指连续道路。自定义实例不得被当作已验证的新统计设计。

完整初始化及调用演示见 [`examples/small.R`](examples/small.R)。生产链应有四个分散初值及足够诊断；该短脚本不能替代它们。本轮诊断入口针对 121 点、168 时刻的冻结道路设计；自定义 n/T 的用户可使用采样 API，但需另建与输入匹配的监控节点及诊断报告。

## 结果与审计

带宽计算保留全部逻辑物理边及其有限归一化 log 权重。只有 `exp(log_weight)` 实际为零且 log 权重不高于 double 最小正 subnormal 的一半所对应的边界时，数值矩阵允许舍入为零；没有另设较大的截断阈值，不做 NN 删边，不改变 h 先验。权重仍按所有物理边的正权均值归一化。

对这类候选，`core_34_underflow_audit.R` 记录下溢边、完整 log 权重、解析扰动量级和常规残差。固定检查为：解析下溢对 gate log-likelihood 的贡献 ≤`1e-12`；独立 Cholesky 逆及主平方根路径的 gate 目标差 ≤`1e-8`；谱残差与正交残差 ≤`256*n*.Machine$double.eps`，协方差重建相对残差 ≤`1024*n*.Machine$double.eps`。后两项是按矩阵规模留余量的工程诊断，前两项约束 log-target 误差；均不修改 Rhat/ESS/PSM 标准。这是受控浮点近似，**不是严格 interval-MH 认证**。检查失败抛出异常、保存故障，不自动拒绝 MH 提议或截断先验。

数值审计默认以 `NUMERICAL_AUDIT` JSON 行写入链日志；API 使用者也可设置 `gm$graphmode_h_numerical_sink <- function(record) ...` 保存单独审计文件，sink 不得调用随机数。主控制器保留失败链记录，正常同伴继续完成，不再因单链失败而终止它们。

资源或控制器异常有不同处理：可用空间低于 12 GiB、输出超过 30 GiB、达到 72 小时时限，或控制器失去正常监督时，停止派发并以 SIGSTOP 暂停所属写入进程，不再等待长链写完。记录 `emergency-TARGET.json`、`suspended-TARGET.json`、PID 和最近已接受的 checkpoint。暂停保留进程内存；只有带有效 SHA256 receipt 的文件才是已落盘 checkpoint，不能把部分文件当作成功保存。恢复需要检查资源及明确决定，不自动 SIGCONT。采样和最终诊断阶段均受资源保护。正常的单链数值失败不会触发同伴暂停。

`progress.json` 仅表示运行进度；`completion.json` 或 `failure.json` 才是控制器终态。每条链每 100 步保存全部状态（含预热）、RNG、数值诊断与 SHA256；各阶段独立日志、进程退出码、receipt 均保留。意外中断不会自动重试；8000 步继续只接受本轮完整 4000 步 receipt，不能接入旧实验 checkpoint。

`summary-4000/` 与必要时的 `summary-8000/` 保存每拟合标量诊断、PSM、代表划分、占用数、边界误并与内部误切、预测矩阵、RMSE、W-minus-C 配对差异和诊断决定。真值与测试数据放在 `evaluation-only.rds`，worker 只读 `blind.rds`。ARI/RMSE 是有诊断状态的试跑描述；未过精度门槛时不能据此给出方法优劣结论。

计时沿用已安装 microbenchmark 的单调时钟封装；启动时降低纳秒精度的提示会保留在日志中。非有限或回退的读数会报错，不裁剪成零、不修改统计状态。

## 源码整理与删除范围

`R/core_01` 至 `core_33` 最初从本地冻结源提取；`SOURCE_MAP.csv` 记录提取时的源模块、哈希和符号。当前版本只在 `core_32` 的权重、谱、平方根和目标函数中增加受控数值近似与审计入口，新增 `core_34`；正常范围回归保持逐位一致，MH、先验及其余采样核未改。部分内部函数名仍含 c8/c9，用于保持核与状态校验一致，并不代表再次运行旧实验。`R/api.R` 提供本轮配置和通用输入，`R/execution.R` 提供分阶段执行。

从当前 `graphMoDE/` 删除历史快照目录、旧文稿/图示、旧调试数据、OSM 北京派生示例、重复旧入口和不再使用的说明；它们仍可从普通 Git 历史恢复。保留当前必要数学核、许可证、合成示例、测试和此说明。不包含北京原始数据、论文、PNARM 源码、密钥、个人绝对路径或大链文件。其他仓库项目不受影响。
