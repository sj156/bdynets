# graphMoDE 开发交接说明

> **当前状态：只用于 Debug，不是发布版，尚未获准运行正式模拟。**
> 最新 K=10 条件分配诊断的 12 个任务中仍有 1 个确定失败，详见
> [`debug/KNOWN_ISSUE.md`](debug/KNOWN_ISSUE.md)。

这个目录是 2026 年 9 月 3 日 graphMoDE/GMDE 开发状态的自包含交接包，
供江老师继续排查。`R/` 中 16 个文件与最近失败运行压缩包内的冻结源码逐一
同哈希；真正的源码身份见
[`provenance/FROZEN_SOURCE_SHA256.csv`](provenance/FROZEN_SOURCE_SHA256.csv)，
不能只用 Git 基点 `ec79ed13f2844e814b88a986eeaa1abdd96d76f0` 代表。

## 包内有什么

- `R/`：当前算法和各阶段诊断的完整执行源码；
- `workflows/`：`2helpers`、`4gibbs` 和已更新状态的 `5simulations` 说明；
- `data/osm-derived/`：100 个固定抽样点和模拟所需的最小 q=4 路网输入；
- `data/debug/`：不含真值的合成计数、K=10 起点、种子、规则、轨迹指纹和
  失败记录；
- `figures/`：论文使用的 100 节点路网图；
- `scripts/`：输入检查、单元测试和单链复现入口；
- `tests/`：条件分配计算的枚举检验与“不改变 RNG”检验。

包内没有原始整张北京路网、真实交通/人口数据、MCMC 链、checkpoint、
terminal state、大型结果 ZIP、缓存、PDF、PNARM 源码或本机绝对路径。

## 江老师收到后先运行

在 R 中把工作目录设为本文件所在的 `graphMoDE`，然后运行：

```r
source("scripts/check_inputs.R")
source("scripts/run_tests.R")
```

这两项是短检查，不会启动正式模拟。若要复现目前唯一的失败并把触发失败的
首个概率向量保存到 Git 已忽略的 `debug-output/`，再运行：

```r
source("scripts/reproduce_k10_failure.R")
```

复现配置固定为 `n=100`、`T=168`、`K=10`、`m=40`、`rho=1`、
3,000 次迭代（前 1,000 次 burn-in）、`substantive_min=5`、mode A、
seed 2026094102。它可能需要数分钟，但只运行这一条链。

## 已知问题

唯一失败任务是 `GMDE-W-mode-A-seed-2`，错误为：

```text
Poisson-binomial threshold recursion failed its mass audit.
```

其余 11 个带被动诊断的任务完成；原来的 12 条 K=10 科学轨迹也均已核验。
目前证据只把问题隔离到了抽样前的确定性诊断路径，还不能据此断言主采样器
本身正确或错误。不能简单放宽 `1e-12` 阈值来让测试通过。全部验收标准见
[`debug/KNOWN_ISSUE.md`](debug/KNOWN_ISSUE.md)。

## 路网图颜色说明与许可

路网底图中浅、中、深三种线条只表示 local、middle 和 major 三类道路的
视觉层级，不代表统计权重、交通流量、不确定性或模拟结果。节点间的细灰色
qNN 连线是统计图的邻接边，也不是实际最短道路路线。

**Map data © OpenStreetMap contributors; available under the Open Database
License (ODbL) 1.0.** 详见
[OpenStreetMap 版权页](https://www.openstreetmap.org/copyright/)和
[ODbL 1.0 正文](https://opendatacommons.org/licenses/odbl/1-0/)。

