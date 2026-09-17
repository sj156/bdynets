# Figure 3(a)：五个模型、两种 W 采样方案——c6代码与结果

本快照补充 **graphMoDE-C、EucMoDE、MoDE、PottsMoDE** 在论文 Figure 3(a)
双臂螺旋路网上的结果，并复用已完成的 **W-reference** 与 **W-joint**。
**这是五个统计模型、六行计算结果；两个W名称不是两个不同的模型。**

This research snapshot supplements four models on the same Figure 3(a) panel.
W-reference and W-joint fit the same graphMoDE-W posterior with different MCMC
updates. All six representative partitions recover the true five classes, but
retained-partition frequencies and the original diagnostic flags differ.

## 方法之间有什么不同？

所有方法共享动态Poisson专家、季节基函数和专家先验；空间分类结构如下。

| 名称 | 空间分类结构 | 专家时间分块 | 门控分块 | 联合拆分/合并 |
|---|---|---|---|---|
| graphMoDE-C | 以道路距离选取q=4近邻，保留0/1连接关系 | 42时点 | 有，contrast ESS；每轮4次门控扫描 | 本轮未使用 |
| EucMoDE | 欧氏距离Matérn核，自适应空间门控 | 42时点 | 同上 | 本轮未使用 |
| MoDE | Dirichlet混合权重，不含空间结构 | 42时点 | 无高斯门控；更新混合权重和分类 | 本轮未使用 |
| PottsMoDE | 道路加权近邻图上的Potts类别先验 | 42时点 | 无高斯门控；逐节点顺序Gibbs更新 | 本轮未使用 |
| W-reference（已有基础方案） | 道路距离加权近邻图上的自适应门控 | 42时点 | 与C/Euc相同 | 未使用 |
| W-joint（已有增强方案） | 与W-reference完全相同 | 42时点 | 与W-reference相同 | 每10步增加一次 |

C也使用道路距离选近邻；C与W的区别是图上连接是否带有距离权重，而非有没有路网信息。
C、Euc和两个W方案的基础分块/缓存设置一致。专家时间块采用随机偏移，边界块可能短于42时点。

**分块更新**每次更新部分参数；**联合拆分/合并**同时提出分区、两条完整专家路径和门控坐标，
再按完整MH接受率统一接受或拒绝。W-joint保持W-reference的模型、先验和目标后验不变。
当前联合模块只实现并验证了动态Poisson W范围。C/Euc可考虑类似扩展，MoDE/Potts需使用各自
类别先验的接受率；本次没有为这些模型实现或运行联合增强。

比较C和W-reference，更接近比较空间模型差异；比较W-reference和W-joint，是比较同一模型的
采样方案。将W-joint直接与其他基础方案比较，会同时混入模型与算法差异。
**相同迭代次数不意味着相同计算工作量**，因此保留完整worker耗时。

## 本面板的分类与耗时

四链合并后，根据后验共聚类矩阵选择Dahl代表性分区，选择过程不使用真值。
所有方法的这张代表图均正确；下面的“完整分区吻合”统计全部保留样本。

| 方法 | 代表分区类别数 | ARI | 完整分区吻合 | 比例 | 四链总耗时 |
|---|---:|---:|---:|---:|---:|
| graphMoDE-C | 5 | 1 | 904/1200 | 75.3% | 31.2分钟 |
| EucMoDE | 5 | 1 | 1200/1200 | 100.0% | 31.4分钟 |
| MoDE | 5 | 1 | 499/1200 | 41.6% | 26.6分钟 |
| PottsMoDE | 5 | 1 | 900/1200 | 75.0% | 27.6分钟 |
| W-reference（已有基础方案） | 5 | 1 | 917/1200 | 76.4% | 31.1分钟 |
| W-joint（已有增强方案） | 5 | 1 | 1200/1200 | 100.0% | 36.7分钟 |

W两行来自既有c5，未在c6重跑。W-joint作为增强采样器结果单列，不宣称它代表W模型本身对
其他模型的优势。表中耗时是每模型四条完整worker的合计，包含预热、更新、检查和结果写入；
不含父进程及随后诊断。C6科学进程7022.328秒、一次诊断80.923秒，约1小时58分钟。

## 41.6%、100%究竟表示什么？

每个模型有4条链，各保留300个样本，总共1200张包含全部121节点的分类图。
“完整分区吻合”要求整张图的分组关系与生成真值一致，允许类别编号任意置换。
它不是节点正确率，也不是1200次独立模拟的成功率。

例如MoDE有499/1200张图完全吻合真值，频率约41.6%；其余701张有分区差异，但不代表
其中所有节点都分错。MoDE最终Dahl代表性分区恰好全对，所以ARI=1，两者并不矛盾。

该频率是辅助指标。在同一数据与可靠后验采样下，更高意味着更多样本支持真实分区；
但它对任何节点误分都作整张不匹配处理，受链的探索程度影响。100%不证明充分收敛，
41.6%也不直接证明某模型比另一模型差。当前短链频率不应被宣称为精确的后验概率，
更不应以把所有方法调到100%作为开发目标。

[RESULTS.md](RESULTS.md)保留原统计标志、曲线RMSE和共聚类Brier误差。
**所有六行的原statistical_valid均为FALSE。** 本次没有放宽阈值、延长链或重复优化。

## 数据与公平比较的范围

- 同一份已保存的Figure 3(a)观测：121节点、168时点，生成真5类，拟合容量K=10。
  四条链是同一面板的不同初始状态，不是四个独立数据集。
- 每条链600步，前300步预热，thin=1；复用c5的迭代零Z和完整专家路径，初始占用类数1/3/7/10。
  C/Euc还复用标准正态白化门控坐标及guidance logits；因协方差不同，物理空间效用并不相同。
  新采样流使用独立种子，详见[SOURCE.json](SOURCE.json)。
- Euc使用nu=1、几何距离中位数范围2.3283463448707398；Potts固定beta=1；MoDE使用
  Dirichlet(0.1,...,0.1)。这些是本次固定开发值，不是验证选出的最优超参数。
- 真类按道路距离的最近种子划分。道路展开后为连续五段，不是按离圆心远近划五个同心环，
  两条完整螺旋臂也不等于两个类别。空间邻近关系与沿路邻近关系有冲突，
  但每节点的168期响应与自适应门控仍可能帮助Euc恢复类别；本轮未单独识别这些因素的贡献。
- 后续更大规模服务器模拟将对全部五个模型统一重新规定迭代、预热、起点、
  模型专属参数、采样改进和评价方案。本轮设置不是新的正式默认值。

此前的[分类图与道路展开图](../2026-09-17-figure3a-joint-c5-v1/figures/graphMoDE-c5-classification.png)
及[Figures 4–8图册](../2026-09-17-figure3a-joint-c5-v1/graphMoDE-c5-figure-gallery.pdf)继续保留。
这些图属于c5 W两种方案，不能当作本次四个新增模型的轨迹图。

## 代码和来源

- [模型与空间核构造](R/graphmode4-design.R)、[图与距离核](R/graphmode-geometry.R)。
- [四模型执行适配器](R/graphmode_compare_run.R)、[执行入口](scripts/graphmode-compare-run.R)、
  [进程控制器](scripts/graphmode-compare-controller.py)、[9组定向检查](scripts/tests/graphmode-compare-run-deterministic.R)。
- [W联合提议](R/graphmode_joint_partition.R)、[W两方案组合](R/graphmode_joint_run.R)。
- [专家时间分块](R/graphmode_expert_blocks.R)、[门控分块](R/graphmode_gate_blocks.R)、
  [门控缓存](R/graphmode_gate_factor_cache.R)。
- [实际执行前的固定方案](docs/GRAPHMODE_FOUR_MODEL_PLAN_2026-09-17.md)。该文件保留当时
  “本次运行不含发布”的范围文字；当前GitHub发布是完成运行后用户另外明确授权的操作。

C6执行来源：`4996cb1479a5e634f806a549dbcb67fbf8776a82`；
复用W结果的c5来源：`1520bd34de41414e2c3c2617a3802286372a0ea5`。
共122个原样源码/测试/来源文件：保留c5已公开的117个，新增c6的5个。
其中41项属于c6登记的执行来源。公开发布commit与执行commit分别记录，不混用。

本目录是研究源码和结果摘要存档，不是已安装bdynets包/API的更新，也不是开箱即跑的复现实验包。
原绝对路径、来源Git检查和输入指纹没有改写；部分检查需要本地保存的历史输入。
旧来源文档中的待审计/待授权状态属于各自日期；本页与RESULTS给出已完成c6的范围和结果。
没有包含原始链、checkpoint、完整论文、私密日志或开发仓库Git历史。
复用已有科学检查与诊断证据，本次发布不启动模拟、不重算诊断。

在本目录可仅核验文件完整性：

```sh
shasum -a 256 -c SOURCE_IDENTITY.sha256
shasum -a 256 -c TRANSFER.sha256
```

保留[原许可证](LICENSE.md)。
