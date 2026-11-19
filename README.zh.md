# expfamily — 随机实验（jamovi 模块）

分析随机实验：回归调整的处理效应与稳健推断、随机化推断、多结果变量校正、功效、缺失与界、子组与析因设计。 英文完整说明见 [README.md](README.md)。

## 分析（jamovi 菜单：Experiments）

| 分析 | 内容 |
|---|---|
| **处理效应** | 均值差、协变量调整、Lin (2013) 交互调整、区组固定效应、多个处理组；HC0–HC3、CR1、CR2 标准误 |
| **随机化推断** | Fisher 精确检验：枚举或模拟，区组和整群分配，区间由检验反推 |
| **多个结果变量** | Bonferroni、Holm、Sidak、Hochberg、BH、BY、Westfall-Young、Romano-Wolf 校正 p 值 |
| **功效与样本量** | 功效、最小可检测效应、样本量，个体与整群设计 |
| **子组效应** | 各子组处理效应、交互 Wald 检验、校正 p 值、森林图 |
| **析因设计** | 两个交叉处理：主效应、简单效应、交互对比及联合检验 |
| **平衡与缺失** | 标准化差异、联合 F 与置换检验、差异性缺失 |
| **缺失下的界** | Lee (2009) 修剪界，bootstrap 标准误与 Imbens-Manski 区间 |
| **最坏情形界** | Manski 界，可加单调处理反应与单调处理选择假设 |

每个分析的输出部分都有"方法说明"和"数据与设置指南"。

全部引擎为基础 R 编写，并与 `estimatr`、`ri2`、`randomizr`、`pwr`、`lmtest` 等对照（`tests/reference.R`、`tests/subfact.R`、`tests/lee.R`、`tests/manski.R`）；Lee 界还与 Stata `leebounds` 对照。示例数据：`sim_trial.csv`、`sim_cluster_trial.csv`、`sim_small_trial.csv`、`sim_subgroup_trial.csv`、`sim_factorial_trial.csv`、`sim_selection_program.csv`。

## 安装
- **jamovi 模块库**：在 jamovi 右上角打开模块按钮，进入 *jamovi library*，搜索并安装（模块上架后）。
- **侧载**：下载 `.jmo` 文件，在同一菜单选 *Install from file*。`.jmo` 只适用于构建它的操作系统、处理器类型和 jamovi 版本。
- **从源码**：需要 R 和 `jmvtools`：`jmvtools::install(home = "C:/Program Files/jamovi 28.17.0.0")`（路径按本机 jamovi 修改）。

## 引用与许可
引用：Sun, M. *expfamily* (jamovi module). https://github.com/mingchao-sun/expfamily 。请同时引用所用方法的原始文献（输出中列出）。

许可：见 `DESCRIPTION`（GPL）；第三方代码与数据见 `NOTICE`。问题与建议：https://github.com/mingchao-sun/expfamily/issues
