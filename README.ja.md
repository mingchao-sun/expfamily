# expfamily — ランダム化実験（jamovi モジュール）

ランダム化実験の分析：回帰調整による処置効果と頑健な推測、ランダム化推測、複数アウトカムの補正、検出力、脱落とバウンド、サブグループと要因計画。 詳しい英語の説明は [README.md](README.md)、中国語の要約は [README.zh.md](README.zh.md) を参照してください。

## 分析（jamovi メニュー：Experiments）

| 分析 | 内容 |
|---|---|
| **処置効果** | 平均差、共変量調整、Lin (2013) 交互作用調整、ブロック固定効果、複数群。HC0–HC3、CR1、CR2 標準誤差 |
| **ランダム化推測** | Fisher の正確検定（全列挙またはシミュレーション）、検定反転による区間 |
| **複数アウトカム** | Bonferroni、Holm、Sidak、Hochberg、BH、BY、Westfall-Young、Romano-Wolf の補正 p 値 |
| **検出力とサンプルサイズ** | 検出力、最小検出可能効果、サンプルサイズ（個人・クラスター設計） |
| **サブグループ効果** | 各サブグループの効果、交互作用 Wald 検定、補正 p 値、フォレスト図 |
| **要因計画** | 2 つの交差する処置：主効果、単純効果、交互作用対比と同時検定 |
| **バランスと脱落** | 標準化差、同時 F 検定と置換検定、差のある脱落 |
| **脱落下のバウンド** | Lee (2009) トリミングバウンド。ブートストラップ標準誤差と Imbens-Manski 区間 |
| **最悪ケースのバウンド** | Manski バウンド。単調処置反応・単調処置選択の仮定を追加可能 |

各分析の出力には「方法の説明」と「データと設定ガイド」があります。

すべてのエンジンは base R で書かれ、`estimatr`、`ri2`、`randomizr`、`pwr`、`lmtest` などと照合しています。Lee バウンドは Stata `leebounds` とも照合。サンプルデータ：`sim_trial.csv`、`sim_cluster_trial.csv`、`sim_small_trial.csv`、`sim_subgroup_trial.csv`、`sim_factorial_trial.csv`、`sim_selection_program.csv`。

## インストール
- **jamovi ライブラリ**：jamovi 右上のモジュールボタンから *jamovi library* を開き、検索してインストール（モジュール公開後）。
- **サイドロード**：`.jmo` ファイルをダウンロードし、同じメニューの *Install from file* から導入します。`.jmo` はビルドした OS、プロセッサ、jamovi のバージョンでのみ動作します。
- **ソースから**：R と `jmvtools` が必要です：`jmvtools::install(home = "C:/Program Files/jamovi 28.17.0.0")`（パスはお使いの jamovi に合わせてください）。

## 引用とライセンス
引用：Sun, M. *expfamily* (jamovi module). https://github.com/mingchao-sun/expfamily 。使用した手法の原論文（出力に記載）も併せて引用してください。

ライセンス：`DESCRIPTION` を参照（GPL）。第三者のコードとデータは `NOTICE` に記載。不具合や提案：https://github.com/mingchao-sun/expfamily/issues
