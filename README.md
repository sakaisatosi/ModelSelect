# ModelSelect
ModelSelectはAICによって予測モデルを選択するためのRのパッケージです。
多くのモデルからデータに一番あったモデルを導き出します。
初期パラメータの最適化や、データの性質にそったモデル選択が可能です。

## インストール
devtoolsパッケージを使ってgithubからModelSelectをインストールできます。

```
require(devtools)
install_github('sakaisatosi/ModelSelect')
```

## 使い方


```
# ライブラリの読み込み
library(ModelSelect)

# データ作成
x <- seq(0,100,5)
y <- c(98, 97, 96, 94, 92, 84, 78, 73, 61, 53, 43, 37, 30, 28, 24, 22, 21, 20, 20, 20, 18)

# モデル比較をおこなう
# 第一引数に応答変数を, 第二引数に説明変数をいれる
compareModel(y, x)

# それぞれのモデルのAICが表示される
```

## License
このソフトウェアはMITライセンスに基づいています。LICENSE.txtを見て下さい。
