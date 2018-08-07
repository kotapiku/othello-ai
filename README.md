Reversi Server & Client 
=======================

ビルドの仕方
------------

    $ stack build 

使い方
------

審判サーバを起動：

    $ stack exec reversi-serv -- -p 30000 -t 500


`-p`はポートの指定．
起動すると 

    Waiting 2 connections ...

と表示される．


ここで、クライアントを起動：

    $ stack exec reversi -- -H "localhost" -p 30000 -n Player1 

すると，サーバ側の表示が,

    Waiting 1 connection ... 

となるので,もう一つクライアントを

    $ stack exec reversi -- -H "localhost" -p 30000 -n Player2

のように起動すると対戦が進む．プレイヤー名は各自で異なっている必要あり．
（黒と白を交替しつつ偶数回対戦する）


サーバのオプション
------------------

`-p`, `--port`
: ポート番号（デフォルトは3000）

`-t`, `--playerstime` 
: 各プレイヤーの持ち時間（デフォルトは十分間）

`--timeout`
: コマンドを待つ許容時間．この時間より遅くなるとタイムアウト（デフォルトは1分）

`-T`, `--byoyomi`
: 持ち時間を使い果した後でも使える時間（デフォルトは500 ms）

`-n`
: プレイヤーの人数．サーバはこの人数の接続があると総当たり戦を始める（デフォルトは2）．

`-r`, `--rounds`
: 二人のプレイヤーはこの回数だけ対戦する（デフォルトは4）．

`-c`, `--concurrency`
: 並行で行う対戦の数．n>=4でないと意味を無さない（デフォルトは2）．


クライアントのオプション
------------------------

`-H`
: サーバのホスト名（デフォルトはlocalhost）

`-p`
: サーバのポート番号（デフォルトは3000）

`-n`
: プレイヤー名．現在サーバはこの値を元に結果を集計するので互いに異なる必要がある．


既知の問題点
------------
  * クライアントが異常終了した場合の挙動があやしい
