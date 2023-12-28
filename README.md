# listr

難解プログラミング言語 listr (リスタ と読まれたい) のインタプリタ `listr.hs`

### 言語の概要

データはすべてリストであり、文字列は文字コード (リストの長さによって表される自然数) のリストとして表される。

プログラムは入力文字列を出力文字列に書き換える命令として、以下の命令を組み合わせて作られる (左から順に実行される)。

|命令|内容|備考|
|:---:|---|---|
|`0`|(head) [a, …] → a, [] → []||
|`1`|(tail) [a, …] → […], [] → []||
|_x_`.`_y_|(cons) 命令 _x_ を実行した結果を、命令 _y_ を実行した結果の先頭に追加|右結合: _x_`.`_y_`.`_z_ = _x_`.(`_y_`.`_z_`)`|
|_x_`\|`_y_|(while) 命令 _x_ を実行した結果が [] になるまで命令 _y_ を実行|右結合、`.`よりも優先順位が低い: _x_`.`_y_`\|`_z_`.`_w_ = `(`_x_`.`_y_`)\|(`_z_`.`_w_`)`|
|`(`_x_`)`|グループ化||

### チューリング完全性の証明

brainfuck で書かれたプログラムは以下の手順で listr のプログラムに変換される。

1. 以下の置き換えを行う
    - `+` → `(0(0.).1)`
    - `,` → `(11100.10.110.11101.1111)`
    - `-` → `(01.1)`
    - `.` → `(0.10.110.1110.(0.11110).11111)`
    - `<` → `(100.101.(0.110).111)`
    - `>` → `(1100.(0.10).1101.111)`
    - `[` → `(0|`
    - `]` → `)`
2. 冒頭に `(.00)(1.1.1.0.1)`、末尾に `1111(0.1.1)(0|01.(00.10).11)10` を追加する
