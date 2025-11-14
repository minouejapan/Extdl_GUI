## 外部ダウンローダー用Windows GUI
na6dl等のNaro2mobi用コンソールアプリケーション型外部ダウンローダーをWindows上で実行するためのフロントエンドGUIです。<br>
作品URLで自動的に対応する外部ダウンローダーを起動してダウンロードします。<br>
<br>
起動するコンソールアプリケーション型の外部ダウンローダーは、na6dl.exe, kakuyomudl.exe, novelupdl.exe, novelbadl.exe, novelismdl.exe, berrysdl.exe, nvldaysdl.exe, novemadl.exeです。

![extdl_gui15](https://github.com/user-attachments/assets/4f25b9b8-40c2-4b7a-ad23-54419c083c85)


## 使い方
解凍したextdl_gui.exeを外部ダウンローダーと同じフォルダー内にコピーして起動すれば使用できます。<br>
- Naro2mobiで使用する外部ダウンローダー定義ファイルExDLoader.txtを読み込みますので、extdl_gui.exeをNaro2mobiと同じフォルダ内に置くか、extdl_gui.exeのフォルダにExDLoader.txtをコピーして下さい。
- ver1.3から外部ダウンローダー設定(定義)ファイルを指定出来るようになりました。ウィンドウ右下の[・・・]ボタンを押して使用したい設定ファイルを選択してください。
- ver1.5でクリップボード監視機能を追加しました。「クリップボードを監視する」チェックをオンにすると監視が有効になります。尚、各作品サイトURLを簡易的にチェックしているため、登録されている作品サイト以外のURLをコピーしても無視されます。


## ビルド方法
Lazarus3.6以降でプロジェクトファイルextdl_gui.lpiを開いてビルドして下さい。
尚、ビルドにはTRegExprが必要です。TRegExprは https://github.com/andgineer/TRegExpr から取得してください。


## ライセンス
MIT
