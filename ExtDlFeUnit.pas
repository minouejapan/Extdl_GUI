(*
  外部ダウンローダーGUI

  1.52  2025/11/16  MetaDarkStyleによるダークモード対応を追加した
  1.51  2025/11/15  実行中はクリップボード監視が無効になるようにっした
  1.5   2025/11/14  開発環境をDelphiからLazarusに変更した
                    URLのクリップボード監視オプションを追加した
  1.4   2025/11/13  ダウンロードページ数処理の不具合を修正した
  1.3   2025/02/05  外部ダウンローダー設定ファイルExtDLoader.txtのコメント行処理を追加した
                    外部ダウンローダー設定ファイルを選択出来るようにした
  1.2   2024/12/22  小説サイト定義をExtDLoader.txtから読み込む用にした
  1.1   2024/12/22  hamelndlを追加した
  1.0   2024/06/27  初版
*)
unit ExtDlFeUnit;

{$MODE Delphi}
{$CODEPAGE UTF8}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,  StdCtrls, Buttons, Dialogs,
  Windows, Messages, ShellAPI, LazUTF8, RegExpr, ClipBrd, uDarkStyleSchemes;

type
  {$IFDEF FPC}
    TWMCopyData = record
      Msg: Cardinal;
      MsgFiller: TDWordFiller;
      From: HWND;
      CopyDataStruct: PCopyDataStruct;
      Result: LRESULT;
    end;
  {$ENDIF}

  { TExtDlfe }

  TExtDlFe = class(TForm)
		IsWatchCB: TCheckBox;
    Panel2: TPanel;
    CancelBtn: TButton;
    StartBtn: TButton;
    NvTitle: TLabel;
    URL: TEdit;
    Elapsed: TLabel;
    Status: TLabel;
    Label1: TLabel;
    ExtDLF: TLabel;
    DLOpenBtn: TSpeedButton;
    OD: TOpenDialog;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure StartBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DLOpenBtnClick(Sender: TObject);
  private
    FNextClipboardOwner: HWnd;
    Cancel: boolean;
    StartTime: TTime;
    PNo,
    PCnt: integer;
    Busy: Boolean;
    SiteUrl,
    Dloader: TStringList;
    ExtDL: string;
    function WMChangeCBChain(AwParam: WParam; AlParam: LParam):LRESULT;
    function WMDrawClipboard(AwParam: WParam; AlParam: LParam):LRESULT;
    function LoadExtList: boolean;
    procedure ExecLoader(Aurl: string; ExeName: string);
    function WhichLoader(Aurl: string): string;
  public
  protected
    procedure WMDownloaderInfo(var Message: TMessage); message WM_USER + 30;
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
  end;

var
  ExtDlFe: TExtDlFe;
  PrevWndProc: WNDPROC;

implementation

{$R *.lfm}

function DecodeAozoraTag(Src: string): string;
var
  tmp: string;
begin
  tmp := StringReplace(Src, '※［＃始め二重山括弧、1-1-52］',    '《', [rfReplaceAll]);
  tmp := StringReplace(tmp, '※［＃終わり二重山括弧、1-1-53］',  '》', [rfReplaceAll]);
  tmp := StringReplace(tmp, '※［＃縦線、1-1-35］',              '｜', [rfReplaceAll]);
  Result := tmp;
end;

// Delphiではprocedure xxx(var Message WMxxx); message WM_XXXX;と宣言することで
// 指定したMessageを受け取った際の処理を記述することが出来るが、Lazarusでは出来
// ないので登録したメッセージ処理内で対応する
//
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
var
  ws: WideString;
  s: string;
  ttlauth: TStringList;
begin
  case uMsg of
    WM_COPYDATA:
      begin
        // 送られてくる文字コードはUTF16形式なのでUTF8に変換する
        ws := PWideChar(PCopyDataStruct(LParam).lpData);
        s := UTF16ToUTF8(ws);
        // タイトル名,作者名で送られてくるのでタイトルだけを分離する
        ttlauth := TStringList.Create;
        try
          ttlauth.Delimiter := ',';
          ttlauth.StrictDelimiter := True;
          ttlauth.CommaText := s;
          if ttlauth.Count > 0 then
            s := ttlauth[0];
        finally
          ttlauth.Free;
        end;
        with ExtDlFe do
        begin
          PNo := PCopyDataStruct(LParam).dwData;
          NvTitle.Caption := '作品タイトル：' + DecodeAozoraTag(s);
          Status.Caption := '各話を取得中 [  0/' + Format('%3d', [PNo]) + ']';
				end;
			end;
    WM_USER + 30:
      begin
        with ExtDlFe do
        begin
          PCnt := wParam;
          Status.Caption := '各話を取得中 [' + Format('%3d', [PCnt]) + '/' + Format('%3d', [PNo]) + '(' + Format('%d', [(PCnt * 100) div PNo]) + '%)]';
        end;
			end;
		// https://wiki.lazarus.freepascal.org/Clipboard
    WM_CHANGECBCHAIN:
      begin
        Result := ExtDlFe.WMChangeCBChain(wParam, lParam);
      end;
    WM_DRAWCLIPBOARD:
      begin
        Result := ExtDlFe.WMDrawClipboard(wParam, lParam);
      end;
    else
      Result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
  end;
end;

// https://wiki.lazarus.freepascal.org/Clipboard
function TExtDlFe.WMChangeCBChain(AwParam: WParam; AlParam: LParam): LRESULT;
var
  Remove, Next: THandle;
begin
  Remove := AwParam;
  Next := AlParam;
  if FNextClipboardOwner = Remove then FNextClipboardOwner := Next
    else if FNextClipboardOwner <> 0 then
      SendMessage(FNextClipboardOwner, WM_ChangeCBChain, Remove, Next);
  Result := 0;
end;

function TExtDlFe.WMDrawClipboard(AwParam: WParam; AlParam: LParam): LRESULT;
var
  u: string;
begin
  if IsWatchCB.Checked and (not Busy) then
    if Clipboard.HasFormat(CF_TEXT) Then
    Begin
      u := Clipboard.AsText;
      if ExecRegExpr('https://(ncode.syosetu.com|novel18.syosetu.com|kakuyomu.jp|novelup.plus|novelba.com|novelism.jp|www.berrys-cafe.jp|novel.daysneo.com|novema.jp)', u) then
        URL.Text := Clipboard.AsText;
    end;
  SendMessage(FNextClipboardOwner, WM_DRAWCLIPBOARD, 0, 0);
  Result := 0;
end;

function TExtDlFe.LoadExtList: boolean;
var
  exlist, eachlt: TStringList;
  i: integer;
begin
  Result := False;
  // 外部ダウンローダー情報を取得
  if FileExists(ExtDL) then
  begin
    exlist := TStringList.Create;
    eachlt := TStringList.Create;
    try
      exlist.WriteBOM := False;
      exlist.LoadFromFile(extdl, TEncoding.UTF8);
      for i := 0 to exlist.Count - 1 do
      begin
        // コメント行をスキップ
        if Pos('#', exlist[i]) = 1 then
          Continue;
        eachlt.CommaText := exlist[i];
        if eachlt.Count < 2 then  // リストが不完全
          Continue;
        if eachlt[4] = '1' then   // Windowsアプリは登録しない
          Continue;
        SiteUrl.Add(eachlt[0]);
        DLoader.Add(eachlt[2]);
      end;
      ExtDLF.Caption := '設定:' + ExtractFileName(ExtDL);
      Result := True;
    finally
      eachlt.Free;
      exlist.Free;
    end;
  end;
end;

// URLから起動させる外部ダウンローダーを返す
function TExtDlFe.WhichLoader(Aurl: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to SiteUrl.Count - 1 do
  begin
    if ExecRegExpr(SiteUrl[i], Aurl) then
    begin
      Result := DLoader[i];
      Break;
    end;
  end;
end;

procedure TExtDlFe.WMDownloaderInfo(var Message: TMessage);
begin

end;

procedure TExtDlFe.WMCopyData(var Message: TWMCopyData);
begin

end;

// 外部ダウンローダー設定ファイルを指定する
procedure TExtDlFe.DLOpenBtnClick(Sender: TObject);
begin
  with OD do
  begin
    if ExtDL <> '' then
      FileName := ExtDL
    else
      InitialDir := ExtractFileDir(Application.ExeName);
    if Execute then
    begin
      ExtDL := FileName;
      LoadExtList;
    end;
  end;
end;

// 外部ダウンローダーを起動させる
procedure TExtDlFe.ExecLoader(Aurl: string; ExeName: string);
var
  cmd, kdl: string;
  SXInfo: TShellExecuteInfoA;
  ret: Cardinal;
begin
  // 外部ダウンローダーをフルパス名にする
  kdl := ExtractFilePath(Application.ExeName) + ExeName;
  if not FileExists(kdl) then
  begin
    Status.Caption := ExeName + 'が見つかりません.';
    Exit;
  end;
  // コマンドライン引数を準備する(-hXXXXは外部ダウンローダーに渡すハンドル)
  cmd := Aurl + ' -h' + IntToStr(Handle);
  with SXInfo do//TShellExecuteInfo構造体の初期化
  begin
    cbSize := SizeOf(SXInfo);
    fMask := SEE_MASK_NOCLOSEPROCESS;//これがないと終了待ち出来ない
    Wnd := Application.Handle;
    lpVerb := nil;
    lpFile := PChar(kdl);
    lpParameters := PChar(cmd);
    lpDirectory := Nil;
    nShow := SW_HIDE;
  end;

  ShellExecuteExA(LPSHELLEXECUTEINFOA(@SXInfo));

  ret := 0;
  //起動したアプリケーションの終了待ち
  while WaitForSingleObject(SXInfo.hProcess, 0) = WAIT_TIMEOUT do
  begin
    Application.ProcessMessages;
    Sleep(200);
    Elapsed.Caption := '経過時間：' + FormatDateTime('nn:ss', Now - StartTime);
    if Cancel then
      TerminateProcess(SXInfo.hProcess, ret);
  end;
  // 終了コードを取得する
  GetExitCodeProcess(SXInfo.hProcess, ret);
  FileClose(SXInfo.hProcess);{ *CloseHandleから変換* }

  if PNo >= PCnt then
    Status.Caption := 'ダウンロードを完了しました.'
  else if Cancel then
  begin
	  Status.Caption := 'ダンロードを中止しました.';
  end else
	  Status.Caption := 'ダンロードが失敗しました.';

end;

procedure TExtDlFe.FormCreate(Sender: TObject);
var
  cfg, opt: string;
  f: TextFile;
begin
  ExtDL := ExtractFilePath(Application.ExeName) + 'ExtDLoader.txt';
  // 表示ポジションを読み込む
  cfg := ChangeFileExt(Application.ExeName, '.cfg');
  if FileExists(cfg) then
  begin
    try
      AssignFile(f, cfg);
      Reset(f);
      if ParamCount = 0 then
      begin
        Readln(f, Opt);
        if Opt = '' then
          Opt := '300';
        Left := StrToInt(Opt);
        Readln(f, Opt);
        if Opt = '' then
          Opt := '200';
        Top := StrToInt(Opt);
        Readln(f, Opt);
        if Opt <> '' then
        begin
          if FileExists(Opt) then
            ExtDL := Opt;
        end;
        Readln(f, Opt);
        if Opt <> '' then
          IsWatchCB.Checked := Opt = '1';
      end;
    finally
      CloseFile(f);
    end;
  end;
  SiteUrl := TStringList.Create;
  Dloader := TStringList.Create;
  if not LoadExtList then
  begin
    MessageDlg('ExtDLoader.txtが見つかりません.', mtError, [mbOK], 0);
    Close;
  end;
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrUInt(@WndCallback)));
  FNextClipboardOwner := SetClipboardViewer(Self.Handle);
  PNo := 0;
end;

procedure TExtDlFe.FormDestroy(Sender: TObject);
begin
  ChangeClipboardChain(Handle, FNextClipboardOwner);
end;

procedure TExtDlFe.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Cancel := True;
end;

procedure TExtDlFe.StartBtnClick(Sender: TObject);
var
  ExtLdr: string;
label
  Quit;
begin
  if (Pos('https:', URL.Text) = 0) or (Length(URL.Text) < 16) then
  begin
    Status.Caption := 'URLをセットしてください.';
    Goto Quit;
  end;

  ExtLdr := WhichLoader(URL.Text);
  if ExtLdr = '' then
  begin
    Status.Caption := 'URLが無効です.';
    Goto Quit;
  end;

  StartBtn.Enabled := False;
  CancelBtn.Enabled := True;
  URL.Enabled := False;
  Cancel := False;
  Busy := True;
  NvTitle.Caption := '作品タイトル：';


  StartTime := Now;

  ExecLoader(URL.Text, ExtLdr);

  Elapsed.Caption := '経過時間：' + FormatDateTime('nn:ss', Now - StartTime);
Quit:
  URL.Enabled := True;
  StartBtn.Enabled := True;
  CancelBtn.Enabled := False;
  Busy := False;
end;

procedure TExtDlFe.FormClose(Sender: TObject; var Action: TCloseAction);
var
  cfg, opt: string;
  f: TextFile;
begin
  SiteUrl := TStringList.Create;
  Dloader := TStringList.Create;
  // 表示ポジションを保存する
  cfg := ChangeFileExt(Application.ExeName, '.cfg');
  try
    AssignFile(f, cfg);
    Rewrite(f);
    opt := IntToStr(Left);
    Writeln(f, opt);
    opt := IntToStr(Top);
    Writeln(f, opt);
    Writeln(f, ExtDL);
    if IsWatchCB.Checked then
      opt := '1'
    else
      opt := '0';
    Writeln(f, opt);
  finally
    CloseFile(f);
  end;
end;

procedure TExtDlFe.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Busy;
end;

procedure TExtDlFe.CancelBtnClick(Sender: TObject);
begin
  Cancel := True;
end;

end.

