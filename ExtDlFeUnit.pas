(*
  外部ダウンローダーGUI

  1.3 2025/02/05  外部ダウンローダー設定ファイルExtDLoader.txtのコメント行処理を追加した
                  外部ダウンローダー設定ファイルを選択出来るようにした
  1.2 2024/12/22  小説サイト定義をExtDLoader.txtから読み込む用にした
  1.1 2024/12/22  hamelndlを追加した
  1.0 2024/06/27  初版
*)
unit ExtDlFeUnit;

interface

uses
  System.Classes, System.SysUtils, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls,  Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Dialogs, WinAPI.Windows, WinAPI.Messages, WinAPI.ShellAPI,
  System.RegularExpressions, EllipsEd;

type

  { TExtDlfe }

  TExtDlFe = class(TForm)
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
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure StartBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DLOpenBtnClick(Sender: TObject);
  private
    Cancel: boolean;
    StartTime: TTime;
    PNo,
    PCnt: integer;
    Busy: Boolean;
    SiteUrl,
    Dloader: TStringList;
    ExtDL: string;
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

implementation

{$R *.dfm}


// 外部ダウンローダーからのメッセージを受け取る
procedure TExtDlFe.WMCopyData(var Message: TWMCopyData);
var
  ttlauth: TStringList;
begin
  PNo := Message.CopyDataStruct.dwData - 1;
  if PNo < 0 then
    PNo := 0;
  ttlauth := TStringList.Create;
  try
    ttlauth.CommaText := PChar(Message.CopyDataStruct.lpData);
    if ttlauth.Count > 1 then
    begin
      NvTitle.Caption := '作品タイトル：' + ttlauth[0];
    end;
  finally
    ttlauth.Free;
  end;
  Status.Caption := '各話を取得中 [  0/' + Format('%3d', [PNo]) + ']';
end;

// 外部ダウンローダーからDL状況を受け取る
procedure TExtDlFe.WMDownloaderInfo(var Message: TMessage);
begin
  PCnt := Message.wParam;
  Status.Caption := '各話を取得中 [' + Format('%3d', [PCnt]) + '/' + Format('%3d', [PNo]) + '(' + Format('%d', [(PCnt * 100) div PNo]) + '%)]';
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
    if TRegEx.IsMatch(Aurl, SiteUrl[i]) then
    begin
      Result := DLoader[i];
      Break;
    end;
  end;
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
  SXInfo: TShellExecuteInfo;
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

  ShellExecuteEx(@SXInfo);

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
  CloseHandle(SXInfo.hProcess);

  if PNo = PCnt then
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
  PNo := 0;
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

