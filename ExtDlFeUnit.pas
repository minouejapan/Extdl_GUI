(*
  外部ダウンローダーGUI

  1.0 2024/06/27  初版
*)
unit ExtDlFeUnit;

interface

uses
  System.Classes, System.SysUtils, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls,  Vcl.StdCtrls,
  Vcl.Buttons, Vcl.Dialogs, WinAPI.Windows, WinAPI.Messages, WinAPI.ShellAPI,
  System.RegularExpressions;

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
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure StartBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Cancel: boolean;
    StartTime: TTime;
    PNo,
    PCnt: integer;
    Busy: Boolean;
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

const
  // 各小説サイトのURL
  NvURL: Array[0..8] of string = (
                  'https://ncode.syosetu.com/n\d{4}\w{1,2}/',
                  'https://novel18.syosetu.com/n\d{4}\w{1,2}/',
                  'https://kakuyomu.jp/works/\d{19,20}',
                  'https://novelup.plus/story/\d{9,10}',
                  'https://novelba.com/indies/works/\d{6,7}',
                  'https://novelism.jp/novel/(\w|-){22}/',
                  'https://www.berrys-cafe.jp/pc/book/n\d{7}/',
                  'https://novel.daysneo.com/works/\w{32}.html',
                  'https://novema.jp/book/n\d{7}'
                );
  // 対応するダウンローダー
  ExtDL: Array[0..8] of string = (
                  'na6dl.exe',
                  'na6dl.exe',
                  'kakuyomudl.exe',
                  'novelupdl.exe',
                  'novelbadl.exe',
                  'novelismdl.exe',
                  'berrysdl.exe',
                  'nvldaysdl.exe',
                  'novemadl.exe'
                );
  ListCnt = 9;


// 外部ダウンローダーからのメッセージを受け取る
procedure TExtDlFe.WMCopyData(var Message: TWMCopyData);
var
  ttlauth: TStringList;
begin
  PNo := Message.CopyDataStruct.dwData;
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
  PCnt := Message.wParam + 1;
  Status.Caption := '各話を取得中 [' + Format('%3d', [PCnt]) + '/' + Format('%3d', [PNo]) + '(' + Format('%d', [(PCnt * 100) div PNo]) + '%)]';
end;

// URLから起動させる外部ダウンローダーを返す
function TExtDlFe.WhichLoader(Aurl: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to ListCnt - 1 do
  begin
    if TRegEx.IsMatch(Aurl, NvURL[i]) then
    begin
      Result := ExtDl[i];
      Break;
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
      end;
    finally
      CloseFile(f);
    end;
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
  // 表示ポジションを保存する
  cfg := ChangeFileExt(Application.ExeName, '.cfg');
  try
    AssignFile(f, cfg);
    Rewrite(f);
    opt := IntToStr(Left);
    Writeln(f, opt);
    opt := IntToStr(Top);
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

