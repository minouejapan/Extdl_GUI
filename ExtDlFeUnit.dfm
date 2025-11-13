object ExtDlFe: TExtDlFe
  Left = 315
  Top = 31
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #22806#37096#12480#12454#12531#12525#12540#12480#12540'GUI'
  ClientHeight = 81
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'Yu Gothic UI'
  Font.Pitch = fpVariable
  Font.Style = []
  Font.Quality = fqDraft
  KeyPreview = True
  OldCreateOrder = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 787
    Height = 79
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object NvTitle: TLabel
      Left = 16
      Top = 58
      Width = 595
      Height = 15
      AutoSize = False
      Caption = #20316#21697#12479#12452#12488#12523#65306
      Color = clBtnFace
      ParentColor = False
    end
    object Elapsed: TLabel
      Left = 516
      Top = 12
      Width = 87
      Height = 15
      Caption = #32076#36942#26178#38291#65306'00:00'
      Color = clBtnFace
      ParentColor = False
    end
    object Status: TLabel
      Left = 204
      Top = 11
      Width = 24
      Height = 15
      Caption = #29366#24907
      Color = clBtnFace
      ParentColor = False
    end
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 95
      Height = 15
      Caption = #23567#35500'TOP'#12506#12540#12472'URL'
      Color = clBtnFace
      ParentColor = False
    end
    object ExtDLF: TLabel
      Left = 617
      Top = 60
      Width = 103
      Height = 15
      Caption = #35373#23450':ExtDLoader.txt'
    end
    object DLOpenBtn: TSpeedButton
      Left = 756
      Top = 57
      Width = 23
      Height = 22
      Hint = #22806#37096#12480#12454#12531#12525#12540#12480#12540#23450#32681#12501#12449#12452#12523#12434#38283#12367
      Caption = #12539#12539#12539
      Flat = True
      ParentShowHint = False
      ShowHint = True
      OnClick = DLOpenBtnClick
    end
    object CancelBtn: TButton
      Left = 709
      Top = 29
      Width = 70
      Height = 25
      Caption = #20013#27490'(&A)'
      Enabled = False
      TabOrder = 0
      OnClick = CancelBtnClick
    end
    object StartBtn: TButton
      Left = 617
      Top = 29
      Width = 86
      Height = 25
      Caption = #12480#12454#12531#12525#12540#12489'(&D)'
      TabOrder = 1
      OnClick = StartBtnClick
    end
    object URL: TEdit
      Left = 12
      Top = 29
      Width = 599
      Height = 23
      TabOrder = 2
    end
  end
  object OD: TOpenDialog
    DefaultExt = 'txt'
    Filter = #12486#12461#12473#12488#12501#12449#12452#12523'(*.txt)|*.txt|'#20840#12390#12398#12501#12449#12452#12523'(*.*)|*.*'
    Title = #22806#37096#12480#12454#12531#12525#12540#12480#12540#35373#23450#12501#12449#12452#12523#12434#25351#23450#12375#12390#19979#12373#12356
    Left = 688
  end
end
