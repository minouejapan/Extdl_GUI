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
    ExplicitWidth = 802
    object NvTitle: TLabel
      Left = 16
      Top = 58
      Width = 763
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
end
