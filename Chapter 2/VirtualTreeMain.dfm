object frmVTV: TfrmVTV
  Left = 0
  Top = 0
  Caption = 'Virtual TreeView'
  ClientHeight = 465
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 109
    Width = 31
    Height = 13
    Caption = 'listbox'
  end
  object Label2: TLabel
    Left = 192
    Top = 109
    Width = 77
    Height = 13
    Caption = 'virtual TreeView'
  end
  object lblLog10k: TLabel
    Left = 160
    Top = 29
    Width = 44
    Height = 13
    Caption = 'lblLog10k'
    Visible = False
  end
  object lblLogAdd100: TLabel
    Left = 160
    Top = 60
    Width = 64
    Height = 13
    Caption = 'lblLogAdd100'
    Visible = False
  end
  object Label3: TLabel
    Left = 360
    Top = 109
    Width = 125
    Height = 13
    Caption = 'virtual TreeView -autosort'
  end
  object Label4: TLabel
    Left = 527
    Top = 109
    Width = 118
    Height = 13
    Caption = 'virtual TreeView +OnInit'
  end
  object VirtualStringTree1: TVirtualStringTree
    Left = 192
    Top = 128
    Width = 137
    Height = 313
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    NodeDataSize = 4
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
    OnGetText = VirtualStringTree1GetText
    Columns = <>
  end
  object ListBox1: TListBox
    Left = 24
    Top = 128
    Width = 137
    Height = 313
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 121
    Height = 25
    Caption = 'Add 10,000 lines'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 55
    Width = 121
    Height = 25
    Caption = 'Add 1 line 100 times'
    TabOrder = 3
    OnClick = Button2Click
  end
  object VirtualStringTree2: TVirtualStringTree
    Left = 360
    Top = 128
    Width = 137
    Height = 313
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    NodeDataSize = 4
    TabOrder = 4
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
    OnGetText = VirtualStringTree2GetText
    Columns = <>
  end
  object VirtualStringTree3: TVirtualStringTree
    Left = 527
    Top = 128
    Width = 137
    Height = 313
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    NodeDataSize = 4
    TabOrder = 5
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
    OnGetText = VirtualStringTree3GetText
    OnInitNode = VirtualStringTree3InitNode
    Columns = <>
  end
end
