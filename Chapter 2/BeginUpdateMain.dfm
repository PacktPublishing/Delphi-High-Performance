object frmBeginUpdate: TfrmBeginUpdate
  Left = 0
  Top = 0
  Caption = 'BeginUpdate / EndUpdate'
  ClientHeight = 545
  ClientWidth = 761
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    761
    545)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 233
    Height = 57
    Caption = 'Add lines _without_ BeginUpdate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 16
    Top = 88
    Width = 357
    Height = 421
    ItemHeight = 13
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 388
    Top = 88
    Width = 357
    Height = 421
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 526
    Width = 761
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Button2: TButton
    Left = 264
    Top = 16
    Width = 233
    Height = 57
    Caption = 'Add lines _with_ BeginUpdate'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 512
    Top = 16
    Width = 233
    Height = 57
    Caption = 'Add text'
    TabOrder = 5
    OnClick = Button3Click
  end
end
