object frmSharedList: TfrmSharedList
  Left = 0
  Top = 0
  Caption = 'Shared list'
  ClientHeight = 305
  ClientWidth = 642
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    642
    305)
  PixelsPerInch = 96
  TextHeight = 13
  object btnShared: TButton
    Left = 16
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Shared lists'
    TabOrder = 0
    OnClick = btnSharedClick
  end
  object ListBox1: TListBox
    Left = 137
    Top = 16
    Width = 491
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnLocked: TButton
    Left = 16
    Top = 56
    Width = 105
    Height = 25
    Caption = 'Locked lists'
    TabOrder = 2
    OnClick = btnLockedClick
  end
  object btnMREW: TButton
    Left = 16
    Top = 96
    Width = 105
    Height = 25
    Caption = 'MREW lists'
    TabOrder = 3
    OnClick = btnMREWClick
  end
  object Button1: TButton
    Left = 16
    Top = 136
    Width = 105
    Height = 25
    Caption = 'TThreadList'
    TabOrder = 4
    OnClick = Button1Click
  end
end
