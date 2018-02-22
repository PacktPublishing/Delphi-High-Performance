object frmDataTypes: TfrmDataTypes
  Left = 0
  Top = 0
  Caption = 'Data types'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    635
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCopyOnWrite: TButton
    Left = 16
    Top = 16
    Width = 161
    Height = 41
    Caption = 'string copy-on-write'
    TabOrder = 0
    OnClick = btnCopyOnWriteClick
  end
  object ListBox1: TListBox
    Left = 192
    Top = 16
    Width = 425
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnSharedDynArrays: TButton
    Left = 16
    Top = 72
    Width = 161
    Height = 41
    Caption = 'shared dynamic arrays'
    TabOrder = 2
    OnClick = btnSharedDynArraysClick
  end
  object btnRecordInit: TButton
    Left = 16
    Top = 128
    Width = 161
    Height = 41
    Caption = 'record initialization'
    TabOrder = 3
    OnClick = btnRecordInitClick
  end
  object btnCopyRec: TButton
    Left = 16
    Top = 183
    Width = 161
    Height = 41
    Caption = 'record copying'
    TabOrder = 4
    OnClick = btnCopyRecClick
  end
end
