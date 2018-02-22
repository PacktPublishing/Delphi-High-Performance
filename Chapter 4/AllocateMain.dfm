object frmAllocate: TfrmAllocate
  Left = 0
  Top = 0
  Caption = 'Allocate'
  ClientHeight = 505
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    545
    505)
  PixelsPerInch = 96
  TextHeight = 13
  object btnAllocClass: TButton
    Left = 24
    Top = 24
    Width = 129
    Height = 41
    Caption = 'Allocate objects'
    TabOrder = 0
    OnClick = btnAllocClassClick
  end
  object btnAllocRecord: TButton
    Left = 24
    Top = 71
    Width = 129
    Height = 41
    Caption = 'Allocate records'
    TabOrder = 1
    OnClick = btnAllocRecordClick
  end
  object ListBox1: TListBox
    Left = 184
    Top = 24
    Width = 337
    Height = 457
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnAllocGeneric: TButton
    Left = 24
    Top = 135
    Width = 129
    Height = 41
    Caption = 'Allocate node<string>'
    TabOrder = 3
    OnClick = btnAllocGenericClick
  end
end
