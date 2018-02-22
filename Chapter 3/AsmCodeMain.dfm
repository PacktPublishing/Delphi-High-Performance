object frmAsmCode: TfrmAsmCode
  Left = 0
  Top = 0
  Caption = 'Pascal vs. assembler'
  ClientHeight = 299
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    365
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object btnPascal: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Pascal'
    TabOrder = 0
    OnClick = btnPascalClick
  end
  object btnAsm: TButton
    Left = 16
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Assembler'
    TabOrder = 1
    OnClick = btnAsmClick
  end
  object ListBox1: TListBox
    Left = 112
    Top = 16
    Width = 233
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
