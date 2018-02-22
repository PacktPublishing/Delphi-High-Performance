object frmPointers: TfrmPointers
  Left = 0
  Top = 0
  Caption = 'Pointers'
  ClientHeight = 299
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    498
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object btnArray: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Array'
    TabOrder = 0
    OnClick = btnArrayClick
  end
  object ListBox1: TListBox
    Left = 112
    Top = 16
    Width = 367
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnPointer: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Pointer'
    TabOrder = 2
    OnClick = btnPointerClick
  end
  object btnPointerMath: TButton
    Left = 16
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Pointer math'
    TabOrder = 3
    OnClick = btnPointerMathClick
  end
end
