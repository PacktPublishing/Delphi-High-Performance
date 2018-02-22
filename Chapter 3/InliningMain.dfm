object frmInlining: TfrmInlining
  Left = 0
  Top = 0
  Caption = 'Inlining'
  ClientHeight = 154
  ClientWidth = 123
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Not inlined'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Inlined'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 106
    Width = 75
    Height = 25
    Caption = 'Inlined? Not!'
    TabOrder = 2
    OnClick = Button3Click
  end
end
