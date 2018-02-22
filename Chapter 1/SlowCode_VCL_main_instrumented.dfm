object frmSlowCode: TfrmSlowCode
  Left = 0
  Top = 0
  Caption = 'Slow Code'
  ClientHeight = 569
  ClientWidth = 818
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    818
    569)
  PixelsPerInch = 96
  TextHeight = 13
  object lblHowMany: TLabel
    Left = 24
    Top = 24
    Width = 98
    Height = 13
    Caption = 'How many numbers:'
  end
  object inpHowMany: TSpinEdit
    Left = 136
    Top = 21
    Width = 81
    Height = 22
    MaxValue = 9999999
    MinValue = 1
    TabOrder = 0
    Value = 100
  end
  object btnTest: TButton
    Left = 232
    Top = 19
    Width = 75
    Height = 25
    Caption = 'Test!'
    TabOrder = 1
    OnClick = btnTestClick
  end
  object outResults: TMemo
    Left = 8
    Top = 64
    Width = 801
    Height = 497
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
end
