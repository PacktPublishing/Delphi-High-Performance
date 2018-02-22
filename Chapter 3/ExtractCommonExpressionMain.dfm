object frmCommonExpression: TfrmCommonExpression
  Left = 0
  Top = 0
  Caption = 'frmCommonExpression'
  ClientHeight = 248
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 185
    Height = 25
    Caption = 'Complicated expression'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 232
    Top = 24
    Width = 185
    Height = 201
    ItemHeight = 13
    TabOrder = 1
  end
  object Button2: TButton
    Left = 24
    Top = 64
    Width = 185
    Height = 25
    Caption = 'Extracted common subexpressions'
    TabOrder = 2
    OnClick = Button2Click
  end
end
