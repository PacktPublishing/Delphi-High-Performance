object frmCacheTest: TfrmCacheTest
  Left = 0
  Top = 0
  Caption = 'Cache test'
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
  object Test: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = TestClick
  end
  object Memo1: TMemo
    Left = 108
    Top = 18
    Width = 509
    Height = 263
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
end
