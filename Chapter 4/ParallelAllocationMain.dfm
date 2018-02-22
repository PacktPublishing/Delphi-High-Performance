object frmParallelAllocation: TfrmParallelAllocation
  Left = 0
  Top = 0
  Caption = 'Parallel allocation'
  ClientHeight = 318
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    473
    318)
  PixelsPerInch = 96
  TextHeight = 13
  object btnRunTest: TButton
    Left = 24
    Top = 19
    Width = 75
    Height = 25
    Caption = 'Run test'
    TabOrder = 0
    OnClick = btnRunTestClick
  end
  object ListBox1: TListBox
    Left = 120
    Top = 19
    Width = 329
    Height = 278
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
end
