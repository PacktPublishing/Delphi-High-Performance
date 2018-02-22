object btnParallelFor: TbtnParallelFor
  Left = 0
  Top = 0
  ActiveControl = btnFor
  Caption = 'TParallel.For'
  ClientHeight = 306
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    641
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object btnFor: TButton
    Left = 16
    Top = 16
    Width = 137
    Height = 25
    Caption = 'for'
    TabOrder = 0
    OnClick = btnForClick
  end
  object ListBox1: TListBox
    Left = 168
    Top = 16
    Width = 459
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object btnParallelFor: TButton
    Left = 16
    Top = 56
    Width = 137
    Height = 25
    Caption = 'parallel for - good'
    TabOrder = 1
    OnClick = btnParallelForClick
  end
  object btnAsyncParallelFor: TButton
    Left = 16
    Top = 144
    Width = 137
    Height = 25
    Caption = 'async parallel for'
    TabOrder = 3
    OnClick = btnAsyncParallelForClick
  end
  object btnParalleForBad: TButton
    Left = 16
    Top = 96
    Width = 137
    Height = 25
    Caption = 'parallel for - bad'
    TabOrder = 2
    OnClick = btnParalleForBadClick
  end
  object btnParallelForException: TButton
    Left = 16
    Top = 192
    Width = 137
    Height = 25
    Caption = 'parallel for exception'
    TabOrder = 4
    OnClick = btnParallelForExceptionClick
  end
end
