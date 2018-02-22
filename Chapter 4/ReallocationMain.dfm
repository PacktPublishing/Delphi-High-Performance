object frmReallocation: TfrmReallocation
  Left = 0
  Top = 0
  Caption = 'Reallocation'
  ClientHeight = 415
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
    415)
  PixelsPerInch = 96
  TextHeight = 13
  object btnAppendString: TButton
    Left = 24
    Top = 24
    Width = 137
    Height = 49
    Caption = 'Append String'
    TabOrder = 0
    OnClick = btnAppendStringClick
  end
  object btnSetLength: TButton
    Left = 24
    Top = 87
    Width = 137
    Height = 49
    Caption = 'SetLength String'
    TabOrder = 1
    OnClick = btnSetLengthClick
  end
  object ListBox1: TListBox
    Left = 192
    Top = 24
    Width = 281
    Height = 367
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object btnAppendArray: TButton
    Left = 24
    Top = 152
    Width = 137
    Height = 49
    Caption = 'Append array'
    TabOrder = 2
    OnClick = btnAppendArrayClick
  end
  object btnSetLengthArray: TButton
    Left = 24
    Top = 215
    Width = 137
    Height = 49
    Caption = 'SetLength array'
    TabOrder = 3
    OnClick = btnSetLengthArrayClick
  end
  object btnAppendTList: TButton
    Left = 24
    Top = 279
    Width = 137
    Height = 49
    Caption = 'Append TList'
    TabOrder = 4
    OnClick = btnAppendTListClick
  end
  object btnSetCapacityTList: TButton
    Left = 24
    Top = 342
    Width = 137
    Height = 49
    Caption = 'TList.SetCapacity'
    TabOrder = 5
    OnClick = btnSetCapacityTListClick
  end
end
