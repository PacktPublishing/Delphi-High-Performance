object frmAsyncAwait: TfrmAsyncAwait
  Left = 0
  Top = 0
  Caption = 'Async/Await'
  ClientHeight = 177
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    521
    177)
  PixelsPerInch = 96
  TextHeight = 13
  object btnLongTask: TButton
    Left = 16
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Long task'
    TabOrder = 0
    OnClick = btnLongTaskClick
  end
  object btnLongTaskAsync: TButton
    Left = 16
    Top = 56
    Width = 113
    Height = 25
    Caption = 'Long task async'
    TabOrder = 1
    OnClick = btnLongTaskAsyncClick
  end
  object btnLongTaskAsync2: TButton
    Left = 16
    Top = 96
    Width = 113
    Height = 25
    Caption = 'Long task async'
    TabOrder = 2
    OnClick = btnLongTaskAsyncClick
  end
  object btnLongTaskAsync3: TButton
    Left = 16
    Top = 136
    Width = 113
    Height = 25
    Caption = 'Long task async'
    TabOrder = 3
    OnClick = btnLongTaskAsyncClick
  end
  object ListBox1: TListBox
    Left = 152
    Top = 16
    Width = 353
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
  end
end
