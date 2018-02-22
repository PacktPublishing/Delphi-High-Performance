object frmFuture: TfrmFuture
  Left = 0
  Top = 0
  Caption = 'TTask.Future'
  ClientHeight = 305
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    633
    305)
  PixelsPerInch = 96
  TextHeight = 13
  object btnFuture: TButton
    Left = 16
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Create Future'
    DragKind = dkDock
    TabOrder = 0
    OnClick = btnFutureClick
  end
  object btnGetValue: TButton
    Left = 48
    Top = 47
    Width = 73
    Height = 25
    Caption = 'Get value'
    DragKind = dkDock
    TabOrder = 1
    OnClick = btnGetValueClick
  end
  object ListBox1: TListBox
    Left = 136
    Top = 16
    Width = 481
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object btnFuture2: TButton
    Left = 16
    Top = 96
    Width = 105
    Height = 25
    Caption = 'Create Future 2'
    DragKind = dkDock
    TabOrder = 2
    OnClick = btnFuture2Click
  end
end
