object frmThreads: TfrmThreads
  Left = 0
  Top = 0
  Caption = 'Threads'
  ClientHeight = 305
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    643
    305)
  PixelsPerInch = 96
  TextHeight = 13
  object btnThread: TButton
    Left = 16
    Top = 16
    Width = 137
    Height = 33
    Caption = 'TThread'
    TabOrder = 0
    OnClick = btnThreadClick
  end
  object btnFreeOnTerm: TButton
    Left = 16
    Top = 95
    Width = 137
    Height = 33
    Caption = 'Free on terminate'
    TabOrder = 1
    OnClick = btnFreeOnTermClick
  end
  object btnAnonymous: TButton
    Left = 16
    Top = 142
    Width = 137
    Height = 33
    Caption = 'Anonymous thread'
    TabOrder = 2
    OnClick = btnAnonymousClick
  end
  object ListBox1: TListBox
    Left = 168
    Top = 16
    Width = 459
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object btnStopThread: TButton
    Left = 78
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 4
    OnClick = btnStopThreadClick
  end
  object btnExceptThread: TButton
    Left = 16
    Top = 190
    Width = 137
    Height = 33
    Caption = 'Exception in a thread'
    TabOrder = 5
    OnClick = btnExceptThreadClick
  end
end
