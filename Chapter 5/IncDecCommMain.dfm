object frmIncDecComm: TfrmIncDecComm
  Left = 0
  Top = 0
  Caption = 'Inc/Dec with communication'
  ClientHeight = 331
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 19
  object btnMessage: TButton
    Left = 8
    Top = 8
    Width = 210
    Height = 41
    Caption = 'Windows message'
    TabOrder = 0
    OnClick = btnMessageClick
  end
  object ListBox1: TListBox
    Left = 224
    Top = 0
    Width = 411
    Height = 331
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 19
    TabOrder = 5
  end
  object btnSynchronize: TButton
    Left = 8
    Top = 102
    Width = 210
    Height = 42
    Caption = 'Synchronize'
    TabOrder = 2
    OnClick = btnSynchronizeClick
  end
  object btnQueue: TButton
    Left = 8
    Top = 150
    Width = 210
    Height = 42
    Caption = 'Queue'
    TabOrder = 3
    OnClick = btnQueueClick
  end
  object btnThQueueAndTImer: TButton
    Left = 8
    Top = 198
    Width = 210
    Height = 42
    Caption = 'TThreadedQueue + TTimer'
    TabOrder = 4
    OnClick = btnThQueueAndTImerClick
  end
  object btnAllocateHwnd: TButton
    Left = 8
    Top = 55
    Width = 210
    Height = 41
    Caption = 'Message + AllocateHwnd'
    TabOrder = 1
    OnClick = btnAllocateHwndClick
  end
  object btnObjInt: TButton
    Left = 8
    Top = 281
    Width = 210
    Height = 42
    Caption = 'object + interface'
    TabOrder = 6
    OnClick = btnObjIntClick
  end
  object TimerCheckQueue: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerCheckQueueTimer
    Left = 552
    Top = 24
  end
end
