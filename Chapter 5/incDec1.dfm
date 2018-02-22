object frmIncDec: TfrmIncDec
  Left = 0
  Top = 0
  Caption = 'Inc/Dec'
  ClientHeight = 344
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 19
  object btnSingleThreaded: TButton
    Left = 8
    Top = 8
    Width = 201
    Height = 41
    Caption = 'Single threaded'
    TabOrder = 0
    OnClick = btnSingleThreadedClick
  end
  object btnMultithreaded: TButton
    Left = 8
    Top = 55
    Width = 201
    Height = 42
    Caption = 'Multithreaded'
    TabOrder = 1
    OnClick = btnMultithreadedClick
  end
  object ListBox1: TListBox
    Left = 215
    Top = 0
    Width = 312
    Height = 344
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 19
    TabOrder = 7
  end
  object btnLocking: TButton
    Left = 8
    Top = 103
    Width = 201
    Height = 42
    Caption = 'MT with locking'
    TabOrder = 2
    OnClick = btnLockingClick
  end
  object btnInterlocked: TButton
    Left = 8
    Top = 295
    Width = 201
    Height = 42
    Caption = 'MT with interlocked'
    TabOrder = 5
    OnClick = btnInterlockedClick
  end
  object btnTMonitor: TButton
    Left = 8
    Top = 199
    Width = 201
    Height = 42
    Caption = 'MT with TMonitor'
    TabOrder = 4
    OnClick = btnTMonitorClick
  end
  object btnMutex: TButton
    Left = 8
    Top = 151
    Width = 201
    Height = 42
    Caption = 'MT with mutex'
    TabOrder = 3
    OnClick = btnMutexClick
  end
  object btnSpinlock: TButton
    Left = 8
    Top = 247
    Width = 201
    Height = 42
    Caption = 'MT with spinlock'
    TabOrder = 6
    OnClick = btnSpinlockClick
  end
end
