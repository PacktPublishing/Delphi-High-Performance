object frmThreadComm: TfrmThreadComm
  Left = 0
  Top = 0
  Caption = 'ThreadComm'
  ClientHeight = 305
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    641
    305)
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 16
    Top = 16
    Width = 137
    Height = 33
    Caption = 'Start thread'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object ListBox1: TListBox
    Left = 168
    Top = 16
    Width = 459
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 8
  end
  object inpPing: TSpinEdit
    Left = 33
    Top = 55
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 17
  end
  object btnChangePing: TButton
    Left = 96
    Top = 55
    Width = 57
    Height = 22
    Caption = 'Change'
    Enabled = False
    TabOrder = 2
    OnClick = btnChangePingClick
  end
  object btnStop: TButton
    Left = 33
    Top = 83
    Width = 120
    Height = 33
    Caption = 'Stop thread'
    Enabled = False
    TabOrder = 3
    OnClick = btnStopClick
  end
  object btnStartTimer: TButton
    Left = 16
    Top = 144
    Width = 137
    Height = 33
    Caption = 'Start thread'
    TabOrder = 4
    OnClick = btnStartTimerClick
  end
  object btnChangePingTimer: TButton
    Left = 96
    Top = 183
    Width = 57
    Height = 22
    Caption = 'Change'
    Enabled = False
    TabOrder = 6
    OnClick = btnChangePingTimerClick
  end
  object btnStopTimer: TButton
    Left = 33
    Top = 211
    Width = 120
    Height = 33
    Caption = 'Stop thread'
    Enabled = False
    TabOrder = 7
    OnClick = btnStopTimerClick
  end
  object inpPingTimer: TSpinEdit
    Left = 33
    Top = 183
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 17
  end
end
