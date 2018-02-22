object frmDeadlock: TfrmDeadlock
  Left = 0
  Top = 0
  Caption = 'frmDeadlock'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnTask1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Task1'
    TabOrder = 0
    OnClick = btnTask1Click
  end
  object btnTask2: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Task2'
    TabOrder = 1
    OnClick = btnTask2Click
  end
  object btnTryTask1: TButton
    Left = 16
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Try Task1'
    TabOrder = 2
    OnClick = btnTryTask1Click
  end
  object btnTryTask2: TButton
    Left = 16
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Try Task2'
    TabOrder = 3
    OnClick = btnTryTask2Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 312
    Top = 152
  end
end
