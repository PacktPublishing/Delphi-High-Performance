object frmCppClassDemo: TfrmCppClassDemo
  Left = 0
  Top = 0
  Caption = 'CppClass demo'
  ClientHeight = 311
  ClientWidth = 648
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    648
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object SpinEdit1: TSpinEdit
    Left = 72
    Top = 21
    Width = 73
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object btnWrap: TButton
    Left = 24
    Top = 56
    Width = 121
    Height = 25
    Caption = 'Use class wrapper'
    TabOrder = 1
    OnClick = btnWrapClick
  end
  object ListBox1: TListBox
    Left = 160
    Top = 21
    Width = 467
    Height = 270
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
