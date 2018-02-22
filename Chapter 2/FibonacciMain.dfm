object frmFibonacci: TfrmFibonacci
  Left = 0
  Top = 0
  Caption = 'Fibonacci'
  ClientHeight = 435
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    680
    435)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 81
    Height = 13
    Caption = 'Element number:'
  end
  object SpinEdit1: TSpinEdit
    Left = 111
    Top = 21
    Width = 73
    Height = 22
    MaxValue = 93
    MinValue = 1
    TabOrder = 0
    Value = 10
  end
  object Button1: TButton
    Left = 208
    Top = 19
    Width = 145
    Height = 25
    Caption = 'Recursive'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 359
    Top = 19
    Width = 145
    Height = 25
    Caption = 'Recursive + memoization'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 510
    Top = 19
    Width = 145
    Height = 25
    Caption = 'Iterative'
    TabOrder = 3
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 24
    Top = 56
    Width = 631
    Height = 361
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
  end
end
