object frmCompilerOptions: TfrmCompilerOptions
  Left = 0
  Top = 0
  Caption = 'Compiler options'
  ClientHeight = 438
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnRangeChecking: TButton
    Left = 24
    Top = 63
    Width = 105
    Height = 25
    Caption = 'Range checking'
    TabOrder = 0
    OnClick = btnRangeCheckingClick
  end
  object ListBox1: TListBox
    Left = 152
    Top = 24
    Width = 553
    Height = 393
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOptimization: TButton
    Left = 24
    Top = 24
    Width = 105
    Height = 25
    Caption = 'Optimization'
    TabOrder = 2
    OnClick = btnOptimizationClick
  end
  object btnRangeError: TButton
    Left = 24
    Top = 264
    Width = 105
    Height = 25
    Caption = 'Range error'
    TabOrder = 3
    OnClick = btnRangeErrorClick
  end
  object btnOverflowChecking: TButton
    Left = 24
    Top = 104
    Width = 105
    Height = 25
    Caption = 'Overflow checking'
    TabOrder = 4
    OnClick = btnOverflowCheckingClick
  end
  object btnOverflowError: TButton
    Left = 24
    Top = 304
    Width = 105
    Height = 25
    Caption = 'Overflow error'
    TabOrder = 5
    OnClick = btnOverflowErrorClick
  end
  object btnAlignment: TButton
    Left = 24
    Top = 144
    Width = 105
    Height = 25
    Caption = 'Record field align'
    TabOrder = 6
    OnClick = btnAlignmentClick
  end
  object Button1: TButton
    Left = 54
    Top = 175
    Width = 75
    Height = 25
    Caption = 'SizeOf()'
    TabOrder = 7
    OnClick = Button1Click
  end
end
