object frmParallelJoin: TfrmParallelJoin
  Left = 0
  Top = 0
  Caption = 'TParallel,Join'
  ClientHeight = 281
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    506
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object btnJoin2: TButton
    Left = 16
    Top = 16
    Width = 113
    Height = 25
    Caption = 'Join 2 tasks'
    TabOrder = 0
    OnClick = btnJoin2Click
  end
  object btnJoin3: TButton
    Left = 16
    Top = 47
    Width = 113
    Height = 25
    Caption = 'Join 3 tasks'
    TabOrder = 1
    OnClick = btnJoin3Click
  end
  object ListBox1: TListBox
    Left = 144
    Top = 16
    Width = 345
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object btnDHPJoin: TButton
    Left = 16
    Top = 135
    Width = 113
    Height = 25
    Caption = 'DHP Join'
    TabOrder = 3
    OnClick = btnDHPJoinClick
  end
  object btnDHPJoinExc: TButton
    Left = 16
    Top = 166
    Width = 113
    Height = 25
    Caption = 'DHP Join Exception'
    TabOrder = 4
    OnClick = btnDHPJoinExcClick
  end
  object btnJoin1p2: TButton
    Left = 16
    Top = 78
    Width = 113
    Height = 25
    Caption = 'Join 1+2 tasks'
    TabOrder = 2
    OnClick = btnJoin1p2Click
  end
end
