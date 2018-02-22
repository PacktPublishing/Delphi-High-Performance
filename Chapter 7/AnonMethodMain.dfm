object frmAnonMthod: TfrmAnonMthod
  Left = 0
  Top = 0
  Caption = 'Anonymous methods'
  ClientHeight = 96
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    460
    96)
  PixelsPerInch = 96
  TextHeight = 13
  object btnAnonProblem: TButton
    Left = 16
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Anon problem'
    TabOrder = 0
    OnClick = btnAnonProblemClick
  end
  object ListBox1: TListBox
    Left = 120
    Top = 16
    Width = 323
    Height = 66
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnAnonFix: TButton
    Left = 16
    Top = 56
    Width = 89
    Height = 25
    Caption = 'Anon fix'
    TabOrder = 2
    OnClick = btnAnonFixClick
  end
end
