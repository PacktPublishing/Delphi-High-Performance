object frmInitFin: TfrmInitFin
  Left = 0
  Top = 0
  Caption = 'Initialize/Finalize'
  ClientHeight = 409
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    657
    409)
  PixelsPerInch = 96
  TextHeight = 13
  object btnNewDisp: TButton
    Left = 24
    Top = 24
    Width = 185
    Height = 25
    Caption = 'New'
    TabOrder = 0
    OnClick = btnNewDispClick
  end
  object btnInitFin: TButton
    Left = 24
    Top = 64
    Width = 185
    Height = 25
    Caption = 'GetMem + Initialize'
    TabOrder = 1
    OnClick = btnInitFinClick
  end
  object ListBox1: TListBox
    Left = 232
    Top = 24
    Width = 401
    Height = 361
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnGetMem: TButton
    Left = 24
    Top = 104
    Width = 185
    Height = 25
    Caption = 'GetMem'
    TabOrder = 3
    OnClick = btnGetMemClick
  end
end
