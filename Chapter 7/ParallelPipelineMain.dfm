object frmPipeline: TfrmPipeline
  Left = 0
  Top = 0
  Caption = 'Pipeline'
  ClientHeight = 306
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    523
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 59
    Height = 13
    Caption = 'Start folder:'
  end
  object lblProcessing: TLabel
    Left = 81
    Top = 38
    Width = 61
    Height = 13
    Caption = 'lblProcessing'
    Visible = False
  end
  object inpStartFolder: TEdit
    Left = 81
    Top = 16
    Width = 344
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnSelectFolder: TButton
    Left = 431
    Top = 14
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Select'
    TabOrder = 1
    OnClick = btnSelectFolderClick
  end
  object btnStart: TButton
    Left = 193
    Top = 56
    Width = 137
    Height = 41
    Anchors = [akTop]
    Caption = 'Start'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = btnStartClick
  end
  object ListBox1: TListBox
    Left = 16
    Top = 112
    Width = 490
    Height = 179
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist]
    Title = 'Select folder'
    Left = 40
    Top = 64
  end
  object TimerUpdateProcessing: TTimer
    Enabled = False
    Interval = 250
    OnTimer = TimerUpdateProcessingTimer
    Left = 120
    Top = 64
  end
end
