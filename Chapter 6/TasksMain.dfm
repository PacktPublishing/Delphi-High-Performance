object frmTasks: TfrmTasks
  Left = 0
  Top = 0
  Caption = 'Tasks'
  ClientHeight = 305
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    643
    305)
  PixelsPerInch = 96
  TextHeight = 13
  object btnTask: TButton
    Left = 17
    Top = 16
    Width = 137
    Height = 33
    Caption = 'ITask'
    TabOrder = 0
    OnClick = btnTaskClick
  end
  object ListBox1: TListBox
    Left = 168
    Top = 16
    Width = 459
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnTwoTasks: TButton
    Left = 17
    Top = 64
    Width = 137
    Height = 33
    Caption = 'Two tasks'
    TabOrder = 2
    OnClick = btnTwoTasksClick
  end
end
