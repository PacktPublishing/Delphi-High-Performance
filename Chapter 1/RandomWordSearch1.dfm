object frmRandomWordSearch: TfrmRandomWordSearch
  Left = 0
  Top = 0
  Caption = 'Random Word Search'
  ClientHeight = 249
  ClientWidth = 298
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
  object lblWordLength: TLabel
    Left = 16
    Top = 192
    Width = 63
    Height = 13
    Caption = 'Word length:'
  end
  object btnUnsortedList: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Unsorted list'
    TabOrder = 0
    OnClick = btnUnsortedListClick
  end
  object lbWords: TListBox
    Left = 104
    Top = 16
    Width = 177
    Height = 217
    ItemHeight = 13
    TabOrder = 1
  end
  object btnSortedList: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Sorted list'
    TabOrder = 2
    OnClick = btnSortedListClick
  end
  object btnDictionary: TButton
    Left = 16
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Dictionary'
    TabOrder = 3
    OnClick = btnDictionaryClick
  end
  object inpWordLength: TSpinEdit
    Left = 16
    Top = 211
    Width = 75
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 4
  end
end
