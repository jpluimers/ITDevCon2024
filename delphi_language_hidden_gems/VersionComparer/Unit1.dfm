object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Easy Version Compare'
  ClientHeight = 229
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblVersionA: TLabel
    Left = 24
    Top = 13
    Width = 49
    Height = 13
    Caption = 'Version &A:'
    FocusControl = cbxVersionA
  end
  object lblVersionB: TLabel
    Left = 190
    Top = 13
    Width = 48
    Height = 13
    Caption = 'Version &B:'
    FocusControl = cbxVersionB
  end
  object lblOperator: TLabel
    Left = 119
    Top = 13
    Width = 48
    Height = 13
    Caption = '&Operator:'
    FocusControl = cbxOperator
  end
  object lblAuthorInfo: TLabel
    Left = 26
    Top = 201
    Width = 253
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Easy Version Compare Demo by Codehunter'
  end
  object cbxVersionA: TComboBox
    Left = 24
    Top = 32
    Width = 89
    Height = 21
    ItemIndex = 7
    TabOrder = 0
    Text = '1.0'
    Items.Strings = (
      '0'
      '0.1'
      '0.0.1'
      '0.0.0.1'
      '0.0.1.1'
      '0.1.1.1'
      '1'
      '1.0'
      '1.0.0'
      '1.0.0.0'
      '1.0.0.1'
      '1.0.1.1'
      '1.1.1.1')
  end
  object cbxVersionB: TComboBox
    Left = 190
    Top = 32
    Width = 89
    Height = 21
    TabOrder = 1
    Text = '0.1'
    Items.Strings = (
      '0'
      '0.1'
      '0.0.1'
      '0.0.0.1'
      '0.0.1.1'
      '0.1.1.1'
      '1'
      '1.0'
      '1.0.0'
      '1.0.0.0'
      '1.0.0.1'
      '1.0.1.1'
      '1.1.1.1')
  end
  object cbxOperator: TComboBox
    Left = 119
    Top = 32
    Width = 65
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 2
    Text = '='
    Items.Strings = (
      '='
      '>'
      '>='
      '<'
      '<='
      '<>')
  end
  object btnCompare: TButton
    Left = 24
    Top = 59
    Width = 255
    Height = 25
    Caption = 'Compare'
    TabOrder = 3
    OnClick = btnCompareClick
  end
  object grpResult: TGroupBox
    Left = 24
    Top = 90
    Width = 255
    Height = 105
    Caption = 'Result'
    TabOrder = 4
    object lblResult: TPanel
      Left = 2
      Top = 15
      Width = 251
      Height = 88
      Align = alClient
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -37
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = lblResultClick
    end
  end
end
