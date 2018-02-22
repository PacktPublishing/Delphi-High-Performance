unit RandomWordSearch1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Samples.Spin;

type
  TWordCheckDelegate = reference to function (const word: string): boolean;

  TfrmRandomWordSearch = class(TForm)
    btnUnsortedList: TButton;
    lbWords: TListBox;
    btnSortedList: TButton;
    btnDictionary: TButton;
    lblWordLength: TLabel;
    inpWordLength: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnUnsortedListClick(Sender: TObject);
    procedure btnSortedListClick(Sender: TObject);
    procedure btnDictionaryClick(Sender: TObject);
  private
    FWordsUnsorted: TStringList;
    FWordsSorted: TStringList;
    FWordsDictionary: TDictionary<string,boolean>;
  protected
    function  GenerateWord: string;
    procedure FindGoodWord(const wordTest: TWordCheckDelegate);
    procedure LoadWords(wordList: TStringList);
  public
  end;

var
  frmRandomWordSearch: TfrmRandomWordSearch;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

const
  CWordsFile = 'words_alpha.txt';

procedure TfrmRandomWordSearch.btnDictionaryClick(Sender: TObject);
begin
  FindGoodWord(
    function (const word: string): boolean
    begin
      Result := FWordsDictionary.ContainsKey(word);
    end);
end;

procedure TfrmRandomWordSearch.btnSortedListClick(Sender: TObject);
begin
  FindGoodWord(
    function (const word: string): boolean
    begin
      Result := FWordsSorted.IndexOf(word) >= 0;
    end);
end;

procedure TfrmRandomWordSearch.btnUnsortedListClick(Sender: TObject);
begin
  FindGoodWord(
    function (const word: string): boolean
    begin
      Result := FWordsUnsorted.IndexOf(word) >= 0;
    end);
end;

procedure TfrmRandomWordSearch.FindGoodWord(const wordTest: TWordCheckDelegate);
var
  word: string;
  isWordOK: boolean;
  time: TStopwatch;
begin
  time := TStopwatch.StartNew;
  repeat
    word := GenerateWord;
    isWordOK := wordTest(word);
  until isWordOK or (time.ElapsedMilliseconds > 10000);
  if isWordOK then
    lbWords.ItemIndex := lbWords.Items.Add(Format('%s (%d ms)', [word, time.ElapsedMilliseconds]))
  else
    lbWords.ItemIndex := lbWords.Items.Add('timeout');
end;

procedure TfrmRandomWordSearch.FormCreate(Sender: TObject);
var
  wordsFile: string;
  wordList: TStringList;
begin
  if FileExists(CWordsFile) then
    wordsFile := CWordsFile
  else if FileExists('..\..\' + CWordsFile) then
    wordsFile := '..\..\' + CWordsFile
  else
    wordsFile := '';

  if wordsFile = '' then
    ShowMessage('File ' + CWordsFile + ' not found!'#13#10#13#10'Copy it to the EXE folder.')
  else
  begin
    wordList := TStringList.Create;
    try
      wordList.LoadFromFile(wordsFile);
      LoadWords(wordList);
      lbWords.Items.Add(Format('%d words loaded', [wordList.Count]));
    except
      on E: Exception do begin
        ShowMessage('Failed to load file ' + CWordsFile + '. ' + E.Message);
      end;
    end;
  end;
end;

procedure TfrmRandomWordSearch.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWordsUnsorted);
  FreeAndNil(FWordsSorted);
  FreeAndNil(FWordsDictionary);
end;

function TfrmRandomWordSearch.GenerateWord: string;
var
  pos: integer;
begin
  Result := '';
  for pos := 1 to inpWordLength.Value do
    Result := Result + Chr(Ord('a') + Random(Ord('z') - Ord('a') + 1));
end;

procedure TfrmRandomWordSearch.LoadWords(wordList: TStringList);
var
  word: string;
begin
  // Load all words from database of English words into internal data structures.
  // Source: https://github.com/dwyl/english-words.
  // Database copyright: infochimps

  FWordsUnsorted := TStringList.Create;
  FWordsUnsorted.Assign(wordList);

  FWordsSorted := TStringList.Create;
  FWordsSorted.Assign(wordList);
  FWordsSorted.Sorted := True;

  FWordsDictionary := TDictionary<string,boolean>.Create(wordList.Count);
  for word in wordList do
    FWordsDictionary.Add(word, True);
end;

end.
