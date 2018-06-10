unit UGUIssing;

{$mode objfpc}{$H+}

interface

uses
  // Lazarus forms units
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,

  // Custom units from project
  UGuesser, UBinarySearch, USternBrocotSearch, UDummyGuesser;

type
  TGuessing = class(TForm)
    FracBtn, IntBtn: TButton;
    YesBtn, NoBtn: TButton;
    ExplanationLbl: TLabel;
    QuestionLbl: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure YesBtnClick(Sender: TObject);
    procedure NoBtnClick(Sender: TObject);
    procedure SetIntSearch(Sender: TObject);
    procedure SetFracSearch(Sender: TObject);
    procedure KeyIntercept(Sender: TObject; var Key: char);
    procedure AnsQn(reply: boolean);
    procedure AskQn;
  private
    Guesser: TGuesser;
  end;

var
  Guessing: TGuessing;

implementation

{$R *.lfm}

{ TGuessing }

procedure TGuessing.FormCreate(Sender: TObject);
begin
  writeln('Initialising game');
  ExplanationLbl.Font.Size := 30;
  Guesser := TDummyGuesser.Create('Welcome to the guessing game! Think of a number and select its domain.');
  AskQn;
end;

procedure TGuessing.SetIntSearch(Sender: TObject);
begin
  writeln('Entering unbounded integer binary search');
  Guesser.Free;
  Guesser := TBinarySearcher.Create;
  AskQn;
end;

procedure TGuessing.SetFracSearch(Sender: TObject);
begin
  writeln('Entering Stern-Brocot search');
  Guesser.Free;
  Guesser := TSternBrocotSearcher.Create;
  AskQn;
end;

procedure TGuessing.YesBtnClick(Sender: TObject);
begin
  writeln('Answer Yes');
  AnsQn(true);
end;

procedure TGuessing.NoBtnClick(Sender: TObject);
begin
  writeln('Answer No');
  AnsQn(False);
end;

procedure TGuessing.KeyIntercept(Sender: TObject; var Key: char);
begin
  writeln('Pressed key ', Key);
  case Key of
    'y':
      YesBtnClick(Sender);
    'n':
      NoBtnClick(Sender);
    'z':
      SetIntSearch(Sender);
    'q':
      SetFracSearch(Sender);
  end;
end;

procedure TGuessing.AnsQn(reply: boolean);
begin
  try
    Guesser.answer_question(reply);
  except
  on e: EGuessSuccessful do begin
    writeln('Guessed: ', e.Message);
    Guesser.Free;
    Guesser := TDummyGuesser.Create(e.Message + ' Think of another number, and select its domain.');
  end;
  end;
  AskQn;
end;

procedure TGuessing.AskQn;
begin
  QuestionLbl.Caption := Guesser.ask_question;
  writeln('Question: ', QuestionLbl.Caption);
end;

end.
