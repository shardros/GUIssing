unit UComputerGuessing;

{$mode objfpc}{$H+}

interface

uses
  // Lazarus forms units
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  // Other form
  UUserGuessing,
  // Custom units from project
  UGuesser, UBinarySearch, USternBrocotSearch, UDummyGuesser;

type

  { TComputerGuessing }
  // Main form - where the computer guesses the user's number
  TComputerGuessing = class(TForm)
    SwitchBtn: TButton;
    FracBtn, IntBtn: TButton;
    YesBtn, NoBtn: TButton;
    ExplanationLbl: TLabel;
    QuestionLbl: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SwitchBtnClick(Sender: TObject);
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
  ComputerGuessing: TComputerGuessing;

implementation

{$R *.lfm}

{ TComputerGuessing }

procedure TComputerGuessing.FormCreate(Sender: TObject);
begin
  writeln('Initialising game');
  ExplanationLbl.Font.Size := 30;
  Guesser := TDummyGuesser.Create('Welcome to the guessing game! Think of a number and select its domain.');
  AskQn;
end;

procedure TComputerGuessing.SwitchBtnClick(Sender: TObject);
begin
  writeln('Switching to user guessing mode');
  UserGuessing.Show;
end;

procedure TComputerGuessing.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  writeln('Closing application');
  Guesser.Free;
end;

procedure TComputerGuessing.SetIntSearch(Sender: TObject);
begin
  writeln('Entering unbounded integer binary search');
  Guesser.Free;
  Guesser := TBinarySearcher.Create;
  AskQn;
end;

procedure TComputerGuessing.SetFracSearch(Sender: TObject);
begin
  writeln('Entering Stern-Brocot search');
  Guesser.Free;
  Guesser := TSternBrocotSearcher.Create;
  AskQn;
end;

procedure TComputerGuessing.YesBtnClick(Sender: TObject);
begin
  writeln('Answer Yes');
  AnsQn(true);
end;

procedure TComputerGuessing.NoBtnClick(Sender: TObject);
begin
  writeln('Answer No');
  AnsQn(False);
end;

procedure TComputerGuessing.KeyIntercept(Sender: TObject; var Key: char);
begin
  write(Format('Pressed key %s ', [Key]));
  case Key of
    'y':
      YesBtnClick(Sender);
    'n':
      NoBtnClick(Sender);
    'z':
      SetIntSearch(Sender);
    'q':
      SetFracSearch(Sender);
    's':
      SwitchBtnClick(Sender);
  end;
end;

procedure TComputerGuessing.AnsQn(reply: boolean);
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

procedure TComputerGuessing.AskQn;
begin
  QuestionLbl.Caption := Guesser.ask_question;
  writeln('Question: ', QuestionLbl.Caption);
end;

end.
