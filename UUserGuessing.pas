unit UUserGuessing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaskEdit, LCLType;

type

  { TUserGuessing }
  // Auxiliary form letting a user guess the computer's number
  TUserGuessing = class(TForm)
    GuessEdt: TEdit;
    LoEdt: TEdit;
    SwitchBtn: TButton;
    GenerateBtn: TButton;
    AnswerLbl: TLabel;
    GuessLbl: TLabel;
    LoLbl: TLabel;
    UpEdt: TEdit;
    UpLbl: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure GuessEdtEditingDone(Sender: TObject);
    procedure KeyIntercept(Sender: TObject; var Key: char);
    procedure SwitchBtnClick(Sender: TObject);
  private
    Num: integer;
  end;

var
  UserGuessing: TUserGuessing;

implementation

{$R *.lfm}

{ TUserGuessing }

procedure TUserGuessing.FormCreate(Sender: TObject);
begin
  writeln('Initialising user-guessing game');
  randomize;
  GenerateBtnClick(Sender);
end;

procedure TUserGuessing.GenerateBtnClick(Sender: TObject);
var
  lo, up: integer;
  success: boolean = True;
begin
  success := success and TryStrToInt(LoEdt.Text, lo);
  success := success and TryStrToInt(UpEdt.Text, up);
  if success then begin
    Num := random(up - lo) + lo;
    writeln(Format('Generated %d between %d, %d', [Num, lo, up]));
    AnswerLbl.Caption := Format('I''m thinking of a number between %d and %d', [lo, up]);
  end else
    AnswerLbl.Caption := 'Invalid boundary';
end;

procedure TUserGuessing.GuessEdtEditingDone(Sender: TObject);
var
  guess: integer;
begin
  if TryStrToInt(GuessEdt.Text, guess) then begin
    if guess = Num then
      AnswerLbl.Caption := 'Correct! You guessed it'
    else if guess < Num then
      AnswerLbl.Caption := 'Too low!'
    else
      AnswerLbl.Caption := 'Too high!';
  end else
    AnswerLbl.Caption := 'Invalid number';
end;

procedure TUserGuessing.KeyIntercept(Sender: TObject; var Key: char);
begin
  write(Format('Pressed key %s ', [Key]));
  case Key of
    'g':
      GenerateBtnClick(Sender);
    'b':
      SwitchBtnClick(Sender);
  end;
end;

procedure TUserGuessing.SwitchBtnClick(Sender: TObject);
begin
  writeln('Closing user-guessing game');
  Close;
end;

end.
