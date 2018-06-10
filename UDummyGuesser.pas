{$MODE OBJFPC}

unit UDummyGuesser;

interface

uses UGuesser, SysUtils;

const
  STARTING_BOUND = 64;

type
  TDummyGuesser = class(TGuesser)
  protected
    msg: string;
  public
    constructor Create(msg_: string);
    function ask_question: string; override;
    procedure answer_question(reply: boolean); override;
  end;

implementation

constructor TDummyGuesser.Create(msg_: string);
begin
    msg := msg_;
end;

function TDummyGuesser.ask_question: string;
begin
    result := msg;
end;

procedure TDummyGuesser.answer_question(reply: boolean);
begin
end;

end.
