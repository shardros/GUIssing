{$MODE OBJFPC}

unit UGuesser;

interface

uses SysUtils;

type
  // base classes
  TGuesser = class abstract
    function ask_question: string; virtual; abstract;
    procedure answer_question(reply: boolean); virtual; abstract;
  end;
  EGuessSuccessful = class(Exception);

implementation

end.
