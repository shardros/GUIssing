{$MODE OBJFPC}

unit UGuesser;

interface

type
    IGuesser = interface
        constructor Create;
        function ask_question: string;
        procedure answer_question(reply: bool);
    end;

    EGuessSuccessful = class(Exception);

implementation

end.
