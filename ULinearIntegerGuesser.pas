{$MODE OBJFPC}

unit ULinearIntegerGuesser;

interface

uses UGuesser;

type
    TLinearGuesser = class(TInterfacedObject, IGuesser)
    private
        curr_guess: integer;
        incr_val: integer;
    public
        function NextGuess(prev_answer): string;
    end;
