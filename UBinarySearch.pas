{$MODE OBJFPC}

unit UBinarySearch;

interface

uses UGuesser;

const
    STARTING_BOUND = 64;

type
    TBinarySearcher = class(TInterfacedObject, IGuesser)
    protected
        has_bounds, has_sign, is_negative: bool;
        up, lo, mid: integer;
    public
        constructor Create;
        function ask_question: string;
        procedure answer_question(reply: bool);
    end;

implementation

constructor TBinarySearcher.Create;
begin
    has_bounds := False;
    has_sign := False;
    is_negative := False;
    up := STARTING_BOUND;
    lo := 0;
end;

function TBinarySearcher.ask_question: string;
begin
    if not has_sign then
        result := 'Think of an integer n. Is n < 0?'
    else if not has_bounds then begin
        if is_negative then
            lo := lo * 2
        else
            hi := hi * 2;
        result := Format('Is %d <= n < %d', [lo, hi]);
    end else begin
        mid := (lo + up) div 2;
        result := Format('Is n >= %d', [mid]);
    end;
end.

procedure TBinarySearcher.answer_question(reply: bool);
begin
    if not has_sign then begin
        is_negative := reply;
        has_sign := True;
    end else if not has_bounds then
        has_bounds := reply
    else begin
        if reply then begin
            lo := mid
        else
            hi := mid;
        if hi - lo <= 1 then
            raise EGuessSuccessful(Format('Your number was %d', [lo]));
    end end;
end;
