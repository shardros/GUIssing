{$MODE OBJFPC}

unit UBinarySearch;

interface

uses UGuesser, SysUtils;

const
  // Symbolic constant representing the starting upper bound. 64 chosen to
  // encompass most 2-digit numbers.
  STARTING_BOUND = 64;

type
  // Simple binary search, using the interval $Z & [lo, up)$.
  // These bounds are determined by continually multiplying up by 2 until the
  // user's number lies in the range.
  TBinarySearcher = class(TGuesser)
  protected
    // various flags representing current stage of guessing
    has_bounds, has_sign: boolean;
    // bounds, and current pivot
    lo, mid, up: integer;
  public
    constructor Create;
    function ask_question: string; override;
    procedure answer_question(reply: boolean); override;
  end;

implementation

constructor TBinarySearcher.Create;
begin
  has_bounds := False;
  has_sign := False;
  up := STARTING_BOUND;
  lo := 0;
end;

function TBinarySearcher.ask_question: string;
begin
  if not has_bounds then begin
    Result := Format('Is %d \le \abs{n} < %d?', [lo, up]);
  end else if not has_sign then
    Result := 'Is n \ge 0?'
  else begin
    mid := (lo + up) div 2;
    Result := Format('Is n \ge %d?', [mid]);
  end;
end;

procedure TBinarySearcher.answer_question(reply: boolean);
var
    tmp: integer;
begin
  if not has_bounds then
    if not reply then begin
      lo := up;
      up := up * 2;
    end else
      has_bounds := True
  else if not has_sign then begin
    if not reply then begin
      tmp := lo;
      // negate bounds, accounting for (non)?strictness
      lo := -up + 1;
      up := -tmp + 1;
    end;
    has_sign := True;
  end else begin
    if reply then
      lo := mid
    else
      up := mid;
    if up - lo <= 1 then
      raise EGuessSuccessful.Create(Format('Your number was %d.', [lo]));
  end;
end;

end.
