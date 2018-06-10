{$MODE OBJFPC}

unit USternBrocotSearch;

interface

uses UGuesser, SysUtils;

type
  // class to descend a Stern-Brocot tree in search of a rational, with added
  // logic to deal with negative rationals.
  // Stern-Brocot searching works by taking the mediant of upper and lower
  // bounds. The median is guaranteed to be strictly between a and b, but also
  // preserves the property of "lower complexity" - ie the denominator grows
  // additively rather than multiplicatively. By setting the upper bounds as
  // 1/0, we can easily search the entirety of Q+, and need only determine sign.
  TSternBrocotSearcher = class(TGuesser)
  protected
    // flags to determine current stage of guessing
    has_sign, is_nonzero, is_mid: boolean;
    // numerator and denominator for each fraction
    lo_n, lo_d, mid_n, mid_d, up_n, up_d: integer;
  public
    // user interfacing methods
    constructor Create;
    function ask_question: string; override;
    procedure answer_question(reply: boolean); override;
  end;

implementation

constructor TSternBrocotSearcher.Create;
begin
  has_sign := False;
  is_nonzero := False;
  is_mid := True;
  up_n := 1; up_d := 0;
  lo_n := 0; lo_d := 1;
end;

function TSternBrocotSearcher.ask_question: string;
begin
  if not is_nonzero then
    result := 'Is q = 0?'
  else if not has_sign then
    result := 'Is q \ge 0?'
  else if is_mid then begin
    mid_n := lo_n + up_n;
    mid_d := lo_d + up_d;
    result := Format('Is q = \frac{%d}{%d}?', [mid_n, mid_d]);
  end else
    result := Format('Is q > \frac{%d}{%d}?', [mid_n, mid_d]);
end;

procedure TSternBrocotSearcher.answer_question(reply: boolean);
begin
  if not is_nonzero then
    if reply then
      raise EGuessSuccessful.Create('Your number was 0')
    else
      is_nonzero := True
  else if not has_sign then begin
    // for negative q, set lower bound to -1/0 (acting as -inf)
    if not reply then begin
      lo_n := -1; lo_d := 0;
      up_n :=  0; up_d := 1;
    end;
    has_sign := True;
  end else if is_mid then
    if reply then
      raise EGuessSuccessful.Create(Format('Your number was \frac{%d}{%d}', [mid_n, mid_d]))
    else
      is_mid := False
  else begin
    if reply then begin
      lo_n := mid_n; lo_d := mid_d;
    end else begin
      up_n := mid_n; up_d := mid_d;
    end;
    is_mid := True;
  end;
end;

end.
