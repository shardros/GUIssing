unit UGUIssing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TGuessing }

  TGuessing = class(TForm)
    DrawBtn: TButton;
    procedure DrawBtnClick(Sender: TObject);
    procedure InitStick(Sender: TObject);
  private
    stickX: integer;
  public

  end;

var
  Guessing: TGuessing;

implementation

{$R *.lfm}

{ TGuessing }

procedure TGuessing.DrawBtnClick(Sender: TObject);
begin
  with canvas do begin
    Brush.color:= clred;
    Ellipse(stickX-5, 117, 205, 128);
    Brush.color:= clblue;
    Rectangle (stickX-8, 130,208,160);
    Brush.color:= clgreen;
    Rectangle (stickX-13, 130,191,162);
    Brush.color:= clyellow;
    Rectangle (stickX+9, 130,213,162);
    Brush.color:= clmaroon;
    Rectangle (stickX-7,161,199,200);
    Brush.color:= clpurple;
    Rectangle (stickX+1,161,207,200);
  end;
  stickX := stickX + 10;
end;

procedure TGuessing.InitGuessing(Sender: TObject);
begin

end;

end.

