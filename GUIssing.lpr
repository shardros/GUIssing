program GUIssing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UComputerGuessing, UUserGuessing
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TComputerGuessing, ComputerGuessing);
  Application.CreateForm(TUserGuessing, UserGuessing);
  Application.Run;
end.

