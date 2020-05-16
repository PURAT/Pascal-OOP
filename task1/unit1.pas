unit Unit1;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, FileUtil, Forms, Controls, Graphics;

type
tFigure =
class
  x,y:Integer;
  isVisible:Boolean;
  color:tColor;
  canvas:tCanvas;
  procedure show();
  procedure hide();
  procedure moveTo(newX, newY:Integer);
  constructor create(initialX, initialY:Integer);
end;

implementation

{$R *.lfm}

constructor tFigure.create(initialX, initialY:Integer);
begin
x:=initialX;
y:=initialY;
end;

procedure tFigure.moveTo(newX, newY:Integer);
begin
hide();
x:=newX;
y:=newY;
show();
end;

procedure tFigure.show();
begin

end;

procedure tFigure.hide();
begin

end;

end.
