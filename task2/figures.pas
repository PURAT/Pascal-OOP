unit figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics;

type
  tFigure = class
    x, y: integer;
    isVisible: boolean;
    color: tColor;
    canvas: tCanvas;
    procedure show();
    procedure hide();
    procedure moveTo(newX, newY: integer);
    constructor Create(initialX, initialY: integer);
    destructor Destroy(); override;
  end;

implementation

  uses Unit1;

  constructor tFigure.Create(initialX, initialY: integer);
  begin
    inherited Create();
    x := initialX;
    y := initialY;
    Form1.label1.Caption := 'Фигура была создана';
  end;

  procedure tFigure.moveTo(newX, newY: integer);
  begin
    hide();
    x := newX;
    y := newY;
    show();
  end;

  procedure tFigure.show();
  begin
    Form1.label1.caption := self.ClassName + #10#13 + ' x=' + IntToStr(x) + ' y=' + IntToStr(y);
  end;

  procedure tFigure.hide();
  begin
    Form1.label1.Caption := 'Фигура скрыта';
  end;

  destructor tFigure.Destroy();
  begin
    Form1.label1.Caption := 'Фигура уничтожена';
    inherited Destroy();
  end;

end.

