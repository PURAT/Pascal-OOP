unit figures;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics;

type
  tFigure = class
    name: string;
    x, y: integer;
    canvas: tCanvas;
    color: tColor;
    procedure show(); virtual; abstract;
    procedure hide(); virtual; abstract;
    function getInfo(): string; virtual;
    procedure moveTo(x, y: integer);
    constructor Create(x, y: integer; canvas: tCanvas);
    destructor Destroy(); override;
  end;

  tDot = class(tFigure)
    procedure show(); override;
    procedure hide(); override;
  end;

  tLine = class(tFigure)
    x1, y1: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, x1, y1: integer; canvas: tCanvas);
  end;

  tCircle = class(tDot)
    r: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, r: integer; canvas: tCanvas);
  end;

  tEllipse = class(tCircle)
    r2: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, r, r2: integer; canvas: tCanvas);
  end;

  tSquare = class(tFigure)
    a: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, a: integer; canvas: tCanvas);
  end;

  tRectangle = class(tSquare)
    b: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, a, b: integer; canvas: tCanvas);
  end;

implementation

  constructor tFigure.Create(x, y: integer; canvas: tCanvas);
  begin
    inherited Create();
    self.canvas:=canvas;
    self.x:=x;
    self.y:=y;
    name:=self.ClassName;
  end;

  procedure tFigure.moveTo(x, y: integer);
  begin
    hide();
    self.x:=x;
    self.y:=y;
    show();
  end;

  function tFigure.getInfo(): string;
  begin
    result:='тип: ' + name + #10#13 + 'x= ' + IntToStr(x) + ', y= ' + IntToStr(y);
  end;

  destructor tFigure.Destroy();
  begin
    hide();
    inherited Destroy();
  end;

  procedure tDot.show();
  begin
    canvas.Pixels[x,y]:=clBlack;
  end;

  procedure tDot.hide();
  begin
    canvas.Pixels[x,y]:=clNone;
  end;

  constructor tCircle.Create(x, y, r: integer; canvas: tCanvas);
  begin
    inherited Create(x, y, canvas);
    self.r:=r;
  end;

  procedure tCircle.show();
  begin
    canvas.Pen.Color:=clBlack;
    canvas.Ellipse(x-r,y-r,x+r,y+r);
  end;

  procedure tCircle.hide();
  begin
    canvas.Pen.Color:=clNone;
    canvas.Ellipse(x-r,y-r,x+r,y+r);
  end;

  constructor tEllipse.Create(x, y, r, r2: integer; canvas: tCanvas);
  begin
    inherited Create(x, y, r, canvas);
    self.r2:=r2;
  end;

  procedure tEllipse.show();
  begin
    canvas.Pen.Color:=clBlack;
    canvas.Ellipse(x-r,y-r2,x+r,y+r2);
  end;

  procedure tEllipse.hide();
  begin
    canvas.Pen.Color:=clNone;
    canvas.Ellipse(x-r,y-r2,x+r,y+r2);
  end;

  constructor tLine.Create(x, y, x1, y1: integer; canvas: tCanvas);
  begin
    inherited Create(x, y, canvas);
    self.x1:=x1;
    self.y1:=y1;
  end;

  procedure tLine.show();
  begin
    canvas.Pen.color:=clBlack;
    canvas.Line(x,y,x1,y1);
  end;

  procedure tLine.hide();
  begin
    canvas.Pen.color:=clNone;
    canvas.Line(x,y,x1,y1);
  end;

  constructor tSquare.Create(x, y, a: integer; canvas: tCanvas);
  begin
    inherited Create(x, y, canvas);
    self.a:=a;
  end;

  procedure tSquare.show();
  begin
    canvas.Pen.Color:=clBlack;
    canvas.Rectangle(x-a,y-a,x+a,y+a);
  end;

  procedure tSquare.hide();
  begin
    canvas.Pen.Color:=clNone;
    canvas.Rectangle(x-a,y-a,x+a,y+a);
  end;

  constructor tRectangle.Create(x, y, a, b: integer; canvas: tCanvas);
  begin
    inherited Create(x, y, a, canvas);
    self.b:=b;
  end;

  procedure tRectangle.show();
  begin
    canvas.Pen.Color:=clBlack;
    canvas.Rectangle(x-a,y-b,x+a,y+b);
  end;

  procedure tRectangle.hide();
  begin
    canvas.Pen.Color:=clNone;
    canvas.Rectangle(x-a,y-b,x+a,y+b);
  end;

end.

