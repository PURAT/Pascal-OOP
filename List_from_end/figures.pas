unit figures;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics;

type
  tListOfFigures = class;

  tFigure = class
    name: string;
    x, y: integer;
    color: tColor;
    owningList: tListOfFigures;
    procedure show(); virtual; abstract;
    procedure hide(); virtual; abstract;
    function getInfo(): string; virtual;
    procedure moveTo(x, y: integer);
    constructor Create(x, y: integer; color: tColor; owningList: tListOfFigures);
    destructor Destroy(); override;
  end;

  tpNode = ^tNode;

  tNode = record
    figure: tFigure;
    pPrevious: tpNode;
  end;

  tListOfFigures = class
    pLastNode: tpNode;
    canvas: tCanvas;
    procedure add(figure:tFigure);
    procedure remove();
    constructor create(canvas: tCanvas);
    destructor destroy(); override;
  end;

  tDot = class(tFigure)
    procedure show(); override;
    procedure hide(); override;
  end;

  tLine = class(tFigure)
    x1, y1: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, x1, y1: integer; color: tColor; owningList: tListOfFigures);
  end;

  tCircle = class(tFigure)
    r: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, r: integer; color: tColor; owningList: tListOfFigures);
  end;

  tEllipse = class(tFigure)
    r, r2: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, r, r2: integer; color: tColor; owningList: tListOfFigures);
  end;

  tSquare = class(tFigure)
    a: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, a: integer; color: tColor; owningList: tListOfFigures);
  end;

  tRectangle = class(tFigure)
    a, b: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, a, b: integer; color: tColor; owningList: tListOfFigures);
  end;

implementation

  constructor tListOfFigures.create(canvas: tCanvas);
  begin
    inherited create();
    self.canvas:=canvas;
    pLastNode:=nil;
  end;

  procedure tListOfFigures.add(figure: tFigure);
  var pCurrent: tpNode;
  begin
    pCurrent:=new(tpNode);
    pCurrent^.figure:=figure;
    pCurrent^.pPrevious:=pLastNode;
    pLastNode:=pCurrent;
    figure.show();
  end;

  procedure tListOfFigures.remove();
  var pCurrent: tpNode;
  begin
    pCurrent:=pLastNode;
    pLastNode:=pCurrent^.pPrevious;
    pCurrent^.figure.destroy();
    dispose(pCurrent);
  end;

  destructor tListOfFigures.destroy();
  var pCurrent: tpNode;
  begin
    while pLastNode <> nil do
    begin
      pCurrent:=pLastNode;
      pCurrent^.figure.destroy();
      pLastNode:=pCurrent^.pPrevious;
      dispose(pCurrent);
    end;
    inherited;
  end;

  constructor tFigure.Create(x, y: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create();
    self.x:=x;
    self.y:=y;
    self.color:=color;
    name:=self.ClassName;
    self.owningList:=owningList;
    owningList.add(self);
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
    owningList.canvas.Pixels[x,y]:=color;
  end;

  procedure tDot.hide();
  begin
    owningList.canvas.Pixels[x,y]:=clNone;
  end;

  constructor tCircle.Create(x, y, r: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.r:=r;
  end;

  procedure tCircle.show();
  begin
    owningList.canvas.Pen.Color:=color;
    owningList.canvas.Ellipse(x-r,y-r,x+r,y+r);
  end;

  procedure tCircle.hide();
  begin
    owningList.canvas.Pen.Color:=clNone;
    owningList.canvas.Ellipse(x-r,y-r,x+r,y+r);
  end;

  constructor tEllipse.Create(x, y, r, r2: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.r:=r;
    self.r2:=r2;
  end;

  procedure tEllipse.show();
  begin
    owningList.canvas.Pen.Color:=color;
    owningList.canvas.Ellipse(x-r,y-r2,x+r,y+r2);
  end;

  procedure tEllipse.hide();
  begin
    owningList.canvas.Pen.Color:=clNone;
    owningList.canvas.Ellipse(x-r,y-r2,x+r,y+r2);
  end;

  constructor tLine.Create(x, y, x1, y1: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.x1:=x1;
    self.y1:=y1;
  end;

  procedure tLine.show();
  begin
    owningList.canvas.Pen.color:=color;
    owningList.canvas.Line(x,y,x1,y1);
  end;

  procedure tLine.hide();
  begin
    owningList.canvas.Pen.color:=clNone;
    owningList.canvas.Line(x,y,x1,y1);
  end;

  constructor tSquare.Create(x, y, a: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.a:=a;
  end;

  procedure tSquare.show();
  begin
    owningList.canvas.Pen.Color:=color;
    owningList.canvas.Rectangle(x-a,y-a,x+a,y+a);
  end;

  procedure tSquare.hide();
  begin
    owningList.canvas.Pen.Color:=clNone;
    owningList.canvas.Rectangle(x-a,y-a,x+a,y+a);
  end;

  constructor tRectangle.Create(x, y, a, b: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.a:=a;
    self.b:=b;
  end;

  procedure tRectangle.show();
  begin
    owningList.canvas.Pen.Color:=color;
    owningList.canvas.Rectangle(x-a,y-b,x+a,y+b);
  end;

  procedure tRectangle.hide();
  begin
    owningList.canvas.Pen.Color:=clNone;
    owningList.canvas.Rectangle(x-a,y-b,x+a,y+b);
  end;

end.

