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
    pNext: tpNode;
  end;

  tListOfFigures = class
    pFirstNode: tpNode;
    pLastNode: tpNode;
    canvas: tCanvas;
    procedure add(figure:tFigure);
    procedure remove();
    function report(): string;
    procedure activateFigure(name: string);
    procedure redraw();
    constructor create(canvas: tCanvas);
    destructor destroy(); override;
  end;

  tDot = class(tFigure)
    index: integer; static;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y: integer; color: tColor; owningList: tListOfFigures);
  end;

  tLine = class(tFigure)
    index: integer; static;
    x1, y1: integer;
    procedure show(); override;
    procedure hide(); override;
    function getInfo(): string; override;
    constructor Create(x, y, x1, y1: integer; color: tColor; owningList: tListOfFigures);
  end;

  tCircle = class(tFigure)
    index: integer; static;
    r: integer;
    procedure show(); override;
    procedure hide(); override;
    function getInfo(): string; override;
    constructor Create(x, y, r: integer; color: tColor; owningList: tListOfFigures);
  end;

  tEllipse = class(tFigure)
    index: integer; static;
    r, r2: integer;
    procedure show(); override;
    procedure hide(); override;
    function getInfo(): string; override;
    constructor Create(x, y, r, r2: integer; color: tColor; owningList: tListOfFigures);
  end;

  tSquare = class(tFigure)
    index: integer; static;
    a: integer;
    procedure show(); override;
    procedure hide(); override;
    function getInfo(): string; override;
    constructor Create(x, y, a: integer; color: tColor; owningList: tListOfFigures);
  end;

  tRectangle = class(tFigure)
    index: integer; static;
    a, b: integer;
    procedure show(); override;
    procedure hide(); override;
    function getInfo(): string; override;
    constructor Create(x, y, a, b: integer; color: tColor; owningList: tListOfFigures);
  end;

implementation

  constructor tListOfFigures.create(canvas: tCanvas);
  begin
    inherited create();
    self.canvas:=canvas;
    pFirstNode:=nil;
    pLastNode:=pFirstNode;
  end;

  procedure tListOfFigures.add(figure: tFigure);
  var pCurrent: tpNode;
  begin
    pCurrent:=new(tpNode);
    pCurrent^.figure:=figure;
    if pFirstNode = nil then
       pFirstNode:=pCurrent
    else
       pLastNode^.pNext:=pCurrent;
    pLastNode:=pCurrent;
    redraw();
  end;

  procedure tListOfFigures.remove();
  var pCurrent: tpNode;
  begin
    pCurrent:=pFirstNode;
    if pCurrent = pLastNode then
    begin
      pFirstNode:=nil;
      pLastNode^.figure.destroy();
      dispose(pLastNode);
    end
    else
    begin
      while pCurrent^.pNext <> pLastNode do
      begin
        pCurrent:=pCurrent^.pNext;
      end;
      pLastNode^.figure.destroy();
      dispose(pLastNode);
      pLastNode:=pCurrent;
    end;
    redraw();
  end;

  function tListOfFigures.report(): string;
  var pCurrent: tpNode;
      tmp: string;
  begin
    result:='';
    pCurrent:=pFirstNode;
    while pCurrent <> nil do
    begin
      tmp:=pCurrent^.figure.name;
      result:=result + tmp + #10#13;
      pCurrent:=pCurrent^.pNext;
    end;
  end;

  procedure tListOfFigures.activateFigure(name: string);
  var pCurrent: tpNode;
      tmpFigure: tFigure;
      tmpName: string;
  begin
     pCurrent:=pFirstNode;
     while pCurrent <> nil do
     begin
       tmpName:=pCurrent^.figure.name;
       if name.equals(tmpName) then
       begin
         tmpFigure:=pLastNode.figure;
         pLastNode^.figure:=pCurrent^.figure;
         pCurrent^.figure:=tmpFigure;
         break;
       end
       else
         pCurrent:=pCurrent^.pNext;
     end;
  end;

  procedure tListOfFigures.redraw();
  var pCurrent: tpNode;
  begin
    pCurrent:=pFirstNode;
    while pCurrent <> pLastNode^.pNext do
    begin
      pCurrent^.figure.show();
      pCurrent:=pCurrent.pNext;
    end;
  end;

  destructor tListOfFigures.destroy();
  var pCurrent: tpNode;
  begin
    while pFirstNode <> nil do
    begin
      pCurrent:=pFirstNode;
      pCurrent^.figure.destroy();
      pFirstNode:=pCurrent^.pNext;
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
    result:='тип: ' + self.ClassName + #10#13 + 'x= ' + IntToStr(x) + ', y= ' + IntToStr(y);
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

  constructor tDot.Create(x, y: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    index+=1;
    name:='dot' + IntToStr(index);
  end;

  procedure tDot.hide();
  begin
    owningList.canvas.Pixels[x,y]:=clNone;
  end;

  constructor tCircle.Create(x, y, r: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.r:=r;
    index+=1;
    name:='circle' + IntToStr(index);
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

  function tCircle.getInfo(): string;
  begin
    result:=inherited getInfo() + ', r= ' + IntToStr(r);
  end;

  constructor tEllipse.Create(x, y, r, r2: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.r:=r;
    self.r2:=r2;
    index+=1;
    name:='ellipse' + IntToStr(index);
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

  function tEllipse.getInfo(): string;
  begin
    result:=inherited getInfo() + 'r1= ' + IntToStr(r) + ', r2= ' + IntToStr(r2);
  end;

  constructor tLine.Create(x, y, x1, y1: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.x1:=x1;
    self.y1:=y1;
    index+=1;
    name:='line' + IntToStr(index);
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

  function tLine.getInfo(): string;
  begin
    result:=inherited getInfo() + ', x1= ' + IntToStr(x1) + ', y1= ' + IntToStr(y1);
  end;

  constructor tSquare.Create(x, y, a: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.a:=a;
    index+=1;
    name:='square' + IntToStr(index);
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

  function tSquare.getInfo(): string;
  begin
    result:=inherited getInfo() + ', a= ' + IntToStr(a);
  end;

  constructor tRectangle.Create(x, y, a, b: integer; color: tColor; owningList: tListOfFigures);
  begin
    inherited Create(x, y, color, owningList);
    self.a:=a;
    self.b:=b;
    index+=1;
    name:='rectangle' + IntToStr(index);
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

  function tRectangle.getInfo(): string;
  begin
    result:=inherited getInfo() + ', a= ' + IntToStr(a) + ', b= ' + IntToStr(b);
  end;

end.

