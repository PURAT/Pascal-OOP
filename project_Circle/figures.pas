unit figures;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  tFigure = class
    name: string;
    x, y: integer;
    procedure show(); virtual; abstract;
    procedure hide(); virtual; abstract;
    function getInfo(): string; virtual;
    procedure moveTo(x, y: integer);
    constructor Create(x, y: integer);
    destructor Destroy(); override;
  end;

  tDot = class(tFigure)
    procedure show(); override;
    procedure hide(); override;
  end;

  tCircle = class(tFigure)
    r: integer;
    procedure show(); override;
    procedure hide(); override;
    constructor Create(x, y, r: integer);
    function getInfo(): string; override;
  end;

implementation

  uses unit1;

  constructor tFigure.Create(x, y: integer);
  begin
    inherited Create();
    self.x:=x;
    self.y:=y;
    name:=self.ClassName;
  end;

  procedure tDot.show();
  begin
    Form1.label1.caption:= self.getInfo();
  end;

  procedure tDot.hide();
  begin
    Form1.label1.caption:= '';
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

  constructor tCircle.Create(x, y, r: integer);
  begin
    inherited Create(x, y);
    self.r:=r;
  end;

  function tCircle.getInfo(): string;
  begin
    result:='тип: ' + name + #10#13 + 'x= ' + IntToStr(x) + ', y= ' + IntToStr(y) + ', r= ' + IntToStr(r);
  end;

  procedure tCircle.show();
  begin
    Form1.label1.caption:= self.getInfo();
  end;

  procedure tCircle.hide();
  begin
    Form1.label1.caption:= '';
  end;
end.

