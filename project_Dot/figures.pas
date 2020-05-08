unit figures;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

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

end.

