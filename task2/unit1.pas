unit unit1;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, figures;

type

{ TForm1 }

TForm1 = class(TForm)
Button1: TButton;
Button2: TButton;
Button3: TButton;
Button4: TButton;
Button5: TButton;
Label1: TLabel;
procedure Button1Click(Sender: TObject);
procedure Button2Click(Sender: TObject);
procedure Button3Click(Sender: TObject);
procedure Button4Click(Sender: TObject);
procedure Button5Click(Sender: TObject);
procedure FormCreate(Sender: TObject);
end;

var
Form1: TForm1;
figure: tFigure;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
figure:=tFigure.create(10, 10);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
figure.show();
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
figure.hide();
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
figure.moveTo(20, 20);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
figure.destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

