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
    Button6: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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
  figure:=tDot.create(10, 10);
  showMessage('Объект типа ' + figure.name + ' создался.');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  figure.show();
  showMessage('Объект типа ' + figure.name + ' показался.');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  figure.hide();
  showMessage('Объект типа ' + figure.name + ' скрылся.');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  figure.moveTo(20, 20);
  label1.caption:=figure.getinfo();
  showMessage('Объект типа ' + figure.name + ' переместился.');
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  figure.destroy;
  showMessage('Объект типа ' + figure.name + ' уничтожен.');
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  figure:=tCircle.create(10, 10, 5);
  showMessage('Объект типа ' + figure.name + ' создался.');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

