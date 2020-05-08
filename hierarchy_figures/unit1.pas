unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  figures;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Image1: TImage;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
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
  figure:=tDot.create(Round(Image1.Width/2), Round(Image1.Height/2), Image1.canvas);
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
  figure.moveTo(80, 50);
  showMessage('Объект типа ' + figure.name + ' переместился.');
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  showMessage('Объект типа ' + figure.name + ' уничтожен.');
  figure.destroy;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  figure:=tCircle.create(Round(Image1.Width/2), Round(Image1.Height/2), 25, Image1.canvas);
  showMessage('Объект типа ' + figure.name + ' создался.');
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  figure:=tEllipse.create(Round(Image1.Width/2), Round(Image1.Height/2), 20, 30, Image1.canvas);
  showMessage('Объект типа ' + figure.name + ' создался.');
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  figure:=tLine.create(40, 50, 70, 70, Image1.canvas);
  showMessage('Объект типа ' + figure.name + ' создался.');
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  figure:=tSquare.create(Round(Image1.Width/2), Round(Image1.Height/2), 40, Image1.canvas);
  showMessage('Объект типа ' + figure.name + ' создался.');
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  figure:=tRectangle.create(Round(Image1.Width/2), Round(Image1.Height/2), 20, 40, Image1.canvas);
  showMessage('Объект типа ' + figure.name + ' создался.');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Image1.show();
end;

end.
