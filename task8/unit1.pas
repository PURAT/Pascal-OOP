unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, figures;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Image1: TImage;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  end;

var
  Form1: TForm1;
  list: tListOfFigures;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  list:=tListOfFigures.create(Image1.canvas);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  tDot.create(Round(Image1.Width/2), Round(Image1.Height/2), clBlack, list);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  list.remove();
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  list.destroy();
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  list.pLastNode^.figure.moveTo(50, 50);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
   Memo1.Text:=list.report();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.show();
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

end.
