unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Types,
  Vcl.ComCtrls,
  Vcl.Themes, Vcl.StdCtrls; // для StyleServices

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1MouseLeave(Sender: TObject);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure Button1Click(Sender: TObject);
  private
    FHotNode: TTreeNode;
    FDownNode: TTreeNode;
    FOldTVProc: TWndMethod;

    procedure TVWndProc(var Msg: TMessage);
    function NodeHasButton(Node: TTreeNode): Boolean;
    function ButtonRect(TV: TCustomTreeView; Node: TTreeNode): TRect;
    procedure InvalidateButton(TV: TCustomTreeView; Node: TTreeNode);
    procedure DoNodeButtonClick(Node: TTreeNode);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  //TreeView1.DoubleBuffered := True;
  //TreeView1.OnAdvancedCustomDrawItem := TreeView1AdvancedCustomDrawItem;
  //TreeView1.OnMouseMove := TreeView1MouseMove;
  //TreeView1.OnMouseDown := TreeView1MouseDown;
  //TreeView1.OnMouseUp := TreeView1MouseUp;
  FOldTVProc := TreeView1.WindowProc;
  TreeView1.WindowProc := TVWndProc;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FOldTVProc) then
    TreeView1.WindowProc := FOldTVProc;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  node: TTreeNode;
  val: Integer;
begin
  val := TreeView1.Items.Count;
  //Memo1.Lines.Add('Button add node ' + IntToStr(val) + ': 0');
  node := TreeView1.Items.Add(nil, '_______________________' + IntToStr(val));
  //Memo1.Lines.Add('Button add node ' + IntToStr(val) + ': 1');
  node.Data := Pointer(val+1);
  //Memo1.Lines.Add('Button add node ' + IntToStr(val) + ': 2');
end;

procedure TForm1.TreeView1MouseLeave(Sender: TObject);
var
  OldHot, OldDown: TTreeNode;
begin
  OldHot := FHotNode;
  OldDown := FDownNode;

  FHotNode := nil;
  FDownNode := nil;      // если хотите как у обычной кнопки: ушли мышью — “нажатие” снимаем
  TreeView1.Cursor := crDefault;

  // Надёжнее всего — перерисовать весь TreeView (без артефактов)
  TreeView1.Invalidate;

  // Если хотите оптимальнее — можно так, но только если уверены, что узлы живы:
  // if OldHot <> nil then InvalidateButton(TreeView1, OldHot);
  // if (OldDown <> nil) and (OldDown <> OldHot) then InvalidateButton(TreeView1, OldDown);
end;

procedure TForm1.TVWndProc(var Msg: TMessage);
var
  OldClientW: Integer;
begin
  OldClientW := TreeView1.ClientWidth;

  // даём TreeView обработать сообщение
  FOldTVProc(Msg);

  // 1) если изменился ClientWidth (появился/исчез скроллбар) — перерисовать всё
  if TreeView1.ClientWidth <> OldClientW then
  begin
    FHotNode := nil;
    FDownNode := nil;
    TreeView1.Cursor := crDefault;
    TreeView1.Invalidate;
    Exit;
  end;

  // 2) при прокрутке/колесе/resize — тоже полный Invalidate
  case Msg.Msg of
    WM_VSCROLL, WM_HSCROLL, WM_MOUSEWHEEL, WM_SIZE:
      begin
        FHotNode := nil;
        FDownNode := nil;
        TreeView1.Cursor := crDefault;
        TreeView1.Invalidate;
      end;
  end;
end;

procedure TForm1.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
begin
  if Node = FHotNode then FHotNode := nil;
  if Node = FDownNode then FDownNode := nil;
end;

function TForm1.NodeHasButton(Node: TTreeNode): Boolean;
begin
  {if Assigned(Node) then
    Memo1.Lines.Add('NodeHasButton: ' + Node.Text + ' ' + IntToStr(Integer(Node.Data)));}
  Result := (Node <> nil) and (Node.Data <> nil); // ваш критерий
end;

function TForm1.ButtonRect(TV: TCustomTreeView; Node: TTreeNode): TRect;
const
  BtnW = 80;
  BtnH = 20;
  MarginRight = 6;
var
  R: TRect;
  TopY: Integer;
begin
  // DisplayRect(True) дает прямоугольник текста — нам важны Top/Height строки
  R := Node.DisplayRect(True);
  TopY := R.Top + (R.Height - BtnH) div 2;

  Result := Rect(
    TV.ClientWidth - BtnW - MarginRight,
    TopY,
    TV.ClientWidth - MarginRight,
    TopY + BtnH
  );
end;

procedure TForm1.InvalidateButton(TV: TCustomTreeView; Node: TTreeNode);
var
  R: TRect;
begin
  if not Assigned(Node) then Exit;
  R := ButtonRect(TV, Node);
  InvalidateRect(TV.Handle, @R, True);
end;

procedure TForm1.DoNodeButtonClick(Node: TTreeNode);
begin
  // пример: удалить узел
  // сбросить “hover/pressed”, чтобы дальше никто не трогал удалённый Node
  if Node = FHotNode then FHotNode := nil;
  if Node = FDownNode then FDownNode := nil;
  TreeView1.Cursor := crDefault;

  Node.Delete;
end;

procedure TForm1.TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  TV: TCustomTreeView;
  RBtn: TRect;
  Details: TThemedElementDetails;
  Pressed, Hot: Boolean;
begin
  if Stage <> cdPostPaint then Exit;

  TV := Sender;
  if not NodeHasButton(Node) then Exit;

  RBtn := ButtonRect(TV, Node);
  Hot := (Node = FHotNode);
  Pressed := (Node = FDownNode);

  // Рисуем как нормальную VCL-кнопку с учетом VCL Styles
  if not TV.Enabled then
    Details := StyleServices.GetElementDetails(tbPushButtonDisabled)
  else if Pressed then
    Details := StyleServices.GetElementDetails(tbPushButtonPressed)
  else if Hot then
    Details := StyleServices.GetElementDetails(tbPushButtonHot)
  else
    Details := StyleServices.GetElementDetails(tbPushButtonNormal);

  StyleServices.DrawElement(TV.Canvas.Handle, Details, RBtn);
  StyleServices.DrawText(TV.Canvas.Handle, Details, '...', RBtn,
    DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0);
end;

procedure TForm1.TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TV: TCustomTreeView;
  Node: TTreeNode;
  OverBtn: Boolean;
begin
  TV := TCustomTreeView(Sender);
  Node := TV.GetNodeAt(X, Y);

  OverBtn := NodeHasButton(Node) and PtInRect(ButtonRect(TV, Node), Point(X, Y));

  if OverBtn then
  begin
    if FHotNode <> Node then
    begin
      InvalidateButton(TV, FHotNode);
      FHotNode := Node;
      InvalidateButton(TV, FHotNode);
    end;
    TV.Cursor := crHandPoint;
  end
  else
  begin
    if FHotNode <> nil then
    begin
      InvalidateButton(TV, FHotNode);
      FHotNode := nil;
    end;
    TV.Cursor := crDefault;
  end;
end;

procedure TForm1.TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TV: TCustomTreeView;
  Node: TTreeNode;
begin
  TV := TCustomTreeView(Sender);
  Node := TV.GetNodeAt(X, Y);

  if (Button = mbLeft) and NodeHasButton(Node) and
     PtInRect(ButtonRect(TV, Node), Point(X, Y)) then
  begin
    FDownNode := Node;
    InvalidateButton(TV, Node);
  end;
end;

procedure TForm1.TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TV: TCustomTreeView;
  ClickNode, NodeUnderMouse: TTreeNode;
begin
  TV := TCustomTreeView(Sender);

  if (Button <> mbLeft) or (FDownNode = nil) then Exit;

  ClickNode := FDownNode;
  FDownNode := nil;                 // сразу обнуляем “pressed”
  InvalidateButton(TV, ClickNode);   // пока узел точно жив

  NodeUnderMouse := TV.GetNodeAt(X, Y);
  if (NodeUnderMouse = ClickNode) and PtInRect(ButtonRect(TV, ClickNode), Point(X, Y)) then
    DoNodeButtonClick(ClickNode);   // тут узел может быть удалён, дальше ClickNode не трогаем
end;

end.
