uses
  Winapi.Windows, Winapi.Messages, System.Types,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Graphics,
  Vcl.Themes; // для StyleServices

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FHotNode: TTreeNode;
    FDownNode: TTreeNode;
    function NodeHasButton(Node: TTreeNode): Boolean;
    function ButtonRect(TV: TCustomTreeView; Node: TTreeNode): TRect;
    procedure InvalidateButton(TV: TCustomTreeView; Node: TTreeNode);
    procedure DoNodeButtonClick(Node: TTreeNode);
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TreeView1.DoubleBuffered := True;

  TreeView1.OnAdvancedCustomDrawItem := TreeView1AdvancedCustomDrawItem;
  TreeView1.OnMouseMove := TreeView1MouseMove;
  TreeView1.OnMouseDown := TreeView1MouseDown;
  TreeView1.OnMouseUp := TreeView1MouseUp;
end;

procedure TForm1.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
begin
  if Node = FHotNode then FHotNode := nil;
  if Node = FDownNode then FDownNode := nil;
end;

function TForm1.NodeHasButton(Node: TTreeNode): Boolean;
begin
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
  StyleServices.DrawText(TV.Canvas.Handle, Details, 'Удалить', RBtn,
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
