unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Types,
  Vcl.ComCtrls,
  Vcl.Themes, Vcl.StdCtrls; // для StyleServices

type
  TNodeButtonKind = (nbNone, nbDelete, nbAction);

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
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure Button1Click(Sender: TObject);
    procedure TreeView1MouseLeave(Sender: TObject);
  private
    FHotNode: TTreeNode;
    FHotBtn: TNodeButtonKind;
    FDownNode: TTreeNode;
    FDownBtn: TNodeButtonKind;
    FOldTVProc: TWndMethod;

    procedure TVWndProc(var Msg: TMessage);
    function NodeHasAction(Node: TTreeNode): Boolean;
    function ButtonRect(TV: TCustomTreeView; Node: TTreeNode; Kind: TNodeButtonKind): TRect;
    function HitTestButton(TV: TCustomTreeView; X, Y: Integer; out Node: TTreeNode; out Kind: TNodeButtonKind): Boolean;
    procedure InvalidateButton(TV: TCustomTreeView; Node: TTreeNode; Kind: TNodeButtonKind);
    procedure DoNodeButtonClick(Node: TTreeNode; Kind: TNodeButtonKind);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
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
  node := TreeView1.Items.Add(nil, '_______________________' + IntToStr(val));
  node.Data := nil;
  if val mod 3 = 0 then
    node.Data := Pointer(val+1);
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
  if Node = FHotNode then begin FHotNode := nil; FHotBtn := nbNone; end;
  if Node = FDownNode then begin FDownNode := nil; FDownBtn := nbNone; end;
end;

function TForm1.NodeHasAction(Node: TTreeNode): Boolean;
begin
  Result := (Node <> nil) and (Node.Data <> nil); // критерий для вызова настроек
end;

function TForm1.ButtonRect(TV: TCustomTreeView; Node: TTreeNode; Kind: TNodeButtonKind): TRect;
const
  MarginRight = 4;
  Gap = 2;
var
  RText: TRect;
  TopY, BtnW, BtnH: Integer;
  RightX: Integer;
  ActionVisible: Boolean;
begin
  RText := Node.DisplayRect(True);
  BtnH := RText.Height;
  BtnW := BtnH;
  TopY := RText.Top + (RText.Height - BtnH) div 2;

  RightX := TV.ClientWidth - MarginRight;
  ActionVisible := NodeHasAction(Node);

  case Kind of
    nbDelete:
      begin
        Result := Rect(RightX - BtnW, TopY, RightX, TopY + BtnH);
      end;

    nbAction:
      begin
        if not ActionVisible then
          Exit(Rect(0,0,0,0));

        // action слева от delete
        Result := Rect(RightX - BtnW - Gap - BtnW, TopY, RightX - BtnW - Gap, TopY + BtnH);
      end;
  else
    Result := Rect(0,0,0,0);
  end;
end;

procedure TForm1.InvalidateButton(TV: TCustomTreeView; Node: TTreeNode; Kind: TNodeButtonKind);
var
  R: TRect;
begin
  if Node = nil then Exit;
  R := ButtonRect(TV, Node, Kind);
  if IsRectEmpty(R) then Exit;
  InflateRect(R, 2, 2);
  InvalidateRect(TV.Handle, @R, True);
end;

function TForm1.HitTestButton(TV: TCustomTreeView; X, Y: Integer; out Node: TTreeNode; out Kind: TNodeButtonKind): Boolean;
var
  P: TPoint;
begin
  Result := False;
  Kind := nbNone;
  Node := TV.GetNodeAt(X, Y);
  if Node = nil then Exit;

  P := Point(X, Y);

  // сначала delete (она всегда есть)
  if PtInRect(ButtonRect(TV, Node, nbDelete), P) then
  begin
    Kind := nbDelete;
    Exit(True);
  end;

  // потом action (если есть)
  if NodeHasAction(Node) and PtInRect(ButtonRect(TV, Node, nbAction), P) then
  begin
    Kind := nbAction;
    Exit(True);
  end;
end;

procedure TForm1.TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);

  procedure DrawBtn(Kind: TNodeButtonKind; const CaptionOrGlyph: string);
  var
    TV: TCustomTreeView;
    R: TRect;
    Details: TThemedElementDetails;
    Hot, Pressed: Boolean;
  begin
    TV := Sender;
    R := ButtonRect(TV, Node, Kind);
    if IsRectEmpty(R) then Exit;

    Hot := (Node = FHotNode) and (Kind = FHotBtn);
    Pressed := (Node = FDownNode) and (Kind = FDownBtn);

    if not TV.Enabled then
      Details := StyleServices.GetElementDetails(tbPushButtonDisabled)
    else if Pressed then
      Details := StyleServices.GetElementDetails(tbPushButtonPressed)
    else if Hot then
      Details := StyleServices.GetElementDetails(tbPushButtonHot)
    else
      Details := StyleServices.GetElementDetails(tbPushButtonNormal);

    StyleServices.DrawElement(TV.Canvas.Handle, Details, R);

    // вариант 1: символ (× / ...)
    StyleServices.DrawText(TV.Canvas.Handle, Details, CaptionOrGlyph, R,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0);
  end;

begin
  if Stage <> cdPostPaint then Exit;

  // delete всегда
  DrawBtn(nbDelete, '×');

  // action только при Data
  if NodeHasAction(Node) then
    DrawBtn(nbAction, '...'{'…'});
end;

procedure TForm1.TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TV: TCustomTreeView;
  N: TTreeNode;
  K: TNodeButtonKind;
begin
  TV := TCustomTreeView(Sender);

  if HitTestButton(TV, X, Y, N, K) then
  begin
    if (FHotNode <> N) or (FHotBtn <> K) then
    begin
      InvalidateButton(TV, FHotNode, FHotBtn);
      FHotNode := N;
      FHotBtn := K;
      InvalidateButton(TV, FHotNode, FHotBtn);
    end;
    TV.Cursor := crHandPoint;
  end
  else
  begin
    if FHotNode <> nil then
    begin
      InvalidateButton(TV, FHotNode, FHotBtn);
      FHotNode := nil;
      FHotBtn := nbNone;
    end;
    TV.Cursor := crDefault;
  end;
end;

procedure TForm1.TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TV: TCustomTreeView;
  N: TTreeNode;
  K: TNodeButtonKind;
begin
  if Button <> mbLeft then Exit;
  TV := TCustomTreeView(Sender);

  if HitTestButton(TV, X, Y, N, K) then
  begin
    FDownNode := N;
    FDownBtn := K;
    InvalidateButton(TV, N, K);
  end;
end;

procedure TForm1.TreeView1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TV: TCustomTreeView;
  DownN, HitN: TTreeNode;
  DownK, HitK: TNodeButtonKind;
begin
  if (Button <> mbLeft) or (FDownNode = nil) then Exit;

  TV := TCustomTreeView(Sender);

  DownN := FDownNode;
  DownK := FDownBtn;
  FDownNode := nil;
  FDownBtn := nbNone;
  InvalidateButton(TV, DownN, DownK);

  if HitTestButton(TV, X, Y, HitN, HitK) and (HitN = DownN) and (HitK = DownK) then
    DoNodeButtonClick(DownN, DownK);
end;

procedure TForm1.DoNodeButtonClick(Node: TTreeNode; Kind: TNodeButtonKind);
begin
  // сброс hover/pressed на всякий случай
  if Node = FHotNode then begin FHotNode := nil; FHotBtn := nbNone; end;
  if Node = FDownNode then begin FDownNode := nil; FDownBtn := nbNone; end;
  TreeView1.Cursor := crDefault;

  case Kind of
    nbDelete:
      Node.Delete;

    nbAction:
      ShowMessage('Action для: ' + Node.Text);
  end;
end;

procedure TForm1.TreeView1MouseLeave(Sender: TObject);
begin
  FHotNode := nil; FHotBtn := nbNone;
  FDownNode := nil; FDownBtn := nbNone;
  TreeView1.Cursor := crDefault;

  TreeView1.Invalidate;
end;

end.
