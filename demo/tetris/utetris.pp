unit utetris;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Web;

Const
  SGameOver = 'Game over!';
  SPlaying  = 'Playing...';

  BlockCount  = 7;
  BlockHigh   = BlockCount-1;
  BlockSize   = 4; // Number of positions in a block
  BoardHeight = 20;
  BoardWidth  = 12;
  CreatePosX = 4;
  CreatePosY = 0;
  BlockColors : Array [0..BlockCount] of String
    = ('white','#8F3985', '#39A275', '#D28140', '#194A8D', '#8D71B4', '#F0889D', '#DF1C44');

Type
  TDirection = (dIdle, dDown, dLeft, dRight);
  TVerticalCollision = (vcNone,vcBlock,vcWall);

{$modeswitch advancedrecords}

  TCoordinate = record
    x,y : Integer;
    Class function Create(aX,aY : integer) : TCoordinate; static;
  end;

  TBlock = Array[0..BlockSize-1] of TCoordinate;
  TBlocks = Array[0..BlockHigh] of TBlock;
  TBoard = Array[0..BoardWidth-1,0..BoardHeight-1] of Integer; // Colors
  TCoordinateBoard = Array[0..BoardWidth-1,0..BoardHeight-1] of TCoordinate; // Coordinates of squares

  { TTetris }

  TTetris = Class(TComponent)
  private
    function DoMouseClick(aEvent: TJSMouseEvent): boolean;
    function MoveBlockLeftRight(isRight: Boolean): Boolean;
  Private
    FCanvasID: String;
    FGameOver : Boolean;
    FCoordinates : TCoordinateBoard;
    FIncLevelInterval: Integer;
    FIncLevelScore: Integer;
    FInterval: Integer;
    FResetID: String;
    FTetrisLogo : TJSHTMLImageElement;
    FCanvas : TJSHTMLCanvasElement;
    FCtx : TJSCanvasRenderingContext2D;
    FScore : Integer;
    FLevel : Integer;
    FBoard : TBoard;
    FBlocks : TBlocks;
    FCurBlock : TBlock;
    FCurBlockColor : Smallint; // Index in color array
    FCurrPos : TCoordinate;
    Fdirection : TDirection;
    FElScore : TJSHTMLElement;
    FElLevel : TJSHTMLElement;
    FElStatus : TJSHTMLElement;
    FBtnReset : TJSHTMLButtonElement;
    FMyInterval : NativeInt;
    function DoResetClick(aEvent: TJSMouseEvent): boolean;
    procedure SetGameOver(AValue: Boolean);
    Procedure CheckBlockDown;
    procedure DrawBlockAt(X, Y, Color: Integer);
    procedure DrawLevel;
    Procedure DrawScore;
    procedure DrawGameStatus;
    procedure EnableTick;
    Function HittingTheWall : Boolean;
    Procedure MoveAllRowsDown(rowsToDelete, startOfDeletion : Integer);
    function CheckForVerticalCollision(aDirection: TDirection; aBlock: TBlock): TVerticalCollision;
    Function CheckForHorizontalCollision (aDirection: TDirection; aBlock: TBlock): Boolean;
    function CheckForCompletedRows : Boolean;
    Procedure CreateCoordArray;
    procedure RecalcScore(aRows: integer);
    procedure SetLevel(AValue: Integer);
    procedure SetScore(AValue: Integer);
    Procedure SetupTetris;
    Procedure DrawBlock;
    Procedure CreateBlocks;
    Procedure CreateBlock;
    Procedure DeleteBlock;
    function MoveBlockDown: Boolean;
    Procedure DropBlock;
    Procedure RotateBlock;
    Procedure ClearBoard;
    function HandleKeyPress(k : TJSKeyBoardEvent) : Boolean;
    Property GameOver : Boolean Read FGameOver Write SetGameOver;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Start;
    // Reset button ID
    Property ResetID : String Read FResetID Write FResetID;
    // our canvas ID
    Property CanvasID : String Read FCanvasID Write FCanvasID;
    Property Canvas : TJSHTMLCanvasElement Read FCanvas;
    Property Ctx : TJSCanvasRenderingContext2D Read FCTX;
    Property Score : Integer Read FScore Write SetScore;
    Property Level : Integer Read FLevel Write SetLevel;
    Property Board : TBoard Read FBoard Write FBoard;
    Property Stopped : TBoard Read FBoard Write FBoard;
    Property Blocks : TBlocks Read FBlocks;
    Property Coordinates : TCoordinateBoard Read FCoordinates;
    Property Interval : Integer Read FInterval Write FInterval;
    Property IncLevelScore : Integer Read FIncLevelScore Write FIncLevelScore;
    Property IncLevelInterval : Integer read FIncLevelInterval write FIncLevelInterval;
  end;

implementation

Class function TCoordinate.Create(aX,aY : integer) : TCoordinate;

begin
  Result.X:=aX;
  Result.Y:=aY;
end;

procedure TTetris.CreateCoordArray;

Const
  XStart = 11;
  XStep  = 23;
  YStart = 9;
  YStep  = 23;

Var
  x,y,i,j : Integer;

begin
  i:=0;
  j:=0;
  X:=XStart;
  For I:=0 to BoardWidth-1 do
    begin
    Y:=YStart;
    For J:=0 to BoardHeight-1 do
      begin
      FCoordinates[I,J]:=TCoordinate.Create(X,Y);
      Y:=Y+YStep;
      end;
    X:=X+XStep;
    end;
end;

Const
  ControlCount = 5;
  ControlNames : Array[1..5] of string = ('left','right','down','rotate','drop');

procedure TTetris.SetupTetris;

Var
  i : Integer;
  el : TJSElement;

begin
  if FCanvasID='' then
    FCanvasID:='my-canvas';
  if FResetID='' then
    FResetID:='btn-reset';
  FCanvas:=TJSHTMLCanvasElement(Document.getElementById(FCanvasID));
  FElScore:=TJSHTMLCanvasElement(Document.getElementById('score'));
  FElLevel:=TJSHTMLCanvasElement(Document.getElementById('level'));
  FElStatus:=TJSHTMLCanvasElement(Document.getElementById('status'));
  FBtnReset:=TJSHTMLButtonElement(Document.getElementById(FResetID));
  for I:=1 to ControlCount do
     begin
     El:=Document.GetElementById('control-'+ControlNames[i]);
     if Assigned(El) then
       TJSHTMLElement(El).onClick:=@DoMouseClick;
     end;
  if Assigned(FBtnReset) then
    FBtnReset.OnClick:=@DoResetClick;
  FCtx:=TJSCanvasRenderingContext2D(FCanvas.getContext('2d'));
  FCanvas.width := Round(FCanvas.OffsetWidth);
  FCanvas.height := Round(FCanvas.OffsetHeight);

  // ctx.scale(2, 2);
  ctx.fillStyle := 'white';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.strokeStyle := 'grey';
  ctx.strokeRect(8, 8, 280, 462);
  document.onkeydown:=@HandleKeyPress;
end;

procedure TTetris.DrawBlock;

Var
  i,X,Y : Integer;

begin
  for i:=0 to 3 do
    begin
    x:=FCurBlock[i].x + FCurrPos.X;
    y:=FCurBlock[i].y + FCurrPos.Y;
    DrawBlockAt(X,Y,FCurBlockColor);
    end;
end;

Function TTetris.MoveBlockLeftRight(isRight : Boolean) : Boolean;

begin
  Result:=False;
  if isRight then
    Fdirection:=dRight
  else
    Fdirection:=dLEFT;
  if (HittingTheWall() or checkForHorizontalCollision(FDirection,FCurBlock)) then
    Exit;
  DeleteBlock();
  if isRight then
    Inc(FCurrPos.X)
  else
    Dec(FCurrPos.X);
  DrawBlock();
  Result:=True;
end;


function TTetris.HandleKeyPress(k: TJSKeyBoardEvent) : Boolean;

  Procedure DisableKey;

  begin
    k.cancelBubble:=True;
    k.preventDefault;
  end;

begin
  Result:=False;
  if GameOver then
    exit;
  if (k.Code = TJSKeyNames.ArrowLeft) then
    begin
    DisableKey;
    Result:=not MoveBlockLeftRight(False)
    end
  else if (k.Code = TJSKeyNames.ArrowRight) then
    begin
    DisableKey;
    Result:=not MoveBlockLeftRight(True)
    end
  else if (k.Code = TJSKeyNames.ArrowDown) then
    begin
    DisableKey;
    MoveBlockDown();
    end
  else if (k.Code = TJSKeyNames.ArrowUp) then
    begin
    DisableKey;
    RotateBlock();
    end
  else if (k.Code = TJSKeyNames.Space) then
    begin
    DisableKey;
    DropBlock();
    end;
end;

constructor TTetris.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  CreateBlocks();
  CreateCoordArray();
  FLevel:=1;
  FScore:=0;
  FInterval:=1000;
  IncLevelScore:=100;
end;

function TTetris.MoveBlockDown: Boolean;

Var
  i,x,y : Integer;
  coll : TVerticalCollision;

  Procedure ShiftBlockDown;

  begin
    DeleteBlock;
    Inc(FCurrPos.Y);
    DrawBlock;
  end;

begin
  Result:=False;
  Fdirection:=dDOWN;
  Coll:=CheckForVerticalCollision(FDirection,FCurBlock);
  Result:=Coll=vcNone;
  if Result then
    ShiftBlockDown
  else
    begin
    if Coll<>vcWall then
      ShiftBlockDown;
    GameOver:=(FCurrPos.Y<=2);
    if Not GameOver then
      begin
      for I:=0 to BlockSize-1 do
        begin
        x:=FCurBlock[i].x + FCurrPos.X;
        y:=FCurBlock[i].y + FCurrPos.Y;
        FBoard[x,y]:=FCurBlockColor;
        end;
      CheckForCompletedRows();
      CreateBlock();
      FDirection:=dIdle;
      FCurrPos.X:=4;
      FCurrPos.Y:=0;
      DrawBlock();
      end;
    end;
end;

procedure TTetris.DropBlock;
begin
  While MoveBlockDown do;
end;

function TTetris.HittingTheWall : Boolean;

Var
  NewX,I : Integer;

begin
  Result:=False;
  I:=0;
  While (I<BlockSize) and Not Result do
    begin
    newX:=FCurBlock[i].X + FCurrPos.X;
    Result:=((newX <= 0) and (Fdirection = dLEFT)) or
            ((newX >= 11) and (Fdirection = dRIGHT));
    Inc(I);
    end;
end;

procedure TTetris.DrawGameStatus;

Var
  S : String;

begin
  if FGameOver then
    S:=SGameOver
  else
    S:=SPlaying;
  FElStatus.InnerText:=S
end;

procedure TTetris.DrawScore;

begin
  if Assigned(FElScore) then
    FElScore.InnerText:=IntToStr(FScore);
end;

procedure TTetris.DrawLevel;

begin
  if Assigned(FElLevel) then
    FElLevel.InnerText:=IntToStr(Flevel);
end;


function TTetris.CheckForVerticalCollision(aDirection : TDirection; aBlock : TBlock): TVerticalCollision;

Var
  X,Y,I : integer;

begin
  Result:=vcNone;
  I:=0;
  While (I<BlockSize) and (Result=vcNone) do
    begin
    x:=aBlock[i].x + FCurrPos.X;
    y:=aBlock[i].y + FCurrPos.Y;
    if (aDirection = dDOWN) then
      inc(Y);
    if FBoard[x,y+1]>0 then
      Result:=vcBlock
    else if (Y>=20) then
      Result:=vcWall;
    inc(I);
    end;
end;

function TTetris.CheckForHorizontalCollision(aDirection: TDirection; aBlock: TBlock): Boolean;

Var
  i, X,y : Integer;
begin
  Result:=False;
  I:=0;
  While (I<BlockSize) and Not Result do
    begin
    x:=aBlock[i].x + FCurrPos.X;
    y:=aBlock[i].y + FCurrPos.Y;
    if (adirection = dLEFT) then
      Dec(x)
    else if (adirection = dRIGHT) then
      Inc(x);
    Result:=FBoard[x,y]>0;
    Inc(i);
    end;
end;

function TTetris.CheckForCompletedRows : Boolean;

Var
  i,x,y,rowsToDelete, startOfDeletion: Integer;

begin
  Result:=False;
  rowsToDelete:=0;
  startOfDeletion:=0;
  y:=0;
  While Y<BoardHeight do
    begin
    Result:=true;
    X:=0;
    While (X<BoardWidth) and Result do
      begin
      Result:=FBoard[X,Y]>0;
      Inc(X);
      end;
    if (Result) then
      begin
      if (StartOfDeletion = 0) then
        startOfDeletion:=y;
      Inc(rowsToDelete);
      for I:=0 to BoardWidth-1 do
        begin
        FBoard[i,y]:=0;
        DrawBlockAt(i,y,0);
        end
      end;
    Inc(Y);
    end;
  if (RowsToDelete > 0) then
    begin
    MoveAllRowsDown(rowsToDelete, startOfDeletion);
    RecalcScore(rowsToDelete);
    end;
end;

procedure TTetris.RecalcScore(aRows : integer);

Var
  newLevel : Integer;

begin
  Inc(FScore,10*aRows);
  DrawScore;
  // Check if we need to increase the level.
  // We cannot use = since score could go from 90 to 110 if 2 rows are deleted
  newLevel:=1+(FScore div FIncLevelScore);
  if (NewLevel>FLevel) then
    begin
    FLevel:=NewLevel;
    FInterval:=FInterval-FIncLevelInterval;
    EnableTick;
    end;
end;

procedure TTetris.SetLevel(AValue: Integer);
begin
  if FLevel=AValue then Exit;
  FLevel:=AValue;
  DrawLevel;
end;

procedure TTetris.SetScore(AValue: Integer);
begin
  if FScore=AValue then Exit;
  FScore:=AValue;
  DrawScore;
end;

procedure TTetris.DrawBlockAt(X,Y,Color : Integer);

Var
  Coord : TCoordinate;

begin
  coord:=coordinates[x,y];
  ctx.fillStyle:=BlockColors[Color];
  ctx.fillRect(coord.X, coord.Y, 21, 21);
end;

procedure TTetris.MoveAllRowsDown(rowsToDelete, startOfDeletion: Integer);

Var
  I,x,y,Dest : Integer;

begin
  for i:=StartOfDeletion - 1 downto 0 do
    for X:=0 to BoardWidth-1 do
      begin
      Y:=I+RowsToDelete;
      Dest:=FBoard[x,i];
      FBoard[x,y]:=Dest;
      DrawBlockAt(X,Y,Dest);
      FBoard[x,i]:=0;
      DrawBlockAt(X,I,0);
      end;
end;

procedure TTetris.DeleteBlock;

var
  I,X,Y : integer;

begin
  For I:=0 to BlockSize-1 do
    begin
    x:=FCurBlock[i].X + FCurrPos.X;
    y:=FCurBlock[i].Y + FCurrPos.Y;
    FBoard[x,y]:=0;
    DrawBlockAt(X,Y,0);
    end;
end;

procedure TTetris.CreateBlocks;

  function co (x,y : Integer) : TCoordinate;
  begin
    Result:=TCoordinate.Create(X,Y);
  end;

begin
  FBlocks[0]:=[co(1,0), co(0,1), co(1,1), co(2,1)]; // T
  FBlocks[1]:=[co(0,0), co(1,0), co(2,0), co(3,0)]; // I
  FBlocks[2]:=[co(0,0), co(0,1), co(1,1), co(2,1)]; // J
  FBlocks[3]:=[co(0,0), co(1,0), co(0,1), co(1,1)]; // square
  FBlocks[4]:=[co(2,0), co(0,1), co(1,1), co(2,1)]; // L
  FBlocks[5]:=[co(1,0), co(2,0), co(0,1), co(1,1)]; // S
  FBlocks[6]:=[co(0,0), co(1,0), co(1,1), co(2,1)]; // Z
end;

procedure TTetris.CreateBlock;

Var
  rnd : Integer;

begin
  RND:=Random(BlockCount);
  FCurBlock:=FBlocks[RND];
  FCurBlockColor:=RND+1; // 0 is white
  FCurrPos.X:=CreatePosX;
  FCurrPos.Y:=CreatePosY;
end;


procedure TTetris.RotateBlock;

Var
  lBlock,newBlock:TBlock;
  x,y,i,maxX : Integer;

begin
  lBlock:=FCurBlock;
  maxX:=0;
  for I:=0 to BlockSize-1 do
    if lBlock[i].x>MaxX then
      MaxX:=lBlock[i].x;
  for I:=0 to BlockSize-1 do
    begin
    x:=lBlock[i].x;
    y:=lBlock[i].y;
    newBlock[i].X:=maxX-y;
    newBlock[i].Y:=x;
    end;
  // It can be that because of rotation, the block goes out of the board area or collisions.
  // In that case we forbid rotating
  // In fact we could try to reposition the block both horizontally and vertically:
  if (CheckForVerticalCollision(dIdle,NewBlock)=vcNone)
     and not CheckForHorizontalCollision(dIdle,NewBlock) then
    begin
    DeleteBlock();
    FCurBlock:=newBlock;
    DrawBlock();
    end;
end;

procedure TTetris.ClearBoard;

Var
  X,Y : integer;

begin
  For X:=0 to BoardWidth-1 do
    for Y:=0 to BoardHeight-1 do
      begin
      FBoard[X,Y]:=0;
      DrawBlockAt(X,Y,0);
      end;
end;

procedure TTetris.Start;

begin
  GameOver:=False;
  Level:=1;
  Score:=0;
  SetupTetris;
  ClearBoard;
  CreateBlock();
  DrawBlock();
  EnableTick;
end;

function TTetris.DoMouseClick(aEvent: TJSMouseEvent): boolean;

Const
  SControl = 'control-';

Var
  S : String;
begin
  Result:=true;
  S:=aEvent.currentTargetElement.ID;
  aEvent.preventDefault;
  if Copy(S,1,Length(SControl))=SControl then
    begin
    Delete(S,1,Length(sControl));
    Case S of
      'left' : MoveBlockLeftRight(False);
      'right' : MoveBlockLeftRight(True);
      'down' : MoveBlockDown;
      'rotate' : RotateBlock;
      'drop' : DropBlock;
    end;
    end;
end;

function TTetris.DoResetClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=True;
  FInterval:=1000;
  Start;
end;

procedure TTetris.SetGameOver(AValue: Boolean);
begin
  if FGameOver=AValue then Exit;
  FGameOver:=AValue;
  DrawGameStatus;
end;

procedure TTetris.CheckBlockDown;

begin
  If Not FGameOver then
    MoveBlockDown;
end;

procedure TTetris.EnableTick;

begin
  if FMyInterval>0 then
    window.clearInterval(FMyInterval);
  FMyInterval:=window.setInterval(@CheckBlockDown,FInterval);
end;

end.

