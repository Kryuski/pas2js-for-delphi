unit upacman;

interface

uses
  sysutils, classes, types, web, js;

const
  TimerInterval = 20;
  GridXSize   = 30;
  GridYSize   = 33;
  DrawGrid = False;

  ControlCount = 5;
  ControlNames : Array[1..ControlCount] of string = ('left','right','down','up','pause');


type
  TAudio = (aStart,aDie,aEatGhost,aEatPill);

  TStr4 = String;   // set of N,E,S,W

  TSprite=record
    SpImg    : TJSHTMLImageElement;   // Image of a ghost
    XY       : TPoint;   // Grid x and y
    Sx,Sy    : double;   // smooth x and y between 0 en 1
    Dir      : char;     // N,E,S,W
    Spd      : double;
    StartPos : TPoint;
  end;

  TCell=record
    WallType  :(wtNone,wtEW,wtNS,wtNE,wtNW,wtSW,wtSE,wtNoGo);
    PillType  :(ptNone,ptPill,ptSuperPill);
    I         :integer; // used for searching the maze
    Dirty : Boolean;
  end;

  TField = array[0..GridYSize-1] of String;

  { TPacman }
  TProcedure = Procedure Of Object;

  { TPacmanAudio }

  TPacmanAudio = Class
  private
    FOnLoaded: TNotifyEvent;
    FLoaded : Boolean;
    procedure AudioLoaded;
    function CheckEnd(Event: TEventListenerEvent): boolean;
    function CheckplayOK(Event: TEventListenerEvent): boolean;
  published
    files   : Array [TAudio] of TJSHTMLAudioElement;
    filesOK : Array [TAudio] of Boolean;
    Playing : Array [TAudio] of Boolean;
    Procedure LoadAudio;
    Procedure play(aAudio : Taudio);
    Procedure DisableSound;
    Procedure Pause;
    Procedure Resume;
    Property Loaded : Boolean Read FLoaded Write FLoaded;
    Property OnLoaded : TNotifyEvent Read FOnLoaded Write FonLoaded;
  end;

  TPacman = class(TComponent)
  Private
    // Html image elements
    // 0 = pacman, virtual
    // 1..4 : Ghost
    // 5 = scared
    ImgGhost : Array[0..5] of TJSHTMLImageElement;
    ImgBonus: TJSHTMLImageElement;
    SpriteTimer: NativeInt;
    pnBonusBarOuter: TJSHTMLElement;
    pnBonusBarInner: TJSHTMLElement;
    pnScareBarOuter: TJSHTMLElement;
    pnScareBarInner: TJSHTMLElement;
    lbBonusCnt: TJSHTMLElement;
    lbLives: TJSHTMLElement;
    lbScore: TJSHTMLElement;
    lbStatus: TJSHTMLElement;
    lbHiscore: TJSHTMLElement;
    lbGhostCnt: TJSHTMLElement;
    FCanvasEl:TJSHTMLCanvasElement;
    FCanvas:TJSCanvasRenderingContext2D;
    FCBXSound:TJSHTMLInputElement;
    FBtnReset : TJSHTMLButtonElement;
    FAudio : TPacmanAudio;
    function CheckSound(Event: TEventListenerEvent): boolean;
    procedure DoAudioLoaded(Sender: TObject);
    function DoResetClick(aEvent: TJSMouseEvent): boolean;
    procedure InitAudio;
    procedure MarkCellsDirty;
  private
    FAudioDisabled: Boolean;
    FCanvasID: String;
    FResetID: String;
    Pause:boolean;
    LivesLeft:integer;
    BonusCnt :integer;
    GhostCnt :integer;
    BonusTimer:integer;
    ScareTimer:integer;
    PacMouthOpen:integer;
    PacMouthOpenDir:integer;
    PillsLeft:integer;
    PacmanDir:char;
    score,HiScore:integer;
    // 0: Packman.
    // 1..4 : ghost
    // 5: Bonus
    Sprite:array[0..5] of TSprite;
    Cells:array[0..GridXSize-1,0..GridYSize] of TCell;
    FDying : Boolean;
// Maze solving code
    function  SolveMaze     (P1,P2: TPoint): boolean;
    function  SolveMazeStep1(P1,P2: TPoint): boolean;
    function  SolveMazeStep2(P1,P2: TPoint): boolean;
    function  SolveMazeStep3(P1,P2: TPoint): boolean;
// Display code
    procedure line(x1, y1, x2, y2: integer);
    procedure DrawCells(DirtyOnly : Boolean = False);
    procedure DrawPacman();
    procedure CheckGameOver;
    procedure StartTimer;
    procedure ShowText(aText: string; OnDone : TProcedure);
    procedure UpdateScore();
    procedure UpdateStatus(aText : String);
// Initializing code
    procedure InitSprite(var aSprite: TSprite; aImg: TJSHTMLImageElement; aSpd: Double);
    procedure InitSprites();
    procedure InitVars(aField: TField);
    procedure InitCells(aField: TField);
    procedure SetGhostScared(aScared: boolean);
// Business code: TestAndGet
    function  GetGhostDir(aXY:TPoint; aOldDir: char): char;
    function  GetBestDir(aXY:TPoint): char;
    function  GetPossibleDir(aXY:TPoint): TStr4;
    function  GetPacmanDir(aXY:TPoint; aOldDir: char): char;
    procedure GetRandomCellAndDir(var aXY:TPoint; var aDir: char);
// Business code: Actions
    procedure StopTimer;
    Function DoRestartClick(aEvent: TJSMouseEvent): boolean;
    procedure EatPill(aXY: TPoint);
    procedure EatSuperPill(aXY: TPoint);
    procedure EatBonus();
    procedure EatGhost(var aGhost: TSprite);
    procedure ClearCell(aXY: TPoint);
    procedure MoveSprite(aSpriteInx:integer);
    function  DoBonusTimer(): boolean;
    procedure DoScareTimer();
    Procedure DrawScene;
// Business code: Decisions
    procedure CollisionDetect(var aXY:TPoint);
    procedure RestartGame();
    procedure RestartLevel();
    procedure PacmanDies();
    procedure NextLevel();
    procedure GameOver();
    // Debug & Test
    // procedure DbgShow();
    // Business code: Actions
    Procedure PlaySound(aAudio : TAudio);
    procedure DoSpriteTimer;
    // User response code
    function HandleKeyPress(k : TJSKeyBoardEvent) : Boolean;
    function DoMouseClick(aEvent: TJSMouseEvent): boolean;
  Public
    // Initializing code
    Constructor Create(aOwner : TComponent); override;
    Procedure SetupPacman;
    Procedure Start;
    Property CanvasID : String Read FCanvasID Write FCanvasID;
    Property ResetID : String Read FResetID Write FResetID;
    Property AudioDisabled : Boolean Read FAudioDisabled Write FAudioDisabled;
  end;

implementation


//==============================================================================
// Generic constants
//==============================================================================
// These constants define the look and feel of the game.
// They set speeds and timeouts, and the define a playing field
// To make the definition of a different playing field easier it is defined as
// an array of strings, in which each character defines specific cell-properties
// The initialization code reads this and uses it to build an array of type TCell[].
//
// The const Level1field defines a playing field.
// These are the characters used to define the habitat of the ghosts and pacman
//   'x'      : a NoGo area. It shows up empty on the screen, but ghosts, pacman
//              and bonusses cannot go there.
//   '-','|'  : a horizontal or verical wall
//   '/','\'  : a cornerwall, which one depends on surrounding cells
//   '1'..'4' : starting position of ghost 1 to 4
//   'P'      : starting position of Pacman
//   ' '      : empty space, Pacman, ghosts and bonusses can go there
//   '.'      : simple pill, Pacman, ghosts and bonusses can go there
//   'o'      : super pill,  Pacman, ghosts and bonusses can go there.
//              This also sets the "ScareTheGhosts" timer
//==============================================================================

const
   CellSize           =   16; // do not change...
   GhostSpeedScared   = 0.10; // Speed of ghosts when scared
   GhostSpeedNormal   = 0.20; // Speed of ghosts when not scared.
   PacmanSpeed        = 0.25; // Speed of Pacman
   BonusSpeed         = 0.04; // speed of cherries
   BonusTimeOut1      =  500; // time for cherries not visible
   BonusTimeOut2      =  300; // time for cherries visible
   ScareTimeOut       =  300; // time that the ghosts stay scared
   HuntFactor         =  0.5; // 0.0:ghosts move random, 1.0=ghosts really hunt

   AudioNames : Array[TAudio] of string = ('start','die','eatghost','eatpill');

const
   Level1Field : TField =
     ('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
      'x/------------\/------------\x',
      'x|............||............|x',
      'x|./--\./---\.||./---\./--\.|x',
      'x|o|xx|.|xxx|.||.|xxx|.|xx|o|x',
      'x|.\--/.\---/.\/.\---/.\--/.|x',
      'x|..........................|x',
      'x|./--\./\./------\./\./--\.|x',
      'x|.\--/.||.\--\/--/.||.\--/.|x',
      'x|......||....||....||......|x',
      'x\----\.|\--\ || /--/|./----/x',
      'xxxxxx|.|/--/ \/ \--\|.|xxxxxx',
      'xxxxxx|.||          ||.|xxxxxx',
      'xxxxxx|.|| /--  --\ ||.|xxxxxx',
      '------/.\/ | 1 3  | \/.\------',
      '       .   |  2 4 |   .       ',
      '------\./\ |      | /\./------',
      'xxxxxx|.|| \------/ ||.|xxxxxx',
      'xxxxxx|.||          ||.|xxxxxx',
      'xxxxxx|.|| /------\ ||.|xxxxxx',
      'x/----/.\/ \--\/--/ \/.\----\x',
      'x|............||............|x',
      'x|./--\./---\.||./---\./--\.|x',
      'x|.\-\|.\---/.\/.\---/.|/-/.|x',
      'x|o..||.......P........||..o|x',
      'x\-\.||./\./------\./\.||./-/x',
      'x/-/.\/.||.\--\/--/.||.\/.\-\x',
      'x|......||....||....||......|x',
      'x|./----/\--\.||./--/\----\.|x',
      'x|.\--------/.\/.\--------/.|x',
      'x|..........................|x',
      'x\--------------------------/x',
      'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');

const
   WallSet = ['-','|','\','/'];

   clBlack = 'black';
   clWhite = 'white';
   clRed = 'red';
   clYellow = '#FFFF00';
   clBlue = 'blue';
   clLime = 'lime';

{ TPacman }


constructor TPacman.Create(aOwner: TComponent);
begin
  inherited;
  FaudioDisabled:=True;
  FAudio:=TPacmanAudio.Create;
  Faudio.OnLoaded:=@DoAudioLoaded;
  SetupPacman;
end;

//==============================================================================
//  Display code
//==============================================================================
// This code is responsible for showing pacman, ghosts, bonuses, scores on the
// screen It uses global variables and the Cells[] array to know where and what
// ShowText()      this code shows a flashing text (how surprising) in the
//                 middle of the playing field for about 3 seconds
// Line()          draws a line on img.canvas (should be a standard function!!!)
// DrawCells()     clears and draws the complete playingfield according to the
//                 cell properties in the Cell[] array. Does not draw Pacman,
//                 ghosts or flying bonusses.
// DrawPacman()    Draws an image of Pacman in sprite[0] depending on direction
// UpdateScore()   Updates the labels for lives, score, hiscore etc.

Type

  { TFlashText }

  TFlashText = Class(TObject)
    FPacMan : TPacMan;
    FText : String;
    FFlashInterval : NativeInt;
    FCount : Integer;
    FonDone : TProcedure;
    Procedure DoFlash;
    Constructor Create(aPacMan : TPacMan; aText : String; aOnDone : TProcedure);
  end;

{ TFlashText }

procedure TFlashText.DoFlash;

var
  n,x,y:integer;
  FS : TJSTextMetrics;

begin
//  FPacMan.FCanvas.fillStyle:=clBlack;
  if FCount mod 2=0 then
    FPacMan.FCanvas.FillStyle:=clRed //textbackground is black
  else
    FPacMan.FCanvas.FillStyle:=clYellow; //textbackground is black
  FPacMan.FCanvas.Font:='40px Roboto';        //make text really big
  // position text in the middle of the field
  FS:=FPacMan.FCanvas.measureText(FText);
  x:=FPacMan.FCanvasEl.Width div 2-Round(FS.width) div 2;
  y:=FPacMan.FCanvasEl.Height div 2- 20 { Round(FS.actualBoundingBoxAscent) div 2};
  FPacMan.FCanvas.FillText(FText,x,y);
  Inc(FCount);
  if FCount>=10 then
    begin
    window.ClearInterval(FFlashInterval);
    FPacMan.DrawScene;
    if Assigned(FonDone) then
      FonDone();
    Free;
    end;
end;

constructor TFlashText.Create(aPacMan : TPacMan; aText: String; aOnDone : TProcedure);
begin
  FPacMan:=aPacMan;
  FText:=aText;
  FOnDone:=aOnDone;
  DoFlash;
  FFlashInterval:=window.setInterval(@DoFlash,150);
end;

procedure TPacman.ShowText(aText: string; OnDone : TProcedure);

begin
  TFlashText.Create(Self,aText,OnDone);
end;

procedure TPacman.line(x1, y1, x2, y2: integer);
begin // should be a standard method of a canvas...
  FCanvas.BeginPath;
  FCanvas.MoveTo(x1,y1);
  FCanvas.LineTo(x2,y2);
  FCanvas.stroke();
end;


procedure TPacman.DrawCells(DirtyOnly : Boolean = False);

const
   Sze=CellSize;
   HSze=CellSize div 2;

   Procedure DoArc(x,y,r,a1,a2 : Double; anti : boolean = false);

   begin
     FCanvas.BeginPath;
     FCanvas.Arc(x,y,r,a1,a2,anti);
     FCanvas.Stroke;
   end;

var
  x,y,sx,sy,r:integer;

begin
  // Clear where necessary
  with FCanvas do
    if DirtyOnly then
      begin
      // Only selected cells
      StrokeStyle:=clBlack;
      FillStyle:=clBlack;
      for x:=0 to GridXSize-1 do
        for y:=0 to GridYSize-1 do
          if Cells[x,y].Dirty or not DirtyOnly then
            begin
            sx:=x*Sze;
            sy:=y*Sze; //calculate pixel position on screen
            FillRect(sx,sy,sze,sze);
            end;
      end
    else
      begin
      // clear screen to black
      FillStyle:='black';
      FillRect(0,0, FCanvasEl.Width,FCanvasEl.Height);
      // Draw supportGrid (helpfull during development, not needed)
      if DrawGrid then
        begin
        lineWidth:=2; // Pen.width:=1;
        StrokeStyle:='#202020';
        for x:=0 to GridXSize do
          line(x*Sze,0,x*Sze,Sze*(GridYSize));
        for y:=0 to GridYSize do
          line(0,y*Sze,Sze*(GridXSize),y*Sze);
        end;
      end;
  // Draw pills
  With FCanvas do
    begin
    // Draw Pills
    StrokeStyle:=clWhite;
    FillStyle:=clWhite;
    for x:=0 to GridXSize-1 do
      for y:=0 to GridYSize-1 do
        if Cells[x,y].Dirty or not DirtyOnly then
          begin
          sx:=x*Sze+HSze;
          sy:=y*Sze+HSze;
          r:=0;
          case Cells[x,y].PillType of
            ptPill      : r:=2;
            ptSuperPill : r:=6;
          end;
          if r>0 then
            begin
            BeginPath;
            Arc(sx,sy,r,0,2*Pi);
            Fill;
            end;
          end;
    end;
  // Draw Walls per cell
  With FCanvas do
    begin
    StrokeStyle:=clBlue;
    FillStyle:=clBlue;
    LineWidth:=sze div 4;
    for x:=0 to GridXSize-1 do
      for y:=0 to GridYSize-1 do
        if Cells[x,y].Dirty or not DirtyOnly then
          begin
          sx:=x*Sze;
          sy:=y*Sze; //calculate pixel position on screen
          case Cells[x,y].WallType of
            wtEW: line(sx,sy+hsze,sx+sze,sy+hsze);      // left to right
            wtNS: line(sx+hsze,sy,sx+hsze,sy+sze);      // top to bottom
            wtSW: DoArc(sx    , sy+Sze, Sze / 2,0   ,(3*Pi/2),true); // bottom to left
            wtNE: DoArc(sx+Sze, sy    , Sze / 2,Pi/2,Pi);      // top to right
            wtSE: DoArc(sx+Sze, sy+Sze, Sze / 2,Pi  ,Pi*3/2); // bottom to right
            wtNW: DoArc(sx    , sy    , Sze / 2,0   ,Pi/2);     // top to left
          end;
          Cells[x,y].Dirty:=False;
          end;
    end;
end;

procedure TPacman.DrawPacman();

Const
   Radius      = 12;
   Offset      = CellSize;
   EyeY        = CellSize * 2/3;
   LeftEyeX    = CellSize * 2/3;
   RightEyeX   = CellSize * 4/3;
   MouthRadius = CellSize * 1/3;
   EyeRadius   = 1.5;

Var
  X,Y : Double;

  Procedure Pie(aAngle : double);

  Var
    aStart,aEnd : Double;

  begin
    if PacMouthOpen=0 then
        begin
        aStart:=0;
        aEnd:=2*pi
        end
      else
        begin
        aStart:=aAngle + (PacMouthOpen/90)*(Pi/2);
        if aStart>2*Pi then
          aStart:=aStart-2*pi;
        aEnd  :=aAngle - (PacMouthOpen/90)*(Pi/2);
        {
        // Draw this to clear first
        FCtx.fillStyle:=clBlack;
        FCtx.StrokeStyle:=clBlack;
        FCtx.Arc(X+15,Y+15,Radius,0,2*pi,True);
        FCtx.Fill;
        }
        end;
    With FCanvas do
      begin
      BeginPath;
      MoveTo(X+OffSet,Y+Offset);
      Arc(X+Offset,Y+Offset,Radius,aStart,aEnd);
      LineTo(X+Offset,Y+Offset);
      Fill;
      end;
  end;

begin
  X:=Sprite[0].XY.x*CellSize-CellSize/2;
  Y:=Sprite[0].XY.y*CellSize-CellSize/2;
  if PacMouthOpen>40 then
    PacMouthOpenDir:=-10  // if maxopen then start closing
  else if PacMouthOpen<2  then
    PacMouthOpenDir:= 10;     // if minopen then start opening
  inc(PacMouthOpen,PacMouthOpenDir);               // adjust mouth opening
  with FCanvas do
    begin
    FillStyle:=clYellow;       // set face color to yellow
    StrokeStyle:=clYellow;         // pen too
    case Sprite[0].Dir of        // draw face depending on direction (opposite to what you'd expect)
      'E': Pie(Pi); // to the right
      'W': Pie(0); // to the left
      'N': Pie(3*Pi/2); // to the top
      'S': Pie(Pi/2); // to the bottom
    else
      beginPath;
      Arc(X+OffSet,y+OffSet,Radius,0,2*pi);     // whole face area
      Fill();
      FillStyle:=clBlack;       //
      StrokeStyle:=clBlack;         //
      beginPath;
      Arc(X+LeftEyeX,Y+EyeY,EyeRadius,0,2*pi);     // left eye
      Stroke;
      beginPath;
      Arc(X+RightEyeX,Y+EyeY,Eyeradius,0,2*pi);    // right eye
      Stroke;
      LineWidth:=3;               //
      beginPath;
      arc(X+offSet,Y+OffSet,MouthRadius,0,Pi);//mouth
      Stroke;
    end;
  end;
end;

procedure TPacman.UpdateScore();
begin
  if Score>HiScore then
    HiScore:=Score;
  lbScore.InnerText   := inttostr(Score);
  lbHiScore.InnerText := inttostr(HiScore);
  lbLives.InnerText   := inttostr(LivesLeft);
  lbBonusCnt.InnerText:= inttostr(BonusCnt);
  lbGhostCnt.InnerText:= inttostr(GhostCnt);
end;

procedure TPacman.UpdateStatus(aText: String);
begin
  lbStatus.InnerText:=aText;
end;

//==============================================================================
//  Initialization code
//==============================================================================
// There are several moments in the game something needs to be put in the
// beginstate.
// InitSprite()    Called by InitSprites on Create(), creates images and presets
//                 sprite variables
// InitSprites()   This code first creates and initializes all objects and
//                 variables sets their beginstate values. Called only once !!
// InitVars()      This gets some sprite properties from a TField constant
//                 and resets counters prior to a new game
// InitCells()     This copies the cell-properties from a TField constant
// SetGhostScared() sets images and speeds of the 4 ghosts depending on param.

procedure TPacman.InitSprite(var aSprite: TSprite; aImg: TJSHTMLImageElement; aSpd: Double);
begin
  aSprite.spImg := aImg;  // get an image instance, owned
  aSprite.SpImg.Width:=28;                 // make the black pixels transparent
  aSprite.SpImg.Height:=28;                // make the black pixels transparent
  aSprite.dir   := '-';                    // no direction
  aSprite.Spd   := aSpd;                   // default speed
  aSprite.XY    := point(1,1);             // Just a non error generating value
  aSprite.Sx    := 0;                      // partial X in the middle of a cell
  aSprite.Sy    := 0;                      // partial Y in the middle of a cell
  aSprite.StartPos:=point(2,2);            // Just a non error generating value
end;

procedure TPacman.InitSprites();

var
  I : integer;

begin
  Sprite[0].SpImg:=Nil;
  For I:=1 to 4 do
   InitSprite(Sprite[I],ImgGhost[i],GhostSpeedNormal);
  Sprite[0].Spd:=PacmanSpeed; // the image is overwritten later
  InitSprite(Sprite[5],ImgBonus ,BonusSpeed);
end;


procedure TPacman.InitVars(aField: TField);

// Uses a TField definition to set the global variable PillCount and the initial
// positions of Pacman and the Ghosts, Also (pre)sets timers and pacman's mouth.

var x,y,n:integer;

begin
  PillsLeft:=0;
  Score    :=0;
  LivesLeft:=3;
  BonusCnt :=0;
  GhostCnt :=0;
  Pause    :=false;
  pacMouthopen:=0;
  pacMouthopenDir:=10; //startvalues for open mouth of pacman
  for x:=0 to GridXSize-1 do
    for y:=0 to GridYSize-1 do
      begin
      case aField[y][x+1] of
        '.','o': inc(PillsLeft); // normal and superpills
        'P'    : sprite[0].StartPos:=point(x,y); // starting position of PacMan
        '1'    : sprite[1].StartPos:=point(x,y); // starting position of Ghost #1
        '2'    : sprite[2].StartPos:=point(x,y); // starting position of Ghost #2
        '3'    : sprite[3].StartPos:=point(x,y); // starting position of Ghost #3
        '4'    : sprite[4].StartPos:=point(x,y); // starting position of Ghost #4
      end;
      end;
  for n:=0 to 4 do
    sprite[n].XY:=sprite[n].StartPos;
  ScareTimer:=0;
  BonusTimer:=0;
end;

procedure TPacman.InitCells(aField: TField);
// Uses a TField definition to set properties of all cells in the Cell[] array
const
  wsH=['-','\','/']; // set of wall chars used in SW-NE detection
  wsV=['|','\','/']; // set of wall chars used in SE-NW detection
var
  x,y:integer;
  c : char;
begin
  for y:=0 to GridYSize-1 do
    for x:=0 to GridXSize-1 do
    begin
    // Set values for WallType from string-field definition
    c:=aField[y][x+1];
    case c of
      '|': Cells[x,y].WallType:=wtNS;      // top to bottom
      '-': Cells[x,y].WallType:=wtEW;      // left to right
      '\': if (aField[y][x] in wsH) and (aField[y+1][x+1] in wsV)
           then Cells[x,y].WallType:=wtSW  // bottom to left
           else Cells[x,y].WallType:=wtNE; // top to right
      '/': if (aField[y][x+2] in wsH) and (aField[y+1][x+1] in wsV)
           then Cells[x,y].WallType:=wtSE  // bottom to right
           else Cells[x,y].WallType:=wtNW; // top to left
      'x': Cells[x,y].Walltype:=wtNoGo;    // no visible wall, but still occupied
    else
       Cells[x,y].WallType:=wtNone;    // no obstacle to pacman and ghosts
    end;
    // set values for PillType from string-field definition
    case c of
      '.': Cells[x,y].PillType := ptPill;  // this cell contains a Pill
      'o': Cells[x,y].PillType := ptSuperPill; // this cell a SuperPill
      else Cells[x,y].PillType := ptNone;  // walls and empty space, no points
    end;
  end;
end;

procedure TPacman.SetGhostScared(aScared: boolean);

  Procedure DoImg(Idx: Integer;aImg : TJSHTMLImageElement; aSpeed : Double);

  begin
    Sprite[Idx].spImg:=aImg;
    Sprite[Idx].Spd:=aSpeed;
  end;

var
  i : Integer;

begin
  if aScared then
    begin // assign "scared" images and set speed to scared
    for I:=1 to 4 do
      DoImg(i,ImgGhost[5],GhostSpeedScared);
    end
  else
    begin        // assign normal ghost images and set speed to normal
    For i:=1 to 4 do
      DoImg(I,ImgGhost[i],GhostSpeedNormal);
    end;
end;

//==============================================================================
//  User input code
//==============================================================================
// This is a very simple piece of code, the only function is FormKeyDown (which
// is an eventproperty of the form) which sets the direction Pacman should go.
// for now only 4 keys are valid, arrow up,down,left,right.

function TPacman.HandleKeyPress(k: TJSKeyBoardEvent): Boolean;

Var
  aCode : String;

begin
  Result:=True;
  if FDying then exit;
  aCode:=k.Key;
  if aCode='' then
    aCode:=K.Code;
  case aCode of
    // For some reason, it is opposite of what you'd expect...
    'Right',  TJSKeyNames.ArrowRight : PacManDir:='W';
    'Up', TJSKeyNames.ArrowUp    : PacManDir:='N';
    'Left', TJSKeyNames.ArrowLeft  : PacManDir:='E';
    'Down', TJSKeyNames.ArrowDown  : PacManDir:='S';
    'P', 'KeyP' : Pause:=not Pause;
  end;
  k.preventDefault;
end;

function TPacman.DoResetClick(aEvent: TJSMouseEvent): boolean;

begin
  Result:=True;
  FDying:=True;
  StopTimer;
  RestartGame();
end;

function TPacman.CheckSound(Event: TEventListenerEvent): boolean;

begin
  Result:=True;
  AudioDisabled:=Not FCBXSound.checked;
  if AudioDisabled then
    FAudio.DisableSound
  else If not FAudio.Loaded then
    begin
    FAudio.OnLoaded:=Nil;
    FAudio.LoadAudio;
    end;
end;

procedure TPacman.DoAudioLoaded(Sender: TObject);
begin
  Start;
end;

function TPacman.DoMouseClick(aEvent: TJSMouseEvent): boolean;

Const
    SControl = 'control-';

Var
  S : String;
begin
  Result:=true;
  //**S:=aEvent.currentTarget.ID;
  S:=aEvent.currentTargetElement.ID; //!![Kryvich]
  aEvent.preventDefault;
  if Copy(S,1,Length(SControl))=SControl then
    begin
    Delete(S,1,Length(sControl));
    Case S of
      'left'  : PacManDir:='E';
      'right' : PacManDir:='W';
      'down'  : PacManDir:='S';
      'up'    : PacManDir:='N';
      'pause' : Pause:=Not Pause;
    end;
    end;
end;

//==============================================================================
//  Business logic, rules of the game.
//==============================================================================
// The ghosts are aware of the position of pacman. Depending on their fear for
// him they try to get to him (Fear=-1) or to get away from him (Fear=1) or anything in
// between.
//
// Every once in a while a bonuscherry starts moving around for a some time.
// When Pacman eats the cherry the score is incremented and the cherry disappears.
// Whenever Pacman eats a small pill the score is incremented and the pill disappears
// Whenever Pacman eats a large pill the score is incremented, the pill diappears,
// and a timer is started that keeps the ghosts to a Fearlavel of 1 al long as the
// timer runs. after that the ghosts wil gradually return to fear=-1;
// When pacman eats a scared ghost the score is incremented and the ghost is sent
// back to his cave...
// When pacman eats a not so scared ghost he dies...
// In this case all ghosts are sent home, and if there are stil lives left the
// game continues with one life less...
// When Pacman runs out of lives the game is ended and a new game is started.
// If all pills are eaten the game is also ended and a new game is started.

//==============================================================================
// Business code: TestAndGet
//==============================================================================
// GetPossibleDir()
// GetGhostDir()
// GetPacmanDir()
// GetRandomCellAndDir()

function TPacman.GetPossibleDir(aXY: TPoint): TStr4;
begin
  result:='';    // Start with an empty string
  if Cells[aXY.X,aXY.Y-1].WallType=wtNone then result:=result+'N'; // up is possible
  if Cells[aXY.X-1,aXY.Y].WallType=wtNone then result:=result+'E'; // left is possible
  if Cells[aXY.X,aXY.Y+1].WallType=wtNone then result:=result+'S'; // down is possible
  if Cells[aXY.X+1,aXY.Y].WallType=wtNone then result:=result+'W'; // right is possible
end;

function TPacman.GetBestDir(aXY: TPoint): char;
begin
  result:='-';
  if SolveMaze(aXY,sprite[0].XY) then begin // fill the SearchIndexes cell[x,y].i
    if Cells[aXY.X,aXY.Y-1].I<-10 then result:='N'; // up    is best
    if Cells[aXY.X-1,aXY.Y].I<-10 then result:='E'; // left  is best
    if Cells[aXY.X,aXY.Y+1].I<-10 then result:='S'; // down  is best
    if Cells[aXY.X+1,aXY.Y].I<-10 then result:='W'; // right is best
  end;
end;

function TPacman.GetGhostDir(aXY: TPoint; aOldDir: char): char;
var BestDir:char; D:Char;s:TStr4;
begin
  result:='-';
  s:=GetPossibleDir(aXY);
  case aOldDir of // get the direction opposite of the current direction
    'W':D:='E'; 'E':D:='W'; 'S':D:='N'; 'N':D:='S'; else D:='-';
  end;
  if (length(s)>1) then begin // more than one direction: make a choice
    BestDir:=GetBestDir(aXY);
    if (scaretimer=0) and (BestDir<>'-') then begin//
      if random < Huntfactor then s:=BestDir; // hunt depends on factor
    end else begin
      delete(s,pos(BestDir,s),1);             // fleeing does not
    end;
  end;
  // if other than the reverse direction are possible then remove the reverse direction
  if (length(s)>1) and (pos(d,s)<>0) then delete(s,pos(d,s),1);
  if (length(s)=1) then result:=s[1];                   // only one direction possible: Go
  if (length(s)>1) then result:=s[1+random(length(s))]; // choose at random
end;

function TPacman.GetPacmanDir(aXY: TPoint; aOldDir: char): char;
var s:TStr4;
begin
  s:=GetPossibleDir(aXY);
  if pos(PacmanDir,s)>0 then s:=pacmandir else
  if pos(aOldDir,s)>0   then s:=aOldDir   else s:='-';
  result:=s[1];
end;

procedure TPacman.GetRandomCellAndDir(var aXY: TPoint; var aDir: char);
begin
  repeat
    aXY:=point(1+random(GridXSize-3),random(GridYSize-3));
  until (Cells[aXY.x,aXY.y].WallType=wtnone);
  aDir:=GetGhostDir(aXY,'-');
end;

procedure TPacman.StopTimer;
begin
  Window.clearInterval(SpriteTimer);
end;

procedure TPacman.MarkCellsDirty;

Var
  n,maxn,x,y,i,j : Integer;

begin
  maxn:=4;
  if BonusTimer>0 then
    inc(maxn);
  for n:=0 to 4 do
    begin
    X:=Sprite[n].XY.x;
    Y:=Sprite[n].XY.Y;
    for i:=-1 to 1 do
      for j:=-1 to 1 do
        Cells[X+i,Y+j].Dirty:=True;
    end;
end;

procedure TPacman.DoSpriteTimer;

var n:integer;

begin
  if Pause=false then
    begin
    MarkCellsDirty;
    DrawCells(True);
    for n:=0 to 4 do
      MoveSprite(n);       // for 'Pacman' and each 'Ghost'
    if DoBonusTimer() then
      MoveSprite(5); // update bonustimer plus cherry
    DoScareTimer();  // update the timer that controls scaring of the ghosts
    DrawPacman();    // the images have moved, update the pacmanface
    end;
end;

//==============================================================================
// Business code: Actions
//==============================================================================
// OnRestartMessage()
// EatPill()
// EatSuperPill()
// EatBonus()
// EatGhost()
// ClearCell()
// MoveSprite()
// DoBonusTimer()
// DoScareTimer()
// OnSpriteTimer()

function TPacman.DoRestartClick(aEvent: TJSMouseEvent): boolean;
begin
  RestartGame(); // start game after VCL is ready drawing the screen
end;

procedure TPacman.EatPill(aXY: TPoint);
begin
  inc(Score, 1);
  ClearCell(aXY);
  dec(PillsLeft);
  UpdateScore();
  playsound(aEatPill);
  if PillsLeft=0 then NextLevel();
end;

procedure TPacman.EatSuperPill(aXY: TPoint);
begin
  ClearCell(aXY);
  ScareTimer:=ScareTimeOut; // Make 'm scared for a while...
  inc(Score,10);
  playsound(aEatPill);
  UpdateScore();
  dec(PillsLeft); if PillsLeft=0 then NextLevel();
end;

procedure TPacman.EatBonus();
begin
  BonusTimer:=0;  // remove cherry
  inc(Score,50);
  inc(BonusCnt);
  UpdateScore();  // write scores to screen
end;

procedure TPacman.EatGhost(var aGhost: TSprite);
begin
  playsound(aEatGhost);
  aGhost.XY:=aGhost.StartPos; // send ghost home
  inc(Score,20);
  inc(GhostCnt);
  UpdateScore();              // write scores to screen
end;

procedure TPacman.ClearCell(aXY: TPoint);
var sx,sy:integer;
begin
  cells[aXY.X,aXY.Y].PillType:=ptNone; // clear cell in Cell[] array
  Fcanvas.FillStyle:=clBlack;     // also clear this part of the canvas
  sx:=aXY.x*CellSize;
  sy:=aXY.y*CellSize;
  FCanvas.fillrect(sx,sy,cellsize,cellsize);
end;

procedure TPacman.MoveSprite(aSpriteInx: integer);
var oXY:TPoint;
begin
  with Sprite[aSpriteInx] do begin
    // change position depending on direction
    oXY:=XY;
    case Dir of
      'N': begin Sy:=Sy-Spd; if Sy<=-1 then begin dec(XY.y); Sy:=Sy+1; end; end;
      'E': begin Sx:=Sx-Spd; if Sx<=-1 then begin dec(XY.x); Sx:=Sx+1; end; end;
      'S': begin Sy:=Sy+Spd; if Sy>= 1 then begin inc(XY.y); Sy:=Sy-1; end; end;
      'W': begin Sx:=Sx+Spd; if Sx>= 1 then begin inc(XY.x); Sx:=Sx-1; end; end;
    else
       begin
       oXY:=point(0,0);
       Sx:=0;Sy:=0;
       end;
    end;
    //if cell changed then choose new direction depending on wall limitations
    if (XY.x<>oXY.x) or (XY.y<>oXY.y) then
      begin
      if aSpriteInx=0  then
        dir:=GetPacmanDir(XY,dir)
      else
        dir:=GetGhostDir (XY,dir);
      if dir in ['E','W'] then  //correct partial displacements
        sy:=0
      else
        sx:=0;
      if aSpriteInx=0 then
        CollisionDetect(XY);  //only for The Man himself...
      end;
    // if position goes offgrid then reenter on the other side of the screen
    if XY.x>GridXSize-3 then XY.x:=2; if XY.x<2 then XY.x:=GridXSize-3;
    if XY.y>GridYSize-3 then XY.y:=2; if XY.y<2 then XY.y:=GridYSize-3;
    // set sprite image position according to new Cx:Sx,Cy,Sy
  // Pacman is drawn separately
  if aSpriteInx<>0 then
    FCanvas.drawImage(spimg,(XY.x+Sx+0.5)*CellSize-SpImg.Width/2,
                            (XY.y+Sy+0.5)*CellSize-SpImg.Height/2);
    // SpImg.Left := round();
    // SPImg.Top  := round((XY.y+Sy+0.5)*CellSize-SpImg.picture.Height/2);
  end;
end;

function TPacman.DoBonusTimer(): boolean;

Var
  S  : String;
  w : Integer;

begin
  if BonusTimer>=0 then begin // bonustimer is positive: cherry is onscreen
    dec(BonusTimer);
    if BonusTimer<=0 then begin // if decrement makes it negative then
      // sprite[5].SpImg.visible:=false; // remove cherry from screen, and
      BonusTimer:=-BonusTimeOut1-random(BonusTimeOut1); // set a negative timeout
    end;
  end else begin   // if bonus timer is negative then cherry is not onscreen
    inc(BonusTimer);
    if BonusTimer>=0 then begin        // when increment makes it positive then
      // sprite[5].SpImg.visible:=true;   // make cherry visible,
      // sprite[5].Sx:=0; sprite[5].Sy:=0;// set partial position to zero, and
      GetRandomCellAndDir(Sprite[5].XY,Sprite[5].Dir);// choose a random position
      BonusTimer:=+BonusTimeOut2+random(BonusTimeOut2); // Set a positive timeout
    end;
  end;
  // update a custom made progressbar on the screen
  S:='background-color: ';
  W:=bonustimer*Round(pnBonusBarOuter.clientWidth) div (2*BonusTimeOut2);
  if BonusTimer>0 then
    S:=S+clLime+'; width: '+IntToStr(W)+'px;'
  else
    S:=S+clRed+'; width: 0px;';
  pnbonusbarInner['style']:=S;
  result:=BonusTimer>0;
end;

procedure TPacman.DoScareTimer();

Var
  S: String;
  w : integer;
begin
  // just after superpill is eaten the caretimer is set to ScareTimeOut
  if scaretimer>=ScareTimeOut then SetGhostScared(true); //frighten them !!
  if ScareTimer>0 then begin
    dec(ScareTimer);
    // if scaretimer becomes zero then scare time is over: return to normal
    if scaretimer=0 then SetGhostScared(false); // fun is over...
    // update a custom made progressbar on the screen
    if   ScareTimer>ScareTimeOut div 5 then
      S:='background-color: '+clLime
    else
      S:='background-color: '+clRed;  // make bar red for last 20% of the time
    W:=ScareTimer*pnScareBarOuter.Clientwidth div ScareTimeOut;
    S:=S+'; width: '+IntToStr(w)+'px;';
    pnScareBarInner.Attrs['style']:=S;
  end;
end;

procedure TPacman.DrawScene;

Var
  I : Integer;

begin
  DrawCells();
  for I:=0 to 4 do
    MoveSprite(I); // For 'Pacman' and each 'Ghost'
  DrawPacMan;
end;


procedure TPacman.SetupPacman;

  Function GetElement(aName : String) : TJSHTMLElement;

  begin
    Result:=TJSHTMLElement(Document.getElementById(aName));
  end;

Var
  I : integer;
  El :  TJSElement;

begin
  if FCanvasID='' then
    FCanvasID:='my-canvas';
  if FResetID='' then
    FResetID:='btn-reset';
  FCanvasEl:=TJSHTMLCanvasElement(Document.getElementById(FCanvasID));
  FCanvas:=TJSCanvasRenderingContext2D(FCanvasEl.getContext('2d'));
  FBtnReset:=TJSHTMLButtonElement(Document.getElementById(FResetID));
  FCBXSound:=TJSHTMLInputElement(GetElement('cbx-sound'));
  FCBXSound.onchange:=@CheckSound;
  if Assigned(FBtnReset) then
    FBtnReset.OnClick:=@DoResetClick;
  FCanvasEl.width := Round(FCanvasEl.OffsetWidth);
  FCanvasEl.height := Round(FCanvasEl.OffsetHeight);
  for I:=1 to 4 do
    ImgGhost[i]:=TJSHTMLImageElement(GetElement('ghost'+IntToStr(i))) ;
  ImgGhost[5]:=TJSHTMLImageElement(GetElement('ghost-scared'));
  ImgBonus:=TJSHTMLImageElement(GetElement('cherry'));
  for I:=1 to ControlCount do
     begin
     El:=GetElement('control-'+ControlNames[i]);
     if Assigned(El) then
       TJSHTMLElement(El).onClick:=@DoMouseClick;
     end;
  pnBonusBarOuter:=GetElement('bonus-outer');
  pnBonusBarInner:= GetElement('bonus-inner');
  pnScareBarOuter:=GetElement('scare-outer');
  pnScareBarInner:=GetElement('scare-inner');
  lbScore:=GetElement('score');
  lbStatus:=GetElement('status');
  lbHiscore:=GetElement('highscore');
  lbLives:=GetElement('lives');
  lbBonusCnt:=GetElement('bonus');
  lbGhostCnt:=GetElement('ghosts');
  // Sprites need the images, so this can only be done in this part
  InitSprites();
  document.onkeydown:=@HandleKeyPress;
  if not AudioDisabled then
    InitAudio()
end;

procedure TPacman.InitAudio;

begin
  Faudio.LoadAudio;
end;

procedure TPacman.StartTimer;

begin
  FDying:=False;
  UpdateStatus('Playing');
  SpriteTimer:=window.setInterval(@DoSpriteTimer,TimerInterval);
end;

procedure TPacman.Start;

begin
  RestartGame;
end;


//==============================================================================
// Business code: Decisions
//==============================================================================
// CollisionDetect()
// RestartGame()
// RestartLevel()
// PacmanDies()
// NextLevel()
// GameOver()

procedure TPacman.CollisionDetect(var aXY: TPoint);
var n,ix,dX,dY:integer;
begin
  case cells[aXY.X,aXY.Y].PillType of
    ptPill      :EatPill(aXY);
    ptSuperPill :EatSuperPill(aXY);
  end;
  ix:=0; for n:=1 to 5 do begin
    dX:=sprite[n].XY.x-aXY.x;
    dY:=sprite[n].XY.y-aXY.y;
    if (abs(dX)<=1) and (abs(dY)<=1) then ix:=n;
  end;
  if (ix=5) and (BonusTimer>0) then EatBonus();
  if ix in [1..4] then begin
    if ScareTimer>0 then EatGhost(sprite[ix]) else PacmanDies();
  end;
end;

procedure TPacman.RestartGame();
begin
  InitVars(Level1Field);
  InitCells(Level1Field);
  RestartLevel();
  UpdateStatus('Playing');
end;

procedure TPacman.RestartLevel();
var n:integer;
begin
  for n:=0 to 4 do
    Sprite[n].XY:=Sprite[n].StartPos;
  UpdateScore();
  SetGhostScared(false);
  DrawScene;
  PacmanDir:='-';
  DrawPacman();                   // the images have moved, set the pacmanface
  PlaySound(aStart);
  ShowText('GET READY !!!',@StartTimer);
  PacmanDir:='-';
end;

procedure TPacman.CheckGameOver;

begin
  if LivesLeft<=0 then
    GameOver()
  else
    ReStartLevel();
end;

procedure TPacman.PacmanDies();
begin
//exit;
  if FDying then
    exit;
  FDying:=True;
  StopTimer;
  playsound(aDie);
  dec(LivesLeft);
  UpdateScore();
  PacmanDir:='-';
  UpdateStatus('You died');
  ShowText('YOU DIE !!!',@CheckGameOver);
end;

procedure TPacman.NextLevel();
begin
  StopTimer;
  ShowText('YOU WIN !!!',@RestartGame);
  UpdateStatus('You win');
end;

procedure TPacman.GameOver();
begin
  ShowText('YOU LOST !!!',@RestartGame);
  UpdateStatus('You lost');
end;

procedure TPacman.PlaySound(aAudio: TAudio);
begin
  if (not AudioDisabled) and (FAudio.Loaded) then
    FAudio.play(aAudio);
end;

//==============================================================================
// Maze solving
//==============================================================================
// Solving a maze is implemented here as a 3 step process.
// Step 1:
//   All accessible maze cells get an searchindex of 0, all blocked cells
//   (f.i. Walls) get an index of -1.
// Step 2:
//   Two arrays are used to keep track of a set of cells that are tested
//   This step begins with adding the first point to the primary array.
//   This now contains exactly one cell. Then a loop starts: for each cell in
//   the primary array the 4 surrounding cells are tested (left,right,up down)
//   If the index of such a cell is 0 then the cell is free and it is added to
//   a secondary array of cell coordinates. The searchindex of the cell is set
//   to a value that is one higher than the searchindex of original cell.
//   If the neighbour cells of all cells in the primary array are tested then
//   the secondary array is copied to the primary array and the secondary array
//   is cleared.
//   There are 2 reasons to end this loop:
//   1: The cell that was searched for is found
//   2: There are no more cells with a searchindex of 0, secondary array is empty
//   When this is all done the cells have a search index that increments as the
//   cell is further away from the originating point. Not all cells are tested.
//   When the loop finds the target in say 10 steps the testing stops, so no cell
//   will get an index higher than 10.
//   Imagine an octopus with growing tentacles that stops when the prey is found
// Step 3:
//   Now that the target is found we have to find "the tentacle that leads back
//   to the octopus", the shortest way back to the originating point.
//   This is done by starting at the endpoint, and looking in the surrounding
//   cells for a valid searchindex that is smaller  than the cells own searchindex.
//   Move the cellpointer to the adjacing cell with a smaller index and eventually
//   you get back to the source.
//   Imagine a river valley in which a lot of streams go down to the middle. Just
//   follow gravity and you will end up in the center.
//   On the way back the cells are marked, and that way you will have a set of
//   cells that give you the shortest route form A to B.
//
// For debugging the searchindexes are set to 10 and higher for the tested cells
// on routes without result, and -10 and lower for the tested cells that are part
// of the shortest route. SearchIndex = 10 or -10 is the startingpoint.
// Blocked cells are -1, Untested cells are 0.
// Cells with an index of -10 or less are the solution.
//
// For this game we are only interested in the first direction decision of a
// Ghost, so after step 1 to 3 we only look which cell in the adjacent cells of
// a Ghost is in the path, and send the Ghost that way (or opposite when it is
// scared).

function TPacman.SolveMaze(P1, P2: TPoint): boolean;
begin  // 3 step maze solving algorithm
                 result := SolveMazeStep1(P1,P2);  // step1
  if result then result := SolveMazeStep2(P1,P2);  // step2
  if result then result := SolveMazeStep3(P1,P2);  // step3
end;

function TPacman.SolveMazeStep1(P1, P2: TPoint): boolean;
var x,y:integer;
begin
  for x:=0 to GridXSize-1 do for y:=0 to GridYSize-1 do begin
    if   Cells[x,y].WallType=wtNone
    then Cells[x,y].I:=0   // these cells can be part of a route
    else Cells[x,y].I:=-1; // these cells can not...
  end;
  // no search is usefull if P1 or P1 is not a valid cell...
  result:= (cells[P1.x ,P1.y].I=0) and (cells[P2.x,P2.y].I=0)
end;

// In the procedure below a fixed size is used for SArr1 and SArr2.
// Of course it is much better to use a dynamic array that is never too small
// I tested the maximum number of alternative routes in this maze is 17, and the
// maximum number of searchloops is 54.
// To keep code as simple as possible the arraysizes are set to 64 (17 needed).
function TPacman.SolveMazeStep2(P1, P2: TPoint): boolean;
var SArr1,SArr2:array[0..63] of tpoint;
    SArr1Cnt,SArr2Cnt:integer;
    SI:integer; n:integer;
  procedure AddLS2(x,y:integer);
  begin
    if (x<0) or (x>=GridXSize) then exit;       // index offgrid: do nothing
    if (y<0) or (y>=GridYSize) then exit;       // index offgrid: do nothing
    if cells[x,y].i<>0         then exit;       // cell is blocked: do nothing
    cells[x,y].i:=SI;                           // cell is usable: give index
    SArr2[SArr2Cnt]:=point(x,y); inc(SArr2Cnt); // add cell to SArr2 for next run
    if (x=P2.x) and (y=P2.y) then Result:=true; // if endpoint is found then stop
  end;
begin
  SI:=10; Result:=false;    // start at 10 to have some special numbers to spare
  cells[p1.x,p1.y].i:=SI;   // for debugging, set the searchindex of first cell
  SArr1Cnt:=1; SArr1[0]:=P1;// prepare primary array with one (the first) cell
  repeat                    // now start searching for PacMan !!
    inc(SI);                // increment search index
    SArr2Cnt:=0;            // clear secondary array
    for n:=0 to SArr1Cnt-1 do begin // for all points in primary array do
      AddLS2(SArr1[n].x+1,SArr1[n].y  );// Test and maybe add cell to the right
      AddLS2(SArr1[n].x  ,SArr1[n].y+1);// Test and maybe add cell below
      AddLS2(SArr1[n].x-1,SArr1[n].y  );// Test and maybe add cell to the left
      AddLS2(SArr1[n].x  ,SArr1[n].y-1);// Test and maybe add cell above
    end;
    //now copy alle new searchpoints in SArr2 to sArr1, and set the number of points
    for n:=0 to SArr2Cnt-1 do SArr1[n]:=SArr2[n]; SArr1Cnt:=SArr2Cnt;
  until Result or (SArr2Cnt=0); // repeat until pacman is found or all cells tested
end;

function TPacman.SolveMazeStep3(P1, P2: TPoint): boolean;
var Rdy:boolean; dP:TPoint; I:integer;
  procedure Check(x,y:integer);
  var It:integer;
  begin
    if (x<0) or (x>=GridXSize) then exit;   // index offgrid: do nothing
    if (y<0) or (y>=GridYSize) then exit;   // index offgrid: do nothing
    It:=cells[x,y].I;               // make a long name short...
    if (It>0) and (It<I) then begin // if index is smaller than the last but >0
      I:=It;                        // then make I the smaller index
      dP:=point(x,y);               // and make the next cell the tested cell
    end;
  end;
begin
  repeat
    I:=cells[P2.x,P2.y].i;          // inx of current cell (P)
    dP:=P2;                         // make next p equal to current cell
    Check(P2.x+1,P2.y  );           // test right
    Check(P2.x-1,P2.y  );           // test left
    Check(P2.x  ,P2.y+1);           // test bottom
    Check(P2.x  ,P2.y-1);           // test top
    Rdy:=(dP.x=P2.x)and(dP.y=P2.y); // if dP still equal to P than search is over
    cells[p2.x,p2.y].i := -cells[p2.x,p2.y].i;// mark this cell as returnpath
    P2:=dP;                         // move current cell to the next one
  until Rdy;
  result:=(P2.x=P1.x)and(P2.y=P1.y);// what can possibly go wrong???
end;


procedure TPacmanAudio.AudioLoaded;

Var
  AllLoaded : Boolean;
  A : TAudio;

begin
  allLoaded:=True;
  For a in TAudio do
    AllLoaded:=AllLoaded and FilesOK[a];
  FLoaded:=allLoaded;
  if Assigned(FonLoaded) then
    FOnLoaded(Self);
end;

function TPacmanAudio.CheckEnd(Event: TEventListenerEvent): boolean;

var
  a : TAudio;

begin
  For a in TAudio do
    if (Files[a]=Event.target) then
      Playing[a]:=False;
end;

function TPacmanAudio.CheckplayOK (Event: TEventListenerEvent): boolean;

var
  a : TAudio;

begin
  For a in TAudio do
    if (Files[a]=Event.target) then
       begin
       Files[a].oncanplaythrough:=nil;
       FilesOK[a]:=True;
       AudioLoaded;
       end;
end;

procedure TPacmanAudio.LoadAudio;

var
  F : TJSHTMLAudioElement;
  A : TAudio;

begin
  for a in TAudio do
    begin
    F:=TJSHTMLAudioElement(Document.GetElementByID('audio-'+audionames[a]));
    Files[a]:=F;
    FilesOK[a]:=F.readyState>=3;
    if not FilesOK[a] then
      F.oncanplaythrough:=@CheckPlayOK;
    end;
  AudioLoaded;
end;

procedure TPacmanAudio.DisableSound;

var
  a : TAudio;

begin
  For a in TAudio do
    if Playing[a] then
      begin
      files[a].pause();
      files[a].currentTime := 0;
      end;
end;


procedure TPacmanAudio.play(aAudio: Taudio);

begin
  Writeln('Attempting to play:',AudioNames[aAudio]);

  if FilesOK[aAudio] then
    begin
    Playing[aAudio]:=true;
    Files[aAudio].play;
    Files[aAudio].onended:=@CheckEnd;
    end;
end;

procedure TPacmanAudio.Pause;

var
  a : TAudio;

begin
  For a in TAudio do
    if Playing[a] and not Files[a].paused then
      files[a].pause();
end;

procedure TPacmanAudio.Resume;

var
  a : TAudio;

begin
  For a in TAudio do
    if Playing[a] and Files[a].paused then
      files[a].play();
end;

end.
