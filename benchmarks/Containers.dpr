program Containers;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  Rapid.Generics in '..\Rapid.Generics.pas';

type
  T = Integer;

const
  ITEMS_COUNT = 1000000;

var
  ITEMS: array[0..ITEMS_COUNT - 1] of T;

procedure FillItems;
var
  i: Integer;
begin
  RandSeed := 0;

  for i := Low(ITEMS) to High(ITEMS) do
    ITEMS[i] := 1 + Random(ITEMS_COUNT - 1);

  for i := 1 to 20 do
    ITEMS[Random(ITEMS_COUNT)] := Default(T);
end;

type
  TTest = class
  public
    constructor Create; virtual; abstract;
    procedure FillContainer(const UseCapacity: Boolean); virtual; abstract;
    procedure Prepare; virtual; abstract;
    function Execute: T; virtual; abstract;

    class procedure Run(const IterationsCount: Integer);
  end;
  TTestClass = class of TTest;

  TSystemListTest = class(TTest)
  public
    List: Generics.Collections.TList<T>;

    constructor Create; override;
    destructor Destroy; override;
    procedure FillContainer(const UseCapacity: Boolean); override;
    procedure Prepare; override;
  end;

  TRapidListTest = class(TTest)
  public
    List: Rapid.Generics.TList<T>;

    constructor Create; override;
    destructor Destroy; override;
    procedure FillContainer(const UseCapacity: Boolean); override;
    procedure Prepare; override;
  end;

  TSystemStackTest = class(TTest)
  public
    Stack: Generics.Collections.TStack<T>;

    constructor Create; override;
    destructor Destroy; override;
    procedure FillContainer(const UseCapacity: Boolean); override;
    procedure Prepare; override;
  end;

  TRapidStackTest = class(TTest)
  public
    Stack: Rapid.Generics.TStack<T>;

    constructor Create; override;
    destructor Destroy; override;
    procedure FillContainer(const UseCapacity: Boolean); override;
    procedure Prepare; override;
  end;

  TSystemQueueTest = class(TTest)
  public
    Queue: Generics.Collections.TQueue<T>;

    constructor Create; override;
    destructor Destroy; override;
    procedure FillContainer(const UseCapacity: Boolean); override;
    procedure Prepare; override;
  end;

  TRapidQueueTest = class(TTest)
  public
    Queue: Rapid.Generics.TQueue<T>;

    constructor Create; override;
    destructor Destroy; override;
    procedure FillContainer(const UseCapacity: Boolean); override;
    procedure Prepare; override;
  end;

  SystemListAdd = class(TSystemListTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

  RapidListAdd = class(TRapidListTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

  SystemListAdd_Capacity = class(TSystemListTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

  RapidListAdd_Capacity = class(TRapidListTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

  SystemListItems = class(TSystemListTest)
    function Execute: T; override;
  end;

  RapidListItems = class(TRapidListTest)
    function Execute: T; override;
  end;

  SystemListDelete = class(TSystemListTest)
    function Execute: T; override;
  end;

  RapidListDelete = class(TRapidListTest)
    function Execute: T; override;
  end;

  SystemListIndexOf = class(TSystemListTest)
    function Execute: T; override;
  end;

  RapidListIndexOf = class(TRapidListTest)
    function Execute: T; override;
  end;

  SystemListReverse = class(TSystemListTest)
    constructor Create; override;
    procedure Prepare; override;
    function Execute: T; override;
  end;

  RapidListReverse = class(TRapidListTest)
    constructor Create; override;
    procedure Prepare; override;
    function Execute: T; override;
  end;

  {$if CompilerVersion >= 23}
  SystemListPack = class(TSystemListTest)
    function Execute: T; override;
  end;
  {$ifend}

  RapidListPack = class(TRapidListTest)
    function Execute: T; override;
  end;

  SystemStackPush = class(TSystemStackTest)
    function Execute: T; override;
  end;

  RapidStackPush = class(TRapidStackTest)
    function Execute: T; override;
  end;

  SystemStackPush_Capacity = class(TSystemStackTest)
    function Execute: T; override;
  end;

  RapidStackPush_Capacity = class(TRapidStackTest)
    function Execute: T; override;
  end;

  SystemStackPop = class(TSystemStackTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

  RapidStackPop = class(TRapidStackTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

  SystemQueueEnqueue = class(TSystemQueueTest)
    function Execute: T; override;
  end;

  RapidQueueEnqueue = class(TRapidQueueTest)
    function Execute: T; override;
  end;

  SystemQueueEnqueue_Capacity = class(TSystemQueueTest)
    function Execute: T; override;
  end;

  RapidQueueEnqueue_Capacity = class(TRapidQueueTest)
    function Execute: T; override;
  end;

  SystemQueueDequeue = class(TSystemQueueTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

  RapidQueueDequeue = class(TRapidQueueTest)
    procedure Prepare; override;
    function Execute: T; override;
  end;

procedure Run(const SystemTest, RapidTest: TTestClass; const IterationsCount: Integer);
begin
  SystemTest.Run(IterationsCount);
  RapidTest.Run(IterationsCount);
end;


{ TTest }

class procedure TTest.Run(const IterationsCount: Integer);
var
  i: Integer;
  TotalTime, Time: Cardinal;
  Instance: TTest;
begin
  Write(Self.ClassName, '... ');

  TotalTime := 0;
  Instance := Self.Create;
  try
    for i := 1 to IterationsCount do
    begin
      Instance.Prepare;

      Time := GetTickCount;
      Instance.Execute;
      Time := GetTickCount - Time;

      Inc(TotalTime, Time);
    end;
  finally
    Instance.Free;
  end;

  Writeln(TotalTime, 'ms');
end;

{ TSystemListTest }

constructor TSystemListTest.Create;
begin
  List := Generics.Collections.TList<T>.Create;
end;

destructor TSystemListTest.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TSystemListTest.FillContainer(const UseCapacity: Boolean);
var
  i: Integer;
begin
  List.Clear;
  if (UseCapacity) then List.Capacity := ITEMS_COUNT;

  for i := 0 to ITEMS_COUNT - 1 do
    List.Add(ITEMS[i]);
end;

procedure TSystemListTest.Prepare;
begin
  FillContainer(True);
end;

{ TRapidListTest }

constructor TRapidListTest.Create;
begin
  List := Rapid.Generics.TList<T>.Create;
end;

destructor TRapidListTest.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TRapidListTest.FillContainer(const UseCapacity: Boolean);
var
  i: Integer;
begin
  List.Clear;
  if (UseCapacity) then List.Capacity := ITEMS_COUNT;

  for i := 0 to ITEMS_COUNT - 1 do
    List.Add(ITEMS[i]);
end;

procedure TRapidListTest.Prepare;
begin
  FillContainer(True);
end;

{ TSystemStackTest }

constructor TSystemStackTest.Create;
begin
  Stack := Generics.Collections.TStack<T>.Create;
end;

destructor TSystemStackTest.Destroy;
begin
  Stack.Free;
  inherited;
end;

procedure TSystemStackTest.FillContainer(const UseCapacity: Boolean);
var
  i: Integer;
begin
  Stack.Clear;
  {$if CompilerVersion >= 22}
  if (UseCapacity) then Stack.Capacity := ITEMS_COUNT;
  {$ifend}

  for i := 0 to ITEMS_COUNT - 1 do
    Stack.Push(ITEMS[i]);
end;

procedure TSystemStackTest.Prepare;
begin
  Stack.Clear;
end;

{ TRapidStackTest }

constructor TRapidStackTest.Create;
begin
  Stack := Rapid.Generics.TStack<T>.Create;
end;

destructor TRapidStackTest.Destroy;
begin
  Stack.Free;
  inherited;
end;

procedure TRapidStackTest.FillContainer(const UseCapacity: Boolean);
var
  i: Integer;
begin
  Stack.Clear;
  {$if CompilerVersion >= 22}
  if (UseCapacity) then Stack.Capacity := ITEMS_COUNT;
  {$ifend}

  for i := 0 to ITEMS_COUNT - 1 do
    Stack.Push(ITEMS[i]);
end;

procedure TRapidStackTest.Prepare;
begin
  Stack.Clear;
end;

{ TSystemQueueTest }

constructor TSystemQueueTest.Create;
begin
  Queue := Generics.Collections.TQueue<T>.Create;
end;

destructor TSystemQueueTest.Destroy;
begin
  Queue.Free;
  inherited;
end;

procedure TSystemQueueTest.FillContainer(const UseCapacity: Boolean);
var
  i: Integer;
begin
  Queue.Clear;
  {$if CompilerVersion >= 22}
  if (UseCapacity) then Queue.Capacity := ITEMS_COUNT;
  {$ifend}

  for i := 0 to ITEMS_COUNT - 1 do
    Queue.Enqueue(ITEMS[i]);
end;

procedure TSystemQueueTest.Prepare;
begin
  Queue.Clear;
end;

{ TRapidQueueTest }

constructor TRapidQueueTest.Create;
begin
  Queue := Rapid.Generics.TQueue<T>.Create;
end;

destructor TRapidQueueTest.Destroy;
begin
  Queue.Free;
  inherited;
end;

procedure TRapidQueueTest.FillContainer(const UseCapacity: Boolean);
var
  i: Integer;
begin
  Queue.Clear;
  if (UseCapacity) then Queue.Capacity := ITEMS_COUNT;

  for i := 0 to ITEMS_COUNT - 1 do
    Queue.Enqueue(ITEMS[i]);
end;

procedure TRapidQueueTest.Prepare;
begin
  Queue.Clear;
end;

{ SystemListAdd }

procedure SystemListAdd.Prepare;
begin
  List.Clear;
end;

function SystemListAdd.Execute: T;
begin
  FillContainer(False);
  Result := Default(T);
end;

{ RapidListAdd }

procedure RapidListAdd.Prepare;
begin
  List.Clear;
end;

function RapidListAdd.Execute: T;
begin
  FillContainer(False);
  Result := Default(T);
end;

{ SystemListAdd_Capacity }

procedure SystemListAdd_Capacity.Prepare;
begin
  List.Clear;
end;

function SystemListAdd_Capacity.Execute: T;
begin
  FillContainer(True);
  Result := Default(T);
end;

{ RapidListAdd_Capacity }

procedure RapidListAdd_Capacity.Prepare;
begin
  List.Clear;
end;

function RapidListAdd_Capacity.Execute: T;
begin
  FillContainer(True);
  Result := Default(T);
end;

{ SystemListItems }

function SystemListItems.Execute: T;
var
  i: Integer;
begin
  for i := 0 to ITEMS_COUNT - 1 do
    Result := List[i];
end;

{ RapidListItems }

function RapidListItems.Execute: T;
var
  i: Integer;
begin
  for i := 0 to ITEMS_COUNT - 1 do
    Result := List[i];
end;

{ SystemListDelete }

function SystemListDelete.Execute: T;
var
  i: Integer;
begin
  for i := ITEMS_COUNT - 1 downto 0 do
    List.Delete(i);

  Result := Default(T);
end;

{ RapidListDelete }

function RapidListDelete.Execute: T;
var
  i: Integer;
begin
  for i := ITEMS_COUNT - 1 downto 0 do
    List.Delete(i);

  Result := Default(T);
end;

{ SystemListIndexOf }

function SystemListIndexOf.Execute: T;
begin
  Result := List.IndexOf(Low(T))
end;

{ RapidListIndexOf }

function RapidListIndexOf.Execute: T;
begin
  Result := List.IndexOf(Low(T))
end;

{ SystemListReverse }

constructor SystemListReverse.Create;
begin
  inherited;
  FillContainer(True);
end;

procedure SystemListReverse.Prepare;
begin
end;

function SystemListReverse.Execute: T;
begin
  List.Reverse;
  Result := Default(T);
end;

{ RapidListReverse }

constructor RapidListReverse.Create;
begin
  inherited;
  FillContainer(True);
end;

procedure RapidListReverse.Prepare;
begin
end;

function RapidListReverse.Execute: T;
begin
  List.Reverse;
  Result := Default(T);
end;

{ SystemListPack }
{$if CompilerVersion >= 23}
function SystemListPack.Execute: T;
begin
  List.Pack;
  Result := Default(T);
end;
{$ifend}

{ RapidListPack }

function RapidListPack.Execute: T;
begin
  List.Pack;
  Result := Default(T);
end;

{ SystemStackPush }

function SystemStackPush.Execute: T;
begin
  FillContainer(False);
  Result := Default(T);
end;

{ RapidStackPush }

function RapidStackPush.Execute: T;
begin
  FillContainer(False);
  Result := Default(T);
end;

{ SystemStackPush_Capacity }

function SystemStackPush_Capacity.Execute: T;
begin
  FillContainer(True);
  Result := Default(T);
end;

{ RapidStackPush_Capacity }

function RapidStackPush_Capacity.Execute: T;
begin
  FillContainer(True);
  Result := Default(T);
end;

{ SystemStackPop }

procedure SystemStackPop.Prepare;
begin
  FillContainer(True);
end;

function SystemStackPop.Execute: T;
var
  i: Integer;
begin
  for i := ITEMS_COUNT - 1 downto 0 do
    Result := Stack.Pop;
end;

{ RapidStackPop }

procedure RapidStackPop.Prepare;
begin
  FillContainer(True);
end;

function RapidStackPop.Execute: T;
var
  i: Integer;
begin
  for i := ITEMS_COUNT - 1 downto 0 do
    Result := Stack.Pop;
end;

{ SystemQueueEnqueue }

function SystemQueueEnqueue.Execute: T;
begin
  FillContainer(False);
  Result := Default(T);
end;

{ RapidQueueEnqueue }

function RapidQueueEnqueue.Execute: T;
begin
  FillContainer(False);
  Result := Default(T);
end;

{ SystemQueueEnqueue_Capacity }

function SystemQueueEnqueue_Capacity.Execute: T;
begin
  FillContainer(True);
  Result := Default(T);
end;

{ RapidQueueEnqueue_Capacity }

function RapidQueueEnqueue_Capacity.Execute: T;
begin
  FillContainer(True);
  Result := Default(T);
end;

{ SystemQueueDequeue }

procedure SystemQueueDequeue.Prepare;
begin
  FillContainer(True);
end;

function SystemQueueDequeue.Execute: T;
var
  i: Integer;
begin
  for i := ITEMS_COUNT - 1 downto 0 do
    Result := Queue.Dequeue;
end;

{ RapidQueueDequeue }

procedure RapidQueueDequeue.Prepare;
begin
  FillContainer(True);
end;

function RapidQueueDequeue.Execute: T;
var
  i: Integer;
begin
  for i := ITEMS_COUNT - 1 downto 0 do
    Result := Queue.Dequeue;
end;

begin
  try
    {$if CompilerVersion < 28}
      Writeln('Attention!');
      Writeln('The library is much faster in XE7+ versions.');
      Writeln;
    {$ifend}

    FillItems;

    Run(SystemListAdd, RapidListAdd, 300);
    Run(SystemListAdd_Capacity, RapidListAdd_Capacity, 300);
    Run(SystemListItems, RapidListItems, 1000);
    Run(SystemListDelete, RapidListDelete, 500);
    Run(SystemListIndexOf, RapidListIndexOf, 1000);
    Run(SystemListReverse, RapidListReverse, 8000);
    {$if CompilerVersion >= 23}
    Run(SystemListPack, RapidListPack, 500);
    {$ifend}
    Run(SystemStackPush, RapidStackPush, 300);
    Run(SystemStackPush_Capacity, RapidStackPush_Capacity, 300);
    Run(SystemStackPop, RapidStackPop, 500);
    Run(SystemQueueEnqueue, RapidQueueEnqueue, 300);
    Run(SystemQueueEnqueue_Capacity, RapidQueueEnqueue_Capacity, 300);
    Run(SystemQueueDequeue, RapidQueueDequeue, 500);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  if (ParamStr(1) <> '-nowait') then
  begin
    Writeln;
    Write('Press Enter to quit');
    Readln;
  end;
end.
