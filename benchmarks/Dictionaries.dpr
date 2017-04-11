program Dictionaries;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  Rapid.Generics in '..\Rapid.Generics.pas';

const
  ITEMS_COUNT = 1024 * (1024 div 4 * 3);

type
  TRunner<T> = class
  public type
    TItems = array[0..ITEMS_COUNT - 1] of T;
    TRandomFunc = reference to function: T;

    TTest = class
      constructor Create(const Items: TItems; const Capacity: Integer); virtual; abstract;
      function ExecuteItems(const Items: TItems): Integer; virtual; abstract;
    end;
    TTestClass = class of TTest;
  private const
    ITERATIONS_COUNT = 10;
    MODES: array[0..2] of string = ('Add', 'Add+Capacity', 'Items');
    CAPACITIES: array[0..2] of Integer = (0, ITEMS_COUNT, ITEMS_COUNT);
  public
    Items: TItems;
    constructor Create(const RandomFunc: TRandomFunc);

    procedure Run(const TestClass: TTestClass);
    procedure RunEach;
  public type
    SystemSystem = class(TTest)
      Dictionary: Generics.Collections.TDictionary<T,Integer>;

      constructor Create(const Items: TItems; const Capacity: Integer); override;
      destructor Destroy; override;
      function ExecuteItems(const Items: TItems): Integer; override;
    end;

    SystemRapid = class(SystemSystem)
      constructor Create(const Items: TItems; const Capacity: Integer); override;
    end;

    RapidRapid = class(TTest)
      Dictionary: Rapid.Generics.TDictionary<T,Integer>;

      constructor Create(const Items: TItems; const Capacity: Integer); override;
      destructor Destroy; override;
      function ExecuteItems(const Items: TItems): Integer; override;
    end;

    RapidDictionary = class(TTest)
      Dictionary: TRapidDictionary<T,Integer>;

      constructor Create(const Items: TItems; const Capacity: Integer); override;
      destructor Destroy; override;
      function ExecuteItems(const Items: TItems): Integer; override;
    end;
  end;


{ TRunner<T>.SystemSystem }

constructor TRunner<T>.SystemSystem.Create(const Items: TItems; const Capacity: Integer);
var
  i: Integer;
begin
  Dictionary := Generics.Collections.TDictionary<T,Integer>.Create(Capacity);
  for i := Low(TItems) to High(TItems) do
    Dictionary.AddOrSetValue(Items[i], i);
end;

destructor TRunner<T>.SystemSystem.Destroy;
begin
  Dictionary.Free;
  inherited;
end;

function TRunner<T>.SystemSystem.ExecuteItems(const Items: TItems): Integer;
var
  i: Integer;
begin
  for i := Low(TItems) to High(TItems) do
    Result := Dictionary.Items[Items[i]];
end;

{ TRunner<T>.SystemRapid }

constructor TRunner<T>.SystemRapid.Create(const Items: TItems; const Capacity: Integer);
var
  i: Integer;
  Comparer: Generics.Defaults.IEqualityComparer<T>;
begin
  IInterface(Comparer) := Rapid.Generics.TEqualityComparer<T>.Default;
  Dictionary := Generics.Collections.TDictionary<T,Integer>.Create(Capacity, Comparer);

  for i := Low(TItems) to High(TItems) do
    Dictionary.AddOrSetValue(Items[i], i);
end;

{ TRunner<T>.RapidRapid }

constructor TRunner<T>.RapidRapid.Create(const Items: TItems; const Capacity: Integer);
var
  i: Integer;
begin
  Dictionary := Rapid.Generics.TDictionary<T,Integer>.Create(Capacity);
  for i := Low(TItems) to High(TItems) do
    Dictionary.AddOrSetValue(Items[i], i);
end;

destructor TRunner<T>.RapidRapid.Destroy;
begin
  Dictionary.Free;
  inherited;
end;

function TRunner<T>.RapidRapid.ExecuteItems(const Items: TItems): Integer;
var
  i: Integer;
begin
  for i := Low(TItems) to High(TItems) do
    Result := Dictionary.Items[Items[i]];
end;

{ TRunner<T>.RapidDictionary }

constructor TRunner<T>.RapidDictionary.Create(const Items: TItems; const Capacity: Integer);
var
  i: Integer;
begin
  Dictionary := TRapidDictionary<T,Integer>.Create(Capacity);
  for i := Low(TItems) to High(TItems) do
    Dictionary.AddOrSetValue(Items[i], i);
end;

destructor TRunner<T>.RapidDictionary.Destroy;
begin
  Dictionary.Free;
  inherited;
end;

function TRunner<T>.RapidDictionary.ExecuteItems(const Items: TItems): Integer;
var
  i: Integer;
begin
  for i := Low(TItems) to High(TItems) do
    Result := Dictionary.Items[Items[i]];
end;


{ TRunner<T> }

constructor TRunner<T>.Create(const RandomFunc: TRandomFunc);
var
  i: Integer;
begin
  for i := Low(TItems) to High(TItems) do
    Items[i] := RandomFunc;
end;

procedure TRunner<T>.Run(const TestClass: TTestClass);
var
  i: Integer;
  Mode: Integer;
  S: string;
  Instance: TTest;
  TotalTime, Time: Cardinal;
begin
  for Mode := Low(MODES) to High(MODES) do
  begin
    S := MODES[Mode];
    Write(TestClass.ClassName, ' ', S, '... ');

    if (Mode <> 2) then
    begin
      TotalTime := 0;
      for i := 1 to ITERATIONS_COUNT do
      begin
        Time := GetTickCount;
        Instance := TestClass.Create(Items, CAPACITIES[Mode]);
        Time := GetTickCount - Time;
        Inc(TotalTime, Time);

        Instance.Free;
      end;
    end else
    // Mode = 2
    begin
      Instance := TestClass.Create(Items, CAPACITIES[Mode]);

      TotalTime := GetTickCount;
      for i := 1 to ITERATIONS_COUNT do
      begin
        Instance.ExecuteItems(Items);
      end;
      TotalTime := GetTickCount - TotalTime;

      Instance.Free;
    end;

    Writeln(TotalTime, 'ms');
  end;
end;

procedure TRunner<T>.RunEach;
begin
  Writeln;
  Writeln(PShortString(NativeUInt(TypeInfo(T)) + 1)^);

  Run(SystemSystem);
  Run(SystemRapid);
  Run(RapidRapid);
  Run(RapidDictionary);
end;

begin
  try
    {$if CompilerVersion < 28}
      Writeln('Attention!');
      Writeln('The library is much faster in XE7+ versions.');
      Writeln;
    {$ifend}

    with TRunner<string>.Create(
      function: string
      var
        Len, i: Integer;
      begin
        Len := 5 + Random(8);
        SetLength(Result, Len);

        for i := 1 to Len do
          Result[i] := Char(Ord('A') + Random(Ord('Z') - Ord('A') + 1));
      end) do
    try
      RunEach;
    finally
      Free;
    end;

    with TRunner<Single>.Create(
      function: Single
      begin
        Result := Random * ITEMS_COUNT;
      end) do
    try
      RunEach;
    finally
      Free;
    end;

    with TRunner<Integer>.Create(
      function: Integer
      begin
        Result := Random(ITEMS_COUNT);
      end) do
    try
      RunEach;
    finally
      Free;
    end;
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
