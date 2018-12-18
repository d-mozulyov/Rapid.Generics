unit Rapid.Generics;

{******************************************************************************}
{ Copyright (c) 2018 Dmitry Mozulyov                                           }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    }
{ THE SOFTWARE.                                                                }
{                                                                              }
{ email: softforyou@inbox.ru                                                   }
{ skype: dimandevil                                                            }
{******************************************************************************}

// compiler directives
{$ifdef FPC}
  {$MESSAGE ERROR 'FreePascal not supported'}
  {$mode delphi}
  {$asmmode intel}
  {$define INLINESUPPORT}
  {$define INLINESUPPORTSIMPLE}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}
  {$if Defined(CPUARM) or Defined(UNIX)}
    {$define POSIX}
  {$ifend}
{$else}
  {$if CompilerVersion >= 24}
    {$LEGACYIFEND ON}
  {$ifend}
  {$if CompilerVersion < 21}
    {$MESSAGE ERROR 'Only 2010+ compiler versions supported'}
  {$ifend}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$if CompilerVersion < 23}
    {$define CPUX86}
  {$ifend}
  {$if CompilerVersion >= 23}
    {$define UNITSCOPENAMES}
    {$define MONITORSUPPORT}
  {$ifend}
  {$if CompilerVersion >= 21}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$ifend}
  {$if CompilerVersion >= 28}
    {$define SMARTGENERICS}
  {$ifend}
{$endif}
{$POINTERMATH ON}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$ifdef CPUX86}
  {$ifNdef NEXTGEN}
    {$define CPUX86ASM}
    {$define CPUINTELASM}
  {$endif}
  {$define CPUINTEL}
{$endif}
{$ifdef CPUX64}
  {$ifNdef NEXTGEN}
    {$define CPUX64ASM}
    {$define CPUINTELASM}
  {$endif}
  {$define CPUINTEL}
{$endif}
{$if Defined(CPUX64) or Defined(CPUARM64)}
  {$define LARGEINT}
{$ifend}
{$if (not Defined(CPUX64)) and (not Defined(CPUARM64))}
  {$define SMALLINT}
{$ifend}
(*$HPPEMIT '#pragma option -w-8022'*)

interface
  uses {$ifdef MSWINDOWS}
         {$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif},
       {$else .POSIX}
         Posix.Time, Posix.Sched, System.TimeSpan, System.DateUtils,
       {$endif}
       {$ifdef USE_LIBICU}
         System.Internal.ICU,
       {$endif}
       {$ifdef MACOS}
         Posix.Langinfo, Posix.Locale, Posix.String_,
       {$endif}
       {$ifdef UNITSCOPENAMES}
         System.Types, System.SysUtils, System.TypInfo, System.Variants,
         System.SysConst, System.RTLConsts, System.Math
       {$else}
         Types, SysUtils, TypInfo, Variants, SysConst, RTLConsts, Math
       {$endif};

type
  {$if CompilerVersion = 21}
    PNativeInt = ^NativeInt;
    PNativeUInt = ^NativeUInt;
  {$ifend}
  {$if CompilerVersion <= 23}
    TDirection = (FromBeginning, FromEnd);
    TDuplicates = (dupIgnore, dupAccept, dupError);
  {$ifend}
  {$if CompilerVersion <= 27}
    TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion);
  {$ifend}
  {$if CompilerVersion <= 24}
    EObjectDisposed = class(Exception);
  {$ifend}
  {$if CompilerVersion <= 23}
    EListError = class(Exception);
  {$ifend}


{ TNothing record
  Dummy null size structure }

  TNothing = packed record
  end;


{ TRecord record
  Universal structure }

  TRecord<T1,T2,T3,T4> = packed record
    Field1: T1;
    Field2: T2;
    Field3: T3;
    Field4: T4;
  end;

  TRecord<T1,T2,T3> = packed record
    Field1: T1;
    Field2: T2;
    Field3: T3;
  end;

  TRecord<T1,T2> = packed record
    Field1: T1;
    Field2: T2;
  end;


{ TProcedure/TFunction
  Universal references }

  TProcedure = reference to procedure;
  TProcedure<T> = reference to procedure (const Arg1: T);
  TProcedure<T1,T2> = reference to procedure (const Arg1: T1; const Arg2: T2);
  TProcedure<T1,T2,T3> = reference to procedure (const Arg1: T1; const Arg2: T2; const Arg3: T3);
  TProcedure<T1,T2,T3,T4> = reference to procedure (const Arg1: T1; const Arg2: T2; const Arg3: T3; const Arg4: T4);
  TFunction<TResult> = reference to function: TResult;
  TFunction<T,TResult> = reference to function (const Arg1: T): TResult;
  TFunction<T1,T2,TResult> = reference to function (const Arg1: T1;const  Arg2: T2): TResult;
  TFunction<T1,T2,T3,TResult> = reference to function (const Arg1: T1; const Arg2: T2; const Arg3: T3): TResult;
  TFunction<T1,T2,T3,T4,TResult> = reference to function (const Arg1: T1; const Arg2: T2; const Arg3: T3; const Arg4: T4): TResult;


{ TOSTime record
  Extremely fast UTC-based system timer (Windows FILETIME format)
  Contains 64-bit value representing the number of 100-nanosecond intervals since January 1, 1601 }

  TOSTime = record
  private
    class var
      FLOCAL_DELTA: Int64;
    {$ifdef POSIX}
      FCLOCK_REALTIME_DELTA: Int64;
      FCLOCK_REALTIME_LOCAL_DELTA: Int64;
    const
      CLOCK_REALTIME_COARSE = 5;
      CLOCK_MONOTONIC_COARSE = 6;
    class function InternalClockGetTime(const ClockId: Integer): Int64; static; inline;
    {$endif}
  public
    const
      MICROSECOND = Int64(10);
      MILLISECOND = MICROSECOND * 1000;
      SECOND = MILLISECOND * 1000;
      MINUT = SECOND * 60;
      HOUR = MINUT * 60;
      DAY = HOUR * 24;
      DATETIME_DELTA = -109205;

    class property LOCAL_DELTA: Int64 read FLOCAL_DELTA;
  private
    class procedure Initialize; static;
    class function GetNow: Int64; static;
    class function GetUTCNow: Int64; static;
    class function GetTickCount: Cardinal; static; {$ifdef MSWINDOWS}inline;{$endif}
  public
    class function ToDateTime(const ATimeStamp: Int64): TDateTime; static;
    class function ToString(const ATimeStamp: Int64): string; static;
    class property TickCount: Cardinal read GetTickCount;
    class property Now: Int64 read GetNow;
    class property UTCNow: Int64 read GetUTCNow;
  end;


{ TCustomObject/ICustomObject class
  TInterfacedObject alternative (inheritor) and own interface, the differences:
   - contains an original object instance
   - optimized initialize, cleanup and atomic operations
   - NEXTGEN-like rule of DisposeOf method, i.e. allows to call destructor before reference count set to zero
   - data in inherited classes is 8 byte aligned (this can be useful for lock-free algorithms)
   - allows to be placed not in memory heap
   - allows to make TMonitor operations faster
   - incompatible with TInterfacedObject.RefCount property }

  TMemoryScheme = (msHeap, msAllocator, msFreeList, msUnknownBuffer);
  PMemoryScheme = ^TMemoryScheme;

  TCustomObject = class;
  ICustomObject = interface
    function GetSelf: TCustomObject {$ifdef AUTOREFCOUNT}unsafe{$endif};
    function GetMemoryScheme: TMemoryScheme;
    function GetDisposed: Boolean;
    function GetRefCount: Integer;
    procedure DisposeOf;
    {$ifdef MONITORSUPPORT}
    procedure OptimizeMonitor;
    {$endif}
    property Self: TCustomObject read GetSelf;
    property MemoryScheme: TMemoryScheme read GetMemoryScheme;
    property Disposed: Boolean read GetDisposed;
    property RefCount: Integer read GetRefCount;
  end;

  TCustomObject = class(TInterfacedObject, ICustomObject)
  protected
    const
      DISPOSED_FLAG = Integer($40000000);
      MEMORY_SCHEME_SHIFT = 28;
      MEMORY_SCHEME_MASK = Integer(High(TMemoryScheme)) shl MEMORY_SCHEME_SHIFT;
      MEMORY_SCHEME_CLEAR = not MEMORY_SCHEME_MASK;
      REFCOUNT_MASK = not (MEMORY_SCHEME_MASK or DISPOSED_FLAG);
      DEFAULT_REFCOUNT = {$ifdef AUTOREFCOUNT}1{$else}0{$endif};
      DUMMY_REFCOUNT = Integer($80000000);
      {$if CompilerVersion >= 32}
      monFlagsMask = NativeInt($01);
      monMonitorMask = not monFlagsMask;
      monWeakReferencedFlag = NativeInt($01);
      {$ifend}
      {$ifNdef AUTOREFCOUNT}
      vmtObjAddRef = SizeOf(Pointer);
      vmtObjRelease = vmtObjAddRef + SizeOf(Pointer);
      {$endif}
    type
      IInterfaceTable = array[0..2] of Pointer;
      ICustomObjectTable = array[0..6 {$ifdef MONITORSUPPORT}+ 1{$endif}] of Pointer;
    class var
      FInterfaceTable: IInterfaceTable;

    class procedure CreateIntfTables; static;
    class function IntfQueryInterface(const Self: PByte; const IID: TGUID; out Obj): HResult; stdcall; static;
    class function IntfAddRef(const Self: PByte): Integer; stdcall; static;
    class function IntfRelease(const Self: PByte): Integer; stdcall; static;
  protected
    function GetSelf: TCustomObject {$ifdef AUTOREFCOUNT}unsafe{$endif};
    function GetRefCount: Integer; inline;
    function ICustomObject.QueryInterface = QueryInterface;
    function ICustomObject._AddRef = _AddRef;
    function ICustomObject._Release = _Release;

    class function CreateEObjectDisposed: EObjectDisposed; static;
    class function CreateEInvalidRefCount(const AObject: TObject; const ARefCount: Integer): EInvalidContainer; static;
    function GetMemoryScheme: TMemoryScheme; inline;
    function GetDisposed: Boolean; inline;
    procedure CheckDisposed; inline;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall; inline;
    function _AddRef: Integer; stdcall; inline;
    function _Release: Integer; stdcall; inline;
  public
    class function NewInstance: TObject; override;
    class function PreallocatedInstance(const AMemory: Pointer; const AMemoryScheme: TMemoryScheme): TObject {$ifdef AUTOREFCOUNT}unsafe{$endif}; virtual;
    procedure FreeInstance; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    {$ifNdef AUTOREFCOUNT}
    procedure Free; reintroduce; inline;
    {$endif}
    procedure DisposeOf; {$if CompilerVersion >= 25}reintroduce;{$ifend}
    function __ObjAddRef: Integer; {$ifdef AUTOREFCOUNT}override{$else}virtual{$endif};
    function __ObjRelease: Integer; {$ifdef AUTOREFCOUNT}override{$else}virtual{$endif};
    {$ifdef MONITORSUPPORT}
    procedure OptimizeMonitor;
    {$endif}
    property MemoryScheme: TMemoryScheme read GetMemoryScheme;
    property Disposed: Boolean read GetDisposed;
    property RefCount: Integer read GetRefCount;
  end;


{ TLiteCustomObject class
  Single-thread code optimized TCustomObject class }

  TLiteCustomObject = class(TCustomObject)
  protected
    class var
      FInterfaceTable: TCustomObject.IInterfaceTable;
      FCustomObjectTable: TCustomObject.ICustomObjectTable;

    class procedure CreateIntfTables; static;
    class function IntfAddRef(const Self: PByte): Integer; stdcall; static;
    class function IntfRelease(const Self: PByte): Integer; stdcall; static;
    class function CustomObjectAddRef(const Self: PByte): Integer; stdcall; static;
    class function CustomObjectRelease(const Self: PByte): Integer; stdcall; static;
    function _AddRef: Integer; stdcall; inline;
    function _Release: Integer; stdcall; inline;
  public
    class function NewInstance: TObject; override;
    class function PreallocatedInstance(const AMemory: Pointer; const AMemoryScheme: TMemoryScheme): TObject; override;
    function __ObjAddRef: Integer; override;
    function __ObjRelease: Integer; override;
  end;


{ TRAIIHelper record
  Low level RTTI routine: initialization/finalization }

  TRAIIHelper = record
  public type
    TClearNativeProc = procedure(P, TypeInfo: Pointer);
    TClearNativeRec = record
      Offset: NativeInt;
      DynTypeInfo: PTypeInfo;
      ClearNativeProc: TClearNativeProc;
    end;
    TClearNatives = record
      Items: TArray<TClearNativeRec>;
      ItemSingle: TClearNativeRec;
      Count: NativeInt;
      procedure Clear; inline;
      procedure Add(AOffset: NativeInt; ADynTypeInfo: PTypeInfo; AClearNativeProc: TClearNativeProc);
    end;
    {$ifdef WEAKINSTREF}
      TInitNativeRec = record
        Offset: NativeInt;
      end;
      TInitNatives = record
        Items: TArray<TInitNativeRec>;
        ItemSingle: TInitNativeRec;
        Count: NativeInt;
        procedure Clear; inline;
        procedure Add(AOffset: NativeInt);
      end;
    {$else}
      TNativeRec = TClearNativeRec;
      TNatives = TClearNatives;
    {$endif}
    TStaticArrayRec = record
      Offset: NativeInt;
      StaticTypeInfo: PTypeInfo;
      Count: NativeUInt;
    end;
    TStaticArrays = record
      Items: TArray<TStaticArrayRec>;
      Count: NativeInt;
      procedure Clear; inline;
      procedure Add(AOffset: NativeInt; AStaticTypeInfo: PTypeInfo; ACount: NativeUInt);
    end;
  public const
    varDeepData = $BFE8;
  private type
    PFieldInfo = ^TFieldInfo;
    TFieldInfo = packed record
      TypeInfo: PPTypeInfo;
      Offset: Cardinal;
      {$ifdef LARGEINT}
      _Padding: Integer;
      {$endif}
    end;
    PFieldTable = ^TFieldTable;
    TFieldTable = packed record
      X: Word;
      Size: Cardinal;
      Count: Cardinal;
      Fields: array [0..0] of TFieldInfo;
    end;
    TData16 = packed record
    case Integer of
      0: (Native: NativeInt);
      1: (Method: TMethod);
      2: (VarData: TVarData);
      3: (Bytes: array[0..15] of Byte);
      4: (Words: array[0..7] of Word);
      5: (Integers: array[0..3] of Integer);
      6: (Int64s: array[0..1] of Int64);
      7: (Natives: array[0..{$ifdef LARGEINT}1{$else .SMALLINT}3{$endif}] of NativeUInt);
    end;
    PData16 = ^TData16;
    TData16<TOffset> = packed record
      Offset: TOffset;
    case Integer of
      0: (Native: NativeInt);
      1: (Method: TMethod);
      2: (VarData: TVarData);
      3: (Bytes: array[0..15] of Byte);
      4: (Words: array[0..7] of Word);
      5: (Integers: array[0..3] of Integer);
      6: (Int64s: array[0..1] of Int64);
      7: (Natives: array[0..{$ifdef LARGEINT}1{$else .SMALLINT}3{$endif}] of NativeUInt);
    end;
    T1 = Byte;
    T2 = Word;
    T3 = array[1..3] of Byte;
    T4 = Cardinal;
    T5 = array[1..5] of Byte;
    T6 = array[1..6] of Byte;
    T7 = array[1..7] of Byte;
    T8 = Int64;
    T9 = array[1..9] of Byte;
    T10 = array[1..10] of Byte;
    T11 = array[1..11] of Byte;
    T12 = array[1..12] of Byte;
    T13 = array[1..13] of Byte;
    T14 = array[1..14] of Byte;
    T15 = array[1..15] of Byte;
    T16 = array[1..16] of Byte;
    T17 = array[1..17] of Byte;
    T18 = array[1..18] of Byte;
    T19 = array[1..19] of Byte;
    T20 = array[1..20] of Byte;
    T21 = array[1..21] of Byte;
    T22 = array[1..22] of Byte;
    T23 = array[1..23] of Byte;
    T24 = array[1..24] of Byte;
    T25 = array[1..25] of Byte;
    T26 = array[1..26] of Byte;
    T27 = array[1..27] of Byte;
    T28 = array[1..28] of Byte;
    T29 = array[1..29] of Byte;
    T30 = array[1..30] of Byte;
    T31 = array[1..31] of Byte;
    T32 = array[1..32] of Byte;
    T33 = array[1..33] of Byte;
    T34 = array[1..34] of Byte;
    T35 = array[1..35] of Byte;
    T36 = array[1..36] of Byte;
    T37 = array[1..37] of Byte;
    T38 = array[1..38] of Byte;
    T39 = array[1..39] of Byte;
    T40 = array[1..40] of Byte;
    TTemp40 = record
    case Integer of
       1: (V1: T1);
       2: (V2: T2);
       3: (V3: T3);
       4: (V4: T4);
       5: (V5: T5);
       6: (V6: T6);
       7: (V7: T7);
       8: (V8: T8);
       9: (V9: T9);
      10: (V10: T10);
      11: (V11: T11);
      12: (V12: T12);
      13: (V13: T13);
      14: (V14: T14);
      15: (V15: T15);
      16: (V16: T16);
      17: (V17: T17);
      18: (V18: T18);
      19: (V19: T19);
      20: (V20: T20);
      21: (V21: T21);
      22: (V22: T22);
      23: (V23: T23);
      24: (V24: T24);
      25: (V25: T25);
      26: (V26: T26);
      27: (V27: T27);
      28: (V28: T28);
      29: (V29: T29);
      30: (V30: T30);
      31: (V31: T31);
      32: (V32: T32);
      33: (V33: T33);
      34: (V34: T34);
      35: (V35: T35);
      36: (V36: T36);
      37: (V37: T37);
      38: (V38: T38);
      39: (V39: T39);
      40: (V40: T40);
    end;
    TNativeIntRec = packed record
    case Boolean of
     False: (Int: Integer);
      True: (Native: NativeInt);
    end;
  private
    FTypeInfo: PTypeInfo;
    FSize: NativeInt;
    FItemSize: NativeInt;
    FWeak: Boolean;

    // type var leak fix
    class procedure RegisterDynamicArray(const P: Pointer); static;
    class procedure UnregisterDynamicArray(const P: Pointer); static;

    // initialization/finalization
    procedure Include(AOffset: NativeInt; Value: PTypeInfo);
    procedure Initialize(Value: PTypeInfo);
    function GetTypeData: PTypeData; inline;
    class function IsManagedTypeInfo(Value: PTypeInfo): Boolean; static;
    class function InitsProcNativeOne(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure InitsArrayProcNativeOne(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function InitsProcNativeTwo(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure InitsArrayProcNativeTwo(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function InitsProcNativeThree(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure InitsArrayProcNativeThree(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function InitsProcNatives(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure InitsArrayProcNatives(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function InitsProc(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure InitsArrayProc(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function ClearsProcNativeOne(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure ClearsArrayProcNativeOne(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function ClearsProcNativeTwo(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure ClearsArrayProcNativeTwo(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function ClearsProcNativeThree(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure ClearsArrayProcNativeThree(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function ClearsProcNatives(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure ClearsArrayProcNatives(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;
    class function ClearsProc(const Self: TRAIIHelper; P: Pointer): Pointer; static;
    class procedure ClearsArrayProc(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt); static;

    // TClearNativeProc-anonyms
    class procedure ULStrClear(P: Pointer); static;
    {$ifdef MSWINDOWS}
    class procedure WStrClear(P: Pointer); static;
    {$endif}
    class procedure IntfClear(P: Pointer); static;
    class procedure VarClear(P: Pointer); static;
    class procedure DynArrayClear(P, TypeInfo: Pointer); static;
    {$ifdef AUTOREFCOUNT}
    class procedure RefObjClear(P: Pointer); static;
    {$endif}
    {$ifdef WEAKINSTREF}
    class procedure WeakObjClear(P: Pointer); static;
    class procedure WeakMethodClear(P: Pointer); static;
    {$endif}
    {$ifdef WEAKINTFREF}
    class procedure WeakIntfClear(P: Pointer); static;
    {$endif}
  public
    {$ifdef WEAKINSTREF}
      InitNatives: TInitNatives;
      ClearNatives: TClearNatives;
    {$else}
      Natives: TNatives;
    {$endif}
    StaticArrays: TStaticArrays;
    InitProc: function(const Self: TRAIIHelper; P: Pointer): Pointer;
    ClearProc: function(const Self: TRAIIHelper; P: Pointer): Pointer;
    InitArrayProc: procedure(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt);
    ClearArrayProc: procedure(const Self: TRAIIHelper; P, Overflow: Pointer; ItemSize: NativeUInt);

    property TypeInfo: PTypeInfo read FTypeInfo write Initialize;
    property TypeData: PTypeData read GetTypeData;
    property Size: NativeInt read FSize;
    property ItemSize: NativeInt read FItemSize;
    property Weak: Boolean read FWeak;
  end;
  PRAIIHelper = ^TRAIIHelper;

  TRAIIHelper<T> = record
  public type
    P = ^T;
    TArrayT = array[0..0] of T;
    PArrayT = ^TArrayT;
    TData = TRAIIHelper.TData16;
    PData = ^TData;
  private
    class var
      FCreated: Boolean;
      FSpinlock: Byte;
      FOptions: TRAIIHelper;

    class procedure InternalCreate; static;
    class function GetManaged: Boolean; static; inline;
    class function GetWeak: Boolean; static; inline;
  public
    class procedure Create; static; inline;
    class function Init(Item: Pointer): Pointer; static; inline;
    class procedure Clear(Item: Pointer); static; inline;
    class function ClearItem(Item: Pointer): Pointer; static; inline;
    class procedure InitArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt); overload; static;
    class procedure InitArray(Item, OverflowItem: Pointer); overload; static; inline;
    class procedure InitArray(Item: Pointer; Count, ItemSize: NativeUInt); overload; static; inline;
    class procedure InitArray(Item: Pointer; Count: NativeUInt); overload; static; inline;
    class procedure ClearArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt); overload; static;
    class procedure ClearArray(Item, OverflowItem: Pointer); overload; static; inline;
    class procedure ClearArray(Item: Pointer; Count, ItemSize: NativeUInt); overload; static; inline;
    class procedure ClearArray(Item: Pointer; Count: NativeUInt); overload; static; inline;

    class property Created: Boolean read FCreated;
    class property Managed: Boolean read GetManaged;
    class property Weak: Boolean read GetWeak;
    class property Options: TRAIIHelper read FOptions;
  end;

  TRAIIHelper<T1,T2,T3,T4> = record
  public type
    T = TRecord<T1,T2,T3,T4>;
    P = ^T;
    TArrayT = array[0..0] of T;
    PArrayT = ^TArrayT;
    TData1 = TRAIIHelper.TData16;
    TData2 = TRAIIHelper.TData16<T1>;
    TData3 = TRAIIHelper.TData16<TRecord<T1,T2>>;
    TData4 = TRAIIHelper.TData16<TRecord<T1,T2,T3>>;
    PData1 = ^TData1;
    PData2 = ^TData2;
    PData3 = ^TData3;
    PData4 = ^TData4;
  private
    class var
      FCreated: Boolean;

    class procedure InternalCreate; static;
    class function GetManaged: Boolean; static; inline;
    class function GetWeak: Boolean; static; inline;
    class function GetOptions: PRAIIHelper; static; inline;
  public
    class procedure Create; static; inline;
    class function Init(Item: Pointer): Pointer; static; inline;
    class procedure Clear(Item: Pointer); static; inline;
    class function ClearItem(Item: Pointer): Pointer; static; inline;
    class procedure InitArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt); overload; static;
    class procedure InitArray(Item, OverflowItem: Pointer); overload; static; inline;
    class procedure InitArray(Item: Pointer; Count, ItemSize: NativeUInt); overload; static; inline;
    class procedure InitArray(Item: Pointer; Count: NativeUInt); overload; static; inline;
    class procedure ClearArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt); overload; static;
    class procedure ClearArray(Item, OverflowItem: Pointer); overload; static; inline;
    class procedure ClearArray(Item: Pointer; Count, ItemSize: NativeUInt); overload; static; inline;
    class procedure ClearArray(Item: Pointer; Count: NativeUInt); overload; static; inline;

    class property Created: Boolean read FCreated;
    class property Managed: Boolean read GetManaged;
    class property Weak: Boolean read GetWeak;
    class property Options: PRAIIHelper read GetOptions;
  end;


{ InterfaceDefaults record
  Default functions/interfaces }

  InterfaceDefaults = record
  public type
    TMethodPtr = procedure of object;
    TTriple = packed record
    case Integer of
      0: (Low: Word; High: Byte);
      1: (Bytes: array[0..2] of Byte);
    end;
    IComparerInst = packed record
      Vtable: Pointer;
      Size: NativeInt;
      QueryInterface,
      AddRef,
      Release,
      Compare: Pointer;
    end;
    IEqualityComparerInst = packed record
      Vtable: Pointer;
      Size: NativeInt;
      QueryInterface,
      AddRef,
      Release,
      Equals,
      GetHashCode: Pointer;
    end;
    TDefaultComparer<T> = record
    public class var
      Created: Boolean;
      Instance: IComparerInst;
    private
      class procedure InternalCreate; static;
    public
      class function Create: Pointer; static; inline;
    end;
    TDefaultEqualityComparer<T> = record
    public class var
      Created: Boolean;
      Instance: IEqualityComparerInst;
    private
      class procedure InternalCreate; static;
    public
      class function Create: Pointer; static; inline;
    end;
  private
    class function Compare_Var_Difficult(Equal: Boolean; Left, Right: PVariant): Integer; static;
    class function GetHashCode_Var_Difficult(Value: PVariant): Integer; static;
  public
    // Nop Interface
    class function NopQueryInterface(Inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall; static;
    class function NopAddRef(Inst: Pointer): Integer; stdcall; static;
    class function NopRelease(Inst: Pointer): Integer; stdcall; static;

    // IComparer<T>
    class function Compare_I1(Inst: Pointer; Left, Right: Shortint): Integer; static;
    class function Compare_U1(Inst: Pointer; Left, Right: Byte): Integer; static;
    class function Compare_I2(Inst: Pointer; Left, Right: Smallint): Integer; static;
    class function Compare_U2(Inst: Pointer; Left, Right: Word): Integer; static;
    class function Compare_I4(Inst: Pointer; Left, Right: Integer): Integer; static;
    class function Compare_U4(Inst: Pointer; Left, Right: Cardinal): Integer; static;
    class function Compare_I8(Inst: Pointer; Left, Right: Int64): Integer; static;
    class function Compare_U8(Inst: Pointer; Left, Right: UInt64): Integer; static;
    class function Compare_F4(Inst: Pointer; Left, Right: Single): Integer; static;
    class function Compare_F8(Inst: Pointer; Left, Right: Double): Integer; static;
    class function Compare_FE(Inst: Pointer; Left, Right: Extended): Integer; static;
    class function Compare_Var(Inst: Pointer; Left, Right: PVarData): Integer; static;
    class function Compare_OStr(Inst: Pointer; Left, Right: PByte): Integer; static;
    class function Compare_LStr(Inst: Pointer; Left, Right: PByte): Integer; static;
    class function Compare_UStr(Inst: Pointer; Left, Right: PByte): Integer; static;
    class function Compare_WStr(Inst: Pointer; Left, Right: PByte): Integer; static;
    class function Compare_Method(Inst: Pointer; const Left, Right: TMethodPtr): Integer; static;
    class function Compare_Dyn(const Inst: IComparerInst; Left, Right: PByte): Integer; static;
    class function Compare_Bin2(Inst: Pointer; Left, Right: Word): Integer; static;
    class function Compare_Bin3(Inst: Pointer; const Left, Right: TTriple): Integer; static;
    class function Compare_Bin4(Inst: Pointer; Left, Right: Cardinal): Integer; static;
    class function Compare_Bin8(Inst: Pointer; Left, Right: Int64): Integer; static;
    class function Compare_Bin(const Inst: IComparerInst; Left, Right: PByte): Integer; static;

    // IEqualityComparer<T>
    class function Equals_N1(Inst: Pointer; Left, Right: Byte): Boolean; static;
    class function GetHashCode_N1(Inst: Pointer; Value: Byte): Integer; static;
    class function Equals_N2(Inst: Pointer; Left, Right: Word): Boolean; static;
    class function GetHashCode_N2(Inst: Pointer; Value: Word): Integer; static;
    class function Equals_N4(Inst: Pointer; Left, Right: Integer): Boolean; static;
    class function GetHashCode_N4(Inst: Pointer; Value: Integer): Integer; static;
    class function Equals_N8(Inst: Pointer; Left, Right: Int64): Boolean; static;
    class function GetHashCode_N8(Inst: Pointer; Value: Int64): Integer; static;
    class function Equals_Class(Inst: Pointer; Left, Right: TObject): Boolean; static;
    class function GetHashCode_Class(Inst: Pointer; Value: TObject): Integer; static;
    class function GetHashCode_Ptr(Inst: Pointer; Value: NativeInt): Integer; static;
    class function Equals_F4(Inst: Pointer; Left, Right: Single): Boolean; static;
    class function GetHashCode_F4(Inst: Pointer; Value: Single): Integer; static;
    class function Equals_F8(Inst: Pointer; Left, Right: Double): Boolean; static;
    class function GetHashCode_F8(Inst: Pointer; Value: Double): Integer; static;
    class function Equals_FE(Inst: Pointer; Left, Right: Extended): Boolean; static;
    class function GetHashCode_FE(Inst: Pointer; Value: Extended): Integer; static;
    class function Equals_Var(Inst: Pointer; Left, Right: PVarData): Boolean; static;
    class function GetHashCode_Var(Inst: Pointer; Value: PVarData): Integer; static;
    class function Equals_OStr(Inst: Pointer; Left, Right: PByte): Boolean; static;
    class function GetHashCode_OStr(Inst: Pointer; Value: PByte): Integer; static;
    class function Equals_LStr(Inst: Pointer; Left, Right: PByte): Boolean; static;
    class function GetHashCode_LStr(Inst: Pointer; Value: PByte): Integer; static;
    class function Equals_UStr(Inst: Pointer; Left, Right: PByte): Boolean; static;
    class function GetHashCode_UStr(Inst: Pointer; Value: PByte): Integer; static;
    class function Equals_WStr(Inst: Pointer; Left, Right: PByte): Boolean; static;
    class function GetHashCode_WStr(Inst: Pointer; Value: PByte): Integer; static;
    class function Equals_Method(Inst: Pointer; const Left, Right: TMethodPtr): Boolean; static;
    class function GetHashCode_Method(Inst: Pointer; const Value: TMethodPtr): Integer; static;
    class function Equals_Dyn(const Inst: IEqualityComparerInst; Left, Right: PByte): Boolean; static;
    class function GetHashCode_Dyn(const Inst: IEqualityComparerInst; Value: PByte): Integer; static;
    class function Equals_Bin3(Inst: Pointer; const Left, Right: TTriple): Boolean; static;
    class function GetHashCode_Bin3(Inst: Pointer; const Value: TTriple): Integer; static;
    class function Equals_Bin(const Inst: IEqualityComparerInst; Left, Right: PByte): Boolean; static;
    class function GetHashCode_Bin(const Inst: IEqualityComparerInst; Value: PByte): Integer; static;
  end;


{ System.Generics.Defaults
  Equivalent types }

  IComparer<T> = interface
    function Compare(const Left, Right: T): Integer;
  end;

  IEqualityComparer<T> = interface
    function Equals(const Left, Right: T): Boolean;
    function GetHashCode(const Value: T): Integer;
  end;

  TComparison<T> = reference to function(const Left, Right: T): Integer;

  // Abstract base class for IComparer<T> implementations, and a provider
  // of default IComparer<T> implementations.
  TComparer<T> = class(TCustomObject, IComparer<T>)
  public
    class function Default: IComparer<T>; static;
    class function DefaultComparison: TComparison<T>; static;
    class function IsDefault(const Comparer: IComparer<T>): Boolean; static; inline;
    class function IsDefaultComparison(const Comparison: TComparison<T>): Boolean; static; inline;
    class function Construct(const Comparison: TComparison<T>): IComparer<T>;
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;

  TEqualityComparison<T> = reference to function(const Left, Right: T): Boolean;
  THasher<T> = reference to function(const Value: T): Integer;

  // Abstract base class for IEqualityComparer<T> implementations, and a provider
  // of default IEqualityComparer<T> implementations.
  TEqualityComparer<T> = class(TCustomObject, IEqualityComparer<T>)
  public
    class function Default: IEqualityComparer<T>; static;
    class function DefaultComparison: TEqualityComparer<T>; static;
    class function IsDefault(const EqualityComparer: IEqualityComparer<T>): Boolean; static; inline;
    class function IsDefaultComparison(const EqualityComparison: TEqualityComparison<T>): Boolean; static; inline;

    class function Construct(const EqualityComparison: TEqualityComparison<T>;
      const Hasher: THasher<T>): IEqualityComparer<T>;

    function Equals(const Left, Right: T): Boolean;
      reintroduce; overload; virtual; abstract;
    function GetHashCode(const Value: T): Integer;
      reintroduce; overload; virtual; abstract;
  end;

  // A non-reference-counted IInterface implementation.
  TSingletonImplementation = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TDelegatedComparer<T> = class(TComparer<T>)
  private
    FCompare: TComparison<T>;
  public
    constructor Create(const ACompare: TComparison<T>);
    function Compare(const Left, Right: T): Integer; override;
  end;

  TDelegatedEqualityComparer<T> = class(TEqualityComparer<T>)
  private
    FEquals: TEqualityComparison<T>;
    FGetHashCode: THasher<T>;
  public
    constructor Create(const AEquals: TEqualityComparison<T>;
      const AGetHashCode: THasher<T>);
    function Equals(const Left, Right: T): Boolean; overload; override;
    function GetHashCode(const Value: T): Integer; overload; override;
  end;

  TCustomComparer<T> = class(TSingletonImplementation, IComparer<T>, IEqualityComparer<T>)
  protected
    function Compare(const Left, Right: T): Integer; virtual; abstract;
    function Equals(const Left, Right: T): Boolean;
      reintroduce; overload; virtual; abstract;
    function GetHashCode(const Value: T): Integer;
      reintroduce; overload; virtual; abstract;
  end;

  TStringComparer = class(TCustomComparer<string>)
  private
    class var
      FOrdinal: TCustomComparer<string>;
  public
    class destructor Destroy;
    class function Ordinal: TStringComparer;
  end;

  TOrdinalIStringComparer = class(TStringComparer)
  private
    function CharsLower(Dest, Src: PWideChar; Count: Integer): Boolean;
    function GetHashCodeLower(const Value: string): Integer;
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean;
      reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer;
      reintroduce; overload; override;
  end;

  TIStringComparer = class(TCustomComparer<string>)
  private
    class var
      FOrdinal: TCustomComparer<string>;
  public
    class destructor Destroy;
    class function Ordinal: TStringComparer;
  end;


{ System.Generics.Collections
  Basic types }

  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TCollectionNotification) of object;

  TEnumerator_ = class abstract (TCustomObject, IEnumerator)
  protected
    function IEnumerator.GetCurrent = DoGetCurrentObject;
    function IEnumerator.MoveNext = DoMoveNext;
    procedure IEnumerator.Reset = DoReset;
    function DoGetCurrentObject: TObject; virtual; abstract;
    function DoMoveNext: Boolean; virtual; abstract;
    procedure DoReset; virtual;
  public
    procedure Reset;
    function MoveNext: Boolean;
    property Current: TObject read DoGetCurrentObject;
  end;

  TEnumerator<T> = class abstract (TEnumerator_, IEnumerator<T>)
  protected
    function IEnumerator<T>.GetCurrent = DoGetCurrent;
    function IEnumerator<T>.MoveNext = DoMoveNext;
    function DoGetCurrentObject: TObject; override;
    function DoGetCurrent: T; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    property CurrentObject: TObject read DoGetCurrentObject;
  end;

  TEnumerable_ = class abstract (TCustomObject, IEnumerable)
  protected
    function IEnumerable.GetEnumerator = GetObjectEnumerator;
    function GetObjectEnumerator: IEnumerator;
    function DoGetObjectEnumerator: TEnumerator_; virtual; abstract;
  public
    function GetEnumerator: TEnumerator_;
  end;

  TEnumerable<T> = class abstract (TEnumerable_, IEnumerable<T>)
  protected
    function IEnumerable<T>.GetEnumerator = GetEnumerator_;
    function DoGetObjectEnumerator: TEnumerator_; override;
    function GetEnumerator_: IEnumerator<T>;
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>; reintroduce;
    function ToArray: TArray<T>; virtual;
  end;

  TPair<TKey,TValue> = packed record
    Key: TKey;
    Value: TValue;
    constructor Create(const AKey: TKey; const AValue: TValue);
  end;

  PObject = ^TObject;


{ TArray class }

  TArray = class
  protected const
    HIGH_NATIVE = {$ifdef LARGEINT}63{$else}31{$endif};
    HIGH_NATIVE_BIT = NativeInt(1) shl HIGH_NATIVE;
    BUFFER_SIZE = 1024;
    RADIX_BUFFER_SIZE = 2048;
    INSERTION_SORT_LIMIT = 45;
  protected type
    TItemList<T> = array[0..15] of T;
    TSortStackItem<T> = record
      First: ^T;
      Last: ^T;
    end;
    TSortStack<T> = array[0..63] of TSortStackItem<T>;

    HugeByteArray = array[0..High(Integer) div SizeOf(Byte) - 1] of Byte;
    HugeWordArray = array[0..High(Integer) div SizeOf(Word) - 1] of Word;
    HugeCardinalArray = array[0..High(Integer) div SizeOf(Cardinal) - 1] of Cardinal;
    HugeUInt64Array = array[0..High(Integer) div SizeOf(UInt64) - 1] of UInt64;
    HugeNativeUIntArray = array[0..High(Integer) div SizeOf(NativeUInt) - 1] of NativeUInt;

    HugeShortIntArray = array[0..High(Integer) div SizeOf(ShortInt) - 1] of ShortInt;
    HugeSmallIntArray = array[0..High(Integer) div SizeOf(SmallInt) - 1] of SmallInt;
    HugeIntegerArray = array[0..High(Integer) div SizeOf(Integer) - 1] of Integer;
    HugeInt64Array = array[0..High(Integer) div SizeOf(Int64) - 1] of Int64;
    HugeNativeIntArray = array[0..High(Integer) div SizeOf(NativeInt) - 1] of NativeInt;

    HugeNativeArray = HugeNativeUIntArray;
    HugeTPointArray = array[0..High(Integer) div SizeOf(TPoint) - 1] of TPoint;
    HugeSingleArray = array[0..High(Integer) div SizeOf(Single) - 1] of Single;
    HugeDoubleArray = array[0..High(Integer) div SizeOf(Double) - 1] of Double;
    HugeExtendedArray = array[0..High(Integer) div SizeOf(Extended) - 1] of Extended;

    TLMemory = packed record
    case Integer of
      0: (LBytes: HugeByteArray);
      1: (LWords: HugeWordArray);
      2: (LCardinals: HugeCardinalArray);
      3: (LNatives: HugeNativeArray);
      4: (L1: array[1..1] of Byte;
          case Integer of
            0: (LWords1: HugeWordArray);
            1: (LCardinals1: HugeCardinalArray);
            2: (LNatives1: HugeNativeArray);
          );
      5: (L2: array[1..2] of Byte;
          case Integer of
            0: (LCardinals2: HugeCardinalArray);
            1: (LNatives2: HugeNativeArray);
          );
      6: (L3: array[1..3] of Byte;
          case Integer of
            0: (LCardinals3: HugeCardinalArray);
            1: (LNatives3: HugeNativeArray);
          );
    {$ifdef LARGEINT}
      7: (L4: array[1..4] of Byte; LNatives4: HugeNativeArray);
      8: (L5: array[1..5] of Byte; LNatives5: HugeNativeArray);
      9: (L6: array[1..6] of Byte; LNatives6: HugeNativeArray);
     10: (L7: array[1..7] of Byte; LNatives7: HugeNativeArray);
    {$endif}
    end;
    PLMemory = ^TLMemory;

    TRMemory = packed record
    case Integer of
      0: (RBytes: HugeByteArray);
      1: (RWords: HugeWordArray);
      2: (RCardinals: HugeCardinalArray);
      3: (RNatives: HugeNativeArray);
      4: (R1: array[1..1] of Byte;
          case Integer of
            0: (RWords1: HugeWordArray);
            1: (RCardinals1: HugeCardinalArray);
            2: (RNatives1: HugeNativeArray);
          );
      5: (R2: array[1..2] of Byte;
          case Integer of
            0: (RCardinals2: HugeCardinalArray);
            1: (RNatives2: HugeNativeArray);
          );
      6: (R3: array[1..3] of Byte;
          case Integer of
            0: (RCardinals3: HugeCardinalArray);
            1: (RNatives3: HugeNativeArray);
          );
    {$ifdef LARGEINT}
      7: (R4: array[1..4] of Byte; RNatives4: HugeNativeArray);
      8: (R5: array[1..5] of Byte; RNatives5: HugeNativeArray);
      9: (R6: array[1..6] of Byte; RNatives6: HugeNativeArray);
     10: (R7: array[1..7] of Byte; RNatives7: HugeNativeArray);
    {$endif}
    end;
    PRMemory = ^TRMemory;

    TSortHelper<T> = record
      Pivot: T;
      Temp: T;
      Inst: Pointer;
      Compare: function(Inst: Pointer; const Left, Right: T): Integer;

      procedure Init(const Comparer: IComparer<T>); overload;
      procedure Init(const Comparison: TComparison<T>); overload;
      procedure Init; overload;
      procedure FillZero;
    end;

    PS1 = ^ShortInt;
    PS2 = ^SmallInt;
    PS4 = ^Integer;
    PS8 = ^Int64;

    PU1 = ^Byte;
    PU2 = ^Word;
    PU4 = ^Cardinal;
    PU8 = ^UInt64;

    PF4 = ^Single;
    PF8 = ^Double;
    PFE = ^Extended;

    TFloat = packed record
    case Integer of
      0: (VSingle: Single);
      1: (VDouble: Double);
      2: (VExtended: Extended);
      3: (SSingle: Integer);
      4: (B1: array[1..SizeOf(Double) - SizeOf(Integer)] of Byte; SDouble: Integer);
      5: (B2: array[1..SizeOf(Extended) - SizeOf(Integer)] of Byte; SExtended: Integer);
    end;

    TSortPivot = packed record
    case Integer of
      0: (Ptr: Pointer);
      1: (Data: array[0..BUFFER_SIZE - 1] of Byte);
    end;

    TSearchHelper = record
      Count: NativeInt;
      Comparer: Pointer;
    end;

    TRadixes = array[Byte] of Word;
    PRadixes = ^TRadixes;
    TInternalRadixStored<T> = record
      Radixes: TRadixes;
      Data: array[0..RADIX_BUFFER_SIZE - 1] of T;
      Index: NativeInt;
      Ptr: array[0..1] of Pointer;
      Mask: NativeInt;
      SingleRadix: Word;
    end;

    TInternalSearchStored = record
      X: NativeUInt;
      ItemPtr: Pointer;
    end;

    TInternalSearchStored<T> = record
      Inst: Pointer;
      Compare: function(const Inst: Pointer; const Left, Right: T): Integer;
      Count: NativeInt;
    end;

  protected
    {$ifdef WEAKREF}
    class procedure WeakExchange<T>(const Left, Right: Pointer); static;
    class procedure WeakReverse<T>(const Values: Pointer; const Count: NativeInt); static;
    {$endif}
    class procedure CheckArrays(Source, Destination: Pointer; SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt); static;
    class function SortItemPivot<T>(const I, J: Pointer): Pointer; static; inline;
    class function SortItemNext<T>(const StackItem, I, J: Pointer): Pointer; static; inline;

    {$ifdef SMARTGENERICS}
    class function SortItemCount<T>(const I, J: Pointer): NativeInt; static; inline;
    class function SortBinaryMarker<T>(const Binary: Pointer): NativeUInt; static; inline;

    class procedure RadixSort<T>(const Values: Pointer; const Count, Flags: NativeInt); static;
    class function RadixSortSigneds<T>(var StackItem: TSortStackItem<T>): Pointer; static;
    class function RadixSortDescendingSigneds<T>(var StackItem: TSortStackItem<T>): Pointer; static;
    class function RadixSortUnsigneds<T>(var StackItem: TSortStackItem<T>): Pointer; static;
    class function RadixSortDescendingUnsigneds<T>(var StackItem: TSortStackItem<T>): Pointer; static;
    class function RadixSortFloats<T>(var StackItem: TSortStackItem<T>): Pointer; static;
    class function RadixSortDescendingFloats<T>(var StackItem: TSortStackItem<T>): Pointer; static;

    class procedure SortSigneds<T>(const Values: Pointer; const Count: NativeInt); static;
    class procedure SortDescendingSigneds<T>(const Values: Pointer; const Count: NativeInt); static;
    class procedure SortUnsigneds<T>(const Values: Pointer; const Count: NativeInt); static;
    class procedure SortDescendingUnsigneds<T>(const Values: Pointer; const Count: NativeInt); static;
    class procedure SortFloats<T>(const Values: Pointer; const Count: NativeInt); static;
    class procedure SortDescendingFloats<T>(const Values: Pointer; const Count: NativeInt); static;
    class procedure SortBinaries<T>(const Values: Pointer; const Count: NativeInt; var PivotBig: T); static;
    class procedure SortDescendingBinaries<T>(const Values: Pointer; const Count: NativeInt; var PivotBig: T); static;
    {$endif}
    {$ifdef WEAKREF}
    class procedure WeakSortUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>); static;
    class procedure WeakSortDescendingUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>); static;
    {$endif}
    class procedure SortUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>); static;
    class procedure SortDescendingUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>); static;

    {$ifdef SMARTGENERICS}
    class function SearchSigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt; static;
    class function SearchUnsigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt; static;
    class function SearchFloats<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt; static;
    class function SearchBinaries<T>(Values: Pointer; Count: NativeInt; const Item: T): NativeInt; static;

    class function SearchDescendingSigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt; static;
    class function SearchDescendingUnsigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt; static;
    class function SearchDescendingFloats<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt; static;
    class function SearchDescendingBinaries<T>(Values: Pointer; Count: NativeInt; const Item: T): NativeInt; static;
    {$endif}
    class function SearchUniversals<T>(Values: Pointer; const Helper: TSearchHelper; const Item: T): NativeInt; static;
    class function SearchDescendingUniversals<T>(Values: Pointer; const Helper: TSearchHelper; const Item: T): NativeInt; static;

    class function InternalSearch<T>(Values: Pointer; Index, Count: Integer; const Item: T;
      out FoundIndex: Integer): Boolean; overload; static; inline;
    class function InternalSearch<T>(Values: Pointer; Index, Count: Integer; const Item: T;
      out FoundIndex: Integer; Comparer: Pointer): Boolean; overload; static; inline;
    class function InternalSearchDescending<T>(Values: Pointer; Index, Count: Integer; const Item: T;
      out FoundIndex: Integer): Boolean; overload; static; inline;
    class function InternalSearchDescending<T>(Values: Pointer; Index, Count: Integer; const Item: T;
      out FoundIndex: Integer; Comparer: Pointer): Boolean; overload; static; inline;
  public
    class procedure Exchange<T>(const Left, Right: Pointer); static; inline;
    class procedure Copy<T>(const Destination, Source: Pointer); overload; static; inline;
    class procedure FillZero<T>(const Values: Pointer); static; inline;
    class procedure Reverse<T>(const Values: Pointer; const Count: NativeInt); overload; static;
    class procedure Reverse<T>(var Values: array of T); overload; static;
    class procedure Shuffle<T>(const Values: Pointer; const Count: NativeInt); overload; static;
    class procedure Shuffle<T>(var Values: array of T); overload; static;
    class procedure Copy<T>(const Source: array of T; var Destination: array of T; SourceIndex, DestIndex, Count: NativeInt); overload; static;
    class procedure Copy<T>(const Source: array of T; var Destination: array of T; Count: NativeInt); overload; static;
    class function Copy<T>(const Source: array of T; SourceIndex, Count: NativeInt): TArray<T>; overload; static;
    class function Copy<T>(const Source: array of T): TArray<T>; overload; static;

    class procedure Sort<T>(var Values: T; const Count: Integer); overload; static;
    class procedure Sort<T>(var Values: T; const Count: Integer; const Comparer: IComparer<T>); overload; static;
    class procedure Sort<T>(var Values: T; const Count: Integer; const Comparison: TComparison<T>); overload; static;
    class procedure Sort<T>(var Values: array of T); overload; static;
    class procedure Sort<T>(var Values: array of T; const Comparer: IComparer<T>); overload; static;
    class procedure Sort<T>(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer); overload; static;
    class procedure Sort<T>(var Values: array of T; const Comparison: TComparison<T>); overload; static;
    class procedure Sort<T>(var Values: array of T; Index, Count: Integer; const Comparison: TComparison<T>); overload; static;

    class procedure SortDescending<T>(var Values: T; const Count: Integer); overload; static;
    class procedure SortDescending<T>(var Values: T; const Count: Integer; const Comparer: IComparer<T>); overload; static;
    class procedure SortDescending<T>(var Values: T; const Count: Integer; const Comparison: TComparison<T>); overload; static;
    class procedure SortDescending<T>(var Values: array of T); overload; static;
    class procedure SortDescending<T>(var Values: array of T; const Comparer: IComparer<T>); overload; static;
    class procedure SortDescending<T>(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer); overload; static;
    class procedure SortDescending<T>(var Values: array of T; const Comparison: TComparison<T>); overload; static;
    class procedure SortDescending<T>(var Values: array of T; Index, Count: Integer; const Comparison: TComparison<T>); overload; static;

    class function BinarySearch<T>(var Values: T; const Item: T; out FoundIndex: Integer; Count: Integer): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T; out FoundIndex: Integer): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T; out FoundIndex: Integer;
      Index, Count: Integer): Boolean; overload; static;

    class function BinarySearch<T>(var Values: T; const Item: T; out FoundIndex: Integer; Count: Integer; const Comparer: IComparer<T>): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>;
      Index, Count: Integer): Boolean; overload; static;
    class function BinarySearch<T>(var Values: T; const Item: T; out FoundIndex: Integer; Count: Integer; const Comparison: TComparison<T>): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T; out FoundIndex: Integer; const Comparison: TComparison<T>): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T; out FoundIndex: Integer;
      Index, Count: Integer; const Comparison: TComparison<T>): Boolean; overload; static;

    class function BinarySearchDescending<T>(var Values: T; const Item: T; out FoundIndex: Integer; Count: Integer): Boolean; overload; static;
    class function BinarySearchDescending<T>(const Values: array of T; const Item: T; out FoundIndex: Integer): Boolean; overload; static;
    class function BinarySearchDescending<T>(const Values: array of T; const Item: T; out FoundIndex: Integer;
      Index, Count: Integer): Boolean; overload; static;

    class function BinarySearchDescending<T>(var Values: T; const Item: T; out FoundIndex: Integer; Count: Integer; const Comparer: IComparer<T>): Boolean; overload; static;
    class function BinarySearchDescending<T>(const Values: array of T; const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean; overload; static;
    class function BinarySearchDescending<T>(const Values: array of T; const Item: T; out FoundIndex: Integer; const Comparer: IComparer<T>;
      Index, Count: Integer): Boolean; overload; static;
    class function BinarySearchDescending<T>(var Values: T; const Item: T; out FoundIndex: Integer; Count: Integer; const Comparison: TComparison<T>): Boolean; overload; static;
    class function BinarySearchDescending<T>(const Values: array of T; const Item: T; out FoundIndex: Integer; const Comparison: TComparison<T>): Boolean; overload; static;
    class function BinarySearchDescending<T>(const Values: array of T; const Item: T; out FoundIndex: Integer;
      Index, Count: Integer; const Comparison: TComparison<T>): Boolean; overload; static;
  end;


{ IEnumerable-compatible collections
  Lightweight optimized enumerator routine
  Extended item processing functionality }

  TCollectionEnumeratorData<T> = record
    {$ifdef AUTOREFCOUNT}[Unsafe]{$endif} Owner: TObject;
    Current: T;
    Tag: NativeInt;
    Reserved: NativeInt;
    procedure Init(const AOwner: TObject; const ATag: NativeInt = -1; const AReserved: NativeInt = -1); inline;
  end;

  ICollection<T> = interface;
  TCollection<T> = class;
  TCollectionEnumerator<T> = record
  public
    type
      TMoveNextFunc = function(var AEnumerator: TCollectionEnumerator<T>): Boolean;
  public
    Data: TCollectionEnumeratorData<T>;
    Intf: IInterface;
    DoMoveNext: TMoveNextFunc;
    {$if CompilerVersion <= 22}
    function GetCurrent: T; inline;
    property Current: T read GetCurrent;
    {$else}
    property Current: T read Data.Current;
    {$ifend}
    function MoveNext: Boolean; inline;
  private
    function GetProxyInteface: Pointer{PCollectionEnumerator<T>}; inline;
    function InitProxyInteface: Pointer{PCollectionEnumerator<T>};
    class function MoveNextProxyEnumerator(var AEnumerator: TCollectionEnumerator<T>): Boolean; static;
    class function MoveNextEnumerator(var AEnumerator: TCollectionEnumerator<T>): Boolean; static;
  public
    procedure Init(const AOwner: TObject; const AMoveNextFunc: TMoveNextFunc;
      const ATag: NativeInt = -1; const AReserved: NativeInt = -1); overload; inline;
    procedure Init(const ACollection: TCollection<T>; const AProxyMode: Boolean); overload;
    procedure Init(const ACollection: ICollection<T>; const AProxyMode: Boolean); overload;
    procedure Init(const AEnumerator: IEnumerator<T>); overload;
    procedure Init(const AEnumerable: IEnumerable<T>); overload;
  end;

  TCollectionEnumeratorObject<T> = class(TCustomObject)
  protected
    FEnumerator: TCollectionEnumerator<T>;
    function GetCurrentT: T; inline;
  public
    constructor Create; overload;
    constructor Create(const AEnumerator: TCollectionEnumerator<T>); overload;
    function MoveNext: Boolean; inline;

    {$if CompilerVersion <= 22}
    property Current: T read GetCurrentT;
    {$else}
    property Current: T read FEnumerator.Data.Current;
    {$ifend}
    property Enumerator: TCollectionEnumerator<T> read FEnumerator write FEnumerator;
  end;

  ICollection<T> = interface(ICustomObject)
    function GetEnumerator: TCollectionEnumerator<T>;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetIsSynchronized: Boolean;
    function GetComparer: IComparer<T>;
    function GetComparison: TComparison<T>;
    function GetEqualityComparer: IEqualityComparer<T>;
    function GetEqualityComparison: TEqualityComparison<T>;
    function ToArray: TArray<T>; overload;
    function ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>; overload;

    function All(const APredicate: TFunction<T,Boolean>): Boolean;
    function Any(const APredicate: TFunction<T,Boolean>): Boolean;
    function Aggregate(const AFunc: TFunction<T,T,T>): T;
    function TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean;
    function Min: T; overload;
    function Min(const AComparer: IComparer<T>): T; overload;
    function Min(const AComparer: TComparison<T>): T; overload;
    function Min(const ASelector: TFunction<T,Integer>): Integer; overload;
    function TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload;
    function TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload;
    function Max: T; overload;
    function Max(const AComparer: IComparer<T>): T; overload;
    function Max(const AComparer: TComparison<T>): T; overload;
    function Max(const ASelector: TFunction<T,Integer>): Integer; overload;
    function TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload;
    function TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload;
    function Sum: T; overload;
    function Sum(const ASelector: TFunction<T,Integer>): Integer; overload;
    function Sum(const ASelector: TFunction<T,Int64>): Int64; overload;
    function Sum(const ASelector: TFunction<T,Extended>): Extended; overload;
    procedure ForEach(const AAction: TProcedure<T>); overload;
    function ForEach(const AAction: TFunction<T,Boolean>): Boolean; overload;
    function ElementAt(const AIndex: Integer): T;
    function ElementAtOrDefault(const AIndex: Integer): T; overload;
    function ElementAtOrDefault(const AIndex: Integer; const ADefaultValue: T): T; overload;
    function TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean;

    function First: T; overload;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(const ADefaultValue: T): T; overload;
    function TryGetFirst(var{out} Value: T): Boolean; overload;
    function First(const APredicate: TFunction<T,Boolean>): T; overload;
    function FirstOrDefault(const APredicate: TFunction<T,Boolean>): T; overload;
    function FirstOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload;
    function TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload;
    function Last: T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(const ADefaultValue: T): T; overload;
    function TryGetLast(var{out} Value: T): Boolean; overload;
    function Last(const APredicate: TFunction<T,Boolean>): T; overload;
    function LastOrDefault(const APredicate: TFunction<T,Boolean>): T; overload;
    function LastOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload;
    function TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload;
    function Single: T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const ADefaultValue: T): T; overload;
    function TryGetSingle(var{out} Value: T): Boolean; overload;
    function Single(const APredicate: TFunction<T,Boolean>): T; overload;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>): T; overload;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload;
    function TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload;

    function Contains(const AValue: T): Boolean; overload;
    function Contains(const AValue: T; const AComparer: IEqualityComparer<T>): Boolean; overload;
    function Contains(const AValue: T; const AComparer: TEqualityComparison<T>): Boolean; overload;
    function IndexOf(const AValue: T): Integer; overload;
    function IndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer; overload;
    function IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload;
    function LastIndexOf(const AValue: T): Integer; overload;
    function LastIndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer; overload;
    function LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload;
    function EqualsTo(const AValues: array of T): Boolean; overload;
    function EqualsTo(const AValues: array of T; const AComparer: IEqualityComparer<T>): Boolean; overload;
    function EqualsTo(const AValues: array of T; const AComparer: TEqualityComparison<T>): Boolean; overload;
    function EqualsTo(const ACollection: TCollection<T>): Boolean; overload;
    function EqualsTo(const ACollection: TCollection<T>; const AComparer: IEqualityComparer<T>): Boolean; overload;
    function EqualsTo(const ACollection: TCollection<T>; const AComparer: TEqualityComparison<T>): Boolean; overload;
    function EqualsTo(const ACollection: ICollection<T>): Boolean; overload;
    function EqualsTo(const ACollection: ICollection<T>; const AComparer: IEqualityComparer<T>): Boolean; overload;
    function EqualsTo(const ACollection: ICollection<T>; const AComparer: TEqualityComparison<T>): Boolean; overload;
    function EqualsTo(const AEnumerable: IEnumerable<T>): Boolean; overload;
    function EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: IEqualityComparer<T>): Boolean; overload;
    function EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: TEqualityComparison<T>): Boolean; overload;

    function Concat(const ACollection: TCollection<T>): ICollection<T>; overload;
    function Concat(const ACollection: ICollection<T>): ICollection<T>; overload;
    function Concat(const AEnumerable: IEnumerable<T>): ICollection<T>; overload;
    function Where(const APredicate: TFunction<T,Boolean>): ICollection<T>;
    function Ordered: ICollection<T>; overload;
    function Ordered(const AComparer: IComparer<T>): ICollection<T>; overload;
    function Ordered(const AComparer: TComparison<T>): ICollection<T>; overload;
    function Reversed: ICollection<T>;
    function Shuffled: ICollection<T>;

    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsSynchronized: Boolean read GetIsSynchronized;
    property Comparer: IComparer<T> read GetComparer;
    property Comparison: TComparison<T> read GetComparison;
    property EqualityComparer: IEqualityComparer<T> read GetEqualityComparer;
    property EqualityComparison: TEqualityComparison<T> read GetEqualityComparison;
    property Enumerator: TCollectionEnumerator<T> read GetEnumerator;
    property Elements[const AIndex: Integer]: T read ElementAt; default;
  end;

  TCollectionHelper = class(TCustomObject)
  public
    type
      TSecondMode = (smInstance, smInterface, smEnumerable);
  protected
    class function EItemNotFound: Exception; static;
    class function EDuplicatesNotAllowed: Exception; static;
    class function EArgumentIsNil(const AArgumentName: string): Exception; static;
    class function EArgumentOutOfRange: Exception; static;
    class procedure CheckArgumentIsNil(const AAssigned: Boolean; const AArgumentName: string); static; inline;
    class function EMethodNotFound(const AClass: TClass; const AMethodName: string): Exception; overload; static;
    function EMethodNotFound(const AMethodName: string): Exception; overload;

    class function InternalSumBytes(Value: PByte; Offset: NativeInt; Count: Integer): Byte; static;
    class function InternalSumWords(Value: PWord; Offset: NativeInt; Count: Integer): Word; static;
    class function InternalSumCardinals(Value: PCardinal; Offset: NativeInt; Count: Integer): Cardinal; static;
    class function InternalSumInt64s(Value: PInt64; Offset: NativeInt; Count: Integer): Int64; static;
    class function InternalSumSingles(Value: PSingle; Offset: NativeInt; Count: Integer): Single; static;
    class function InternalSumDoubles(Value: PDouble; Offset: NativeInt; Count: Integer): Double; static;
    class function InternalSumExtendeds(Value: PExtended; Offset: NativeInt; Count: Integer): Extended; static;
  end;

  TCollection<T> = class(TCollectionHelper, ICollection<T>, IEnumerable<T>)
{ public
    type
      TEnumerator = record
        Data: TCollectionEnumeratorData<T>;
        property Current: T read Data.Current;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TEnumerator; }
  strict private
    type
      TEnumeratorAdapter = class(TCollectionEnumeratorObject<T>, IEnumerator<T>)
        function IEnumerator<T>.GetCurrent = GetCurrentT;
        function GetCurrent: TObject;
        procedure Reset;
      end;
    function IEnumerable<T>.GetEnumerator = GetEnumeratorT;
    function GetEnumerator: IEnumerator;
    function GetEnumeratorT: IEnumerator<T>;
  protected
    type
      P = ^T;
      TLinearItems = packed record
        Values1: P;
        Count1: Integer;
        Values2: P;
        Count2: Integer;
        Offset: Integer;

        function Combine(var AResult: TLinearItems; const ASecond: TLinearItems): Boolean;
        procedure ToArray(var AResult: TArray<T>); overload;
        procedure ToArray(var AResult: TArray<T>; const APredicate: TFunction<T,Boolean>); overload;
        function All(const APredicate: TFunction<T,Boolean>): Boolean;
        function Any(const APredicate: TFunction<T,Boolean>): Boolean;
        function Aggregate(const AFunc: TFunction<T,T,T>): T;
        function TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean;
        function Min(const AComparer: TComparison<T>): T; overload;
        function Min(const ASelector: TFunction<T,Integer>): Integer; overload;
        function Max(const AComparer: TComparison<T>): T; overload;
        function Max(const ASelector: TFunction<T,Integer>): Integer; overload;
        function Sum: T; overload;
        function Sum(const ASelector: TFunction<T,Integer>): Integer; overload;
        function Sum(const ASelector: TFunction<T,Int64>): Int64; overload;
        function Sum(const ASelector: TFunction<T,Extended>): Extended; overload;
        procedure ForEach(const AAction: TProcedure<T>); overload;
        function ForEach(const AAction: TFunction<T,Boolean>): Boolean; overload;
        function TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
        function TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
        function TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
        function IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
        function LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
        function EqualsTo(const ALinearItems: TLinearItems; const AComparer: TEqualityComparison<T>): Boolean; overload;
        function EqualsTo(var AEnumerator: TCollectionEnumerator<T>; const AComparer: TEqualityComparison<T>): Boolean; overload;
        function EqualsTo(const AEnumerator: IEnumerator<T>; const AComparer: TEqualityComparison<T>): Boolean; overload;
      end;
      PLinearItems = ^TLinearItems;

    class function InternalAll(Value: P; Offset: NativeInt; Count: Integer; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalAll(const AEnumerator: IEnumerator<T>; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalAll(const AEnumerable: IEnumerable<T>; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalAny(Value: P; Offset: NativeInt; Count: Integer; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalAny(const AEnumerator: IEnumerator<T>; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalAny(const AEnumerable: IEnumerable<T>; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalAggregate(Value: P; Offset: NativeInt; Count: Integer; const ABaseValue: T; const AFunc: TFunction<T,T,T>): T; static;
    class function InternalTryAggregate(var AEnumerator: TCollectionEnumerator<T>; var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean; overload; static;
    class function InternalTryAggregate(const ACollection: TCollection<T>; var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean; overload; static;
    class function InternalTryAggregate(const ACollection: ICollection<T>; var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean; overload; static;
    class function InternalTryAggregate(const AEnumerator: IEnumerator<T>; var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean; overload; static;
    class function InternalTryAggregate(const AEnumerable: IEnumerable<T>; var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean; overload; static;
    class function InternalMin(Value: P; Offset: NativeInt; Count: Integer; const AComparer: TComparison<T>): T; overload; static;
    class function InternalMin(Value: P; Offset: NativeInt; Count: Integer; const ASelector: TFunction<T,Integer>): Integer; overload; static;
    class function InternalTryGetMin(var{out} Value: T; const AEnumerator: IEnumerator<T>; const AComparer: TComparison<T>): Boolean; overload; static;
    class function InternalTryGetMin(var{out} Value: T; const AEnumerable: IEnumerable<T>; const AComparer: TComparison<T>): Boolean; overload; static;
    class function InternalTryGetMin(var{out} Value: Integer; const AEnumerator: IEnumerator<T>; const ASelector: TFunction<T,Integer>): Boolean; overload; static;
    class function InternalTryGetMin(var{out} Value: Integer; const AEnumerable: IEnumerable<T>; const ASelector: TFunction<T,Integer>): Boolean; overload; static;
    class function InternalMax(Value: P; Offset: NativeInt; Count: Integer; const AComparer: TComparison<T>): T; overload; static;
    class function InternalMax(Value: P; Offset: NativeInt; Count: Integer; const ASelector: TFunction<T,Integer>): Integer; overload; static;
    class function InternalTryGetMax(var{out} Value: T; const AEnumerator: IEnumerator<T>; const AComparer: TComparison<T>): Boolean; overload; static;
    class function InternalTryGetMax(var{out} Value: T; const AEnumerable: IEnumerable<T>; const AComparer: TComparison<T>): Boolean; overload; static;
    class function InternalTryGetMax(var{out} Value: Integer; const AEnumerator: IEnumerator<T>; const ASelector: TFunction<T,Integer>): Boolean; overload; static;
    class function InternalTryGetMax(var{out} Value: Integer; const AEnumerable: IEnumerable<T>; const ASelector: TFunction<T,Integer>): Boolean; overload; static;
    class function InternalSum(Value: P; Offset: NativeInt; Count: Integer; const ASelector: TFunction<T,Integer>): Integer; overload; static;
    class function InternalSum(Value: P; Offset: NativeInt; Count: Integer; const ASelector: TFunction<T,Int64>): Int64; overload; static;
    class function InternalSum(Value: P; Offset: NativeInt; Count: Integer; const ASelector: TFunction<T,Extended>): Extended; overload; static;
    class function InternalSum(const AEnumerator: IEnumerator<T>): T; overload; static;
    class function InternalSum(const AEnumerable: IEnumerable<T>): T; overload; static;
    class function InternalSum(const AEnumerator: IEnumerator<T>; const ASelector: TFunction<T,Integer>): Integer; overload; static;
    class function InternalSum(const AEnumerable: IEnumerable<T>; const ASelector: TFunction<T,Integer>): Integer; overload; static;
    class function InternalSum(const AEnumerator: IEnumerator<T>; const ASelector: TFunction<T,Int64>): Int64; overload; static;
    class function InternalSum(const AEnumerable: IEnumerable<T>; const ASelector: TFunction<T,Int64>): Int64; overload; static;
    class function InternalSum(const AEnumerator: IEnumerator<T>; const ASelector: TFunction<T,Extended>): Extended; overload; static;
    class function InternalSum(const AEnumerable: IEnumerable<T>; const ASelector: TFunction<T,Extended>): Extended; overload; static;
    class procedure InternalForEach(Value: P; Offset: NativeInt; Count: Integer; const AAction: TProcedure<T>); overload; static;
    class procedure InternalForEach(const AEnumerator: IEnumerator<T>; const AAction: TProcedure<T>); overload; static;
    class procedure InternalForEach(const AEnumerable: IEnumerable<T>; const AAction: TProcedure<T>); overload; static;
    class function InternalForEach(Value: P; Offset: NativeInt; Count: Integer; const AAction: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalForEach(const AEnumerator: IEnumerator<T>; const AAction: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalForEach(const AEnumerable: IEnumerable<T>; const AAction: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalTryGetElementAt(const AEnumerator: IEnumerator<T>; var{out} Value: T; const AIndex: Integer): Boolean; overload; static;
    class function InternalTryGetElementAt(const AEnumerable: IEnumerable<T>; var{out} Value: T; const AIndex: Integer): Boolean; overload; static;
    class function InternalGetFirst(Value: P; Offset: NativeInt; Count: Integer; const APredicate: TFunction<T,Boolean>): Integer; overload; static;
    class function InternalTryGetFirst(const AEnumerator: IEnumerator<T>; var{out} Value: T): Boolean; overload; static;
    class function InternalTryGetFirst(const AEnumerable: IEnumerable<T>; var{out} Value: T): Boolean; overload; static;
    class function InternalTryGetFirst(const AEnumerator: IEnumerator<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalTryGetFirst(const AEnumerable: IEnumerable<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalGetLast(Value: P; Offset: NativeInt; Count: Integer; const APredicate: TFunction<T,Boolean>): Integer; static;
    class function InternalTryGetLast(const AEnumerator: IEnumerator<T>; var{out} Value: T): Boolean; overload; static;
    class function InternalTryGetLast(const AEnumerable: IEnumerable<T>; var{out} Value: T): Boolean; overload; static;
    class function InternalTryGetLast(const AEnumerator: IEnumerator<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalTryGetLast(const AEnumerable: IEnumerable<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; static;
    class function InternalGetSingle(Value: P; Offset: NativeInt; Count: Integer; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(var AEnumerator: TCollectionEnumerator<T>; var{out} Value: T; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const ACollection: TCollection<T>; var{out} Value: T; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const ACollection: ICollection<T>; var{out} Value: T; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const AEnumerator: IEnumerator<T>; var{out} Value: T; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const AEnumerable: IEnumerable<T>; var{out} Value: T; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(var AEnumerator: TCollectionEnumerator<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const ACollection: TCollection<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const ACollection: ICollection<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const AEnumerator: IEnumerator<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer; overload; static;
    class function InternalTryGetSingle(const AEnumerable: IEnumerable<T>; var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer; overload; static;
    class function InternalIndexOf(const AValue: T; Value: P; Offset: NativeInt; Count: Integer; const AComparer: TComparison<T>): Integer; overload; static;
    class function InternalIndexOf(const AValue: T; Value: P; Offset: NativeInt; Count: Integer; const AComparer: TEqualityComparison<T>): Integer; overload; static;
    class function InternalIndexOf(const AValue: T; const AEnumerator: IEnumerator<T>; const AComparer: TEqualityComparison<T>): Integer; overload; static;
    class function InternalIndexOf(const AValue: T; const AEnumerable: IEnumerable<T>; const AComparer: TEqualityComparison<T>): Integer; overload; static;
    class function InternalLastIndexOf(const AValue: T; Value: P; Offset: NativeInt; Count: Integer; const AComparer: TComparison<T>): Integer; overload; static;
    class function InternalLastIndexOf(const AValue: T; Value: P; Offset: NativeInt; Count: Integer; const AComparer: TEqualityComparison<T>): Integer; overload; static;
    class function InternalLastIndexOf(const AValue: T; const AEnumerator: IEnumerator<T>; const AComparer: TEqualityComparison<T>): Integer; overload; static;
    class function InternalLastIndexOf(const AValue: T; const AEnumerable: IEnumerable<T>; const AComparer: TEqualityComparison<T>): Integer; overload; static;
    class function InternalEqualsTo(Value1, Value2: P; Offset1, Offset2: NativeInt; Count: Integer; const AComparer: TEqualityComparison<T>): Boolean; overload; static;
    class function InternalEqualsTo(Value: P; Offset: NativeInt; Count: Integer; var AEnumerator: TCollectionEnumerator<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; static;
    class function InternalEqualsTo(Value: P; Offset: NativeInt; Count: Integer; const AEnumerator: IEnumerator<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; static;
  protected
    FComparer: IComparer<T>;
    FEqualityComparer: IEqualityComparer<T>;
    {8-bytes aligned}
    function ICollection<T>.GetEnumerator = DoGetEnumerator;
    function ICollection<T>.GetCount = DoGetCount;
    function ICollection<T>.GetIsEmpty = DoGetIsEmpty;
    function ICollection<T>.GetIsSynchronized = DoGetIsSynchronized;
    function ICollection<T>.GetComparer = DoGetComparer;
    function ICollection<T>.GetComparison = DoGetComparison;
    function ICollection<T>.GetEqualityComparer = DoGetEqualityComparer;
    function ICollection<T>.GetEqualityComparison = DoGetEqualityComparison;
    function DoGetCount: Integer; virtual; abstract;
    function DoGetIsEmpty: Boolean; virtual; abstract;
    function DoGetIsSynchronized: Boolean; virtual;
    function DoGetIsOrdered: Boolean; virtual;
    function DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean; virtual;
    function DoGetComparer: IComparer<T>;
    function DoGetComparison: TComparison<T>;
    function DoGetEqualityComparer: IEqualityComparer<T>;
    function DoGetEqualityComparison: TEqualityComparison<T>;
    function DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean; virtual;
    function DoGetEnumerator: TCollectionEnumerator<T>; virtual; abstract;
    function InternalTryGetSingle(var{out} Value: T): Integer; overload;
    function InternalTryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Integer; overload;
  public
    constructor Create;
    function ToArray: TArray<T>; overload; virtual;
    function ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>; overload; virtual;

    function All(const APredicate: TFunction<T,Boolean>): Boolean; virtual;
    function Any(const APredicate: TFunction<T,Boolean>): Boolean; virtual;
    function Aggregate(const AFunc: TFunction<T,T,T>): T; virtual;
    function TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean; virtual;
    function Min: T; overload; virtual;
    function Min(const AComparer: IComparer<T>): T; overload; virtual;
    function Min(const AComparer: TComparison<T>): T; overload; virtual;
    function Min(const ASelector: TFunction<T,Integer>): Integer; overload; virtual;
    function TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; virtual;
    function TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; virtual;
    function Max: T; overload; virtual;
    function Max(const AComparer: IComparer<T>): T; overload; virtual;
    function Max(const AComparer: TComparison<T>): T; overload; virtual;
    function Max(const ASelector: TFunction<T,Integer>): Integer; overload; virtual;
    function TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; virtual;
    function TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; virtual;
    function Sum: T; overload; virtual;
    function Sum(const ASelector: TFunction<T,Integer>): Integer; overload; virtual;
    function Sum(const ASelector: TFunction<T,Int64>): Int64; overload; virtual;
    function Sum(const ASelector: TFunction<T,Extended>): Extended; overload; virtual;
    procedure ForEach(const AAction: TProcedure<T>); overload; virtual;
    function ForEach(const AAction: TFunction<T,Boolean>): Boolean; overload; virtual;
    function ElementAt(const AIndex: Integer): T; virtual;
    function ElementAtOrDefault(const AIndex: Integer): T; overload; virtual;
    function ElementAtOrDefault(const AIndex: Integer; const ADefaultValue: T): T; overload; virtual;
    function TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean; virtual;

    function First: T; overload; virtual;
    function FirstOrDefault: T; overload; virtual;
    function FirstOrDefault(const ADefaultValue: T): T; overload; virtual;
    function TryGetFirst(var{out} Value: T): Boolean; overload; virtual;
    function First(const APredicate: TFunction<T,Boolean>): T; overload; virtual;
    function FirstOrDefault(const APredicate: TFunction<T,Boolean>): T; overload; virtual;
    function FirstOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload; virtual;
    function TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; virtual;
    function Last: T; overload; virtual;
    function LastOrDefault: T; overload; virtual;
    function LastOrDefault(const ADefaultValue: T): T; overload; virtual;
    function TryGetLast(var{out} Value: T): Boolean; overload; virtual;
    function Last(const APredicate: TFunction<T,Boolean>): T; overload; virtual;
    function LastOrDefault(const APredicate: TFunction<T,Boolean>): T; overload; virtual;
    function LastOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload; virtual;
    function TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; virtual;
    function Single: T; overload; virtual;
    function SingleOrDefault: T; overload; virtual;
    function SingleOrDefault(const ADefaultValue: T): T; overload; virtual;
    function TryGetSingle(var{out} Value: T): Boolean; overload; virtual;
    function Single(const APredicate: TFunction<T,Boolean>): T; overload; virtual;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>): T; overload; virtual;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload; virtual;
    function TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; virtual;

    function Contains(const AValue: T): Boolean; overload; virtual;
    function Contains(const AValue: T; const AComparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function Contains(const AValue: T; const AComparer: TEqualityComparison<T>): Boolean; overload; virtual;
    function IndexOf(const AValue: T): Integer; overload; virtual;
    function IndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer; overload; virtual;
    function IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; virtual;
    function LastIndexOf(const AValue: T): Integer; overload; virtual;
    function LastIndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer; overload; virtual;
    function LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; virtual;
    function EqualsTo(const AValues: array of T): Boolean; overload; virtual;
    function EqualsTo(const AValues: array of T; const AComparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function EqualsTo(const AValues: array of T; const AComparer: TEqualityComparison<T>): Boolean; overload; virtual;
    function EqualsTo(const ACollection: TCollection<T>): Boolean; overload; virtual;
    function EqualsTo(const ACollection: TCollection<T>; const AComparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function EqualsTo(const ACollection: TCollection<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; virtual;
    function EqualsTo(const ACollection: ICollection<T>): Boolean; overload; virtual;
    function EqualsTo(const ACollection: ICollection<T>; const AComparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function EqualsTo(const ACollection: ICollection<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; virtual;
    function EqualsTo(const AEnumerable: IEnumerable<T>): Boolean; overload; virtual;
    function EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; virtual;

    function Concat(const ACollection: TCollection<T>): ICollection<T>; overload; virtual;
    function Concat(const ACollection: ICollection<T>): ICollection<T>; overload; virtual;
    function Concat(const AEnumerable: IEnumerable<T>): ICollection<T>; overload; virtual;
    function Where(const APredicate: TFunction<T,Boolean>): ICollection<T>; virtual;
    function Ordered: ICollection<T>; overload; virtual;
    function Ordered(const AComparer: IComparer<T>): ICollection<T>; overload; virtual;
    function Ordered(const AComparer: TComparison<T>): ICollection<T>; overload; virtual;
    function Reversed: ICollection<T>; virtual;
    function Shuffled: ICollection<T>; virtual;

    property Count: Integer read DoGetCount;
    property IsEmpty: Boolean read DoGetIsEmpty;
    property IsSynchronized: Boolean read DoGetIsSynchronized;
    property Comparer: IComparer<T> read DoGetComparer;
    property Comparison: TComparison<T> read DoGetComparison;
    property EqualityComparer: IEqualityComparer<T> read DoGetEqualityComparer;
    property EqualityComparison: TEqualityComparison<T> read DoGetEqualityComparison;
    property Enumerator: TCollectionEnumerator<T> read DoGetEnumerator;
    property Elements[const AIndex: Integer]: T read ElementAt; default;
  end;


{ TCustomListCollection<T1,T,T3,T4> class
  Optimized class, that implements array-based collections }

  TCustomListCollection<T1,T,T3,T4> = class(TCollection<T>)
  public type
    TItem = TRecord<T1,T,T3,T4>;
    PItem = ^TItem;
    TItemList = array[0..1] of TItem;
    PItemList = ^TItemList;
    TLinearItems = TCollection<T>.TLinearItems;

    TEnumerator = record
      Data: TCollectionEnumeratorData<T>;
      {$if CompilerVersion <= 22}
      function GetCurrent: T; inline;
      property Current: T read GetCurrent;
      {$else}
      property Current: T read Data.Current;
      {$ifend}
      function MoveNext: Boolean; inline;
      function ReversedMoveNext: Boolean; inline;
     end;
  protected
    FItems: PItemList;
    FCapacity: TRAIIHelper.TNativeIntRec;
    FCount: TRAIIHelper.TNativeIntRec;
    {$ifdef SMALLINT}
    __Align: Integer;
    {$endif}

    function DoGetCount: Integer; override;
    function DoGetIsEmpty: Boolean; override;
    function GetIsEmpty: Boolean; inline;
    function DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean; override;
    function DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean; override;
    function DoGetEnumerator: TCollectionEnumerator<T>; override;
    function GetElementAt(const AIndex: Integer): T; inline;
  public
    function GetEnumerator: TEnumerator;
    function ToArray: TArray<T>; overload; override;
    function ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>; overload; override;

    function All(const APredicate: TFunction<T,Boolean>): Boolean; override;
    function Any(const APredicate: TFunction<T,Boolean>): Boolean; override;
    function Aggregate(const AFunc: TFunction<T,T,T>): T; override;
    function TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean; override;
    function Min: T; overload; override;
    function Min(const AComparer: IComparer<T>): T; overload; override;
    function Min(const AComparer: TComparison<T>): T; overload; override;
    function Min(const ASelector: TFunction<T,Integer>): Integer; overload; override;
    function TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; override;
    function TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; override;
    function Max: T; overload; override;
    function Max(const AComparer: IComparer<T>): T; overload; override;
    function Max(const AComparer: TComparison<T>): T; overload; override;
    function Max(const ASelector: TFunction<T,Integer>): Integer; overload; override;
    function TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; override;
    function TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; override;
    function Sum: T; overload; override;
    function Sum(const ASelector: TFunction<T,Integer>): Integer; overload; override;
    function Sum(const ASelector: TFunction<T,Int64>): Int64; overload; override;
    function Sum(const ASelector: TFunction<T,Extended>): Extended; overload; override;
    procedure ForEach(const AAction: TProcedure<T>); overload; override;
    function ForEach(const AAction: TFunction<T,Boolean>): Boolean; overload; override;
    function ElementAt(const AIndex: Integer): T; override;
    function ElementAtOrDefault(const AIndex: Integer): T; overload; override;
    function ElementAtOrDefault(const AIndex: Integer; const ADefaultValue: T): T; overload; override;
    function TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean; override;

    function First: T; overload; override;
    function FirstOrDefault: T; overload; override;
    function FirstOrDefault(const ADefaultValue: T): T; overload; override;
    function TryGetFirst(var{out} Value: T): Boolean; overload; override;
    function TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function Last: T; overload; override;
    function LastOrDefault: T; overload; override;
    function LastOrDefault(const ADefaultValue: T): T; overload; override;
    function TryGetLast(var{out} Value: T): Boolean; overload; override;
    function TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function Single: T; overload; override;
    function SingleOrDefault: T; overload; override;
    function SingleOrDefault(const ADefaultValue: T): T; overload; override;
    function TryGetSingle(var{out} Value: T): Boolean; overload; override;
    function Single(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload; override;
    function TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;

    function Contains(const AValue: T): Boolean; overload; override;
    function Contains(const AValue: T; const AComparer: IEqualityComparer<T>): Boolean; overload; override;
    function Contains(const AValue: T; const AComparer: TEqualityComparison<T>): Boolean; overload; override;
    function IndexOf(const AValue: T): Integer; overload; override;
    function IndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer; overload; override;
    function IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;
    function LastIndexOf(const AValue: T): Integer; overload; override;
    function LastIndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer; overload; override;
    function LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;

    property Count: Integer read FCount.Int;
    property IsEmpty: Boolean read GetIsEmpty;
    property Capacity: Integer read FCapacity.Int;
    property Enumerator: TEnumerator read GetEnumerator;
    property Elements[const AIndex: Integer]: T read GetElementAt; default;
  end;


{ TArrayCollection<T> class
  Dynamic array based container }

  TArrayCollection<T> = class(TCustomListCollection<TNothing,T,TNothing,TNothing>)
  protected
    FList: TArray<T>;
  public
    constructor Create(const AList: TArray<T>; const AComparer: IComparer<T>; const AEqualityComparer: IEqualityComparer<T>);
    property List: TArray<T> read FList;
  end;


{ TConcatedCollection<T> class
  Pair: basic collection and iterator (collection or enumerable) }

  TConcatedCollection<T> = class(TCollection<T>)
  public
    type
      TSecondMode = TCollectionHelper.TSecondMode;
  protected
    type
      TLinearItems = TCollection<T>.TLinearItems;

    class function EnumeratorMoveNext(var AEnumerator: TCollectionEnumerator<T>): Boolean; static;
    class function ReversedEnumeratorMoveNext(var AEnumerator: TCollectionEnumerator<T>): Boolean; static;
  protected
    FCollection: TCollection<T>;
    FSecondInstance: TCollection<T>;
    FSecondInterface: ICollection<T>;
    FSecondEnumerable: IEnumerable<T>;
    FSecondMode: TSecondMode;

    function DoGetCount: Integer; override;
    function DoGetIsEmpty: Boolean; override;
    function InternalEnumerableCount: Integer;
    function InternalEnumerableEmpty: Boolean;
    function InternalEnumerableToArray: TArray<T>; overload;
    function InternalEnumerableToArray(const APredicate: TFunction<T,Boolean>): TArray<T>; overload;
    function DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean; override;
    function DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean; override;
    function DoGetEnumerator: TCollectionEnumerator<T>; override;
    function InternalTryGetSingle(var{out} Value: T): Integer; overload;
    function InternalTryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Integer; overload;
  public
    constructor Create(const ACollection: TCollection<T>; const ASecond: Pointer; const ASecondMode: TSecondMode);
    function ToArray: TArray<T>; overload; override;
    function ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>; overload; override;

    function All(const APredicate: TFunction<T,Boolean>): Boolean; override;
    function Any(const APredicate: TFunction<T,Boolean>): Boolean; override;
    function TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean; override;
    function TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean; override;
    function TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; override;
    function TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean; override;
    function TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; override;
    function Sum: T; overload; override;
    function Sum(const ASelector: TFunction<T,Integer>): Integer; overload; override;
    function Sum(const ASelector: TFunction<T,Int64>): Int64; overload; override;
    function Sum(const ASelector: TFunction<T,Extended>): Extended; overload; override;
    procedure ForEach(const AAction: TProcedure<T>); overload; override;
    function ForEach(const AAction: TFunction<T,Boolean>): Boolean; overload; override;
    function TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean; override;

    function TryGetFirst(var{out} Value: T): Boolean; overload; override;
    function TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function TryGetLast(var{out} Value: T): Boolean; overload; override;
    function TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;
    function LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;

    property SecondMode: TSecondMode read FSecondMode;
  end;


{ TCustomProxyCollection<T> class
  Abstract collection that depends on original collection and specified enumerator }

  TCustomProxyCollection<T> = class(TCollection<T>)
  protected
    FCollection: TCollection<T>;
    function DoGetCount: Integer; override;
    function DoGetIsEmpty: Boolean; override;
    function DoGetIsSynchronized: Boolean; override;
  public
    constructor Create(const ACollection: TCollection<T>);

    function All(const APredicate: TFunction<T,Boolean>): Boolean; override;
    function Any(const APredicate: TFunction<T,Boolean>): Boolean; override;
    function TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; override;
    function TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; override;
    function TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; override;
    function TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; override;
    function Single: T; overload; override;
    function SingleOrDefault: T; overload; override;
    function SingleOrDefault(const ADefaultValue: T): T; overload; override;
    function TryGetSingle(var{out} Value: T): Boolean; overload; override;
    function Single(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function SingleOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload; override;
    function TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function Sum: T; overload; override;
    function Sum(const ASelector: TFunction<T,Integer>): Integer; overload; override;
    function Sum(const ASelector: TFunction<T,Int64>): Int64; overload; override;
    function Sum(const ASelector: TFunction<T,Extended>): Extended; overload; override;
    function Contains(const AValue: T; const AComparer: TEqualityComparison<T>): Boolean; overload; override;
    function Ordered(const AComparer: TComparison<T>): ICollection<T>; overload; override;
    function Shuffled: ICollection<T>; override;

    property Collection: TCollection<T> read FCollection;
  end;


{ TProxyCollection<T> class
  All collection methods are redirected to the original collection }

  TProxyCollection<T> = class(TCustomProxyCollection<T>)
  protected
    type
      TLinearItems = TCollection<T>.TLinearItems;

    function DoGetIsOrdered: Boolean; override;
    function DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean; override;
    function DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean; override;
    function DoGetEnumerator: TCollectionEnumerator<T>; override;
  public
    function ToArray: TArray<T>; overload; override;
    function ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>; overload; override;

    function TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean; override;
    function TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; override;
    function TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; override;
    function TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean; overload; override;
    function TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean; overload; override;
    procedure ForEach(const AAction: TProcedure<T>); overload; override;
    function ForEach(const AAction: TFunction<T,Boolean>): Boolean; overload; override;
    function TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean; override;

    function TryGetFirst(var{out} Value: T): Boolean; overload; override;
    function TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function TryGetLast(var{out} Value: T): Boolean; overload; override;
    function TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;
    function LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;
    function EqualsTo(const AValues: array of T; const AComparer: TEqualityComparison<T>): Boolean; overload; override;
    function EqualsTo(const ACollection: TCollection<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; override;
    function EqualsTo(const ACollection: ICollection<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; override;
    function EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: TEqualityComparison<T>): Boolean; overload; override;

    function Concat(const ACollection: TCollection<T>): ICollection<T>; overload; override;
    function Concat(const ACollection: ICollection<T>): ICollection<T>; overload; override;
    function Concat(const AEnumerable: IEnumerable<T>): ICollection<T>; overload; override;
    function Where(const APredicate: TFunction<T,Boolean>): ICollection<T>; override;
    function Reversed: ICollection<T>; override;
  end;


{ TReversedCollection<T> class
  Ñollection that contains original collection with a reverse enumerator }

  TReversedCollection<T> = class(TCustomProxyCollection<T>)
  protected
    function DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean; override;
    function DoGetEnumerator: TCollectionEnumerator<T>; override;
  public
    function ToArray: TArray<T>; overload; override;
    function ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>; overload; override;
    function TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean; override;
    function First: T; overload; override;
    function FirstOrDefault: T; overload; override;
    function FirstOrDefault(const ADefaultValue: T): T; overload; override;
    function TryGetFirst(var{out} Value: T): Boolean; overload; override;
    function First(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function FirstOrDefault(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function FirstOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload; override;
    function TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function Last: T; overload; override;
    function LastOrDefault: T; overload; override;
    function LastOrDefault(const ADefaultValue: T): T; overload; override;
    function TryGetLast(var{out} Value: T): Boolean; overload; override;
    function Last(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function LastOrDefault(const APredicate: TFunction<T,Boolean>): T; overload; override;
    function LastOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T; overload; override;
    function TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean; overload; override;
    function IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;
    function LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer; overload; override;
    function Reversed: ICollection<T>; override;
  end;


{ IContainer<T> interface
  Universal modifiable collection }

  IContainer<T> = interface(ICollection<T>)
    function GetCapacity: Integer;
    procedure SetCapacity(const AValue: Integer);
    procedure Clear;
    procedure TrimExcess;

    function TryEnqueue(const AValue: T): Boolean; overload;
    function TryEnqueue(const AValue: T; const ATimeout: Cardinal): Boolean; overload;
    procedure Enqueue(const AValue: T); overload;
    procedure Enqueue(const AValues: array of T); overload;
    procedure Enqueue(const ACollection: TCollection<T>); overload;
    procedure Enqueue(const ACollection: ICollection<T>); overload;
    procedure Enqueue(const AEnumerable: IEnumerable<T>); overload;
    function TryDequeue(var{out} AValue: T): Boolean; overload;
    function TryDequeue(var{out} AValue: T; const ATimeout: Cardinal): Boolean; overload;
    function TryDequeue(var AValues: array of T): Boolean; overload;
    function Dequeue: T; overload;
    function Dequeue(const ACount: Integer): TArray<T>; overload;
    function DequeueAll: TArray<T>;

    property Capacity: Integer read GetCapacity write SetCapacity;
  end;


{ TCustomDictionary<TKey,TValue> class
  Basic class for TDictionary<TKey,TValue>, TRapidDictionary<TKey,TValue> }

  TCustomDictionary<TKey,TValue> = class(TCollection<TPair<TKey,TValue>>)
  public type
    PItem = ^TItem;
    TItem = packed record
    private
      FKey: TKey;
      FValue: TValue;
      FNext: PItem;
      FHashCode: Integer;
    public
      property Key: TKey read FKey;
      property Value: TValue read FValue write FValue;
      property HashCode: Integer read FHashCode;
    end;
    TItemList = array[0..0] of TItem;
    PItemList = ^TItemList;

    TPairEnumerator = record
      Data: TCollectionEnumeratorData<TPair<TKey,TValue>>;
      {$if CompilerVersion <= 22}
      function GetCurrent: TPair<TKey,TValue>; inline;
      property Current: TPair<TKey,TValue> read GetCurrent;
      {$else}
      property Current: TPair<TKey,TValue> read Data.Current;
      {$ifend}
      function MoveNext: Boolean; inline;
    end;

    TKeyEnumerator = record
      Data: TCollectionEnumeratorData<TKey>;
      {$if CompilerVersion <= 22}
      function GetCurrent: TKey; inline;
      property Current: TKey read GetCurrent;
      {$else}
      property Current: TKey read Data.Current;
      {$ifend}
      function MoveNext: Boolean; inline;
    end;

    TValueEnumerator = record
      Data: TCollectionEnumeratorData<TValue>;
      {$if CompilerVersion <= 22}
      function GetCurrent: TValue; inline;
      property Current: TValue read GetCurrent;
      {$else}
      property Current: TValue read Data.Current;
      {$ifend}
      function MoveNext: Boolean; inline;
    end;

    TKeyCollection = class(TCollection<TKey>)
    protected
      {$ifdef AUTOREFCOUNT}[Unsafe]{$endif} FDictionary: TCustomDictionary<TKey,TValue>;
      function DoGetCount: Integer; override;
      function DoGetIsEmpty: Boolean; override;
      function GetCount: Integer; inline;
      function GetIsEmpty: Boolean; inline;
      function DoGetEnumerator: TCollectionEnumerator<TKey>; override;
    public
      constructor Create(const ADictionary: TCustomDictionary<TKey,TValue>);
      function GetEnumerator: TKeyEnumerator;
      function ToArray: TArray<TKey>; override; final;
      property Count: Integer read GetCount;
      property IsEmpty: Boolean read GetIsEmpty;
    end;

    TValueCollection = class(TCollection<TValue>)
    protected
      {$ifdef AUTOREFCOUNT}[Unsafe]{$endif} FDictionary: TCustomDictionary<TKey,TValue>;
      function DoGetCount: Integer; override;
      function DoGetIsEmpty: Boolean; override;
      function GetCount: Integer; inline;
      function GetIsEmpty: Boolean; inline;
      function DoGetEnumerator: TCollectionEnumerator<TValue>; override;
    public
      constructor Create(const ADictionary: TCustomDictionary<TKey,TValue>);
      function GetEnumerator: TValueEnumerator;
      function ToArray: TArray<TValue>; override; final;
      property Count: Integer read GetCount;
      property IsEmpty: Boolean read GetIsEmpty;
    end;
  private type
    PKey = ^TKey;
    PValue = ^TValue;
    THashList = array[0..0] of PItem;
    PHashList = ^THashList;
    TData16 = TRAIIHelper.TData16;
    PData16 = ^TData16;
    TKeyRec = TRAIIHelper.TData16;
    PKeyRec = ^TKeyRec;
    TValueRec = TRAIIHelper.TData16<TKey>;
    PValueRec = ^TValueRec;
  protected
    FItems: PItemList;
    FCapacity: NativeInt;
    FHashTable: TArray<PItem>;
    FHashTableMask: NativeInt;
    FCount: TRAIIHelper.TNativeIntRec;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;

    // rehash
    procedure Rehash(NewTableCount: NativeInt);
    procedure SetCapacity(ACapacity: NativeInt);
    function Grow: TCustomDictionary<TKey,TValue>;

    // items
    function NewItem: Pointer{PItem};
    procedure DisposeItem(Item: Pointer{PItem});
    procedure DoCleanupItems(Item: PItem; Count: NativeInt); virtual;

    // enumerators
    function DoGetCount: Integer; override;
    function DoGetEnumerator: TCollectionEnumerator<TPair<TKey,TValue>>; override;
    function InitKeyCollection: TKeyCollection {$ifdef AUTOREFCOUNT}unsafe{$endif};
    function InitValueCollection: TValueCollection {$ifdef AUTOREFCOUNT}unsafe{$endif};
    function GetKeys: TKeyCollection {$ifdef AUTOREFCOUNT}unsafe{$endif}; inline;
    function GetValues: TValueCollection {$ifdef AUTOREFCOUNT}unsafe{$endif}; inline;

    // helpers
    class function IntfMethod(Intf: Pointer; MethodNum: NativeUInt = 3): TMethod; inline; static;
    class procedure ClearMethod(var Method); inline; static;
  protected
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;
    FInternalKeyNotify: TCollectionNotifyEvent<TKey>;
    FInternalValueNotify: TCollectionNotifyEvent<TValue>;
    FInternalItemNotify: procedure(const Item: TItem; Action: TCollectionNotification) of object;

    procedure SetKeyNotify(const Value: TCollectionNotifyEvent<TKey>);
    procedure SetValueNotify(const Value: TCollectionNotifyEvent<TValue>);
    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); virtual;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); virtual;
    procedure KeyNotifyCaller(Sender: TObject; const Item: TKey; Action: TCollectionNotification);
    procedure ValueNotifyCaller(Sender: TObject; const Item: TValue; Action: TCollectionNotification);
    procedure ItemNotifyCaller(const Item: TItem; Action: TCollectionNotification);
    procedure ItemNotifyEvents(const Item: TItem; Action: TCollectionNotification);
    procedure ItemNotifyKey(const Item: TItem; Action: TCollectionNotification);
    procedure ItemNotifyValue(const Item: TItem; Action: TCollectionNotification);
    procedure SetNotifyMethods; virtual;

    property List: PItemList read FItems;
  protected const
    FOUND_NONE = 0;
    FOUND_EXCEPTION = 1;
    FOUND_DELETE = 2;
    FOUND_REPLACE = 3;
    FOUND_MASK = 3;

    EMPTY_NONE = 0 shl 2;
    EMPTY_EXCEPTION = 1 shl 2;
    EMPTY_NEW = 2 shl 2;
    EMPTY_MASK = 3 shl 2;
  protected
    FInternalFindValue: ^TValue;
    FDefaultValue: TValue;
  public
    constructor Create(ACapacity: Integer = 0);
    destructor Destroy; override;
    procedure Clear;
    procedure TrimExcess;
    function ContainsValue(const Value: TValue): Boolean;

    function GetEnumerator: TPairEnumerator;
    function ToArray: TArray<TPair<TKey,TValue>>; override; final;

    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
  end;


{ TDictionary<TKey,TValue>
  System.Generics.Collections equivalent

  Included 3 members:
    function Find(const Key: TKey): PItem;
    function FindOrAdd(const Key: TKey): PItem;
    property List: PItemList read FItems; }

  TDictionary<TKey,TValue> = class(TCustomDictionary<TKey,TValue>)
  private type
    TInternalFindStored = record
      HashCode: Integer;
      Parent: Pointer;
    end;
  protected
    FComparer: IEqualityComparer<TKey>;
    FComparerEquals: function(const Left, Right: TKey): Boolean of object;
    FComparerGetHashCode: function(const Value: TKey): Integer of object;

    function InternalFindItem(const Key: TKey; const FindMode: Integer): Pointer{Pitem};
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;

    function Find(const Key: TKey): Pointer{PItem};
    function FindOrAdd(const Key: TKey): Pointer{PItem};
    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property List;
    property Count: Integer read FCount.Int;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write SetKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write SetValueNotify;
  end;


{ TRapidDictionary<TKey,TValue> class
  Rapid "inline" TDictionary equivalent with default hash code and comparer }

  TRapidDictionary<TKey,TValue> = class(TCustomDictionary<TKey,TValue>)
  private type
    TInternalFindStored = record
      HashCode: Integer;
      Parent: Pointer;
      {$if CompilerVersion >= 29}
      Self: TRapidDictionary<TKey,TValue>;
      case Integer of
        0: (SingleRec: packed record
              Exponent: Integer;
              case Integer of
                0: (Mantissa: Single);
                1: (HighInt: Integer);
            end);
        1: (DoubleRec: packed record
              Exponent: Integer;
              case Integer of
                0: (Mantissa: Double);
                1: (LowInt: Integer; HighInt: Integer);
            end);
        2: (ExtendedRec: packed record
              Exponent: Integer;
              case Integer of
                0: (Mantissa: Extended);
                1: (LowInt: Integer; Middle: Word; HighInt: Integer);
            end);
      {$ifend}
    end;
  protected
    {$if CompilerVersion >= 29}
    function InternalFindItem(const Key: TKey; const FindMode: Integer): TCustomDictionary<TKey,TValue>.Pitem;
    {$else}
    FComparerEquals: function(const Left, Right: TKey): Boolean of object;
    FComparerGetHashCode: function(const Value: TKey): Integer of object;
    function InternalFindItem(const Key: TKey; const FindMode: Integer): Pointer{Pitem};
    {$ifend}
    function GetItem(const Key: TKey): TValue; inline;
    procedure SetItem(const Key: TKey; const Value: TValue); inline;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); overload;
    destructor Destroy; override;

    function Find(const Key: TKey): Pointer{PItem}; inline;
    function FindOrAdd(const Key: TKey): Pointer{PItem}; inline;
    procedure Add(const Key: TKey; const Value: TValue); inline;
    procedure Remove(const Key: TKey); inline;
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue); inline;
    function ContainsKey(const Key: TKey): Boolean; inline;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property List;
    property Count: Integer read FCount.Int;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write SetKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write SetValueNotify;
  end;


{ TCustomList<T> class
  Basic class for TList<T>, TQueue<T>, TStack<T> }

  TCustomList<T> = class(TCollection<T>)
  public type
    TItem = T;
    PItem = ^TItem;
    TItemList = array[0..0] of TItem;
    PItemList = ^TItemList;

    TEnumerator = record
      Data: TCollectionEnumeratorData<T>;
      {$if CompilerVersion <= 22}
      function GetCurrent: T; inline;
      property Current: T read GetCurrent;
      {$else}
      property Current: T read Data.Current;
      {$ifend}
      function MoveNext: Boolean;
    end;
  protected
    FItems: PItemList;
    FCapacity: TRAIIHelper.TNativeIntRec;
    FCount: TRAIIHelper.TNativeIntRec;
    FTail: NativeInt;
    FHead: NativeInt;
    FOnNotify: TCollectionNotifyEvent<T>;
    FInternalNotify: TCollectionNotifyEvent<T>;

    class procedure ClearMethod(var Method); static; inline;
    class function EmptyException: Exception; static;
    class function OutOfRangeException: Exception; static;
    procedure SetCapacity(Value: Integer);
    procedure Grow;
    procedure GrowTo(Value: Integer);
    procedure SetOnNotify(const Value: TCollectionNotifyEvent<T>);
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
    procedure NotifyCaller(Sender: TObject; const Item: T; Action: TCollectionNotification);
    procedure SetNotifyMethods; virtual;
    function DoGetCount: Integer; override;
    function GetIsEmpty: Boolean; inline;
    function DoGetIsEmpty: Boolean; override;
    function DoGetEnumerator: TCollectionEnumerator<T>; override;

    property List: PItemList read FItems;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure TrimExcess; inline;
    function ToArray: TArray<T>; override; final;
    function GetEnumerator: TEnumerator;

    property Count: Integer read FCount.Int;
    property IsEmpty: Boolean read GetIsEmpty;
    property Capacity: Integer read FCapacity.Int write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write SetOnNotify;
  end;


{ TList<T>
  System.Generics.Collections equivalent }

  TList<T> = class(TCustomList<T>)
  private type
    TData16 = TRAIIHelper.TData16;
    PData16 = ^TData16;
    TCompare = function(Inst: Pointer; const Left, Right: T): Integer;
    TEquals = function(Inst: Pointer; const Left, Right: T): Boolean;
    TInternalStored = packed record
      Self: Pointer;
      InternalNotify: TMethod;
      Count: NativeInt;
      Item: TCustomList<T>.PItem;
      ACount: Integer;
    end;
    TComparerInst = packed record
      Vtable: Pointer;
      Method: TMethod;
      QueryInterface,
      AddRef,
      Release,
      Call: Pointer;
    end;
  public type
    TDirection = {$if CompilerVersion <= 23}Rapid.Generics{$else}System.Types{$ifend}.TDirection;
    TEmptyFunc = reference to function (const L, R: T): Boolean;
    TListCompareFunc = reference to function (const L, R: T): Integer;
  protected
    FComparer: IComparer<T>;

    function GetItem(Index: Integer): T; inline;
    procedure ReplaceItemNotify(Index: Integer; const Value: T);
    procedure SetItem(Index: Integer; const Value: T); inline;
    function ItemValue(const Item: T): NativeInt;
    {$ifdef WEAKREF}
    class procedure InternalWeakInsert(const Item: Pointer; const ItemsCount, InsertCount: NativeUInt); static;
    {$endif}
    function InternalInsert(Index: NativeInt; const Value: T): Integer;
    procedure InternalDelete(Index: NativeInt; Action: TCollectionNotification);
    procedure SetCount(Value: Integer);
    procedure InternalMove(CurIndex, NewIndex: Integer);
    procedure InternalMove40(CurIndex, NewIndex: Integer);
    function InternalIndexOf(const Value: T): NativeInt; overload;
    function InternalIndexOf(const Value: T; const Comparer: IComparer<T>): NativeInt; overload;
    function InternalIndexOfRev(const Value: T): NativeInt; overload;
    function InternalIndexOfRev(const Value: T; const Comparer: IComparer<T>): NativeInt; overload;
    {$ifdef WEAKREF}
    procedure InternalWeakPack; overload;
    procedure InternalWeakPack(const IsEmpty: TEmptyFunc); overload;
    procedure InternalWeakPackComparer;
    {$endif}
    {$ifdef SMARTGENERICS}
    procedure InternalPackDifficults;
    {$endif}
    procedure InternalPackComparer;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(const Collection: TEnumerable<T>); overload;

    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
    {$ifNdef NEXTGEN}
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
    {$endif}

    function First: T; reintroduce; //inline;
    function Last: T; reintroduce; //inline;

    function Add(const Value: T): Integer; inline;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(const Collection: TEnumerable<T>); overload;

    procedure Insert(Index: Integer; const Value: T); inline;
    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;

    procedure Delete(Index: Integer); inline;
    procedure DeleteRange(AIndex, ACount: Integer);

    function Expand: TList<T>; inline;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer); inline;
    procedure Reverse; inline;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    procedure Sort(const AComparison: TComparison<T>); overload;
    procedure Sort(Index, Count: Integer); overload;
    procedure Sort(Index, Count: Integer; const AComparer: IComparer<T>); overload;
    procedure Sort(Index, Count: Integer; const AComparison: TComparison<T>); overload;
    procedure SortDescending; overload;
    procedure SortDescending(const AComparer: IComparer<T>); overload;
    procedure SortDescending(const AComparison: TComparison<T>); overload;
    procedure SortDescending(Index, Count: Integer); overload;
    procedure SortDescending(Index, Count: Integer; const AComparer: IComparer<T>); overload;
    procedure SortDescending(Index, Count: Integer; const AComparison: TComparison<T>); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; const AComparison: TComparison<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Index, Count: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>; Index, Count: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Index, Count: Integer; const AComparison: TComparison<T>): Boolean; overload;
    function BinarySearchDescending(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearchDescending(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>): Boolean; overload;
    function BinarySearchDescending(const Item: T; out FoundIndex: Integer; const AComparison: TComparison<T>): Boolean; overload;
    function BinarySearchDescending(const Item: T; out FoundIndex: Integer; Index, Count: Integer): Boolean; overload;
    function BinarySearchDescending(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>; Index, Count: Integer): Boolean; overload;
    function BinarySearchDescending(const Item: T; out FoundIndex: Integer; Index, Count: Integer; const AComparison: TComparison<T>): Boolean; overload;
         {$WARNINGS OFF}
    function Contains(const Value: T): Boolean; inline;
    function IndexOf(const Value: T): Integer; inline;
    function IndexOfItem(const Value: T; Direction: TDirection): Integer; inline;
    function LastIndexOf(const Value: T): Integer; inline;
    function Remove(const Value: T): Integer;
    function RemoveItem(const Value: T; Direction: TDirection): Integer;
    function Extract(const Value: T): T;
    function ExtractItem(const Value: T; Direction: TDirection): T;

    procedure Pack; overload;
    procedure Pack(const IsEmpty: TEmptyFunc); overload;

    property Count: Integer read FCount.Int write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List;
         {$WARNINGS ON}
  end;


{ TStack<T>
  System.Generics.Collections equivalent }

  TStack<T> = class(TCustomList<T>)
  protected
    procedure InternalPush(const Value: T);
    function InternalPop(const Action: TCollectionNotification): T;
  public
    constructor Create; overload;
    constructor Create(const Collection: TEnumerable<T>); overload;

    procedure Push(const Value: T); inline;
    function Pop: T; inline;
    function Extract: T; inline;
    function Peek: T; inline;
  end;

{ TQueue<T>
  System.Generics.Collections equivalent }

  TQueue<T> = class(TCustomList<T>)
  protected
    procedure InternalEnqueue(const Value: T);
    function InternalDequeue(const Action: TCollectionNotification): T;
  public
    constructor Create; overload;
    constructor Create(const Collection: TEnumerable<T>); overload;

    procedure Enqueue(const Value: T); inline;
    function Dequeue: T; inline;
    function Extract: T; inline;
    function Peek: T; inline;
  end;


{ TThreadList, TThreadedQueue classes
  Deprected synchronized routine }

  TThreadList<T> = class(TCustomObject)
  private
    FList: TList<T>;
    FLock: TObject;
    FDuplicates: TDuplicates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Item: T);
    procedure Clear;
    function LockList: TList<T>;
    procedure Remove(const Item: T); inline;
    procedure RemoveItem(const Item: T; Direction: TDirection);
    procedure UnlockList; inline;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  {$if CompilerVersion >= 22}
  TThreadedQueue<T> = class(TCustomObject)
  private
    FQueue: array of T;
    FQueueSize, FQueueOffset: Integer;
    FQueueNotEmpty,
    FQueueNotFull: TObject;
    FQueueLock: TObject;
    FShutDown: Boolean;
    FPushTimeout, FPopTimeout: LongWord;
    FTotalItemsPushed, FTotalItemsPopped: LongWord;
  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: LongWord = INFINITE; PopTimeout: LongWord = INFINITE);
    destructor Destroy; override;

    procedure Grow(ADelta: Integer);
    function PushItem(const AItem: T): TWaitResult; overload;
    function PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult; overload;
    function PopItem: T; overload;
    function PopItem(var AQueueSize: Integer): T; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult; overload;
    function PopItem(var AItem: T): TWaitResult; overload;
    procedure DoShutDown;

    property QueueSize: Integer read FQueueSize;
    property ShutDown: Boolean read FShutDown;
    property TotalItemsPushed: LongWord read FTotalItemsPushed;
    property TotalItemsPopped: LongWord read FTotalItemsPopped;
  end;
  {$ifend}


{  TObjectList, TObjectStack, TObjectDictionary, TRapidObjectDictionary
   Class-oriented containers }

  TObjectList<T: class> = class(TList<T>)
  protected
    FOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
    procedure DisposeNotifyEvent(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
    procedure DisposeOnly(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
    procedure SetNotifyMethods; override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read FOwnsObjects write SetOwnsObjects;
  end;

  TObjectStack<T: class> = class(TStack<T>)
  protected
    FOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
    procedure DisposeNotifyEvent(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
    procedure DisposeOnly(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
    procedure SetNotifyMethods; override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Pop;
    property OwnsObjects: Boolean read FOwnsObjects write SetOwnsObjects;
  end;

  TObjectQueue<T: class> = class(TQueue<T>)
  protected
    FOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
    procedure DisposeNotifyEvent(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
    procedure DisposeOnly(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
    procedure SetNotifyMethods; override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Dequeue;
    property OwnsObjects: Boolean read FOwnsObjects write SetOwnsObjects;
  end;

  TDictionaryOwnerships = set of (doOwnsKeys, doOwnsValues);

  TObjectDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
  public type
    TItem = TCustomDictionary<TKey,TValue>.TItem;
  private type
    TOnKeyNotify = procedure(Data, Sender: TObject; const Key: TObject; Action: TCollectionNotification);
    TOnValueNotify = procedure(Data, Sender: TObject; const Value: TObject; Action: TCollectionNotification);
  protected
    FOwnerships: TDictionaryOwnerships;
    procedure DisposeKeyEvent(Sender: TObject; const Key: TObject; Action: TCollectionNotification);
    procedure DisposeKeyOnly(Sender: TObject; const Key: TObject; Action: TCollectionNotification);
    procedure DisposeValueEvent(Sender: TObject; const Value: TObject; Action: TCollectionNotification);
    procedure DisposeValueOnly(Sender: TObject; const Value: TObject; Action: TCollectionNotification);
    procedure DisposeItemNotifyKeyCaller(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyKeyEvent(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyKeyOnly(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyValueCaller(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyValueEvent(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyValueOnly(const Item: TItem; Action: TCollectionNotification);
    procedure SetNotifyMethods; override;
  public
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0); overload;
    constructor Create(Ownerships: TDictionaryOwnerships;
      const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer;
      const AComparer: IEqualityComparer<TKey>); overload;
  end;

  TRapidObjectDictionary<TKey,TValue> = class(TRapidDictionary<TKey,TValue>)
  public type
    TItem = TCustomDictionary<TKey,TValue>.TItem;
  private type
    TOnKeyNotify = procedure(Data, Sender: TObject; const Key: TObject; Action: TCollectionNotification);
    TOnValueNotify = procedure(Data, Sender: TObject; const Value: TObject; Action: TCollectionNotification);
  protected
    FOwnerships: TDictionaryOwnerships;
    procedure DisposeKeyEvent(Sender: TObject; const Key: TObject; Action: TCollectionNotification);
    procedure DisposeKeyOnly(Sender: TObject; const Key: TObject; Action: TCollectionNotification);
    procedure DisposeValueEvent(Sender: TObject; const Value: TObject; Action: TCollectionNotification);
    procedure DisposeValueOnly(Sender: TObject; const Value: TObject; Action: TCollectionNotification);
    procedure DisposeItemNotifyKeyCaller(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyKeyEvent(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyKeyOnly(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyValueCaller(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyValueEvent(const Item: TItem; Action: TCollectionNotification);
    procedure DisposeItemNotifyValueOnly(const Item: TItem; Action: TCollectionNotification);
    procedure SetNotifyMethods; override;
  public
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0);
  end;


{ TSyncYield record
  Improves the performance of spin loops by providing the processor with a hint
  displaying that the current code is in a spin loop }

  PSyncYield = ^TSyncYield;
  TSyncYield = packed record
  private
    FCount: Byte;
  public
    class function Create: TSyncYield; static; inline;
    procedure Reset; inline;
    procedure Execute;

    property Count: Byte read FCount write FCount;
  end;


{ TSyncSpinlock record
  The simplest and sufficiently fast synchronization primitive
  Accepts only two values: locked and unlocked }

  PSyncSpinlock = ^TSyncSpinlock;
  TSyncSpinlock = record
  private
    {$if CompilerVersion >= 29}[Volatile]{$ifend}
    FValue: Byte;
    function GetLocked: Boolean; inline;
  public
    class function Create: TSyncSpinlock; static;

    function TryEnter: Boolean;
    function Enter(const ATimeout: Cardinal): Boolean; overload;
    procedure Enter; overload;
    procedure Leave;

    property Locked: Boolean read GetLocked;
  end;


{ TSyncLocker record
  Synchronization primitive, minimizes thread serialization to gain
  read access to a resource shared among threads while still providing complete
  exclusivity to callers needing write access to the shared resource }

  PSyncLocker = ^TSyncLocker;
  TSyncLocker = record
  private
    {$if CompilerVersion >= 29}[Volatile]{$ifend}
    FValue: Integer;
    function GetLocked: Boolean; inline;
    function GetLockedRead: Boolean; inline;
    function GetLockedExclusive: Boolean; inline;
  public
    class function Create: TSyncLocker; static;

    function TryEnterRead: Boolean;
    function TryEnterExclusive: Boolean;
    function EnterRead(const ATimeout: Cardinal): Boolean; overload;
    function EnterExclusive(const ATimeout: Cardinal): Boolean; overload;

    procedure EnterRead; overload;
    procedure EnterExclusive; overload;
    procedure LeaveRead;
    procedure LeaveExclusive;

    property Locked: Boolean read GetLocked;
    property LockedRead: Boolean read GetLockedRead;
    property LockedExclusive: Boolean read GetLockedExclusive;
  end;


{ TSyncSmallLocker record
  One-byte implementation of TSyncLocker }

  PSyncSmallLocker = ^TSyncSmallLocker;
  TSyncSmallLocker = record
  private
    {$if CompilerVersion >= 29}[Volatile]{$ifend}
    FValue: Byte;
    function GetLocked: Boolean; inline;
    function GetLockedRead: Boolean; inline;
    function GetLockedExclusive: Boolean; inline;
    class function InternalCAS(var AValue: Byte; const NewValue, Comparand: Byte): Boolean; static; {$ifNdef CPUINTELASM}inline;{$endif}
  public
    class function Create: TSyncSmallLocker; static;

    function TryEnterRead: Boolean;
    function TryEnterExclusive: Boolean;
    function EnterRead(const ATimeout: Cardinal): Boolean; overload;
    function EnterExclusive(const ATimeout: Cardinal): Boolean; overload;

    procedure EnterRead; overload;
    procedure EnterExclusive; overload;
    procedure LeaveRead;
    procedure LeaveExclusive;

    property Locked: Boolean read GetLocked;
    property LockedRead: Boolean read GetLockedRead;
    property LockedExclusive: Boolean read GetLockedExclusive;
  end;


implementation

resourcestring
  SInvalidPointerAlign = 'Invalid pointer %p align: should be %u';
  SInvalidRefCount = 'Invalid %s.RefCount value %d';
  {$if CompilerVersion <= 27}
  SObjectDisposed = 'Method called on disposed object';
  {$ifend}
  {$if CompilerVersion <= 27}
  sSameArrays = 'Source and Destination arrays must not be the same';
  {$ifend}
  SMethodNotSupported = 'Method %s not supported';

type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
    {$ifdef LARGEINT}
    _Padding: Integer;
    {$endif}
    RefCnt: Integer;
    Length: NativeInt;
  end;

// x86 architecture compatibility (Word mode)
{$ifNdef CPUX86}
function Swap(const X: NativeUInt): NativeUInt; inline;
begin
  Result := (Byte(X) shl 8) + Byte(X shr 8);
end;
{$endif}

{$if CompilerVersion <= 23}
function AtomicDecrement(var Target: Integer): Integer;
asm
  {$ifdef CPUX86}
    or edx, -1
    lock xadd [eax], edx
    lea eax, [edx - 1]
  {$else .CPUX64}
    or edx, -1
    lock xadd [rcx], edx
    or eax, -1
    add eax, edx
  {$endif}
end;
{$ifend}

{$if CompilerVersion <= 22}
procedure Frexp(const X: Single; var Mantissa: Single; var Exponent: Integer); overload;
{ Mantissa ptr in EAX, Exponent ptr in EDX }
asm // StackAlignSafe
        FLD     X
        PUSH    EAX
        MOV     dword ptr [edx], 0    { if X = 0, return 0 }

        FTST
        FSTSW   AX
        FWAIT
        SAHF
        JZ      @@Done

        FXTRACT                 // ST(1) = exponent, (pushed) ST = fraction
        FXCH

// The FXTRACT instruction normalizes the fraction 1 bit higher than
// wanted for the definition of frexp() so we need to tweak the result
// by scaling the fraction down and incrementing the exponent.

        FISTP   dword ptr [edx]
        FLD1
        FCHS
        FXCH
        FSCALE                  // scale fraction
        INC     dword ptr [edx] // exponent biased to match
        FSTP ST(1)              // discard -1, leave fraction as TOS

@@Done:
        POP     EAX
        FSTP    dword ptr [eax]
        FWAIT
end;

procedure Frexp(const X: Double; var Mantissa: Double; var Exponent: Integer); overload;
{ Mantissa ptr in EAX, Exponent ptr in EDX }
asm // StackAlignSafe
        FLD     X
        PUSH    EAX
        MOV     dword ptr [edx], 0    { if X = 0, return 0 }

        FTST
        FSTSW   AX
        FWAIT
        SAHF
        JZ      @@Done

        FXTRACT                 // ST(1) = exponent, (pushed) ST = fraction
        FXCH

// The FXTRACT instruction normalizes the fraction 1 bit higher than
// wanted for the definition of frexp() so we need to tweak the result
// by scaling the fraction down and incrementing the exponent.

        FISTP   dword ptr [edx]
        FLD1
        FCHS
        FXCH
        FSCALE                  // scale fraction
        INC     dword ptr [edx] // exponent biased to match
        FSTP ST(1)              // discard -1, leave fraction as TOS

@@Done:
        POP     EAX
        FSTP    qword ptr [eax]
        FWAIT
end;

procedure Frexp(const X: Extended; var Mantissa: Extended; var Exponent: Integer); overload;
{ Mantissa ptr in EAX, Exponent ptr in EDX }
asm // StackAlignSafe
        FLD     X
        PUSH    EAX
        MOV     dword ptr [edx], 0    { if X = 0, return 0 }

        FTST
        FSTSW   AX
        FWAIT
        SAHF
        JZ      @@Done

        FXTRACT                 // ST(1) = exponent, (pushed) ST = fraction
        FXCH

// The FXTRACT instruction normalizes the fraction 1 bit higher than
// wanted for the definition of frexp() so we need to tweak the result
// by scaling the fraction down and incrementing the exponent.

        FISTP   dword ptr [edx]
        FLD1
        FCHS
        FXCH
        FSCALE                  // scale fraction
        INC     dword ptr [edx] // exponent biased to match
        FSTP ST(1)              // discard -1, leave fraction as TOS

@@Done:
        POP     EAX
        FSTP    tbyte ptr [eax]
        FWAIT
end;
{$ifend}


{ TOSTime }

{$ifdef POSIX}
class function TOSTime.InternalClockGetTime(const ClockId: Integer): Int64;
var
  TimeSpec: Posix.Time.timespec;
begin
  clock_gettime(ClockId, @TimeSpec);
  Result := TimeSpec.tv_sec * SECOND + Trunc(TimeSpec.tv_nsec * (1 / 100));
end;
{$endif}

class procedure TOSTime.Initialize;
{$ifdef MSWINDOWS}
var
  UTCTime, LocalTime: TFileTime;
begin
  GetSystemTimeAsFileTime(UTCTime);
  FileTimeToLocalFileTime(UTCTime, LocalTime);
  FLOCAL_DELTA := Int64(LocalTime) - Int64(UTCTime);
end;
{$else .POSIX}
var
  i: Integer;
  Delta: Int64;
begin
  FLOCAL_DELTA := TTimeZone.Local.UtcOffset.Ticks;

  FCLOCK_REALTIME_DELTA := InternalClockGetTime(CLOCK_REALTIME_COARSE) - InternalClockGetTime(CLOCK_MONOTONIC_COARSE);
  for i := 1 to 10 do
  begin
    Delta := InternalClockGetTime(CLOCK_REALTIME_COARSE) - InternalClockGetTime(CLOCK_MONOTONIC_COARSE);
    if (Delta < FCLOCK_REALTIME_DELTA) then
      FCLOCK_REALTIME_DELTA := Delta;
  end;
  FCLOCK_REALTIME_DELTA := FCLOCK_REALTIME_DELTA + 134774 * DAY;
  FCLOCK_REALTIME_LOCAL_DELTA := FCLOCK_REALTIME_DELTA + FLOCAL_DELTA;
end;
{$endif}

class function TOSTime.GetTickCount: Cardinal;
{$ifdef MSWINDOWS}
begin
  Result := Winapi.Windows.GetTickCount;
end;
{$else .POSIX}
var
  TimeSpec: Posix.Time.timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_COARSE, @TimeSpec);
  Result := Cardinal(TimeSpec.tv_sec * 1000 + Round(TimeSpec.tv_nsec * (1 / 1000000)));
end;
{$endif}

class function TOSTime.GetNow: Int64;
{$ifdef MSWINDOWS}
var
  FileTime: TFileTime;
begin
  GetSystemTimeAsFileTime(FileTime);
  Result := Int64(FileTime) + FLOCAL_DELTA;
end;
{$else .POSIX}
begin
  Result := InternalClockGetTime(CLOCK_MONOTONIC_COARSE) + FCLOCK_REALTIME_LOCAL_DELTA;
end;
{$endif}

class function TOSTime.GetUTCNow: Int64;
{$ifdef MSWINDOWS}
var
  FileTime: TFileTime;
begin
  GetSystemTimeAsFileTime(FileTime);
  Result := Int64(FileTime);
end;
{$else .POSIX}
begin
  Result := InternalClockGetTime(CLOCK_MONOTONIC_COARSE) + FCLOCK_REALTIME_DELTA;
end;
{$endif}

class function TOSTime.ToDateTime(const ATimeStamp: Int64): TDateTime;
begin
  Result := ATimeStamp * (1 / TOSTime.DAY) + DATETIME_DELTA;
end;

class function TOSTime.ToString(const ATimeStamp: Int64): string;
var
  DateTime: TDateTime;
  Year, Month, Day, Hour, Minut, Second, MilliSecond: Word;
begin
  DateTime := ToDateTime(ATimeStamp);
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minut, Second, MilliSecond);
  Result := Format('%.4d-%.2u-%.2u %.2u:%.2u:%.2u.%.3u', [Year, Month, Day, Hour, Minut, Second, MilliSecond]);
end;


{ TCustomObject }

class function TCustomObject.NewInstance: TObject;
label
  _0, _1, _2, _3, _4, _5, _6, _7, _8;
type
  HugeNativeIntArray = array[0..High(Integer) div SizeOf(NativeInt) - 1] of NativeInt;
var
  LSize: Integer;
  LPtr: ^HugeNativeIntArray;
  LNull: NativeInt;
  LClass: TClass;
  LTable: PInterfaceTable;
  LEntry, LTopEntry: PInterfaceEntry;
  LValue: Pointer;
begin
  // allocate
  LSize := (PInteger(PByte(Self) + vmtInstanceSize)^ + (SizeOf(NativeInt) - 1)) and (-SizeOf(NativeInt));
  GetMem(LPtr, LSize);

  // TCustomObject initialization
  LPtr[0] := NativeInt(Self);
  LPtr[1]{FRefCount} := (1 or Ord(msHeap));
  LPtr[2] := NativeInt(@TCustomObject.FInterfaceTable);
  LPtr[3] := NativeInt(PInterfaceTable(PPointer(PByte(TCustomObject) + vmtIntfTable)^).Entries[0].VTable);

  // fill zero
  Dec(LSize, 4 * SizeOf(NativeInt));
  Inc(NativeInt(LPtr), 4 * SizeOf(NativeInt));
  LSize := LSize shr {$ifdef LARGEINT}3{$else .SMALLINT}2{$endif};
  LNull := 0;
  case (LSize) of
    8: goto _8;
    7: goto _7;
    6: goto _6;
    5: goto _5;
    4: goto _4;
    3: goto _3;
    2: goto _2;
    1: goto _1;
    0: goto _0;
  else
    FillChar(LPtr^, LSize shl {$ifdef LARGEINT}3{$else .SMALLINT}2{$endif}, #0);
    goto _0;
  end;
  _8: LPtr[7] := LNull;
  _7: LPtr[6] := LNull;
  _6: LPtr[5] := LNull;
  _5: LPtr[4] := LNull;
  _4: LPtr[3] := LNull;
  _3: LPtr[2] := LNull;
  _2: LPtr[1] := LNull;
  _1: LPtr[0] := LNull;
  _0: Dec(NativeInt(LPtr), 4 * SizeOf(NativeInt));

  // interfaces
  LClass := Self;
  if (LClass <> TCustomObject) then
  repeat
    LTable := PInterfaceTable(PPointer(PByte(LClass) + vmtIntfTable)^);
    if (Assigned(LTable)) then
    begin
      LTopEntry := @LTable.Entries[LTable.EntryCount];
      LEntry := @LTable.Entries[0];
      if (LEntry <> LTopEntry) then
      repeat
        LValue := LEntry.VTable;
        if (Assigned(LValue)) then
          PPointer(PByte(LPtr) + LEntry.IOffset)^ := LValue;

        Inc(LEntry);
      until (LEntry = LTopEntry);
    end;

    LClass := TClass(PPointer(PPointer(PByte(LClass) + vmtParent)^)^);
  until (LClass = TCustomObject);

  // result
  Result := Pointer(LPtr);
end;

class function TCustomObject.PreallocatedInstance(const AMemory: Pointer;
  const AMemoryScheme: TMemoryScheme): TObject;
label
  _0, _1, _2, _3, _4, _5, _6, _7, _8;
type
  HugeNativeIntArray = array[0..High(Integer) div SizeOf(NativeInt) - 1] of NativeInt;
var
  LInstanceSize, LSize: Integer;
  LPtr: ^HugeNativeIntArray;
  LNull: NativeInt;
  LClass: TClass;
  LTable: PInterfaceTable;
  LEntry, LTopEntry: PInterfaceEntry;
  LValue: Pointer;
begin
  // memory
  if (NativeUInt(AMemory) <= High(Word)) or (NativeInt(AMemory) and 7 <> 0) then
  begin
    if (NativeUInt(AMemory) <= High(Word)) then
    begin
      raise EInvalidPointer.CreateRes(Pointer(@SInvalidPointer));
    end else
    begin
      raise EInvalidPointer.CreateResFmt(Pointer(@SInvalidPointerAlign), [AMemory, 8]);
    end;
  end;

  // size
  LInstanceSize := PInteger(PByte(Self) + vmtInstanceSize)^;
  LSize := LInstanceSize and (-SizeOf(NativeInt));
  if (LInstanceSize and (SizeOf(NativeInt) - 1) <> 0) then
  begin
    PNativeInt(PByte(AMemory) + (LInstanceSize - SizeOf(NativeInt)))^ := 0;
  end;
  LPtr := AMemory;

  // TCustomObject initialization
  LPtr[0] := NativeInt(Self);
  LPtr[1]{FRefCount} := 1 + (Ord(AMemoryScheme) shl MEMORY_SCHEME_SHIFT);
  LPtr[2] := NativeInt(@TCustomObject.FInterfaceTable);
  LPtr[3] := NativeInt(PInterfaceTable(PPointer(PByte(TCustomObject) + vmtIntfTable)^).Entries[0].VTable);

  // fill zero
  Dec(LSize, 4 * SizeOf(NativeInt));
  Inc(NativeInt(LPtr), 4 * SizeOf(NativeInt));
  LSize := LSize shr {$ifdef LARGEINT}3{$else .SMALLINT}2{$endif};
  LNull := 0;
  case (LSize) of
    8: goto _8;
    7: goto _7;
    6: goto _6;
    5: goto _5;
    4: goto _4;
    3: goto _3;
    2: goto _2;
    1: goto _1;
    0: goto _0;
  else
    FillChar(LPtr^, LSize shl {$ifdef LARGEINT}3{$else .SMALLINT}2{$endif}, #0);
    goto _0;
  end;
  _8: LPtr[7] := LNull;
  _7: LPtr[6] := LNull;
  _6: LPtr[5] := LNull;
  _5: LPtr[4] := LNull;
  _4: LPtr[3] := LNull;
  _3: LPtr[2] := LNull;
  _2: LPtr[1] := LNull;
  _1: LPtr[0] := LNull;
  _0: Dec(NativeInt(LPtr), 4 * SizeOf(NativeInt));

  // interfaces
  LClass := Self;
  if (LClass <> TCustomObject) then
  repeat
    LTable := PInterfaceTable(PPointer(PByte(LClass) + vmtIntfTable)^);
    if (Assigned(LTable)) then
    begin
      LTopEntry := @LTable.Entries[LTable.EntryCount];
      LEntry := @LTable.Entries[0];
      if (LEntry <> LTopEntry) then
      repeat
        LValue := LEntry.VTable;
        if (Assigned(LValue)) then
          PPointer(PByte(LPtr) + LEntry.IOffset)^ := LValue;

        Inc(LEntry);
      until (LEntry = LTopEntry);
    end;

    LClass := TClass(PPointer(PPointer(PByte(LClass) + vmtParent)^)^);
  until (LClass = TCustomObject);

  // result
  Result := Pointer(LPtr);
end;

{$if Defined(WEAKREF) and Defined(CPUINTELASM)}
procedure _CleanupInstance(Instance: Pointer);
asm
  jmp System.@CleanupInstance
end;
{$ifend}

{$if Defined(CPUINTELASM)}
procedure FinalizeRecord(P: Pointer; TypeInfo: Pointer);
asm
  jmp System.@FinalizeRecord
end;
{$elseif CompilerVersion < 31}
procedure FinalizeRecord(P: Pointer; TypeInfo: Pointer); inline;
begin
  System.FinalizeArray(P, TypeInfo, 1);
end;
{$ifend}

procedure TCustomObject.FreeInstance;
label
  next_class, free_memory;
var
  LRefCount: Integer;
  LSize: Integer;
  {$if (not Defined(WEAKREF)) or Defined(CPUINTELASM) or (CompilerVersion >= 32)}
    LClass: TClass;
    LTypeInfo: PTypeInfo;
    {$ifdef MONITORSUPPORT}
    LMonitor, LMonitorFlags: NativeInt;
    LLockEvent: Pointer;
    {$endif}
    FieldTable: TRAIIHelper.PFieldTable;
    Field, TopField: TRAIIHelper.PFieldInfo;
    {$ifdef WEAKREF}
    WeakMode: Boolean;
    {$endif}
    LPtr: Pointer;
    VType: Integer;
  {$ifend}
begin
   // check reference count
   LRefCount := FRefCount and (REFCOUNT_MASK xor DUMMY_REFCOUNT);
   if (LRefCount <> 0) then
     raise CreateEInvalidRefCount(Self, LRefCount);

  {$if (not Defined(WEAKREF)) or Defined(CPUINTELASM) or (CompilerVersion >= 32)}
    // monitor start, weak references
    {$ifdef MONITORSUPPORT} // XE2+
    LSize := PInteger(PNativeInt(Self)^ + vmtInstanceSize)^;
    LMonitorFlags := PNativeInt(PByte(Self) + LSize + (- hfFieldSize + hfMonitorOffset))^;
    {$if CompilerVersion >= 32}
    if (LMonitorFlags and monWeakReferencedFlag <> 0) then
    {$ifend}
    {$endif}
    {$ifdef WEAKREF}
    begin
      {$ifdef CPUINTELASM}
      _CleanupInstance(Pointer(Self));
      {$else .NEXTGEN}
      Self.CleanupInstance;
      goto free_memory;
      {$endif}
    end;
    {$endif}

    // monitor finish
    {$ifdef MONITORSUPPORT}
    LMonitor  := LMonitorFlags {$if CompilerVersion >= 32}and monMonitorMask{$ifend};
    if (LMonitor <> 0) then
    begin
      LLockEvent := PPointer(LMonitor + (SizeOf(Integer) + SizeOf(Integer) + SizeOf(System.TThreadID)))^;
      if Assigned(LLockEvent) then
      begin
        MonitorSupport.FreeSyncObject(LLockEvent);
      end;

      FreeMem(Pointer(LMonitor));
    end;
    {$endif}

    // fields
    LClass := PPointer(Self)^;
    if (LClass <> TCustomObject) then
    repeat
      LTypeInfo := PPointer(PByte(LClass) + vmtInitTable)^;
      if (not Assigned(LTypeInfo)) then
        goto next_class;

      FieldTable := Pointer(PByte(LTypeInfo) + PByte(@LTypeInfo.Name)^);
      if (FieldTable.Count = 0) then
        goto next_class;

      TopField := @FieldTable.Fields[FieldTable.Count];
      Field := @FieldTable.Fields[0];
      repeat
        {$ifdef WEAKREF}
        WeakMode := False;
        {$endif}

        LPtr := PByte(Self) + NativeInt(Field.Offset);
        {$ifdef WEAKREF}
        if (Field.TypeInfo = nil) then
        begin
          WeakMode := True;
        end;
        if (not WeakMode) then
        begin
        {$endif}
          case (Field.TypeInfo^.Kind) of
            tkVariant:
            begin
              VType := Word(LPtr^);
              if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
                (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
                System.VarClear(Variant(LPtr^));
            end;
            {$ifdef AUTOREFCOUNT}
            tkClass:
            begin
              if Assigned(PPointer(LPtr)^) then
                TRAIIHelper.RefObjClear(LPtr);
            end;
            {$endif}
            {$ifdef WEAKINSTREF}
            tkMethod:
            begin
              Inc(NativeInt(LPtr), SizeOf(Pointer));
              if Assigned(PPointer(LPtr)^) then
                TRAIIHelper.WeakMethodClear(LPtr);
            end;
            {$endif}
            {$ifdef MSWINDOWS}
            tkWString:
            begin
              if Assigned(PPointer(LPtr)^) then
                TRAIIHelper.WStrClear(LPtr);
            end;
            {$else}
            tkWString,
            {$endif}
            tkLString, tkUString:
            begin
              if Assigned(PPointer(LPtr)^) then
                TRAIIHelper.ULStrClear(LPtr);
            end;
            tkInterface:
            begin
              if Assigned(PPointer(LPtr)^) then
                TRAIIHelper.IntfClear(LPtr);
            end;
            tkDynArray:
            begin
              if Assigned(PPointer(LPtr)^) then
                TRAIIHelper.DynArrayClear(LPtr, Field.TypeInfo^);
            end;
            tkArray{static array}:
            begin
              System.FinalizeArray(LPtr, Field.TypeInfo^, FieldTable.Count);
            end;
            tkRecord:
            begin
              FinalizeRecord(LPtr, Field.TypeInfo^);
            end;
          end;
        {$ifdef WEAKREF}
        end else
        case Field.TypeInfo^.Kind of
        {$ifdef WEAKINTFREF}
          tkInterface:
          begin
            if Assigned(PPointer(LPtr)^) then
              TRAIIHelper.WeakIntfClear(LPtr);
          end;
        {$endif}
        {$ifdef WEAKINSTREF}
          tkClass:
          begin
            if Assigned(PPointer(LPtr)^) then
              TRAIIHelper.WeakObjClear(LPtr);
          end;
          tkMethod:
          begin
            Inc(NativeInt(LPtr), SizeOf(Pointer));
            if Assigned(PPointer(LPtr)^) then
              TRAIIHelper.WeakMethodClear(LPtr);
          end;
        {$endif}
        end;
        {$endif .WEAKREF}

        Inc(Field);
      until (Field = TopField);

    next_class:
      LClass := TClass(PPointer(PPointer(PByte(LClass) + vmtParent)^)^);
    until (LClass = TCustomObject);
  {$else}
  next_class{dummy}:
    Self.CleanupInstance;
  {$ifend}

  // memory
free_memory:
  case (FRefCount shr MEMORY_SCHEME_SHIFT) and Ord(High(TMemoryScheme)) of
    Ord(msHeap):
    begin
      FreeMem(Pointer(Self));
    end;
    Ord(msAllocator):
    begin
      raise EInvalidOpException.Create('msAllocator');
    end;
    Ord(msFreeList):
    begin
      raise EInvalidOpException.Create('msFreeList');
    end;
  else
    // msUnknownBuffer
    // Do nothing
  end;
end;

function TCustomObject.GetSelf: TCustomObject;
begin
  Result := Self;
end;

class function TCustomObject.CreateEObjectDisposed: EObjectDisposed;
begin
  Result := EObjectDisposed.CreateRes(Pointer(@SObjectDisposed));
end;

class function TCustomObject.CreateEInvalidRefCount(const AObject: TObject; const ARefCount: Integer): EInvalidContainer;
begin
  Result := EInvalidContainer.CreateResFmt(Pointer(@SInvalidRefCount), [AObject.ClassName, ARefCount]);
end;

function TCustomObject.GetMemoryScheme: TMemoryScheme;
begin
  Result := TMemoryScheme(Integer((FRefCount shr MEMORY_SCHEME_SHIFT) and Ord(High(TMemoryScheme))));
end;

function TCustomObject.GetDisposed: Boolean;
begin
  Result := (FRefCount and DISPOSED_FLAG <> 0);
end;

procedure TCustomObject.CheckDisposed;
begin
  if (FRefCount and DISPOSED_FLAG = 0) then
    raise CreateEObjectDisposed;
end;

function TCustomObject.GetRefCount: Integer;
begin
  Result := FRefCount and REFCOUNT_MASK;
end;

procedure TCustomObject.AfterConstruction;
begin
  {$ifNdef AUTOREFCOUNT}
  if (FRefCount <> 1) then
  begin
    AtomicDecrement(FRefCount);
  end else
  begin
    FRefCount := 0;
  end;
  {$endif}
end;

procedure TCustomObject.BeforeDestruction;
begin
end;

{$ifNdef AUTOREFCOUNT}
procedure TCustomObject.Free;
begin
  DisposeOf;
end;
{$endif}

function TCustomObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TCustomObject.DisposeOf;
type
  TBeforeDestructionProc = procedure(Instance: Pointer);
  TDestructorProc = procedure(Instance: Pointer; OuterMost: ShortInt);
var
  LRef: Integer;
  Proc: Pointer;
begin
  if (Self = nil) then
    Exit;

  // no references
  {$ifNdef AUTOREFCOUNT}
  if (FRefCount and MEMORY_SCHEME_CLEAR = DEFAULT_REFCOUNT) then
  begin
    FRefCount := FRefCount or (DUMMY_REFCOUNT or DISPOSED_FLAG);
    Destroy;
    Exit;
  end;
  {$endif}

  // has references: the instance remains alive
  // mark disposed (exit if already disposed)
  repeat
    LRef := FRefCount;
    if (LRef and DISPOSED_FLAG <> 0) then
      Exit;

    if (Cardinal(LRef and MEMORY_SCHEME_CLEAR) <= DEFAULT_REFCOUNT) then
    begin
      FRefCount := LRef or DISPOSED_FLAG;
      Break;
    end;

    if (AtomicCmpExchange(FRefCount, LRef or DISPOSED_FLAG, LRef) = LRef) then
      Break;
  until (False);

  // before destruction
  Proc := PPointer(PNativeInt(Self)^ + vmtBeforeDestruction)^;
  if (Proc <> @TCustomObject.BeforeDestruction) then
    TBeforeDestructionProc(Proc)(Self);

  // destructor
  Proc := PPointer(PNativeInt(Self)^ + vmtDestroy)^;
  if (Proc <> @TCustomObject.Destroy) then
    TDestructorProc(Proc)(Self, 0);
end;

function TCustomObject.__ObjAddRef: Integer;
begin
  Result := FRefCount;
  if (Cardinal(Result and REFCOUNT_MASK) <= DEFAULT_REFCOUNT) then
  begin
    Inc(Result);
    FRefCount := Result;
    Result := Result and REFCOUNT_MASK;
    Exit;
  end else
  begin
    Result := AtomicIncrement(FRefCount);
  end;
end;

function TCustomObject._AddRef: Integer; stdcall;
begin
  if (PPointer(PNativeInt(Self)^ + vmtObjAddRef)^ = @TCustomObject.__ObjAddRef) then
  begin
    Result := FRefCount;
    if (Cardinal(Result and REFCOUNT_MASK) <= DEFAULT_REFCOUNT) then
    begin
      Inc(Result);
      FRefCount := Result;
      Result := Result and REFCOUNT_MASK;
      Exit;
    end else
    begin
      Result := AtomicIncrement(FRefCount);
      Exit;
    end;
  end else
  begin
    Result := __ObjAddRef;
  end;
end;

function TCustomObject.__ObjRelease: Integer;
begin
  // release reference
  Result := FRefCount;
  if (Result and REFCOUNT_MASK <> 1) then
  begin
    Result := AtomicDecrement(FRefCount) and REFCOUNT_MASK;
    if (Result <> 0) then
      Exit;
  end else
  begin
    Dec(Result);
    FRefCount := Result;
    Result := 0{Result and REFCOUNT_MASK};
  end;

  // no references: destroy/freeinstance
  if (FRefCount and DISPOSED_FLAG = 0) then
  begin
    FRefCount := FRefCount or (DUMMY_REFCOUNT or DISPOSED_FLAG);
    Destroy;
    Exit;
  end else
  begin
    FreeInstance;
  end;
end;

function TCustomObject._Release: Integer; stdcall;
begin
  if (PPointer(PNativeInt(Self)^ + vmtObjRelease)^ = @TCustomObject.__ObjRelease) then
  begin
    // release reference
    Result := FRefCount;
    if (Result and REFCOUNT_MASK = 1) then
    begin
      Dec(Result);
      FRefCount := Result;
      Result := 0{Result and REFCOUNT_MASK};
    end else
    begin
      Result := AtomicDecrement(FRefCount) and REFCOUNT_MASK;
      if (Result <> 0) then
        Exit;
    end;

    // no references: destroy/freeinstance
    if (FRefCount and DISPOSED_FLAG = 0) then
    begin
      FRefCount := FRefCount or (DUMMY_REFCOUNT or DISPOSED_FLAG);
      Destroy;
      Exit;
    end else
    begin
      FreeInstance;
      Exit;
    end;
  end else
  begin
    Result := __ObjRelease;
  end;
end;

{$ifdef MONITORSUPPORT}
procedure TCustomObject.OptimizeMonitor;
const
  OPTIMIZED_SPIN_COUNT = 5;
var
  LSize: Integer;
  LMonitor: NativeInt;
  LField: PInteger;
begin
  LSize := PInteger(PNativeInt(Self)^ + vmtInstanceSize)^;
  LMonitor := PNativeInt(PByte(Self) + LSize + (- hfFieldSize + hfMonitorOffset))^;
  {$if CompilerVersion >= 32}
  LMonitor := LMonitor and monMonitorMask;
  {$ifend}
  if (LMonitor <> 0) then
  begin
    LField := PInteger(LMonitor + (SizeOf(Integer) + SizeOf(Integer) + SizeOf(System.TThreadID) + SizeOf(Pointer)));
    if (LField^ = OPTIMIZED_SPIN_COUNT) then
      Exit;

    if (CPUCount = 1) then
      Exit;
  end;

  TMonitor.SetSpinCount(Self, OPTIMIZED_SPIN_COUNT);
end;
{$endif}

class procedure TCustomObject.CreateIntfTables;
begin
  TCustomObject.FInterfaceTable[0] := @TCustomObject.IntfQueryInterface;
  TCustomObject.FInterfaceTable[1] := @TCustomObject.IntfAddRef;
  TCustomObject.FInterfaceTable[2] := @TCustomObject.IntfRelease;
end;

class function TCustomObject.IntfQueryInterface(const Self: PByte;
  const IID: TGUID; out Obj): HResult; stdcall;
begin
  with TCustomObject(Self - 2 * SizeOf(Pointer)) do
    Result := QueryInterface(IID, Obj);
end;

class function TCustomObject.IntfAddRef(const Self: PByte): Integer; stdcall;
begin
  with TCustomObject(Self - 2 * SizeOf(Pointer)) do
    Result := _AddRef;
end;

class function TCustomObject.IntfRelease(const Self: PByte): Integer; stdcall;
begin
  with TCustomObject(Self - 2 * SizeOf(Pointer)) do
    Result := _Release;
end;


{ TLiteCustomObject }

class function TLiteCustomObject.NewInstance: TObject;
type
  HugePointerArray = array[0..High(Integer) div SizeOf(NativeInt) - 1] of Pointer;
  PHugePointerArray = ^HugePointerArray;
begin
  Result := inherited NewInstance;
  PHugePointerArray(Result)[2] := @TLiteCustomObject.FInterfaceTable;
  PHugePointerArray(Result)[3] := @TLiteCustomObject.FCustomObjectTable;
end;

class function TLiteCustomObject.PreallocatedInstance(const AMemory: Pointer;
  const AMemoryScheme: TMemoryScheme): TObject;
type
  HugePointerArray = array[0..High(Integer) div SizeOf(NativeInt) - 1] of Pointer;
  PHugePointerArray = ^HugePointerArray;
begin
  Result := inherited PreallocatedInstance(AMemory, AMemoryScheme);
  PHugePointerArray(Result)[2] := @TLiteCustomObject.FInterfaceTable;
  PHugePointerArray(Result)[3] := @TLiteCustomObject.FCustomObjectTable;
end;

function TLiteCustomObject.__ObjAddRef: Integer;
begin
  Result := FRefCount + 1;
  FRefCount := Result;
  Result := Result and REFCOUNT_MASK;
end;

function TLiteCustomObject._AddRef: Integer;
begin
  if (PPointer(PNativeInt(Self)^ + vmtObjAddRef)^ = @TLiteCustomObject.__ObjAddRef) then
  begin
    Result := FRefCount + 1;
    FRefCount := Result;
    Result := Result and REFCOUNT_MASK;
    Exit;
  end else
  begin
    Result := __ObjAddRef;
  end;
end;

function TLiteCustomObject.__ObjRelease: Integer;
begin
  Result := FRefCount - 1;
  FRefCount := Result;
  Result := Result and REFCOUNT_MASK;
  if (Result <> 0) then
    Exit;

  if (FRefCount and DISPOSED_FLAG = 0) then
  begin
    FRefCount := FRefCount or (DUMMY_REFCOUNT or DISPOSED_FLAG);
    Destroy;
    Exit;
  end else
  begin
    FreeInstance;
  end;
end;

function TLiteCustomObject._Release: Integer;
begin
  if (PPointer(PNativeInt(Self)^ + vmtObjRelease)^ = @TLiteCustomObject.__ObjRelease) then
  begin
    Result := FRefCount - 1;
    FRefCount := Result;
    Result := Result and REFCOUNT_MASK;
    if (Result <> 0) then
      Exit;

    if (FRefCount and DISPOSED_FLAG = 0) then
    begin
      FRefCount := FRefCount or (DUMMY_REFCOUNT or DISPOSED_FLAG);
      Destroy;
      Exit;
    end else
    begin
      FreeInstance;
      Exit;
    end;
  end else
  begin
    Result := __ObjRelease;
  end;
end;

class procedure TLiteCustomObject.CreateIntfTables;
var
  LTable: PInterfaceTable;
begin
  TLiteCustomObject.FInterfaceTable := TCustomObject.FInterfaceTable;
  TLiteCustomObject.FInterfaceTable[1] := @TLiteCustomObject.IntfAddRef;
  TLiteCustomObject.FInterfaceTable[2] := @TLiteCustomObject.IntfRelease;

  LTable := PInterfaceTable(PPointer(PByte(TCustomObject) + vmtIntfTable)^);
  System.Move(LTable.Entries[0].VTable^, TLiteCustomObject.FCustomObjectTable, SizeOf(TLiteCustomObject.FCustomObjectTable));
  TLiteCustomObject.FCustomObjectTable[1] := @TLiteCustomObject.CustomObjectAddRef;
  TLiteCustomObject.FCustomObjectTable[2] := @TLiteCustomObject.CustomObjectRelease;
end;

class function TLiteCustomObject.IntfAddRef(const Self: PByte): Integer; stdcall;
begin
  with TLiteCustomObject(Self - 2 * SizeOf(Pointer)) do
    Result := _AddRef;
end;

class function TLiteCustomObject.IntfRelease(const Self: PByte): Integer; stdcall;
begin
  with TLiteCustomObject(Self - 2 * SizeOf(Pointer)) do
    Result := _Release;
end;

class function TLiteCustomObject.CustomObjectAddRef(const Self: PByte): Integer; stdcall;
begin
  with TLiteCustomObject(Self - 3 * SizeOf(Pointer)) do
    Result := _AddRef;
end;

class function TLiteCustomObject.CustomObjectRelease(const Self: PByte): Integer; stdcall;
begin
  with TLiteCustomObject(Self - 3 * SizeOf(Pointer)) do
    Result := _Release;
end;


{ TRAIIHelper.TClearNatives }

procedure TRAIIHelper.TClearNatives.Clear;
begin
  TRAIIHelper.UnregisterDynamicArray(Pointer(Items));
  Items := nil;
  Count := 0;
end;

procedure TRAIIHelper.TClearNatives.Add(AOffset: NativeInt; ADynTypeInfo: PTypeInfo;
  AClearNativeProc: TClearNativeProc);
begin
  if (Count = 0) then
  begin
    ItemSingle.ClearNativeProc := AClearNativeProc;
    ItemSingle.Offset := AOffset;
    ItemSingle.DynTypeInfo := ADynTypeInfo;
  end;

  Inc(Count);
  SetLength(Items, Count);
  with Items[Count - 1] do
  begin
    ClearNativeProc := AClearNativeProc;
    Offset := AOffset;
    DynTypeInfo := ADynTypeInfo;
  end;
end;

{$ifdef WEAKINSTREF}
{ TRAIIHelper.TInitNatives }

procedure TRAIIHelper.TInitNatives.Clear;
begin
  TRAIIHelper.UnregisterDynamicArray(Pointer(Items));
  Items := nil;
  Count := 0;
end;

procedure TRAIIHelper.TInitNatives.Add(AOffset: NativeInt);
begin
  if (Count = 0) then
  begin
    ItemSingle.Offset := AOffset;
  end;

  Inc(Count);
  SetLength(Items, Count);
  Items[Count - 1].Offset := AOffset;
end;
{$endif}

{ TRAIIHelper.TStaticArrays }

procedure TRAIIHelper.TStaticArrays.Clear;
begin
  TRAIIHelper.UnregisterDynamicArray(Pointer(Items));
  Items := nil;
  Count := 0;
end;

procedure TRAIIHelper.TStaticArrays.Add(AOffset: NativeInt; AStaticTypeInfo: PTypeInfo; ACount: NativeUInt);
begin
  Inc(Count);
  SetLength(Items, Count);
  with Items[Count - 1] do
  begin
    Offset := AOffset;
    StaticTypeInfo := AStaticTypeInfo;
    Count := ACount;
  end;
end;

{ TRAIIHelper }

{$WARNINGS OFF}
class procedure TRAIIHelper.RegisterDynamicArray(const P: Pointer);
begin
  {$ifdef MSWINDOWS}
  if (Assigned(P)) then
    System.RegisterExpectedMemoryLeak(Pointer(NativeInt(P) - SizeOf(TDynArrayRec)));
  {$endif}
end;

class procedure TRAIIHelper.UnregisterDynamicArray(const P: Pointer);
begin
  {$ifdef MSWINDOWS}
  if (Assigned(P)) then
    System.UnregisterExpectedMemoryLeak(Pointer(NativeInt(P) - SizeOf(TDynArrayRec)));
  {$endif}
end;
{$WARNINGS ON}

class procedure TRAIIHelper.ULStrClear(P: Pointer);
type
  PStrRec = ^StrRec;
  StrRec = packed record
    {$ifdef LARGEINT}
    _Padding: Integer;
    {$endif}
    codePage: Word;
    elemSize: Word;
    refCnt: Integer;
    length: Integer;
  end;
var
  Rec: PStrRec;
  RefCnt: Integer;
begin
  Rec := PPointer(P)^;
  Dec(Rec);
  RefCnt := Rec.refCnt;
  if (RefCnt > 0) then
  begin
    if (RefCnt = 1) or (AtomicDecrement(Rec.refCnt) = 0) then
      FreeMem(Rec);
  end;
end;

{$ifdef MSWINDOWS}
procedure SysFreeString(P: Pointer); stdcall; external 'oleaut32.dll';

class procedure TRAIIHelper.WStrClear(P: Pointer);
begin
  SysFreeString(PPointer(P)^);
end;
{$endif}

class procedure TRAIIHelper.IntfClear(P: Pointer);
begin
  IInterface(PPointer(P)^)._Release;
end;

class procedure TRAIIHelper.VarClear(P: Pointer);
var
  VType: Integer;
begin
  VType := PVarData(P).VType;
  if (VType and varDeepData <> 0) and (VType <> varBoolean) and
    (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
    System.VarClear(PVariant(P)^);
end;

class procedure TRAIIHelper.DynArrayClear(P, TypeInfo: Pointer);
var
  Rec: PDynArrayRec;
  RefCnt: Integer;
begin
  Rec := PPointer(P)^;
  Dec(Rec);
  RefCnt := Rec.RefCnt;
  if (RefCnt > 0) then
  begin
    if (RefCnt = 1) or (AtomicDecrement(Rec.RefCnt) = 0) then
    begin
      Inc(PByte(TypeInfo), PByte(@PDynArrayTypeInfo(TypeInfo).name)^);
      TypeInfo := PDynArrayTypeInfo(TypeInfo).elType;
      if (TypeInfo <> nil) and (Rec.Length <> 0) then
      begin
        System.FinalizeArray(Pointer(NativeUInt(Rec) + SizeOf(TDynArrayRec)),
          PPointer(TypeInfo)^, Rec.Length);
      end;

      FreeMem(Rec);
    end;
  end;
end;

{$ifdef AUTOREFCOUNT}
class procedure TRAIIHelper.RefObjClear(P: Pointer);
begin
  TObject(PPointer(P)^).__ObjRelease;
end;
{$endif}

{$ifdef WEAKINSTREF}
class procedure TRAIIHelper.WeakObjClear(P: Pointer);
{$ifdef CPUINTELASM}
asm
  jmp System.@InstWeakClear
end;
{$else}
type
  TInstance = record
    [Weak] Obj: TObject;
  end;
  PInstance = ^TInstance;
begin
  PInstance(P).Obj := nil;
end;
{$endif}

class procedure TRAIIHelper.WeakMethodClear(P: Pointer);
{$ifdef CPUINTELASM}
asm
  {$ifdef CPUX86}
    sub eax, 4
  {$else}
    sub rcx, 8
  {$endif}
  jmp System.@ClosureRemoveWeakRef
end;
{$else}
type
  TInstance = record
    Method: procedure of object;
  end;
  PInstance = ^TInstance;
begin
  PInstance(NativeInt(P) - SizeOf(Pointer)).Method := nil;
end;
{$endif}
{$endif}

{$ifdef WEAKINTFREF}
class procedure TRAIIHelper.WeakIntfClear(P: Pointer);
{$ifdef CPUINTELASM}
asm
  jmp System.@IntfWeakClear
end;
{$else}
type
  TInstance = record
    [Weak] Intf: IInterface;
  end;
  PInstance = ^TInstance;
begin
  PInstance(P).Intf := nil;
end;
{$endif}
{$endif}

procedure TRAIIHelper.Include(AOffset: NativeInt; Value: PTypeInfo);
var
  i: Cardinal;
  {$ifdef WEAKREF}
  WeakMode: Boolean;
  {$endif}
  FieldTable: PFieldTable;
  ChildSize, ChildOffset: NativeInt;
begin
  case Value.Kind of
    tkVariant:
    begin
      {$ifdef WEAKINSTREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.VarClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.VarClear));
      {$endif}
    end;
    {$ifdef AUTOREFCOUNT}
    tkClass:
    begin
      {$ifdef WEAKINSTREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.RefObjClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.RefObjClear));
      {$endif}
    end;
    {$endif}
    {$ifdef WEAKINSTREF}
    tkMethod:
    begin
      Self.FWeak := True;
      Self.InitNatives.Add(AOffset);
      Self.InitNatives.Add(AOffset + SizeOf(Pointer));
      Self.ClearNatives.Add(AOffset + SizeOf(Pointer), nil, TClearNativeProc(@TRAIIHelper.WeakMethodClear));
    end;
    {$endif}
    {$ifdef MSWINDOWS}
    tkWString:
    begin
      {$ifdef WEAKINSTREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.WStrClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.WStrClear));
      {$endif}
    end;
    {$else}
    tkWString,
    {$endif}
    tkLString, tkUString:
    begin
      {$ifdef WEAKINSTREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.ULStrClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.ULStrClear));
      {$endif}
    end;
    tkInterface:
    begin
      {$ifdef WEAKINSTREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.IntfClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.IntfClear));
      {$endif}
    end;
    tkDynArray:
    begin
      {$ifdef WEAKINSTREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, Value, TClearNativeProc(@TRAIIHelper.DynArrayClear));
      {$else}
        Self.Natives.Add(AOffset, Value, TClearNativeProc(@TRAIIHelper.DynArrayClear));
      {$endif}
    end;
    tkArray{static array}:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if (FieldTable.Fields[0].TypeInfo <> nil) and IsManagedTypeInfo(FieldTable.Fields[0].TypeInfo^) then
      begin
        if (FieldTable.Count > 4) then
        begin
          Self.StaticArrays.Add(AOffset, FieldTable.Fields[0].TypeInfo^, FieldTable.Count);
        end else
        begin
          ChildSize := FieldTable.Size div FieldTable.Count;
          for i := 1 to FieldTable.Count do
          begin
            Self.Include(AOffset, FieldTable.Fields[0].TypeInfo^);
            Inc(AOffset, ChildSize);
          end;
        end;
      end;
    end;
    tkRecord:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if FieldTable.Count > 0 then
      begin
        {$ifdef WEAKREF}
        WeakMode := False;
        {$endif}
        for i := 0 to FieldTable.Count - 1 do
        begin
          ChildOffset := AOffset + NativeInt(FieldTable.Fields[i].Offset);
          {$ifdef WEAKREF}
          if FieldTable.Fields[i].TypeInfo = nil then
          begin
            WeakMode := True;
            Self.FWeak := True;
            Continue;
          end;
          if (not WeakMode) then
          begin
          {$endif}
            Self.Include(ChildOffset, FieldTable.Fields[i].TypeInfo^);
          {$ifdef WEAKREF}
          end else
          case FieldTable.Fields[i].TypeInfo^.Kind of
          {$ifdef WEAKINTFREF}
            tkInterface:
            begin
              {$ifdef WEAKINSTREF}
                Self.InitNatives.Add(ChildOffset);
                Self.ClearNatives.Add(ChildOffset, nil, TClearNativeProc(@TRAIIHelper.WeakIntfClear));
              {$else}
                Self.Natives.Add(ChildOffset, nil, TClearNativeProc(@TRAIIHelper.WeakIntfClear));
              {$endif}
            end;
          {$endif}
          {$ifdef WEAKINSTREF}
            tkClass:
            begin
              Self.InitNatives.Add(ChildOffset);
              Self.ClearNatives.Add(ChildOffset, nil, TClearNativeProc(@TRAIIHelper.WeakObjClear));
            end;
            tkMethod:
            begin
              Self.InitNatives.Add(ChildOffset);
              Self.InitNatives.Add(ChildOffset + SizeOf(Pointer));

              Self.ClearNatives.Add(ChildOffset + SizeOf(Pointer), nil, TClearNativeProc(@TRAIIHelper.WeakMethodClear));
            end;
          {$endif}
          end;
          {$endif .WEAKREF}
        end;
      end;
    end;
  end;
end;

function TRAIIHelper.GetTypeData: PTypeData;
begin
  Result := Pointer(FTypeInfo);
  Inc(NativeUInt(Result), NativeUInt(PByte(@PTypeInfo(Result).Name)^) + 2);
end;

procedure TRAIIHelper.Initialize(Value: PTypeInfo);
var
  FieldTable: PFieldTable;
  TypeData: PTypeData;
begin
  // clear
  FTypeInfo := Value;
  FItemSize := 0;
  FWeak := False;
  {$ifdef WEAKINSTREF}
  InitNatives.Clear;
  ClearNatives.Clear;
  {$else}
  Natives.Clear;
  {$endif}
  StaticArrays.Clear;
  InitProc := nil;
  ClearProc := nil;
  InitArrayProc := nil;
  ClearArrayProc := nil;

  // type data
  TypeData := Pointer(Value);
  Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);

  // type kind
  case Value.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
    begin
      case (TypeData.OrdType) of
        otSByte, otUByte: FSize := SizeOf(Byte);
        otSWord, otUWord: FSize := SizeOf(Word);
        otSLong, otULong: FSize := SizeOf(Cardinal);
      end;
      Exit;
    end;
    tkSet:
    begin
      TypeData := Pointer(TypeData.CompType^);
      Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);
      with TypeData^ do
      begin
        FSize := (((MaxValue + 7 + 1) and ($FF shl 3)) - (MinValue and ($FF shl 3))) shr 3;
        if (FSize = 3) then FSize := 4;
      end;
      Exit;
    end;
    tkFloat:
    begin
      case (TypeData.FloatType) of
        ftSingle: FSize := SizeOf(Single);
        ftDouble: FSize := SizeOf(Double);
      ftExtended: FSize := SizeOf(Extended);
      else
        FItemSize := -1;
        case (TypeData.FloatType) of
          ftComp: FSize := SizeOf(Comp);
          ftCurr: FSize := SizeOf(Currency);
        end;
      end;
      Exit;
    end;
    tkInt64:
    begin
      FSize := SizeOf(Int64);
      Exit;
    end;
    {$ifNdef NEXTGEN}
    tkString:
    begin
      FSize := TypeData.MaxLength + 1;
      Exit;
    end;
    {$endif}
    tkClassRef, tkPointer, tkProcedure:
    begin
      FSize := SizeOf(Pointer);
      Exit;
    end;

    tkVariant:
    begin
      FSize := SizeOf(Variant);
      Self.Include(0, Value);
    end;
    tkMethod:
    begin
      FSize := SizeOf(TMethod);
      Self.Include(0, Value);
    end;
    tkClass, tkLString, tkWString, tkInterface, tkUString:
    begin
      FSize := SizeOf(Pointer);
      Self.Include(0, Value);
    end;
    tkDynArray:
    begin
      FSize := SizeOf(Pointer);
      FItemSize := TypeData.elSize;
      Self.Include(0, Value);
    end;
    tkArray:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      FSize := FieldTable.Size;
      FItemSize := FieldTable.Size div FieldTable.Count;
      Self.Include(0, Value);
    end;
    tkRecord:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      FSize := FieldTable.Size;
      Self.Include(0, Value);
    end;
  else
    System.Error(reInvalidPtr);
  end;

  // initialization
  if (StaticArrays.Count <> 0) then
  begin
    InitProc := Self.InitsProc;
    InitArrayProc := Self.InitsArrayProc;
  end else
  case ({$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Count) of
    0: ;
    1:
    begin
      InitProc := Self.InitsProcNativeOne;
      InitArrayProc := Self.InitsArrayProcNativeOne;
    end;
    2:
    begin
      InitProc := Self.InitsProcNativeTwo;
      InitArrayProc := Self.InitsArrayProcNativeTwo;
    end;
    3:
    begin
      InitProc := Self.InitsProcNativeThree;
      InitArrayProc := Self.InitsArrayProcNativeThree;
    end;
  else
    InitProc := Self.InitsProcNatives;
    InitArrayProc := Self.InitsArrayProcNatives;
  end;

  // finalization
  if (StaticArrays.Count <> 0) then
  begin
    ClearProc := Self.ClearsProc;
    ClearArrayProc := Self.ClearsArrayProc;
  end else
  case ({$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Count) of
    0: ;
    1:
    begin
      ClearProc := Self.ClearsProcNativeOne;
      ClearArrayProc := Self.ClearsArrayProcNativeOne;
    end;
    2:
    begin
      ClearProc := Self.ClearsProcNativeTwo;
      ClearArrayProc := Self.ClearsArrayProcNativeTwo;
    end;
    3:
    begin
      ClearProc := Self.ClearsProcNativeThree;
      ClearArrayProc := Self.ClearsArrayProcNativeThree;
    end;
  else
    ClearProc := Self.ClearsProcNatives;
    ClearArrayProc := Self.ClearsArrayProcNatives;
  end;

  // dynamic arrays
  {$ifdef WEAKINSTREF}
  TRAIIHelper.RegisterDynamicArray(Pointer(InitNatives.Items));
  TRAIIHelper.RegisterDynamicArray(Pointer(ClearNatives.Items));
  {$else}
  TRAIIHelper.RegisterDynamicArray(Pointer(Natives.Items));
  {$endif}
end;

class function TRAIIHelper.IsManagedTypeInfo(Value: PTypeInfo): Boolean;
var
  i: Cardinal;
  {$ifdef WEAKREF}
  WeakMode: Boolean;
  {$endif}
  FieldTable: PFieldTable;
begin
  Result := False;

  if Assigned(Value) then
  case Value.Kind of
    tkVariant:
    begin
      Exit(True);
    end;
    {$ifdef AUTOREFCOUNT}
    tkClass:
    begin
      Exit(True);
    end;
    {$endif}
    {$ifdef WEAKINSTREF}
    tkMethod:
    begin
      Exit(True);
    end;
    {$endif}
    tkWString, tkLString, tkUString, tkInterface, tkDynArray:
    begin
      Exit(True);
    end;
    tkArray{static array}:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if (FieldTable.Fields[0].TypeInfo <> nil) then
        Result := IsManagedTypeInfo(FieldTable.Fields[0].TypeInfo^);
    end;
    tkRecord:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      if FieldTable.Count > 0 then
      begin
        {$ifdef WEAKREF}
        WeakMode := False;
        {$endif}
        for i := 0 to FieldTable.Count - 1 do
        begin
         {$ifdef WEAKREF}
          if FieldTable.Fields[i].TypeInfo = nil then
          begin
            WeakMode := True;
            Continue;
          end;
          if (not WeakMode) then
          begin
          {$endif}
            if (IsManagedTypeInfo(FieldTable.Fields[i].TypeInfo^)) then
              Exit(True);
          {$ifdef WEAKREF}
          end else
          begin
            Exit(True);
          end;
          {$endif}
        end;
      end;
    end;
  end;
end;

class function TRAIIHelper.InitsProcNativeOne(const Self: TRAIIHelper; P: Pointer): Pointer;
begin
  Inc(NativeInt(P), Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.ItemSingle.Offset);
  PNativeInt(P)^ := 0;
  Result := P;
end;

class procedure TRAIIHelper.InitsArrayProcNativeOne(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Offset: NativeInt;
  Null: NativeInt;
  LItemSize: NativeInt;
begin
  Offset := Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.ItemSingle.Offset;
  Inc(NativeInt(P), Offset);
  Inc(NativeInt(Overflow), Offset);

  LItemSize := ItemSize;
  Null := 0;
  if (P <> Overflow) then
  repeat
    PNativeInt(P)^ := Null;
    Inc(NativeUInt(P), LItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.InitsProcNativeTwo(const Self: TRAIIHelper; P: Pointer): Pointer;
var
  Null: NativeInt;
  Item: ^{$ifdef WEAKINSTREF}TInitNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);
  Null := 0;

  PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
  Inc(Item);
  PNativeInt(NativeInt(P) + Item.Offset)^ := Null;

  Result := P;
end;

class procedure TRAIIHelper.InitsArrayProcNativeTwo(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Ptr: PNativeInt;
  Item: ^{$ifdef WEAKINSTREF}TInitNativeRec{$else}TNativeRec{$endif};
  StoredItem: Pointer;
begin
  StoredItem := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);

  if (P <> Overflow) then
  repeat
    Item := StoredItem;

    Ptr := P;
    Inc(NativeInt(Ptr), Item.Offset);
    Ptr^ := 0;
    Inc(Item);
    Ptr := P;
    Inc(NativeInt(Ptr), Item.Offset);
    Ptr^ := 0;

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.InitsProcNativeThree(const Self: TRAIIHelper; P: Pointer): Pointer;
var
  Null: NativeInt;
  Item: ^{$ifdef WEAKINSTREF}TInitNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);
  Null := 0;

  PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
  Inc(Item);
  PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
  Inc(Item);
  PNativeInt(NativeInt(P) + Item.Offset)^ := Null;

  Result := P;
end;

class procedure TRAIIHelper.InitsArrayProcNativeThree(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Ptr: PNativeInt;
  Item: ^{$ifdef WEAKINSTREF}TInitNativeRec{$else}TNativeRec{$endif};
  StoredItem: Pointer;
begin
  StoredItem := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);

  if (P <> Overflow) then
  repeat
    Item := StoredItem;

    Ptr := P;
    Inc(NativeInt(Ptr), Item.Offset);
    Ptr^ := 0;
    Inc(Item);
    Ptr := P;
    Inc(NativeInt(Ptr), Item.Offset);
    Ptr^ := 0;
    Ptr := P;
    Inc(NativeInt(Ptr), Item.Offset);
    Ptr^ := 0;

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.InitsProcNatives(const Self: TRAIIHelper; P: Pointer): Pointer;
label
  _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11;
var
  Count, Null: NativeInt;
  Item: ^{$ifdef WEAKINSTREF}TInitNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);
  Count := Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Count;
  Null := 0;

  case Count of
    11:
    begin
    _11:
      Dec(Count, 10);
      repeat
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Dec(Count);
        Inc(Item);
      until (Count = 0);
      goto _10;
    end;
    10:
    begin
    _10:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _9;
    end;
    9:
    begin
    _9:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _8;
    end;
    8:
    begin
    _8:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _7;
    end;
    7:
    begin
    _7:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _6;
    end;
    6:
    begin
    _6:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _5;
    end;
    5:
    begin
    _5:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _4;
    end;
    4:
    begin
    _4:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _3;
    end;
    3:
    begin
    _3:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _2;
    end;
    2:
    begin
    _2:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      Inc(Item);
      goto _1;
    end;
    1:
    begin
    _1:
      PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
    end;
    0: ;
  else
    goto _11;
  end;

  Result := P;
end;

class procedure TRAIIHelper.InitsArrayProcNatives(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
label
  _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11;
var
  Count, Null: NativeInt;
  Item: ^{$ifdef WEAKINSTREF}TInitNativeRec{$else}TNativeRec{$endif};
  Stored: record
    Item: Pointer;
    Count: NativeInt;
  end;
begin
  Stored.Item := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);
  Stored.Count := Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Count;

  if (P <> Overflow) then
  repeat
    Item := Stored.Item;
    Count := Stored.Count;
    Null := 0;

    case Count of
      11:
      begin
      _11:
        Dec(Count, 10);
        repeat
          PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
          Dec(Count);
          Inc(Item);
        until (Count = 0);
        goto _10;
      end;
      10:
      begin
      _10:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _9;
      end;
      9:
      begin
      _9:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _8;
      end;
      8:
      begin
      _8:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _7;
      end;
      7:
      begin
      _7:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _6;
      end;
      6:
      begin
      _6:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _5;
      end;
      5:
      begin
      _5:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _4;
      end;
      4:
      begin
      _4:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _3;
      end;
      3:
      begin
      _3:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _2;
      end;
      2:
      begin
      _2:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Inc(Item);
        goto _1;
      end;
      1:
      begin
      _1:
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
      end;
      0: ;
    else
      goto _11;
    end;

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.InitsProc(const Self: TRAIIHelper; P: Pointer): Pointer;
var
  i: NativeInt;
  Item: ^{$ifdef WEAKINSTREF}TInitNativeRec{$else}TNativeRec{$endif};
  StaticArrayRec: ^TStaticArrayRec;
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);
  for i := 1 to Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Count do
  begin
    PNativeInt(NativeInt(P) + Item.Offset)^ := 0;
    Inc(Item);
  end;

  StaticArrayRec := Pointer(Self.StaticArrays.Items);
  for i := 1 to Self.StaticArrays.Count do
  begin
    System.InitializeArray(Pointer(NativeInt(P) + StaticArrayRec.Offset),
      StaticArrayRec.StaticTypeInfo, StaticArrayRec.Count);
    Inc(StaticArrayRec);
  end;

  Result := P;
end;

class procedure TRAIIHelper.InitsArrayProc(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Item, OverflowItem: ^TClearNativeRec;
  StaticArrayRec, OverflowStaticArrayRec: ^TStaticArrayRec;
begin
  if (P <> Overflow) then
  repeat
    Item := Pointer(Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Items);
    OverflowItem := Item + Self.{$ifdef WEAKINSTREF}InitNatives{$else}Natives{$endif}.Count;
    if (Item <> OverflowItem) then
    repeat
      PNativeInt(NativeInt(P) + Item.Offset)^ := 0;
      Inc(Item);
    until (Item = OverflowItem);

    StaticArrayRec := Pointer(Self.StaticArrays.Items);
    OverflowStaticArrayRec := StaticArrayRec + Self.StaticArrays.Count;
    if (StaticArrayRec <> OverflowStaticArrayRec) then
    repeat
      System.InitializeArray(Pointer(NativeInt(P) + StaticArrayRec.Offset),
        StaticArrayRec.StaticTypeInfo, StaticArrayRec.Count);
      Inc(StaticArrayRec);
    until (StaticArrayRec = OverflowStaticArrayRec);

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.ClearsProcNativeOne(const Self: TRAIIHelper; P: Pointer): Pointer;
var
  Value: PNativeInt;
begin
  Value := Pointer(NativeInt(P) + Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.ItemSingle.Offset);
  if (Value^ <> 0) then
  begin
    Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.ItemSingle.ClearNativeProc(Value,
      Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.ItemSingle.DynTypeInfo);
  end;

  Result := P;
end;

class procedure TRAIIHelper.ClearsArrayProcNativeOne(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Offset: NativeInt;
  ClearNativeProc: TClearNativeProc;
  DynTypeInfo: PTypeInfo;
  Value: PNativeInt;
begin
  Offset := Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.ItemSingle.Offset;
  ClearNativeProc := Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.ItemSingle.ClearNativeProc;
  DynTypeInfo := Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.ItemSingle.DynTypeInfo;
  if (P <> Overflow) then
  repeat
    Value := Pointer(NativeInt(P) + Offset);
    if (Value^ <> 0) then
      ClearNativeProc(Value, DynTypeInfo);

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.ClearsProcNativeTwo(const Self: TRAIIHelper; P: Pointer): Pointer;
var
  Value: PNativeInt;
  Item: ^{$ifdef WEAKINSTREF}TClearNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);

  Value := Pointer(NativeInt(P) + Item.Offset);
  if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
  Inc(Item);
  Value := Pointer(NativeInt(P) + Item.Offset);
  if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);

  Result := P;
end;

class procedure TRAIIHelper.ClearsArrayProcNativeTwo(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Value: PNativeInt;
  Item: ^{$ifdef WEAKINSTREF}TClearNativeRec{$else}TNativeRec{$endif};
  StoredItem: Pointer;
begin
  StoredItem := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);

  if (P <> Overflow) then
  repeat
    Item := StoredItem;

    Value := Pointer(NativeInt(P) + Item.Offset);
    if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
    Inc(Item);
    Value := Pointer(NativeInt(P) + Item.Offset);
    if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.ClearsProcNativeThree(const Self: TRAIIHelper; P: Pointer): Pointer;
var
  Value: PNativeInt;
  Item: ^{$ifdef WEAKINSTREF}TClearNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);

  Value := Pointer(NativeInt(P) + Item.Offset);
  if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
  Inc(Item);
  Value := Pointer(NativeInt(P) + Item.Offset);
  if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
  Inc(Item);
  Value := Pointer(NativeInt(P) + Item.Offset);
  if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);

  Result := P;
end;

class procedure TRAIIHelper.ClearsArrayProcNativeThree(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Value: PNativeInt;
  Item: ^{$ifdef WEAKINSTREF}TClearNativeRec{$else}TNativeRec{$endif};
  StoredItem: Pointer;
begin
  StoredItem := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);

  if (P <> Overflow) then
  repeat
    Item := StoredItem;

    Value := Pointer(NativeInt(P) + Item.Offset);
    if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
    Inc(Item);
    Value := Pointer(NativeInt(P) + Item.Offset);
    if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
    Inc(Item);
    Value := Pointer(NativeInt(P) + Item.Offset);
    if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.ClearsProcNatives(const Self: TRAIIHelper; P: Pointer): Pointer;
label
  _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11;
var
  Count: NativeInt;
  Value: PNativeInt;
  Item: ^{$ifdef WEAKINSTREF}TClearNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);
  Count := Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Count;

  case Count of
    11:
    begin
    _11:
      Dec(Count, 10);
      repeat
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Dec(Count);
        Inc(Item);
      until (Count = 0);
      goto _10;
    end;
    10:
    begin
    _10:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _9;
    end;
    9:
    begin
    _9:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _8;
    end;
    8:
    begin
    _8:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _7;
    end;
    7:
    begin
    _7:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _6;
    end;
    6:
    begin
    _6:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _5;
    end;
    5:
    begin
    _5:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _4;
    end;
    4:
    begin
    _4:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _3;
    end;
    3:
    begin
    _3:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _2;
    end;
    2:
    begin
    _2:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      Inc(Item);
      goto _1;
    end;
    1:
    begin
    _1:
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
    end;
    0: ;
  else
    goto _11;
  end;

  Result := P;
end;

class procedure TRAIIHelper.ClearsArrayProcNatives(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
label
  _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11;
var
  Count: NativeInt;
  Value: PNativeInt;
  Item, OverflowItem: ^{$ifdef WEAKINSTREF}TClearNativeRec{$else}TNativeRec{$endif};
  Stored: record
    Item: Pointer;
    Count: NativeInt;
  end;
begin
  Stored.Item := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);
  Stored.Count := Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Count;

  if (P <> Overflow) then
  repeat
    Item := Stored.Item;
    Count := Stored.Count;

    case Count of
      11:
      begin
      _11:
        OverflowItem := Item + (Count - 10);
        repeat
          Value := Pointer(NativeInt(P) + Item.Offset);
          if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
          Inc(Item);
        until (Item = OverflowItem);
        goto _10;
      end;
      10:
      begin
      _10:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _9;
      end;
      9:
      begin
      _9:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _8;
      end;
      8:
      begin
      _8:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _7;
      end;
      7:
      begin
      _7:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _6;
      end;
      6:
      begin
      _6:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _5;
      end;
      5:
      begin
      _5:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _4;
      end;
      4:
      begin
      _4:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _3;
      end;
      3:
      begin
      _3:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _2;
      end;
      2:
      begin
      _2:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Inc(Item);
        goto _1;
      end;
      1:
      begin
      _1:
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
      end;
      0: ;
    else
      goto _11;
    end;

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

class function TRAIIHelper.ClearsProc(const Self: TRAIIHelper; P: Pointer): Pointer;
var
  i: NativeInt;
  Value: PNativeInt;
  Item: ^TClearNativeRec;
  StaticArrayRec: ^TStaticArrayRec;
begin
  Item := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);
  for i := 1 to Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Count do
  begin
    Value := Pointer(NativeInt(P) + Item.Offset);
    if (Value^ <> 0) then
    begin
      Item.ClearNativeProc(Value, Item.DynTypeInfo);
    end;
    Inc(Item);
  end;

  StaticArrayRec := Pointer(Self.StaticArrays.Items);
  for i := 1 to Self.StaticArrays.Count do
  begin
    System.FinalizeArray(Pointer(NativeInt(P) + StaticArrayRec.Offset),
      StaticArrayRec.StaticTypeInfo, StaticArrayRec.Count);
    Inc(StaticArrayRec);
  end;

  Result := P;
end;

class procedure TRAIIHelper.ClearsArrayProc(const Self: TRAIIHelper;
  P, Overflow: Pointer; ItemSize: NativeUInt);
var
  Value: PNativeInt;
  Item, OverflowItem: ^TClearNativeRec;
  StaticArrayRec, OverflowStaticArrayRec: ^TStaticArrayRec;
begin
  if (P <> Overflow) then
  repeat
    Item := Pointer(Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Items);
    OverflowItem := Item + Self.{$ifdef WEAKINSTREF}ClearNatives{$else}Natives{$endif}.Count;
    if (Item <> OverflowItem) then
    repeat
      Value := Pointer(NativeInt(P) + Item.Offset);
      if (Value^ <> 0) then
      begin
        Item.ClearNativeProc(Value, Item.DynTypeInfo);
      end;
      Inc(Item);
    until (Item = OverflowItem);

    StaticArrayRec := Pointer(Self.StaticArrays.Items);
    OverflowStaticArrayRec := StaticArrayRec + Self.StaticArrays.Count;
    if (StaticArrayRec <> OverflowStaticArrayRec) then
    repeat
      System.FinalizeArray(Pointer(NativeInt(P) + StaticArrayRec.Offset),
        StaticArrayRec.StaticTypeInfo, StaticArrayRec.Count);
      Inc(StaticArrayRec);
    until (StaticArrayRec = OverflowStaticArrayRec);

    Inc(NativeUInt(P), ItemSize);
  until (P = Overflow);
end;

{ TRAIIHelper<T> }

class procedure TRAIIHelper<T>.Create;
begin
  if (not FCreated) then
    InternalCreate;
end;

class procedure TRAIIHelper<T>.InternalCreate;
var
  Yield: TSyncYield;
  Spinlock: PSyncSpinlock;
begin
  Spinlock := Pointer(@FSpinlock);
  if (Spinlock.TryEnter) then
  begin
    try
      if (not FCreated) then
      begin
        FOptions.TypeInfo := TypeInfo(T);
        FCreated := True;
      end;
    finally
      Spinlock.Leave;
    end;
  end else
  begin
    Yield := TSyncYield.Create;
    repeat
      Yield.Execute;
    until (FCreated);
  end;
end;

class function TRAIIHelper<T>.GetManaged: Boolean;
begin
  {$ifdef SMARTGENERICS}
  Result := System.IsManagedType(T);
  {$else}
  Result := Assigned(FOptions.ClearProc);
  {$endif}
end;

class function TRAIIHelper<T>.GetWeak: Boolean;
begin
  {$ifdef WEAKREF}
    {$ifdef SMARTGENERICS}
      Result := System.HasWeakRef(T) {$ifNdef WEAKINSTREF}and (GetTypeKind(T) <> tkMethod){$endif};
    {$else}
      Result := FOptions.FWeak;
    {$endif}
  {$else}
    Result := False;
  {$endif}
end;

class function TRAIIHelper<T>.Init(Item: Pointer): Pointer;
var
  LNull: NativeInt;
begin
  Result := Item;

  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$else}
  if (Assigned(FOptions.InitProc)) then
  {$endif}
  begin
    {$ifdef SMARTGENERICS}
    if (GetTypeKind(T) = tkVariant) or (SizeOf(T) <= 16) then
    begin
      LNull := 0;

      if (GetTypeKind(T) = tkVariant) then
      begin
        TData(Result^).Integers[0] := LNull;
      end else
      begin
        {$ifdef SMALLINT}
          if (SizeOf(T) >= SizeOf(Integer) * 1) then TData(Result^).Integers[0] := LNull;
          if (SizeOf(T) >= SizeOf(Integer) * 2) then TData(Result^).Integers[1] := LNull;
          if (SizeOf(T) >= SizeOf(Integer) * 3) then TData(Result^).Integers[2] := LNull;
          if (SizeOf(T)  = SizeOf(Integer) * 4) then TData(Result^).Integers[3] := LNull;
        {$else .LARGEINT}
          if (SizeOf(T) >= SizeOf(Int64) * 1) then TData(Result^).Int64s[0] := LNull;
          if (SizeOf(T)  = SizeOf(Int64) * 2) then TData(Result^).Int64s[1] := LNull;
          case SizeOf(T) of
             4..7: TData(Result^).Integers[0] := LNull;
           12..15: TData(Result^).Integers[2] := LNull;
          end;
        {$endif}
        case SizeOf(T) of
           2,3: TData(Result^).Words[0] := 0;
           6,7: TData(Result^).Words[2] := 0;
         10,11: TData(Result^).Words[4] := 0;
         14,15: TData(Result^).Words[6] := 0;
        end;
        case SizeOf(T) of
           1: TData(Result^).Bytes[ 1-1] := 0;
           3: TData(Result^).Bytes[ 3-1] := 0;
           5: TData(Result^).Bytes[ 5-1] := 0;
           7: TData(Result^).Bytes[ 7-1] := 0;
           9: TData(Result^).Bytes[ 9-1] := 0;
          11: TData(Result^).Bytes[11-1] := 0;
          13: TData(Result^).Bytes[13-1] := 0;
          15: TData(Result^).Bytes[15-1] := 0;
        end;
      end;
    end else
    {$else}
    if (SizeOf(T) <= 16) then
    begin
      LNull := 0;
      Inc(NativeInt(Result), SizeOf(T) - SizeOf(NativeInt));
      NativeInt(Result^) := LNull;
      Dec(NativeInt(Result), SizeOf(T) - SizeOf(NativeInt));
      {$ifdef SMALLINT}
        if (SizeOf(T) > SizeOf(Integer) * 1) then TData(Result^).Integers[0] := LNull;
        if (SizeOf(T) > SizeOf(Integer) * 2) then TData(Result^).Integers[1] := LNull;
        if (SizeOf(T) > SizeOf(Integer) * 3) then TData(Result^).Integers[2] := LNull;
      {$else .LARGEINT}
        if (SizeOf(T) > SizeOf(Int64) * 1) then TData(Result^).Int64s[0] := LNull;
      {$endif}
    end else
    {$endif}
    begin
      Result := FOptions.InitProc(FOptions, Result);
    end;
  end;
end;

class procedure TRAIIHelper<T>.Clear(Item: Pointer);
{$ifNdef SMARTGENERICS}
begin
  if (Assigned(FOptions.InitProc)) then
    FOptions.InitProc(FOptions, Item);
end;
{$else .SMARTGENERICS}
var
  VType: Integer;
begin
  if (System.IsManagedType(T)) then
  begin
    if (not (GetTypeKind(T) in [tkArray, tkRecord])) then
    begin
      case GetTypeKind(T) of
        {$ifdef AUTOREFCOUNT}
        tkClass,
        {$endif}
        tkWString, tkLString, tkUString, tkInterface, tkDynArray:
        begin
          if (TData(Item^).Native <> 0) then
          case GetTypeKind(T) of
            {$ifdef AUTOREFCOUNT}
            tkClass: TRAIIHelper.RefObjClear(@TData(Item^).Native);
            {$endif}
            {$ifdef MSWINDOWS}
            tkWString: TRAIIHelper.WStrClear(@TData(Item^).Native);
            {$else}
            tkWString,
            {$endif}
            tkLString, tkUString: TRAIIHelper.ULStrClear(@TData(Item^).Native);
            tkInterface: IInterface(Pointer(@TData(Item^).Native))._Release;
            tkDynArray: TRAIIHelper.DynArrayClear(@TData(Item^).Native, TypeInfo(T));
          end;
        end;
        {$ifdef WEAKINSTREF}
        tkMethod:
        begin
          if (TData(Item^).Natives[1] <> 0) then
            TRAIIHelper.WeakMethodClear(@TData(Item^).Natives[1]);
        end;
        {$endif}
        tkVariant:
        begin
          VType := Word(Item^);
          if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
            (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
            System.VarClear(Variant(Item^));
        end;
      end;
    end else
    begin
      FOptions.InitProc(FOptions, Item);
    end;
  end;
end;
{$endif}

class function TRAIIHelper<T>.ClearItem(Item: Pointer): Pointer;
{$ifNdef SMARTGENERICS}
begin
  Result := Item;
  if (Assigned(FOptions.InitProc)) then
    Result := FOptions.InitProc(FOptions, Result);
end;
{$else .SMARTGENERICS}
var
  VType: Integer;
begin
  if (System.IsManagedType(T)) then
  begin
    if (not (GetTypeKind(T) in [tkArray, tkRecord])) then
    begin
      case GetTypeKind(T) of
        {$ifdef AUTOREFCOUNT}
        tkClass,
        {$endif}
        tkWString, tkLString, tkUString, tkInterface, tkDynArray:
        begin
          if (TData(Item^).Native <> 0) then
          case GetTypeKind(T) of
            {$ifdef AUTOREFCOUNT}
            tkClass: TRAIIHelper.RefObjClear(@TData(Item^).Native);
            {$endif}
            {$ifdef MSWINDOWS}
            tkWString: TRAIIHelper.WStrClear(@TData(Item^).Native);
            {$else}
            tkWString,
            {$endif}
            tkLString, tkUString: TRAIIHelper.ULStrClear(@TData(Item^).Native);
            tkInterface: IInterface(Pointer(@TData(Item^).Native))._Release;
            tkDynArray: TRAIIHelper.DynArrayClear(@TData(Item^).Native, TypeInfo(T));
          end;
        end;
        {$ifdef WEAKINSTREF}
        tkMethod:
        begin
          if (TData(Item^).Natives[1] <> 0) then
            TRAIIHelper.WeakMethodClear(@TData(Item^).Natives[1]);
        end;
        {$endif}
        tkVariant:
        begin
          VType := Word(Item^);
          if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
            (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
            System.VarClear(Variant(Item^));
        end;
      end;

      Result := Item;
    end else
    begin
      Result := FOptions.InitProc(FOptions, Item);
    end;
  end else
  begin
    Result := Item;
  end;
end;
{$endif}

class procedure TRAIIHelper<T>.InitArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt);
const
  FILLZERO_ITEM_SIZE = {$ifdef SMARTGENERICS}3 * SizeOf(Pointer) - 1{$else}16{$endif};
{$ifdef SMARTGENERICS}
var
  LItemSize: NativeUInt;
{$endif}
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$else}
  if (Assigned(FOptions.InitProc)) then
  {$endif}
  if (Item <> OverflowItem) then
  begin
    if {$ifdef SMARTGENERICS}(SizeOf(T) <= FILLZERO_ITEM_SIZE) and{$endif}(ItemSize <= FILLZERO_ITEM_SIZE) then
    begin
      FillChar(Item^, NativeInt(OverflowItem) - NativeInt(Item), #0);
    end else
    {$ifdef SMARTGENERICS}
    if (GetTypeKind(T) = tkVariant) or (SizeOf(T) <= 16) then
    begin
      LItemSize := ItemSize;
      repeat
        Item := TRAIIHelper<T>.Init(Item);
        Inc(NativeUInt(Item), LItemSize);
      until (Item = OverflowItem);
    end else
    {$endif}
    begin
      FOptions.InitArrayProc(FOptions, Item, OverflowItem, ItemSize);
    end;
  end;
end;

class procedure TRAIIHelper<T>.InitArray(Item, OverflowItem: Pointer);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$endif}
    InitArray(Item, OverflowItem, SizeOf(T));
end;

class procedure TRAIIHelper<T>.InitArray(Item: Pointer; Count, ItemSize: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$endif}
    InitArray(Item, PByte(Item) + Count * ItemSize, ItemSize);
end;

class procedure TRAIIHelper<T>.InitArray(Item: Pointer; Count: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$endif}
    InitArray(Item, P(Item) + Count, SizeOf(T));
end;

class procedure TRAIIHelper<T>.ClearArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt);
{$ifdef SMARTGENERICS}
var
  LItemSize: NativeUInt;
{$endif}
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$else}
  if (Assigned(FOptions.ClearProc)) then
  {$endif}
  if (Item <> OverflowItem) then
  begin
    {$ifdef SMARTGENERICS}
    if (not (GetTypeKind(T) in [tkArray, tkRecord])) then
    begin
      LItemSize := ItemSize;
      repeat
        TRAIIHelper<T>.Clear(Item);
        Inc(NativeUInt(Item), LItemSize);
      until (Item = OverflowItem);
    end else
    {$endif}
    begin
      FOptions.ClearArrayProc(FOptions, Item, OverflowItem, ItemSize);
    end;
  end;
end;

class procedure TRAIIHelper<T>.ClearArray(Item, OverflowItem: Pointer);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$endif}
    ClearArray(Item, OverflowItem, SizeOf(T));
end;

class procedure TRAIIHelper<T>.ClearArray(Item: Pointer; Count, ItemSize: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$endif}
    ClearArray(Item, PByte(Item) + Count * ItemSize, ItemSize);
end;

class procedure TRAIIHelper<T>.ClearArray(Item: Pointer; Count: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$endif}
    ClearArray(Item, P(Item) + Count, SizeOf(T));
end;


{ TRAIIHelper<T1,T2,T3,T4> }

class procedure TRAIIHelper<T1,T2,T3,T4>.Create;
begin
  if (not FCreated) then
    InternalCreate;
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.InternalCreate;
begin
  TRAIIHelper<TRecord<T1,T2,T3,T4>>.Create;
  TRAIIHelper<T1>.Create;
  TRAIIHelper<T2>.Create;
  TRAIIHelper<T3>.Create;
  TRAIIHelper<T4>.Create;
  FCreated := True;
end;

class function TRAIIHelper<T1,T2,T3,T4>.GetManaged: Boolean;
begin
  {$ifdef SMARTGENERICS}
  Result := System.IsManagedType(TRecord<T1,T2,T3,T4>);
  {$else}
  Result := Assigned(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.ClearProc);
  {$endif}
end;

class function TRAIIHelper<T1,T2,T3,T4>.GetWeak: Boolean;
begin
  {$ifdef WEAKREF}
    {$ifdef SMARTGENERICS}
      {$ifdef WEAKINSTREF}
        Result := System.HasWeakRef(TRecord<T1,T2,T3,T4>);
      {$else}
        Result := (System.HasWeakRef(T1) and (GetTypeKind(T1) <> tkMethod)) or
          (System.HasWeakRef(T2) and (GetTypeKind(T2) <> tkMethod)) or
          (System.HasWeakRef(T3) and (GetTypeKind(T3) <> tkMethod)) or
          (System.HasWeakRef(T4) and (GetTypeKind(T4) <> tkMethod));
      {$endif}
    {$else}
      Result := TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.FWeak;
    {$endif}
  {$else}
    Result := False;
  {$endif}
end;

class function TRAIIHelper<T1,T2,T3,T4>.GetOptions: PRAIIHelper;
begin
  Result := @TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions;
end;

class function TRAIIHelper<T1,T2,T3,T4>.Init(Item: Pointer): Pointer;
var
  LNull: NativeInt;
begin
  Result := Item;

  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$else}
  if (Assigned(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.InitProc)) then
  {$endif}
  begin
    {$ifdef SMARTGENERICS}
    if (not System.IsManagedType(T1) or (GetTypeKind(T1) = tkVariant) or (SizeOf(T1) <= 16)) and
      (not System.IsManagedType(T2) or (GetTypeKind(T2) = tkVariant) or (SizeOf(T2) <= 16)) and
      (not System.IsManagedType(T3) or (GetTypeKind(T3) = tkVariant) or (SizeOf(T3) <= 16)) and
      (not System.IsManagedType(T4) or (GetTypeKind(T4) = tkVariant) or (SizeOf(T4) <= 16)) then
    begin
      LNull := 0;

      if (System.IsManagedType(T1)) then
      begin
        if (GetTypeKind(T1) = tkVariant) then
        begin
          TData1(Result^).Integers[0] := LNull;
        end else
        begin
          {$ifdef SMALLINT}
            if (SizeOf(T1) >= SizeOf(Integer) * 1) then TData1(Result^).Integers[0] := LNull;
            if (SizeOf(T1) >= SizeOf(Integer) * 2) then TData1(Result^).Integers[1] := LNull;
            if (SizeOf(T1) >= SizeOf(Integer) * 3) then TData1(Result^).Integers[2] := LNull;
            if (SizeOf(T1)  = SizeOf(Integer) * 4) then TData1(Result^).Integers[3] := LNull;
          {$else .LARGEINT}
            if (SizeOf(T1) >= SizeOf(Int64) * 1) then TData1(Result^).Int64s[0] := LNull;
            if (SizeOf(T1)  = SizeOf(Int64) * 2) then TData1(Result^).Int64s[1] := LNull;
            case SizeOf(T1) of
               4..7: TData1(Result^).Integers[0] := LNull;
             12..15: TData1(Result^).Integers[2] := LNull;
            end;
          {$endif}
          case SizeOf(T1) of
             2,3: TData1(Result^).Words[0] := 0;
             6,7: TData1(Result^).Words[2] := 0;
           10,11: TData1(Result^).Words[4] := 0;
           14,15: TData1(Result^).Words[6] := 0;
          end;
          case SizeOf(T1) of
             1: TData1(Result^).Bytes[ 1-1] := 0;
             3: TData1(Result^).Bytes[ 3-1] := 0;
             5: TData1(Result^).Bytes[ 5-1] := 0;
             7: TData1(Result^).Bytes[ 7-1] := 0;
             9: TData1(Result^).Bytes[ 9-1] := 0;
            11: TData1(Result^).Bytes[11-1] := 0;
            13: TData1(Result^).Bytes[13-1] := 0;
            15: TData1(Result^).Bytes[15-1] := 0;
          end;
        end;
      end;

      if (System.IsManagedType(T2)) then
      begin
        if (GetTypeKind(T2) = tkVariant) then
        begin
          TData2(Result^).Integers[0] := LNull;
        end else
        begin
          {$ifdef SMALLINT}
            if (SizeOf(T2) >= SizeOf(Integer) * 1) then TData2(Result^).Integers[0] := LNull;
            if (SizeOf(T2) >= SizeOf(Integer) * 2) then TData2(Result^).Integers[1] := LNull;
            if (SizeOf(T2) >= SizeOf(Integer) * 3) then TData2(Result^).Integers[2] := LNull;
            if (SizeOf(T2)  = SizeOf(Integer) * 4) then TData2(Result^).Integers[3] := LNull;
          {$else .LARGEINT}
            if (SizeOf(T2) >= SizeOf(Int64) * 1) then TData2(Result^).Int64s[0] := LNull;
            if (SizeOf(T2)  = SizeOf(Int64) * 2) then TData2(Result^).Int64s[1] := LNull;
            case SizeOf(T2) of
               4..7: TData2(Result^).Integers[0] := LNull;
             12..15: TData2(Result^).Integers[2] := LNull;
            end;
          {$endif}
          case SizeOf(T2) of
             2,3: TData2(Result^).Words[0] := 0;
             6,7: TData2(Result^).Words[2] := 0;
           10,11: TData2(Result^).Words[4] := 0;
           14,15: TData2(Result^).Words[6] := 0;
          end;
          case SizeOf(T2) of
             1: TData2(Result^).Bytes[ 1-1] := 0;
             3: TData2(Result^).Bytes[ 3-1] := 0;
             5: TData2(Result^).Bytes[ 5-1] := 0;
             7: TData2(Result^).Bytes[ 7-1] := 0;
             9: TData2(Result^).Bytes[ 9-1] := 0;
            11: TData2(Result^).Bytes[11-1] := 0;
            13: TData2(Result^).Bytes[13-1] := 0;
            15: TData2(Result^).Bytes[15-1] := 0;
          end;
        end;
      end;

      if (System.IsManagedType(T3)) then
      begin
        if (GetTypeKind(T3) = tkVariant) then
        begin
          TData3(Result^).Integers[0] := LNull;
        end else
        begin
          {$ifdef SMALLINT}
            if (SizeOf(T3) >= SizeOf(Integer) * 1) then TData3(Result^).Integers[0] := LNull;
            if (SizeOf(T3) >= SizeOf(Integer) * 2) then TData3(Result^).Integers[1] := LNull;
            if (SizeOf(T3) >= SizeOf(Integer) * 3) then TData3(Result^).Integers[2] := LNull;
            if (SizeOf(T3)  = SizeOf(Integer) * 4) then TData3(Result^).Integers[3] := LNull;
          {$else .LARGEINT}
            if (SizeOf(T3) >= SizeOf(Int64) * 1) then TData3(Result^).Int64s[0] := LNull;
            if (SizeOf(T3)  = SizeOf(Int64) * 2) then TData3(Result^).Int64s[1] := LNull;
            case SizeOf(T3) of
               4..7: TData3(Result^).Integers[0] := LNull;
             12..15: TData3(Result^).Integers[2] := LNull;
            end;
          {$endif}
          case SizeOf(T3) of
             2,3: TData3(Result^).Words[0] := 0;
             6,7: TData3(Result^).Words[2] := 0;
           10,11: TData3(Result^).Words[4] := 0;
           14,15: TData3(Result^).Words[6] := 0;
          end;
          case SizeOf(T3) of
             1: TData3(Result^).Bytes[ 1-1] := 0;
             3: TData3(Result^).Bytes[ 3-1] := 0;
             5: TData3(Result^).Bytes[ 5-1] := 0;
             7: TData3(Result^).Bytes[ 7-1] := 0;
             9: TData3(Result^).Bytes[ 9-1] := 0;
            11: TData3(Result^).Bytes[11-1] := 0;
            13: TData3(Result^).Bytes[13-1] := 0;
            15: TData3(Result^).Bytes[15-1] := 0;
          end;
        end;
      end;

      if (System.IsManagedType(T4)) then
      begin
        if (GetTypeKind(T4) = tkVariant) then
        begin
          TData4(Result^).Integers[0] := LNull;
        end else
        begin
          {$ifdef SMALLINT}
            if (SizeOf(T4) >= SizeOf(Integer) * 1) then TData4(Result^).Integers[0] := LNull;
            if (SizeOf(T4) >= SizeOf(Integer) * 2) then TData4(Result^).Integers[1] := LNull;
            if (SizeOf(T4) >= SizeOf(Integer) * 3) then TData4(Result^).Integers[2] := LNull;
            if (SizeOf(T4)  = SizeOf(Integer) * 4) then TData4(Result^).Integers[3] := LNull;
          {$else .LARGEINT}
            if (SizeOf(T4) >= SizeOf(Int64) * 1) then TData4(Result^).Int64s[0] := LNull;
            if (SizeOf(T4)  = SizeOf(Int64) * 2) then TData4(Result^).Int64s[1] := LNull;
            case SizeOf(T4) of
               4..7: TData4(Result^).Integers[0] := LNull;
             12..15: TData4(Result^).Integers[2] := LNull;
            end;
          {$endif}
          case SizeOf(T4) of
             2,3: TData4(Result^).Words[0] := 0;
             6,7: TData4(Result^).Words[2] := 0;
           10,11: TData4(Result^).Words[4] := 0;
           14,15: TData4(Result^).Words[6] := 0;
          end;
          case SizeOf(T4) of
             1: TData4(Result^).Bytes[ 1-1] := 0;
             3: TData4(Result^).Bytes[ 3-1] := 0;
             5: TData4(Result^).Bytes[ 5-1] := 0;
             7: TData4(Result^).Bytes[ 7-1] := 0;
             9: TData4(Result^).Bytes[ 9-1] := 0;
            11: TData4(Result^).Bytes[11-1] := 0;
            13: TData4(Result^).Bytes[13-1] := 0;
            15: TData4(Result^).Bytes[15-1] := 0;
          end;
        end;
      end;
    end else
    {$else}
    if (SizeOf(TRecord<T1,T2,T3,T4>) <= 16) then
    begin
      LNull := 0;
      Inc(NativeInt(Result), SizeOf(TRecord<T1,T2,T3,T4>) - SizeOf(NativeInt));
      NativeInt(Result^) := LNull;
      Dec(NativeInt(Result), SizeOf(TRecord<T1,T2,T3,T4>) - SizeOf(NativeInt));
      {$ifdef SMALLINT}
        if (SizeOf(TRecord<T1,T2,T3,T4>) > SizeOf(Integer) * 1) then TData1(Result^).Integers[0] := LNull;
        if (SizeOf(TRecord<T1,T2,T3,T4>) > SizeOf(Integer) * 2) then TData1(Result^).Integers[1] := LNull;
        if (SizeOf(TRecord<T1,T2,T3,T4>) > SizeOf(Integer) * 3) then TData1(Result^).Integers[2] := LNull;
      {$else .LARGEINT}
        if (SizeOf(TRecord<T1,T2,T3,T4>) > SizeOf(Int64) * 1) then TData1(Result^).Int64s[0] := LNull;
      {$endif}
    end else
    {$endif}
    begin
      Result := TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.InitProc(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions, Result);
    end;
  end;
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.Clear(Item: Pointer);
{$ifNdef SMARTGENERICS}
begin
  if (Assigned(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.ClearProc)) then
    TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.ClearProc(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions, Item);
end;
{$else .SMARTGENERICS}
var
  LData: PNativeUInt;
  VType: Integer;
begin
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  begin
    if (not System.IsManagedType(T1) or not (GetTypeKind(T1) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T2) or not (GetTypeKind(T2) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T3) or not (GetTypeKind(T3) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T4) or not (GetTypeKind(T4) in [tkArray, tkRecord])) then
    begin
      if (System.IsManagedType(T1)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T1) = tkMethod) then
          LData := @TData1(Item^).Natives[1]
        else
        {$endif}
          LData := @TData1(Item^).Natives[0];

        case GetTypeKind(T1) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T1) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T1));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;

      if (System.IsManagedType(T2)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T2) = tkMethod) then
          LData := @TData2(Item^).Natives[1]
        else
        {$endif}
          LData := @TData2(Item^).Natives[0];

        case GetTypeKind(T2) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T2) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T2));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;

      if (System.IsManagedType(T3)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T3) = tkMethod) then
          LData := @TData3(Item^).Natives[1]
        else
        {$endif}
          LData := @TData3(Item^).Natives[0];

        case GetTypeKind(T3) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T3) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T3));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;

      if (System.IsManagedType(T4)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T4) = tkMethod) then
          LData := @TData4(Item^).Natives[1]
        else
        {$endif}
          LData := @TData4(Item^).Natives[0];

        case GetTypeKind(T4) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T4) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T4));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;
    end else
    begin
      TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.InitProc(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions, Item);
    end;
  end;
end;
{$endif}

class function TRAIIHelper<T1,T2,T3,T4>.ClearItem(Item: Pointer): Pointer;
{$ifNdef SMARTGENERICS}
begin
  Result := Item;
  if (Assigned(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.ClearProc)) then
    Result := TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.ClearProc(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions, Result);
end;
{$else .SMARTGENERICS}
var
  LData: PNativeUInt;
  VType: Integer;
begin
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  begin
    if (not System.IsManagedType(T1) or not (GetTypeKind(T1) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T2) or not (GetTypeKind(T2) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T3) or not (GetTypeKind(T3) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T4) or not (GetTypeKind(T4) in [tkArray, tkRecord])) then
    begin
      if (System.IsManagedType(T1)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T1) = tkMethod) then
          LData := @TData1(Item^).Natives[1]
        else
        {$endif}
          LData := @TData1(Item^).Natives[0];

        case GetTypeKind(T1) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T1) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T1));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;

      if (System.IsManagedType(T2)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T2) = tkMethod) then
          LData := @TData2(Item^).Natives[1]
        else
        {$endif}
          LData := @TData2(Item^).Natives[0];

        case GetTypeKind(T2) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T2) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T2));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;

      if (System.IsManagedType(T3)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T3) = tkMethod) then
          LData := @TData3(Item^).Natives[1]
        else
        {$endif}
          LData := @TData3(Item^).Natives[0];

        case GetTypeKind(T3) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T3) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T3));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;

      if (System.IsManagedType(T4)) then
      begin
        {$ifdef WEAKINSTREF}
        if (GetTypeKind(T4) = tkMethod) then
          LData := @TData4(Item^).Natives[1]
        else
        {$endif}
          LData := @TData4(Item^).Natives[0];

        case GetTypeKind(T4) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          {$ifdef WEAKINSTREF}
          tkMethod,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (LData^ <> 0) then
            case GetTypeKind(T4) of
              {$ifdef AUTOREFCOUNT}
              tkClass: TRAIIHelper.RefObjClear(LData);
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString: TRAIIHelper.WStrClear(LData);
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString: TRAIIHelper.ULStrClear(LData);
              tkInterface: IInterface(Pointer(LData))._Release;
              tkDynArray: TRAIIHelper.DynArrayClear(LData, TypeInfo(T4));
              {$ifdef WEAKINSTREF}
              tkMethod: TRAIIHelper.WeakMethodClear(LData);
              {$endif}
            end;
          end;
          tkVariant:
          begin
            VType := Word(LData^);
            if (VType and TRAIIHelper.varDeepData <> 0) and (VType <> varBoolean) and
              (Cardinal(VType - (varUnknown + 1)) > (varUInt64 - varUnknown - 1)) then
              System.VarClear(Variant(Pointer(LData)^));
          end;
        end;
      end;

      Result := Item;
    end else
    begin
      Result := TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.InitProc(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions, Item);
    end;
  end else
  begin
    Result := Item;
  end;
end;
{$endif}

class procedure TRAIIHelper<T1,T2,T3,T4>.InitArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt);
const
  FILLZERO_ITEM_SIZE = {$ifdef SMARTGENERICS}3 * SizeOf(Pointer) - 1{$else}16{$endif};
{$ifdef SMARTGENERICS}
var
  LItemSize: NativeUInt;
{$endif}
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$else}
  if (Assigned(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.InitProc)) then
  {$endif}
  if (Item <> OverflowItem) then
  begin
    if {$ifdef SMARTGENERICS}(SizeOf(TRecord<T1,T2,T3,T4>) <= FILLZERO_ITEM_SIZE) and{$endif}(ItemSize <= FILLZERO_ITEM_SIZE) then
    begin
      FillChar(Item^, NativeInt(OverflowItem) - NativeInt(Item), #0);
    end else
    {$ifdef SMARTGENERICS}
    if (not System.IsManagedType(T1) or (GetTypeKind(T1) = tkVariant) or (SizeOf(T1) <= 16)) and
      (not System.IsManagedType(T2) or (GetTypeKind(T2) = tkVariant) or (SizeOf(T2) <= 16)) and
      (not System.IsManagedType(T3) or (GetTypeKind(T3) = tkVariant) or (SizeOf(T3) <= 16)) and
      (not System.IsManagedType(T4) or (GetTypeKind(T4) = tkVariant) or (SizeOf(T4) <= 16)) then
    begin
      LItemSize := ItemSize;
      repeat
        Item := TRAIIHelper<T1,T2,T3,T4>.Init(Item);
        Inc(NativeUInt(Item), LItemSize);
      until (Item = OverflowItem);
    end else
    {$endif}
    begin
      TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.InitArrayProc(
        TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions, Item, OverflowItem, ItemSize);
    end;
  end;
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.InitArray(Item, OverflowItem: Pointer);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$endif}
    InitArray(Item, OverflowItem, SizeOf(TRecord<T1,T2,T3,T4>));
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.InitArray(Item: Pointer; Count, ItemSize: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$endif}
    InitArray(Item, PByte(Item) + Count * ItemSize, ItemSize);
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.InitArray(Item: Pointer; Count: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$endif}
    InitArray(Item, P(Item) + Count, SizeOf(TRecord<T1,T2,T3,T4>));
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.ClearArray(Item, OverflowItem: Pointer; ItemSize: NativeUInt);
{$ifdef SMARTGENERICS}
var
  LItemSize: NativeUInt;
{$endif}
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$else}
  if (Assigned(TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.ClearProc)) then
  {$endif}
  if (Item <> OverflowItem) then
  begin
    {$ifdef SMARTGENERICS}
    if (not System.IsManagedType(T1) or not (GetTypeKind(T1) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T2) or not (GetTypeKind(T2) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T3) or not (GetTypeKind(T3) in [tkArray, tkRecord])) and
      (not System.IsManagedType(T4) or not (GetTypeKind(T4) in [tkArray, tkRecord])) then
    begin
      LItemSize := ItemSize;
      repeat
        TRAIIHelper<T1,T2,T3,T4>.Clear(Item);
        Inc(NativeUInt(Item), LItemSize);
      until (Item = OverflowItem);
    end else
    {$endif}
    begin
      TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions.ClearArrayProc(
        TRAIIHelper<TRecord<T1,T2,T3,T4>>.FOptions, Item, OverflowItem, ItemSize);
    end;
  end;
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.ClearArray(Item, OverflowItem: Pointer);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$endif}
    ClearArray(Item, OverflowItem, SizeOf(TRecord<T1,T2,T3,T4>));
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.ClearArray(Item: Pointer; Count, ItemSize: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$endif}
    ClearArray(Item, PByte(Item) + Count * ItemSize, ItemSize);
end;

class procedure TRAIIHelper<T1,T2,T3,T4>.ClearArray(Item: Pointer; Count: NativeUInt);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TRecord<T1,T2,T3,T4>)) then
  {$endif}
    ClearArray(Item, P(Item) + Count, SizeOf(TRecord<T1,T2,T3,T4>));
end;


{ InterfaceDefaults }

class function InterfaceDefaults.TDefaultComparer<T>.Create: Pointer;
begin
  if (not Created) then InternalCreate;
  Result := @Instance;
end;

class procedure InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
var
  TypeData: PTypeData;
begin
  Instance.Vtable := @Instance.QueryInterface;
  Instance.Size := SizeOf(T);
  Instance.QueryInterface := @InterfaceDefaults.NopQueryInterface;
  Instance.AddRef := @InterfaceDefaults.NopAddRef;
  Instance.Release := @InterfaceDefaults.NopRelease;

  // Compare
  TypeData := Pointer(TypeInfo(T));
  Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);
  case {$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} of
    tkInteger, tkEnumeration, tkChar, tkWChar:
    case TypeData.OrdType of
      otSByte: Instance.Compare := @InterfaceDefaults.Compare_I1;
      otUByte: Instance.Compare := @InterfaceDefaults.Compare_U1;
      otSWord: Instance.Compare := @InterfaceDefaults.Compare_I2;
      otUWord: Instance.Compare := @InterfaceDefaults.Compare_U2;
      otSLong: Instance.Compare := @InterfaceDefaults.Compare_I4;
      otULong: Instance.Compare := @InterfaceDefaults.Compare_U4;
    end;
    tkInt64:
    begin
      if (TypeData.MaxInt64Value > TypeData.MinInt64Value) then
      begin
        Instance.Compare := @InterfaceDefaults.Compare_I8
      end else
      begin
        Instance.Compare := @InterfaceDefaults.Compare_U8;
      end;
    end;
    tkClass, tkInterface, tkClassRef, tkPointer, tkProcedure:
    begin
      {$ifdef LARGEINT}
        Instance.Compare := @InterfaceDefaults.Compare_U8;
      {$else .SMALLINT}
        Instance.Compare := @InterfaceDefaults.Compare_U4;
      {$endif}
    end;
    tkFloat:
    case TypeData.FloatType of
        ftSingle: Instance.Compare := @InterfaceDefaults.Compare_F4;
        ftDouble: Instance.Compare := @InterfaceDefaults.Compare_F8;
      ftExtended: Instance.Compare := @InterfaceDefaults.Compare_FE;
    else
      Instance.Compare := @InterfaceDefaults.Compare_U8;
    end;
    tkMethod:
    begin
      Instance.Compare := @InterfaceDefaults.Compare_Method;
    end;
    tkVariant:
    begin
      Instance.Compare := @InterfaceDefaults.Compare_Var;
    end;
    tkString:
    begin
      Instance.Compare := @InterfaceDefaults.Compare_OStr;
    end;
    tkLString:
    begin
      Instance.Compare := @InterfaceDefaults.Compare_LStr;
    end;
    tkWString:
    begin
      Instance.Compare := @InterfaceDefaults.Compare_WStr;
    end;
    tkUString:
    begin
      Instance.Compare := @InterfaceDefaults.Compare_UStr;
    end;
    tkDynArray:
    begin
      Instance.Size := TypeData.elSize;
      Instance.Compare := @InterfaceDefaults.Compare_Dyn;
    end;
  else
    // binary
    case SizeOf(T) of
      1: Instance.Compare := @InterfaceDefaults.Compare_U1;
      2: Instance.Compare := @InterfaceDefaults.Compare_Bin2;
      3: Instance.Compare := @InterfaceDefaults.Compare_Bin3;
      4: Instance.Compare := @InterfaceDefaults.Compare_Bin4;
      {$ifdef LARGEINT}
      8: Instance.Compare := @InterfaceDefaults.Compare_Bin8;
      {$endif}
    else
      Instance.Compare := @InterfaceDefaults.Compare_Bin;
    end;
  end;

  Created := True;
end;

class function InterfaceDefaults.TDefaultEqualityComparer<T>.Create: Pointer;
begin
  if (not Created) then InternalCreate;
  Result := @Instance;
end;

class procedure InterfaceDefaults.TDefaultEqualityComparer<T>.InternalCreate;
var
  TypeData: PTypeData;
begin
  Instance.Vtable := @Instance.QueryInterface;
  Instance.Size := SizeOf(T);
  Instance.QueryInterface := @InterfaceDefaults.NopQueryInterface;
  Instance.AddRef := @InterfaceDefaults.NopAddRef;
  Instance.Release := @InterfaceDefaults.NopRelease;

  // Equals/GetHashCode
  TypeData := Pointer(TypeInfo(T));
  Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);
  case {$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} of
    tkClass:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_Class;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_Class;
    end;
    tkInterface, tkClassRef, tkPointer, tkProcedure:
    begin
      {$ifdef LARGEINT}
        Instance.Equals := @InterfaceDefaults.Equals_N8;
      {$else .SMALLINT}
        Instance.Equals := @InterfaceDefaults.Equals_N4;
      {$endif}
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_Ptr;
    end;
    tkInt64:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_N8;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_N8;
    end;
    tkFloat:
    case TypeData.FloatType of
      ftSingle:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_F4;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_F4;
      end;
      ftDouble:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_F8;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_F8;
      end;
      ftExtended:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_FE;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_FE;
      end;
    else
      Instance.Equals := @InterfaceDefaults.Equals_N8;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_N8;
    end;
    tkMethod:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_Method;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_Method;
    end;
    tkVariant:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_Var;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_Var;
    end;
    tkString:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_OStr;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_OStr;
    end;
    tkLString:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_LStr;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_LStr;
    end;
    tkWString:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_WStr;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_WStr;
    end;
    tkUString:
    begin
      Instance.Equals := @InterfaceDefaults.Equals_UStr;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_UStr;
    end;
    tkDynArray:
    begin
      Instance.Size := TypeData.elSize;
      Instance.Equals := @InterfaceDefaults.Equals_Dyn;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_Dyn;
    end;
  else
    // binary
    case SizeOf(T) of
      1:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_N1;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_N1;
      end;
      2:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_N2;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_N2;
      end;
      3:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_Bin3;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_Bin3;
      end;
      4:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_N4;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_N4;
      end;
      {$ifdef LARGEINT}
      8:
      begin
        Instance.Equals := @InterfaceDefaults.Equals_N8;
        Instance.GetHashCode := @InterfaceDefaults.GetHashCode_N8;
      end;
      {$endif}
    else
      Instance.Equals := @InterfaceDefaults.Equals_Bin;
      Instance.GetHashCode := @InterfaceDefaults.GetHashCode_Bin;
    end;
  end;

  Created := True;
end;

class function InterfaceDefaults.NopQueryInterface(Inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

class function InterfaceDefaults.NopAddRef(Inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

class function InterfaceDefaults.NopRelease(Inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

class function InterfaceDefaults.Compare_I1(Inst: Pointer; Left, Right: Shortint): Integer;
begin
  Result := Integer(Left) - Integer(Right);
end;

class function InterfaceDefaults.Compare_U1(Inst: Pointer; Left, Right: Byte): Integer;
begin
  Result := Integer(Left) - Integer(Right);
end;

class function InterfaceDefaults.Equals_N1(Inst: Pointer; Left, Right: Byte): Boolean;
begin
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_N1(Inst: Pointer; Value: Byte): Integer;
begin
  Result := Value;
  Inc(Result, + (Result shr 4) * 63689);
end;

class function InterfaceDefaults.Compare_I2(Inst: Pointer; Left, Right: Smallint): Integer;
begin
  Result := Integer(Left) - Integer(Right);
end;

class function InterfaceDefaults.Compare_U2(Inst: Pointer; Left, Right: Word): Integer;
begin
  Result := Integer(Left) - Integer(Right);
end;

class function InterfaceDefaults.Equals_N2(Inst: Pointer; Left, Right: Word): Boolean;
begin
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_N2(Inst: Pointer; Value: Word): Integer;
begin
  Result := Byte(Value);
  Inc(Result, (Result shr 4) * 63689);
  Inc(Result, (Integer(Value) shr 8) * -1660269137);
end;

class function InterfaceDefaults.Compare_I4(Inst: Pointer; Left, Right: Integer): Integer;
{$ifNdef CPUINTELASM}
begin
  Result := Shortint(Byte(Left >= Right) - Byte(Left <= Right));
end;
{$else}
asm
  xor eax, eax
  {$ifdef CPUX86}
    cmp edx, ecx
  {$else .CPUX64}
    cmp edx, r8d
  {$endif}
  mov edx, 1
  mov ecx, -1
  cmovg eax, edx
  cmovl eax, ecx
end;
{$endif}

class function InterfaceDefaults.Compare_U4(Inst: Pointer; Left, Right: Cardinal): Integer;
{$ifNdef CPUINTELASM}
begin
  Result := Shortint(Byte(Left >= Right) - Byte(Left <= Right));
end;
{$else}
asm
  xor eax, eax
  {$ifdef CPUX86}
    cmp edx, ecx
  {$else .CPUX64}
    cmp edx, r8d
  {$endif}
  mov edx, 1
  mov ecx, -1
  cmova eax, edx
  cmovb eax, ecx
end;
{$endif}

class function InterfaceDefaults.Equals_N4(Inst: Pointer; Left, Right: Integer): Boolean;
begin
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_N4(Inst: Pointer; Value: Integer): Integer;
begin
  Result := Value + ((Value shr 8) * 63689) + ((Value shr 16) * -1660269137) +
    ((Value shr 24) * -1092754919);
end;

class function InterfaceDefaults.Compare_I8(Inst: Pointer; Left, Right: Int64): Integer;
{$if Defined(CPUX64ASM)}
asm
  xor eax, eax
  cmp rdx, r8
  mov edx, 1
  mov ecx, -1
  cmovg eax, edx
  cmovl eax, ecx
end;
{$elseif Defined(LARGEINT)}
begin
  Result := Shortint(Byte(Left >= Right) - Byte(Left <= Right));
end;
{$else .SMALLINT}
var
  IL, IR: Integer;
  UL, UR: Cardinal;
begin
  IL := TPoint(Left).Y;
  IR := TPoint(Right).Y;
  if (IL <> IR) then
  begin
    Result := Shortint(Byte(IL >= IR) - Byte(IL <= IR));
  end else
  begin
    UL := TPoint(Left).X;
    UR := TPoint(Right).X;
    Result := Shortint(Byte(UL >= UR) - Byte(UL <= UR));
  end;
end;
{$ifend}

class function InterfaceDefaults.Compare_U8(Inst: Pointer; Left, Right: UInt64): Integer;
{$if Defined(CPUX64ASM)}
asm
  xor eax, eax
  cmp rdx, r8
  mov edx, 1
  mov ecx, -1
  cmova eax, edx
  cmovb eax, ecx
end;
{$elseif Defined(LARGEINT)}
begin
  Result := Shortint(Byte(Left >= Right) - Byte(Left <= Right));
end;
{$else .SMALLINT}
type
  UInt64Rec = array[0..1] of Cardinal;
var
  Index: NativeUInt;
  L, R: ^UInt64Rec;
  UL, UR: Cardinal;
begin
  Index := Byte(TPoint(Left).Y <> TPoint(Right).Y);
  L := @UInt64Rec(Left);
  R := @UInt64Rec(Right);
  UL := L[Index];
  UR := R[Index];
  Result := Shortint(Byte(UL >= UR) - Byte(UL <= UR));
end;
{$ifend}

class function InterfaceDefaults.Equals_N8(Inst: Pointer; Left, Right: Int64): Boolean;
begin
  {$ifdef LARGEINT}
    Result := (Left = Right);
  {$else .SMALLINT}
    Result := ((TPoint(Left).X - TPoint(Right).X) or (TPoint(Left).Y - TPoint(Right).Y) = 0);
  {$endif}
end;

class function InterfaceDefaults.GetHashCode_N8(Inst: Pointer; Value: Int64): Integer;
begin
  {$ifdef LARGEINT}
    Result := Integer(Value) + Integer(Value shr 32) * 63689;
  {$else .SMALLINT}
    Result := TPoint(Value).X + TPoint(Value).Y * 63689;
  {$endif}

  Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
    ((Result shr 24) * -1092754919));
end;

class function InterfaceDefaults.Equals_Class(Inst: Pointer; Left, Right: TObject): Boolean;
begin
  if (Left <> nil) then
  begin
    if (PPointer(Pointer(Left)^)[vmtEquals div SizeOf(Pointer)] = @TObject.Equals) then
    begin
      Result := (Left = Right);
      Exit;
    end else
    begin
      Result := Left.Equals(Right);
      Exit;
    end;
  end else
  begin
    Result := (Right = nil);
  end;
end;

class function InterfaceDefaults.GetHashCode_Class(Inst: Pointer; Value: TObject): Integer;
begin
  if (Assigned(Value)) then
  begin
    if (PPointer(Pointer(Value)^)[vmtGetHashCode div SizeOf(Pointer)] = @TObject.GetHashCode) then
    begin
      {$ifdef LARGEINT}
        Result := Integer(NativeInt(Value) xor (NativeInt(Value) shr 32));
      {$else .SMALLINT}
        Result := Integer(Value);
      {$endif}
      Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
          ((Result shr 24) * -1092754919));
      Exit;
    end else
    begin
      Result := Value.GetHashCode;
      Exit;
    end;
  end else
  begin
    Result := 0;
  end;
end;

class function InterfaceDefaults.GetHashCode_Ptr(Inst: Pointer; Value: NativeInt): Integer;
begin
  {$ifdef LARGEINT}
    Value := Value xor (Value shr 32);
  {$endif}
  Result := Integer(Value);
  Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
      ((Result shr 24) * -1092754919));
end;

class function InterfaceDefaults.Compare_F4(Inst: Pointer; Left, Right: Single): Integer;
{$if Defined(CPUX86ASM)}
asm
  fld Left
  fcomp Right
  fstsw ax
  xor ecx, ecx
  or ebp, -1
  lea edx, [ecx + 1]
  test ax, 256
  cmovnz edx, ebp
  test ax, 16384
  cmovnz edx, ecx
  xchg eax, edx
end;
{$elseif Defined(CPUX64ASM)}
asm
  or eax, -1
  xor edx, edx
  mov ecx, 1
  comiss xmm1,xmm2
  cmovz eax, edx
  cmova eax, ecx
end;
{$else}
begin
  Result := Shortint(Byte(Left >= Right) - Byte(Left <= Right));
end;
{$ifend}

class function InterfaceDefaults.Equals_F4(Inst: Pointer; Left, Right: Single): Boolean;
begin
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_F4(Inst: Pointer; Value: Single): Integer;
type
  TSingleRec = packed record
    Exponent: Integer;
    case Integer of
      0: (Mantissa: Single);
      1: (HighInt: Integer);
  end;
var
  SingleRec: TSingleRec;
begin
  Result := 0;
  if (Value <> 0) then
  begin
    Frexp(Value, SingleRec.Mantissa, SingleRec.Exponent);
    Result := SingleRec.Exponent + SingleRec.HighInt * 63689;
    Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
        ((Result shr 24) * -1092754919));
  end;
end;

class function InterfaceDefaults.Compare_F8(Inst: Pointer; Left, Right: Double): Integer;
{$if Defined(CPUX86ASM)}
asm
  fld Left
  fcomp Right
  fstsw ax
  xor ecx, ecx
  or ebp, -1
  lea edx, [ecx + 1]
  test ax, 256
  cmovnz edx, ebp
  test ax, 16384
  cmovnz edx, ecx
  xchg eax, edx
end;
{$elseif Defined(CPUX64ASM)}
asm
  or eax, -1
  xor edx, edx
  mov ecx, 1
  comisd xmm1,xmm2
  cmovz eax, edx
  cmova eax, ecx
end;
{$else}
begin
  Result := Shortint(Byte(Left >= Right) - Byte(Left <= Right));
end;
{$ifend}

class function InterfaceDefaults.Equals_F8(Inst: Pointer; Left, Right: Double): Boolean;
begin
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_F8(Inst: Pointer; Value: Double): Integer;
type
  TDoubleRec = packed record
    Exponent: Integer;
    case Integer of
      0: (Mantissa: Double);
      1: (LowInt: Integer; HighInt: Integer);
  end;
var
  DoubleRec: TDoubleRec;
begin
  Result := 0;
  if (Value <> 0) then
  begin
    Frexp(Value, DoubleRec.Mantissa, DoubleRec.Exponent);
    Result := DoubleRec.Exponent + DoubleRec.LowInt * 63689 + DoubleRec.HighInt * -1660269137;
    Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
        ((Result shr 24) * -1092754919));
  end;
end;

class function InterfaceDefaults.Compare_FE(Inst: Pointer; Left, Right: Extended): Integer;
{$if Defined(CPUX86ASM)}
asm
  fld Right
  fld Left
  fcompp
  fstsw ax
  xor ecx, ecx
  or ebp, -1
  lea edx, [ecx + 1]
  test ax, 256
  cmovnz edx, ebp
  test ax, 16384
  cmovnz edx, ecx
  xchg eax, edx
end;
{$elseif Defined(CPUX64ASM)}
asm
  or eax, -1
  xor edx, edx
  mov ecx, 1
  comisd xmm1,xmm2
  cmovz eax, edx
  cmova eax, ecx
end;
{$else}
begin
  Result := Shortint(Byte(Left >= Right) - Byte(Left <= Right));
end;
{$ifend}

class function InterfaceDefaults.Equals_FE(Inst: Pointer; Left, Right: Extended): Boolean;
begin
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_FE(Inst: Pointer; Value: Extended): Integer;
type
  TExtendedRec = packed record
    Exponent: Integer;
    case Integer of
      0: (Mantissa: Extended);
      1: (LowInt: Integer; {$if SizeOf(Extended) = 10}Middle: Word;{$ifend} HighInt: Integer);
  end;
var
  ExtendedRec: TExtendedRec;
begin
  Result := 0;
  if (Value <> 0) then
  begin
    Frexp(Value, ExtendedRec.Mantissa, ExtendedRec.Exponent);
    Result := ExtendedRec.Exponent + ExtendedRec.LowInt * 63689 + ExtendedRec.HighInt * -1660269137
      {$if SizeOf(Extended) = 10}+ Integer(ExtendedRec.Middle) * -1092754919{$ifend};
    Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
        ((Result shr 24) * -1092754919));
  end;
end;

class function InterfaceDefaults.Compare_Var_Difficult(Equal: Boolean; Left, Right: PVariant): Integer;
var
  S1, S2: string;
  Inst: IComparerInst;
begin
  try
    if (Left^ = Right^) then
    begin
      Result := 0;
    end else
    if (not Equal) then
    begin
      Result := 2 * Ord(Left^ > Right^) - 1;
    end else
    begin
      Result := 1;
    end;
  except // if comparison failed with exception, compare as string.
    try
      S1 := Left^;
      S2 := Right^;
      if (Equal) and (Length(S1) <> Length(S2)) then
        Exit(1);
      Result := InterfaceDefaults.Compare_UStr(nil, Pointer(S1), Pointer(S2));
    except  // if comparison fails again, compare bytes.
      Inst.Size := SizeOf(Variant);
      Result := InterfaceDefaults.Compare_Bin(Inst, Pointer(Left), Pointer(Right));
    end;
  end;
end;

class function InterfaceDefaults.Compare_Var(Inst: Pointer; Left, Right: PVarData): Integer;
label
  has_left, has_right, difficult;
var
  VLeft, VRight: Integer;
begin
  VLeft := Left.VType;
  if (VLeft <> varByRef or varVariant) then
  begin
  has_left:
    VRight := Right.VType;
    if (VRight <> varByRef or varVariant) then
    begin
    has_right:
      if (VLeft > varNull) and (VRight > varNull) then
      begin
        if (VLeft <> VRight) then goto difficult;

        case (VLeft) of
          varShortInt:
          begin
            Result := Integer(Left.VShortInt) - Integer(Right.VShortInt);
          end;
          varBoolean, varByte:
          begin
            Result := Integer(Left.VByte) - Integer(Right.VByte);
          end;
          varSmallint:
          begin
            Result := Integer(Left.VSmallInt) - Integer(Right.VSmallInt);
          end;
          varWord:
          begin
            Result := Integer(Left.VWord) - Integer(Right.VWord);
          end;
          varInteger:
          begin
            Result := Shortint(Byte(Left.VInteger >= Right.VInteger) - Byte(Left.VInteger <= Right.VInteger));
          end;
          varLongWord:
          begin
            Result := Shortint(Byte(Left.VLongWord >= Right.VLongWord) - Byte(Left.VLongWord <= Right.VLongWord));
          end;
          varInt64, varCurrency:
          begin
            {$ifdef LARGEINT}
              Result := Shortint(Byte(Left.VInt64 >= Right.VInt64) - Byte(Left.VInt64 <= Right.VInt64));
            {$else .SMALLINT}
              VLeft := Left.VLongs[2];
              VRight := Right.VLongs[2];
              if (VLeft <> VRight) then
              begin
                Result := Shortint(Byte(VLeft >= VRight) - Byte(VLeft <= VRight));
              end else
              begin
                VLeft := Left.VInteger;
                VRight := Right.VInteger;
                Result := Shortint(Byte(Cardinal(VLeft) >= Cardinal(VRight)) - Byte(Cardinal(VLeft) <= Cardinal(VRight)));
              end;
            {$endif}
          end;
          varUInt64:
          begin
            {$ifdef LARGEINT}
              Result := Shortint(Byte(Left.VUInt64 >= Right.VUInt64) - Byte(Left.VUInt64 <= Right.VUInt64));
            {$else .SMALLINT}
              VLeft := Left.VLongs[2];
              VRight := Right.VLongs[2];
              if (VLeft = VRight) then
              begin
                VLeft := Left.VInteger;
                VRight := Right.VInteger;
              end;
              Result := Shortint(Byte(Cardinal(VLeft) >= Cardinal(VRight)) - Byte(Cardinal(VLeft) <= Cardinal(VRight)));
            {$endif}
          end;
          varSingle:
          begin
            Result := Shortint(Byte(Left.VSingle >= Right.VSingle) - Byte(Left.VSingle <= Right.VSingle));
          end;
          varDouble, varDate:
          begin
            Result := Shortint(Byte(Left.VDouble >= Right.VDouble) - Byte(Left.VDouble <= Right.VDouble));
          end;
          varString:
          begin
            Result := InterfaceDefaults.Compare_LStr(nil, Left.VPointer, Right.VPointer);
          end;
          varUString:
          begin
            Result := InterfaceDefaults.Compare_UStr(nil, Left.VPointer, Right.VPointer);
          end;
          varOleStr:
          begin
            Result := InterfaceDefaults.Compare_WStr(nil, Left.VPointer, Right.VPointer);
          end;
        else
        difficult:
          Result := InterfaceDefaults.Compare_Var_Difficult(False, PVariant(Left), PVariant(Right));
        end;
      end else
      begin
        Result := 0;
      end;
    end else
    begin
      repeat
        Right := Right.VPointer;
        VRight := Right.VType;
      until (VRight <> varByRef or varVariant);
      goto has_right;
    end;
  end else
  begin
    repeat
      Left := Left.VPointer;
      VLeft := Left.VType;
    until (VLeft <> varByRef or varVariant);
    goto has_left;
  end;
end;

class function InterfaceDefaults.Equals_Var(Inst: Pointer; Left, Right: PVarData): Boolean;
var
  VLeft, VRight: Integer;
begin
  VLeft := Left.VType;
  if (VLeft = varByRef or varVariant) then
  repeat
    Left := Left.VPointer;
    VLeft := Left.VType;
  until (VLeft <> varByRef or varVariant);

  VRight := Right.VType;
  if (VRight = varByRef or varVariant) then
  repeat
    Right := Right.VPointer;
    VRight := Right.VType;
  until (VRight <> varByRef or varVariant);

  if (VLeft > varNull) and (VRight > varNull) then
  begin
    if (VLeft = VRight) then
    case (VLeft) of
      varShortInt, varBoolean, varByte:
      begin
        Result := (Left.VByte = Right.VByte);
        Exit;
      end;
      varSmallint, varWord:
      begin
        Result := (Left.VWord = Right.VWord);
        Exit;
      end;
      varInteger, varLongWord:
      begin
        Result := (Left.VInteger = Right.VInteger);
        Exit;
      end;
      varInt64, varCurrency, varUInt64:
      begin
        {$ifdef LARGEINT}
          Result := (Left.VInt64 = Right.VInt64);
        {$else .SMALLINT}
          Result := ((Left.VLongs[1] - Right.VLongs[1]) or (Left.VLongs[2] - Right.VLongs[2]) = 0);
        {$endif}
        Exit;
      end;
      varSingle:
      begin
        Result := (Left.VSingle >= Right.VSingle) = (Left.VSingle <= Right.VSingle);
        Exit;
      end;
      varDouble, varDate:
      begin
        Result := (Left.VDouble >= Right.VDouble) = (Left.VDouble <= Right.VDouble);
        Exit;
      end;
      varString:
      begin
        Result := InterfaceDefaults.Equals_LStr(nil, Left.VPointer, Right.VPointer);
        Exit;
      end;
      varUString:
      begin
        Result := InterfaceDefaults.Equals_UStr(nil, Left.VPointer, Right.VPointer);
        Exit;
      end;
      varOleStr:
      begin
        Result := InterfaceDefaults.Equals_WStr(nil, Left.VPointer, Right.VPointer);
        Exit;
      end;
    end;

    Result := (InterfaceDefaults.Compare_Var_Difficult(True, PVariant(Left), PVariant(Right)) = 0);
  end else
  begin
    Result := True;
  end;
end;

class function InterfaceDefaults.GetHashCode_Var_Difficult(Value: PVariant): Integer;
var
  S: string;
  Instance: IEqualityComparerInst;
begin
  try
    S := Value^;
    Result := GetHashCode_UStr(nil, Pointer(S));
  except
    Instance.Size := SizeOf(Variant);
    Result := GetHashCode_Bin(Instance, Pointer(Value));
  end;
end;

class function InterfaceDefaults.GetHashCode_Var(Inst: Pointer; Value: PVarData): Integer;
label
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10,
  null, write_uint64, init_write_cardinal, write_cardinal,
  write_ordinal_string, write_terminated_string, write_string;
const
  DIGITS: array[0..99] of array[1..2] of Char = (
    '00','01','02','03','04','05','06','07','08','09',
    '10','11','12','13','14','15','16','17','18','19',
    '20','21','22','23','24','25','26','27','28','29',
    '30','31','32','33','34','35','36','37','38','39',
    '40','41','42','43','44','45','46','47','48','49',
    '50','51','52','53','54','55','56','57','58','59',
    '60','61','62','63','64','65','66','67','68','69',
    '70','71','72','73','74','75','76','77','78','79',
    '80','81','82','83','84','85','86','87','88','89',
    '90','91','92','93','94','95','96','97','98','99');
var
  V32: Cardinal;
  V64, V64Buffer: UInt64;
  VFloat: Extended;
  VSign: Boolean;
  S, Top: PChar;
  Count: NativeUInt;
  M, N: Integer;
  Buffer: array[0..31] of Char;
begin
  VSign := False;

  case Integer(Value.VType) of
    varEmpty, varNull:
    begin
    null:
      Result := 0;
      Exit;
    end;
    varBoolean:
    begin
      if (Value.VBoolean) then
      begin
        Result := -34292652;
        Exit;
      end else
      begin
        Result := 1454490910;
        Exit;
      end;
    end;
    varShortInt:
    begin
      V32 := Cardinal(Integer(Value.VShortInt));
      if (Integer(V32) < 0) then
      begin
        VSign := True;
        V32 := Cardinal(-Integer(V32));
      end;
      goto init_write_cardinal;
    end;
    varByte:
    begin
      V32 := Value.VByte;
      goto init_write_cardinal;
    end;
    varSmallint:
    begin
      V32 := Cardinal(Integer(Value.VSmallInt));
      if (Integer(V32) < 0) then
      begin
        VSign := True;
        V32 := Cardinal(-Integer(V32));
      end;
      goto init_write_cardinal;
    end;
    varWord:
    begin
      V32 := Value.VWord;
      goto init_write_cardinal;
    end;
    varInteger:
    begin
      V32 := Cardinal(Integer(Value.VInteger));
      if (Integer(V32) < 0) then
      begin
        VSign := True;
        V32 := Cardinal(-Integer(V32));
      end;
      goto init_write_cardinal;
    end;
    varLongWord:
    begin
      V32 := Value.VLongWord;
      goto init_write_cardinal;
    end;
    varInt64:
    begin
      V64 := Value.VUInt64;
      if ({$ifdef LARGEINT}Int64(V64){$else}TPoint(V64).Y{$endif} < 0) then
      begin
        VSign := True;
        Int64(V64) := -Int64(V64);
      end;
      goto write_uint64;
    end;
    varUInt64:
    begin
      V64 := Value.VUInt64;
    write_uint64:
      S := @Buffer[High(Buffer)];
      V32 := V64;
      if ({$ifdef LARGEINT}V64 <> V32{$else}TPoint(V64).Y <> 0{$endif}) then
      repeat
        V64Buffer := V64;
        V64 := V64 div 100;
        V64Buffer := V64Buffer - V64 * 100;
        Dec(S, 2);
        PCardinal(S)^ := PCardinal(@DIGITS[NativeUInt(V64Buffer)])^;
        V32 := V64;
      until ({$ifdef LARGEINT}V64 = V32{$else}TPoint(V64).Y = 0{$endif});
      goto write_cardinal;
    end;
    varSingle:
    begin
      VFloat := Value.VSingle;
      Top := @Buffer[FloatToText(Buffer, VFloat, fvExtended, ffGeneral, 15, 0{$if CompilerVersion > 21}, FormatSettings{$ifend})];
      S := @Buffer[0];
      goto write_terminated_string;
    end;
    varDouble:
    begin
      VFloat := Value.VDouble;
      Top := @Buffer[FloatToText(Buffer, VFloat, fvExtended, ffGeneral, 15, 0{$if CompilerVersion > 21}, FormatSettings{$ifend})];
      S := @Buffer[0];
      goto write_terminated_string;
    end;
    varCurrency:
    begin
      Top := @Buffer[FloatToText(Buffer, Value.VCurrency, fvCurrency, ffGeneral, 0, 0{$if CompilerVersion > 21}, FormatSettings{$ifend})];
      S := @Buffer[0];
      goto write_terminated_string;
    end;
    varUString:
    begin
      S := Value.VPointer;
      if (S = nil) then goto null;
      Top := Pointer(@S[PInteger(PByte(S) - SizeOf(Integer))^]);
      goto write_string;
    end;
    varOleStr:
    begin
      S := Value.VPointer;
      if (S = nil) then goto null;
      Top := Pointer(@S[PInteger(PByte(S) - SizeOf(Integer))^ {$ifdef MSWINDOWS}shr 1{$endif}]);
      {$ifdef MSWINDOWS}if (S = Top) then goto null;{$endif}
      goto write_string;
    end;
  else
    Result := GetHashCode_Var_Difficult(Pointer(Value));
    Exit;
  end;

init_write_cardinal:
  S := @Buffer[High(Buffer)];
write_cardinal:
  if (V32 >= 10000) then
  begin
    if (V32 >= 100000000) then
    begin
      M := V32;
      V32 := V32 div 100;
      Dec(M, Integer(V32) * 100);
      Dec(S, 2);
      PCardinal(S)^ := PCardinal(@DIGITS[M])^;
    end;
    M := V32;
    V32 := V32 div 10000;
    Dec(M, Integer(V32) * 10000);
    N := (M * $147B) shr 19; // N := M div 100;
    M := M - (N * 100); // M := M mod 100;
    Dec(S, 2);
    PCardinal(S)^ := PCardinal(@DIGITS[M])^;
    Dec(S, 2);
    PCardinal(S)^ := PCardinal(@DIGITS[N])^;
  end;
  N := (Integer(V32) * $147B) shr 19; // N := M div 100;
  M := Integer(V32) - (N * 100); // M := M mod 100;
  Dec(S, 2);
  PCardinal(S)^ := PCardinal(@DIGITS[M])^;
  Dec(S, 2);
  PCardinal(S)^ := PCardinal(@DIGITS[N])^;
  Inc(S, 3);
  Dec(S, Byte(Byte(V32 > 9) + Byte(V32 > 99) + Byte(V32 > 999)));

write_ordinal_string:
  Top := @Buffer[High(Buffer)];
  if (VSign) then
  begin
    Dec(S);
    S^ := '-';
  end;

  // InterfaceDefaults.GetHashCode_UStr(S..Top)
write_terminated_string:
  Top^ := #0;
write_string:
  Inc(Top);
  Count := NativeInt(Top) - NativeInt(S);
  Count := Count and -4;
  Result := Integer(Count) + PInteger(@PByte(S)[Count - SizeOf(Integer)])^ * 63689;
  case (Count - 1) shr 2 of
   10: goto hash10;
    9: goto hash9;
    8: goto hash8;
    7: goto hash7;
    6: goto hash6;
    5: goto hash5;
    4: goto hash4;
    3: goto hash3;
    2: goto hash2;
    1: goto hash1;
    0: goto hash0;
  else
    Dec(Count);
    M := -1660269137;
    repeat
      Result := Result * M + PInteger(S)^;
      Dec(Count, SizeOf(Integer));
      Inc(PByte(S), SizeOf(Integer));
      M := M * 378551;
    until (Count <= 43);

    hash10:
      Result := Result * 631547855 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash9:
      Result := Result * -1987506439 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash8:
      Result := Result * -1653913089 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash7:
      Result := Result * -186114231 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash6:
      Result := Result * 915264303 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash5:
      Result := Result * -794603367 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash4:
      Result := Result * 135394143 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash3:
      Result := Result * 2012804575 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash2:
      Result := Result * -1092754919 + PInteger(S)^;
      Inc(PByte(S), SizeOf(Integer));
    hash1:
      Result := Result * -1660269137 + PInteger(S)^;
    hash0:
  end;

  Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
    ((Result shr 24) * -1092754919));
end;

class function InterfaceDefaults.Compare_OStr(Inst: Pointer; Left, Right: PByte): Integer;
label
  make_result, make_result_swaped;
var
  X, Y, Count: NativeUInt;
  Modify: Integer;
begin
  X := Left^;
  Y := Right^;
  if (Left <> Right) and (X <> 0) and (Y <> 0) then
  begin
    if (Left[1] = Right[1]) then
    begin
      if (X < Y) then
      begin
        Modify := -1;
        Count := X;
      end else
      begin
        Modify := NativeInt(Y - X) shr {$ifdef SMALLINT}31{$else}63{$endif};
        Count := Y;
      end;
      Inc(Left);
      Inc(Right);

      repeat
        if (Count < SizeOf(NativeUInt)) then Break;
        X := PNativeUInt(Left)^;
        Dec(Count, SizeOf(NativeUInt));
        Y := PNativeUInt(Right)^;
        Inc(Left, SizeOf(NativeUInt));
        Inc(Right, SizeOf(NativeUInt));

        if (X <> Y) then
        begin
          {$ifdef LARGEINT}
            if (Integer(X) = Integer(Y)) then
            begin
              X := X shr 32;
              Y := Y shr 32;
            end else
            begin
              X := Cardinal(X);
              Y := Cardinal(Y);
            end;
          {$endif}

          goto make_result;
        end;
      until (False);

      {$ifdef LARGEINT}
      if (Count and 4 <> 0) then
      begin
        X := PCardinal(Left)^;
        Y := PCardinal(Right)^;
        Inc(Left, SizeOf(Cardinal));
        Inc(Right, SizeOf(Cardinal));

        if (X <> Y) then goto make_result;
      end;
      {$endif}

      case Count of
        1: begin
             X := PByte(Left)^;
             Y := PByte(Right)^;
             if (X <> Y) then goto make_result_swaped;
           end;
        2: begin
             X := Swap(PWord(Left)^);
             Y := Swap(PWord(Right)^);
             if (X <> Y) then goto make_result_swaped;
           end;
        3: begin
             X := Swap(PWord(Left)^);
             Y := Swap(PWord(Right)^);
             Inc(Left, SizeOf(Word));
             Inc(Right, SizeOf(Word));
             X := (X shl 8) or PByte(Left)^;
             Y := (Y shl 8) or PByte(Right)^;
             if (X <> Y) then goto make_result_swaped;
           end;
      end;

      Result := Modify;
      Exit;
    make_result:
      X := (Swap(X) shl 16) + Swap(X shr 16);
      Y := (Swap(Y) shl 16) + Swap(Y shr 16);

    make_result_swaped:
      Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
      Exit;
    end else
    begin
      Inc(Left);
      Inc(Right);
      X := Left^;
      Y := Right^;
    end;
  end;

  Result := Integer(X) - Integer(Y);
end;

class function InterfaceDefaults.Equals_OStr(Inst: Pointer; Left, Right: PByte): Boolean;
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  done;
var
  Count: NativeUInt;
begin
  if (Left = Right) then goto done;
  Count := Left^;
  if (Count <> Right^) then goto done;

  // natives (40 bytes static) compare
  case Count shr {$ifdef LARGEINT}3{$else}2{$endif} of
  {$ifdef SMALLINT}
   10: goto cmp10;
    9: goto cmp9;
    8: goto cmp8;
    7: goto cmp7;
    6: goto cmp6;
  {$endif}
    5: goto cmp5;
    4: goto cmp4;
    3: goto cmp3;
    2: goto cmp2;
    1: goto cmp1;
    0: goto cmp0;
  else
    repeat
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Dec(Count, SizeOf(NativeUInt));
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    until (Count < (40 + SizeOf(NativeUInt) - 1));

    {$ifdef SMALLINT}
    cmp10:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp9:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp8:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp7:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp6:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    {$endif}
    cmp5:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp4:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp3:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp2:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp1:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp0:
  end;

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    if (PCardinal(Left)^ <> PCardinal(Right)^) then goto done;
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Count and 3 <> 0) then
  begin
    if (Count and 2 <> 0) then
    begin
      if (PWord(Left)^ <> PWord(Right)^) then goto done;
    end;
    Dec(Left);
    Dec(Right);
    if (Left[Count] <> Right[Count]) then goto done;
  end;

// Result := True
  Left := nil;
  Right := nil;
done:
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_OStr(Inst: Pointer; Value: PByte): Integer;
label
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10;
var
  Count: NativeUInt;
  M: Integer;
begin
  Count := Value^;
  Inc(Count);

  if (Count >= SizeOf(Integer)) then
  begin
    Result := Integer(Count) + PInteger(@Value[Count - SizeOf(Integer)])^ * 63689;

    case (Count - 1) shr 2 of
     10: goto hash10;
      9: goto hash9;
      8: goto hash8;
      7: goto hash7;
      6: goto hash6;
      5: goto hash5;
      4: goto hash4;
      3: goto hash3;
      2: goto hash2;
      1: goto hash1;
      0: goto hash0;
    else
      Dec(Count);
      M := -1660269137;
      repeat
        Result := Result * M + PInteger(Value)^;
        Dec(Count, SizeOf(Integer));
        Inc(Value, SizeOf(Integer));
        M := M * 378551;
      until (Count <= 43);

      hash10:
        Result := Result * 631547855 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash9:
        Result := Result * -1987506439 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash8:
        Result := Result * -1653913089 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash7:
        Result := Result * -186114231 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash6:
        Result := Result * 915264303 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash5:
        Result := Result * -794603367 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash4:
        Result := Result * 135394143 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash3:
        Result := Result * 2012804575 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash2:
        Result := Result * -1092754919 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash1:
        Result := Result * -1660269137 + PInteger(Value)^;
      hash0:
    end;

    Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
      ((Result shr 24) * -1092754919));
    Exit;
  end else
  begin
    Result := Integer(Value[0]);
    Result := Result + (Result shr 4) * 63689;
    if (Count > 1) then
    begin
      Result := Result + Integer(Value[1]) * -1660269137;
      if (Count > 2) then
      begin
        Result := Result + Integer(Value[2]) * -1092754919;
      end;
    end;
  end;
end;

class function InterfaceDefaults.Compare_LStr(Inst: Pointer; Left, Right: PByte): Integer;
label
  make_result, make_result_swaped;
var
  X, Y,Count: NativeUInt;
  Modify: Integer;
begin
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = nil) or (Right = nil) or (Left = Right) then goto make_result_swaped;

  X := Left^;
  Y := Right^;
  if (X <> Y) then goto make_result_swaped;

  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  X := PInteger(Left)^;
  Y := PInteger(Right)^;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));
  if (X < Y) then
  begin
    Modify := -1;
    Count := X + 1;
  end else
  begin
    Modify := NativeInt(Y - X) shr {$ifdef SMALLINT}31{$else}63{$endif};
    Count := Y + 1;
  end;

  repeat
    if (Count < SizeOf(NativeUInt)) then Break;
    X := PNativeUInt(Left)^;
    Dec(Count, SizeOf(NativeUInt));
    Y := PNativeUInt(Right)^;
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));

    if (X <> Y) then
    begin
      {$ifdef LARGEINT}
        if (Integer(X) = Integer(Y)) then
        begin
          X := X shr 32;
          Y := Y shr 32;
        end else
        begin
          X := Cardinal(X);
          Y := Cardinal(Y);
        end;
      {$endif}

      goto make_result;
    end;
  until (False);

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    X := PCardinal(Left)^;
    Y := PCardinal(Right)^;
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));

    if (X <> Y) then goto make_result;
  end;
  {$endif}

  if (Count and 2 <> 0) then
  begin
    X := Swap(PWord(Left)^);
    Y := Swap(PWord(Right)^);
    if (X <> Y) then goto make_result_swaped;
  end;

  Result := Modify;
  Exit;
make_result:
  X := (Swap(X) shl 16) + Swap(X shr 16);
  Y := (Swap(Y) shl 16) + Swap(Y shr 16);

make_result_swaped:
  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
end;

class function InterfaceDefaults.Equals_LStr(Inst: Pointer; Left, Right: PByte): Boolean;
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  done;
var
  Count: NativeUInt;
begin
  if (Left = nil) or (Right = nil) or (Left = Right) then goto done;
  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  Count := PInteger(Left)^;
  if (Integer(Count) <> PInteger(Right)^) then goto done;
  Inc(Count);
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));

  // natives (40 bytes static) compare
  case Count shr {$ifdef LARGEINT}3{$else}2{$endif} of
  {$ifdef SMALLINT}
   10: goto cmp10;
    9: goto cmp9;
    8: goto cmp8;
    7: goto cmp7;
    6: goto cmp6;
  {$endif}
    5: goto cmp5;
    4: goto cmp4;
    3: goto cmp3;
    2: goto cmp2;
    1: goto cmp1;
    0: goto cmp0;
  else
    repeat
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Dec(Count, SizeOf(NativeUInt));
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    until (Count < (40 + SizeOf(NativeUInt) - 1));

    {$ifdef SMALLINT}
    cmp10:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp9:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp8:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp7:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp6:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    {$endif}
    cmp5:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp4:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp3:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp2:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp1:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp0:
  end;

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    if (PCardinal(Left)^ <> PCardinal(Right)^) then goto done;
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Count and 2 <> 0) then
  begin
    if (PWord(Left)^ <> PWord(Right)^) then goto done;
  end;

// Result := True
  Left := nil;
  Right := nil;
done:
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_LStr(Inst: Pointer; Value: PByte): Integer;
label
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10;
var
  Count: NativeUInt;
  M: Integer;
begin
  if (Value <> nil) then
  begin
    Count := PInteger(@Value[-SizeOf(Integer)])^;

    if (Count >= SizeOf(Integer)) then
    begin
      Result := Integer(Count) + PInteger(@Value[Count - SizeOf(Integer)])^ * 63689;

      case (Count - 1) shr 2 of
       10: goto hash10;
        9: goto hash9;
        8: goto hash8;
        7: goto hash7;
        6: goto hash6;
        5: goto hash5;
        4: goto hash4;
        3: goto hash3;
        2: goto hash2;
        1: goto hash1;
        0: goto hash0;
      else
        Dec(Count);
        M := -1660269137;
        repeat
          Result := Result * M + PInteger(Value)^;
          Dec(Count, SizeOf(Integer));
          Inc(Value, SizeOf(Integer));
          M := M * 378551;
        until (Count <= 43);

        hash10:
          Result := Result * 631547855 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash9:
          Result := Result * -1987506439 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash8:
          Result := Result * -1653913089 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash7:
          Result := Result * -186114231 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash6:
          Result := Result * 915264303 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash5:
          Result := Result * -794603367 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash4:
          Result := Result * 135394143 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash3:
          Result := Result * 2012804575 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash2:
          Result := Result * -1092754919 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash1:
          Result := Result * -1660269137 + PInteger(Value)^;
        hash0:
      end;

      Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
        ((Result shr 24) * -1092754919));
      Exit;
    end else
    begin
      Result := Integer(Value[0]);
      Result := Result + (Result shr 4) * 63689;
      if (Count > 1) then
      begin
        Result := Result + Integer(Value[1]) * -1660269137;
        if (Count > 2) then
        begin
          Result := Result + Integer(Value[2]) * -1092754919;
        end;
      end;
    end;
  end else
  begin
    Result := 0;
  end;
end;

class function InterfaceDefaults.Compare_UStr(Inst: Pointer; Left, Right: PByte): Integer;
label
  make_result, make_result_swaped;
var
  X, Y, Count: NativeUInt;
  Modify: Integer;
begin
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = nil) or (Right = nil) or (Left = Right) then goto make_result_swaped;

  X := PWord(Left)^;
  Y := PWord(Right)^;
  if (X <> Y) then goto make_result_swaped;

  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  X := PInteger(Left)^;
  Y := PInteger(Right)^;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));
  if (X < Y) then
  begin
    Modify := -1;
    Count := X * 2 + 2;
  end else
  begin
    Modify := NativeInt(Y - X) shr {$ifdef SMALLINT}31{$else}63{$endif};
    Count := Y * 2 + 2;
  end;

  repeat
    if (Count < SizeOf(NativeUInt)) then Break;
    X := PNativeUInt(Left)^;
    Dec(Count, SizeOf(NativeUInt));
    Y := PNativeUInt(Right)^;
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));

    if (X <> Y) then
    begin
      {$ifdef LARGEINT}
        if (Integer(X) = Integer(Y)) then
        begin
          X := X shr 32;
          Y := Y shr 32;
        end else
        begin
          X := Cardinal(X);
          Y := Cardinal(Y);
        end;
      {$endif}

      goto make_result;
    end;
  until (False);

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    X := PCardinal(Left)^;
    Y := PCardinal(Right)^;
    if (X <> Y) then goto make_result;
  end;
  {$endif}

  Result := Modify;
  Exit;
make_result:
  X := {$ifdef LARGEINT}Cardinal{$endif}(X shl 16) + (X shr 16);
  Y := {$ifdef LARGEINT}Cardinal{$endif}(Y shl 16) + (Y shr 16);
make_result_swaped:
  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
end;

class function InterfaceDefaults.Equals_UStr(Inst: Pointer; Left, Right: PByte): Boolean;
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  done;
var
  Count: NativeUInt;
begin
  if (Left = nil) or (Right = nil) or (Left = Right) then goto done;
  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  Count := PInteger(Left)^;
  if (Integer(Count) <> PInteger(Right)^) then goto done;
  Count := Count * 2 + 2;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));

  // natives (40 bytes static) compare
  case Count shr {$ifdef LARGEINT}3{$else}2{$endif} of
  {$ifdef SMALLINT}
   10: goto cmp10;
    9: goto cmp9;
    8: goto cmp8;
    7: goto cmp7;
    6: goto cmp6;
  {$endif}
    5: goto cmp5;
    4: goto cmp4;
    3: goto cmp3;
    2: goto cmp2;
    1: goto cmp1;
    0: goto cmp0;
  else
    repeat
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Dec(Count, SizeOf(NativeUInt));
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    until (Count < (40 + SizeOf(NativeUInt) - 1));

    {$ifdef SMALLINT}
    cmp10:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp9:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp8:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp7:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp6:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    {$endif}
    cmp5:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp4:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp3:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp2:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp1:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      {$ifdef LARGEINT}
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
      {$endif}
    cmp0:
  end;

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    if (PCardinal(Left)^ <> PCardinal(Right)^) then goto done;
  end;
  {$endif}

// Result := True
  Left := nil;
  Right := nil;
done:
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_UStr(Inst: Pointer; Value: PByte): Integer;
label
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10;
var
  Count: NativeUInt;
  M: Integer;
begin
  if (Value <> nil) then
  begin
    Count := PInteger(@Value[-SizeOf(Integer)])^;
    Count := Count * 2 + 2;
    Count := Count and -4;

    Result := Integer(Count) + PInteger(@Value[Count - SizeOf(Integer)])^ * 63689;
    case (Count - 1) shr 2 of
     10: goto hash10;
      9: goto hash9;
      8: goto hash8;
      7: goto hash7;
      6: goto hash6;
      5: goto hash5;
      4: goto hash4;
      3: goto hash3;
      2: goto hash2;
      1: goto hash1;
      0: goto hash0;
    else
      Dec(Count);
      M := -1660269137;
      repeat
        Result := Result * M + PInteger(Value)^;
        Dec(Count, SizeOf(Integer));
        Inc(Value, SizeOf(Integer));
        M := M * 378551;
      until (Count <= 43);

      hash10:
        Result := Result * 631547855 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash9:
        Result := Result * -1987506439 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash8:
        Result := Result * -1653913089 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash7:
        Result := Result * -186114231 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash6:
        Result := Result * 915264303 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash5:
        Result := Result * -794603367 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash4:
        Result := Result * 135394143 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash3:
        Result := Result * 2012804575 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash2:
        Result := Result * -1092754919 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash1:
        Result := Result * -1660269137 + PInteger(Value)^;
      hash0:
    end;

    Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
      ((Result shr 24) * -1092754919));
    Exit;
  end else
  begin
    Result := 0;
  end;
end;

class function InterfaceDefaults.Compare_WStr(Inst: Pointer; Left, Right: PByte): Integer;
label
  {$ifdef MSWINDOWS}left_nil, right_nil,{$endif}
  make_result, make_result_swaped;
var
  X, Y, Count: NativeUInt;
  Modify: Integer;
begin
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = Right) then goto make_result_swaped;
  if (Left = nil) then goto {$ifdef MSWINDOWS}left_nil{$else}make_result_swaped{$endif};
  if (Right = nil) then goto {$ifdef MSWINDOWS}right_nil{$else}make_result_swaped{$endif};

  X := PWord(Left)^;
  Y := PWord(Right)^;
  if (X <> Y) then goto make_result_swaped;

  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  X := PInteger(Left)^;
  Y := PInteger(Right)^;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));
  if (X < Y) then
  begin
    Modify := -1;
    Count := X {$ifNdef MSWINDOWS}* 2{$endif} + 2;
  end else
  begin
    Modify := NativeInt(Y - X) shr {$ifdef SMALLINT}31{$else}63{$endif};
    Count := Y {$ifNdef MSWINDOWS}* 2{$endif} + 2;
  end;

  repeat
    if (Count < SizeOf(NativeUInt)) then Break;
    X := PNativeUInt(Left)^;
    Dec(Count, SizeOf(NativeUInt));
    Y := PNativeUInt(Right)^;
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));

    if (X <> Y) then
    begin
      {$ifdef LARGEINT}
        if (Integer(X) = Integer(Y)) then
        begin
          X := X shr 32;
          Y := Y shr 32;
        end else
        begin
          X := Cardinal(X);
          Y := Cardinal(Y);
        end;
      {$endif}

      goto make_result;
    end;
  until (False);

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    X := PCardinal(Left)^;
    Y := PCardinal(Right)^;
    if (X <> Y) then goto make_result;
  end;
  {$endif}

  Result := Modify;
  Exit;
make_result:
  X := {$ifdef LARGEINT}Cardinal{$endif}(X shl 16) + (X shr 16);
  Y := {$ifdef LARGEINT}Cardinal{$endif}(Y shl 16) + (Y shr 16);
make_result_swaped:
  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
{$ifdef MSWINDOWS}
  Exit;
left_nil:
  Dec(Right, SizeOf(Integer));
  Result := -Ord(PInteger(Right)^ <> 0);
  Exit;
right_nil:
  Dec(Left, SizeOf(Integer));
  Result := Ord(PInteger(Left)^ <> 0);
{$endif}
end;

class function InterfaceDefaults.Equals_WStr(Inst: Pointer; Left, Right: PByte): Boolean;
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  {$ifdef MSWINDOWS}left_nil, right_nil,{$endif}
  done;
var
  Count: NativeUInt;
begin
  if (Left = Right) then goto done;
  if (Left = nil) then goto {$ifdef MSWINDOWS}left_nil{$else}done{$endif};
  if (Right = nil) then goto {$ifdef MSWINDOWS}right_nil{$else}done{$endif};
  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  Count := PInteger(Left)^;
  if (Integer(Count) <> PInteger(Right)^) then goto done;
  Count := Count {$ifNdef MSWINDOWS}* 2{$endif} + 2;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));

  // natives (40 bytes static) compare
  case Count shr {$ifdef LARGEINT}3{$else}2{$endif} of
  {$ifdef SMALLINT}
   10: goto cmp10;
    9: goto cmp9;
    8: goto cmp8;
    7: goto cmp7;
    6: goto cmp6;
  {$endif}
    5: goto cmp5;
    4: goto cmp4;
    3: goto cmp3;
    2: goto cmp2;
    1: goto cmp1;
    0: goto cmp0;
  else
    repeat
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Dec(Count, SizeOf(NativeUInt));
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    until (Count < (40 + SizeOf(NativeUInt) - 1));

    {$ifdef SMALLINT}
    cmp10:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp9:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp8:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp7:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp6:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    {$endif}
    cmp5:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp4:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp3:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp2:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp1:
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      {$ifdef LARGEINT}
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
      {$endif}
    cmp0:
  end;

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    if (PCardinal(Left)^ <> PCardinal(Right)^) then goto done;
  end;
  {$endif}

// Result := True
  Left := nil;
  Right := nil;
done:
  Result := (Left = Right);
{$ifdef MSWINDOWS}
  Exit;
left_nil:
  Dec(Right, SizeOf(Integer));
  Result := (PInteger(Right)^ = 0);
  Exit;
right_nil:
  Dec(Left, SizeOf(Integer));
  Result := (PInteger(Left)^ = 0);
{$endif}
end;

class function InterfaceDefaults.GetHashCode_WStr(Inst: Pointer; Value: PByte): Integer;
label
  {$ifdef MSWINDOWS}null,{$endif}
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10;
var
  Count: NativeUInt;
  M: Integer;
begin
  if (Value <> nil) then
  begin
    Count := PInteger(@Value[-SizeOf(Integer)])^;
    {$ifdef MSWINDOWS}if (Count = 0) then goto null;{$endif}
    Count := Count {$ifNdef MSWINDOWS}* 2{$endif} + 2;
    Count := Count and -4;

    Result := Integer(Count) + PInteger(@Value[Count - SizeOf(Integer)])^ * 63689;
    case (Count - 1) shr 2 of
     10: goto hash10;
      9: goto hash9;
      8: goto hash8;
      7: goto hash7;
      6: goto hash6;
      5: goto hash5;
      4: goto hash4;
      3: goto hash3;
      2: goto hash2;
      1: goto hash1;
      0: goto hash0;
    else
      Dec(Count);
      M := -1660269137;
      repeat
        Result := Result * M + PInteger(Value)^;
        Dec(Count, SizeOf(Integer));
        Inc(Value, SizeOf(Integer));
        M := M * 378551;
      until (Count <= 43);

      hash10:
        Result := Result * 631547855 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash9:
        Result := Result * -1987506439 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash8:
        Result := Result * -1653913089 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash7:
        Result := Result * -186114231 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash6:
        Result := Result * 915264303 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash5:
        Result := Result * -794603367 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash4:
        Result := Result * 135394143 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash3:
        Result := Result * 2012804575 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash2:
        Result := Result * -1092754919 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash1:
        Result := Result * -1660269137 + PInteger(Value)^;
      hash0:
    end;

    Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
      ((Result shr 24) * -1092754919));
    Exit;
  end else
  begin
  {$ifdef MSWINDOWS}null:{$endif}
    Result := 0;
  end;
end;

class function InterfaceDefaults.Compare_Method(Inst: Pointer; const Left, Right: TMethodPtr): Integer;
var
  X, Y: NativeUInt;
begin
  X := NativeUInt(TMethod(Left).Data);
  Y := NativeUInt(TMethod(Right).Data);
  if (X = Y) then
  begin
    X := NativeUInt(TMethod(Left).Code);
    Y := NativeUInt(TMethod(Right).Code);
  end;

  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
end;

class function InterfaceDefaults.Equals_Method(Inst: Pointer; const Left, Right: TMethodPtr): Boolean;
begin
  Result := ((NativeInt(TMethod(Left).Data) - NativeInt(TMethod(Right).Data)) or
    (NativeInt(TMethod(Left).Code) - NativeInt(TMethod(Right).Code)) = 0);
end;

class function InterfaceDefaults.GetHashCode_Method(Inst: Pointer; const Value: TMethodPtr): Integer;
{$ifdef LARGEINT}
var
  Data: PByte;
{$endif}
begin
  {$ifdef SMALLINT}
    Result := PPoint(@Value).X + PPoint(@Value).Y * 63689;
  {$else .LARGEINT}
    Data := Pointer(@Value);
    Result := Integer(SizeOf(TMethodPtr)) + PInteger(Data)[3] * 63689;
    Result := Result * 2012804575 + PInteger(Data)[0];
    Result := Result * -1092754919 + PInteger(Data)[1];
    Result := Result * -1660269137 + PInteger(Data)[2];
  {$endif}

  Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
    ((Result shr 24) * -1092754919));
end;

class function InterfaceDefaults.Compare_Dyn(const Inst: IComparerInst; Left, Right: PByte): Integer;
label
  make_result, make_result_swaped;
var
  X, Y,Count: NativeUInt;
  Modify: Integer;
begin
  Count := Inst.Size;
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = nil) or (Right = nil) or (Left = Right) then goto make_result_swaped;

  X := Left^;
  Y := Right^;
  if (X <> Y) then goto make_result_swaped;

  Dec(Left, SizeOf(NativeUInt));
  Dec(Right, SizeOf(NativeUInt));
  X := PNativeUInt(Left)^;
  Y := PNativeUInt(Right)^;
  Inc(Left, SizeOf(NativeUInt));
  Inc(Right, SizeOf(NativeUInt));
  if (X < Y) then
  begin
    Modify := -1;
    NativeInt(Count) := NativeInt(Count) * NativeInt(X);
  end else
  begin
    Modify := NativeInt(Y - X) shr {$ifdef SMALLINT}31{$else}63{$endif};
    NativeInt(Count) := NativeInt(Count) * NativeInt(Y);
  end;

  repeat
    if (Count < SizeOf(NativeUInt)) then Break;
    X := PNativeUInt(Left)^;
    Dec(Count, SizeOf(NativeUInt));
    Y := PNativeUInt(Right)^;
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));

    if (X <> Y) then
    begin
      {$ifdef LARGEINT}
        if (Integer(X) = Integer(Y)) then
        begin
          X := X shr 32;
          Y := Y shr 32;
        end else
        begin
          X := Cardinal(X);
          Y := Cardinal(Y);
        end;
      {$endif}

      goto make_result;
    end;
  until (False);

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    X := PCardinal(Left)^;
    Y := PCardinal(Right)^;
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));

    if (X <> Y) then goto make_result;
  end;
  {$endif}

  case Count of
    1: begin
         X := PByte(Left)^;
         Y := PByte(Right)^;
         if (X <> Y) then goto make_result_swaped;
       end;
    2: begin
         X := Swap(PWord(Left)^);
         Y := Swap(PWord(Right)^);
         if (X <> Y) then goto make_result_swaped;
       end;
    3: begin
         X := Swap(PWord(Left)^);
         Y := Swap(PWord(Right)^);
         Inc(Left, SizeOf(Word));
         Inc(Right, SizeOf(Word));
         X := (X shl 8) or PByte(Left)^;
         Y := (Y shl 8) or PByte(Right)^;
         if (X <> Y) then goto make_result_swaped;
       end;
  end;

  Result := Modify;
  Exit;
make_result:
  X := (Swap(X) shl 16) + Swap(X shr 16);
  Y := (Swap(Y) shl 16) + Swap(Y shr 16);

make_result_swaped:
  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
end;


class function InterfaceDefaults.Equals_Dyn(const Inst: IEqualityComparerInst; Left, Right: PByte): Boolean;
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  done;
var
  Count: NativeUInt;
begin
  if (Left = nil) or (Right = nil) or (Left = Right) then goto done;
  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  Count := PInteger(Left)^;
  if (Integer(Count) <> PInteger(Right)^) then goto done;
  NativeInt(Count) := NativeInt(Count) * Inst.Size;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));

  // natives (40 bytes static) compare
  case Count shr {$ifdef LARGEINT}3{$else}2{$endif} of
  {$ifdef SMALLINT}
   10: goto cmp10;
    9: goto cmp9;
    8: goto cmp8;
    7: goto cmp7;
    6: goto cmp6;
  {$endif}
    5: goto cmp5;
    4: goto cmp4;
    3: goto cmp3;
    2: goto cmp2;
    1: goto cmp1;
    0: goto cmp0;
  else
    repeat
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Dec(Count, SizeOf(NativeUInt));
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    until (Count < (40 + SizeOf(NativeUInt) - 1));

    {$ifdef SMALLINT}
    cmp10:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp9:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp8:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp7:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp6:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    {$endif}
    cmp5:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp4:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp3:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp2:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp1:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp0:
  end;

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    if (PCardinal(Left)^ <> PCardinal(Right)^) then goto done;
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Count and 3 <> 0) then
  begin
    if (Count and 2 <> 0) then
    begin
      if (PWord(Left)^ <> PWord(Right)^) then goto done;
    end;
    Dec(Left);
    Dec(Right);
    if (Left[Count] <> Right[Count]) then goto done;
  end;

// Result := True
  Left := nil;
  Right := nil;
done:
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_Dyn(const Inst: IEqualityComparerInst; Value: PByte): Integer;
label
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10;
var
  Count: NativeUInt;
  M: Integer;
begin
  if (Value <> nil) then
  begin
    Count := NativeUInt(PNativeInt(@Value[-SizeOf(NativeInt)])^ * Inst.Size);

    if (Count >= SizeOf(Integer)) then
    begin
      Result := Integer(Count) + PInteger(@Value[Count - SizeOf(Integer)])^ * 63689;

      case (Count - 1) shr 2 of
       10: goto hash10;
        9: goto hash9;
        8: goto hash8;
        7: goto hash7;
        6: goto hash6;
        5: goto hash5;
        4: goto hash4;
        3: goto hash3;
        2: goto hash2;
        1: goto hash1;
        0: goto hash0;
      else
        Dec(Count);
        M := -1660269137;
        repeat
          Result := Result * M + PInteger(Value)^;
          Dec(Count, SizeOf(Integer));
          Inc(Value, SizeOf(Integer));
          M := M * 378551;
        until (Count <= 43);

        hash10:
          Result := Result * 631547855 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash9:
          Result := Result * -1987506439 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash8:
          Result := Result * -1653913089 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash7:
          Result := Result * -186114231 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash6:
          Result := Result * 915264303 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash5:
          Result := Result * -794603367 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash4:
          Result := Result * 135394143 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash3:
          Result := Result * 2012804575 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash2:
          Result := Result * -1092754919 + PInteger(Value)^;
          Inc(Value, SizeOf(Integer));
        hash1:
          Result := Result * -1660269137 + PInteger(Value)^;
        hash0:
      end;

      Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
        ((Result shr 24) * -1092754919));
      Exit;
    end else
    begin
      Result := Integer(Value[0]);
      Result := Result + (Result shr 4) * 63689;
      if (Count > 1) then
      begin
        Result := Result + Integer(Value[1]) * -1660269137;
        if (Count > 2) then
        begin
          Result := Result + Integer(Value[2]) * -1092754919;
        end;
      end;
    end;
  end else
  begin
    Result := 0;
  end;
end;

class function InterfaceDefaults.Compare_Bin2(Inst: Pointer; Left, Right: Word): Integer;
var
  L, R: NativeUInt;
begin
  L := Left;
  R := Right;
  L := Swap(L);
  R := Swap(R);
  Result := Integer(L) - Integer(R);
end;

class function InterfaceDefaults.Compare_Bin3(Inst: Pointer; const Left, Right: TTriple): Integer;
var
  L, R: NativeUInt;
begin
  L := Left.Low;
  R := Right.Low;
  L := (Swap(L) shl 8) + Left.High;
  R := (Swap(R) shl 8) + Right.High;
  Result := Integer(L) - Integer(R);
end;

class function InterfaceDefaults.Equals_Bin3(Inst: Pointer; const Left, Right: TTriple): Boolean;
begin
  Result := ((Integer(Left.High) shl 16) + Left.Low) = ((Integer(Right.High) shl 16) + Right.Low);
end;

class function InterfaceDefaults.GetHashCode_Bin3(Inst: Pointer; const Value: TTriple): Integer;
begin
  Result := Integer(Value.Bytes[0]);
  Result := Result + (Result shr 4) * 63689 + Integer(Value.Bytes[1]) * -1660269137 +
    Integer(Value.Bytes[2]) * -1092754919;
end;

class function InterfaceDefaults.Compare_Bin4(Inst: Pointer; Left, Right: Cardinal): Integer;
var
  X, Y: NativeUInt;
begin
  {$ifdef LARGEINT}
    X := Left;
    Y := Right;
    X := (Swap(X) shl 16) + Swap(X shr 16);
    Y := (Swap(Y) shl 16) + Swap(Y shr 16);
  {$else .SMALLINT}
    X := (Swap(Left) shl 16) + Swap(Left shr 16);
    Y := (Swap(Right) shl 16) + Swap(Right shr 16);
  {$endif}

  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
end;

class function InterfaceDefaults.Compare_Bin8(Inst: Pointer; Left, Right: Int64): Integer;
var
  X, Y: NativeUInt;
begin
  {$ifdef LARGEINT}
    if (Integer(Left) = Integer(Right)) then
    begin
      Left := Left shr 32;
      Right := Right shr 32;
    end else
    begin
      Left := Cardinal(Left);
      Right := Cardinal(Right);
    end;
    X := (Swap(Left) shl 16) + Swap(Left shr 16);
    Y := (Swap(Right) shl 16) + Swap(Right shr 16);
  {$else .SMALLINT}
    X := TPoint(Left).X;
    Y := TPoint(Right).X;
    if (X = Y) then
    begin
      X := TPoint(Left).Y;
      Y := TPoint(Right).Y;
    end;
    X := (Swap(X) shl 16) + Swap(X shr 16);
    Y := (Swap(Y) shl 16) + Swap(Y shr 16);
  {$endif}

  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
end;

class function InterfaceDefaults.Compare_Bin(const Inst: IComparerInst; Left, Right: PByte): Integer;
label
  make_result, make_result_swaped;
var
  X, Y, Count: NativeUInt;
begin
  Count := Inst.Size;
  repeat
    if (Count < SizeOf(NativeUInt)) then Break;
    X := PNativeUInt(Left)^;
    Dec(Count, SizeOf(NativeUInt));
    Y := PNativeUInt(Right)^;
    Inc(Left, SizeOf(NativeUInt));
    Inc(Right, SizeOf(NativeUInt));

    if (X <> Y) then
    begin
      {$ifdef LARGEINT}
        if (Integer(X) = Integer(Y)) then
        begin
          X := X shr 32;
          Y := Y shr 32;
        end else
        begin
          X := Cardinal(X);
          Y := Cardinal(Y);
        end;
      {$endif}

      goto make_result;
    end;
  until (False);

  // read last
  {$ifdef LARGEINT}
  if (Count >= SizeOf(Cardinal)) then
  begin
    X := PCardinal(Left)^;
    Dec(Count, SizeOf(Cardinal));
    Y := PCardinal(Right)^;
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));

    if (X <> Y) then goto make_result;
  end;
  {$endif}

  case Count of
    1: begin
         X := PByte(Left)^;
         Y := PByte(Right)^;
         Result := Integer(X) - Integer(Y);
         Exit;
       end;
    2: begin
         X := Swap(PWord(Left)^);
         Y := Swap(PWord(Right)^);
         Result := Integer(X) - Integer(Y);
         Exit;
       end;
    3: begin
         X := Swap(PWord(Left)^);
         Y := Swap(PWord(Right)^);
         Inc(Left, SizeOf(Word));
         Inc(Right, SizeOf(Word));
         X := (X shl 8) or PByte(Left)^;
         Y := (Y shl 8) or PByte(Right)^;
         Result := Integer(X) - Integer(Y);
         Exit;
       end;
  else
    // 0
    Result := 0;
    Exit;
  end;

make_result:
  X := (Swap(X) shl 16) + Swap(X shr 16);
  Y := (Swap(Y) shl 16) + Swap(Y shr 16);

make_result_swaped:
  Result := Shortint(Byte(X >= Y) - Byte(X <= Y));
end;

class function InterfaceDefaults.Equals_Bin(const Inst: IEqualityComparerInst; Left, Right: PByte): Boolean;
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  done;
var
  Count: NativeUInt;
begin
  if (Left = Right) then goto done;
  Count := Inst.Size;

  // natives (40 bytes static) compare
  case Count shr {$ifdef LARGEINT}3{$else}2{$endif} of
  {$ifdef SMALLINT}
   10: goto cmp10;
    9: goto cmp9;
    8: goto cmp8;
    7: goto cmp7;
    6: goto cmp6;
  {$endif}
    5: goto cmp5;
    4: goto cmp4;
    3: goto cmp3;
    2: goto cmp2;
    1: goto cmp1;
    0: goto cmp0;
  else
    repeat
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Dec(Count, SizeOf(NativeUInt));
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    until (Count < (40 + SizeOf(NativeUInt) - 1));

    {$ifdef SMALLINT}
    cmp10:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp9:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp8:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp7:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp6:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    {$endif}
    cmp5:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp4:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp3:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp2:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp1:
      Dec(Count, SizeOf(NativeUInt));
      if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto done;
      Inc(Left, SizeOf(NativeUInt));
      Inc(Right, SizeOf(NativeUInt));
    cmp0:
  end;

  {$ifdef LARGEINT}
  if (Count and 4 <> 0) then
  begin
    if (PCardinal(Left)^ <> PCardinal(Right)^) then goto done;
    Inc(Left, SizeOf(Cardinal));
    Inc(Right, SizeOf(Cardinal));
  end;
  {$endif}

  if (Count and 3 <> 0) then
  begin
    if (Count and 2 <> 0) then
    begin
      if (PWord(Left)^ <> PWord(Right)^) then goto done;
    end;
    Dec(Left);
    Dec(Right);
    if (Left[Count] <> Right[Count]) then goto done;
  end;

// Result := True
  Left := nil;
  Right := nil;
done:
  Result := (Left = Right);
end;

class function InterfaceDefaults.GetHashCode_Bin(const Inst: IEqualityComparerInst; Value: PByte): Integer;
label
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10;
var
  Count: NativeUInt;
  M: Integer;
begin
  Count := Inst.Size;

  if (Count >= SizeOf(Integer)) then
  begin
    Result := Integer(Count) + PInteger(@Value[Count - SizeOf(Integer)])^ * 63689;

    case (Count - 1) shr 2 of
     10: goto hash10;
      9: goto hash9;
      8: goto hash8;
      7: goto hash7;
      6: goto hash6;
      5: goto hash5;
      4: goto hash4;
      3: goto hash3;
      2: goto hash2;
      1: goto hash1;
      0: goto hash0;
    else
      Dec(Count);
      M := -1660269137;
      repeat
        Result := Result * M + PInteger(Value)^;
        Dec(Count, SizeOf(Integer));
        Inc(Value, SizeOf(Integer));
        M := M * 378551;
      until (Count <= 43);

      hash10:
        Result := Result * 631547855 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash9:
        Result := Result * -1987506439 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash8:
        Result := Result * -1653913089 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash7:
        Result := Result * -186114231 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash6:
        Result := Result * 915264303 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash5:
        Result := Result * -794603367 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash4:
        Result := Result * 135394143 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash3:
        Result := Result * 2012804575 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash2:
        Result := Result * -1092754919 + PInteger(Value)^;
        Inc(Value, SizeOf(Integer));
      hash1:
        Result := Result * -1660269137 + PInteger(Value)^;
      hash0:
    end;

    Inc(Result, ((Result shr 8) * 63689) + ((Result shr 16) * -1660269137) +
      ((Result shr 24) * -1092754919));
    Exit;
  end else
  if (Count <> 0) then
  begin
    Result := Integer(Value[0]);
    Result := Result + (Result shr 4) * 63689;
    if (Count > 1) then
    begin
      Result := Result + Integer(Value[1]) * -1660269137;
      if (Count > 2) then
      begin
        Result := Result + Integer(Value[2]) * -1092754919;
      end;
    end;
  end else
  begin
    Result := 0;
  end;
end;

{ TComparer<T> }

class function TComparer<T>.Default: IComparer<T>;
begin
  Result := IComparer<T>(InterfaceDefaults.TDefaultComparer<T>.Create);
end;

class function TComparer<T>.DefaultComparison: TComparison<T>;
begin
  Result := TComparison<T>(IInterface(InterfaceDefaults.TDefaultComparer<T>.Create));
end;

class function TComparer<T>.IsDefault(const Comparer: IComparer<T>): Boolean;
begin
  Result := (Pointer(Comparer) = @InterfaceDefaults.TDefaultComparer<T>.Instance);
end;

class function TComparer<T>.IsDefaultComparison(const Comparison: TComparison<T>): Boolean;
begin
  Result := (Pointer(@Comparison) = @InterfaceDefaults.TDefaultComparer<T>.Instance);
end;

class function TComparer<T>.Construct(const Comparison: TComparison<T>): IComparer<T>;
begin
  { Much faster way to have IComparer<T> interface, than
    TDelegatedComparer<T> instance }
  IInterface(Result) := IInterface(PPointer(@Comparison)^);
end;

{ TEqualityComparer<T> }

class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
begin
  Result := IEqualityComparer<T>(InterfaceDefaults.TDefaultEqualityComparer<T>.Create);
end;

class function TEqualityComparer<T>.DefaultComparison: TEqualityComparer<T>;
begin
  Result := TEqualityComparer<T>(IInterface(InterfaceDefaults.TDefaultEqualityComparer<T>.Create));
end;

class function TEqualityComparer<T>.IsDefault(const EqualityComparer: IEqualityComparer<T>): Boolean;
begin
  Result := (Pointer(EqualityComparer) = @InterfaceDefaults.TDefaultEqualityComparer<T>.Instance);
end;

class function TEqualityComparer<T>.IsDefaultComparison(const EqualityComparison: TEqualityComparison<T>): Boolean;
begin
  Result := (Pointer(@EqualityComparison) = @InterfaceDefaults.TDefaultEqualityComparer<T>.Instance);
end;

class function TEqualityComparer<T>.Construct(
  const EqualityComparison: TEqualityComparison<T>;
  const Hasher: THasher<T>): IEqualityComparer<T>;
begin
  Result := TDelegatedEqualityComparer<T>.Create(EqualityComparison, Hasher);
end;


{ TSingletonImplementation }

function TSingletonImplementation.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TSingletonImplementation._AddRef: Integer;
begin
  Result := -1;
end;

function TSingletonImplementation._Release: Integer;
begin
  Result := -1;
end;


{ Delegated Comparers }

constructor TDelegatedComparer<T>.Create(const ACompare: TComparison<T>);
begin
  FCompare := ACompare;
end;

function TDelegatedComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := FCompare(Left, Right);
end;

constructor TDelegatedEqualityComparer<T>.Create(const AEquals: TEqualityComparison<T>; const AGetHashCode: THasher<T>);
begin
  FEquals := AEquals;
  FGetHashCode := AGetHashCode;
end;

function TDelegatedEqualityComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  Result := FEquals(Left, Right);
end;

function TDelegatedEqualityComparer<T>.GetHashCode(const Value: T): Integer;
begin
  Result := FGetHashCode(Value);
end;


{ TOrdinalStringComparer }

type
  TOrdinalStringComparer = class(TStringComparer)
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean;
      reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer;
      reintroduce; overload; override;
  end;

function TOrdinalStringComparer.Compare(const Left, Right: string): Integer;
{$ifNdef CPUINTELASM}
begin
  Result := InterfaceDefaults.Compare_UStr(nil, Pointer(Left), Pointer(Right));
end;
{$else}
asm
  jmp InterfaceDefaults.Compare_UStr
end;
{$endif}

function TOrdinalStringComparer.Equals(const Left, Right: string): Boolean;
{$ifNdef CPUINTELASM}
begin
  Result := InterfaceDefaults.Equals_UStr(nil, Pointer(Left), Pointer(Right));
end;
{$else}
asm
  jmp InterfaceDefaults.Equals_UStr
end;
{$endif}

function TOrdinalStringComparer.GetHashCode(const Value: string): Integer;
{$ifNdef CPUINTELASM}
begin
  Result := InterfaceDefaults.GetHashCode_UStr(nil, Pointer(Value));
end;
{$else}
asm
  jmp InterfaceDefaults.GetHashCode_UStr
end;
{$endif}

{ TStringComparer }

class destructor TStringComparer.Destroy;
begin
  FreeAndNil(FOrdinal);
end;

class function TStringComparer.Ordinal: TStringComparer;
begin
  if FOrdinal = nil then
    FOrdinal := TOrdinalStringComparer.Create;
  Result := TStringComparer(FOrdinal);
end;

{ TOrdinalIStringComparer }

function TOrdinalIStringComparer.Compare(const Left, Right: string): Integer;
begin
  Result := AnsiCompareText(Left, Right);
end;

function TOrdinalIStringComparer.Equals(const Left, Right: string): Boolean;
var
  Count: Integer;
begin
  if (NativeInt(Left) <> NativeInt(Right)) and (Pointer(Left) <> nil) and (Pointer(Right) <> nil) then
  begin
    Count := PInteger(NativeUInt(Left) - SizeOf(Integer))^;
    Result := (Count = PInteger(NativeUInt(Right) - SizeOf(Integer))^) and
      (0 = AnsiCompareText(Left, Right));
  end else
  begin
    Result := (NativeUInt(Left) = NativeUInt(Right));
  end;
end;

function TOrdinalIStringComparer.CharsLower(Dest, Src: PWideChar; Count: Integer): Boolean;
{$IF defined(MSWINDOWS)}
begin
  Move(Src^, Dest^, Count * SizeOf(WideChar));
  CharLowerBuff(Dest, Count);
  Result := True;
end;
{$ELSEIF defined(USE_LIBICU)}
var
  ErrorCode: UErrorCode;
begin
  ErrorCode := U_ZERO_ERROR;
  Result := (Count = u_strToLower(Dest, Count, Src, Count, UTF8CompareLocale, ErrorCode)) and
    (ErrorCode <= U_ZERO_ERROR);
end;
{$ELSEIF defined(MACOS)}
var
  MutableStringRef: CFMutableStringRef;
begin
  Move(Src^, Dest^, Count * SizeOf(WideChar));

  MutableStringRef := CFStringCreateMutableWithExternalCharactersNoCopy(kCFAllocatorDefault,
    Dest, Count, Count, kCFAllocatorNull);
  if (MutableStringRef <> nil) then
  begin
    try
      CFStringLowercase(MutableStringRef, UTF8CompareLocale);
      Result := (Count = CFStringGetLength(CFStringRef(MutableStringRef)));
    finally
      CFRelease(MutableStringRef);
    end;
  end else
  begin
    Result := False;
  end;
end;
{$ELSEIF Defined(POSIX)}
begin
  Result := False;
end;
{$IFEND POSIX}

function TOrdinalIStringComparer.GetHashCodeLower(const Value: string): Integer;
var
  S: string;
begin
  S := AnsiLowerCase(Value);
  Result := InterfaceDefaults.GetHashCode_UStr(nil, Pointer(S));
end;

function TOrdinalIStringComparer.GetHashCode(const Value: string): Integer;
var
  Count: Integer;
  Buffer: packed record
    _Padding: array[1..12] of Byte;
    Count: Integer;
    Chars: array[0..1024] of WideChar;
  end;
begin
  if (NativeInt(Value) <> 0) then
  begin
    Count := PInteger(NativeUInt(Value) - SizeOf(Integer))^;
    if (Count <= 1024) and (CharsLower(@Buffer.Chars[0], Pointer(Value), Count)) then
    begin
      Buffer.Count := Count;
      Buffer.Chars[Count] := #0;
      Result := InterfaceDefaults.GetHashCode_UStr(nil, Pointer(@Buffer.Chars[0]));
    end else
    begin
      Result := GetHashCodeLower(Value);
    end;
  end else
  begin
    Result := 0;
  end;
end;


{ TIStringComparer }

class destructor TIStringComparer.Destroy;
begin
  FreeAndNil(FOrdinal);
end;

class function TIStringComparer.Ordinal: TStringComparer;
begin
  if FOrdinal = nil then
    FOrdinal := TOrdinalIStringComparer.Create;
  Result := TStringComparer(FOrdinal);
end;


{ TEnumerator_ }

procedure TEnumerator_.Reset;
begin
  DoReset;
end;

procedure TEnumerator_.DoReset;
begin
  raise ENotSupportedException.CreateResFmt(Pointer(@SMethodNotSupported), ['Reset']);
end;

function TEnumerator_.MoveNext: Boolean;
begin
  Result := DoMoveNext;
end;

{ TEnumerator<T> }

function TEnumerator<T>.DoGetCurrentObject: TObject;
begin
  if ({$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} = tkClass) then
  begin
    T(Pointer(@Result)^) := DoGetCurrent;
  end else
  begin
    Result := nil;
  end;
end;


{ TEnumerable_ }

function TEnumerable_.GetObjectEnumerator: IEnumerator;
begin
  Result := DoGetObjectEnumerator;
end;

function TEnumerable_.GetEnumerator: TEnumerator_;
begin
  Result := DoGetObjectEnumerator;
end;

{ TEnumerable<T> }

// The overridden destructor that simply invokes 'inherited' is
// required to instantiate the destructor for C++ code
destructor TEnumerable<T>.Destroy;
begin
  inherited;
end;

function TEnumerable<T>.DoGetObjectEnumerator: TEnumerator_;
begin
  if ({$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} = tkClass) then
  begin
    Result := DoGetEnumerator;
  end else
  begin
    Result := nil;
  end;
end;

function TEnumerable<T>.GetEnumerator_: IEnumerator<T>;
begin
  Result := DoGetEnumerator;
end;

function TEnumerable<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := DoGetEnumerator;
end;

function TEnumerable<T>.ToArray: TArray<T>;
var
  Count, Buffered: NativeUInt;
  Value: T;
begin
  Count := 0;
  Buffered := 16;
  Result := nil;
  SetLength(Result, Buffered);

  for Value in Self do
  begin
    if (Count = Buffered) then
    begin
      Buffered := Buffered * 2;
      SetLength(Result, Buffered);
    end;

    Result[Count] := Value;
    Inc(Count);
  end;

  SetLength(Result, Count);
end;

{ TPair<TKey,TValue> }

constructor TPair<TKey,TValue>.Create(const AKey: TKey; const AValue: TValue);
begin
  Key := AKey;
  Value := AValue;
end;


{ TArray }

{$ifdef WEAKREF}
class procedure TArray.WeakExchange<T>(const Left, Right: Pointer);
var
  Buffer: T;
begin
  Buffer := T(Left^);
  T(Left^) := T(Right^);
  T(Right^) := Buffer;
end;
{$endif}

class procedure TArray.Exchange<T>(const Left, Right: Pointer);
var
  Index: NativeInt;
  Temp1: Byte;
  Temp2: Word;
  Temp4: Cardinal;
  TempNative: NativeUInt;
begin
  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<T>.Weak) then
  {$else}
  if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
  if (TRAIIHelper<T>.FOptions.FWeak) then
  {$endif}
  begin
    TArray.WeakExchange<T>(Left, Right);
  end else
  {$endif}
  with TLMemory(Left^), TRMemory(Right^) do
  case SizeOf(T) of
    0: ;
    1:
    begin
      Temp1 := LBytes[0];
      LBytes[0] := RBytes[0];
      RBytes[0] := Temp1;
    end;
    2:
    begin
      Temp2 := LWords[0];
      LWords[0] := RWords[0];
      RWords[0] := Temp2;
    end;
    3:
    begin
      Temp2 := LWords[0];
      LWords[0] := RWords[0];
      RWords[0] := Temp2;

      Temp1 := LBytes[2];
      LBytes[2] := RBytes[2];
      RBytes[2] := Temp1;
    end;
    4..7:
    begin
      Temp4 := LCardinals[0];
      LCardinals[0] := RCardinals[0];
      RCardinals[0] := Temp4;

      case SizeOf(T) of
        5:
        begin
          Temp1 := LBytes[4];
          LBytes[4] := RBytes[4];
          RBytes[4] := Temp1;
        end;
        6:
        begin
          Temp2 := LWords[2];
          LWords[2] := RWords[2];
          RWords[2] := Temp2;
        end;
        7:
        begin
          Temp2 := LWords[2];
          LWords[2] := RWords[2];
          RWords[2] := Temp2;
          Temp1 := LBytes[6];
          LBytes[6] := RBytes[6];
          RBytes[6] := Temp1;
        end;
      end;
    end;
    8..16:
    begin
      TempNative := LNatives[0];
      LNatives[0] := RNatives[0];
      RNatives[0] := TempNative;

      if (SizeOf(T) >= SizeOf(NativeUInt) * 2) then
      begin
        TempNative := LNatives[1];
        LNatives[1] := RNatives[1];
        RNatives[1] := TempNative;
      end;

      if (SizeOf(T) >= SizeOf(NativeUInt) * 3) then
      begin
        TempNative := LNatives[2];
        LNatives[2] := RNatives[2];
        RNatives[2] := TempNative;
      end;

      if (SizeOf(T)  = SizeOf(NativeUInt) * 4) then
      begin
        TempNative := LNatives[3];
        LNatives[3] := RNatives[3];
        RNatives[3] := TempNative;
      end;

      {$ifdef LARGEINT}
      case SizeOf(T) of
        12, 13, 14, 15:
        begin
          Temp4 := LCardinals[2];
          LCardinals[2] := RCardinals[2];
          RCardinals[2] := Temp4;
        end;
      end;
      {$endif}

      case SizeOf(T) of
        9:
        begin
          Temp1 := LBytes[8];
          LBytes[8] := RBytes[8];
          RBytes[8] := Temp1;
        end;
        10:
        begin
          Temp2 := LWords[4];
          LWords[4] := RWords[4];
          RWords[4] := Temp2;
        end;
        11:
        begin
          Temp2 := LWords[4];
          LWords[4] := RWords[4];
          RWords[4] := Temp2;
          Temp1 := LBytes[10];
          LBytes[10] := RBytes[10];
          RBytes[10] := Temp1;
        end;
        13:
        begin
          Temp2 := LWords[5];
          LWords[5] := RWords[5];
          RWords[5] := Temp2;
          Temp1 := LBytes[12];
          LBytes[12] := RBytes[12];
          RBytes[12] := Temp1;
        end;
        14:
        begin
          Temp2 := LWords[6];
          LWords[6] := RWords[6];
          RWords[6] := Temp2;
        end;
        15:
        begin
          Temp2 := LWords[6];
          LWords[6] := RWords[6];
          RWords[6] := Temp2;
          Temp1 := LBytes[14];
          LBytes[14] := RBytes[14];
          RBytes[14] := Temp1;
        end;
      end;
    end;
  else
    Index := 0;
    repeat
      TempNative := LNatives[Index];
      LNatives[Index] := RNatives[Index];
      RNatives[Index] := TempNative;
      Inc(Index);
    until (Index = SizeOf(T) div SizeOf(NativeUInt));

    if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
    begin
      {$ifdef LARGEINT}
      if (SizeOf(T) and 4 <> 0) then
      begin
        Index := SizeOf(T) div SizeOf(Cardinal) - 1;
        Temp4 := LCardinals[Index];
        LCardinals[Index] := RCardinals[Index];
        RCardinals[Index] := Temp4;
      end;
      {$endif}

      if (SizeOf(T) and 2 <> 0) then
      begin
        Index := SizeOf(T) div SizeOf(Word) - 1;
        Temp2 := LWords[Index];
        LWords[Index] := RWords[Index];
        RWords[Index] := Temp2;
      end;

      if (SizeOf(T) and 1 <> 0) then
      begin
        Index := SizeOf(T) div SizeOf(Byte) - 1;
        Temp1 := LBytes[Index];
        LBytes[Index] := RBytes[Index];
        RBytes[Index] := Temp1;
      end;
    end;
  end;
end;

class procedure TArray.Copy<T>(const Destination, Source: Pointer);
var
  Index: NativeInt;
begin
  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<T>.Weak) then
  {$else}
  if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
  if (TRAIIHelper<T>.FOptions.FWeak) then
  {$endif}
  begin
    T(Destination^) := T(Source^);
  end else
  {$endif}
  with TLMemory(Destination^), TRMemory(Source^) do
  case SizeOf(T) of
    0: ;
    1: LBytes[0] := RBytes[0];
    2: LWords[0] := RWords[0];
    3:
    begin
      LWords[0] := RWords[0];
      LBytes[2] := RBytes[2];
    end;
    4..7:
    begin
      LCardinals[0] := RCardinals[0];

      case SizeOf(T) of
        5: LCardinals1[0] := RCardinals1[0];
        6: LCardinals2[0] := RCardinals2[0];
        7: LCardinals3[0] := RCardinals3[0];
      end;
    end;
    8..16:
    begin
      LNatives[0] := RNatives[0];

      if (SizeOf(T) >= SizeOf(NativeUInt) * 2) then
        LNatives[1] := RNatives[1];

      if (SizeOf(T) >= SizeOf(NativeUInt) * 3) then
        LNatives[2] := RNatives[2];

      if (SizeOf(T)  = SizeOf(NativeUInt) * 4) then
        LNatives[3] := RNatives[3];

      {$ifdef SMALLINT}
      case SizeOf(T) of
         9: LNatives1[1] := RNatives1[1];
        10: LNatives2[1] := RNatives2[1];
        11: LNatives3[1] := RNatives3[1];
        13: LNatives1[2] := RNatives1[2];
        14: LNatives2[2] := RNatives2[2];
        15: LNatives3[2] := RNatives3[2];
      end;
      {$else .LARGEINT}
      case SizeOf(T) of
         9: LNatives1[1] := RNatives1[1];
        10: LNatives2[1] := RNatives2[1];
        11: LNatives3[1] := RNatives3[1];
        12: LNatives4[1] := RNatives4[1];
        13: LNatives5[1] := RNatives5[1];
        14: LNatives6[1] := RNatives6[1];
        15: LNatives7[1] := RNatives7[1];
      end;
      {$endif}
    end;
  else
    Index := 0;
    repeat
      LNatives[Index] := RNatives[Index];
      Inc(Index);
    until (Index = SizeOf(T) div SizeOf(NativeUInt) - 1);

    if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
    begin
      Index := SizeOf(T) div SizeOf(NativeUInt) - 1;
      case SizeOf(T) and (SizeOf(NativeUInt) - 1) of
        1: LNatives1[Index] := RNatives1[Index];
        2: LNatives2[Index] := RNatives2[Index];
        3: LNatives3[Index] := RNatives3[Index];
      {$ifdef LARGEINT}
        4: LNatives4[Index] := RNatives4[Index];
        5: LNatives5[Index] := RNatives5[Index];
        6: LNatives6[Index] := RNatives6[Index];
        7: LNatives7[Index] := RNatives7[Index];
      {$endif}
      end;
    end;
  end;
end;

class procedure TArray.FillZero<T>(const Values: Pointer);
var
  Null4: Cardinal;
  NullNative: NativeUInt;
  Index: NativeInt;
begin
  with TLMemory(Values^) do
  case SizeOf(T) of
    0: ;
    1: LBytes[0] := 0;
    2: LWords[0] := 0;
    3:
    begin
      LWords[0] := 0;
      LBytes[2] := 0;
    end;
    4..7:
    begin
      Null4 := 0;
      LCardinals[0] := Null4;

      case SizeOf(T) of
        5: LCardinals1[0] := Null4;
        6: LCardinals2[0] := Null4;
        7: LCardinals3[0] := Null4;
      end;
    end;
    8..16:
    begin
      NullNative := 0;
      LNatives[0] := NullNative;

      if (SizeOf(T) >= SizeOf(NativeUInt) * 2) then
        LNatives[1] := NullNative;

      if (SizeOf(T) >= SizeOf(NativeUInt) * 3) then
        LNatives[2] := NullNative;

      if (SizeOf(T)  = SizeOf(NativeUInt) * 4) then
        LNatives[3] := NullNative;

      {$ifdef SMALLINT}
      case SizeOf(T) of
         9: LNatives1[1] := NullNative;
        10: LNatives2[1] := NullNative;
        11: LNatives3[1] := NullNative;
        13: LNatives1[2] := NullNative;
        14: LNatives2[2] := NullNative;
        15: LNatives3[2] := NullNative;
      end;
      {$else .LARGEINT}
      case SizeOf(T) of
         9: LNatives1[1] := NullNative;
        10: LNatives2[1] := NullNative;
        11: LNatives3[1] := NullNative;
        12: LNatives4[1] := NullNative;
        13: LNatives5[1] := NullNative;
        14: LNatives6[1] := NullNative;
        15: LNatives7[1] := NullNative;
      end;
      {$endif}
    end;
  else
    Index := 0;
    repeat
      LNatives[Index] := NullNative;
      Inc(Index);
    until (Index = SizeOf(T) div SizeOf(NativeUInt) - 1);

    if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
    begin
      Index := SizeOf(T) div SizeOf(NativeUInt) - 1;
      case SizeOf(T) and (SizeOf(NativeUInt) - 1) of
        1: LNatives1[Index] := NullNative;
        2: LNatives2[Index] := NullNative;
        3: LNatives3[Index] := NullNative;
      {$ifdef LARGEINT}
        4: LNatives4[Index] := NullNative;
        5: LNatives5[Index] := NullNative;
        6: LNatives6[Index] := NullNative;
        7: LNatives7[Index] := NullNative;
      {$endif}
      end;
    end;
  end;
end;

{$ifdef WEAKREF}
class procedure TArray.WeakReverse<T>(const Values: Pointer; const Count: NativeInt);
var
  X, Y: ^T;
  Buffer: T;
begin
  if (Count > 1) then
  begin
    X := Values;
    Y := TRAIIHelper<T>.P(Values) + Count - 1;

    repeat
      Buffer := X^;
      X^ := Y^;
      Y^ := Buffer;

      Inc(X);
      Dec(Y);
    until (NativeUInt(X) >= NativeUInt(Y));
  end;
end;
{$endif}

class procedure TArray.Reverse<T>(const Values: Pointer; const Count: NativeInt);
var
  X, Y: Pointer;
  Index: NativeInt;
  Temp1: Byte;
  Temp2: Word;
  Temp4: Cardinal;
  TempNative: NativeUInt;
begin
  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<T>.Weak) then
  {$else}
  if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
  if (TRAIIHelper<T>.FOptions.FWeak) then
  {$endif}
  begin
    TArray.WeakReverse<T>(Values, Count);
  end else
  {$endif}
  if (Count > 1) then
  begin
    X := Values;
    Y := TRAIIHelper<T>.P(Values) + Count - 1;

    repeat
      if (SizeOf(T) <= 16) then
      begin
        TArray.Exchange<T>(X, Y);
        Inc(NativeUInt(X), SizeOf(T));
        Dec(NativeUInt(Y), SizeOf(T));
      end else
      begin
        Index := 0;
        repeat
          Inc(Index);
          TempNative := PNativeUInt(X)^;
          PNativeUInt(X)^ := PNativeUInt(Y)^;
          PNativeUInt(Y)^ := TempNative;
          Inc(NativeUInt(X), SizeOf(NativeUInt));
          Inc(NativeUInt(Y), SizeOf(NativeUInt));
        until (Index = SizeOf(T) div SizeOf(NativeUInt));

        {$ifdef LARGEINT}
        if (SizeOf(T) and 4 <> 0) then
        begin
          Temp4 := PCardinal(X)^;
          PCardinal(X)^ := PCardinal(Y)^;
          PCardinal(Y)^ := Temp4;
          Inc(NativeUInt(X), SizeOf(Cardinal));
          Inc(NativeUInt(Y), SizeOf(Cardinal));
        end;
        {$endif}

        if (SizeOf(T) and 2 <> 0) then
        begin
          Temp2 := PWord(X)^;
          PWord(X)^ := PWord(Y)^;
          PWord(Y)^ := Temp2;
          Inc(NativeUInt(X), SizeOf(Word));
          Inc(NativeUInt(Y), SizeOf(Word));
        end;

        if (SizeOf(T) and 1 <> 0) then
        begin
          Temp1 := PByte(X)^;
          PByte(X)^ := PByte(Y)^;
          PByte(Y)^ := Temp1;
          Inc(NativeUInt(X), SizeOf(Byte));
          Inc(NativeUInt(Y), SizeOf(Byte));
        end;

        Dec(NativeUInt(Y), 2 * SizeOf(T));
      end;
    until (NativeUInt(X) >= NativeUInt(Y));
  end;
end;

class procedure TArray.Reverse<T>(var Values: array of T);
begin
  if (High(Values) > 0) then
    TArray.Reverse<T>(@Values[0], Length(Values));
end;

class procedure TArray.Shuffle<T>(const Values: Pointer; const Count: NativeInt);
var
  LValues: TRAIIHelper<T>.PArrayT;
  LCount, LIndex, LRandom: NativeInt;
begin
  if (Count > 1) then
  begin
    LValues := Values;
    LCount := Count;

    LIndex := 0;
    repeat
      LRandom := Random(LCount) + LIndex;
      Exchange<T>(@LValues[LRandom], @LValues[LIndex]);
      Dec(LCount);
      Inc(LIndex);
    until (LCount = 1);
  end;
end;

class procedure TArray.Shuffle<T>(var Values: array of T);
begin
  if (High(Values) > 0) then
    TArray.Shuffle<T>(@Values[0], Length(Values));
end;

procedure TArray.TSortHelper<T>.Init(const Comparer: IComparer<T>);
begin
  Self.Inst := Pointer(Comparer);
  Self.Compare := PPointer(PNativeUInt(Self.Inst)^ + 3 * SizeOf(Pointer))^;
end;

procedure TArray.TSortHelper<T>.Init(const Comparison: TComparison<T>);
begin
  Self.Inst := PPointer(@Comparison)^;
  Self.Compare := PPointer(PNativeUInt(Self.Inst)^ + 3 * SizeOf(Pointer))^;
end;

procedure TArray.TSortHelper<T>.Init;
begin
  Self.Inst := InterfaceDefaults.TDefaultComparer<T>.Create;
  Self.Compare := InterfaceDefaults.TDefaultComparer<T>.Instance.Compare;
end;

procedure TArray.TSortHelper<T>.FillZero;
begin
  FillChar(Self, SizeOf(T), #0);
end;

class procedure TArray.CheckArrays(Source, Destination: Pointer; SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt);
begin
  if (SourceIndex < 0) or (DestIndex < 0) or (SourceIndex >= SourceLength) or (DestIndex >= DestLength) or
     (SourceIndex + Count > SourceLength) or (DestIndex + Count > DestLength) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Source = Destination then
    raise EArgumentException.CreateRes(Pointer(@sSameArrays));
end;

class procedure TArray.Copy<T>(const Source: array of T; var Destination: array of T; SourceIndex, DestIndex, Count: NativeInt);
begin
  CheckArrays(Pointer(@Source[0]), Pointer(@Destination[0]), SourceIndex, Length(Source), DestIndex, Length(Destination), Count);
  if (Count <> 0) then
  begin
    TRAIIHelper<T>.Create;
    if TRAIIHelper<T>.Managed then
      System.CopyArray(Pointer(@Destination[DestIndex]), Pointer(@Source[SourceIndex]), TypeInfo(T), Count)
    else
      System.Move(Pointer(@Source[SourceIndex])^, Pointer(@Destination[DestIndex])^, Count * SizeOf(T));
  end;
end;

class procedure TArray.Copy<T>(const Source: array of T; var Destination: array of T; Count: NativeInt);
begin
  Copy<T>(Source, Destination, 0, 0, Count);
end;

class function TArray.Copy<T>(const Source: array of T; SourceIndex, Count: NativeInt): TArray<T>;
begin
  if (Count < 0) or (SourceIndex + Count > Length(Source)) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  if (Count <> 0) then
  begin
    TRAIIHelper<T>.Create;
    if TRAIIHelper<T>.Managed then
      System.CopyArray(Pointer(Result), Pointer(@Source[SourceIndex]), TypeInfo(T), Count)
    else
      System.Move(Pointer(@Source[SourceIndex])^, Pointer(@Result)^, Count * SizeOf(T));
  end;
end;

class function TArray.Copy<T>(const Source: array of T): TArray<T>;
var
  Count: NativeInt;
begin
  Count := Length(Source);
  SetLength(Result, Count);
  if (Count <> 0) then
  begin
    TRAIIHelper<T>.Create;
    if TRAIIHelper<T>.Managed then
      System.CopyArray(Pointer(Result), Pointer(@Source[0]), TypeInfo(T), Count)
    else
      System.Move(Pointer(@Source[0])^, Pointer(@Result)^, Count * SizeOf(T));
  end;
end;

class function TArray.SortItemPivot<T>(const I, J: Pointer): Pointer;
var
  Index: NativeInt;
begin
  if (SizeOf(T) and (SizeOf(T) - 1) = 0) and (SizeOf(T) <= 256) then
  begin
    Index := NativeInt(J) - NativeInt(I);
    case SizeOf(T) of
    0, 1: Index := Index shr 1;
       2: Index := Index shr 2;
       4: Index := Index shr 3;
       8: Index := Index shr 4;
      16: Index := Index shr 5;
      32: Index := Index shr 6;
      64: Index := Index shr 7;
     128: Index := Index shr 8;
    else
    // 256:
      Index := Index shr 9;
    end;
  end else
  begin
    Index := NativeInt(Round((NativeInt(J) - NativeInt(I)) * (1 / SizeOf(T)))) shr 1;
  end;

  Result := TRAIIHelper<T>.P(I) + Index;
end;

class function TArray.SortItemNext<T>(const StackItem, I, J: Pointer): Pointer;
var
  Item: ^TSortStackItem<T>;
  DiffI, DiffJ: NativeInt;
  Buf: Pointer;
begin
  Item := StackItem;

  // next "recursion" iteration
  // if (i < last) qs(s_arr, i, last);
  // if (first < j) qs(s_arr, first, j);
  DiffI := NativeInt(Item^.Last) - NativeInt(I);
  DiffJ := NativeInt(J) - NativeInt(Item^.First);
  if (DiffI > 0) then
  begin
    if (DiffJ <= 0) then
    begin
      Item^.First := I;
      // Item.Last := Item.Last;
    end else
    if (DiffI >= DiffJ) then
    begin
      // i..last, first..j
      Buf := Item^.First;
      Item^.First := I;
      Inc(Item);
      Item^.First := Buf;
      Item^.Last := J;
    end else
    begin
      // first..j, i..last
      Buf := Item^.Last;
      Item^.Last := J;
      Inc(Item);
      Item^.First := I;
      Item^.Last := Buf;
    end;
  end else
  if (DiffJ > 0) then
  begin
    // Item.First := Item.First;
    Item^.Last := J;
  end else
  begin
    Inc(NativeInt(Item), HIGH_NATIVE_BIT);
  end;

  Result := Item;
end;

{$ifdef SMARTGENERICS}
class function TArray.SortItemCount<T>(const I, J: Pointer): NativeInt;
begin
  if (SizeOf(T) and (SizeOf(T) - 1) = 0) and (SizeOf(T) <= 256) then
  begin
    Result := NativeInt(J) + SizeOf(T) - NativeInt(I);
    case SizeOf(T) of
       2: Result := Result shr 1;
       4: Result := Result shr 2;
       8: Result := Result shr 3;
      16: Result := Result shr 4;
      32: Result := Result shr 5;
      64: Result := Result shr 6;
     128: Result := Result shr 7;
     256: Result := Result shr 8;
    end;
  end else
  begin
    Result := Round((NativeInt(J) + SizeOf(T) - NativeInt(I)) * (1 / SizeOf(T)));
  end;
end;

class function TArray.SortBinaryMarker<T>(const Binary: Pointer): NativeUInt;
begin
  case GetTypeKind(T) of
    tkMethod: Result := NativeUInt(TMethod(Binary^).Data);
    tkLString, tkWString, tkUString, tkDynArray:
    begin
      Result := PNativeUInt(Binary)^;
      if (Result <> 0) then
      case GetTypeKind(T) of
        tkLString:
        begin
          Result := PWord(Result)^;
          Result := Swap(Result);
        end;
        {$ifdef MSWINDOWS}
        tkWString:
        begin
          Dec(Result, SizeOf(Integer));
          if (PInteger(Result)^ = 0) then
          begin
            Result := 0;
            Exit;
          end else
          begin
            Inc(Result, SizeOf(Integer));
            Result := PCardinal(Result)^;
            Result := Cardinal((Result shl 16) + (Result shr 16));
          end;
        end;
        {$else}
        tkWString,
        {$endif}
        tkUString:
        begin
          Result := PCardinal(Result)^;
          Result := Cardinal((Result shl 16) + (Result shr 16));
        end;
        tkDynArray:
        begin
          Result := PByte(Result)^;
        end;
      end;
    end;
    tkString:
    begin
      Result := PWord(Binary)^;
      if (Result and $ff = 0) then
      begin
        Result := 0;
      end else
      begin
        Result := Result shr 8;
      end;
    end;
  else
    with TLMemory(Binary^) do
    case SizeOf(T) of
      1: Result := LBytes[0];
      2:
      begin
        Result := LWords[0];
        Result := Swap(Result);
      end;
      3:
      begin
        Result := LWords[0];
        Result := Swap(Result);
        Result := Result shl 8;
        Inc(Result, LBytes[2]);
      end;
    else
      Result := LCardinals[0];
      Result := (Swap(Result) shl 16) + Swap(Result shr 16);
    end;
  end;
end;

class procedure TArray.RadixSort<T>(const Values: Pointer; const Count, Flags: NativeInt);
label
  clean_radixes;
var
  i, Mask, Index, TargetIndex: NativeInt;
  Source, TopSourceFirst, TopSource: PByte;
  Radixes: PRadixes;
  Radix, TopRadix: PWord;
  Target: NativeInt;
  Stored: TInternalRadixStored<T>;
begin
  Stored.SingleRadix := Count * SizeOf(T);
  Stored.Ptr[0] := Values;
  Stored.Ptr[1] := @Stored.Data;
  Stored.Index := 0;

  i := 0;
  repeat
    // clear radixes
    FillChar(Stored.Radixes, SizeOf(Stored.Radixes), #0);
  clean_radixes:

    // xor mask
    Mask := Flags;
    if (i = SizeOf(T) - 1) then Mask := Mask shr 8;
    Mask := Byte(Mask);

    // fill radixes basics
    Source := Pointer(Stored.Ptr[Stored.Index]);
    Inc(Source, i);
    TopSourceFirst := Pointer(TRAIIHelper<T>.P(Source) + Count);
    repeat
      Index := Source^;
      Index := Index xor Mask;
      Radixes := @Stored.Radixes;
      Inc(Radixes[Index], SizeOf(T));
      Inc(Source, SizeOf(T));
    until (Source = TopSourceFirst);

    // check the same radixes
    Dec(Source, SizeOf(T));
    Index := Source^;
    Index := Index xor Mask;
    if (Stored.Radixes[Index] = Stored.SingleRadix) then
    begin
      Inc(i);
      if (i = SizeOf(T)) then Break;

      Stored.Radixes[Index] := 0;
      goto clean_radixes;
    end;
    Stored.Mask := Mask;

    // calculate offsets
    Radix := @Stored.Radixes[0];
    TopRadix := Radix + 256;
    Index := 0;
    repeat
      TargetIndex := Radix^;
      Inc(Radix);
      if (TargetIndex = 0) then
      begin
        if (Radix = TopRadix) then Break;
      end else
      begin
        Dec(Radix);
        Radix^ := Index;
        Inc(Index, TargetIndex);
        Inc(Radix);
        if (Radix = TopRadix) then Break;
      end;
    until (False);

    // fill target
    Source := Pointer(Stored.Ptr[Stored.Index]);
    Inc(Source, i);
    Stored.Index := Stored.Index xor 1;
    TopSource := Pointer(TRAIIHelper<T>.P(Source) + Count);
    Target := NativeInt(Stored.Ptr[Stored.Index]) - SizeOf(T);
    case Stored.Mask of
      $80:
      begin
        repeat
          Index := Source^;
          Index := Index xor $80;

          Radixes := @Stored.Radixes;
          TargetIndex := Radixes[Index];
          Inc(TargetIndex, SizeOf(T));
          Radixes[Index] := TargetIndex;

          TRAIIHelper<T>.P(TargetIndex + Target)^ := TRAIIHelper<T>.P(Source - i)^;
          Inc(Source, SizeOf(T));
        until (Source = TopSource);
      end;
      $ff:
      begin
        repeat
          Index := Source^;
          Index := Index xor $ff;

          Radixes := @Stored.Radixes;
          TargetIndex := Radixes[Index];
          Inc(TargetIndex, SizeOf(T));
          Radixes[Index] := TargetIndex;

          TRAIIHelper<T>.P(TargetIndex + Target)^ := TRAIIHelper<T>.P(Source - i)^;
          Inc(Source, SizeOf(T));
        until (Source = TopSource);
      end;
      $7f:
      begin
        repeat
          Index := Source^;
          Index := Index xor $7f;

          Radixes := @Stored.Radixes;
          TargetIndex := Radixes[Index];
          Inc(TargetIndex, SizeOf(T));
          Radixes[Index] := TargetIndex;

          TRAIIHelper<T>.P(TargetIndex + Target)^ := TRAIIHelper<T>.P(Source - i)^;
          Inc(Source, SizeOf(T));
        until (Source = TopSource);
      end;
    else
      // $00
      repeat
        Index := Source^;

        Radixes := @Stored.Radixes;
        TargetIndex := Radixes[Index];
        Inc(TargetIndex, SizeOf(T));
        Radixes[Index] := TargetIndex;

        TRAIIHelper<T>.P(TargetIndex + Target)^ := TRAIIHelper<T>.P(Source - i)^;
        Inc(Source, SizeOf(T));
      until (Source = TopSource);
    end;

    // next iteration
    Inc(i);
  until (i = SizeOf(T));

  // ValuesArray guarantee
  if (Stored.Index <> 0) then
    System.Move(Stored.Data, Stored.Ptr[0]^, Count * SizeOf(T));
end;

class function TArray.RadixSortSigneds<T>(var StackItem: TSortStackItem<T>): Pointer;
const
  Flags = $8000;
begin
  case SizeOf(T) of
    1: TArray.RadixSort<Byte>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    2: TArray.RadixSort<Word>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    4: TArray.RadixSort<Integer>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    8: TArray.RadixSort<Int64>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
  end;
  Result := @StackItem;
end;

class function TArray.RadixSortDescendingSigneds<T>(var StackItem: TSortStackItem<T>): Pointer;
const
  Flags = not $8000;
begin
  case SizeOf(T) of
    1: TArray.RadixSort<Byte>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    2: TArray.RadixSort<Word>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    4: TArray.RadixSort<Integer>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    8: TArray.RadixSort<Int64>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
  end;
  Result := @StackItem;
end;

class function TArray.RadixSortUnsigneds<T>(var StackItem: TSortStackItem<T>): Pointer;
const
  Flags = $0000;
begin
  case SizeOf(T) of
    1: TArray.RadixSort<Byte>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    2: TArray.RadixSort<Word>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    4: TArray.RadixSort<Integer>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    8: TArray.RadixSort<Int64>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
  end;
  Result := @StackItem;
end;

class function TArray.RadixSortDescendingUnsigneds<T>(var StackItem: TSortStackItem<T>): Pointer;
const
  Flags = not $0000;
begin
  case SizeOf(T) of
    1: TArray.RadixSort<Byte>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    2: TArray.RadixSort<Word>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    4: TArray.RadixSort<Integer>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
    8: TArray.RadixSort<Int64>(StackItem.First, SortItemCount<T>(StackItem.First, StackItem.Last), Flags);
  end;
  Result := @StackItem;
end;

class function TArray.RadixSortFloats<T>(var StackItem: TSortStackItem<T>): Pointer;
label
  sign_sorted;
var
  I, J: ^TFloat;
  Count: NativeInt;
begin
  I := Pointer(StackItem.First);
  J := Pointer(StackItem.Last);

  repeat
    Dec(NativeInt(I), SizeOf(T));
    repeat
      Inc(NativeInt(I), SizeOf(T));
      if (I > J) then goto sign_sorted;
      case SizeOf(T) of
        4: if (I.SSingle >= 0) then Break;
        8: if (I.SDouble >= 0) then Break;
       10: if (I.SExtended >= 0) then Break;
      end;
    until (False);

    Inc(NativeInt(J), SizeOf(T));
    repeat
      Dec(NativeInt(J), SizeOf(T));
      if (J < I) then goto sign_sorted;
      case SizeOf(T) of
        4: if (J.SSingle < 0) then Break;
        8: if (J.SDouble < 0) then Break;
       10: if (J.SExtended < 0) then Break;
      end;
    until (False);

    TArray.Exchange<T>(I, J);
    Inc(NativeInt(I), SizeOf(T));
    Dec(NativeInt(J), SizeOf(T));
  until (False);
sign_sorted:

  Count := SortItemCount<T>(StackItem.First, J);
  if (Count > 1) then
  begin
    if (Count <= ((SizeOf(T) div 4) * 125 - (SizeOf(T) div 10) * 1050)) then
    begin
      {$if CompilerVersion = 28}
        TArray.Sort<T>(StackItem.First^, Count);
      {$else}
        TArray.SortFloats<T>(StackItem.First, Count);
      {$ifend}
    end else
    case SizeOf(T) of
      4: TArray.RadixSort<Integer>(StackItem.First, Count, not $0000);
      8: TArray.RadixSort<Int64>(StackItem.First, Count, not $0000);
    else
      TArray.RadixSort<Extended>(StackItem.First, Count, not $0000);
    end;
  end;

  Count := SortItemCount<T>(I, StackItem.Last);
  if (Count > 1) then
  begin
    if (Count <= ((SizeOf(T) div 4) * 125 - (SizeOf(T) div 10) * 1050)) then
    begin
      {$if CompilerVersion = 28}
        TArray.Sort<T>(TRAIIHelper<T>.P(I)^, Count);
      {$else}
        TArray.SortFloats<T>(I, Count);
      {$ifend}
    end else
    case SizeOf(T) of
      4: TArray.RadixSort<Integer>(I, Count, $0000);
      8: TArray.RadixSort<Int64>(I, Count, $0000);
    else
      TArray.RadixSort<Extended>(I, Count, $0000);
    end;
  end;

  Result := @StackItem;
end;

class function TArray.RadixSortDescendingFloats<T>(var StackItem: TSortStackItem<T>): Pointer;
label
  sign_sorted;
var
  I, J: ^TFloat;
  Count: NativeInt;
begin
  I := Pointer(StackItem.First);
  J := Pointer(StackItem.Last);

  repeat
    Dec(NativeInt(I), SizeOf(T));
    repeat
      Inc(NativeInt(I), SizeOf(T));
      if (I > J) then goto sign_sorted;
      case SizeOf(T) of
        4: if (I.SSingle < 0) then Break;
        8: if (I.SDouble < 0) then Break;
       10: if (I.SExtended < 0) then Break;
      end;
    until (False);

    Inc(NativeInt(J), SizeOf(T));
    repeat
      Dec(NativeInt(J), SizeOf(T));
      if (J < I) then goto sign_sorted;
      case SizeOf(T) of
        4: if (J.SSingle >= 0) then Break;
        8: if (J.SDouble >= 0) then Break;
       10: if (J.SExtended >= 0) then Break;
      end;
    until (False);

    TArray.Exchange<T>(I, J);
    Inc(NativeInt(I), SizeOf(T));
    Dec(NativeInt(J), SizeOf(T));
  until (False);
sign_sorted:

  Count := SortItemCount<T>(StackItem.First, J);
  if (Count > 1) then
  begin
    if (Count <= ((SizeOf(T) div 4) * 125 - (SizeOf(T) div 10) * 1050)) then
    begin
      {$if CompilerVersion = 28}
        TArray.SortDescending<T>(StackItem.First^, Count);
      {$else}
        TArray.SortDescendingFloats<T>(StackItem.First, Count);
      {$ifend}
    end else
    case SizeOf(T) of
      4: TArray.RadixSort<Integer>(StackItem.First, Count, not $0000);
      8: TArray.RadixSort<Int64>(StackItem.First, Count, not $0000);
    else
      TArray.RadixSort<Extended>(StackItem.First, Count, not $0000);
    end;
  end;

  Count := SortItemCount<T>(I, StackItem.Last);
  if (Count > 1) then
  begin
    if (Count <= ((SizeOf(T) div 4) * 125 - (SizeOf(T) div 10) * 1050)) then
    begin
      {$if CompilerVersion = 28}
        TArray.SortDescending<T>(TRAIIHelper<T>.P(I)^, Count);
      {$else}
        TArray.SortDescendingFloats<T>(I, Count);
      {$ifend}
    end else
    case SizeOf(T) of
      4: TArray.RadixSort<Integer>(I, Count, $0000);
      8: TArray.RadixSort<Int64>(I, Count, $0000);
    else
      TArray.RadixSort<Extended>(I, Count, $0000);
    end;
  end;

  Result := @StackItem;
end;

class procedure TArray.SortSigneds<T>(const Values: Pointer; const Count: NativeInt);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Pivot4: Integer;
  {$ifdef LARGEINT}
    Pivot8: Int64;
  {$else .SMALLINT}
    Pivot8Low: Cardinal;
    Pivot8High, Buffer8High: Integer;
  {$endif}
  Temp: T;
  Temp4: Cardinal;
  Size: NativeInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion/radix sort
  Size := NativeInt(J) - NativeInt(I) + SizeOf(T);
  if (Size <= RADIX_BUFFER_SIZE * SizeOf(T)) and
    (
    (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) or
    (Size >= (SizeOf(T) * 45 + (SizeOf(T) div 4) * 30 - (SizeOf(T) div 8) * 220) * SizeOf(T))
    ) then
  begin
    if (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      goto insertion_init;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        case SizeOf(T) of
          1: if (Pivot4 <= PS1(J)^) then Continue;
          2: if (Pivot4 <= PS2(J)^) then Continue;
          4: if (Pivot4 <= PS4(J)^) then Continue;
        else
          {$ifdef LARGEINT}
            if (Pivot8 <= PS8(J)^) then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Pivot8High < Buffer8High) or
              ((Pivot8High = Buffer8High) and (Pivot8Low <= PCardinal(J)^)) then Continue;
          {$endif}
        end;

      insertion_init:
        I := J;
        case SizeOf(T) of
          1: Pivot4 := PS1(J)^;
          2: Pivot4 := PS2(J)^;
          4: Pivot4 := PS4(J)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PS8(J)^;
          {$else .SMALLINT}
            with PPoint(J)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
      until (False);
      I^ := J^;
      case SizeOf(T) of
        1: PS1(J)^ := Pivot4;
        2: PS2(J)^ := Pivot4;
        4: PS4(J)^ := Pivot4;
      else
        {$ifdef LARGEINT}
          PS8(J)^ := Pivot8;
        {$else .SMALLINT}
          with PPoint(J)^ do
          begin
            X := Pivot8Low;
            Y := Pivot8High;
          end;
        {$endif}
      end;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        case SizeOf(T) of
          1: Pivot4 := PS1(J + 1)^;
          2: Pivot4 := PS2(J + 1)^;
          4: Pivot4 := PS4(J + 1)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PS8(J + 1)^;
          {$else .SMALLINT}
            with PPoint(J + 1)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
        case SizeOf(T) of
          1: if (PS1(J)^ <= Pivot4) then Continue;
          2: if (PS2(J)^ <= Pivot4) then Continue;
          4: if (PS4(J)^ <= Pivot4) then Continue;
        else
          {$ifdef LARGEINT}
            if (PS8(J)^ <= Pivot8)  then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Buffer8High < Pivot8High) or
              ((Buffer8High = Pivot8High) and (PCardinal(J)^ <= Pivot8Low)) then Continue;
          {$endif}
        end;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          case SizeOf(T) of
            1: if (PS1(I)^ <= Pivot4) then Break;
            2: if (PS2(I)^ <= Pivot4) then Break;
            4: if (PS4(I)^ <= Pivot4) then Break;
          else
            {$ifdef LARGEINT}
              if (PS8(I)^ <= Pivot8)  then Break;
            {$else .SMALLINT}
              Buffer8High := PPoint(I).Y;
              if (Buffer8High < Pivot8High) or
                ((Buffer8High = Pivot8High) and (PCardinal(I)^ <= Pivot8Low)) then Break;
            {$endif}
          end;
        until (False);

        case SizeOf(T) of
          1: PS1(I + 1)^ := Pivot4;
          2: PS2(I + 1)^ := Pivot4;
          4: PS4(I + 1)^ := Pivot4;
        else
          {$ifdef LARGEINT}
            PS8(I + 1)^ := Pivot8;
          {$else .SMALLINT}
            with PPoint(I + 1)^ do
            begin
              X := Pivot8Low;
              Y := Pivot8High;
            end;
          {$endif}
        end;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end else
    begin
      StackItem := TArray.RadixSortSigneds<T>(StackItem^);
      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end;
  end;

  // pivot
  case SizeOf(T) of
    1: Pivot4 := PS1(SortItemPivot<T>(I, J))^;
    2: Pivot4 := PS2(SortItemPivot<T>(I, J))^;
    4: Pivot4 := PS4(SortItemPivot<T>(I, J))^;
  else
    {$ifdef LARGEINT}
      Pivot8 := PS8(SortItemPivot<T>(I, J))^;
    {$else .SMALLINT}
      with PPoint(I + ((NativeInt(J) - NativeInt(I)) shr 4))^ do
      begin
        Pivot8Low := X;
        Pivot8High := Y;
      end;
    {$endif}
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);
      case SizeOf(T) of
        1: if (Pivot4 <= PS1(I)^) then Break;
        2: if (Pivot4 <= PS2(I)^) then Break;
        4: if (Pivot4 <= PS4(I)^) then Break;
      else
        {$ifdef LARGEINT}
          if (Pivot8 <= PS8(I)^) then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(I).Y;
          if (Pivot8High < Buffer8High) or
            ((Pivot8High = Buffer8High) and (Pivot8Low <= PCardinal(I)^)) then Break;
        {$endif}
      end;
    until (False);

    repeat
      Dec(J);
      case SizeOf(T) of
        1: if (PS1(J)^ <= Pivot4) then Break;
        2: if (PS2(J)^ <= Pivot4) then Break;
        4: if (PS4(J)^ <= Pivot4) then Break;
      else
        {$ifdef LARGEINT}
          if (PS8(J)^ <= Pivot8)  then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(J).Y;
          if (Buffer8High < Pivot8High) or
            ((Buffer8High = Pivot8High) and (PCardinal(J)^ <= Pivot8Low)) then Break;
        {$endif}
      end;
    until (False);

    if (I <= J) then
    begin
      {$ifdef SMALLINT}
      if (SizeOf(T) = 8) then
      begin
        Temp4 := TLMemory(Pointer(I)^).LCardinals[0];
        TLMemory(Pointer(I)^).LCardinals[0] := TLMemory(Pointer(J)^).LCardinals[0];
        TLMemory(Pointer(J)^).LCardinals[0] := Temp4;
        Temp4 := TLMemory(Pointer(I)^).LCardinals[1];
        TLMemory(Pointer(I)^).LCardinals[1] := TLMemory(Pointer(J)^).LCardinals[1];
        TLMemory(Pointer(J)^).LCardinals[1] := Temp4;
      end else
      {$endif}
      begin
        Temp := I^;
        I^ := J^;
        J^ := Temp;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;

class procedure TArray.SortDescendingSigneds<T>(const Values: Pointer; const Count: NativeInt);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Pivot4: Integer;
  {$ifdef LARGEINT}
    Pivot8: Int64;
  {$else .SMALLINT}
    Pivot8Low: Cardinal;
    Pivot8High, Buffer8High: Integer;
  {$endif}
  Temp: T;
  Temp4: Cardinal;
  Size: NativeInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion/radix sort
  Size := NativeInt(J) - NativeInt(I) + SizeOf(T);
  if (Size <= RADIX_BUFFER_SIZE * SizeOf(T)) and
    (
    (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) or
    (Size >= (SizeOf(T) * 45 + (SizeOf(T) div 4) * 30 - (SizeOf(T) div 8) * 220) * SizeOf(T))
    ) then
  begin
    if (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      goto insertion_init;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        case SizeOf(T) of
          1: if (Pivot4 >= PS1(J)^) then Continue;
          2: if (Pivot4 >= PS2(J)^) then Continue;
          4: if (Pivot4 >= PS4(J)^) then Continue;
        else
          {$ifdef LARGEINT}
            if (Pivot8 >= PS8(J)^) then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Pivot8High > Buffer8High) or
              ((Pivot8High = Buffer8High) and (Pivot8Low >= PCardinal(J)^)) then Continue;
          {$endif}
        end;

      insertion_init:
        I := J;
        case SizeOf(T) of
          1: Pivot4 := PS1(J)^;
          2: Pivot4 := PS2(J)^;
          4: Pivot4 := PS4(J)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PS8(J)^;
          {$else .SMALLINT}
            with PPoint(J)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
      until (False);
      I^ := J^;
      case SizeOf(T) of
        1: PS1(J)^ := Pivot4;
        2: PS2(J)^ := Pivot4;
        4: PS4(J)^ := Pivot4;
      else
        {$ifdef LARGEINT}
          PS8(J)^ := Pivot8;
        {$else .SMALLINT}
          with PPoint(J)^ do
          begin
            X := Pivot8Low;
            Y := Pivot8High;
          end;
        {$endif}
      end;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        case SizeOf(T) of
          1: Pivot4 := PS1(J + 1)^;
          2: Pivot4 := PS2(J + 1)^;
          4: Pivot4 := PS4(J + 1)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PS8(J + 1)^;
          {$else .SMALLINT}
            with PPoint(J + 1)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
        case SizeOf(T) of
          1: if (PS1(J)^ >= Pivot4) then Continue;
          2: if (PS2(J)^ >= Pivot4) then Continue;
          4: if (PS4(J)^ >= Pivot4) then Continue;
        else
          {$ifdef LARGEINT}
            if (PS8(J)^ >= Pivot8)  then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Buffer8High > Pivot8High) or
              ((Buffer8High = Pivot8High) and (PCardinal(J)^ >= Pivot8Low)) then Continue;
          {$endif}
        end;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          case SizeOf(T) of
            1: if (PS1(I)^ >= Pivot4) then Break;
            2: if (PS2(I)^ >= Pivot4) then Break;
            4: if (PS4(I)^ >= Pivot4) then Break;
          else
            {$ifdef LARGEINT}
              if (PS8(I)^ >= Pivot8)  then Break;
            {$else .SMALLINT}
              Buffer8High := PPoint(I).Y;
              if (Buffer8High > Pivot8High) or
                ((Buffer8High = Pivot8High) and (PCardinal(I)^ >= Pivot8Low)) then Break;
            {$endif}
          end;
        until (False);

        case SizeOf(T) of
          1: PS1(I + 1)^ := Pivot4;
          2: PS2(I + 1)^ := Pivot4;
          4: PS4(I + 1)^ := Pivot4;
        else
          {$ifdef LARGEINT}
            PS8(I + 1)^ := Pivot8;
          {$else .SMALLINT}
            with PPoint(I + 1)^ do
            begin
              X := Pivot8Low;
              Y := Pivot8High;
            end;
          {$endif}
        end;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end else
    begin
      StackItem := TArray.RadixSortDescendingSigneds<T>(StackItem^);
      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end;
  end;

  // pivot
  case SizeOf(T) of
    1: Pivot4 := PS1(SortItemPivot<T>(I, J))^;
    2: Pivot4 := PS2(SortItemPivot<T>(I, J))^;
    4: Pivot4 := PS4(SortItemPivot<T>(I, J))^;
  else
    {$ifdef LARGEINT}
      Pivot8 := PS8(SortItemPivot<T>(I, J))^;
    {$else .SMALLINT}
      with PPoint(I + ((NativeInt(J) - NativeInt(I)) shr 4))^ do
      begin
        Pivot8Low := X;
        Pivot8High := Y;
      end;
    {$endif}
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);
      case SizeOf(T) of
        1: if (Pivot4 >= PS1(I)^) then Break;
        2: if (Pivot4 >= PS2(I)^) then Break;
        4: if (Pivot4 >= PS4(I)^) then Break;
      else
        {$ifdef LARGEINT}
          if (Pivot8 >= PS8(I)^) then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(I).Y;
          if (Pivot8High > Buffer8High) or
            ((Pivot8High = Buffer8High) and (Pivot8Low >= PCardinal(I)^)) then Break;
        {$endif}
      end;
    until (False);

    repeat
      Dec(J);
      case SizeOf(T) of
        1: if (PS1(J)^ >= Pivot4) then Break;
        2: if (PS2(J)^ >= Pivot4) then Break;
        4: if (PS4(J)^ >= Pivot4) then Break;
      else
        {$ifdef LARGEINT}
          if (PS8(J)^ >= Pivot8)  then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(J).Y;
          if (Buffer8High > Pivot8High) or
            ((Buffer8High = Pivot8High) and (PCardinal(J)^ >= Pivot8Low)) then Break;
        {$endif}
      end;
    until (False);

    if (I <= J) then
    begin
      {$ifdef SMALLINT}
      if (SizeOf(T) = 8) then
      begin
        Temp4 := TLMemory(Pointer(I)^).LCardinals[0];
        TLMemory(Pointer(I)^).LCardinals[0] := TLMemory(Pointer(J)^).LCardinals[0];
        TLMemory(Pointer(J)^).LCardinals[0] := Temp4;
        Temp4 := TLMemory(Pointer(I)^).LCardinals[1];
        TLMemory(Pointer(I)^).LCardinals[1] := TLMemory(Pointer(J)^).LCardinals[1];
        TLMemory(Pointer(J)^).LCardinals[1] := Temp4;
      end else
      {$endif}
      begin
        Temp := I^;
        I^ := J^;
        J^ := Temp;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;

class procedure TArray.SortUnsigneds<T>(const Values: Pointer; const Count: NativeInt);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Pivot4: Cardinal;
  {$ifdef LARGEINT}
    Pivot8: UInt64;
  {$else .SMALLINT}
    Pivot8Low: Cardinal;
    Pivot8High, Buffer8High: Cardinal;
  {$endif}
  Temp: T;
  Temp4: Cardinal;
  Size: NativeInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion/radix sort
  Size := NativeInt(J) - NativeInt(I) + SizeOf(T);
  if (Size <= RADIX_BUFFER_SIZE * SizeOf(T)) and
    (
    (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) or
    (Size >= (SizeOf(T) * 45 + (SizeOf(T) div 4) * 30 - (SizeOf(T) div 8) * 220) * SizeOf(T))
    ) then
  begin
    if (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      goto insertion_init;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        case SizeOf(T) of
          1: if (Pivot4 <= PU1(J)^) then Continue;
          2: if (Pivot4 <= PU2(J)^) then Continue;
          4: if (Pivot4 <= PU4(J)^) then Continue;
        else
          {$ifdef LARGEINT}
            if (Pivot8 <= PU8(J)^) then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Pivot8High < Buffer8High) or
              ((Pivot8High = Buffer8High) and (Pivot8Low <= PCardinal(J)^)) then Continue;
          {$endif}
        end;

      insertion_init:
        I := J;
        case SizeOf(T) of
          1: Pivot4 := PU1(J)^;
          2: Pivot4 := PU2(J)^;
          4: Pivot4 := PU4(J)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PU8(J)^;
          {$else .SMALLINT}
            with PPoint(J)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
      until (False);
      I^ := J^;
      case SizeOf(T) of
        1: PU1(J)^ := Pivot4;
        2: PU2(J)^ := Pivot4;
        4: PU4(J)^ := Pivot4;
      else
        {$ifdef LARGEINT}
          PU8(J)^ := Pivot8;
        {$else .SMALLINT}
          with PPoint(J)^ do
          begin
            X := Pivot8Low;
            Y := Pivot8High;
          end;
        {$endif}
      end;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        case SizeOf(T) of
          1: Pivot4 := PU1(J + 1)^;
          2: Pivot4 := PU2(J + 1)^;
          4: Pivot4 := PU4(J + 1)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PU8(J + 1)^;
          {$else .SMALLINT}
            with PPoint(J + 1)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
        case SizeOf(T) of
          1: if (PU1(J)^ <= Pivot4) then Continue;
          2: if (PU2(J)^ <= Pivot4) then Continue;
          4: if (PU4(J)^ <= Pivot4) then Continue;
        else
          {$ifdef LARGEINT}
            if (PU8(J)^ <= Pivot8)  then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Buffer8High < Pivot8High) or
              ((Buffer8High = Pivot8High) and (PCardinal(J)^ <= Pivot8Low)) then Continue;
          {$endif}
        end;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          case SizeOf(T) of
            1: if (PU1(I)^ <= Pivot4) then Break;
            2: if (PU2(I)^ <= Pivot4) then Break;
            4: if (PU4(I)^ <= Pivot4) then Break;
          else
            {$ifdef LARGEINT}
              if (PU8(I)^ <= Pivot8)  then Break;
            {$else .SMALLINT}
              Buffer8High := PPoint(I).Y;
              if (Buffer8High < Pivot8High) or
                ((Buffer8High = Pivot8High) and (PCardinal(I)^ <= Pivot8Low)) then Break;
            {$endif}
          end;
        until (False);

        case SizeOf(T) of
          1: PU1(I + 1)^ := Pivot4;
          2: PU2(I + 1)^ := Pivot4;
          4: PU4(I + 1)^ := Pivot4;
        else
          {$ifdef LARGEINT}
            PU8(I + 1)^ := Pivot8;
          {$else .SMALLINT}
            with PPoint(I + 1)^ do
            begin
              X := Pivot8Low;
              Y := Pivot8High;
            end;
          {$endif}
        end;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end else
    begin
      StackItem := TArray.RadixSortUnsigneds<T>(StackItem^);
      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end;
  end;

  // pivot
  case SizeOf(T) of
    1: Pivot4 := PU1(SortItemPivot<T>(I, J))^;
    2: Pivot4 := PU2(SortItemPivot<T>(I, J))^;
    4: Pivot4 := PU4(SortItemPivot<T>(I, J))^;
  else
    {$ifdef LARGEINT}
      Pivot8 := PU8(SortItemPivot<T>(I, J))^;
    {$else .SMALLINT}
      with PPoint(I + ((NativeInt(J) - NativeInt(I)) shr 4))^ do
      begin
        Pivot8Low := X;
        Pivot8High := Y;
      end;
    {$endif}
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);
      case SizeOf(T) of
        1: if (Pivot4 <= PU1(I)^) then Break;
        2: if (Pivot4 <= PU2(I)^) then Break;
        4: if (Pivot4 <= PU4(I)^) then Break;
      else
        {$ifdef LARGEINT}
          if (Pivot8 <= PU8(I)^) then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(I).Y;
          if (Pivot8High < Buffer8High) or
            ((Pivot8High = Buffer8High) and (Pivot8Low <= PCardinal(I)^)) then Break;
        {$endif}
      end;
    until (False);

    repeat
      Dec(J);
      case SizeOf(T) of
        1: if (PU1(J)^ <= Pivot4) then Break;
        2: if (PU2(J)^ <= Pivot4) then Break;
        4: if (PU4(J)^ <= Pivot4) then Break;
      else
        {$ifdef LARGEINT}
          if (PU8(J)^ <= Pivot8)  then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(J).Y;
          if (Buffer8High < Pivot8High) or
            ((Buffer8High = Pivot8High) and (PCardinal(J)^ <= Pivot8Low)) then Break;
        {$endif}
      end;
    until (False);

    if (I <= J) then
    begin
      {$ifdef SMALLINT}
      if (SizeOf(T) = 8) then
      begin
        Temp4 := TLMemory(Pointer(I)^).LCardinals[0];
        TLMemory(Pointer(I)^).LCardinals[0] := TLMemory(Pointer(J)^).LCardinals[0];
        TLMemory(Pointer(J)^).LCardinals[0] := Temp4;
        Temp4 := TLMemory(Pointer(I)^).LCardinals[1];
        TLMemory(Pointer(I)^).LCardinals[1] := TLMemory(Pointer(J)^).LCardinals[1];
        TLMemory(Pointer(J)^).LCardinals[1] := Temp4;
      end else
      {$endif}
      begin
        Temp := I^;
        I^ := J^;
        J^ := Temp;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;

class procedure TArray.SortDescendingUnsigneds<T>(const Values: Pointer; const Count: NativeInt);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Pivot4: Cardinal;
  {$ifdef LARGEINT}
    Pivot8: UInt64;
  {$else .SMALLINT}
    Pivot8Low: Cardinal;
    Pivot8High, Buffer8High: Cardinal;
  {$endif}
  Temp: T;
  Temp4: Cardinal;
  Size: NativeInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion/radix sort
  Size := NativeInt(J) - NativeInt(I) + SizeOf(T);
  if (Size <= RADIX_BUFFER_SIZE * SizeOf(T)) and
    (
    (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) or
    (Size >= (SizeOf(T) * 45 + (SizeOf(T) div 4) * 30 - (SizeOf(T) div 8) * 220) * SizeOf(T))
    ) then
  begin
    if (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      goto insertion_init;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        case SizeOf(T) of
          1: if (Pivot4 >= PU1(J)^) then Continue;
          2: if (Pivot4 >= PU2(J)^) then Continue;
          4: if (Pivot4 >= PU4(J)^) then Continue;
        else
          {$ifdef LARGEINT}
            if (Pivot8 >= PU8(J)^) then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Pivot8High > Buffer8High) or
              ((Pivot8High = Buffer8High) and (Pivot8Low >= PCardinal(J)^)) then Continue;
          {$endif}
        end;

      insertion_init:
        I := J;
        case SizeOf(T) of
          1: Pivot4 := PU1(J)^;
          2: Pivot4 := PU2(J)^;
          4: Pivot4 := PU4(J)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PU8(J)^;
          {$else .SMALLINT}
            with PPoint(J)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
      until (False);
      I^ := J^;
      case SizeOf(T) of
        1: PU1(J)^ := Pivot4;
        2: PU2(J)^ := Pivot4;
        4: PU4(J)^ := Pivot4;
      else
        {$ifdef LARGEINT}
          PU8(J)^ := Pivot8;
        {$else .SMALLINT}
          with PPoint(J)^ do
          begin
            X := Pivot8Low;
            Y := Pivot8High;
          end;
        {$endif}
      end;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        case SizeOf(T) of
          1: Pivot4 := PU1(J + 1)^;
          2: Pivot4 := PU2(J + 1)^;
          4: Pivot4 := PU4(J + 1)^;
        else
          {$ifdef LARGEINT}
            Pivot8 := PU8(J + 1)^;
          {$else .SMALLINT}
            with PPoint(J + 1)^ do
            begin
              Pivot8Low := X;
              Pivot8High := Y;
            end;
          {$endif}
        end;
        case SizeOf(T) of
          1: if (PU1(J)^ >= Pivot4) then Continue;
          2: if (PU2(J)^ >= Pivot4) then Continue;
          4: if (PU4(J)^ >= Pivot4) then Continue;
        else
          {$ifdef LARGEINT}
            if (PU8(J)^ >= Pivot8)  then Continue;
          {$else .SMALLINT}
            Buffer8High := PPoint(J).Y;
            if (Buffer8High > Pivot8High) or
              ((Buffer8High = Pivot8High) and (PCardinal(J)^ >= Pivot8Low)) then Continue;
          {$endif}
        end;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          case SizeOf(T) of
            1: if (PU1(I)^ >= Pivot4) then Break;
            2: if (PU2(I)^ >= Pivot4) then Break;
            4: if (PU4(I)^ >= Pivot4) then Break;
          else
            {$ifdef LARGEINT}
              if (PU8(I)^ >= Pivot8)  then Break;
            {$else .SMALLINT}
              Buffer8High := PPoint(I).Y;
              if (Buffer8High > Pivot8High) or
                ((Buffer8High = Pivot8High) and (PCardinal(I)^ >= Pivot8Low)) then Break;
            {$endif}
          end;
        until (False);

        case SizeOf(T) of
          1: PU1(I + 1)^ := Pivot4;
          2: PU2(I + 1)^ := Pivot4;
          4: PU4(I + 1)^ := Pivot4;
        else
          {$ifdef LARGEINT}
            PU8(I + 1)^ := Pivot8;
          {$else .SMALLINT}
            with PPoint(I + 1)^ do
            begin
              X := Pivot8Low;
              Y := Pivot8High;
            end;
          {$endif}
        end;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end else
    begin
      StackItem := TArray.RadixSortDescendingUnsigneds<T>(StackItem^);
      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end;
  end;

  // pivot
  case SizeOf(T) of
    1: Pivot4 := PU1(SortItemPivot<T>(I, J))^;
    2: Pivot4 := PU2(SortItemPivot<T>(I, J))^;
    4: Pivot4 := PU4(SortItemPivot<T>(I, J))^;
  else
    {$ifdef LARGEINT}
      Pivot8 := PU8(SortItemPivot<T>(I, J))^;
    {$else .SMALLINT}
      with PPoint(I + ((NativeInt(J) - NativeInt(I)) shr 4))^ do
      begin
        Pivot8Low := X;
        Pivot8High := Y;
      end;
    {$endif}
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);
      case SizeOf(T) of
        1: if (Pivot4 >= PU1(I)^) then Break;
        2: if (Pivot4 >= PU2(I)^) then Break;
        4: if (Pivot4 >= PU4(I)^) then Break;
      else
        {$ifdef LARGEINT}
          if (Pivot8 >= PU8(I)^) then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(I).Y;
          if (Pivot8High > Buffer8High) or
            ((Pivot8High = Buffer8High) and (Pivot8Low >= PCardinal(I)^)) then Break;
        {$endif}
      end;
    until (False);

    repeat
      Dec(J);
      case SizeOf(T) of
        1: if (PU1(J)^ >= Pivot4) then Break;
        2: if (PU2(J)^ >= Pivot4) then Break;
        4: if (PU4(J)^ >= Pivot4) then Break;
      else
        {$ifdef LARGEINT}
          if (PU8(J)^ >= Pivot8)  then Break;
        {$else .SMALLINT}
          Buffer8High := PPoint(J).Y;
          if (Buffer8High > Pivot8High) or
            ((Buffer8High = Pivot8High) and (PCardinal(J)^ >= Pivot8Low)) then Break;
        {$endif}
      end;
    until (False);

    if (I <= J) then
    begin
      {$ifdef SMALLINT}
      if (SizeOf(T) = 8) then
      begin
        Temp4 := TLMemory(Pointer(I)^).LCardinals[0];
        TLMemory(Pointer(I)^).LCardinals[0] := TLMemory(Pointer(J)^).LCardinals[0];
        TLMemory(Pointer(J)^).LCardinals[0] := Temp4;
        Temp4 := TLMemory(Pointer(I)^).LCardinals[1];
        TLMemory(Pointer(I)^).LCardinals[1] := TLMemory(Pointer(J)^).LCardinals[1];
        TLMemory(Pointer(J)^).LCardinals[1] := Temp4;
      end else
      {$endif}
      begin
        Temp := I^;
        I^ := J^;
        J^ := Temp;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;

class procedure TArray.SortFloats<T>(const Values: Pointer; const Count: NativeInt);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Pivot4: Single;
  Pivot8: Double;
  PivotE: Extended;
  TempNative: NativeUInt;
  Size: NativeInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion/radix sort
  Size := NativeInt(J) - NativeInt(I) + SizeOf(T);
  if (Size <= RADIX_BUFFER_SIZE * SizeOf(T)) and
    (
    (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) or
    (Size >= ((SizeOf(T) div 4) * 125 - (SizeOf(T) div 10) * 50) * SizeOf(T))
    ) then
  begin
    if (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      goto insertion_init;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        case SizeOf(T) of
          4: if (Pivot4 <= PF4(J)^) then Continue;
          8: if (Pivot8 <= PF8(J)^) then Continue;
        else
          if (PivotE <= PFE(J)^) then Continue;
        end;

      insertion_init:
        I := J;
        case SizeOf(T) of
          4: Pivot4 := PF4(J)^;
          8: Pivot8 := PF8(J)^;
        else
          PivotE := PFE(J)^;
        end;
      until (False);
      I^ := J^;
      case SizeOf(T) of
        4: PF4(J)^ := Pivot4;
        8: PF8(J)^ := Pivot8;
      else
        PFE(J)^ := PivotE;
      end;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        case SizeOf(T) of
          4: Pivot4 := PF4(J + 1)^;
          8: Pivot8 := PF8(J + 1)^;
        else
          PivotE := PFE(J + 1)^;
        end;
        case SizeOf(T) of
          4: if (PF4(J)^ <= Pivot4) then Continue;
          8: if (PF8(J)^ <= Pivot8) then Continue;
        else
          if (PFE(J)^ <= PivotE)  then Continue;
        end;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          case SizeOf(T) of
            4: if (PF4(I)^ <= Pivot4) then Break;
            8: if (PF8(I)^ <= Pivot8) then Break;
          else
            if (PFE(I)^ <= PivotE)  then Break;
          end;
        until (False);

        case SizeOf(T) of
          4: PF4(I + 1)^ := Pivot4;
          8: PF8(I + 1)^ := Pivot8;
        else
          PFE(I + 1)^ := PivotE;
        end;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end else
    begin
      StackItem := TArray.RadixSortFloats<T>(StackItem^);
      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end;
  end;

  // pivot
  case SizeOf(T) of
    4: Pivot4 := PF4(SortItemPivot<T>(I, J))^;
    8: Pivot8 := PF8(SortItemPivot<T>(I, J))^;
  else
    PivotE := PFE(SortItemPivot<T>(I, J))^;
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);
      case SizeOf(T) of
        4: if (Pivot4 <= PF4(I)^) then Break;
        8: if (Pivot8 <= PF8(I)^) then Break;
      else
        if (PivotE <= PFE(I)^) then Break;
      end;
    until (False);

    repeat
      Dec(J);
      case SizeOf(T) of
        4: if (PF4(J)^ <= Pivot4) then Break;
        8: if (PF8(J)^ <= Pivot8) then Break;
      else
        if (PFE(J)^ <= PivotE)  then Break;
      end;
    until (False);

    if (I <= J) then
    begin
      if (SizeOf(T) = 4) then
      begin
        TempNative := TLMemory(Pointer(I)^).LCardinals[0];
        TLMemory(Pointer(I)^).LCardinals[0] := TLMemory(Pointer(J)^).LCardinals[0];
        TLMemory(Pointer(J)^).LCardinals[0] := TempNative;
      end else
      begin
        TempNative := TLMemory(Pointer(I)^).LNatives[0];
        TLMemory(Pointer(I)^).LNatives[0] := TLMemory(Pointer(J)^).LNatives[0];
        TLMemory(Pointer(J)^).LNatives[0] := TempNative;

        if (SizeOf(T) >= 2 * SizeOf(NativeUInt)) then
        begin
          TempNative := TLMemory(Pointer(I)^).LNatives[1];
          TLMemory(Pointer(I)^).LNatives[1] := TLMemory(Pointer(J)^).LNatives[1];
          TLMemory(Pointer(J)^).LNatives[1] := TempNative;
        end;

        if (SizeOf(T) = 10) then
        begin
          TempNative := TLMemory(Pointer(I)^).LWords[4];
          TLMemory(Pointer(I)^).LWords[4] := TLMemory(Pointer(J)^).LWords[4];
          TLMemory(Pointer(J)^).LWords[4] := TempNative;
        end;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;

class procedure TArray.SortDescendingFloats<T>(const Values: Pointer; const Count: NativeInt);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Pivot4: Single;
  Pivot8: Double;
  PivotE: Extended;
  TempNative: NativeUInt;
  Size: NativeInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion/radix sort
  Size := NativeInt(J) - NativeInt(I) + SizeOf(T);
  if (Size <= RADIX_BUFFER_SIZE * SizeOf(T)) and
    (
    (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) or
    (Size >= ((SizeOf(T) div 4) * 125 - (SizeOf(T) div 10) * 50) * SizeOf(T))
    ) then
  begin
    if (Size <= INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      goto insertion_init;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        case SizeOf(T) of
          4: if (Pivot4 >= PF4(J)^) then Continue;
          8: if (Pivot8 >= PF8(J)^) then Continue;
        else
          if (PivotE >= PFE(J)^) then Continue;
        end;

      insertion_init:
        I := J;
        case SizeOf(T) of
          4: Pivot4 := PF4(J)^;
          8: Pivot8 := PF8(J)^;
        else
          PivotE := PFE(J)^;
        end;
      until (False);
      I^ := J^;
      case SizeOf(T) of
        4: PF4(J)^ := Pivot4;
        8: PF8(J)^ := Pivot8;
      else
        PFE(J)^ := PivotE;
      end;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        case SizeOf(T) of
          4: Pivot4 := PF4(J + 1)^;
          8: Pivot8 := PF8(J + 1)^;
        else
          PivotE := PFE(J + 1)^;
        end;
        case SizeOf(T) of
          4: if (PF4(J)^ >= Pivot4) then Continue;
          8: if (PF8(J)^ >= Pivot8) then Continue;
        else
          if (PFE(J)^ >= PivotE)  then Continue;
        end;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          case SizeOf(T) of
            4: if (PF4(I)^ >= Pivot4) then Break;
            8: if (PF8(I)^ >= Pivot8) then Break;
          else
            if (PFE(I)^ >= PivotE)  then Break;
          end;
        until (False);

        case SizeOf(T) of
          4: PF4(I + 1)^ := Pivot4;
          8: PF8(I + 1)^ := Pivot8;
        else
          PFE(I + 1)^ := PivotE;
        end;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end else
    begin
      StackItem := TArray.RadixSortDescendingFloats<T>(StackItem^);
      if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
      Exit;
    end;
  end;

  // pivot
  case SizeOf(T) of
    4: Pivot4 := PF4(SortItemPivot<T>(I, J))^;
    8: Pivot8 := PF8(SortItemPivot<T>(I, J))^;
  else
    PivotE := PFE(SortItemPivot<T>(I, J))^;
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);
      case SizeOf(T) of
        4: if (Pivot4 >= PF4(I)^) then Break;
        8: if (Pivot8 >= PF8(I)^) then Break;
      else
        if (PivotE >= PFE(I)^) then Break;
      end;
    until (False);

    repeat
      Dec(J);
      case SizeOf(T) of
        4: if (PF4(J)^ >= Pivot4) then Break;
        8: if (PF8(J)^ >= Pivot8) then Break;
      else
        if (PFE(J)^ >= PivotE)  then Break;
      end;
    until (False);

    if (I <= J) then
    begin
      if (SizeOf(T) = 4) then
      begin
        TempNative := TLMemory(Pointer(I)^).LCardinals[0];
        TLMemory(Pointer(I)^).LCardinals[0] := TLMemory(Pointer(J)^).LCardinals[0];
        TLMemory(Pointer(J)^).LCardinals[0] := TempNative;
      end else
      begin
        TempNative := TLMemory(Pointer(I)^).LNatives[0];
        TLMemory(Pointer(I)^).LNatives[0] := TLMemory(Pointer(J)^).LNatives[0];
        TLMemory(Pointer(J)^).LNatives[0] := TempNative;

        if (SizeOf(T) >= 2 * SizeOf(NativeUInt)) then
        begin
          TempNative := TLMemory(Pointer(I)^).LNatives[1];
          TLMemory(Pointer(I)^).LNatives[1] := TLMemory(Pointer(J)^).LNatives[1];
          TLMemory(Pointer(J)^).LNatives[1] := TempNative;
        end;

        if (SizeOf(T) = 10) then
        begin
          TempNative := TLMemory(Pointer(I)^).LWords[4];
          TLMemory(Pointer(I)^).LWords[4] := TLMemory(Pointer(J)^).LWords[4];
          TLMemory(Pointer(J)^).LWords[4] := TempNative;
        end;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;

class procedure TArray.SortBinaries<T>(const Values: Pointer; const Count: NativeInt; var PivotBig: T);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Index: NativeInt;
  Temp1: Byte;
  Temp2: Word;
  Temp4: Cardinal;
  TempNative: NativeUInt;

  I, J, PStop: ^T;
  Pivot: TSortPivot;
  X, Y: NativeUInt;
  Buffer: Pointer;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion sort
  if (SizeOf(T) <= SizeOf(Pivot)) then
  if ((NativeUInt(J) - NativeUInt(I)) < INSERTION_SORT_LIMIT * SizeOf(T)) then
  begin
    PStop := StackItem^.First;
    goto insertion_init;
    repeat
      if (J = PStop) then Break;
      Dec(J);

      // compare pivot/J
      Y := TArray.SortBinaryMarker<T>(J);
      if (X < Y) then Continue;
      if (X = Y) then
      begin
        if (GetTypeKind(T) = tkMethod) then
        begin
          if (NativeUInt(Pivot.Ptr) <= NativeUInt(Pointer(J)^)) then Continue;
        end else
        {$if CompilerVersion = 28}
          if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
            TRAIIHelper<T>.P(@Pivot)^, J^) <= 0) then Continue;
        {$else}
          if (GetTypeKind(T) = tkString) then
          begin
            if (InterfaceDefaults.Compare_OStr(nil, Pointer(@Pivot), Pointer(J)) <= 0) then Continue;
          end else
          if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
          begin
            Buffer := Pointer(Pointer(J)^);
            if (Pivot.Ptr = Buffer) then Continue;
            case GetTypeKind(T) of
              tkLString: if (InterfaceDefaults.Compare_LStr(nil, Pivot.Ptr, Buffer) <= 0) then Continue;
              tkWString: if (InterfaceDefaults.Compare_WStr(nil, Pivot.Ptr, Buffer) <= 0) then Continue;
              tkUString: if (InterfaceDefaults.Compare_UStr(nil, Pivot.Ptr, Buffer) <= 0) then Continue;
             tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Pivot.Ptr, Buffer) <= 0) then Continue;
            end;
          end else
          case SizeOf(T) of
            0..SizeOf(Cardinal): Continue;
            {$ifdef LARGEINT}
            SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(@Pivot)^, PInt64(J)^) <= 0) then Continue;
            {$endif}
          else
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(@Pivot), Pointer(J)) <= 0) then Continue;
          end;
        {$ifend}
      end;
    insertion_init:
      I := J;
      // pivot := J^
      if (SizeOf(T) = SizeOf(Pointer)) then
      begin
        Pivot.Ptr := Pointer(Pointer(J)^);
      end else
      begin
        TArray.Copy<T>(@Pivot, J);
      end;
      X := TArray.SortBinaryMarker<T>(@Pivot);
    until (False);
    // I^ := J^
    if (SizeOf(T) = SizeOf(Pointer)) then
    begin
      PNativeUInt(I)^ := PNativeUInt(J)^;
    end else
    begin
      TArray.Copy<T>(I, J);
    end;
    // J := pivot
    if (SizeOf(T) = SizeOf(Pointer)) then
    begin
      Pointer(Pointer(J)^) := Pivot.Ptr;
    end else
    begin
      TArray.Copy<T>(J, @Pivot);
    end;

    PStop := StackItem^.Last;
    repeat
      Inc(J);
      if (J = PStop) then Break;

      // pivot := J[1]
      if (SizeOf(T) = SizeOf(Pointer)) then
      begin
        Pivot.Ptr := Pointer(Pointer(J + 1)^);
      end else
      begin
        TArray.Copy<T>(@Pivot, J + 1);
      end;
      X := TArray.SortBinaryMarker<T>(@Pivot);

      // compare J/pivot
      Y := TArray.SortBinaryMarker<T>(J);
      if (Y < X) then Continue;
      if (Y = X) then
      begin
        if (GetTypeKind(T) = tkMethod) then
        begin
          if (NativeUInt(Pointer(J)^) <= NativeUInt(Pivot.Ptr)) then Continue;
        end else
        {$if CompilerVersion = 28}
          if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
            J^, TRAIIHelper<T>.P(@Pivot)^) <= 0) then Continue;
        {$else}
          if (GetTypeKind(T) = tkString) then
          begin
            if (InterfaceDefaults.Compare_OStr(nil, Pointer(J), Pointer(@Pivot)) <= 0) then Continue;
          end else
          if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
          begin
            Buffer := Pointer(Pointer(J)^);
            if (Buffer = Pivot.Ptr) then Continue;
            case GetTypeKind(T) of
              tkLString: if (InterfaceDefaults.Compare_LStr(nil, Buffer, Pivot.Ptr) <= 0) then Continue;
              tkWString: if (InterfaceDefaults.Compare_WStr(nil, Buffer, Pivot.Ptr) <= 0) then Continue;
              tkUString: if (InterfaceDefaults.Compare_UStr(nil, Buffer, Pivot.Ptr) <= 0) then Continue;
             tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Buffer, Pivot.Ptr) <= 0) then Continue;
            end;
          end else
          case SizeOf(T) of
            0..SizeOf(Cardinal): Continue;
            {$ifdef LARGEINT}
            SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(J)^, PInt64(@Pivot)^) <= 0) then Continue;
            {$endif}
          else
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(J), Pointer(@Pivot)) <= 0) then Continue;
          end;
        {$ifend}
      end;

      I := J;
      repeat
        // I[1] := I^;
        if (SizeOf(T) = SizeOf(Pointer)) then
        begin
          PNativeUInt(I + 1)^ := PNativeUInt(I)^;
        end else
        begin
          TArray.Copy<T>(I + 1, I);
        end;
        Dec(I);

        // compare I/pivot
        Y := TArray.SortBinaryMarker<T>(I);
        if (Y < X) then Break;
        if (Y = X) then
        begin
          if (GetTypeKind(T) = tkMethod) then
          begin
            if (NativeUInt(Pointer(I)^) <= NativeUInt(Pivot.Ptr)) then Break;
          end else
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              I^, TRAIIHelper<T>.P(@Pivot)^) <= 0) then Break;
          {$else}
            if (GetTypeKind(T) = tkString) then
            begin
              if (InterfaceDefaults.Compare_OStr(nil, Pointer(I), Pointer(@Pivot)) <= 0) then Break;
            end else
            if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
            begin
              Buffer := Pointer(Pointer(I)^);
              if (Buffer = Pivot.Ptr) then Break;
              case GetTypeKind(T) of
                tkLString: if (InterfaceDefaults.Compare_LStr(nil, Buffer, Pivot.Ptr) <= 0) then Break;
                tkWString: if (InterfaceDefaults.Compare_WStr(nil, Buffer, Pivot.Ptr) <= 0) then Break;
                tkUString: if (InterfaceDefaults.Compare_UStr(nil, Buffer, Pivot.Ptr) <= 0) then Break;
               tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Buffer, Pivot.Ptr) <= 0) then Break;
              end;
            end else
            case SizeOf(T) of
              0..SizeOf(Cardinal): Break;
              {$ifdef LARGEINT}
              SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(I)^, PInt64(@Pivot)^) <= 0) then Break;
              {$endif}
            else
              if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
                Pointer(I), Pointer(@Pivot)) <= 0) then Break;
            end;
          {$ifend}
        end;
      until (False);

      // I[1] := pivot
      if (SizeOf(T) = SizeOf(Pointer)) then
      begin
        Pointer(Pointer(I + 1)^) := Pivot.Ptr;
      end else
      begin
        TArray.Copy<T>(I + 1, @Pivot);
      end;
    until (False);

    if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
    Exit;
  end;

  // pivot
  if (SizeOf(T) <= SizeOf(Pivot)) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) then
    begin
      Pivot.Ptr := Pointer(SortItemPivot<T>(I, J)^);
    end else
    begin
      TArray.Copy<T>(@Pivot, SortItemPivot<T>(I, J));
    end;
    X := TArray.SortBinaryMarker<T>(@Pivot);
  end else
  begin
    TArray.Copy<T>(@PivotBig, SortItemPivot<T>(I, J));
    X := TArray.SortBinaryMarker<T>(@PivotBig);
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);

      Y := TArray.SortBinaryMarker<T>(I);
      if (X < Y) then Break;
      if (X = Y) then
      begin
        if (SizeOf(T) <= SizeOf(Pivot)) then
        begin
          // compare pivot/I
          if (GetTypeKind(T) = tkMethod) then
          begin
            if (NativeUInt(Pivot.Ptr) <= NativeUInt(Pointer(I)^)) then Break;
          end else
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              TRAIIHelper<T>.P(@Pivot)^, I^) <= 0) then Break;
          {$else}
            if (GetTypeKind(T) = tkString) then
            begin
              if (InterfaceDefaults.Compare_OStr(nil, Pointer(@Pivot), Pointer(I)) <= 0) then Break;
            end else
            if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
            begin
              Buffer := Pointer(Pointer(I)^);
              if (Pivot.Ptr = Buffer) then Break;
              case GetTypeKind(T) of
                tkLString: if (InterfaceDefaults.Compare_LStr(nil, Pivot.Ptr, Buffer) <= 0) then Break;
                tkWString: if (InterfaceDefaults.Compare_WStr(nil, Pivot.Ptr, Buffer) <= 0) then Break;
                tkUString: if (InterfaceDefaults.Compare_UStr(nil, Pivot.Ptr, Buffer) <= 0) then Break;
               tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Pivot.Ptr, Buffer) <= 0) then Break;
              end;
            end else
            case SizeOf(T) of
              0..SizeOf(Cardinal): Break;
              {$ifdef LARGEINT}
              SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(@Pivot)^, PInt64(I)^) <= 0) then Break;
              {$endif}
            else
              if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
                Pointer(@Pivot), Pointer(I)) <= 0) then Break;
            end;
          {$ifend}
        end else
        begin
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              TRAIIHelper<T>.P(@PivotBig)^, I^) <= 0) then Break;
          {$else}
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(@PivotBig), Pointer(I)) <= 0) then Break;
          {$ifend}
        end;
      end;
    until (False);

    repeat
      Dec(J);

      Y := TArray.SortBinaryMarker<T>(J);
      if (Y < X) then Break;
      if (Y = X) then
      begin
        if (SizeOf(T) <= SizeOf(Pivot)) then
        begin
          // compare J/pivot
          if (GetTypeKind(T) = tkMethod) then
          begin
            if (NativeUInt(Pointer(J)^) <= NativeUInt(Pivot.Ptr)) then Break;
          end else
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              J^, TRAIIHelper<T>.P(@Pivot)^) <= 0) then Break;
          {$else}
            if (GetTypeKind(T) = tkString) then
            begin
              if (InterfaceDefaults.Compare_OStr(nil, Pointer(J), Pointer(@Pivot)) <= 0) then Break;
            end else
            if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
            begin
              Buffer := Pointer(Pointer(J)^);
              if (Buffer = Pivot.Ptr) then Break;
              case GetTypeKind(T) of
                tkLString: if (InterfaceDefaults.Compare_LStr(nil, Buffer, Pivot.Ptr) <= 0) then Break;
                tkWString: if (InterfaceDefaults.Compare_WStr(nil, Buffer, Pivot.Ptr) <= 0) then Break;
                tkUString: if (InterfaceDefaults.Compare_UStr(nil, Buffer, Pivot.Ptr) <= 0) then Break;
               tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Buffer, Pivot.Ptr) <= 0) then Break;
              end;
            end else
            case SizeOf(T) of
              0..SizeOf(Cardinal): Break;
              {$ifdef LARGEINT}
              SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(J)^, PInt64(@Pivot)^) <= 0) then Break;
              {$endif}
            else
              if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
                Pointer(J), Pointer(@Pivot)) <= 0) then Break;
            end;
          {$ifend}
        end else
        begin
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              J^, TRAIIHelper<T>.P(@PivotBig)^) <= 0) then Break;
          {$else}
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(J), Pointer(@PivotBig)) <= 0) then Break;
          {$ifend}
        end;
      end;
    until (False);

    if (I <= J) then
    begin
      // TArray.Exchange<T>(I, J);
      case SizeOf(T) of
        0: ;
        1:
        begin
          Temp1 := PLMemory(I).LBytes[0];
          PLMemory(I).LBytes[0] := PRMemory(J).RBytes[0];
          PRMemory(J).RBytes[0] := Temp1;
        end;
        2:
        begin
          Temp2 := PLMemory(I).LWords[0];
          PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
          PRMemory(J).RWords[0] := Temp2;
        end;
        3:
        begin
          Temp2 := PLMemory(I).LWords[0];
          PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
          PRMemory(J).RWords[0] := Temp2;

          Temp1 := PLMemory(I).LBytes[2];
          PLMemory(I).LBytes[2] := PRMemory(J).RBytes[2];
          PRMemory(J).RBytes[2] := Temp1;
        end;
        4..7:
        begin
          Temp4 := PLMemory(I).LCardinals[0];
          PLMemory(I).LCardinals[0] := PRMemory(J).RCardinals[0];
          PRMemory(J).RCardinals[0] := Temp4;

          case SizeOf(T) of
            5:
            begin
              Temp1 := PLMemory(I).LBytes[4];
              PLMemory(I).LBytes[4] := PRMemory(J).RBytes[4];
              PRMemory(J).RBytes[4] := Temp1;
            end;
            6:
            begin
              Temp2 := PLMemory(I).LWords[2];
              PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
              PRMemory(J).RWords[2] := Temp2;
            end;
            7:
            begin
              Temp2 := PLMemory(I).LWords[2];
              PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
              PRMemory(J).RWords[2] := Temp2;
              Temp1 := PLMemory(I).LBytes[6];
              PLMemory(I).LBytes[6] := PRMemory(J).RBytes[6];
              PRMemory(J).RBytes[6] := Temp1;
            end;
          end;
        end;
        8..16:
        begin
          TempNative := PLMemory(I).LNatives[0];
          PLMemory(I).LNatives[0] := PRMemory(J).RNatives[0];
          PRMemory(J).RNatives[0] := TempNative;

          if (SizeOf(T) >= SizeOf(NativeUInt) * 2) then
          begin
            TempNative := PLMemory(I).LNatives[1];
            PLMemory(I).LNatives[1] := PRMemory(J).RNatives[1];
            PRMemory(J).RNatives[1] := TempNative;
          end;

          if (SizeOf(T) >= SizeOf(NativeUInt) * 3) then
          begin
            TempNative := PLMemory(I).LNatives[2];
            PLMemory(I).LNatives[2] := PRMemory(J).RNatives[2];
            PRMemory(J).RNatives[2] := TempNative;
          end;

          if (SizeOf(T)  = SizeOf(NativeUInt) * 4) then
          begin
            TempNative := PLMemory(I).LNatives[3];
            PLMemory(I).LNatives[3] := PRMemory(J).RNatives[3];
            PRMemory(J).RNatives[3] := TempNative;
          end;

          {$ifdef LARGEINT}
          case SizeOf(T) of
            12, 13, 14, 15:
            begin
              Temp4 := PLMemory(I).LCardinals[2];
              PLMemory(I).LCardinals[2] := PRMemory(J).RCardinals[2];
              PRMemory(J).RCardinals[2] := Temp4;
            end;
          end;
          {$endif}

          case SizeOf(T) of
            9:
            begin
              Temp1 := PLMemory(I).LBytes[8];
              PLMemory(I).LBytes[8] := PRMemory(J).RBytes[8];
              PRMemory(J).RBytes[8] := Temp1;
            end;
            10:
            begin
              Temp2 := PLMemory(I).LWords[4];
              PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
              PRMemory(J).RWords[4] := Temp2;
            end;
            11:
            begin
              Temp2 := PLMemory(I).LWords[4];
              PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
              PRMemory(J).RWords[4] := Temp2;
              Temp1 := PLMemory(I).LBytes[10];
              PLMemory(I).LBytes[10] := PRMemory(J).RBytes[10];
              PRMemory(J).RBytes[10] := Temp1;
            end;
            13:
            begin
              Temp2 := PLMemory(I).LWords[5];
              PLMemory(I).LWords[5] := PRMemory(J).RWords[5];
              PRMemory(J).RWords[5] := Temp2;
              Temp1 := PLMemory(I).LBytes[12];
              PLMemory(I).LBytes[12] := PRMemory(J).RBytes[12];
              PRMemory(J).RBytes[12] := Temp1;
            end;
            14:
            begin
              Temp2 := PLMemory(I).LWords[6];
              PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
              PRMemory(J).RWords[6] := Temp2;
            end;
            15:
            begin
              Temp2 := PLMemory(I).LWords[6];
              PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
              PRMemory(J).RWords[6] := Temp2;
              Temp1 := PLMemory(I).LBytes[14];
              PLMemory(I).LBytes[14] := PRMemory(J).RBytes[14];
              PRMemory(J).RBytes[14] := Temp1;
            end;
          end;
        end;
      else
        Index := 0;
        repeat
          TempNative := PLMemory(I).LNatives[Index];
          PLMemory(I).LNatives[Index] := PRMemory(J).RNatives[Index];
          PRMemory(J).RNatives[Index] := TempNative;
          Inc(Index);
        until (Index = SizeOf(T) div SizeOf(NativeUInt));

        if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
        begin
          {$ifdef LARGEINT}
          if (SizeOf(T) and 4 <> 0) then
          begin
            Index := SizeOf(T) div SizeOf(Cardinal) - 1;
            Temp4 := PLMemory(I).LCardinals[Index];
            PLMemory(I).LCardinals[Index] := PRMemory(J).RCardinals[Index];
            PRMemory(J).RCardinals[Index] := Temp4;
          end;
          {$endif}

          if (SizeOf(T) and 2 <> 0) then
          begin
            Index := SizeOf(T) div SizeOf(Word) - 1;
            Temp2 := PLMemory(I).LWords[Index];
            PLMemory(I).LWords[Index] := PRMemory(J).RWords[Index];
            PRMemory(J).RWords[Index] := Temp2;
          end;

          if (SizeOf(T) and 1 <> 0) then
          begin
            Index := SizeOf(T) div SizeOf(Byte) - 1;
            Temp1 := PLMemory(I).LBytes[Index];
            PLMemory(I).LBytes[Index] := PRMemory(J).RBytes[Index];
            PRMemory(J).RBytes[Index] := Temp1;
          end;
        end;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;

class procedure TArray.SortDescendingBinaries<T>(const Values: Pointer; const Count: NativeInt; var PivotBig: T);
label
  proc_loop, proc_loop_current, insertion_init, swap_loop;
var
  Index: NativeInt;
  Temp1: Byte;
  Temp2: Word;
  Temp4: Cardinal;
  TempNative: NativeUInt;

  I, J, PStop: ^T;
  Pivot: TSortPivot;
  X, Y: NativeUInt;
  Buffer: Pointer;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

proc_loop:
  Dec(StackItem);
proc_loop_current:
  I := StackItem^.First;
  J := StackItem^.Last;

  // insertion sort
  if (SizeOf(T) <= SizeOf(Pivot)) then
  if ((NativeUInt(J) - NativeUInt(I)) < INSERTION_SORT_LIMIT * SizeOf(T)) then
  begin
    PStop := StackItem^.First;
    goto insertion_init;
    repeat
      if (J = PStop) then Break;
      Dec(J);

      // compare pivot/J
      Y := TArray.SortBinaryMarker<T>(J);
      if (X > Y) then Continue;
      if (X = Y) then
      begin
        if (GetTypeKind(T) = tkMethod) then
        begin
          if (NativeUInt(Pivot.Ptr) >= NativeUInt(Pointer(J)^)) then Continue;
        end else
        {$if CompilerVersion = 28}
          if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
            TRAIIHelper<T>.P(@Pivot)^, J^) >= 0) then Continue;
        {$else}
          if (GetTypeKind(T) = tkString) then
          begin
            if (InterfaceDefaults.Compare_OStr(nil, Pointer(@Pivot), Pointer(J)) >= 0) then Continue;
          end else
          if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
          begin
            Buffer := Pointer(Pointer(J)^);
            if (Pivot.Ptr = Buffer) then Continue;
            case GetTypeKind(T) of
              tkLString: if (InterfaceDefaults.Compare_LStr(nil, Pivot.Ptr, Buffer) >= 0) then Continue;
              tkWString: if (InterfaceDefaults.Compare_WStr(nil, Pivot.Ptr, Buffer) >= 0) then Continue;
              tkUString: if (InterfaceDefaults.Compare_UStr(nil, Pivot.Ptr, Buffer) >= 0) then Continue;
             tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Pivot.Ptr, Buffer) >= 0) then Continue;
            end;
          end else
          case SizeOf(T) of
            0..SizeOf(Cardinal): Continue;
            {$ifdef LARGEINT}
            SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(@Pivot)^, PInt64(J)^) >= 0) then Continue;
            {$endif}
          else
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(@Pivot), Pointer(J)) >= 0) then Continue;
          end;
        {$ifend}
      end;
    insertion_init:
      I := J;
      // pivot := J^
      if (SizeOf(T) = SizeOf(Pointer)) then
      begin
        Pivot.Ptr := Pointer(Pointer(J)^);
      end else
      begin
        TArray.Copy<T>(@Pivot, J);
      end;
      X := TArray.SortBinaryMarker<T>(@Pivot);
    until (False);
    // I^ := J^
    if (SizeOf(T) = SizeOf(Pointer)) then
    begin
      PNativeUInt(I)^ := PNativeUInt(J)^;
    end else
    begin
      TArray.Copy<T>(I, J);
    end;
    // J := pivot
    if (SizeOf(T) = SizeOf(Pointer)) then
    begin
      Pointer(Pointer(J)^) := Pivot.Ptr;
    end else
    begin
      TArray.Copy<T>(J, @Pivot);
    end;

    PStop := StackItem^.Last;
    repeat
      Inc(J);
      if (J = PStop) then Break;

      // pivot := J[1]
      if (SizeOf(T) = SizeOf(Pointer)) then
      begin
        Pivot.Ptr := Pointer(Pointer(J + 1)^);
      end else
      begin
        TArray.Copy<T>(@Pivot, J + 1);
      end;
      X := TArray.SortBinaryMarker<T>(@Pivot);

      // compare J/pivot
      Y := TArray.SortBinaryMarker<T>(J);
      if (Y > X) then Continue;
      if (Y = X) then
      begin
        if (GetTypeKind(T) = tkMethod) then
        begin
          if (NativeUInt(Pointer(J)^) >= NativeUInt(Pivot.Ptr)) then Continue;
        end else
        {$if CompilerVersion = 28}
          if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
            J^, TRAIIHelper<T>.P(@Pivot)^) >= 0) then Continue;
        {$else}
          if (GetTypeKind(T) = tkString) then
          begin
            if (InterfaceDefaults.Compare_OStr(nil, Pointer(J), Pointer(@Pivot)) >= 0) then Continue;
          end else
          if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
          begin
            Buffer := Pointer(Pointer(J)^);
            if (Buffer = Pivot.Ptr) then Continue;
            case GetTypeKind(T) of
              tkLString: if (InterfaceDefaults.Compare_LStr(nil, Buffer, Pivot.Ptr) >= 0) then Continue;
              tkWString: if (InterfaceDefaults.Compare_WStr(nil, Buffer, Pivot.Ptr) >= 0) then Continue;
              tkUString: if (InterfaceDefaults.Compare_UStr(nil, Buffer, Pivot.Ptr) >= 0) then Continue;
             tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Buffer, Pivot.Ptr) >= 0) then Continue;
            end;
          end else
          case SizeOf(T) of
            0..SizeOf(Cardinal): Continue;
            {$ifdef LARGEINT}
            SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(J)^, PInt64(@Pivot)^) >= 0) then Continue;
            {$endif}
          else
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(J), Pointer(@Pivot)) >= 0) then Continue;
          end;
        {$ifend}
      end;

      I := J;
      repeat
        // I[1] := I^;
        if (SizeOf(T) = SizeOf(Pointer)) then
        begin
          PNativeUInt(I + 1)^ := PNativeUInt(I)^;
        end else
        begin
          TArray.Copy<T>(I + 1, I);
        end;
        Dec(I);

        // compare I/pivot
        Y := TArray.SortBinaryMarker<T>(I);
        if (Y > X) then Break;
        if (Y = X) then
        begin
          if (GetTypeKind(T) = tkMethod) then
          begin
            if (NativeUInt(Pointer(I)^) >= NativeUInt(Pivot.Ptr)) then Break;
          end else
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              I^, TRAIIHelper<T>.P(@Pivot)^) >= 0) then Break;
          {$else}
            if (GetTypeKind(T) = tkString) then
            begin
              if (InterfaceDefaults.Compare_OStr(nil, Pointer(I), Pointer(@Pivot)) >= 0) then Break;
            end else
            if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
            begin
              Buffer := Pointer(Pointer(I)^);
              if (Buffer = Pivot.Ptr) then Break;
              case GetTypeKind(T) of
                tkLString: if (InterfaceDefaults.Compare_LStr(nil, Buffer, Pivot.Ptr) >= 0) then Break;
                tkWString: if (InterfaceDefaults.Compare_WStr(nil, Buffer, Pivot.Ptr) >= 0) then Break;
                tkUString: if (InterfaceDefaults.Compare_UStr(nil, Buffer, Pivot.Ptr) >= 0) then Break;
               tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Buffer, Pivot.Ptr) >= 0) then Break;
              end;
            end else
            case SizeOf(T) of
              0..SizeOf(Cardinal): Break;
              {$ifdef LARGEINT}
              SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(I)^, PInt64(@Pivot)^) >= 0) then Break;
              {$endif}
            else
              if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
                Pointer(I), Pointer(@Pivot)) >= 0) then Break;
            end;
          {$ifend}
        end;
      until (False);

      // I[1] := pivot
      if (SizeOf(T) = SizeOf(Pointer)) then
      begin
        Pointer(Pointer(I + 1)^) := Pivot.Ptr;
      end else
      begin
        TArray.Copy<T>(I + 1, @Pivot);
      end;
    until (False);

    if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
    Exit;
  end;

  // pivot
  if (SizeOf(T) <= SizeOf(Pivot)) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) then
    begin
      Pivot.Ptr := Pointer(SortItemPivot<T>(I, J)^);
    end else
    begin
      TArray.Copy<T>(@Pivot, SortItemPivot<T>(I, J));
    end;
    X := TArray.SortBinaryMarker<T>(@Pivot);
  end else
  begin
    TArray.Copy<T>(@PivotBig, SortItemPivot<T>(I, J));
    X := TArray.SortBinaryMarker<T>(@PivotBig);
  end;

  // quick sort
  Dec(J);
  Dec(I);
  swap_loop:
  begin
    Inc(J, 2);

    repeat
      Inc(I);

      Y := TArray.SortBinaryMarker<T>(I);
      if (X > Y) then Break;
      if (X = Y) then
      begin
        if (SizeOf(T) >= SizeOf(Pivot)) then
        begin
          // compare pivot/I
          if (GetTypeKind(T) = tkMethod) then
          begin
            if (NativeUInt(Pivot.Ptr) >= NativeUInt(Pointer(I)^)) then Break;
          end else
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              TRAIIHelper<T>.P(@Pivot)^, I^) >= 0) then Break;
          {$else}
            if (GetTypeKind(T) = tkString) then
            begin
              if (InterfaceDefaults.Compare_OStr(nil, Pointer(@Pivot), Pointer(I)) >= 0) then Break;
            end else
            if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
            begin
              Buffer := Pointer(Pointer(I)^);
              if (Pivot.Ptr = Buffer) then Break;
              case GetTypeKind(T) of
                tkLString: if (InterfaceDefaults.Compare_LStr(nil, Pivot.Ptr, Buffer) >= 0) then Break;
                tkWString: if (InterfaceDefaults.Compare_WStr(nil, Pivot.Ptr, Buffer) >= 0) then Break;
                tkUString: if (InterfaceDefaults.Compare_UStr(nil, Pivot.Ptr, Buffer) >= 0) then Break;
               tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Pivot.Ptr, Buffer) >= 0) then Break;
              end;
            end else
            case SizeOf(T) of
              0..SizeOf(Cardinal): Break;
              {$ifdef LARGEINT}
              SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(@Pivot)^, PInt64(I)^) >= 0) then Break;
              {$endif}
            else
              if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
                Pointer(@Pivot), Pointer(I)) >= 0) then Break;
            end;
          {$ifend}
        end else
        begin
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              TRAIIHelper<T>.P(@PivotBig)^, I^) >= 0) then Break;
          {$else}
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(@PivotBig), Pointer(I)) >= 0) then Break;
          {$ifend}
        end;
      end;
    until (False);

    repeat
      Dec(J);

      Y := TArray.SortBinaryMarker<T>(J);
      if (Y > X) then Break;
      if (Y = X) then
      begin
        if (SizeOf(T) <= SizeOf(Pivot)) then
        begin
          // compare J/pivot
          if (GetTypeKind(T) = tkMethod) then
          begin
            if (NativeUInt(Pointer(J)^) >= NativeUInt(Pivot.Ptr)) then Break;
          end else
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              J^, TRAIIHelper<T>.P(@Pivot)^) >= 0) then Break;
          {$else}
            if (GetTypeKind(T) = tkString) then
            begin
              if (InterfaceDefaults.Compare_OStr(nil, Pointer(J), Pointer(@Pivot)) >= 0) then Break;
            end else
            if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
            begin
              Buffer := Pointer(Pointer(J)^);
              if (Buffer = Pivot.Ptr) then Break;
              case GetTypeKind(T) of
                tkLString: if (InterfaceDefaults.Compare_LStr(nil, Buffer, Pivot.Ptr) >= 0) then Break;
                tkWString: if (InterfaceDefaults.Compare_WStr(nil, Buffer, Pivot.Ptr) >= 0) then Break;
                tkUString: if (InterfaceDefaults.Compare_UStr(nil, Buffer, Pivot.Ptr) >= 0) then Break;
               tkDynArray: if (InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, Buffer, Pivot.Ptr) >= 0) then Break;
              end;
            end else
            case SizeOf(T) of
              0..SizeOf(Cardinal): Break;
              {$ifdef LARGEINT}
              SizeOf(Int64): if (InterfaceDefaults.Compare_Bin8(nil, PInt64(J)^, PInt64(@Pivot)^) >= 0) then Break;
              {$endif}
            else
              if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
                Pointer(J), Pointer(@Pivot)) >= 0) then Break;
            end;
          {$ifend}
        end else
        begin
          {$if CompilerVersion = 28}
            if (IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
              J^, TRAIIHelper<T>.P(@PivotBig)^) >= 0) then Break;
          {$else}
            if (InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(J), Pointer(@PivotBig)) >= 0) then Break;
          {$ifend}
        end;
      end;
    until (False);

    if (I <= J) then
    begin
      // TArray.Exchange<T>(I, J);
      case SizeOf(T) of
        0: ;
        1:
        begin
          Temp1 := PLMemory(I).LBytes[0];
          PLMemory(I).LBytes[0] := PRMemory(J).RBytes[0];
          PRMemory(J).RBytes[0] := Temp1;
        end;
        2:
        begin
          Temp2 := PLMemory(I).LWords[0];
          PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
          PRMemory(J).RWords[0] := Temp2;
        end;
        3:
        begin
          Temp2 := PLMemory(I).LWords[0];
          PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
          PRMemory(J).RWords[0] := Temp2;

          Temp1 := PLMemory(I).LBytes[2];
          PLMemory(I).LBytes[2] := PRMemory(J).RBytes[2];
          PRMemory(J).RBytes[2] := Temp1;
        end;
        4..7:
        begin
          Temp4 := PLMemory(I).LCardinals[0];
          PLMemory(I).LCardinals[0] := PRMemory(J).RCardinals[0];
          PRMemory(J).RCardinals[0] := Temp4;

          case SizeOf(T) of
            5:
            begin
              Temp1 := PLMemory(I).LBytes[4];
              PLMemory(I).LBytes[4] := PRMemory(J).RBytes[4];
              PRMemory(J).RBytes[4] := Temp1;
            end;
            6:
            begin
              Temp2 := PLMemory(I).LWords[2];
              PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
              PRMemory(J).RWords[2] := Temp2;
            end;
            7:
            begin
              Temp2 := PLMemory(I).LWords[2];
              PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
              PRMemory(J).RWords[2] := Temp2;
              Temp1 := PLMemory(I).LBytes[6];
              PLMemory(I).LBytes[6] := PRMemory(J).RBytes[6];
              PRMemory(J).RBytes[6] := Temp1;
            end;
          end;
        end;
        8..16:
        begin
          TempNative := PLMemory(I).LNatives[0];
          PLMemory(I).LNatives[0] := PRMemory(J).RNatives[0];
          PRMemory(J).RNatives[0] := TempNative;

          if (SizeOf(T) >= SizeOf(NativeUInt) * 2) then
          begin
            TempNative := PLMemory(I).LNatives[1];
            PLMemory(I).LNatives[1] := PRMemory(J).RNatives[1];
            PRMemory(J).RNatives[1] := TempNative;
          end;

          if (SizeOf(T) >= SizeOf(NativeUInt) * 3) then
          begin
            TempNative := PLMemory(I).LNatives[2];
            PLMemory(I).LNatives[2] := PRMemory(J).RNatives[2];
            PRMemory(J).RNatives[2] := TempNative;
          end;

          if (SizeOf(T)  = SizeOf(NativeUInt) * 4) then
          begin
            TempNative := PLMemory(I).LNatives[3];
            PLMemory(I).LNatives[3] := PRMemory(J).RNatives[3];
            PRMemory(J).RNatives[3] := TempNative;
          end;

          {$ifdef LARGEINT}
          case SizeOf(T) of
            12, 13, 14, 15:
            begin
              Temp4 := PLMemory(I).LCardinals[2];
              PLMemory(I).LCardinals[2] := PRMemory(J).RCardinals[2];
              PRMemory(J).RCardinals[2] := Temp4;
            end;
          end;
          {$endif}

          case SizeOf(T) of
            9:
            begin
              Temp1 := PLMemory(I).LBytes[8];
              PLMemory(I).LBytes[8] := PRMemory(J).RBytes[8];
              PRMemory(J).RBytes[8] := Temp1;
            end;
            10:
            begin
              Temp2 := PLMemory(I).LWords[4];
              PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
              PRMemory(J).RWords[4] := Temp2;
            end;
            11:
            begin
              Temp2 := PLMemory(I).LWords[4];
              PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
              PRMemory(J).RWords[4] := Temp2;
              Temp1 := PLMemory(I).LBytes[10];
              PLMemory(I).LBytes[10] := PRMemory(J).RBytes[10];
              PRMemory(J).RBytes[10] := Temp1;
            end;
            13:
            begin
              Temp2 := PLMemory(I).LWords[5];
              PLMemory(I).LWords[5] := PRMemory(J).RWords[5];
              PRMemory(J).RWords[5] := Temp2;
              Temp1 := PLMemory(I).LBytes[12];
              PLMemory(I).LBytes[12] := PRMemory(J).RBytes[12];
              PRMemory(J).RBytes[12] := Temp1;
            end;
            14:
            begin
              Temp2 := PLMemory(I).LWords[6];
              PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
              PRMemory(J).RWords[6] := Temp2;
            end;
            15:
            begin
              Temp2 := PLMemory(I).LWords[6];
              PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
              PRMemory(J).RWords[6] := Temp2;
              Temp1 := PLMemory(I).LBytes[14];
              PLMemory(I).LBytes[14] := PRMemory(J).RBytes[14];
              PRMemory(J).RBytes[14] := Temp1;
            end;
          end;
        end;
      else
        Index := 0;
        repeat
          TempNative := PLMemory(I).LNatives[Index];
          PLMemory(I).LNatives[Index] := PRMemory(J).RNatives[Index];
          PRMemory(J).RNatives[Index] := TempNative;
          Inc(Index);
        until (Index = SizeOf(T) div SizeOf(NativeUInt));

        if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
        begin
          {$ifdef LARGEINT}
          if (SizeOf(T) and 4 <> 0) then
          begin
            Index := SizeOf(T) div SizeOf(Cardinal) - 1;
            Temp4 := PLMemory(I).LCardinals[Index];
            PLMemory(I).LCardinals[Index] := PRMemory(J).RCardinals[Index];
            PRMemory(J).RCardinals[Index] := Temp4;
          end;
          {$endif}

          if (SizeOf(T) and 2 <> 0) then
          begin
            Index := SizeOf(T) div SizeOf(Word) - 1;
            Temp2 := PLMemory(I).LWords[Index];
            PLMemory(I).LWords[Index] := PRMemory(J).RWords[Index];
            PRMemory(J).RWords[Index] := Temp2;
          end;

          if (SizeOf(T) and 1 <> 0) then
          begin
            Index := SizeOf(T) div SizeOf(Byte) - 1;
            Temp1 := PLMemory(I).LBytes[Index];
            PLMemory(I).LBytes[Index] := PRMemory(J).RBytes[Index];
            PRMemory(J).RBytes[Index] := Temp1;
          end;
        end;
      end;

      Dec(J, 2);
      if (I <= J) then goto swap_loop;
      Inc(I);
      Inc(J);
    end;
  end;

  // next iteration
  StackItem := SortItemNext<T>(StackItem, I, J);
  if (NativeInt(StackItem) >= 0) then goto proc_loop_current;
  Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
  if (StackItem <> Pointer(@Stack[0])) then goto proc_loop;
end;
{$endif .SMARTGENERICS}

{$ifdef WEAKREF}
class procedure TArray.WeakSortUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>);
var
  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

  repeat
    Dec(StackItem);
    I := StackItem^.First;
    J := StackItem^.Last;

    // insertion sort
    if ((NativeUInt(J) - NativeUInt(I)) < INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      Helper.Temp := J^;
      I := J;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        if (Helper.Compare(Helper.Inst, Helper.Temp, J^) <= 0) then Continue;
        I := J;
      until (False);
      I^ := J^;
      J^ := Helper.Temp;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        Helper.Temp := (J + 1)^;
        if (Helper.Compare(Helper.Inst, J^, Helper.Temp) <= 0) then Continue;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          if (Helper.Compare(Helper.Inst, I^, Helper.Temp) <= 0) then Break;
        until (False);

        (I + 1)^ := Helper.Temp;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then Continue;
      Exit;
    end;

    // pivot
    System.Move(SortItemPivot<T>(I, J)^, Helper.Pivot, SizeOf(T));

    // quick sort
    Dec(J);
    Dec(I);
    repeat
      Inc(J, 2);

      repeat
        Inc(I);
        if (Helper.Compare(Helper.Inst, Helper.Pivot, I^) <= 0) then Break;
      until (False);

      repeat
        Dec(J);
        if (Helper.Compare(Helper.Inst, J^, Helper.Pivot) <= 0) then Break;
      until (False);

      if (I <= J) then
      begin
        Helper.Temp := I^;
        I^ := J^;
        J^ := Helper.Temp;

        Dec(J, 2);
        if (I <= J) then Continue;
        Inc(I);
        Inc(J);
      end;
      Break;
    until (False);

    // next iteration
    StackItem := SortItemNext<T>(StackItem, I, J);
    if (NativeInt(StackItem) >= 0) then
    begin
      Inc(StackItem);
      Continue;
    end;
    Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
    if (StackItem <> Pointer(@Stack[0])) then Continue;
    Break;
  until (False);
end;
{$endif}

class procedure TArray.SortUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>);
var
  Index: NativeInt;
  Temp1: Byte;
  Temp2: Word;
  Temp4: Cardinal;
  TempNative: NativeUInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

  repeat
    Dec(StackItem);
    I := StackItem^.First;
    J := StackItem^.Last;

    // insertion sort
    if ((NativeUInt(J) - NativeUInt(I)) < INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;

      case SizeOf(T) of
        1: TRAIIHelper.T1(Pointer(@Helper.Pivot)^) := TRAIIHelper.T1(Pointer(J)^);
        2: TRAIIHelper.T2(Pointer(@Helper.Pivot)^) := TRAIIHelper.T2(Pointer(J)^);
        3: TRAIIHelper.T3(Pointer(@Helper.Pivot)^) := TRAIIHelper.T3(Pointer(J)^);
        4: TRAIIHelper.T4(Pointer(@Helper.Pivot)^) := TRAIIHelper.T4(Pointer(J)^);
        5: TRAIIHelper.T5(Pointer(@Helper.Pivot)^) := TRAIIHelper.T5(Pointer(J)^);
        6: TRAIIHelper.T6(Pointer(@Helper.Pivot)^) := TRAIIHelper.T6(Pointer(J)^);
        7: TRAIIHelper.T7(Pointer(@Helper.Pivot)^) := TRAIIHelper.T7(Pointer(J)^);
        8: TRAIIHelper.T8(Pointer(@Helper.Pivot)^) := TRAIIHelper.T8(Pointer(J)^);
        9: TRAIIHelper.T9(Pointer(@Helper.Pivot)^) := TRAIIHelper.T9(Pointer(J)^);
       10: TRAIIHelper.T10(Pointer(@Helper.Pivot)^) := TRAIIHelper.T10(Pointer(J)^);
       11: TRAIIHelper.T11(Pointer(@Helper.Pivot)^) := TRAIIHelper.T11(Pointer(J)^);
       12: TRAIIHelper.T12(Pointer(@Helper.Pivot)^) := TRAIIHelper.T12(Pointer(J)^);
       13: TRAIIHelper.T13(Pointer(@Helper.Pivot)^) := TRAIIHelper.T13(Pointer(J)^);
       14: TRAIIHelper.T14(Pointer(@Helper.Pivot)^) := TRAIIHelper.T14(Pointer(J)^);
       15: TRAIIHelper.T15(Pointer(@Helper.Pivot)^) := TRAIIHelper.T15(Pointer(J)^);
       16: TRAIIHelper.T16(Pointer(@Helper.Pivot)^) := TRAIIHelper.T16(Pointer(J)^);
       17: TRAIIHelper.T17(Pointer(@Helper.Pivot)^) := TRAIIHelper.T17(Pointer(J)^);
       18: TRAIIHelper.T18(Pointer(@Helper.Pivot)^) := TRAIIHelper.T18(Pointer(J)^);
       19: TRAIIHelper.T19(Pointer(@Helper.Pivot)^) := TRAIIHelper.T19(Pointer(J)^);
       20: TRAIIHelper.T20(Pointer(@Helper.Pivot)^) := TRAIIHelper.T20(Pointer(J)^);
       21: TRAIIHelper.T21(Pointer(@Helper.Pivot)^) := TRAIIHelper.T21(Pointer(J)^);
       22: TRAIIHelper.T22(Pointer(@Helper.Pivot)^) := TRAIIHelper.T22(Pointer(J)^);
       23: TRAIIHelper.T23(Pointer(@Helper.Pivot)^) := TRAIIHelper.T23(Pointer(J)^);
       24: TRAIIHelper.T24(Pointer(@Helper.Pivot)^) := TRAIIHelper.T24(Pointer(J)^);
       25: TRAIIHelper.T25(Pointer(@Helper.Pivot)^) := TRAIIHelper.T25(Pointer(J)^);
       26: TRAIIHelper.T26(Pointer(@Helper.Pivot)^) := TRAIIHelper.T26(Pointer(J)^);
       27: TRAIIHelper.T27(Pointer(@Helper.Pivot)^) := TRAIIHelper.T27(Pointer(J)^);
       28: TRAIIHelper.T28(Pointer(@Helper.Pivot)^) := TRAIIHelper.T28(Pointer(J)^);
       29: TRAIIHelper.T29(Pointer(@Helper.Pivot)^) := TRAIIHelper.T29(Pointer(J)^);
       30: TRAIIHelper.T30(Pointer(@Helper.Pivot)^) := TRAIIHelper.T30(Pointer(J)^);
       31: TRAIIHelper.T31(Pointer(@Helper.Pivot)^) := TRAIIHelper.T31(Pointer(J)^);
       32: TRAIIHelper.T32(Pointer(@Helper.Pivot)^) := TRAIIHelper.T32(Pointer(J)^);
       33: TRAIIHelper.T33(Pointer(@Helper.Pivot)^) := TRAIIHelper.T33(Pointer(J)^);
       34: TRAIIHelper.T34(Pointer(@Helper.Pivot)^) := TRAIIHelper.T34(Pointer(J)^);
       35: TRAIIHelper.T35(Pointer(@Helper.Pivot)^) := TRAIIHelper.T35(Pointer(J)^);
       36: TRAIIHelper.T36(Pointer(@Helper.Pivot)^) := TRAIIHelper.T36(Pointer(J)^);
       37: TRAIIHelper.T37(Pointer(@Helper.Pivot)^) := TRAIIHelper.T37(Pointer(J)^);
       38: TRAIIHelper.T38(Pointer(@Helper.Pivot)^) := TRAIIHelper.T38(Pointer(J)^);
       39: TRAIIHelper.T39(Pointer(@Helper.Pivot)^) := TRAIIHelper.T39(Pointer(J)^);
       40: TRAIIHelper.T40(Pointer(@Helper.Pivot)^) := TRAIIHelper.T40(Pointer(J)^);
      else
        System.Move(J^, Helper.Pivot, SizeOf(T));
      end;
      I := J;

      repeat
        if (J = PStop) then Break;
        Dec(J);

        if (Helper.Compare(Helper.Inst, Helper.Pivot, J^) <= 0) then Continue;
        I := J;
        TArray.Copy<T>(@Helper.Pivot, J);
      until (False);
      TArray.Copy<T>(I, J);
      TArray.Copy<T>(J, @Helper.Pivot);

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        TArray.Copy<T>(@Helper.Pivot, J + 1);
        if (Helper.Compare(Helper.Inst, J^, Helper.Pivot) <= 0) then Continue;

        I := J;
        repeat
          TArray.Copy<T>(I + 1, I);
          Dec(I);
          if (Helper.Compare(Helper.Inst, I^, Helper.Pivot) <= 0) then Break;
        until (False);

        TArray.Copy<T>(I + 1, @Helper.Pivot);
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then Continue;
      Exit;
    end;

    // pivot
    TArray.Copy<T>(@Helper.Pivot, SortItemPivot<T>(I, J));

    // quick sort
    Dec(J);
    Dec(I);
    repeat
      Inc(J, 2);

      repeat
        Inc(I);
        if (Helper.Compare(Helper.Inst, Helper.Pivot, I^) <= 0) then Break;
      until (False);

      repeat
        Dec(J);
        if (Helper.Compare(Helper.Inst, J^, Helper.Pivot) <= 0) then Break;
      until (False);

      if (I <= J) then
      begin
        // TArray.Exchange<T>(I, J);
        case SizeOf(T) of
          0: ;
          1:
          begin
            Temp1 := PLMemory(I).LBytes[0];
            PLMemory(I).LBytes[0] := PRMemory(J).RBytes[0];
            PRMemory(J).RBytes[0] := Temp1;
          end;
          2:
          begin
            Temp2 := PLMemory(I).LWords[0];
            PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
            PRMemory(J).RWords[0] := Temp2;
          end;
          3:
          begin
            Temp2 := PLMemory(I).LWords[0];
            PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
            PRMemory(J).RWords[0] := Temp2;

            Temp1 := PLMemory(I).LBytes[2];
            PLMemory(I).LBytes[2] := PRMemory(J).RBytes[2];
            PRMemory(J).RBytes[2] := Temp1;
          end;
          4..7:
          begin
            Temp4 := PLMemory(I).LCardinals[0];
            PLMemory(I).LCardinals[0] := PRMemory(J).RCardinals[0];
            PRMemory(J).RCardinals[0] := Temp4;

            case SizeOf(T) of
              5:
              begin
                Temp1 := PLMemory(I).LBytes[4];
                PLMemory(I).LBytes[4] := PRMemory(J).RBytes[4];
                PRMemory(J).RBytes[4] := Temp1;
              end;
              6:
              begin
                Temp2 := PLMemory(I).LWords[2];
                PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
                PRMemory(J).RWords[2] := Temp2;
              end;
              7:
              begin
                Temp2 := PLMemory(I).LWords[2];
                PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
                PRMemory(J).RWords[2] := Temp2;
                Temp1 := PLMemory(I).LBytes[6];
                PLMemory(I).LBytes[6] := PRMemory(J).RBytes[6];
                PRMemory(J).RBytes[6] := Temp1;
              end;
            end;
          end;
          8..16:
          begin
            TempNative := PLMemory(I).LNatives[0];
            PLMemory(I).LNatives[0] := PRMemory(J).RNatives[0];
            PRMemory(J).RNatives[0] := TempNative;

            if (SizeOf(T) >= SizeOf(NativeUInt) * 2) then
            begin
              TempNative := PLMemory(I).LNatives[1];
              PLMemory(I).LNatives[1] := PRMemory(J).RNatives[1];
              PRMemory(J).RNatives[1] := TempNative;
            end;

            if (SizeOf(T) >= SizeOf(NativeUInt) * 3) then
            begin
              TempNative := PLMemory(I).LNatives[2];
              PLMemory(I).LNatives[2] := PRMemory(J).RNatives[2];
              PRMemory(J).RNatives[2] := TempNative;
            end;

            if (SizeOf(T)  = SizeOf(NativeUInt) * 4) then
            begin
              TempNative := PLMemory(I).LNatives[3];
              PLMemory(I).LNatives[3] := PRMemory(J).RNatives[3];
              PRMemory(J).RNatives[3] := TempNative;
            end;

            {$ifdef LARGEINT}
            case SizeOf(T) of
              12, 13, 14, 15:
              begin
                Temp4 := PLMemory(I).LCardinals[2];
                PLMemory(I).LCardinals[2] := PRMemory(J).RCardinals[2];
                PRMemory(J).RCardinals[2] := Temp4;
              end;
            end;
            {$endif}

            case SizeOf(T) of
              9:
              begin
                Temp1 := PLMemory(I).LBytes[8];
                PLMemory(I).LBytes[8] := PRMemory(J).RBytes[8];
                PRMemory(J).RBytes[8] := Temp1;
              end;
              10:
              begin
                Temp2 := PLMemory(I).LWords[4];
                PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
                PRMemory(J).RWords[4] := Temp2;
              end;
              11:
              begin
                Temp2 := PLMemory(I).LWords[4];
                PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
                PRMemory(J).RWords[4] := Temp2;
                Temp1 := PLMemory(I).LBytes[10];
                PLMemory(I).LBytes[10] := PRMemory(J).RBytes[10];
                PRMemory(J).RBytes[10] := Temp1;
              end;
              13:
              begin
                Temp2 := PLMemory(I).LWords[5];
                PLMemory(I).LWords[5] := PRMemory(J).RWords[5];
                PRMemory(J).RWords[5] := Temp2;
                Temp1 := PLMemory(I).LBytes[12];
                PLMemory(I).LBytes[12] := PRMemory(J).RBytes[12];
                PRMemory(J).RBytes[12] := Temp1;
              end;
              14:
              begin
                Temp2 := PLMemory(I).LWords[6];
                PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
                PRMemory(J).RWords[6] := Temp2;
              end;
              15:
              begin
                Temp2 := PLMemory(I).LWords[6];
                PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
                PRMemory(J).RWords[6] := Temp2;
                Temp1 := PLMemory(I).LBytes[14];
                PLMemory(I).LBytes[14] := PRMemory(J).RBytes[14];
                PRMemory(J).RBytes[14] := Temp1;
              end;
            end;
          end;
        else
          Index := 0;
          repeat
            TempNative := PLMemory(I).LNatives[Index];
            PLMemory(I).LNatives[Index] := PRMemory(J).RNatives[Index];
            PRMemory(J).RNatives[Index] := TempNative;
            Inc(Index);
          until (Index = SizeOf(T) div SizeOf(NativeUInt));

          if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
          begin
            {$ifdef LARGEINT}
            if (SizeOf(T) and 4 <> 0) then
            begin
              Index := SizeOf(T) div SizeOf(Cardinal) - 1;
              Temp4 := PLMemory(I).LCardinals[Index];
              PLMemory(I).LCardinals[Index] := PRMemory(J).RCardinals[Index];
              PRMemory(J).RCardinals[Index] := Temp4;
            end;
            {$endif}

            if (SizeOf(T) and 2 <> 0) then
            begin
              Index := SizeOf(T) div SizeOf(Word) - 1;
              Temp2 := PLMemory(I).LWords[Index];
              PLMemory(I).LWords[Index] := PRMemory(J).RWords[Index];
              PRMemory(J).RWords[Index] := Temp2;
            end;

            if (SizeOf(T) and 1 <> 0) then
            begin
              Index := SizeOf(T) div SizeOf(Byte) - 1;
              Temp1 := PLMemory(I).LBytes[Index];
              PLMemory(I).LBytes[Index] := PRMemory(J).RBytes[Index];
              PRMemory(J).RBytes[Index] := Temp1;
            end;
          end;
        end;

        Dec(J, 2);
        if (I <= J) then Continue;
        Inc(I);
        Inc(J);
      end;
      Break;
    until (False);

    // next iteration
    StackItem := SortItemNext<T>(StackItem, I, J);
    if (NativeInt(StackItem) >= 0) then
    begin
      Inc(StackItem);
      Continue;
    end;
    Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
    if (StackItem <> Pointer(@Stack[0])) then Continue;
    Break;
  until (False);
end;

{$ifdef WEAKREF}
class procedure TArray.WeakSortDescendingUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>);
var
  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

  repeat
    Dec(StackItem);
    I := StackItem^.First;
    J := StackItem^.Last;

    // insertion sort
    if ((NativeUInt(J) - NativeUInt(I)) < INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;
      Helper.Temp := J^;
      I := J;
      repeat
        if (J = PStop) then Break;
        Dec(J);

        if (Helper.Compare(Helper.Inst, Helper.Temp, J^) >= 0) then Continue;
        I := J;
      until (False);
      I^ := J^;
      J^ := Helper.Temp;

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        Helper.Temp := (J + 1)^;
        if (Helper.Compare(Helper.Inst, J^, Helper.Temp) >= 0) then Continue;

        I := J;
        repeat
          (I + 1)^ := I^;
          Dec(I);
          if (Helper.Compare(Helper.Inst, I^, Helper.Temp) >= 0) then Break;
        until (False);

        (I + 1)^ := Helper.Temp;
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then Continue;
      Exit;
    end;

    // pivot
    System.Move(SortItemPivot<T>(I, J)^, Helper.Pivot, SizeOf(T));

    // quick sort
    Dec(J);
    Dec(I);
    repeat
      Inc(J, 2);

      repeat
        Inc(I);
        if (Helper.Compare(Helper.Inst, Helper.Pivot, I^) >= 0) then Break;
      until (False);

      repeat
        Dec(J);
        if (Helper.Compare(Helper.Inst, J^, Helper.Pivot) >= 0) then Break;
      until (False);

      if (I <= J) then
      begin
        Helper.Temp := I^;
        I^ := J^;
        J^ := Helper.Temp;

        Dec(J, 2);
        if (I <= J) then Continue;
        Inc(I);
        Inc(J);
      end;
      Break;
    until (False);

    // next iteration
    StackItem := SortItemNext<T>(StackItem, I, J);
    if (NativeInt(StackItem) >= 0) then
    begin
      Inc(StackItem);
      Continue;
    end;
    Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
    if (StackItem <> Pointer(@Stack[0])) then Continue;
    Break;
  until (False);
end;
{$endif}

class procedure TArray.SortDescendingUniversals<T>(const Values: Pointer; const Count: NativeInt; var Helper: TSortHelper<T>);
var
  Index: NativeInt;
  Temp1: Byte;
  Temp2: Word;
  Temp4: Cardinal;
  TempNative: NativeUInt;

  I, J, PStop: ^T;
  StackItem: ^TSortStackItem<T>;
  Stack: TSortStack<T>;
begin
  Stack[0].First := Values;
  Stack[0].Last := TRAIIHelper<T>.P(Values) + Count - 1;
  StackItem := Pointer(@Stack[1]);

  repeat
    Dec(StackItem);
    I := StackItem^.First;
    J := StackItem^.Last;

    // insertion sort
    if ((NativeUInt(J) - NativeUInt(I)) < INSERTION_SORT_LIMIT * SizeOf(T)) then
    begin
      PStop := StackItem^.First;

      case SizeOf(T) of
        1: TRAIIHelper.T1(Pointer(@Helper.Pivot)^) := TRAIIHelper.T1(Pointer(J)^);
        2: TRAIIHelper.T2(Pointer(@Helper.Pivot)^) := TRAIIHelper.T2(Pointer(J)^);
        3: TRAIIHelper.T3(Pointer(@Helper.Pivot)^) := TRAIIHelper.T3(Pointer(J)^);
        4: TRAIIHelper.T4(Pointer(@Helper.Pivot)^) := TRAIIHelper.T4(Pointer(J)^);
        5: TRAIIHelper.T5(Pointer(@Helper.Pivot)^) := TRAIIHelper.T5(Pointer(J)^);
        6: TRAIIHelper.T6(Pointer(@Helper.Pivot)^) := TRAIIHelper.T6(Pointer(J)^);
        7: TRAIIHelper.T7(Pointer(@Helper.Pivot)^) := TRAIIHelper.T7(Pointer(J)^);
        8: TRAIIHelper.T8(Pointer(@Helper.Pivot)^) := TRAIIHelper.T8(Pointer(J)^);
        9: TRAIIHelper.T9(Pointer(@Helper.Pivot)^) := TRAIIHelper.T9(Pointer(J)^);
       10: TRAIIHelper.T10(Pointer(@Helper.Pivot)^) := TRAIIHelper.T10(Pointer(J)^);
       11: TRAIIHelper.T11(Pointer(@Helper.Pivot)^) := TRAIIHelper.T11(Pointer(J)^);
       12: TRAIIHelper.T12(Pointer(@Helper.Pivot)^) := TRAIIHelper.T12(Pointer(J)^);
       13: TRAIIHelper.T13(Pointer(@Helper.Pivot)^) := TRAIIHelper.T13(Pointer(J)^);
       14: TRAIIHelper.T14(Pointer(@Helper.Pivot)^) := TRAIIHelper.T14(Pointer(J)^);
       15: TRAIIHelper.T15(Pointer(@Helper.Pivot)^) := TRAIIHelper.T15(Pointer(J)^);
       16: TRAIIHelper.T16(Pointer(@Helper.Pivot)^) := TRAIIHelper.T16(Pointer(J)^);
       17: TRAIIHelper.T17(Pointer(@Helper.Pivot)^) := TRAIIHelper.T17(Pointer(J)^);
       18: TRAIIHelper.T18(Pointer(@Helper.Pivot)^) := TRAIIHelper.T18(Pointer(J)^);
       19: TRAIIHelper.T19(Pointer(@Helper.Pivot)^) := TRAIIHelper.T19(Pointer(J)^);
       20: TRAIIHelper.T20(Pointer(@Helper.Pivot)^) := TRAIIHelper.T20(Pointer(J)^);
       21: TRAIIHelper.T21(Pointer(@Helper.Pivot)^) := TRAIIHelper.T21(Pointer(J)^);
       22: TRAIIHelper.T22(Pointer(@Helper.Pivot)^) := TRAIIHelper.T22(Pointer(J)^);
       23: TRAIIHelper.T23(Pointer(@Helper.Pivot)^) := TRAIIHelper.T23(Pointer(J)^);
       24: TRAIIHelper.T24(Pointer(@Helper.Pivot)^) := TRAIIHelper.T24(Pointer(J)^);
       25: TRAIIHelper.T25(Pointer(@Helper.Pivot)^) := TRAIIHelper.T25(Pointer(J)^);
       26: TRAIIHelper.T26(Pointer(@Helper.Pivot)^) := TRAIIHelper.T26(Pointer(J)^);
       27: TRAIIHelper.T27(Pointer(@Helper.Pivot)^) := TRAIIHelper.T27(Pointer(J)^);
       28: TRAIIHelper.T28(Pointer(@Helper.Pivot)^) := TRAIIHelper.T28(Pointer(J)^);
       29: TRAIIHelper.T29(Pointer(@Helper.Pivot)^) := TRAIIHelper.T29(Pointer(J)^);
       30: TRAIIHelper.T30(Pointer(@Helper.Pivot)^) := TRAIIHelper.T30(Pointer(J)^);
       31: TRAIIHelper.T31(Pointer(@Helper.Pivot)^) := TRAIIHelper.T31(Pointer(J)^);
       32: TRAIIHelper.T32(Pointer(@Helper.Pivot)^) := TRAIIHelper.T32(Pointer(J)^);
       33: TRAIIHelper.T33(Pointer(@Helper.Pivot)^) := TRAIIHelper.T33(Pointer(J)^);
       34: TRAIIHelper.T34(Pointer(@Helper.Pivot)^) := TRAIIHelper.T34(Pointer(J)^);
       35: TRAIIHelper.T35(Pointer(@Helper.Pivot)^) := TRAIIHelper.T35(Pointer(J)^);
       36: TRAIIHelper.T36(Pointer(@Helper.Pivot)^) := TRAIIHelper.T36(Pointer(J)^);
       37: TRAIIHelper.T37(Pointer(@Helper.Pivot)^) := TRAIIHelper.T37(Pointer(J)^);
       38: TRAIIHelper.T38(Pointer(@Helper.Pivot)^) := TRAIIHelper.T38(Pointer(J)^);
       39: TRAIIHelper.T39(Pointer(@Helper.Pivot)^) := TRAIIHelper.T39(Pointer(J)^);
       40: TRAIIHelper.T40(Pointer(@Helper.Pivot)^) := TRAIIHelper.T40(Pointer(J)^);
      else
        System.Move(J^, Helper.Pivot, SizeOf(T));
      end;
      I := J;

      repeat
        if (J = PStop) then Break;
        Dec(J);

        if (Helper.Compare(Helper.Inst, Helper.Pivot, J^) >= 0) then Continue;
        I := J;
        TArray.Copy<T>(@Helper.Pivot, J);
      until (False);
      TArray.Copy<T>(I, J);
      TArray.Copy<T>(J, @Helper.Pivot);

      PStop := StackItem^.Last;
      repeat
        Inc(J);
        if (J = PStop) then Break;

        TArray.Copy<T>(@Helper.Pivot, J + 1);
        if (Helper.Compare(Helper.Inst, J^, Helper.Pivot) >= 0) then Continue;

        I := J;
        repeat
          TArray.Copy<T>(I + 1, I);
          Dec(I);
          if (Helper.Compare(Helper.Inst, I^, Helper.Pivot) >= 0) then Break;
        until (False);

        TArray.Copy<T>(I + 1, @Helper.Pivot);
      until (False);

      if (StackItem <> Pointer(@Stack[0])) then Continue;
      Exit;
    end;

    // pivot
    TArray.Copy<T>(@Helper.Pivot, SortItemPivot<T>(I, J));

    // quick sort
    Dec(J);
    Dec(I);
    repeat
      Inc(J, 2);

      repeat
        Inc(I);
        if (Helper.Compare(Helper.Inst, Helper.Pivot, I^) >= 0) then Break;
      until (False);

      repeat
        Dec(J);
        if (Helper.Compare(Helper.Inst, J^, Helper.Pivot) >= 0) then Break;
      until (False);

      if (I <= J) then
      begin
        // TArray.Exchange<T>(I, J);
        case SizeOf(T) of
          0: ;
          1:
          begin
            Temp1 := PLMemory(I).LBytes[0];
            PLMemory(I).LBytes[0] := PRMemory(J).RBytes[0];
            PRMemory(J).RBytes[0] := Temp1;
          end;
          2:
          begin
            Temp2 := PLMemory(I).LWords[0];
            PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
            PRMemory(J).RWords[0] := Temp2;
          end;
          3:
          begin
            Temp2 := PLMemory(I).LWords[0];
            PLMemory(I).LWords[0] := PRMemory(J).RWords[0];
            PRMemory(J).RWords[0] := Temp2;

            Temp1 := PLMemory(I).LBytes[2];
            PLMemory(I).LBytes[2] := PRMemory(J).RBytes[2];
            PRMemory(J).RBytes[2] := Temp1;
          end;
          4..7:
          begin
            Temp4 := PLMemory(I).LCardinals[0];
            PLMemory(I).LCardinals[0] := PRMemory(J).RCardinals[0];
            PRMemory(J).RCardinals[0] := Temp4;

            case SizeOf(T) of
              5:
              begin
                Temp1 := PLMemory(I).LBytes[4];
                PLMemory(I).LBytes[4] := PRMemory(J).RBytes[4];
                PRMemory(J).RBytes[4] := Temp1;
              end;
              6:
              begin
                Temp2 := PLMemory(I).LWords[2];
                PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
                PRMemory(J).RWords[2] := Temp2;
              end;
              7:
              begin
                Temp2 := PLMemory(I).LWords[2];
                PLMemory(I).LWords[2] := PRMemory(J).RWords[2];
                PRMemory(J).RWords[2] := Temp2;
                Temp1 := PLMemory(I).LBytes[6];
                PLMemory(I).LBytes[6] := PRMemory(J).RBytes[6];
                PRMemory(J).RBytes[6] := Temp1;
              end;
            end;
          end;
          8..16:
          begin
            TempNative := PLMemory(I).LNatives[0];
            PLMemory(I).LNatives[0] := PRMemory(J).RNatives[0];
            PRMemory(J).RNatives[0] := TempNative;

            if (SizeOf(T) >= SizeOf(NativeUInt) * 2) then
            begin
              TempNative := PLMemory(I).LNatives[1];
              PLMemory(I).LNatives[1] := PRMemory(J).RNatives[1];
              PRMemory(J).RNatives[1] := TempNative;
            end;

            if (SizeOf(T) >= SizeOf(NativeUInt) * 3) then
            begin
              TempNative := PLMemory(I).LNatives[2];
              PLMemory(I).LNatives[2] := PRMemory(J).RNatives[2];
              PRMemory(J).RNatives[2] := TempNative;
            end;

            if (SizeOf(T)  = SizeOf(NativeUInt) * 4) then
            begin
              TempNative := PLMemory(I).LNatives[3];
              PLMemory(I).LNatives[3] := PRMemory(J).RNatives[3];
              PRMemory(J).RNatives[3] := TempNative;
            end;

            {$ifdef LARGEINT}
            case SizeOf(T) of
              12, 13, 14, 15:
              begin
                Temp4 := PLMemory(I).LCardinals[2];
                PLMemory(I).LCardinals[2] := PRMemory(J).RCardinals[2];
                PRMemory(J).RCardinals[2] := Temp4;
              end;
            end;
            {$endif}

            case SizeOf(T) of
              9:
              begin
                Temp1 := PLMemory(I).LBytes[8];
                PLMemory(I).LBytes[8] := PRMemory(J).RBytes[8];
                PRMemory(J).RBytes[8] := Temp1;
              end;
              10:
              begin
                Temp2 := PLMemory(I).LWords[4];
                PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
                PRMemory(J).RWords[4] := Temp2;
              end;
              11:
              begin
                Temp2 := PLMemory(I).LWords[4];
                PLMemory(I).LWords[4] := PRMemory(J).RWords[4];
                PRMemory(J).RWords[4] := Temp2;
                Temp1 := PLMemory(I).LBytes[10];
                PLMemory(I).LBytes[10] := PRMemory(J).RBytes[10];
                PRMemory(J).RBytes[10] := Temp1;
              end;
              13:
              begin
                Temp2 := PLMemory(I).LWords[5];
                PLMemory(I).LWords[5] := PRMemory(J).RWords[5];
                PRMemory(J).RWords[5] := Temp2;
                Temp1 := PLMemory(I).LBytes[12];
                PLMemory(I).LBytes[12] := PRMemory(J).RBytes[12];
                PRMemory(J).RBytes[12] := Temp1;
              end;
              14:
              begin
                Temp2 := PLMemory(I).LWords[6];
                PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
                PRMemory(J).RWords[6] := Temp2;
              end;
              15:
              begin
                Temp2 := PLMemory(I).LWords[6];
                PLMemory(I).LWords[6] := PRMemory(J).RWords[6];
                PRMemory(J).RWords[6] := Temp2;
                Temp1 := PLMemory(I).LBytes[14];
                PLMemory(I).LBytes[14] := PRMemory(J).RBytes[14];
                PRMemory(J).RBytes[14] := Temp1;
              end;
            end;
          end;
        else
          Index := 0;
          repeat
            TempNative := PLMemory(I).LNatives[Index];
            PLMemory(I).LNatives[Index] := PRMemory(J).RNatives[Index];
            PRMemory(J).RNatives[Index] := TempNative;
            Inc(Index);
          until (Index = SizeOf(T) div SizeOf(NativeUInt));

          if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
          begin
            {$ifdef LARGEINT}
            if (SizeOf(T) and 4 <> 0) then
            begin
              Index := SizeOf(T) div SizeOf(Cardinal) - 1;
              Temp4 := PLMemory(I).LCardinals[Index];
              PLMemory(I).LCardinals[Index] := PRMemory(J).RCardinals[Index];
              PRMemory(J).RCardinals[Index] := Temp4;
            end;
            {$endif}

            if (SizeOf(T) and 2 <> 0) then
            begin
              Index := SizeOf(T) div SizeOf(Word) - 1;
              Temp2 := PLMemory(I).LWords[Index];
              PLMemory(I).LWords[Index] := PRMemory(J).RWords[Index];
              PRMemory(J).RWords[Index] := Temp2;
            end;

            if (SizeOf(T) and 1 <> 0) then
            begin
              Index := SizeOf(T) div SizeOf(Byte) - 1;
              Temp1 := PLMemory(I).LBytes[Index];
              PLMemory(I).LBytes[Index] := PRMemory(J).RBytes[Index];
              PRMemory(J).RBytes[Index] := Temp1;
            end;
          end;
        end;

        Dec(J, 2);
        if (I <= J) then Continue;
        Inc(I);
        Inc(J);
      end;
      Break;
    until (False);

    // next iteration
    StackItem := SortItemNext<T>(StackItem, I, J);
    if (NativeInt(StackItem) >= 0) then
    begin
      Inc(StackItem);
      Continue;
    end;
    Dec(NativeInt(StackItem), HIGH_NATIVE_BIT);
    if (StackItem <> Pointer(@Stack[0])) then Continue;
    Break;
  until (False);
end;

class procedure TArray.Sort<T>(var Values: T; const Count: Integer);
{$ifdef SMARTGENERICS}
var
  TypeData: PTypeData;
  PivotBig: ^T;
begin
  if (Count <= 1) then Exit;

  if (GetTypeKind(T) in [tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64]) or
    ((GetTypeKind(T) = tkFloat) and (SizeOf(T) = 8)) then
  begin
    TypeData := Pointer(TypeInfo(T));
    Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);
  end;

  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<T>.Weak) then
  {$else}
  if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
  if (TRAIIHelper<T>.FOptions.FWeak) then
  {$endif}
  begin
    TArray.Sort<T>(Values, Count, IComparer<T>(InterfaceDefaults.TDefaultComparer<T>.Create));
  end else
  {$endif}
  case GetTypeKind(T) of
    tkInteger, tkEnumeration, tkChar, tkWChar:
    case SizeOf(T) of
      1:
      begin
        case TypeData.OrdType of
          otSByte: SortSigneds<{$if CompilerVersion = 28}T{$else}ShortInt{$ifend}>(@Values, Count);
          otUByte: SortUnsigneds<{$if CompilerVersion = 28}T{$else}Byte{$ifend}>(@Values, Count);
        end;
      end;
      2:
      begin
        case TypeData.OrdType of
          otSWord: SortSigneds<{$if CompilerVersion = 28}T{$else}SmallInt{$ifend}>(@Values, Count);
          otUWord: SortUnsigneds<{$if CompilerVersion = 28}T{$else}Word{$ifend}>(@Values, Count);
        end;
      end;
      4:
      begin
        case TypeData.OrdType of
          otSLong: SortSigneds<{$if CompilerVersion = 28}T{$else}Integer{$ifend}>(@Values, Count);
          otULong: SortUnsigneds<{$if CompilerVersion = 28}T{$else}Cardinal{$ifend}>(@Values, Count);
        end;
      end;
    end;
    tkInt64:
    begin
      if (TypeData.MaxInt64Value > TypeData.MinInt64Value) then
      begin
        SortSigneds<{$if CompilerVersion = 28}T{$else}Int64{$ifend}>(@Values, Count);
      end else
      begin
        SortUnsigneds<{$if CompilerVersion = 28}T{$else}UInt64{$ifend}>(@Values, Count);
      end;
    end;
    tkClass, tkInterface, tkClassRef, tkPointer, tkProcedure:
    begin
      {$ifdef LARGEINT}
        SortUnsigneds<{$if CompilerVersion = 28}T{$else}UInt64{$ifend}>(@Values, Count);
      {$else .SMALLINT}
        SortUnsigneds<{$if CompilerVersion = 28}T{$else}Cardinal{$ifend}>(@Values, Count);
      {$endif}
    end;
    tkFloat:
    case SizeOf(T) of
       4: SortFloats<{$if CompilerVersion = 28}T{$else}Single{$ifend}>(@Values, Count);
      10: SortFloats<{$if CompilerVersion = 28}T{$else}Extended{$ifend}>(@Values, Count);
    else
      if (TypeData.FloatType = ftDouble) then
      begin
        SortFloats<{$if CompilerVersion = 28}T{$else}Double{$ifend}>(@Values, Count);
      end else
      begin
        SortSigneds<{$if CompilerVersion = 28}T{$else}Int64{$ifend}>(@Values, Count);
      end;
    end;
    tkVariant:
    begin
      {$if CompilerVersion = 28}
        TArray.Sort<T>(Values, Count, IComparer<T>(InterfaceDefaults.TDefaultComparer<Variant>.Create));
      {$else}
        TArray.Sort<Variant>(PVariant(@Values)^, Count, IComparer<Variant>(InterfaceDefaults.TDefaultComparer<Variant>.Create));
      {$ifend}
    end;
    {$if CompilerVersion = 28}
      tkMethod, tkString, tkLString, tkWString, tkUString:
      begin
        if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
          InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
        SortBinaries<T>(@Values, Count, T(nil^));
      end;
    {$else}
      tkMethod:
      begin
        SortBinaries<InterfaceDefaults.TMethodPtr>(@Values, Count, InterfaceDefaults.TMethodPtr(nil^));
      end;
      tkString:
      begin
        SortBinaries<T>(@Values, Count, T(nil^));
      end;
      tkLString:
      begin
        {$ifdef NEXTGEN}
          SortBinaries<T>(@Values, Count, T(nil^));
        {$else}
          SortBinaries<AnsiString>(@Values, Count, AnsiString(nil^));
        {$endif}
      end;
      {$ifdef MSWINDOWS}
      tkWString:
      begin
        SortBinaries<WideString>(@Values, Count, WideString(nil^));
      end;
      {$else}
      tkWString,
      {$endif}
      tkUString:
      begin
        SortBinaries<UnicodeString>(@Values, Count, UnicodeString(nil^));
      end;
      tkDynArray:
      begin
        if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
          InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
        SortBinaries<T>(@Values, Count, T(nil^));
      end;
    {$ifend}
  else
    // binary
    if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
      InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
    case SizeOf(T) of
      0: ;
      1: SortUnsigneds<{$if CompilerVersion = 28}T{$else}Byte{$ifend}>(@Values, Count);
      2..BUFFER_SIZE: SortBinaries<T>(@Values, Count, T(nil^));
    else
      GetMem(PivotBig, SizeOf(T));
      try
        SortBinaries<T>(@Values, Count, PivotBig^);
      finally
        FreeMem(PivotBig);
      end;
    end;
  end;
end;
{$else}
begin
  TArray.Sort<T>(Values, Count, IComparer<T>(InterfaceDefaults.TDefaultComparer<T>.Create));
end;
{$endif}

class procedure TArray.Sort<T>(var Values: T; const Count: Integer; const Comparer: IComparer<T>);
var
  HelperBuffer: array[0..BUFFER_SIZE - 1] of Byte;
  Helper: ^TSortHelper<T>;
begin
  if (Count <= 1) then Exit;

  Helper := Pointer(@HelperBuffer);
  if (SizeOf(TSortHelper<T>) > SizeOf(HelperBuffer)) then GetMem(Helper, SizeOf(TSortHelper<T>));
  try
    Helper^.Init(Comparer);

    {$ifdef WEAKREF}
    {$ifdef SMARTGENERICS}
    if (TRAIIHelper<T>.Weak) then
    {$else}
    if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
    if (TRAIIHelper<T>.FOptions.FWeak) then
    {$endif}
    begin
      System.Initialize(Helper.Temp);
      try
        TArray.WeakSortUniversals<T>(@Values, Count, Helper^);
      finally
        System.Finalize(Helper.Temp);
      end;
    end else
    {$endif}
    begin
      TArray.SortUniversals<T>(@Values, Count, Helper^);
    end;
  finally
    if (Helper <> Pointer(@HelperBuffer)) then
      FreeMem(Helper);
  end;
end;

class procedure TArray.Sort<T>(var Values: T; const Count: Integer; const Comparison: TComparison<T>);
var
  HelperBuffer: array[0..BUFFER_SIZE - 1] of Byte;
  Helper: ^TSortHelper<T>;
begin
  if (Count <= 1) then Exit;

  Helper := Pointer(@HelperBuffer);
  if (SizeOf(TSortHelper<T>) > SizeOf(HelperBuffer)) then GetMem(Helper, SizeOf(TSortHelper<T>));
  try
    Helper^.Init(Comparison);

    {$ifdef WEAKREF}
    {$ifdef SMARTGENERICS}
    if (TRAIIHelper<T>.Weak) then
    {$else}
    if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
    if (TRAIIHelper<T>.FOptions.FWeak) then
    {$endif}
    begin
      System.Initialize(Helper.Temp);
      try
        TArray.WeakSortUniversals<T>(@Values, Count, Helper^);
      finally
        System.Finalize(Helper.Temp);
      end;
    end else
    {$endif}
    begin
      TArray.SortUniversals<T>(@Values, Count, Helper^);
    end;
  finally
    if (Helper <> Pointer(@HelperBuffer)) then
      FreeMem(Helper);
  end;
end;

class procedure TArray.Sort<T>(var Values: array of T);
begin
  if (High(Values) > 0) then
    Sort<T>(Values[0], Length(Values));
end;

class procedure TArray.Sort<T>(var Values: array of T; const Comparer: IComparer<T>);
begin
  if (High(Values) > 0) then
   Sort<T>(Values[0], Length(Values), Comparer);
end;

class procedure TArray.Sort<T>(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer);
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Count <= 1 then
    Exit;

  Sort<T>(Values[Index], Count, Comparer);
end;

class procedure TArray.Sort<T>(var Values: array of T; const Comparison: TComparison<T>);
begin
  if (High(Values) > 0) then
   Sort<T>(Values[0], Length(Values), Comparison);
end;

class procedure TArray.Sort<T>(var Values: array of T; Index, Count: Integer; const Comparison: TComparison<T>);
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Count <= 1 then
    Exit;

  Sort<T>(Values[Index], Count, Comparison);
end;

class procedure TArray.SortDescending<T>(var Values: T; const Count: Integer);
{$ifdef SMARTGENERICS}
var
  TypeData: PTypeData;
  PivotBig: ^T;
begin
  if (Count <= 1) then Exit;

  if (GetTypeKind(T) in [tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64]) or
    ((GetTypeKind(T) = tkFloat) and (SizeOf(T) = 8)) then
  begin
    TypeData := Pointer(TypeInfo(T));
    Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);
  end;

  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<T>.Weak) then
  {$else}
  if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
  if (TRAIIHelper<T>.FOptions.FWeak) then
  {$endif}
  begin
    TArray.SortDescending<T>(Values, Count, IComparer<T>(InterfaceDefaults.TDefaultComparer<T>.Create));
  end else
  {$endif}
  case GetTypeKind(T) of
    tkInteger, tkEnumeration, tkChar, tkWChar:
    case SizeOf(T) of
      1:
      begin
        case TypeData.OrdType of
          otSByte: SortDescendingSigneds<{$if CompilerVersion = 28}T{$else}ShortInt{$ifend}>(@Values, Count);
          otUByte: SortDescendingUnsigneds<{$if CompilerVersion = 28}T{$else}Byte{$ifend}>(@Values, Count);
        end;
      end;
      2:
      begin
        case TypeData.OrdType of
          otSWord: SortDescendingSigneds<{$if CompilerVersion = 28}T{$else}SmallInt{$ifend}>(@Values, Count);
          otUWord: SortDescendingUnsigneds<{$if CompilerVersion = 28}T{$else}Word{$ifend}>(@Values, Count);
        end;
      end;
      4:
      begin
        case TypeData.OrdType of
          otSLong: SortDescendingSigneds<{$if CompilerVersion = 28}T{$else}Integer{$ifend}>(@Values, Count);
          otULong: SortDescendingUnsigneds<{$if CompilerVersion = 28}T{$else}Cardinal{$ifend}>(@Values, Count);
        end;
      end;
    end;
    tkInt64:
    begin
      if (TypeData.MaxInt64Value > TypeData.MinInt64Value) then
      begin
        SortDescendingSigneds<{$if CompilerVersion = 28}T{$else}Int64{$ifend}>(@Values, Count);
      end else
      begin
        SortDescendingUnsigneds<{$if CompilerVersion = 28}T{$else}UInt64{$ifend}>(@Values, Count);
      end;
    end;
    tkClass, tkInterface, tkClassRef, tkPointer, tkProcedure:
    begin
      {$ifdef LARGEINT}
        SortDescendingUnsigneds<{$if CompilerVersion = 28}T{$else}UInt64{$ifend}>(@Values, Count);
      {$else .SMALLINT}
        SortDescendingUnsigneds<{$if CompilerVersion = 28}T{$else}Cardinal{$ifend}>(@Values, Count);
      {$endif}
    end;
    tkFloat:
    case SizeOf(T) of
       4: SortDescendingFloats<{$if CompilerVersion = 28}T{$else}Single{$ifend}>(@Values, Count);
      10: SortDescendingFloats<{$if CompilerVersion = 28}T{$else}Extended{$ifend}>(@Values, Count);
    else
      if (TypeData.FloatType = ftDouble) then
      begin
        SortDescendingFloats<{$if CompilerVersion = 28}T{$else}Double{$ifend}>(@Values, Count);
      end else
      begin
        SortDescendingSigneds<{$if CompilerVersion = 28}T{$else}Int64{$ifend}>(@Values, Count);
      end;
    end;
    tkVariant:
    begin
      {$if CompilerVersion = 28}
        TArray.SortDescending<T>(Values, Count, IComparer<T>(InterfaceDefaults.TDefaultComparer<Variant>.Create));
      {$else}
        TArray.SortDescending<Variant>(PVariant(@Values)^, Count, IComparer<Variant>(InterfaceDefaults.TDefaultComparer<Variant>.Create));
      {$ifend}
    end;
    {$if CompilerVersion = 28}
      tkMethod, tkString, tkLString, tkWString, tkUString:
      begin
        if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
          InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
        SortDescendingBinaries<T>(@Values, Count, T(nil^));
      end;
    {$else}
      tkMethod:
      begin
        SortDescendingBinaries<InterfaceDefaults.TMethodPtr>(@Values, Count, InterfaceDefaults.TMethodPtr(nil^));
      end;
      tkString:
      begin
        SortDescendingBinaries<T>(@Values, Count, T(nil^));
      end;
      tkLString:
      begin
        {$ifdef NEXTGEN}
          SortDescendingBinaries<T>(@Values, Count, T(nil^));
        {$else}
          SortDescendingBinaries<AnsiString>(@Values, Count, AnsiString(nil^));
        {$endif}
      end;
      {$ifdef MSWINDOWS}
      tkWString:
      begin
        SortDescendingBinaries<WideString>(@Values, Count, WideString(nil^));
      end;
      {$else}
      tkWString,
      {$endif}
      tkUString:
      begin
        SortDescendingBinaries<UnicodeString>(@Values, Count, UnicodeString(nil^));
      end;
      tkDynArray:
      begin
        if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
          InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
        SortDescendingBinaries<T>(@Values, Count, T(nil^));
      end;
    {$ifend}
  else
    // binary
    if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
      InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
    case SizeOf(T) of
      0: ;
      1: SortDescendingUnsigneds<{$if CompilerVersion = 28}T{$else}Byte{$ifend}>(@Values, Count);
      2..BUFFER_SIZE: SortDescendingBinaries<T>(@Values, Count, T(nil^));
    else
      GetMem(PivotBig, SizeOf(T));
      try
        SortDescendingBinaries<T>(@Values, Count, PivotBig^);
      finally
        FreeMem(PivotBig);
      end;
    end;
  end;
end;
{$else}
begin
  TArray.SortDescending<T>(Values, Count, IComparer<T>(InterfaceDefaults.TDefaultComparer<T>.Create));
end;
{$endif}

class procedure TArray.SortDescending<T>(var Values: T; const Count: Integer; const Comparer: IComparer<T>);
var
  HelperBuffer: array[0..BUFFER_SIZE - 1] of Byte;
  Helper: ^TSortHelper<T>;
begin
  if (Count <= 1) then Exit;

  Helper := Pointer(@HelperBuffer);
  if (SizeOf(TSortHelper<T>) > SizeOf(HelperBuffer)) then GetMem(Helper, SizeOf(TSortHelper<T>));
  try
    Helper^.Init(Comparer);

    {$ifdef WEAKREF}
    {$ifdef SMARTGENERICS}
    if (TRAIIHelper<T>.Weak) then
    {$else}
    if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
    if (TRAIIHelper<T>.FOptions.FWeak) then
    {$endif}
    begin
      System.Initialize(Helper.Temp);
      try
        TArray.WeakSortDescendingUniversals<T>(@Values, Count, Helper^);
      finally
        System.Finalize(Helper.Temp);
      end;
    end else
    {$endif}
    begin
      TArray.SortDescendingUniversals<T>(@Values, Count, Helper^);
    end;
  finally
    if (Helper <> Pointer(@HelperBuffer)) then
      FreeMem(Helper);
  end;
end;

class procedure TArray.SortDescending<T>(var Values: T; const Count: Integer; const Comparison: TComparison<T>);
var
  HelperBuffer: array[0..BUFFER_SIZE - 1] of Byte;
  Helper: ^TSortHelper<T>;
begin
  if (Count <= 1) then Exit;

  Helper := Pointer(@HelperBuffer);
  if (SizeOf(TSortHelper<T>) > SizeOf(HelperBuffer)) then GetMem(Helper, SizeOf(TSortHelper<T>));
  try
    Helper^.Init(Comparison);

    {$ifdef WEAKREF}
    {$ifdef SMARTGENERICS}
    if (TRAIIHelper<T>.Weak) then
    {$else}
    if (not TRAIIHelper<T>.FCreated) then TRAIIHelper<T>.InternalCreate;
    if (TRAIIHelper<T>.FOptions.FWeak) then
    {$endif}
    begin
      System.Initialize(Helper.Temp);
      try
        TArray.WeakSortDescendingUniversals<T>(@Values, Count, Helper^);
      finally
        System.Finalize(Helper.Temp);
      end;
    end else
    {$endif}
    begin
      TArray.SortDescendingUniversals<T>(@Values, Count, Helper^);
    end;
  finally
    if (Helper <> Pointer(@HelperBuffer)) then
      FreeMem(Helper);
  end;
end;

class procedure TArray.SortDescending<T>(var Values: array of T);
begin
  if (High(Values) > 0) then
    SortDescending<T>(Values[0], Length(Values));
end;

class procedure TArray.SortDescending<T>(var Values: array of T; const Comparer: IComparer<T>);
begin
  if (High(Values) > 0) then
   SortDescending<T>(Values[0], Length(Values), Comparer);
end;

class procedure TArray.SortDescending<T>(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer);
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Count <= 1 then
    Exit;

  SortDescending<T>(Values[Index], Count, Comparer);
end;

class procedure TArray.SortDescending<T>(var Values: array of T; const Comparison: TComparison<T>);
begin
  if (High(Values) > 0) then
   SortDescending<T>(Values[0], Length(Values), Comparison);
end;

class procedure TArray.SortDescending<T>(var Values: array of T; Index, Count: Integer; const Comparison: TComparison<T>);
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Count <= 1 then
    Exit;

  SortDescending<T>(Values[Index], Count, Comparison);
end;

{$ifdef SMARTGENERICS}
class function TArray.SearchSigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt;
label
  middle_init, not_found;
type
  PArray1 = ^HugeShortIntArray;
  PArray2 = ^HugeSmallIntArray;
  PArray4 = ^HugeIntegerArray;
  PArray8 = ^{$ifdef LARGEINT}HugeInt64Array{$else .SMALLINT}HugeTPointArray{$endif};
var
  Item1: ShortInt;
  Item2: SmallInt;
  Item4: Integer;
  {$ifdef LARGEINT}
    Item8: Int64;
  {$else .SMALLINT}
    Item8Low: Cardinal;
    Item8High, Buffer8High: Integer;
  {$endif}
  Left, Right, Middle: NativeInt;
begin
  case SizeOf(T) of
    1: Byte(Item1) := PByte(Item)^;
    2: Word(Item2) := PWord(Item)^;
    4: Integer(Item4) := PInteger(Item)^;
  else
    {$ifdef LARGEINT}
      Int64(Item8) := PInt64(Item)^;
    {$else .SMALLINT}
      Item8Low := PPoint(Item).X;
      Item8High := PPoint(Item).Y;
    {$endif}
  end;

  Middle := -1;
  Right := Count + (-1);
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case SizeOf(T) of
      1: if (PArray1(Values)[Middle] < Item1) then Continue;
      2: if (PArray2(Values)[Middle] < Item2) then Continue;
      4: if (PArray4(Values)[Middle] < Item4) then Continue;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Middle] < Item8) then Continue;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Middle].X;
        Dec(NativeUInt(Values), SizeOf(Integer));
        if (Buffer8High < Item8High) then Continue;
        if (Buffer8High = Item8High) then
        begin
          if (Cardinal(PArray8(Values)[Middle].X) < Item8Low) then Continue;
        end;
      {$endif}
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    case SizeOf(T) of
      1: if (PArray1(Values)[Left] <> Item1) then goto not_found;
      2: if (PArray2(Values)[Left] <> Item2) then goto not_found;
      4: if (PArray4(Values)[Left] <> Item4) then goto not_found;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Left] <> Item8) then goto not_found;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Dec(Item8High, PArray8(Values)[Left].X);
        Dec(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Left].X;
        Dec(Buffer8High, Item8Low);
        if (Buffer8High or Item8High <> 0) then goto not_found;
      {$endif}
    end;
  end else
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchDescendingSigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt;
label
  middle_init, not_found;
type
  PArray1 = ^HugeShortIntArray;
  PArray2 = ^HugeSmallIntArray;
  PArray4 = ^HugeIntegerArray;
  PArray8 = ^{$ifdef LARGEINT}HugeInt64Array{$else .SMALLINT}HugeTPointArray{$endif};
var
  Item1: ShortInt;
  Item2: SmallInt;
  Item4: Integer;
  {$ifdef LARGEINT}
    Item8: Int64;
  {$else .SMALLINT}
    Item8Low: Cardinal;
    Item8High, Buffer8High: Integer;
  {$endif}
  Left, Right, Middle: NativeInt;
begin
  case SizeOf(T) of
    1: Byte(Item1) := PByte(Item)^;
    2: Word(Item2) := PWord(Item)^;
    4: Integer(Item4) := PInteger(Item)^;
  else
    {$ifdef LARGEINT}
      Int64(Item8) := PInt64(Item)^;
    {$else .SMALLINT}
      Item8Low := PPoint(Item).X;
      Item8High := PPoint(Item).Y;
    {$endif}
  end;

  Middle := -1;
  Right := Count + (-1);
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case SizeOf(T) of
      1: if (PArray1(Values)[Middle] > Item1) then Continue;
      2: if (PArray2(Values)[Middle] > Item2) then Continue;
      4: if (PArray4(Values)[Middle] > Item4) then Continue;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Middle] > Item8) then Continue;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Middle].X;
        Dec(NativeUInt(Values), SizeOf(Integer));
        if (Buffer8High > Item8High) then Continue;
        if (Buffer8High = Item8High) then
        begin
          if (Cardinal(PArray8(Values)[Middle].X) > Item8Low) then Continue;
        end;
      {$endif}
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    case SizeOf(T) of
      1: if (PArray1(Values)[Left] <> Item1) then goto not_found;
      2: if (PArray2(Values)[Left] <> Item2) then goto not_found;
      4: if (PArray4(Values)[Left] <> Item4) then goto not_found;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Left] <> Item8) then goto not_found;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Dec(Item8High, PArray8(Values)[Left].X);
        Dec(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Left].X;
        Dec(Buffer8High, Item8Low);
        if (Buffer8High or Item8High <> 0) then goto not_found;
      {$endif}
    end;
  end else
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchUnsigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt;
label
  middle_init, not_found;
type
  PArray1 = ^HugeByteArray;
  PArray2 = ^HugeWordArray;
  PArray4 = ^HugeCardinalArray;
  PArray8 = ^{$ifdef LARGEINT}HugeUInt64Array{$else .SMALLINT}HugeTPointArray{$endif};
var
  Item1: Byte;
  Item2: Word;
  Item4: Cardinal;
  {$ifdef LARGEINT}
    Item8: UInt64;
  {$else .SMALLINT}
    Item8Low: Cardinal;
    Item8High, Buffer8High: Cardinal;
  {$endif}
  Left, Right, Middle: NativeInt;
begin
  case SizeOf(T) of
    1: Byte(Item1) := PByte(Item)^;
    2: Word(Item2) := PWord(Item)^;
    4: Integer(Item4) := PInteger(Item)^;
  else
    {$ifdef LARGEINT}
      Int64(Item8) := PInt64(Item)^;
    {$else .SMALLINT}
      Item8Low := PPoint(Item).X;
      Item8High := PPoint(Item).Y;
    {$endif}
  end;

  Middle := -1;
  Right := Count + (-1);
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case SizeOf(T) of
      1: if (PArray1(Values)[Middle] < Item1) then Continue;
      2: if (PArray2(Values)[Middle] < Item2) then Continue;
      4: if (PArray4(Values)[Middle] < Item4) then Continue;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Middle] < Item8) then Continue;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Middle].X;
        Dec(NativeUInt(Values), SizeOf(Integer));
        if (Buffer8High < Item8High) then Continue;
        if (Buffer8High = Item8High) then
        begin
          if (Cardinal(PArray8(Values)[Middle].X) < Item8Low) then Continue;
        end;
      {$endif}
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    case SizeOf(T) of
      1: if (PArray1(Values)[Left] <> Item1) then goto not_found;
      2: if (PArray2(Values)[Left] <> Item2) then goto not_found;
      4: if (PArray4(Values)[Left] <> Item4) then goto not_found;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Left] <> Item8) then goto not_found;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Dec(Item8High, PArray8(Values)[Left].X);
        Dec(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Left].X;
        Dec(Buffer8High, Item8Low);
        if (Buffer8High or Item8High <> 0) then goto not_found;
      {$endif}
    end;
  end else
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchDescendingUnsigneds<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt;
label
  middle_init, not_found;
type
  PArray1 = ^HugeByteArray;
  PArray2 = ^HugeWordArray;
  PArray4 = ^HugeCardinalArray;
  PArray8 = ^{$ifdef LARGEINT}HugeUInt64Array{$else .SMALLINT}HugeTPointArray{$endif};
var
  Item1: Byte;
  Item2: Word;
  Item4: Cardinal;
  {$ifdef LARGEINT}
    Item8: UInt64;
  {$else .SMALLINT}
    Item8Low: Cardinal;
    Item8High, Buffer8High: Cardinal;
  {$endif}
  Left, Right, Middle: NativeInt;
begin
  case SizeOf(T) of
    1: Byte(Item1) := PByte(Item)^;
    2: Word(Item2) := PWord(Item)^;
    4: Integer(Item4) := PInteger(Item)^;
  else
    {$ifdef LARGEINT}
      Int64(Item8) := PInt64(Item)^;
    {$else .SMALLINT}
      Item8Low := PPoint(Item).X;
      Item8High := PPoint(Item).Y;
    {$endif}
  end;

  Middle := -1;
  Right := Count + (-1);
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case SizeOf(T) of
      1: if (PArray1(Values)[Middle] > Item1) then Continue;
      2: if (PArray2(Values)[Middle] > Item2) then Continue;
      4: if (PArray4(Values)[Middle] > Item4) then Continue;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Middle] > Item8) then Continue;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Middle].X;
        Dec(NativeUInt(Values), SizeOf(Integer));
        if (Buffer8High > Item8High) then Continue;
        if (Buffer8High = Item8High) then
        begin
          if (Cardinal(PArray8(Values)[Middle].X) > Item8Low) then Continue;
        end;
      {$endif}
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    case SizeOf(T) of
      1: if (PArray1(Values)[Left] <> Item1) then goto not_found;
      2: if (PArray2(Values)[Left] <> Item2) then goto not_found;
      4: if (PArray4(Values)[Left] <> Item4) then goto not_found;
    else
      {$ifdef LARGEINT}
        if (PArray8(Values)[Left] <> Item8) then goto not_found;
      {$else .SMALLINT}
        Inc(NativeUInt(Values), SizeOf(Integer));
        Dec(Item8High, PArray8(Values)[Left].X);
        Dec(NativeUInt(Values), SizeOf(Integer));
        Buffer8High := PArray8(Values)[Left].X;
        Dec(Buffer8High, Item8Low);
        if (Buffer8High or Item8High <> 0) then goto not_found;
      {$endif}
    end;
  end else
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchFloats<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt;
label
  middle_init, not_found;
type
  PArray4 = ^HugeSingleArray;
  PArray8 = ^HugeDoubleArray;
  PArrayE = ^HugeExtendedArray;
var
  Item4: {$ifdef CPUX86}Extended{$else}Single{$endif};
  Item8: {$ifdef CPUX86}Extended{$else}Double{$endif};
  ItemE: Extended;
  Left, Right, Middle: NativeInt;
begin
  case SizeOf(T) of
    4: Item4 := PSingle(Item)^;
    8: Item8 := PDouble(Item)^;
  else
    ItemE := PExtended(Item)^;
  end;

  Middle := -1;
  Right := Count + (-1);
  repeat
    Inc(Middle);
    Left := Middle;
    if (Middle > Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case SizeOf(T) of
      4: if (PArray4(Values)[Middle] < Item4) then Continue;
      8: if (PArray8(Values)[Middle] < Item8) then Continue;
    else
      if (PArrayE(Values)[Middle] < ItemE) then Continue;
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    case SizeOf(T) of
      4: if (PArray4(Values)[Left] <> Item4) then goto not_found;
      8: if (PArray8(Values)[Left] <> Item8) then goto not_found;
    else
      if (PArrayE(Values)[Left] <> ItemE) then goto not_found;
    end;
  end else
  if (Left >= Count) then
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchDescendingFloats<T>(Values: Pointer; Count: NativeInt; Item: Pointer): NativeInt;
label
  middle_init, not_found;
type
  PArray4 = ^HugeSingleArray;
  PArray8 = ^HugeDoubleArray;
  PArrayE = ^HugeExtendedArray;
var
  Item4: {$ifdef CPUX86}Extended{$else}Single{$endif};
  Item8: {$ifdef CPUX86}Extended{$else}Double{$endif};
  ItemE: Extended;
  Left, Right, Middle: NativeInt;
begin
  case SizeOf(T) of
    4: Item4 := PSingle(Item)^;
    8: Item8 := PDouble(Item)^;
  else
    ItemE := PExtended(Item)^;
  end;

  Middle := -1;
  Right := Count + (-1);
  repeat
    Inc(Middle);
    Left := Middle;
    if (Middle > Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case SizeOf(T) of
      4: if (PArray4(Values)[Middle] > Item4) then Continue;
      8: if (PArray8(Values)[Middle] > Item8) then Continue;
    else
      if (PArrayE(Values)[Middle] > ItemE) then Continue;
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    case SizeOf(T) of
      4: if (PArray4(Values)[Left] <> Item4) then goto not_found;
      8: if (PArray8(Values)[Left] <> Item8) then goto not_found;
    else
      if (PArrayE(Values)[Left] <> ItemE) then goto not_found;
    end;
  end else
  if (Left >= Count) then
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchBinaries<T>(Values: Pointer; Count: NativeInt; const Item: T): NativeInt;
label
  middle_init, not_found;
var
  Left, Right, Middle: NativeInt;
  X, Y: NativeUInt;
  BufferMiddle, BufferLeft: Pointer;
  Cmp: Integer;
  Stored: TInternalSearchStored;
begin
  Middle := -1;
  Right := Count + (-1);
  X := TArray.SortBinaryMarker<T>(@Item);
  Stored.X := X;
  if (GetTypeKind(T) in [tkMethod, tkLString, tkWString, tkUString, tkDynArray]) then
  begin
    Stored.ItemPtr := PPointer(@Item)^;
  end;
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case GetTypeKind(T) of
      tkMethod:
      begin
        Y := Middle;
        Y := Y shl {$ifdef LARGEINT}4{$else .SMALLINT}3{$endif};
        Inc(Y, NativeUInt(Values));
        Y := NativeUInt(PMethod(Y).Data);
      end;
      tkLString, tkWString, tkUString, tkDynArray:
      begin
        Y := PNativeUInt(TRAIIHelper<T>.P(Values) + Middle)^;
        if (Y <> 0) then
        case GetTypeKind(T) of
          tkLString:
          begin
            Y := PWord(Y)^;
            Y := Swap(Y);
          end;
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            Dec(Y, SizeOf(Integer));
            if (PInteger(Y)^ = 0) then
            begin
              Y := 0;
            end else
            begin
              Inc(Y, SizeOf(Integer));
              Y := PCardinal(Y)^;
              Y := Cardinal((Y shl 16) + (Y shr 16));
            end;
          end;
          {$else}
          tkWString,
          {$endif}
          tkUString:
          begin
            Y := PCardinal(Y)^;
            Y := Cardinal((Y shl 16) + (Y shr 16));
          end;
          tkDynArray:
          begin
            Y := PByte(Y)^;
          end;
        end;
      end;
    else
      Y := TArray.SortBinaryMarker<T>(TRAIIHelper<T>.P(Values) + Middle);
    end;

    if (Y < X) then Continue;
    if (Y = X) then
    begin
      if (GetTypeKind(T) = tkMethod) then
      begin
        Y := Middle;
        Y := Y shl {$ifdef LARGEINT}4{$else .SMALLINT}3{$endif};
        Inc(Y, NativeInt(Values));
        Y := NativeUInt(PMethod(Y).Data);
        if (Y < NativeUInt(Stored.ItemPtr)) then Continue;
      end else
      begin
        {$if CompilerVersion = 28}
          Cmp := IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
            (TRAIIHelper<T>.P(Values) + Middle)^, Item);
        {$else}
          if (GetTypeKind(T) = tkString) then
          begin
            Cmp := InterfaceDefaults.Compare_OStr(nil, Pointer(TRAIIHelper<T>.P(Values) + Middle), Pointer(@Item));
          end else
          if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
          begin
            BufferMiddle := PPointer(TRAIIHelper<T>.P(Values) + Middle)^;
            Cmp := 0;
            if (BufferMiddle <> Stored.ItemPtr) then
            begin
              case GetTypeKind(T) of
                tkLString: Cmp := InterfaceDefaults.Compare_LStr(nil, BufferMiddle, Stored.ItemPtr);
                tkWString: Cmp := InterfaceDefaults.Compare_WStr(nil, BufferMiddle, Stored.ItemPtr);
                tkUString: Cmp := InterfaceDefaults.Compare_UStr(nil, BufferMiddle, Stored.ItemPtr);
               tkDynArray: Cmp := InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, BufferMiddle, Stored.ItemPtr);
              end;
            end;
          end else
          case SizeOf(T) of
            0..SizeOf(Cardinal): Cmp := 0;
            {$ifdef LARGEINT}
            SizeOf(Int64): Cmp := InterfaceDefaults.Compare_Bin8(nil, PInt64(TRAIIHelper<T>.P(Values) + Middle)^, PInt64(@Item)^);
            {$endif}
          else
            Cmp := InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(TRAIIHelper<T>.P(Values) + Middle), Pointer(@Item));
          end;
        {$ifend}

        X := Stored.X;
        if (Cmp < 0) then Continue;
      end;
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    BufferLeft := TRAIIHelper<T>.P(Values) + Left;
    Y := TArray.SortBinaryMarker<T>(BufferLeft);
    if (Y <> X) then goto not_found;
    if (GetTypeKind(T) = tkMethod) then
    begin
      if (PNativeUInt(BufferLeft)^ <> NativeUInt(Stored.ItemPtr)) then goto not_found;
    end else
    begin
      {$if CompilerVersion = 28}
        Cmp := IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
          TRAIIHelper<T>.P(BufferLeft)^, Item);
      {$else}
        if (GetTypeKind(T) = tkString) then
        begin
          Cmp := InterfaceDefaults.Compare_OStr(nil, BufferLeft, Pointer(@Item));
        end else
        if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
        begin
          BufferLeft := PPointer(BufferLeft)^;
          Cmp := 0;
          if (BufferLeft <> Stored.ItemPtr) then
          begin
            case GetTypeKind(T) of
              tkLString: Cmp := InterfaceDefaults.Compare_LStr(nil, BufferLeft, Stored.ItemPtr);
              tkWString: Cmp := InterfaceDefaults.Compare_WStr(nil, BufferLeft, Stored.ItemPtr);
              tkUString: Cmp := InterfaceDefaults.Compare_UStr(nil, BufferLeft, Stored.ItemPtr);
             tkDynArray: Cmp := InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, BufferLeft, Stored.ItemPtr);
            end;
          end;
        end else
        case SizeOf(T) of
          0..SizeOf(Cardinal): Cmp := 0;
          {$ifdef LARGEINT}
          SizeOf(Int64): Cmp := InterfaceDefaults.Compare_Bin8(nil, PInt64(BufferLeft)^, PInt64(@Item)^);
          {$endif}
        else
          Cmp := InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance, BufferLeft, Pointer(@Item));
        end;
      {$ifend}

      if (Cmp <> 0) then goto not_found;
    end;
  end else
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchDescendingBinaries<T>(Values: Pointer; Count: NativeInt; const Item: T): NativeInt;
label
  middle_init, not_found;
var
  Left, Right, Middle: NativeInt;
  X, Y: NativeUInt;
  BufferMiddle, BufferLeft: Pointer;
  Cmp: Integer;
  Stored: TInternalSearchStored;
begin
  Middle := -1;
  Right := Count + (-1);
  X := TArray.SortBinaryMarker<T>(@Item);
  Stored.X := X;
  if (GetTypeKind(T) in [tkMethod, tkLString, tkWString, tkUString, tkDynArray]) then
  begin
    Stored.ItemPtr := PPointer(@Item)^;
  end;
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  middle_init:
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    case GetTypeKind(T) of
      tkMethod:
      begin
        Y := Middle;
        Y := Y shl {$ifdef LARGEINT}4{$else .SMALLINT}3{$endif};
        Inc(Y, NativeUInt(Values));
        Y := NativeUInt(PMethod(Y).Data);
      end;
      tkLString, tkWString, tkUString, tkDynArray:
      begin
        Y := PNativeUInt(TRAIIHelper<T>.P(Values) + Middle)^;
        if (Y <> 0) then
        case GetTypeKind(T) of
          tkLString:
          begin
            Y := PWord(Y)^;
            Y := Swap(Y);
          end;
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            Dec(Y, SizeOf(Integer));
            if (PInteger(Y)^ = 0) then
            begin
              Y := 0;
            end else
            begin
              Inc(Y, SizeOf(Integer));
              Y := PCardinal(Y)^;
              Y := Cardinal((Y shl 16) + (Y shr 16));
            end;
          end;
          {$else}
          tkWString,
          {$endif}
          tkUString:
          begin
            Y := PCardinal(Y)^;
            Y := Cardinal((Y shl 16) + (Y shr 16));
          end;
          tkDynArray:
          begin
            Y := PByte(Y)^;
          end;
        end;
      end;
    else
      Y := TArray.SortBinaryMarker<T>(TRAIIHelper<T>.P(Values) + Middle);
    end;

    if (Y > X) then Continue;
    if (Y = X) then
    begin
      if (GetTypeKind(T) = tkMethod) then
      begin
        Y := Middle;
        Y := Y shl {$ifdef LARGEINT}4{$else .SMALLINT}3{$endif};
        Inc(Y, NativeInt(Values));
        Y := NativeUInt(PMethod(Y).Data);
        if (Y > NativeUInt(Stored.ItemPtr)) then Continue;
      end else
      begin
        {$if CompilerVersion = 28}
          Cmp := IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
            (TRAIIHelper<T>.P(Values) + Middle)^, Item);
        {$else}
          if (GetTypeKind(T) = tkString) then
          begin
            Cmp := InterfaceDefaults.Compare_OStr(nil, Pointer(TRAIIHelper<T>.P(Values) + Middle), Pointer(@Item));
          end else
          if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
          begin
            BufferMiddle := PPointer(TRAIIHelper<T>.P(Values) + Middle)^;
            Cmp := 0;
            if (BufferMiddle <> Stored.ItemPtr) then
            begin
              case GetTypeKind(T) of
                tkLString: Cmp := InterfaceDefaults.Compare_LStr(nil, BufferMiddle, Stored.ItemPtr);
                tkWString: Cmp := InterfaceDefaults.Compare_WStr(nil, BufferMiddle, Stored.ItemPtr);
                tkUString: Cmp := InterfaceDefaults.Compare_UStr(nil, BufferMiddle, Stored.ItemPtr);
               tkDynArray: Cmp := InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, BufferMiddle, Stored.ItemPtr);
              end;
            end;
          end else
          case SizeOf(T) of
            0..SizeOf(Cardinal): Cmp := 0;
            {$ifdef LARGEINT}
            SizeOf(Int64): Cmp := InterfaceDefaults.Compare_Bin8(nil, PInt64(TRAIIHelper<T>.P(Values) + Middle)^, PInt64(@Item)^);
            {$endif}
          else
            Cmp := InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance,
              Pointer(TRAIIHelper<T>.P(Values) + Middle), Pointer(@Item));
          end;
        {$ifend}

        X := Stored.X;
        if (Cmp > 0) then Continue;
      end;
    end;

    Right := Middle + (-1);
    if (not (Left > Right)) then goto middle_init;
    Break;
  until (False);

  if (Left < Count) then
  begin
    BufferLeft := TRAIIHelper<T>.P(Values) + Left;
    Y := TArray.SortBinaryMarker<T>(BufferLeft);
    if (Y <> X) then goto not_found;
    if (GetTypeKind(T) = tkMethod) then
    begin
      if (PNativeUInt(BufferLeft)^ <> NativeUInt(Stored.ItemPtr)) then goto not_found;
    end else
    begin
      {$if CompilerVersion = 28}
        Cmp := IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance).Compare(
          TRAIIHelper<T>.P(BufferLeft)^, Item);
      {$else}
        if (GetTypeKind(T) = tkString) then
        begin
          Cmp := InterfaceDefaults.Compare_OStr(nil, BufferLeft, Pointer(@Item));
        end else
        if (GetTypeKind(T) in [tkLString, tkWString, tkUString, tkDynArray]) then
        begin
          BufferLeft := PPointer(BufferLeft)^;
          Cmp := 0;
          if (BufferLeft <> Stored.ItemPtr) then
          begin
            case GetTypeKind(T) of
              tkLString: Cmp := InterfaceDefaults.Compare_LStr(nil, BufferLeft, Stored.ItemPtr);
              tkWString: Cmp := InterfaceDefaults.Compare_WStr(nil, BufferLeft, Stored.ItemPtr);
              tkUString: Cmp := InterfaceDefaults.Compare_UStr(nil, BufferLeft, Stored.ItemPtr);
             tkDynArray: Cmp := InterfaceDefaults.Compare_Dyn(InterfaceDefaults.TDefaultComparer<T>.Instance, BufferLeft, Stored.ItemPtr);
            end;
          end;
        end else
        case SizeOf(T) of
          0..SizeOf(Cardinal): Cmp := 0;
          {$ifdef LARGEINT}
          SizeOf(Int64): Cmp := InterfaceDefaults.Compare_Bin8(nil, PInt64(BufferLeft)^, PInt64(@Item)^);
          {$endif}
        else
          Cmp := InterfaceDefaults.Compare_Bin(InterfaceDefaults.TDefaultComparer<T>.Instance, BufferLeft, Pointer(@Item));
        end;
      {$ifend}

      if (Cmp <> 0) then goto not_found;
    end;
  end else
  begin
  not_found:
    Left := not Left;
  end;

  Result := Left;
end;
{$endif .SMARTGENERICS}

class function TArray.SearchUniversals<T>(Values: Pointer; const Helper: TSearchHelper; const Item: T): NativeInt;
{$ifdef SMARTGENERICS}
label
  middle_init;
{$endif}
var
  Left, Right, Middle: NativeInt;
  Stored: TInternalSearchStored<T>;
begin
  Stored.Inst := Helper.Comparer;
  Stored.Compare := PPointer(PNativeUInt(Stored.Inst)^ + 3 * SizeOf(Pointer))^;
  Stored.Count := Helper.Count;

  Middle := -1;
  Right := Stored.Count + (-1);
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  {$ifdef SMARTGENERICS}
  middle_init:
  {$endif}
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    if (Stored.Compare(Stored.Inst, TRAIIHelper<T>.PArrayT(Values)[Middle], Item) < 0) then Continue;

    Right := Middle + (-1);
    if (not (Left > Right)) then
    begin
      {$ifdef SMARTGENERICS}
        goto middle_init;
      {$else}
        Middle := Left + (-1);
      {$endif}
    end else
    begin
      Break;
    end;
  until (False);

  if (Left >= Stored.Count) or
    (Stored.Compare(Stored.Inst, TRAIIHelper<T>.PArrayT(Values)[Left], Item) <> 0) then
  begin
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.SearchDescendingUniversals<T>(Values: Pointer; const Helper: TSearchHelper; const Item: T): NativeInt;
{$ifdef SMARTGENERICS}
label
  middle_init;
{$endif}
var
  Left, Right, Middle: NativeInt;
  Stored: TInternalSearchStored<T>;
begin
  Stored.Inst := Helper.Comparer;
  Stored.Compare := PPointer(PNativeUInt(Stored.Inst)^ + 3 * SizeOf(Pointer))^;
  Stored.Count := Helper.Count;

  Middle := -1;
  Right := Stored.Count + (-1);
  repeat
    Left := Middle + 1;
    if (Middle >= Right) then Break;

  {$ifdef SMARTGENERICS}
  middle_init:
  {$endif}
    Middle := Right;
    Dec(Middle, Left);
    Middle := Left + (Middle shr 1);

    if (Stored.Compare(Stored.Inst, TRAIIHelper<T>.PArrayT(Values)[Middle], Item) > 0) then Continue;

    Right := Middle + (-1);
    if (not (Left > Right)) then
    begin
      {$ifdef SMARTGENERICS}
        goto middle_init;
      {$else}
        Middle := Left + (-1);
      {$endif}
    end else
    begin
      Break;
    end;
  until (False);

  if (Left >= Stored.Count) or
    (Stored.Compare(Stored.Inst, TRAIIHelper<T>.PArrayT(Values)[Left], Item) <> 0) then
  begin
    Left := not Left;
  end;

  Result := Left;
end;

class function TArray.InternalSearch<T>(Values: Pointer; Index, Count: Integer; const Item: T;
  out FoundIndex: Integer): Boolean;
var
  I: Integer;
  Helper: TSearchHelper;
  {$ifdef SMARTGENERICS}
  TypeData: PTypeData;
  {$endif}
begin
  if (Count <= 0) then
  begin
    if (Count = 0) then
    begin
      FoundIndex := Index;
      Result := True;
      Exit;
    end else
    begin
      raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
    end;
  end;

  {$ifdef SMARTGENERICS}
    if (GetTypeKind(T) in [tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64]) or
      ((GetTypeKind(T) = tkFloat) and (SizeOf(T) = 8)) then
    begin
      TypeData := Pointer(TypeInfo(T));
      Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);
    end;

    case GetTypeKind(T) of
      tkInteger, tkEnumeration, tkChar, tkWChar:
      case SizeOf(T) of
        1:
        begin
          case TypeData.OrdType of
            otSByte: I := SearchSigneds<ShortInt>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
            otUByte: I := SearchUnsigneds<Byte>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
          end;
        end;
        2:
        begin
          case TypeData.OrdType of
            otSWord: I := SearchSigneds<SmallInt>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
            otUWord: I := SearchUnsigneds<Word>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
          end;
        end;
        4:
        begin
          case TypeData.OrdType of
            otSLong: I := SearchSigneds<Integer>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
            otULong: I := SearchUnsigneds<Cardinal>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
          end;
        end;
      end;
      tkInt64:
      begin
        if (TypeData.MaxInt64Value > TypeData.MinInt64Value) then
        begin
          I := SearchSigneds<Int64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end else
        begin
          I := SearchUnsigneds<UInt64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end;
      end;
      tkClass, tkInterface, tkClassRef, tkPointer, tkProcedure:
      begin
        {$ifdef LARGEINT}
          I := SearchUnsigneds<UInt64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        {$else .SMALLINT}
          I := SearchUnsigneds<Cardinal>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        {$endif}
      end;
      tkFloat:
      case SizeOf(T) of
         4: I := SearchFloats<Single>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        10: I := SearchFloats<Extended>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
      else
        if (TypeData.FloatType = ftDouble) then
        begin
          I := SearchFloats<Double>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end else
        begin
          I := SearchSigneds<Int64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end;
      end;
      tkVariant:
      begin
        Helper.Count := Count;
        Helper.Comparer := InterfaceDefaults.TDefaultComparer<Variant>.Create;
        I := SearchUniversals<T>(TRAIIHelper<T>.P(Values) + Index, Helper, Item);
      end;
      {$if CompilerVersion = 28}
        tkMethod, tkString, tkLString, tkWString, tkUString:
        begin
          if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
            InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
          I := SearchBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
        end;
      {$else}
        tkMethod:
        begin
          I := SearchBinaries<InterfaceDefaults.TMethodPtr>(TRAIIHelper<T>.P(Values) + Index, Count,
            InterfaceDefaults.TMethodPtr(Pointer(@Item)^));
        end;
        tkString:
        begin
          I := SearchBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
        end;
        tkLString:
        begin
          {$ifdef NEXTGEN}
            I := SearchBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
          {$else}
            I := SearchBinaries<AnsiString>(TRAIIHelper<T>.P(Values) + Index, Count, AnsiString(Pointer(@Item)^));
          {$endif}
        end;
        {$ifdef MSWINDOWS}
        tkWString:
        begin
          I := SearchBinaries<WideString>(TRAIIHelper<T>.P(Values) + Index, Count, WideString(Pointer(@Item)^));
        end;
        {$else}
        tkWString,
        {$endif}
        tkUString:
        begin
          I := SearchBinaries<UnicodeString>(TRAIIHelper<T>.P(Values) + Index, Count, UnicodeString(Pointer(@Item)^));
        end;
        tkDynArray:
        begin
          if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
            InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
          I := SearchBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
        end;
      {$ifend}
    else
      // binary
      if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
        InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
      I := SearchBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
    end;
  {$else}
    Helper.Count := Count;
    Helper.Comparer := InterfaceDefaults.TDefaultComparer<T>.Create;
    I := SearchUniversals<T>(TRAIIHelper<T>.P(Values) + Index, Helper, Item);
  {$endif}

  if (I < 0) then
  begin
    FoundIndex := Index + (not I);
    Result := False;
  end else
  begin
    FoundIndex := Index + I;
    Result := True;
  end;
end;

class function TArray.InternalSearch<T>(Values: Pointer; Index, Count: Integer; const Item: T;
  out FoundIndex: Integer; Comparer: Pointer): Boolean;
var
  I: Integer;
  Helper: TSearchHelper;
begin
  if (Count <= 0) then
  begin
    if (Count = 0) then
    begin
      FoundIndex := Index;
      Result := True;
      Exit;
    end else
    begin
      raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
    end;
  end;

  Helper.Count := Count;
  Helper.Comparer := Comparer;
  I := TArray.SearchUniversals<T>(TRAIIHelper<T>.P(Values) + Index, Helper, Item);
  if (I < 0) then
  begin
    FoundIndex := Index + (not I);
    Result := False;
  end else
  begin
    FoundIndex := Index + I;
    Result := True;
  end;
end;

class function TArray.BinarySearch<T>(var Values: T; const Item: T;
  out FoundIndex: Integer; Count: Integer): Boolean;
begin
  Result := TArray.InternalSearch<T>(@Values, 0, Count, Item, FoundIndex);
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer): Boolean;
begin
  Result := TArray.InternalSearch<T>(@Values[0], 0, Length(Values), Item, FoundIndex);
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; Index, Count: Integer): Boolean;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) {or (Count < 0)}
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  Result := TArray.InternalSearch<T>(@Values[0], Index, Count, Item, FoundIndex);
end;

class function TArray.BinarySearch<T>(var Values: T; const Item: T;
  out FoundIndex: Integer; Count: Integer; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.InternalSearch<T>(@Values, 0, Count, Item, FoundIndex, Pointer(Comparer));
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.InternalSearch<T>(@Values[0], 0, Length(Values), Item, FoundIndex, Pointer(Comparer));
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>; Index, Count: Integer): Boolean;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) {or (Count < 0)}
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  Result := TArray.InternalSearch<T>(@Values[0], Index, Count, Item, FoundIndex, Pointer(Comparer));
end;

class function TArray.BinarySearch<T>(var Values: T; const Item: T;
  out FoundIndex: Integer; Count: Integer; const Comparison: TComparison<T>): Boolean;
begin
  Result := TArray.InternalSearch<T>(@Values, 0, Count, Item, FoundIndex, PPointer(@Comparison)^);
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparison: TComparison<T>): Boolean;
begin
  Result := TArray.InternalSearch<T>(@Values[0], 0, Length(Values), Item, FoundIndex, PPointer(@Comparison)^);
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; Index, Count: Integer; const Comparison: TComparison<T>): Boolean;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) {or (Count < 0)}
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  Result := TArray.InternalSearch<T>(@Values[0], Index, Count, Item, FoundIndex, PPointer(@Comparison)^);
end;

class function TArray.InternalSearchDescending<T>(Values: Pointer; Index, Count: Integer; const Item: T;
  out FoundIndex: Integer): Boolean;
var
  I: Integer;
  Helper: TSearchHelper;
  {$ifdef SMARTGENERICS}
  TypeData: PTypeData;
  {$endif}
begin
  if (Count <= 0) then
  begin
    if (Count = 0) then
    begin
      FoundIndex := Index;
      Result := True;
      Exit;
    end else
    begin
      raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
    end;
  end;

  {$ifdef SMARTGENERICS}
    if (GetTypeKind(T) in [tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64]) or
      ((GetTypeKind(T) = tkFloat) and (SizeOf(T) = 8)) then
    begin
      TypeData := Pointer(TypeInfo(T));
      Inc(NativeUInt(TypeData), NativeUInt(PByte(@PTypeInfo(TypeData).Name)^) + 2);
    end;

    case GetTypeKind(T) of
      tkInteger, tkEnumeration, tkChar, tkWChar:
      case SizeOf(T) of
        1:
        begin
          case TypeData.OrdType of
            otSByte: I := SearchDescendingSigneds<ShortInt>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
            otUByte: I := SearchDescendingUnsigneds<Byte>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
          end;
        end;
        2:
        begin
          case TypeData.OrdType of
            otSWord: I := SearchDescendingSigneds<SmallInt>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
            otUWord: I := SearchDescendingUnsigneds<Word>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
          end;
        end;
        4:
        begin
          case TypeData.OrdType of
            otSLong: I := SearchDescendingSigneds<Integer>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
            otULong: I := SearchDescendingUnsigneds<Cardinal>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
          end;
        end;
      end;
      tkInt64:
      begin
        if (TypeData.MaxInt64Value > TypeData.MinInt64Value) then
        begin
          I := SearchDescendingSigneds<Int64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end else
        begin
          I := SearchDescendingUnsigneds<UInt64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end;
      end;
      tkClass, tkInterface, tkClassRef, tkPointer, tkProcedure:
      begin
        {$ifdef LARGEINT}
          I := SearchDescendingUnsigneds<UInt64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        {$else .SMALLINT}
          I := SearchDescendingUnsigneds<Cardinal>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        {$endif}
      end;
      tkFloat:
      case SizeOf(T) of
         4: I := SearchDescendingFloats<Single>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        10: I := SearchDescendingFloats<Extended>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
      else
        if (TypeData.FloatType = ftDouble) then
        begin
          I := SearchDescendingFloats<Double>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end else
        begin
          I := SearchDescendingSigneds<Int64>(TRAIIHelper<T>.P(Values) + Index, Count, @Item);
        end;
      end;
      tkVariant:
      begin
        Helper.Count := Count;
        Helper.Comparer := InterfaceDefaults.TDefaultComparer<Variant>.Create;
        I := SearchDescendingUniversals<T>(TRAIIHelper<T>.P(Values) + Index, Helper, Item);
      end;
      {$if CompilerVersion = 28}
        tkMethod, tkString, tkLString, tkWString, tkUString:
        begin
          I := SearchDescendingBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
        end;
      {$else}
        tkMethod:
        begin
          if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
            InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
          I := SearchDescendingBinaries<InterfaceDefaults.TMethodPtr>(TRAIIHelper<T>.P(Values) + Index, Count,
            InterfaceDefaults.TMethodPtr(Pointer(@Item)^));
        end;
        tkString:
        begin
          I := SearchDescendingBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
        end;
        tkLString:
        begin
          {$ifdef NEXTGEN}
            I := SearchDescendingBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
          {$else}
            I := SearchDescendingBinaries<AnsiString>(TRAIIHelper<T>.P(Values) + Index, Count, AnsiString(Pointer(@Item)^));
          {$endif}
        end;
        {$ifdef MSWINDOWS}
        tkWString:
        begin
          I := SearchDescendingBinaries<WideString>(TRAIIHelper<T>.P(Values) + Index, Count, WideString(Pointer(@Item)^));
        end;
        {$else}
        tkWString,
        {$endif}
        tkUString:
        begin
          I := SearchDescendingBinaries<UnicodeString>(TRAIIHelper<T>.P(Values) + Index, Count, UnicodeString(Pointer(@Item)^));
        end;
        tkDynArray:
        begin
          if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
            InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
          I := SearchDescendingBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
        end;
      {$ifend}
    else
      // binary
      if (not InterfaceDefaults.TDefaultComparer<T>.Created) then
        InterfaceDefaults.TDefaultComparer<T>.InternalCreate;
      I := SearchDescendingBinaries<T>(TRAIIHelper<T>.P(Values) + Index, Count, Item);
    end;
  {$else}
    Helper.Count := Count;
    Helper.Comparer := InterfaceDefaults.TDefaultComparer<T>.Create;
    I := SearchDescendingUniversals<T>(TRAIIHelper<T>.P(Values) + Index, Helper, Item);
  {$endif}

  if (I < 0) then
  begin
    FoundIndex := Index + (not I);
    Result := False;
  end else
  begin
    FoundIndex := Index + I;
    Result := True;
  end;
end;

class function TArray.InternalSearchDescending<T>(Values: Pointer; Index, Count: Integer; const Item: T;
  out FoundIndex: Integer; Comparer: Pointer): Boolean;
var
  I: Integer;
  Helper: TSearchHelper;
begin
  if (Count <= 0) then
  begin
    if (Count = 0) then
    begin
      FoundIndex := Index;
      Result := True;
      Exit;
    end else
    begin
      raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
    end;
  end;

  Helper.Count := Count;
  Helper.Comparer := Comparer;
  I := TArray.SearchDescendingUniversals<T>(TRAIIHelper<T>.P(Values) + Index, Helper, Item);
  if (I < 0) then
  begin
    FoundIndex := Index + (not I);
    Result := False;
  end else
  begin
    FoundIndex := Index + I;
    Result := True;
  end;
end;

class function TArray.BinarySearchDescending<T>(var Values: T; const Item: T;
  out FoundIndex: Integer; Count: Integer): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(@Values, 0, Count, Item, FoundIndex);
end;

class function TArray.BinarySearchDescending<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(@Values[0], 0, Length(Values), Item, FoundIndex);
end;

class function TArray.BinarySearchDescending<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; Index, Count: Integer): Boolean;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) {or (Count < 0)}
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  Result := TArray.InternalSearchDescending<T>(@Values[0], Index, Count, Item, FoundIndex);
end;

class function TArray.BinarySearchDescending<T>(var Values: T; const Item: T;
  out FoundIndex: Integer; Count: Integer; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(@Values, 0, Count, Item, FoundIndex, Pointer(Comparer));
end;

class function TArray.BinarySearchDescending<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(@Values[0], 0, Length(Values), Item, FoundIndex, Pointer(Comparer));
end;

class function TArray.BinarySearchDescending<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>; Index, Count: Integer): Boolean;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) {or (Count < 0)}
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  Result := TArray.InternalSearchDescending<T>(@Values[0], Index, Count, Item, FoundIndex, Pointer(Comparer));
end;

class function TArray.BinarySearchDescending<T>(var Values: T; const Item: T;
  out FoundIndex: Integer; Count: Integer; const Comparison: TComparison<T>): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(@Values, 0, Count, Item, FoundIndex, PPointer(@Comparison)^);
end;

class function TArray.BinarySearchDescending<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparison: TComparison<T>): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(@Values[0], 0, Length(Values), Item, FoundIndex, PPointer(@Comparison)^);
end;

class function TArray.BinarySearchDescending<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; Index, Count: Integer; const Comparison: TComparison<T>): Boolean;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) {or (Count < 0)}
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  Result := TArray.InternalSearchDescending<T>(@Values[0], Index, Count, Item, FoundIndex, PPointer(@Comparison)^);
end;


{ TCollectionEnumeratorData<T> }

procedure TCollectionEnumeratorData<T>.Init(const AOwner: TObject; const ATag, AReserved: NativeInt);
begin
  Owner := AOwner;
  Tag := ATag;
  Reserved := AReserved;
end;

{ TCollectionEnumerator<T> }

{$if CompilerVersion <= 22}
function TCollectionEnumerator<T>.GetCurrent: T;
begin
  Result := Self.Data.Current;
end;
{$ifend}

function TCollectionEnumerator<T>.MoveNext: Boolean;
begin
  Result := DoMoveNext(Self);
end;

function TCollectionEnumerator<T>.GetProxyInteface: Pointer{PCollectionEnumerator<T>};
begin
  {Result := @TCollectionEnumeratorObject<T>(ICustomObject(Intf).Self).FEnumerator;}
  Result := PByte(Pointer(Intf)) + SizeOf(IInterface);
end;

function TCollectionEnumerator<T>.InitProxyInteface: Pointer{PCollectionEnumerator<T>};
begin
  if (not Assigned(Intf)) or (not (Intf is TCollectionEnumeratorObject<T>)) then
  begin
    ICustomObject(Intf) := TCollectionEnumeratorObject<T>.Create;
  end;

  Result := GetProxyInteface;
end;

class function TCollectionEnumerator<T>.MoveNextProxyEnumerator(var AEnumerator: TCollectionEnumerator<T>): Boolean;
var
  {$ifdef AUTOREFCOUNT}[Unsafe]{$endif} LObject: TCollectionEnumeratorObject<T>;
begin
  LObject := TCollectionEnumeratorObject<T>(ICustomObject(AEnumerator.Intf).Self);
  if (LObject.FEnumerator.DoMoveNext(LObject.FEnumerator)) then
  begin
    AEnumerator.Data.Current := LObject.FEnumerator.Current;
    Result := True;
    Exit;
  end;

  Result := False;
end;

class function TCollectionEnumerator<T>.MoveNextEnumerator(var AEnumerator: TCollectionEnumerator<T>): Boolean;
begin
  with IEnumerator<T>(AEnumerator.Intf) do
  if (MoveNext) then
  begin
    AEnumerator.Data.Current := Current;
    Result := True;
    Exit;
  end;

  Result := False;
end;

procedure TCollectionEnumerator<T>.Init(const AOwner: TObject;
  const AMoveNextFunc: TMoveNextFunc; const ATag, AReserved: NativeInt);
begin
  Data.Owner := AOwner;
  Data.Tag := ATag;
  Data.Reserved := AReserved;
  DoMoveNext := AMoveNextFunc;
end;

procedure TCollectionEnumerator<T>.Init(const AEnumerator: IEnumerator<T>);
begin
  Self.Intf := AEnumerator;
  Self.DoMoveNext := MoveNextEnumerator;
end;

procedure TCollectionEnumerator<T>.Init(const AEnumerable: IEnumerable<T>);
begin
  Init(AEnumerable.GetEnumerator);
end;

procedure TCollectionEnumerator<T>.Init(const ACollection: TCollection<T>; const AProxyMode: Boolean);
var
  Enumerator: Pointer{PCollectionEnumerator<T>};
begin
  Enumerator := @Self;
  Self.DoMoveNext := MoveNextProxyEnumerator;
  if (AProxyMode) then
  begin
    Enumerator := InitProxyInteface;
  end;

  TCollectionEnumerator<T>(Enumerator^) := ACollection.Enumerator;
end;

procedure TCollectionEnumerator<T>.Init(const ACollection: ICollection<T>; const AProxyMode: Boolean);
var
  Enumerator: Pointer{PCollectionEnumerator<T>};
begin
  Enumerator := @Self;
  Self.DoMoveNext := MoveNextProxyEnumerator;
  if (AProxyMode) then
  begin
    Enumerator := InitProxyInteface;
  end;

  TCollectionEnumerator<T>(Enumerator^) := ACollection.Enumerator;
end;


{ TCollectionEnumeratorObject<T> }

constructor TCollectionEnumeratorObject<T>.Create;
begin
  inherited Create;
end;

constructor TCollectionEnumeratorObject<T>.Create(const AEnumerator: TCollectionEnumerator<T>);
begin
  inherited Create;
  FEnumerator := AEnumerator;
end;

function TCollectionEnumeratorObject<T>.GetCurrentT: T;
begin
  Result := FEnumerator.Data.Current;
end;

function TCollectionEnumeratorObject<T>.MoveNext: Boolean;
begin
  Result := FEnumerator.DoMoveNext(FEnumerator);
end;


{ TCollectionHelper }

class function TCollectionHelper.EItemNotFound: Exception;
begin
  Result := EListError.CreateRes(Pointer(@SGenericItemNotFound));
end;

class function TCollectionHelper.EDuplicatesNotAllowed: Exception;
begin
  Result := EListError.CreateRes(Pointer(@SGenericDuplicateItem));
end;

class function TCollectionHelper.EArgumentIsNil(const AArgumentName: string): Exception;
begin
  Result := EArgumentException.CreateResFmt(Pointer(@SParamIsNil), [AArgumentName]);
end;

class function TCollectionHelper.EArgumentOutOfRange: Exception;
begin
  Result := EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
end;

class procedure TCollectionHelper.CheckArgumentIsNil(const AAssigned: Boolean; const AArgumentName: string);
begin
  if (not AAssigned) then
    raise EArgumentIsNil(AArgumentName) {$if CompilerVersion >= 23}at ReturnAddress{$ifend};
end;

class function TCollectionHelper.EMethodNotFound(const AClass: TClass; const AMethodName: string): Exception;
begin
  Result := ENotSupportedException.CreateResFmt(Pointer(@SMethodNotFound), [AMethodName, AClass.ClassName]);
end;

function TCollectionHelper.EMethodNotFound(const AMethodName: string): Exception;
begin
  Result := EMethodNotFound(Self.ClassType, AMethodName);
end;

class function TCollectionHelper.InternalSumBytes(Value: PByte; Offset: NativeInt;
  Count: Integer): Byte;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + Value^;
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollectionHelper.InternalSumWords(Value: PWord; Offset: NativeInt;
  Count: Integer): Word;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + Value^;
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollectionHelper.InternalSumCardinals(Value: PCardinal; Offset: NativeInt;
  Count: Integer): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + Value^;
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollectionHelper.InternalSumInt64s(Value: PInt64; Offset: NativeInt;
  Count: Integer): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + Value^;
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollectionHelper.InternalSumSingles(Value: PSingle; Offset: NativeInt;
  Count: Integer): Single;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + Value^;
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollectionHelper.InternalSumDoubles(Value: PDouble; Offset: NativeInt;
  Count: Integer): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + Value^;
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollectionHelper.InternalSumExtendeds(Value: PExtended; Offset: NativeInt;
  Count: Integer): Extended;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + Value^;
    Inc(NativeInt(Value), Offset);
  end;
end;


{ TCollection<T>.TEnumeratorAdapter }

function TCollection<T>.TEnumeratorAdapter.GetCurrent: TObject;
begin
  if ({$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} = tkClass) then
  begin
    T(Pointer(@Result)^) := FEnumerator.Data.Current;
  end else
  begin
    Result := nil;
  end;
end;

procedure TCollection<T>.TEnumeratorAdapter.Reset;
begin
  raise EMethodNotFound(ClassType, 'Reset');
end;

{ TCollection<T>.TLinearItems }

function TCollection<T>.TLinearItems.Combine(var AResult: TLinearItems; const ASecond: TLinearItems): Boolean;
var
  LLeftCounts, LRightCounts: Integer;
begin
  if (Self.Offset = ASecond.Offset) then
  begin
    LLeftCounts := Ord(Self.Count1 <> 0) or Ord(Self.Count2 <> 0);
    LRightCounts := Ord(ASecond.Count1 <> 0) or Ord(ASecond.Count2 <> 0);
    if (LLeftCounts + LRightCounts <= 2) then
    begin
      if (LRightCounts = 0) then
      begin
        AResult := Self;
      end else
      if (LLeftCounts = 0) then
      begin
        AResult := ASecond;
      end else
      // if (LLeftCounts = 1) and (LRightCounts = 1) then
      begin
        AResult.Offset := Self.Offset;

        if (Self.Count1 <> 0) then
        begin
          AResult.Values1 := Self.Values1;
          AResult.Count1 := Self.Count1;
        end else
        // if (Self.Count2 <> 0) then
        begin
          AResult.Values1 := Self.Values2;
          AResult.Count1 := Self.Count2;
        end;

        if (ASecond.Count1 <> 0) then
        begin
          AResult.Values2 := ASecond.Values1;
          AResult.Count2 := ASecond.Count1;
        end else
        // if (ASecond.Count2 <> 0) then
        begin
          AResult.Values2 := ASecond.Values2;
          AResult.Count2 := ASecond.Count2;
        end;
      end;

      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

procedure TCollection<T>.TLinearItems.ToArray(var AResult: TArray<T>);
var
  Count, i: Integer;
  Src, Dest: P;
  LOffset: NativeUInt;
begin
  Count := Count1 + Count2;
  if (Count = 0) then
  begin
    if (Pointer(AResult) <> nil) then
      AResult := nil;

    Exit;
  end;

  SetLength(AResult, Count);
  Dest := Pointer(AResult);

  LOffset := Self.Offset;
  if (LOffset <> SizeOf(T)) then
  begin
    Src := Values1;
    for i := 1 to Count1 do
    begin
      Dest^ := Src^;
      Inc(NativeUInt(Src), LOffset);
      Inc(Dest);
    end;

    Src := Values2;
    for i := 1 to Count2 do
    begin
      Dest^ := Src^;
      Inc(NativeUInt(Src), LOffset);
      Inc(Dest);
    end;

    Exit;
  end;

  if (TRAIIHelper<T>.Managed) then
  begin
    Count := Count1;
    if (Count <> 0) then
    begin
      System.CopyArray(Dest, Values1, TypeInfo(T), Count);
      Inc(Dest, Count);
    end;

    Count := Count2;
    if (Count <> 0) then
    begin
      System.CopyArray(Dest, Values2, TypeInfo(T), Count);
    end;
  end else
  begin
    Count := Count1;
    if (Count <> 0) then
    begin
      System.Move(Values1^, Dest^, Count * SizeOf(T));
      Inc(Dest, Count);
    end;

    Count := Count2;
    if (Count <> 0) then
    begin
      System.Move(Values2^, Dest^, Count * SizeOf(T));
    end;
  end;
end;

procedure TCollection<T>.TLinearItems.ToArray(var AResult: TArray<T>;
  const APredicate: TFunction<T,Boolean>);
var
  i: Integer;
  Item: P;
  Count, Buffered: NativeUInt;
  LOffset: NativeUInt;
begin
  if (Pointer(AResult) <> nil) then
    AResult := nil;
  if (Count1 or Count2 = 0) then
    Exit;

  Count := 0;
  Buffered := 16;
  LOffset := Offset;
  SetLength(AResult, Buffered);

  Item := Values1;
  for i := 1 to Count1 do
  begin
    if (APredicate(Item^)) then
    begin
      if (Count = Buffered) then
      begin
        Buffered := Buffered * 2;
        SetLength(AResult, Buffered);
      end;

      AResult[Count] := Item^;
      Inc(Count);
    end;

    Inc(NativeUInt(Item), LOffset);
  end;

  Item := Values2;
  for i := 1 to Count2 do
  begin
    if (APredicate(Item^)) then
    begin
      if (Count = Buffered) then
      begin
        Buffered := Buffered * 2;
        SetLength(AResult, Buffered);
      end;

      AResult[Count] := Item^;
      Inc(Count);
    end;

    Inc(NativeUInt(Item), LOffset);
  end;

  SetLength(AResult, Count);
end;

function TCollection<T>.TLinearItems.All(const APredicate: TFunction<T,Boolean>): Boolean;
var
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Result := InternalAll(Values1, Offset, Count, APredicate);
    if (not Result) then
      Exit;
  end;

  Count := Count2;
  if (Count <> 0) then
  begin
    Result := InternalAll(Values2, Offset, Count, APredicate);
    if (not Result) then
      Exit;
  end;

  Result := True;
end;

function TCollection<T>.TLinearItems.Any(const APredicate: TFunction<T,Boolean>): Boolean;
var
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Result := InternalAny(Values1, Offset, Count, APredicate);
    if (Result) then
      Exit;
  end;

  Count := Count2;
  if (Count <> 0) then
  begin
    Result := InternalAny(Values2, Offset, Count, APredicate);
    if (Result) then
      Exit;
  end;

  Result := False;
end;

function TCollection<T>.TLinearItems.Aggregate(const AFunc: TFunction<T,T,T>): T;
var
  Count: Integer;
  Item: P;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Item := Values1;
    Result := Item^;
    Inc(Item);
    Dec(Count);
    if (Count <> 0) then
      Result := InternalAggregate(Item, Offset, Count, Result, AFunc);

    Item := Values2;
    Count := Count2;
  end else
  begin
    Item := Values2;
    Count := Count2;
    Result := Item^;
    Inc(Item);
    Dec(Count);
  end;

  if (Count <> 0) then
    Result := InternalAggregate(Item, Offset, Count, Result, AFunc);
end;

function TCollection<T>.TLinearItems.TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>;
  const AFound: Boolean): Boolean;
var
  Count: Integer;
  Item: P;
begin
  Result := AFound;

  Count := Count1;
  if (Count <> 0) then
  begin
    Item := Values1;
    if (not Result) then
    begin
      Value := Item^;
      Inc(Item);
      Dec(Count);
      Result := True;
    end;
    if (Count <> 0) then
      Value := InternalAggregate(Item, Offset, Count, Value, AFunc);
  end;

  Count := Count2;
  if (Count <> 0) then
  begin
    Item := Values2;
    if (not Result) then
    begin
      Value := Item^;
      Inc(Item);
      Dec(Count);
      Result := True;
    end;
    if (Count <> 0) then
      Value := InternalAggregate(Item, Offset, Count, Value, AFunc);
  end;
end;

function TCollection<T>.TLinearItems.Min(const AComparer: TComparison<T>): T;
var
  Temp: T;
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Result := InternalMin(Values1, Offset, Count, AComparer);

    Count := Count2;
    if (Count <> 0) then
    begin
      Temp := InternalMin(Values2, Offset, Count, AComparer);
      if (AComparer(Temp, Result) < 0) then
        Result := Temp;
    end;
  end else
  begin
    Result := InternalMin(Values2, Offset, Count2, AComparer);
  end;
end;

function TCollection<T>.TLinearItems.Min(const ASelector: TFunction<T,Integer>): Integer;
var
  Temp: Integer;
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Result := InternalMin(Values1, Offset, Count, ASelector);

    Count := Count2;
    if (Count <> 0) then
    begin
      Temp := InternalMin(Values2, Offset, Count, ASelector);
      if (Temp < Result) then
        Result := Temp;
    end;
  end else
  begin
    Result := InternalMin(Values2, Offset, Count2, ASelector);
  end;
end;

function TCollection<T>.TLinearItems.Max(const AComparer: TComparison<T>): T;
var
  Temp: T;
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Result := InternalMax(Values1, Offset, Count, AComparer);

    Count := Count2;
    if (Count <> 0) then
    begin
      Temp := InternalMax(Values2, Offset, Count, AComparer);
      if (AComparer(Temp, Result) > 0) then
        Result := Temp;
    end;
  end else
  begin
    Result := InternalMax(Values2, Offset, Count2, AComparer);
  end;
end;

function TCollection<T>.TLinearItems.Max(const ASelector: TFunction<T,Integer>): Integer;
var
  Temp: Integer;
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Result := InternalMax(Values1, Offset, Count, ASelector);

    Count := Count2;
    if (Count <> 0) then
    begin
      Temp := InternalMax(Values2, Offset, Count, ASelector);
      if (Temp > Result) then
        Result := Temp;
    end;
  end else
  begin
    Result := InternalMax(Values2, Offset, Count2, ASelector);
  end;
end;

function TCollection<T>.TLinearItems.Sum: T;
begin
  Result := Default(T);

  case {$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} of
    tkInteger:
    begin
      case (GetTypeData(TypeInfo(T)).OrdType) of
        otSByte, otUByte:
        begin
          if (Count1 <> 0) then PByte(@Result)^ := InternalSumBytes(Pointer(Values1), Offset, Count1);
          if (Count2 <> 0) then PByte(@Result)^ := PByte(@Result)^ + InternalSumBytes(Pointer(Values2), Offset, Count2);
        end;
        otSWord, otUWord:
        begin
          if (Count1 <> 0) then PWord(@Result)^ := InternalSumWords(Pointer(Values1), Offset, Count1);
          if (Count2 <> 0) then PWord(@Result)^ := PWord(@Result)^ + InternalSumWords(Pointer(Values2), Offset, Count2);
        end;
      else
        // otSLong, otULong:
        if (Count1 <> 0) then PCardinal(@Result)^ := InternalSumCardinals(Pointer(Values1), Offset, Count1);
        if (Count2 <> 0) then PCardinal(@Result)^ := PCardinal(@Result)^ + InternalSumCardinals(Pointer(Values2), Offset, Count2);
      end;
    end;
    tkInt64:
    begin
      if (Count1 <> 0) then PInt64(@Result)^ := InternalSumInt64s(Pointer(Values1), Offset, Count1);
      if (Count2 <> 0) then PInt64(@Result)^ := PInt64(@Result)^ + InternalSumInt64s(Pointer(Values2), Offset, Count2);
    end;
    tkFloat:
    begin
      case (GetTypeData(TypeInfo(T)).FloatType) of
        ftSingle:
        begin
          if (Count1 <> 0) then PSingle(@Result)^ := InternalSumSingles(Pointer(Values1), Offset, Count1);
          if (Count2 <> 0) then PSingle(@Result)^ := PSingle(@Result)^ + InternalSumSingles(Pointer(Values2), Offset, Count2);
        end;
        ftDouble:
        begin
          if (Count1 <> 0) then PDouble(@Result)^ := InternalSumDoubles(Pointer(Values1), Offset, Count1);
          if (Count2 <> 0) then PDouble(@Result)^ := PDouble(@Result)^ + InternalSumDoubles(Pointer(Values2), Offset, Count2);
        end;
        ftExtended:
        begin
          if (Count1 <> 0) then PExtended(@Result)^ := InternalSumExtendeds(Pointer(Values1), Offset, Count1);
          if (Count2 <> 0) then PExtended(@Result)^ := PExtended(@Result)^ + InternalSumExtendeds(Pointer(Values2), Offset, Count2);
        end;
      else
        if (Count1 <> 0) then PInt64(@Result)^ := InternalSumInt64s(Pointer(Values1), Offset, Count1);
        if (Count2 <> 0) then PInt64(@Result)^ := PInt64(@Result)^ + InternalSumInt64s(Pointer(Values2), Offset, Count2);
      end;
    end;
  end;
end;

function TCollection<T>.TLinearItems.Sum(const ASelector: TFunction<T,Integer>): Integer;
begin
  Result := InternalSum(Values1, Offset, Count1, ASelector) +
    InternalSum(Values2, Offset, Count2, ASelector);
end;

function TCollection<T>.TLinearItems.Sum(const ASelector: TFunction<T,Int64>): Int64;
begin
  Result := InternalSum(Values1, Offset, Count1, ASelector) +
    InternalSum(Values2, Offset, Count2, ASelector);
end;

function TCollection<T>.TLinearItems.Sum(const ASelector: TFunction<T,Extended>): Extended;
begin
  Result := InternalSum(Values1, Offset, Count1, ASelector) +
    InternalSum(Values2, Offset, Count2, ASelector);
end;

procedure TCollection<T>.TLinearItems.ForEach(const AAction: TProcedure<T>);
var
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    InternalForEach(Values1, Offset, Count, AAction);
  end;

  Count := Count2;
  if (Count <> 0) then
    InternalForEach(Values2, Offset, Count, AAction);
end;

function TCollection<T>.TLinearItems.ForEach(const AAction: TFunction<T,Boolean>): Boolean;
var
  Count: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Result := InternalForEach(Values1, Offset, Count, AAction);
    if (not Result) then
      Exit;
  end;

  Count := Count2;
  if (Count <> 0) then
  begin
    Result := InternalForEach(Values2, Offset, Count, AAction);
  end else
  begin
    Result := True;
  end;
end;

function TCollection<T>.TLinearItems.TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  Count, Index: Integer;
begin
  Count := Count1;
  if (Count <> 0) then
  begin
    Index := InternalGetFirst(Values1, Offset, Count, APredicate);
    if (Index >= 0) then
    begin
      Value := Values1[Index];
      Result := True;
      Exit;
    end;
  end;

  Count := Count2;
  if (Count <> 0) then
  begin
    Index := InternalGetFirst(Values2, Offset, Count, APredicate);
    if (Index >= 0) then
    begin
      Value := Values2[Index];
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TCollection<T>.TLinearItems.TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  Count, Index: Integer;
begin
  Count := Count2;
  if (Count <> 0) then
  begin
    Index := InternalGetLast(Values2, Offset, Count, APredicate);
    if (Index >= 0) then
    begin
      Value := Values2[Index];
      Result := True;
      Exit;
    end;
  end;

  Count := Count1;
  if (Count <> 0) then
  begin
    Index := InternalGetLast(Values1, Offset, Count, APredicate);
    if (Index >= 0) then
    begin
      Value := Values1[Index];
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TCollection<T>.TLinearItems.TryGetSingle(var{out} Value: T;
  const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
var
  Index: Integer;
begin
  if (Count1 <> 0) then
  begin
    Index := InternalGetSingle(Values1, Offset, Count1, APredicate, AFound);
    if (Index < 0) then
    begin
      if (Index = -2) then
      begin
        Result := 2;
        Exit;
      end;
    end else
    // if (Index >= 0) then
    begin
      if (not AFound) and ((Count2 = 0) or (InternalGetSingle(Values2, Offset, Count2, APredicate, False) = -1)) then
      begin
        Value := Values1[Index];
        Result := 1;
        Exit;
      end else
      begin
        Result := 2;
        Exit;
      end;
    end;
  end;

  if (Count2 <> 0) then
  begin
    Index := InternalGetSingle(Values2, Offset, Count2, APredicate, AFound);
    if (Index >= 0) then
    begin
      Value := Values2[Index];
      Result := 1;
      Exit;
    end
  end;

  Result := Ord(AFound);
end;

function TCollection<T>.TLinearItems.IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  if (Count1 <> 0) then
  begin
    Result := InternalIndexOf(AValue, Values1, Offset, Count1, AComparer);
    if (Result >= 0) then
      Exit;
  end;

  if (Count2 <> 0) then
  begin
    Result := InternalIndexOf(AValue, Values2, Offset, Count2, AComparer);
    if (Result >= 0) then
    begin
      Inc(Result, Count1);
      Exit;
    end;
  end;

  Result := -1;
end;

function TCollection<T>.TLinearItems.LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  if (Count2 <> 0) then
  begin
    Result := InternalLastIndexOf(AValue, Values2, Offset, Count2, AComparer);
    if (Result >= 0) then
    begin
      Inc(Result, Count1);
      Exit;
    end;
  end;

  if (Count1 <> 0) then
  begin
    Result := InternalLastIndexOf(AValue, Values1, Offset, Count1, AComparer);
    Exit;
  end;

  Result := -1;
end;

function TCollection<T>.TLinearItems.EqualsTo(const ALinearItems: TLinearItems;
  const AComparer: TEqualityComparison<T>): Boolean;
var
  LLeftCount, LRightCount, LCount: Integer;
  LLeftValues, LRightValues: P;
begin
  // first items
  LLeftCount := Self.Count1;
  LRightCount := ALinearItems.Count1;
  if (LLeftCount = LRightCount) then
  begin
    if (LLeftCount <> 0) then
    begin
      Result := InternalEqualsTo(Values1, ALinearItems.Values1, Self.Offset, ALinearItems.Offset, LLeftCount, AComparer);
      if (not Result) then
        Exit;
    end;

    LLeftValues := Values2;
    LRightValues := ALinearItems.Values2;
    LCount := Count2;
  end else
  if (LLeftCount > LRightCount) then
  begin
    LLeftValues := Values1;

    // compare right count elements
    if (LRightCount <> 0) then
    begin
      Result := InternalEqualsTo(LLeftValues, ALinearItems.Values1, Self.Offset, ALinearItems.Offset, LRightCount, AComparer);
      if (not Result) then
        Exit;

      Inc(NativeInt(LLeftValues), LRightCount * Self.Offset);
      Dec(LLeftCount, LRightCount);
    end;

    // compare remain left elements
    LRightValues := ALinearItems.Values2;
    begin
      Result := InternalEqualsTo(LLeftValues, LRightValues, Self.Offset, ALinearItems.Offset, LLeftCount, AComparer);
      if (not Result) then
        Exit;

      Inc(NativeInt(LRightValues), LLeftCount * ALinearItems.Offset);
    end;

    LLeftValues := Values2;
    LCount := Count2;
  end else
  if (LLeftCount < LRightCount) then
  begin
    LRightValues := ALinearItems.Values1;

    // compare left count elements
    if (LLeftCount <> 0) then
    begin
      Result := InternalEqualsTo(Values1, LRightValues, Self.Offset, ALinearItems.Offset, LLeftCount, AComparer);
      if (not Result) then
        Exit;

      Inc(NativeInt(LRightValues), LLeftCount * Self.Offset);
      Dec(LRightCount, LLeftCount);
    end;

    // compare remain right elements
    LLeftValues := Values2;
    begin
      Result := InternalEqualsTo(LLeftValues, LRightValues, Self.Offset, ALinearItems.Offset, LRightCount, AComparer);
      if (not Result) then
        Exit;

      Inc(NativeInt(LLeftValues), LRightCount * ALinearItems.Offset);
    end;

    LRightValues := ALinearItems.Values2;
    LCount := ALinearItems.Count2;
  end;

  // last items
  Result := (LCount = 0) or (InternalEqualsTo(LLeftValues, LRightValues, Self.Offset,
    ALinearItems.Offset, LCount, AComparer));
end;

function TCollection<T>.TLinearItems.EqualsTo(var AEnumerator: TCollectionEnumerator<T>;
  const AComparer: TEqualityComparison<T>): Boolean;
var
  Count: Integer;
begin
  Count := Self.Count1;
  if (Count <> 0) then
  begin
    Result := InternalEqualsTo(Values1, Offset, Count, AEnumerator, AComparer);
    if (not Result) then
      Exit;
  end;

  Count := Self.Count2;
  if (Count <> 0) then
  begin
    Result := InternalEqualsTo(Values2, Offset, Count, AEnumerator, AComparer);
    if (not Result) then
      Exit;
  end;

  Result := True;
end;

function TCollection<T>.TLinearItems.EqualsTo(const AEnumerator: IEnumerator<T>;
  const AComparer: TEqualityComparison<T>): Boolean;
var
  Count: Integer;
begin
  Count := Self.Count1;
  if (Count <> 0) then
  begin
    Result := InternalEqualsTo(Values1, Offset, Count, AEnumerator, AComparer);
    if (not Result) then
      Exit;
  end;

  Count := Self.Count2;
  if (Count <> 0) then
  begin
    Result := InternalEqualsTo(Values2, Offset, Count, AEnumerator, AComparer);
    if (not Result) then
      Exit;
  end;

  Result := True;
end;

{ TCollection<T> }

function TCollection<T>.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorT;
end;

function TCollection<T>.GetEnumeratorT: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter.Create(DoGetEnumerator);
end;

class function TCollection<T>.InternalAll(Value: P; Offset: NativeInt; Count: Integer;
  const APredicate: TFunction<T,Boolean>): Boolean;
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    Result := APredicate(Value^);
    if (not Result) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := True;
end;

class function TCollection<T>.InternalAll(const AEnumerator: IEnumerator<T>;
  const APredicate: TFunction<T,Boolean>): Boolean;
begin
  while (AEnumerator.MoveNext) do
  begin
    Result := APredicate(AEnumerator.Current);
    if (not Result) then
      Exit;
  end;

  Result := True;
end;

class function TCollection<T>.InternalAll(const AEnumerable: IEnumerable<T>;
  const APredicate: TFunction<T,Boolean>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  Result := (not Assigned(LEnumerator)) or InternalAll(LEnumerator, APredicate);
end;

class function TCollection<T>.InternalAny(Value: P; Offset: NativeInt; Count: Integer;
  const APredicate: TFunction<T,Boolean>): Boolean;
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    Result := APredicate(Value^);
    if (Result) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := False;
end;

class function TCollection<T>.InternalAny(const AEnumerator: IEnumerator<T>;
  const APredicate: TFunction<T,Boolean>): Boolean;
begin
  while (AEnumerator.MoveNext) do
  begin
    Result := APredicate(AEnumerator.Current);
    if (Result) then
      Exit;
  end;

  Result := False;
end;

class function TCollection<T>.InternalAny(const AEnumerable: IEnumerable<T>;
  const APredicate: TFunction<T,Boolean>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  Result := Assigned(LEnumerator) and InternalAll(LEnumerator, APredicate);
end;

class function TCollection<T>.InternalAggregate(Value: P; Offset: NativeInt; Count: Integer;
  const ABaseValue: T; const AFunc: TFunction<T,T,T>): T;
var
  i: Integer;
begin
  Result := ABaseValue;
  for i := 1 to Count do
  begin
    Result := AFunc(Result, Value^);
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalTryAggregate(var AEnumerator: TCollectionEnumerator<T>;
  var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean;
begin
  if (not AEnumerator.MoveNext) then
  begin
    Result := AFound;
    Exit;
  end;

  if (not AFound) then
  begin
    Value := AEnumerator.Data.Current;
    if (not AEnumerator.MoveNext) then
    begin
      Result := True;
      Exit;
    end;
  end;

  repeat
    Value := AFunc(Value, AEnumerator.Data.Current);
  until (not AEnumerator.MoveNext);

  Result := True;
end;

class function TCollection<T>.InternalTryAggregate(const ACollection: TCollection<T>;
  var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean;
var
  LEnumerator: TCollectionEnumerator<T>;
begin
  LEnumerator := ACollection.Enumerator;
  Result := InternalTryAggregate(LEnumerator, Value, AFunc, AFound);
end;

class function TCollection<T>.InternalTryAggregate(const ACollection: ICollection<T>;
  var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean;
var
  LEnumerator: TCollectionEnumerator<T>;
begin
  LEnumerator := ACollection.Enumerator;
  Result := InternalTryAggregate(LEnumerator, Value, AFunc, AFound);
end;

class function TCollection<T>.InternalTryAggregate(const AEnumerator: IEnumerator<T>;
  var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean;
begin
  if (not AEnumerator.MoveNext) then
  begin
    Result := AFound;
    Exit;
  end;

  if (not AFound) then
  begin
    Value := AEnumerator.Current;
    if (not AEnumerator.MoveNext) then
    begin
      Result := True;
      Exit;
    end;
  end;

  repeat
    Value := AFunc(Value, AEnumerator.Current);
  until (not AEnumerator.MoveNext);

  Result := True;
end;

class function TCollection<T>.InternalTryAggregate(const AEnumerable: IEnumerable<T>;
  var{out} Value: T; const AFunc: TFunction<T,T,T>; const AFound: Boolean): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryAggregate(LEnumerator, Value, AFunc, AFound);
  end else
  begin
    Result := AFound;
  end;
end;

class function TCollection<T>.InternalMin(Value: P; Offset: NativeInt; Count: Integer;
  const AComparer: TComparison<T>): T;
var
  i: Integer;
begin
  Result := Value^;
  Inc(NativeInt(Value), Offset);
  for i := 1 to Count - 1 do
  begin
    if (AComparer(Value^, Result) < 0) then
      Result := Value^;

    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalMin(Value: P; Offset: NativeInt; Count: Integer;
  const ASelector: TFunction<T,Integer>): Integer;
var
  i: Integer;
  LValue: Integer;
begin
  Result := ASelector(Value^);
  Inc(NativeInt(Value), Offset);
  for i := 1 to Count - 1 do
  begin
    LValue := ASelector(Value^);
    if (LValue < Result) then
      Result := LValue;

    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalTryGetMin(var{out} Value: T;
   const AEnumerator: IEnumerator<T>; const AComparer: TComparison<T>): Boolean;
var
  Temp: T;
begin
  Result := AEnumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := AEnumerator.Current;
  while (AEnumerator.MoveNext) do
  begin
    Temp := AEnumerator.Current;
    if (AComparer(Temp, Value) < 0) then
      Value := Temp;
  end;

  Result := True;
end;

class function TCollection<T>.InternalTryGetMin(var{out} Value: T;
  const AEnumerable: IEnumerable<T>; const AComparer: TComparison<T>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  Result := Assigned(LEnumerator) and InternalTryGetMin(Value, LEnumerator, AComparer);
end;

class function TCollection<T>.InternalTryGetMin(var{out} Value: Integer;
  const AEnumerator: IEnumerator<T>; const ASelector: TFunction<T,Integer>): Boolean;
var
  Temp: Integer;
begin
  Result := AEnumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := ASelector(AEnumerator.Current);
  while (AEnumerator.MoveNext) do
  begin
    Temp := ASelector(AEnumerator.Current);
    if (Temp < Value) then
      Value := Temp;
  end;

  Result := True;
end;

class function TCollection<T>.InternalTryGetMin(var{out} Value: Integer;
  const AEnumerable: IEnumerable<T>; const ASelector: TFunction<T,Integer>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  Result := Assigned(LEnumerator) and InternalTryGetMin(Value, LEnumerator, ASelector);
end;

class function TCollection<T>.InternalMax(Value: P; Offset: NativeInt; Count: Integer;
  const AComparer: TComparison<T>): T;
var
  i: Integer;
begin
  Result := Value^;
  Inc(NativeInt(Value), Offset);
  for i := 1 to Count - 1 do
  begin
    if (AComparer(Value^, Result) > 0) then
      Result := Value^;

    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalMax(Value: P; Offset: NativeInt; Count: Integer;
  const ASelector: TFunction<T,Integer>): Integer;
var
  i: Integer;
  LValue: Integer;
begin
  Result := ASelector(Value^);
  Inc(NativeInt(Value), Offset);
  for i := 1 to Count - 1 do
  begin
    LValue := ASelector(Value^);
    if (LValue > Result) then
      Result := LValue;

    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalTryGetMax(var{out} Value: T;
   const AEnumerator: IEnumerator<T>; const AComparer: TComparison<T>): Boolean;
var
  Temp: T;
begin
  Result := AEnumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := AEnumerator.Current;
  while (AEnumerator.MoveNext) do
  begin
    Temp := AEnumerator.Current;
    if (AComparer(Temp, Value) > 0) then
      Value := Temp;
  end;

  Result := True;
end;

class function TCollection<T>.InternalTryGetMax(var{out} Value: T;
   const AEnumerable: IEnumerable<T>; const AComparer: TComparison<T>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  Result := (Assigned(LEnumerator)) and InternalTryGetMax(Value, LEnumerator, AComparer);
end;

class function TCollection<T>.InternalTryGetMax(var{out} Value: Integer;
  const AEnumerator: IEnumerator<T>; const ASelector: TFunction<T,Integer>): Boolean;
var
  Temp: Integer;
begin
  Result := AEnumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := ASelector(AEnumerator.Current);
  while (AEnumerator.MoveNext) do
  begin
    Temp := ASelector(AEnumerator.Current);
    if (Temp > Value) then
      Value := Temp;
  end;

  Result := True;
end;

class function TCollection<T>.InternalTryGetMax(var{out} Value: Integer;
  const AEnumerable: IEnumerable<T>; const ASelector: TFunction<T,Integer>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  Result := (Assigned(LEnumerator)) and InternalTryGetMax(Value, LEnumerator, ASelector);
end;

class function TCollection<T>.InternalSum(Value: P; Offset: NativeInt; Count: Integer;
  const ASelector: TFunction<T,Integer>): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + ASelector(Value^);
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalSum(Value: P; Offset: NativeInt; Count: Integer;
  const ASelector: TFunction<T,Int64>): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + ASelector(Value^);
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalSum(Value: P; Offset: NativeInt; Count: Integer;
  const ASelector: TFunction<T,Extended>): Extended;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
  begin
    Result := Result + ASelector(Value^);
    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalSum(const AEnumerator: IEnumerator<T>): T;
var
  Mode: Cardinal;
  Temp: T;
begin
  Mode := Cardinal(-1);
  case {$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} of
    tkInteger:
    begin
      case (GetTypeData(TypeInfo(T)).OrdType) of
        otSByte, otUByte: Mode := 0;
        otSWord, otUWord: Mode := 1;
        otSLong, otULong: Mode := 2;
      end;
    end;
    tkInt64:
    begin
      Mode := 3;
    end;
    tkFloat:
    begin
      case (GetTypeData(TypeInfo(T)).FloatType) of
        ftSingle: Mode := 4;
        ftDouble: Mode := 5;
      ftExtended: Mode := 6;
      else
        Mode := 3;
      end;
    end;
  end;

  Result := Default(T);
  if (Mode = Cardinal(-1)) then
    Exit;

  while (AEnumerator.MoveNext) do
  begin
    Temp := AEnumerator.Current;

    case Mode of
      0: PByte(@Result)^ := PByte(@Result)^ + PByte(@Temp)^;
      1: PWord(@Result)^ := PWord(@Result)^ + PWord(@Temp)^;
      2: PCardinal(@Result)^ := PCardinal(@Result)^ + PCardinal(@Temp)^;
      3: PInt64(@Result)^ := PInt64(@Result)^ + PInt64(@Temp)^;
      4: PSingle(@Result)^ := PSingle(@Result)^ + PSingle(@Temp)^;
      5: PDouble(@Result)^ := PDouble(@Result)^ + PDouble(@Temp)^;
    else
      // 6:
      PExtended(@Result)^ := PExtended(@Result)^ + PExtended(@Temp)^;
    end;
  end;
end;

class function TCollection<T>.InternalSum(const AEnumerable: IEnumerable<T>): T;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalSum(LEnumerator);
  end else
  begin
    Result := Default(T);
  end;
end;

class function TCollection<T>.InternalSum(const AEnumerator: IEnumerator<T>;
  const ASelector: TFunction<T,Integer>): Integer;
begin
  Result := 0;
  while (AEnumerator.MoveNext) do
    Result := Result + ASelector(AEnumerator.Current);
end;

class function TCollection<T>.InternalSum(const AEnumerable: IEnumerable<T>;
  const ASelector: TFunction<T,Integer>): Integer;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalSum(LEnumerator, ASelector);
  end else
  begin
    Result := 0;
  end;
end;

class function TCollection<T>.InternalSum(const AEnumerator: IEnumerator<T>;
  const ASelector: TFunction<T,Int64>): Int64;
begin
  Result := 0;
  while (AEnumerator.MoveNext) do
    Result := Result + ASelector(AEnumerator.Current);
end;

class function TCollection<T>.InternalSum(const AEnumerable: IEnumerable<T>;
  const ASelector: TFunction<T,Int64>): Int64;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalSum(LEnumerator, ASelector);
  end else
  begin
    Result := 0;
  end;
end;
class function TCollection<T>.InternalSum(const AEnumerator: IEnumerator<T>;
  const ASelector: TFunction<T,Extended>): Extended;
begin
  Result := 0;
  while (AEnumerator.MoveNext) do
    Result := Result + ASelector(AEnumerator.Current);
end;

class function TCollection<T>.InternalSum(const AEnumerable: IEnumerable<T>;
  const ASelector: TFunction<T,Extended>): Extended;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalSum(LEnumerator, ASelector);
  end else
  begin
    Result := 0;
  end;
end;

class procedure TCollection<T>.InternalForEach(Value: P; Offset: NativeInt; Count: Integer;
  const AAction: TProcedure<T>);
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    AAction(Value^);
    Inc(NativeInt(Value), Offset);
  end;
end;

class procedure TCollection<T>.InternalForEach(const AEnumerator: IEnumerator<T>;
  const AAction: TProcedure<T>);
begin
  while (AEnumerator.MoveNext) do
  begin
    AAction(AEnumerator.Current);
  end;
end;

class procedure TCollection<T>.InternalForEach(const AEnumerable: IEnumerable<T>;
  const AAction: TProcedure<T>);
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    InternalForEach(LEnumerator, AAction);
  end;
end;

class function TCollection<T>.InternalForEach(Value: P; Offset: NativeInt; Count: Integer;
  const AAction: TFunction<T,Boolean>): Boolean;
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    Result := AAction(Value^);
    if (not Result) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := True;
end;

class function TCollection<T>.InternalForEach(const AEnumerator: IEnumerator<T>;
  const AAction: TFunction<T,Boolean>): Boolean;
begin
  while (AEnumerator.MoveNext) do
  begin
    Result := AAction(AEnumerator.Current);
    if (not Result) then
      Exit;
  end;

  Result := True;
end;

class function TCollection<T>.InternalForEach(const AEnumerable: IEnumerable<T>;
  const AAction: TFunction<T,Boolean>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalForEach(LEnumerator, AAction);
  end else
  begin
    Result := True;
  end;
end;

class function TCollection<T>.InternalTryGetElementAt(const AEnumerator: IEnumerator<T>;
  var{out} Value: T; const AIndex: Integer): Boolean;
var
  LCounter: Integer;
begin
  LCounter := AIndex;
  while (AEnumerator.MoveNext) do
  begin
    if (LCounter = 0) then
    begin
      Value := AEnumerator.Current;
      Result := True;
      Exit;
    end;
    Dec(LCounter);
  end;

  Result := True;
end;

class function TCollection<T>.InternalTryGetElementAt(const AEnumerable: IEnumerable<T>;
  var{out} Value: T; const AIndex: Integer): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryGetElementAt(LEnumerator, Value, AIndex);
  end else
  begin
    Result := False;
  end;
end;

class function TCollection<T>.InternalGetFirst(Value: P; Offset: NativeInt; Count: Integer;
  const APredicate: TFunction<T,Boolean>): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if (APredicate(Value^)) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := -1;
end;

class function TCollection<T>.InternalTryGetFirst(const AEnumerator: IEnumerator<T>;
  var{out} Value: T): Boolean;
begin
  Result := AEnumerator.MoveNext;
  if (Result) then
  begin
    Value := AEnumerator.Current;
  end;
end;

class function TCollection<T>.InternalTryGetFirst(const AEnumerable: IEnumerable<T>;
  var{out} Value: T): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryGetFirst(LEnumerator, Value);
  end else
  begin
    Result := False;
  end;
end;

class function TCollection<T>.InternalTryGetFirst(const AEnumerator: IEnumerator<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  if (AEnumerator.MoveNext) then
  begin
    Value := AEnumerator.Current;
    if (APredicate(Value)) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

class function TCollection<T>.InternalTryGetFirst(const AEnumerable: IEnumerable<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryGetFirst(LEnumerator, Value, APredicate);
  end else
  begin
    Result := False;
  end;
end;

class function TCollection<T>.InternalGetLast(Value: P; Offset: NativeInt; Count: Integer;
  const APredicate: TFunction<T,Boolean>): Integer;
begin
  Inc(NativeInt(Value), Integer(Offset) * (Count - 1));
  for Result := Count - 1 downto 0 do
  begin
    if (APredicate(Value^)) then
      Exit;

    Dec(NativeInt(Value), Offset);
  end;

  Result := -1;
end;

class function TCollection<T>.InternalTryGetLast(const AEnumerator: IEnumerator<T>;
  var{out} Value: T): Boolean;
begin
  if (AEnumerator.MoveNext) then
  begin
    repeat
      Value := AEnumerator.Current;
    until (not AEnumerator.MoveNext);
    Result := True;
    Exit;
  end;

  Result := False;
end;

class function TCollection<T>.InternalTryGetLast(const AEnumerable: IEnumerable<T>;
  var{out} Value: T): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryGetLast(LEnumerator, Value);
  end else
  begin
    Result := False;
  end;
end;

class function TCollection<T>.InternalTryGetLast(const AEnumerator: IEnumerator<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  Temp: T;
begin
  Result := False;
  while (AEnumerator.MoveNext) do
  begin
    Temp := AEnumerator.Current;
    if (APredicate(Temp)) then
    begin
      Result := True;
      Value := Temp;
    end;
  end;
end;

class function TCollection<T>.InternalTryGetLast(const AEnumerable: IEnumerable<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryGetLast(LEnumerator, Value, APredicate);
  end else
  begin
    Result := False;
  end;
end;

class function TCollection<T>.InternalGetSingle(Value: P; Offset: NativeInt; Count: Integer;
  const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if (APredicate(Value^)) then
    begin
      if (Result < 0) and (not AFound) then
      begin
        Result := i;
      end else
      begin
        Result := -2;
        Exit;
      end;
    end;

    Inc(NativeInt(Value), Offset);
  end;
end;

class function TCollection<T>.InternalTryGetSingle(var AEnumerator: TCollectionEnumerator<T>;
  var{out} Value: T; const AFound: Boolean): Integer;
begin
  Result := Ord(AFound);

  while (AEnumerator.MoveNext) do
  begin
    Inc(Result);
    if (Result > 1) then
      Exit;

    Value := AEnumerator.Data.Current;
  end;
end;

class function TCollection<T>.InternalTryGetSingle(const ACollection: TCollection<T>;
  var{out} Value: T; const AFound: Boolean): Integer;
var
  LEnumerator: TCollectionEnumerator<T>;
begin
  LEnumerator := ACollection.Enumerator;
  Result := InternalTryGetSingle(LEnumerator, Value, AFound);
end;

class function TCollection<T>.InternalTryGetSingle(const ACollection: ICollection<T>;
  var{out} Value: T; const AFound: Boolean): Integer;
var
  LEnumerator: TCollectionEnumerator<T>;
begin
  LEnumerator := ACollection.Enumerator;
  Result := InternalTryGetSingle(LEnumerator, Value, AFound);
end;

class function TCollection<T>.InternalTryGetSingle(const AEnumerator: IEnumerator<T>;
  var{out} Value: T; const AFound: Boolean): Integer;
begin
  Result := Ord(AFound);

  while (AEnumerator.MoveNext) do
  begin
    Inc(Result);
    if (Result > 1) then
      Exit;

    Value := AEnumerator.Current;
  end;
end;

class function TCollection<T>.InternalTryGetSingle(const AEnumerable: IEnumerable<T>;
  var{out} Value: T; const AFound: Boolean): Integer;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryGetSingle(LEnumerator, Value, AFound);
  end else
  begin
    Result := Ord(AFound);
  end;
end;

class function TCollection<T>.InternalTryGetSingle(var AEnumerator: TCollectionEnumerator<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
begin
  Result := Ord(AFound);

  while (AEnumerator.MoveNext) do
  begin
    if (APredicate(AEnumerator.Data.Current)) then
    begin
      Inc(Result);
      if (Result > 1) then
        Exit;

      Value := AEnumerator.Data.Current;
    end;
  end;
end;

class function TCollection<T>.InternalTryGetSingle(const ACollection: TCollection<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
var
  LEnumerator: TCollectionEnumerator<T>;
begin
  LEnumerator := ACollection.Enumerator;
  Result := InternalTryGetSingle(LEnumerator, Value, APredicate, AFound);
end;

class function TCollection<T>.InternalTryGetSingle(const ACollection: ICollection<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
var
  LEnumerator: TCollectionEnumerator<T>;
begin
  LEnumerator := ACollection.Enumerator;
  Result := InternalTryGetSingle(LEnumerator, Value, APredicate, AFound);
end;

class function TCollection<T>.InternalTryGetSingle(const AEnumerator: IEnumerator<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
var
  Temp: T;
begin
  Result := Ord(AFound);

  while (AEnumerator.MoveNext) do
  begin
    Temp := AEnumerator.Current;
    if (APredicate(Temp)) then
    begin
      Inc(Result);
      if (Result > 1) then
        Exit;

      Value := Temp;
    end;
  end;
end;

class function TCollection<T>.InternalTryGetSingle(const AEnumerable: IEnumerable<T>;
  var{out} Value: T; const APredicate: TFunction<T,Boolean>; const AFound: Boolean): Integer;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalTryGetSingle(LEnumerator, Value, APredicate, AFound);
  end else
  begin
    Result := Ord(AFound);
  end;
end;

class function TCollection<T>.InternalIndexOf(const AValue: T; Value: P; Offset: NativeInt;
  Count: Integer; const AComparer: TComparison<T>): Integer;
begin
  if (TComparer<T>.IsDefaultComparison(AComparer)) then
  begin
    Result := InternalIndexOf(AValue, Value, Offset, Count,
      TEqualityComparison<T>(IInterface(InterfaceDefaults.TDefaultEqualityComparer<T>.Create)));
    Exit;
  end;

  for Result := 0 to Count - 1 do
  begin
    if (AComparer(AValue, Value^) = 0) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := -1;
end;

class function TCollection<T>.InternalIndexOf(const AValue: T; Value: P; Offset: NativeInt;
  Count: Integer; const AComparer: TEqualityComparison<T>): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if (AComparer(AValue, Value^)) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := -1;
end;

class function TCollection<T>.InternalIndexOf(const AValue: T; const AEnumerator: IEnumerator<T>;
  const AComparer: TEqualityComparison<T>): Integer;
begin
  Result := 0;
  while (AEnumerator.MoveNext) do
  begin
    if (AComparer(AValue, AEnumerator.Current)) then
      Exit;

    Inc(Result);
  end;

  Result := -1;
end;

class function TCollection<T>.InternalIndexOf(const AValue: T; const AEnumerable: IEnumerable<T>;
  const AComparer: TEqualityComparison<T>): Integer;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalIndexOf(AValue, LEnumerator, AComparer);
  end else
  begin
    Result := -1;
  end;
end;

class function TCollection<T>.InternalLastIndexOf(const AValue: T; Value: P; Offset: NativeInt;
  Count: Integer; const AComparer: TComparison<T>): Integer;
begin
  if (TComparer<T>.IsDefaultComparison(AComparer)) then
  begin
    Result := InternalLastIndexOf(AValue, Value, Offset, Count,
      TEqualityComparison<T>(IInterface(InterfaceDefaults.TDefaultEqualityComparer<T>.Create)));
    Exit;
  end;

  Inc(NativeInt(Value), Integer(Offset) * (Count - 1));
  for Result := Count - 1 downto 0 do
  begin
    if (AComparer(AValue, Value^) = 0) then
      Exit;

    Dec(NativeInt(Value), Offset);
  end;

  Result := -1;
end;

class function TCollection<T>.InternalLastIndexOf(const AValue: T; Value: P; Offset: NativeInt;
  Count: Integer; const AComparer: TEqualityComparison<T>): Integer;
begin
  Inc(NativeInt(Value), Integer(Offset) * (Count - 1));
  for Result := Count - 1 downto 0 do
  begin
    if (AComparer(AValue, Value^)) then
      Exit;

    Dec(NativeInt(Value), Offset);
  end;

  Result := -1;
end;

class function TCollection<T>.InternalLastIndexOf(const AValue: T; const AEnumerator: IEnumerator<T>;
  const AComparer: TEqualityComparison<T>): Integer;
var
  Index: Integer;
begin
  Result := -1;
  Index := 0;
  while (AEnumerator.MoveNext) do
  begin
    if (AComparer(AValue, AEnumerator.Current)) then
      Result := Index;

    Inc(Index);
  end;
end;

class function TCollection<T>.InternalLastIndexOf(const AValue: T; const AEnumerable: IEnumerable<T>;
  const AComparer: TEqualityComparison<T>): Integer;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := AEnumerable.GetEnumerator;
  if (Assigned(LEnumerator)) then
  begin
    Result := InternalLastIndexOf(AValue, LEnumerator, AComparer);
  end else
  begin
    Result := -1;
  end;
end;

class function TCollection<T>.InternalEqualsTo(Value1, Value2: P; Offset1, Offset2: NativeInt;
  Count: Integer; const AComparer: TEqualityComparison<T>): Boolean;
var
  i: Integer;
  LOffset2: NativeInt;
begin
  LOffset2 := Offset2;
  for i := 1 to Count do
  begin
    Result := AComparer(Value1^, Value2^);
    if (not Result) then
      Exit;

    Inc(NativeInt(Value1), Offset1);
    Inc(NativeInt(Value2), LOffset2);
  end;

  Result := True;
end;

class function TCollection<T>.InternalEqualsTo(Value: P; Offset: NativeInt; Count: Integer;
  var AEnumerator: TCollectionEnumerator<T>; const AComparer: TEqualityComparison<T>): Boolean;
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    Result := AEnumerator.MoveNext;
    if (not Result) then
      Exit;

    Result := AComparer(Value^, AEnumerator.Data.Current);
    if (not Result) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := True;
end;

class function TCollection<T>.InternalEqualsTo(Value: P; Offset: NativeInt; Count: Integer;
  const AEnumerator: IEnumerator<T>; const AComparer: TEqualityComparison<T>): Boolean;
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    Result := AEnumerator.MoveNext;
    if (not Result) then
      Exit;

    Result := AComparer(Value^, AEnumerator.Current);
    if (not Result) then
      Exit;

    Inc(NativeInt(Value), Offset);
  end;

  Result := True;
end;

constructor TCollection<T>.Create;
begin
  inherited Create;
  TRAIIHelper<T>.Create;
  FComparer := TComparer<T>.Default;
  FEqualityComparer := TEqualityComparer<T>.Default;
end;

function TCollection<T>.DoGetIsSynchronized: Boolean;
begin
  Result := False;
end;

function TCollection<T>.DoGetIsOrdered: Boolean;
begin
  Result := False;
end;

function TCollection<T>.DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean;
begin
  Result := False;
end;

function TCollection<T>.DoGetComparer: IComparer<T>;
begin
  Result := FComparer;
end;

function TCollection<T>.DoGetComparison: TComparison<T>;
begin
  Result := TComparison<T>(FComparer);
end;

function TCollection<T>.DoGetEqualityComparer: IEqualityComparer<T>;
begin
  Result := FEqualityComparer;
end;

function TCollection<T>.DoGetEqualityComparison: TEqualityComparison<T>;
begin
  Result := TEqualityComparison<T>(FEqualityComparer);
end;

function TCollection<T>.DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean;
begin
  Result := False;
end;

function TCollection<T>.ToArray: TArray<T>;
var
  Count, Buffered: NativeUInt;
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    LinearItems.ToArray(Result);
    Exit;
  end;

  Count := 0;
  Buffered := DoGetCount;
  Result := nil;
  SetLength(Result, Buffered);

  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
  begin
    if (Count = Buffered) then
    begin
      Buffered := Buffered * 2;
      SetLength(Result, Buffered);
    end;

    Result[Count] := Enumerator.Data.Current;
    Inc(Count);
  end;

  SetLength(Result, Count);
end;

function TCollection<T>.ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>;
var
  Count, Buffered: NativeUInt;
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    LinearItems.ToArray(Result, APredicate);
    Exit;
  end;

  Count := 0;
  Buffered := 16;
  Result := nil;
  SetLength(Result, Buffered);

  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
  begin
    if (APredicate(Enumerator.Data.Current)) then
    begin
      if (Count = Buffered) then
      begin
        Buffered := Buffered * 2;
        SetLength(Result, Buffered);
      end;

      Result[Count] := Enumerator.Data.Current;
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

function TCollection<T>.All(const APredicate: TFunction<T,Boolean>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.All(APredicate);
    Exit;
  end;

  Result := False;
  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
  begin
    if (not APredicate(Enumerator.Data.Current)) then
      Exit;
  end;

  Result := True;
end;

function TCollection<T>.Any(const APredicate: TFunction<T,Boolean>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.Any(APredicate);
    Exit;
  end;

  Result := True;
  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
  begin
    if (APredicate(Enumerator.Data.Current)) then
      Exit;
  end;

  Result := False;
end;

function TCollection<T>.Aggregate(const AFunc: TFunction<T,T,T>): T;
begin
  if (not TryAggregate(Result, AFunc)) then
    raise EItemNotFound;
end;

function TCollection<T>.TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(AFunc), 'Func');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    if (LinearItems.Count1 or LinearItems.Count2 = 0) then
    begin
      Result := False;
      Exit;
    end;

    Value := LinearItems.Aggregate(AFunc);
    Result := True;
    Exit;
  end;

  Enumerator := DoGetEnumerator;
  if (not Enumerator.MoveNext) then
  begin
    Result := False;
    Exit;
  end;

  Value := Enumerator.Data.Current;
  while (Enumerator.MoveNext) do
  begin
    Value := AFunc(Value, Enumerator.Data.Current);
  end;

  Result := True;
end;

function TCollection<T>.Min: T;
begin
  if (not TryGetMin(Result, TComparison<T>(FComparer))) then
    raise EItemNotFound;
end;

function TCollection<T>.Min(const AComparer: IComparer<T>): T;
begin
  if (not TryGetMin(Result, TComparison<T>(AComparer))) then
    raise EItemNotFound;
end;

function TCollection<T>.Min(const AComparer: TComparison<T>): T;
begin
  if (not TryGetMin(Result, AComparer)) then
    raise EItemNotFound;
end;

function TCollection<T>.Min(const ASelector: TFunction<T,Integer>): Integer;
begin
  if (not TryGetMin(Result, ASelector)) then
    raise EItemNotFound;
end;

function TCollection<T>.TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := (LinearItems.Count1 or LinearItems.Count2 <> 0);
    if (not Result) then
      Exit;

    Value := LinearItems.Min(AComparer);
    Result := True;
    Exit;
  end;

  Enumerator := DoGetEnumerator;
  Result := Enumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := Enumerator.Data.Current;
  while (Enumerator.MoveNext) do
  begin
    if (AComparer(Enumerator.Data.Current, Value) < 0) then
      Value := Enumerator.Data.Current;
  end;
  Result := True;
end;

function TCollection<T>.TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
  Temp: Integer;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := (LinearItems.Count1 or LinearItems.Count2 <> 0);
    if (not Result) then
      Exit;

    Value := LinearItems.Min(ASelector);
    Result := True;
    Exit;
  end;

  Enumerator := DoGetEnumerator;
  Result := Enumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := ASelector(Enumerator.Data.Current);
  while (Enumerator.MoveNext) do
  begin
    Temp := ASelector(Enumerator.Data.Current);
    if (Temp < Value) then
      Value := Temp;
  end;
  Result := True;
end;

function TCollection<T>.Max: T;
begin
  if (not TryGetMax(Result, TComparison<T>(FComparer))) then
    raise EItemNotFound;
end;

function TCollection<T>.Max(const AComparer: IComparer<T>): T;
begin
  if (not TryGetMax(Result, TComparison<T>(AComparer))) then
    raise EItemNotFound;
end;

function TCollection<T>.Max(const AComparer: TComparison<T>): T;
begin
  if (not TryGetMax(Result, AComparer)) then
    raise EItemNotFound;
end;

function TCollection<T>.Max(const ASelector: TFunction<T,Integer>): Integer;
begin
  if (not TryGetMax(Result, ASelector)) then
    raise EItemNotFound;
end;

function TCollection<T>.TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := (LinearItems.Count1 or LinearItems.Count2 <> 0);
    if (not Result) then
      Exit;

    Value := LinearItems.Max(AComparer);
    Result := True;
    Exit;
  end;

  Enumerator := DoGetEnumerator;
  Result := Enumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := Enumerator.Data.Current;
  while (Enumerator.MoveNext) do
  begin
    if (AComparer(Enumerator.Data.Current, Value) > 0) then
      Value := Enumerator.Data.Current;
  end;
  Result := True;
end;

function TCollection<T>.TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
  Temp: Integer;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := (LinearItems.Count1 or LinearItems.Count2 <> 0);
    if (not Result) then
      Exit;

    Value := LinearItems.Max(ASelector);
    Result := True;
    Exit;
  end;

  Enumerator := DoGetEnumerator;
  Result := Enumerator.MoveNext;
  if (not Result) then
    Exit;

  Value := ASelector(Enumerator.Data.Current);
  while (Enumerator.MoveNext) do
  begin
    Temp := ASelector(Enumerator.Data.Current);
    if (Temp > Value) then
      Value := Temp;
  end;
  Result := True;
end;

function TCollection<T>.Sum: T;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.Sum;
    Exit;
  end;

  Result := Default(T);
  Enumerator := DoGetEnumerator;
  case {$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} of
    tkInteger:
    begin
      case (GetTypeData(TypeInfo(T)).OrdType) of
        otSByte, otUByte: while (Enumerator.MoveNext) do PByte(@Result)^ := PByte(@Result)^ + PByte(@Enumerator.Data.Current)^;
        otSWord, otUWord: while (Enumerator.MoveNext) do PWord(@Result)^ := PWord(@Result)^ + PWord(@Enumerator.Data.Current)^;
        otSLong, otULong: while (Enumerator.MoveNext) do PCardinal(@Result)^ := PCardinal(@Result)^ + PCardinal(@Enumerator.Data.Current)^;
      end;
    end;
    tkInt64:
    begin
      while (Enumerator.MoveNext) do PInt64(@Result)^ := PInt64(@Result)^ + PInt64(@Enumerator.Data.Current)^;
    end;
    tkFloat:
    begin
      case (GetTypeData(TypeInfo(T)).FloatType) of
        ftSingle: while (Enumerator.MoveNext) do PSingle(@Result)^ := PSingle(@Result)^ + PSingle(@Enumerator.Data.Current)^;
        ftDouble: while (Enumerator.MoveNext) do PDouble(@Result)^ := PDouble(@Result)^ + PDouble(@Enumerator.Data.Current)^;
      ftExtended: while (Enumerator.MoveNext) do PExtended(@Result)^ := PExtended(@Result)^ + PExtended(@Enumerator.Data.Current)^;
      else
        while (Enumerator.MoveNext) do PInt64(@Result)^ := PInt64(@Result)^ + PInt64(@Enumerator.Data.Current)^;
      end;
    end;
  end;
end;

function TCollection<T>.Sum(const ASelector: TFunction<T,Integer>): Integer;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.Sum(ASelector);
    Exit;
  end;

  Result := 0;
  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
    Result := Result + ASelector(Enumerator.Data.Current);
end;

function TCollection<T>.Sum(const ASelector: TFunction<T,Int64>): Int64;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.Sum(ASelector);
    Exit;
  end;

  Result := 0;
  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
    Result := Result + ASelector(Enumerator.Data.Current);
end;

function TCollection<T>.Sum(const ASelector: TFunction<T,Extended>): Extended;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.Sum(ASelector);
    Exit;
  end;

  Result := 0;
  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
    Result := Result + ASelector(Enumerator.Data.Current);
end;

procedure TCollection<T>.ForEach(const AAction: TProcedure<T>);
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(AAction), 'Action');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    LinearItems.ForEach(AAction);
    Exit;
  end;

  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
    AAction(Enumerator.Data.Current);
end;

function TCollection<T>.ForEach(const AAction: TFunction<T,Boolean>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(AAction), 'Action');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.ForEach(AAction);
    Exit;
  end;

  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
  begin
    Result := AAction(Enumerator.Data.Current);
    if (not Result) then
      Exit;
  end;

  Result := True;
end;

function TCollection<T>.ElementAt(const AIndex: Integer): T;
begin
  if (not TryGetElementAt(Result, AIndex)) then
    raise EArgumentOutOfRange;
end;

function TCollection<T>.ElementAtOrDefault(const AIndex: Integer): T;
begin
  if (not TryGetElementAt(Result, AIndex)) then
    Result := Default(T);
end;

function TCollection<T>.ElementAtOrDefault(const AIndex: Integer; const ADefaultValue: T): T;
begin
  if (not TryGetElementAt(Result, AIndex)) then
    Result := ADefaultValue;
end;

function TCollection<T>.TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean;
var
  Counter: Integer;
  Enumerator: TCollectionEnumerator<T>;
  Count1, Count2: Integer;
  LinearItems: TLinearItems;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Count1 := LinearItems.Count1;
    Count2 := LinearItems.Count2;
    if (Cardinal(AIndex) < Cardinal(Count1 + Count2)) then
    begin
      if (AIndex < Count1) then
      begin
        Value := P(PByte(LinearItems.Values1) + LinearItems.Offset * AIndex)^;
        Result := True;
        Exit;
      end else
      begin
        Value := P(PByte(LinearItems.Values1) + LinearItems.Offset * (AIndex - Count1))^;
        Result := True;
        Exit;
      end;
    end;
  end else
  if (Cardinal(AIndex) < Cardinal(DoGetCount)) then
  begin
    Counter := AIndex;
    Enumerator := DoGetEnumerator;
    while (Enumerator.MoveNext) do
    begin
      if (Counter = 0) then
      begin
        Value := Enumerator.Data.Current;
        Result := True;
        Exit;
      end;
      Dec(Counter);
    end;
  end;

  Result := False;
end;

function TCollection<T>.First: T;
begin
  if (not TryGetFirst(Result)) then
    raise EItemNotFound;
end;

function TCollection<T>.FirstOrDefault: T;
begin
  if (not TryGetFirst(Result)) then
    Result := Default(T);
end;

function TCollection<T>.FirstOrDefault(const ADefaultValue: T): T;
begin
  if (not TryGetFirst(Result)) then
    Result := ADefaultValue;
end;

function TCollection<T>.TryGetFirst(var{out} Value: T): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    if (LinearItems.Count1 <> 0) then
    begin
      Value := LinearItems.Values1^;
      Result := True;
      Exit;
    end else
    if (LinearItems.Count2 <> 0) then
    begin
      Value := LinearItems.Values2^;
      Result := True;
      Exit;
    end else
    begin
      Result := False;
      Exit;
    end;
  end else
  begin
    Enumerator := DoGetEnumerator;
    Result := Enumerator.MoveNext;
    if (Result) then
      Value := Enumerator.Data.Current;
  end;
end;

function TCollection<T>.First(const APredicate: TFunction<T,Boolean>): T;
begin
  if (not TryGetFirst(Result, APredicate)) then
    raise EItemNotFound;
end;

function TCollection<T>.FirstOrDefault(const APredicate: TFunction<T,Boolean>): T;
begin
  if (not TryGetFirst(Result, APredicate)) then
    Result := Default(T);
end;

function TCollection<T>.FirstOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T;
begin
  if (not TryGetFirst(Result, APredicate)) then
    Result := ADefaultValue;
end;

function TCollection<T>.TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.TryGetFirst(Value, APredicate);
  end else
  begin
    Enumerator := DoGetEnumerator;
    if (Enumerator.MoveNext) then
    begin
      if (APredicate(Enumerator.Data.Current)) then
      begin
        Value := Enumerator.Data.Current;
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;
end;

function TCollection<T>.Last: T;
begin
  if (not TryGetLast(Result)) then
    raise EItemNotFound;
end;

function TCollection<T>.LastOrDefault: T;
begin
  if (not TryGetLast(Result)) then
    Result := Default(T);
end;

function TCollection<T>.LastOrDefault(const ADefaultValue: T): T;
begin
  if (not TryGetLast(Result)) then
    Result := ADefaultValue;
end;

function TCollection<T>.TryGetLast(var{out} Value: T): Boolean;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    if (LinearItems.Count2 <> 0) then
    begin
      Value := P(PByte(LinearItems.Values2) + (LinearItems.Count2 - 1) * LinearItems.Offset)^;
      Result := True;
      Exit;
    end else
    if (LinearItems.Count1 <> 0) then
    begin
      Value := P(PByte(LinearItems.Values1) + (LinearItems.Count1 - 1) * LinearItems.Offset)^;
      Result := True;
      Exit;
    end;
  end else
  if (not DoGetIsEmpty) then
  begin
    if (DoTryGetReversedEnumerator(Enumerator)) then
    begin
      if (Enumerator.MoveNext) then
      begin
        Value := Enumerator.Data.Current;
        Result := True;
        Exit;
      end;
    end else
    begin
      Enumerator := DoGetEnumerator;
      if (Enumerator.MoveNext) then
      begin
        repeat
          Value := Enumerator.Data.Current;
        until (not Enumerator.MoveNext);
        Result := True;
        Exit;
      end;
    end;
  end;

  Result := False;
end;

function TCollection<T>.Last(const APredicate: TFunction<T,Boolean>): T;
begin
  if (not TryGetLast(Result, APredicate)) then
    raise EItemNotFound;
end;

function TCollection<T>.LastOrDefault(const APredicate: TFunction<T,Boolean>): T;
begin
  if (not TryGetLast(Result, APredicate)) then
    Result := Default(T);
end;

function TCollection<T>.LastOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T;
begin
  if (not TryGetLast(Result, APredicate)) then
    Result := ADefaultValue;
end;

function TCollection<T>.TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  Found: Boolean;
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.TryGetLast(Value, APredicate);
    Exit;
  end else
  if (not DoGetIsEmpty) then
  begin
    if (DoTryGetReversedEnumerator(Enumerator)) then
    begin
      while (Enumerator.MoveNext) do
      begin
        if (APredicate(Enumerator.Data.Current)) then
        begin
          Value := Enumerator.Data.Current;
          Result := True;
          Exit;
        end;
      end;
    end else
    begin
      Found := False;

      Enumerator := DoGetEnumerator;
      while (Enumerator.MoveNext) do
      begin
        if (APredicate(Enumerator.Data.Current)) then
        begin
          Found := True;
          Value := Enumerator.Data.Current;
        end;
      end;

      Result := True;
      if (Found) then
        Exit;
    end;
  end;

  Result := False;
end;

function TCollection<T>.InternalTryGetSingle(var{out} Value: T): Integer;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.Count1 + LinearItems.Count2;
    if (Result = 1) then
    begin
      if (LinearItems.Count1 = 1) then
      begin
        Value := LinearItems.Values1^;
        Exit;
      end else
      begin
        Value := LinearItems.Values2^;
        Exit;
      end;
    end;
  end else
  if (not DoGetIsEmpty) then
  begin
    Enumerator := DoGetEnumerator;
    if (Enumerator.MoveNext) then
    begin
      Value := Enumerator.Data.Current;
      Result := Ord(Enumerator.MoveNext) + 1;
      Exit;
    end;
  end;

  Result := 0;
end;

function TCollection<T>.InternalTryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Integer;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.TryGetSingle(Value, APredicate, False);
    Exit;
  end;

  Result := 0;
  if (not DoGetIsEmpty) then
  begin
    Enumerator := DoGetEnumerator;
    while (Enumerator.MoveNext) do
    begin
      if (APredicate(Enumerator.Data.Current)) then
      begin
        Inc(Result);
        if (Result > 1) then
          Break;

        Value := Enumerator.Data.Current;
      end;
    end;
  end;
end;

function TCollection<T>.Single: T;
begin
  case InternalTryGetSingle(Result) of
    0: raise EItemNotFound;
    1: ;
  else
    raise EDuplicatesNotAllowed;
  end;
end;

function TCollection<T>.SingleOrDefault: T;
begin
  case InternalTryGetSingle(Result) of
    1: ;
  else
    Result := Default(T);
  end;
end;

function TCollection<T>.SingleOrDefault(const ADefaultValue: T): T;
begin
  case InternalTryGetSingle(Result) of
    1: ;
  else
    Result := ADefaultValue;
  end;
end;

function TCollection<T>.TryGetSingle(var{out} Value: T): Boolean;
begin
  Result := (InternalTryGetSingle(Value) = 1);
end;

function TCollection<T>.Single(const APredicate: TFunction<T,Boolean>): T;
begin
  case InternalTryGetSingle(Result, APredicate) of
    0: raise EItemNotFound;
    1: ;
  else
    raise EDuplicatesNotAllowed;
  end;
end;

function TCollection<T>.SingleOrDefault(const APredicate: TFunction<T,Boolean>): T;
begin
  case InternalTryGetSingle(Result, APredicate) of
    1: ;
  else
    Result := Default(T);
  end;
end;

function TCollection<T>.SingleOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T;
begin
  case InternalTryGetSingle(Result, APredicate) of
    1: ;
  else
    Result := ADefaultValue;
  end;
end;

function TCollection<T>.TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := (InternalTryGetSingle(Value, APredicate) = 1);
end;

function TCollection<T>.Contains(const AValue: T): Boolean;
begin
  Result := (IndexOf(AValue, TEqualityComparison<T>(FEqualityComparer)) >= 0);
end;

function TCollection<T>.Contains(const AValue: T; const AComparer: IEqualityComparer<T>): Boolean;
begin
  Result := (IndexOf(AValue, TEqualityComparison<T>(AComparer)) >= 0);
end;

function TCollection<T>.Contains(const AValue: T; const AComparer: TEqualityComparison<T>): Boolean;
begin
  Result := (IndexOf(AValue, AComparer) >= 0);
end;

function TCollection<T>.IndexOf(const AValue: T): Integer;
begin
  Result := IndexOf(AValue, TEqualityComparison<T>(FEqualityComparer));
end;

function TCollection<T>.IndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer;
begin
  Result := IndexOf(AValue, TEqualityComparison<T>(AComparer));
end;

function TCollection<T>.IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
var
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.IndexOf(AValue, AComparer);
    Exit;
  end;

  Result := 0;
  Enumerator := DoGetEnumerator;
  while (Enumerator.MoveNext) do
  begin
    if (AComparer(AValue, Enumerator.Data.Current)) then
      Exit;

    Inc(Result);
  end;

  Result := -1;
end;

function TCollection<T>.LastIndexOf(const AValue: T): Integer;
begin
  Result := LastIndexOf(AValue, TEqualityComparison<T>(FEqualityComparer));
end;

function TCollection<T>.LastIndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer;
begin
  Result := LastIndexOf(AValue, TEqualityComparison<T>(AComparer));
end;

function TCollection<T>.LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
var
  Index: Integer;
  Enumerator: TCollectionEnumerator<T>;
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (DoTryGetLinearItems(LinearItems)) then
  begin
    Result := LinearItems.LastIndexOf(AValue, AComparer);
    Exit;
  end;

  if (not DoGetIsEmpty) then
  begin
    if (DoTryGetReversedEnumerator(Enumerator)) then
    begin
      Result := DoGetCount - 1;
      while (Index >= 0) and (Enumerator.MoveNext) do
      begin
        if (AComparer(AValue, Enumerator.Data.Current)) then
          Exit;

        Dec(Result);
      end;
    end else
    begin
      Result := -1;
      Index := 0;
      Enumerator := DoGetEnumerator;
      while (Enumerator.MoveNext) do
      begin
        if (AComparer(AValue, Enumerator.Data.Current)) then
          Result := Index;

        Inc(Index);
      end;
      Exit;
    end;
  end;

  Result := -1;
end;

function TCollection<T>.EqualsTo(const AValues: array of T): Boolean;
begin
  Result := EqualsTo(AValues, TEqualityComparison<T>(FEqualityComparer));
end;

function TCollection<T>.EqualsTo(const AValues: array of T; const AComparer: IEqualityComparer<T>): Boolean;
begin
  Result := EqualsTo(AValues, TEqualityComparison<T>(AComparer));
end;

function TCollection<T>.EqualsTo(const AValues: array of T; const AComparer: TEqualityComparison<T>): Boolean;
var
  LCount: Integer;
  LLeftItems, LRightItems: TLinearItems;
  Enumerator: TCollectionEnumerator<T>;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');
  LCount := Length(AValues);
  Result := (LCount = DoGetCount);
  if (Result) and (LCount <> 0) then
  begin
    LRightItems.Values1 := Pointer(@AValues);
    LRightItems.Count1 := LCount;
    LRightItems.Count2 := 0;
    LRightItems.Offset := SizeOf(T);

    if (Self.DoTryGetLinearItems(LLeftItems)) then
    begin
      Result := ((LLeftItems.Count1 + LLeftItems.Count2) = LCount) and
        LLeftItems.EqualsTo(LRightItems, AComparer);
    end else
    begin
      Enumerator := DoGetEnumerator;
      Result := LRightItems.EqualsTo(Enumerator, AComparer) and (not Enumerator.MoveNext);
    end;
  end;
end;

function TCollection<T>.EqualsTo(const ACollection: TCollection<T>): Boolean;
begin
  Result := EqualsTo(ACollection, TEqualityComparison<T>(FEqualityComparer));
end;

function TCollection<T>.EqualsTo(const ACollection: TCollection<T>; const AComparer: IEqualityComparer<T>): Boolean;
begin
  Result := EqualsTo(ACollection, TEqualityComparison<T>(AComparer));
end;

function TCollection<T>.EqualsTo(const ACollection: TCollection<T>; const AComparer: TEqualityComparison<T>): Boolean;
var
  LCount: Integer;
  LLeftItems, LRightItems: TLinearItems;
  LLeftEnumerator, LRightEnumerator: TCollectionEnumerator<T>;
begin
  CheckArgumentIsNil(Assigned(ACollection), 'Collection');
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');
  if (Self = ACollection) then
  begin
    Result := True;
    Exit;
  end;

  LCount := ACollection.Count;
  Result := (LCount = DoGetCount);
  if (Result) and (LCount <> 0) then
  begin
    if (Self.DoTryGetLinearItems(LLeftItems)) then
    begin
      if (ACollection.DoTryGetLinearItems(LRightItems)) then
      begin
        Result := ((LLeftItems.Count1 + LLeftItems.Count2) = (LRightItems.Count1 + LRightItems.Count2)) and
          LLeftItems.EqualsTo(LRightItems, AComparer);
      end else
      begin
        LRightEnumerator := ACollection.DoGetEnumerator;
        Result := LLeftItems.EqualsTo(LRightEnumerator, AComparer) and (not LRightEnumerator.MoveNext);
      end;
    end else
    if (ACollection.DoTryGetLinearItems(LRightItems)) then
    begin
      LLeftEnumerator := Self.DoGetEnumerator;
      Result := LRightItems.EqualsTo(LLeftEnumerator, AComparer) and (not LLeftEnumerator.MoveNext);
    end else
    begin
      LLeftEnumerator := Self.DoGetEnumerator;
      LRightEnumerator := ACollection.DoGetEnumerator;

      while (LLeftEnumerator.MoveNext) do
      begin
        if (not LRightEnumerator.MoveNext) or
          (not AComparer(LLeftEnumerator.Data.Current, LRightEnumerator.Data.Current)) then
        begin
          Result := False;
          Exit;
        end;
      end;

      Result := (not LRightEnumerator.MoveNext);
    end;
  end;
end;

function TCollection<T>.EqualsTo(const ACollection: ICollection<T>): Boolean;
begin
  Result := EqualsTo(ACollection, TEqualityComparison<T>(FEqualityComparer));
end;

function TCollection<T>.EqualsTo(const ACollection: ICollection<T>; const AComparer: IEqualityComparer<T>): Boolean;
begin
  Result := EqualsTo(ACollection, TEqualityComparison<T>(AComparer));
end;

function TCollection<T>.EqualsTo(const ACollection: ICollection<T>; const AComparer: TEqualityComparison<T>): Boolean;
var
  LCount: Integer;
  LinearItems: TLinearItems;
  LLeftEnumerator, LRightEnumerator: TCollectionEnumerator<T>;
begin
  if (ACollection.Self is TCollection<T>) then
  begin
    Result := EqualsTo(TCollection<T>(ACollection.Self), AComparer);
    Exit;
  end;

  CheckArgumentIsNil(Assigned(ACollection), 'Collection');
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  LCount := ACollection.Count;
  Result := (LCount = DoGetCount);
  if (Result) and (LCount <> 0) then
  begin
    if (Self.DoTryGetLinearItems(LinearItems)) then
    begin
      LRightEnumerator := ACollection.GetEnumerator;
      Result := LinearItems.EqualsTo(LRightEnumerator, AComparer) and (not LRightEnumerator.MoveNext);
    end else
    begin
      LLeftEnumerator := Self.DoGetEnumerator;
      LRightEnumerator := ACollection.GetEnumerator;

      while (LLeftEnumerator.MoveNext) do
      begin
        if (not LRightEnumerator.MoveNext) or
          (not AComparer(LLeftEnumerator.Data.Current, LRightEnumerator.Data.Current)) then
        begin
          Result := False;
          Exit;
        end;
      end;

      Result := (not LRightEnumerator.MoveNext);
    end;
  end;
end;

function TCollection<T>.EqualsTo(const AEnumerable: IEnumerable<T>): Boolean;
begin
  Result := EqualsTo(AEnumerable, TEqualityComparison<T>(FEqualityComparer));
end;

function TCollection<T>.EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: IEqualityComparer<T>): Boolean;
begin
  Result := EqualsTo(AEnumerable, TEqualityComparison<T>(AComparer));
end;

function TCollection<T>.EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: TEqualityComparison<T>): Boolean;
var
  LinearItems: TLinearItems;
  LLeftEnumerator: TCollectionEnumerator<T>;
  LRightEnumerator: IEnumerator<T>;
begin
  if (AEnumerable is TCollection<T>) then
  begin
    Result := EqualsTo(AEnumerable as TCollection<T>, AComparer);
    Exit;
  end;

  CheckArgumentIsNil(Assigned(AEnumerable), 'Enumerable');
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (Self.DoTryGetLinearItems(LinearItems)) then
  begin
    LRightEnumerator := AEnumerable.GetEnumerator;
    Result := LinearItems.EqualsTo(LRightEnumerator, AComparer) and (not LRightEnumerator.MoveNext);
  end else
  begin
    LLeftEnumerator := Self.DoGetEnumerator;
    LRightEnumerator := AEnumerable.GetEnumerator;

    while (LLeftEnumerator.MoveNext) do
    begin
      if (not LRightEnumerator.MoveNext) or
        (not AComparer(LLeftEnumerator.Data.Current, LRightEnumerator.Current)) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := (not LRightEnumerator.MoveNext);
  end;
end;

function TCollection<T>.Concat(const ACollection: TCollection<T>): ICollection<T>;
begin
  CheckArgumentIsNil(Assigned(ACollection), 'Collection');
  Result := TConcatedCollection<T>.Create(Self, Pointer(ACollection), smInstance);
end;

function TCollection<T>.Concat(const ACollection: ICollection<T>): ICollection<T>;
begin
  CheckArgumentIsNil(Assigned(ACollection), 'Collection');
  Result := TConcatedCollection<T>.Create(Self, Pointer(ACollection), smInterface);
end;

function TCollection<T>.Concat(const AEnumerable: IEnumerable<T>): ICollection<T>;
begin
  CheckArgumentIsNil(Assigned(AEnumerable), 'Enumerable');
  Result := TConcatedCollection<T>.Create(Self, Pointer(AEnumerable), smEnumerable);
end;

function TCollection<T>.Where(const APredicate: TFunction<T,Boolean>): ICollection<T>;
var
  LArray: TArray<T>;
begin
  LArray := ToArray(APredicate);
  Result := TArrayCollection<T>.Create(LArray, FComparer, FEqualityComparer);
end;

function TCollection<T>.Ordered: ICollection<T>;
begin
  Result := Ordered(TComparison<T>(FComparer));
end;

function TCollection<T>.Ordered(const AComparer: IComparer<T>): ICollection<T>;
var
  LArray: TArray<T>;
begin
  if (DoGetIsOrdered) then
  begin
    Result := TProxyCollection<T>.Create(Self);
  end else
  begin
    LArray := ToArray;
    TArray.Sort<T>(LArray, AComparer);
    Result := TArrayCollection<T>.Create(LArray, AComparer, FEqualityComparer);
  end;
end;

function TCollection<T>.Ordered(const AComparer: TComparison<T>): ICollection<T>;
begin
  Result := Ordered(IComparer<T>(@AComparer));
end;

function TCollection<T>.Reversed: ICollection<T>;
var
  LArray: TArray<T>;
begin
  if (DoTryGetReversedEnumerator(TCollectionEnumerator<T>(nil^))) then
  begin
    Result := TReversedCollection<T>.Create(Self);
  end else
  begin
    LArray := ToArray;
    TArray.Reverse<T>(LArray);
    Result := TArrayCollection<T>.Create(LArray, FComparer, FEqualityComparer);
  end;
end;

function TCollection<T>.Shuffled: ICollection<T>;
var
  LArray: TArray<T>;
begin
  LArray := ToArray;
  TArray.Shuffle<T>(LArray);
  Result := TArrayCollection<T>.Create(LArray, FComparer, FEqualityComparer);
end;


{ TCustomListCollection<T1,T,T3,T4>.TEnumerator }

{$if CompilerVersion <= 22}
function TCustomListCollection<T1,T,T3,T4>.TEnumerator.GetCurrent: T;
begin
  Result := Data.Current;
end;
{$ifend}

function TCustomListCollection<T1,T,T3,T4>.TEnumerator.MoveNext: Boolean;
var
  LIndex: NativeUInt;
begin
  LIndex := NativeUInt(Data.Tag) + 1;
  with TCustomListCollection<T1,T,T3,T4>(Data.Owner) do
  begin
    if (LIndex < NativeUInt(FCount.Native)) then
    begin
      Data.Tag := LIndex;
      Data.Current := FItems[LIndex].Field2;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TCustomListCollection<T1,T,T3,T4>.TEnumerator.ReversedMoveNext: Boolean;
var
  LIndex: NativeUInt;
begin
  LIndex := NativeUInt(Data.Tag) - 1;
  with TCustomListCollection<T1,T,T3,T4>(Data.Owner) do
  begin
    if (LIndex < NativeUInt(FCount.Native)) then
    begin
      Data.Tag := LIndex;
      Data.Current := FItems[LIndex].Field2;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

{ TCustomListCollection<T1,T,T3,T4> }

function TCustomListCollection<T1,T,T3,T4>.DoGetCount: Integer;
begin
  Result := FCount.Int;
end;

function TCustomListCollection<T1,T,T3,T4>.DoGetIsEmpty: Boolean;
begin
  Result := (FCount.Native = 0);
end;

function TCustomListCollection<T1,T,T3,T4>.GetIsEmpty: Boolean;
begin
  Result := (FCount.Native = 0);
end;

function TCustomListCollection<T1,T,T3,T4>.DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean;
begin
  ALinearItems.Values1 := @FItems[0].Field2;
  ALinearItems.Count1 := FCount.Int;
  ALinearItems.Offset := SizeOf(TItem);
  ALinearItems.Count2 := 0;
  Result := True;
end;

function TCustomListCollection<T1,T,T3,T4>.DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean;
begin
  if (Assigned(@Enumerator)) then
  begin
    Enumerator.Data.Init(Self);
    Enumerator.Data.Tag := FCount.Native;
    Pointer(@Enumerator.DoMoveNext) := @TEnumerator.ReversedMoveNext;
  end;
  Result := True;
end;

function TCustomListCollection<T1,T,T3,T4>.DoGetEnumerator: TCollectionEnumerator<T>;
begin
  Result.Data.Init(Self);
  Pointer(@Result.DoMoveNext) := @TEnumerator.MoveNext;
end;

function TCustomListCollection<T1,T,T3,T4>.GetEnumerator: TEnumerator;
begin
  Result.Data.Init(Self);
end;

function TCustomListCollection<T1,T,T3,T4>.ToArray: TArray<T>;
var
  LinearItems: TLinearItems;
begin
  if (FCount.Int <> 0) then
  begin
    LinearItems.Values1 := @FItems[0].Field2;
    LinearItems.Count1 := FCount.Int;
    LinearItems.Offset := SizeOf(TItem);
    LinearItems.Count2 := 0;
    LinearItems.ToArray(Result);
    Exit;
  end else
  begin
    if (Pointer(Result) <> nil) then
      Result := nil;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>;
var
  LinearItems: TLinearItems;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (FCount.Int <> 0) then
  begin
    LinearItems.Values1 := @FItems[0].Field2;
    LinearItems.Count1 := FCount.Int;
    LinearItems.Offset := SizeOf(TItem);
    LinearItems.Count2 := 0;
    LinearItems.ToArray(Result, APredicate);
    Exit;
  end else
  begin
    if (Pointer(Result) <> nil) then
      Result := nil;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.All(const APredicate: TFunction<T,Boolean>): Boolean;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');
  Result := (FCount.Int = 0) or InternalAll(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate);
end;

function TCustomListCollection<T1,T,T3,T4>.Any(const APredicate: TFunction<T,Boolean>): Boolean;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');
  Result := (FCount.Int <> 0) and InternalAll(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate);
end;

function TCustomListCollection<T1,T,T3,T4>.Aggregate(const AFunc: TFunction<T,T,T>): T;
begin
  CheckArgumentIsNil(Assigned(AFunc), 'Func');

  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end;
  else
    Result := InternalAggregate(@FItems[1].Field2, SizeOf(TItem), FCount.Int, FItems[0].Field2, AFunc);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean;
begin
  CheckArgumentIsNil(Assigned(AFunc), 'Func');

  case (FCount.Native) of
    0:
    begin
      Result := False;
      Exit;
    end;
    1:
    begin
      Value := FItems[0].Field2;
      Result := True;
      Exit;
    end;
  else
    Value := InternalAggregate(@FItems[1].Field2, SizeOf(TItem), FCount.Int, FItems[0].Field2, AFunc);
    Result := True;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Min: T;
begin
  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end;
  else
    Result := InternalMin(@FItems[0].Field2, SizeOf(TItem), FCount.Int, TComparison<T>(FComparer));
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Min(const AComparer: IComparer<T>): T;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end;
  else
    Result := InternalMin(@FItems[0].Field2, SizeOf(TItem), FCount.Int, TComparison<T>(AComparer));
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Min(const AComparer: TComparison<T>): T;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end;
  else
    Result := InternalMin(@FItems[0].Field2, SizeOf(TItem), FCount.Int, AComparer);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Min(const ASelector: TFunction<T,Integer>): Integer;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := ASelector(FItems[0].Field2);
      Exit;
    end;
  else
    Result := InternalMin(@FItems[0].Field2, SizeOf(TItem), FCount.Int, ASelector);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  case (FCount.Native) of
    0:
    begin
      Result := False;
      Exit;
    end;
    1:
    begin
      Value := FItems[0].Field2;
      Result := True;
      Exit;
    end;
  else
    Value := InternalMin(@FItems[0].Field2, SizeOf(TItem), FCount.Int, AComparer);
    Result := True;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  case (FCount.Native) of
    0:
    begin
      Result := False;
      Exit;
    end;
    1:
    begin
      Value := ASelector(FItems[0].Field2);
      Result := True;
      Exit;
    end;
  else
    Value := InternalMin(@FItems[0].Field2, SizeOf(TItem), FCount.Int, ASelector);
    Result := True;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Max: T;
begin
  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end
  else
    Result := InternalMax(@FItems[0].Field2, SizeOf(TItem), FCount.Int, TComparison<T>(FComparer));
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Max(const AComparer: IComparer<T>): T;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end;
  else
    Result := InternalMax(@FItems[0].Field2, SizeOf(TItem), FCount.Int, TComparison<T>(AComparer));
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Max(const AComparer: TComparison<T>): T;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end;
  else
    Result := InternalMax(@FItems[0].Field2, SizeOf(TItem), FCount.Int, AComparer);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Max(const ASelector: TFunction<T,Integer>): Integer;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  case (FCount.Native) of
    0: raise EItemNotFound;
    1:
    begin
      Result := ASelector(FItems[0].Field2);
      Exit;
    end;
  else
    Result := InternalMax(@FItems[0].Field2, SizeOf(TItem), FCount.Int, ASelector);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  case (FCount.Native) of
    0:
    begin
      Result := False;
      Exit;
    end;
    1:
    begin
      Value := FItems[0].Field2;
      Result := True;
      Exit;
    end;
  else
    Value := InternalMax(@FItems[0].Field2, SizeOf(TItem), FCount.Int, AComparer);
    Result := True;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  case (FCount.Native) of
    0:
    begin
      Result := False;
      Exit;
    end;
    1:
    begin
      Value := ASelector(FItems[0].Field2);
      Result := True;
      Exit;
    end;
  else
    Value := InternalMax(@FItems[0].Field2, SizeOf(TItem), FCount.Int, ASelector);
    Result := True;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Sum: T;
var
  LinearItems: TLinearItems;
begin
  if (FCount.Int <> 0) then
  begin
    LinearItems.Values1 := @FItems[0].Field2;
    LinearItems.Count1 := FCount.Int;
    LinearItems.Offset := SizeOf(TItem);
    LinearItems.Count2 := 0;
    Result := LinearItems.Sum;
    Exit;
  end else
  begin
    Result := Default(T);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Sum(const ASelector: TFunction<T,Integer>): Integer;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (FCount.Int <> 0) then
  begin
    Result := InternalSum(@FItems[0].Field2, SizeOf(TItem), FCount.Int, ASelector);
    Exit;
  end else
  begin
    Result := 0;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Sum(const ASelector: TFunction<T,Int64>): Int64;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (FCount.Int <> 0) then
  begin
    Result := InternalSum(@FItems[0].Field2, SizeOf(TItem), FCount.Int, ASelector);
    Exit;
  end else
  begin
    Result := 0;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Sum(const ASelector: TFunction<T,Extended>): Extended;
begin
  CheckArgumentIsNil(Assigned(ASelector), 'Selector');

  if (FCount.Int <> 0) then
  begin
    Result := InternalSum(@FItems[0].Field2, SizeOf(TItem), FCount.Int, ASelector);
    Exit;
  end else
  begin
    Result := 0;
  end;
end;

procedure TCustomListCollection<T1,T,T3,T4>.ForEach(const AAction: TProcedure<T>);
begin
  CheckArgumentIsNil(Assigned(AAction), 'Action');

  if (FCount.Int <> 0) then
  begin
    InternalForEach(@FItems[0].Field2, SizeOf(TItem), FCount.Int, AAction);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.ForEach(const AAction: TFunction<T,Boolean>): Boolean;
begin
  CheckArgumentIsNil(Assigned(AAction), 'Action');

  if (FCount.Int <> 0) then
  begin
    Result := InternalForEach(@FItems[0].Field2, SizeOf(TItem), FCount.Int, AAction);
    Exit;
  end else
  begin
    Result := True;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.GetElementAt(const AIndex: Integer): T;
begin
  if (Cardinal(AIndex) < Cardinal(FCount.Int)) then
  begin
    Result := FItems[Cardinal(AIndex)].Field2;
    Exit;
  end else
  begin
    raise EItemNotFound;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.ElementAt(const AIndex: Integer): T;
begin
  if (Cardinal(AIndex) < Cardinal(FCount.Int)) then
  begin
    Result := FItems[Cardinal(AIndex)].Field2;
    Exit;
  end else
  begin
    raise EItemNotFound;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.ElementAtOrDefault(const AIndex: Integer): T;
begin
  if (Cardinal(AIndex) < Cardinal(FCount.Int)) then
  begin
    Result := FItems[Cardinal(AIndex)].Field2;
    Exit;
  end else
  begin
    Result := Default(T);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.ElementAtOrDefault(const AIndex: Integer; const ADefaultValue: T): T;
begin
  if (Cardinal(AIndex) < Cardinal(FCount.Int)) then
  begin
    Result := FItems[Cardinal(AIndex)].Field2;
    Exit;
  end else
  begin
    Result := ADefaultValue;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean;
begin
  if (Cardinal(AIndex) < Cardinal(FCount.Int)) then
  begin
    Value := FItems[Cardinal(AIndex)].Field2;
    Result := True;
    Exit;
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.First: T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[0].Field2;
    Exit;
  end else
  begin
    raise EItemNotFound;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.FirstOrDefault: T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[0].Field2;
    Exit;
  end else
  begin
    Result := Default(T);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.FirstOrDefault(const ADefaultValue: T): T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[0].Field2;
    Exit;
  end else
  begin
    Result := ADefaultValue;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetFirst(var{out} Value: T): Boolean;
begin
  if (FCount.Native <> 0) then
  begin
    Value := FItems[0].Field2;
    Result := True;
    Exit;
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  LIndex: Integer;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (FCount.Native <> 0) then
  begin
    LIndex := InternalGetFirst(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate);
    if (LIndex >= 0) then
    begin
      Value := FItems[Cardinal(LIndex)].Field2;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TCustomListCollection<T1,T,T3,T4>.Last: T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[FCount.Native - 1].Field2;
    Exit;
  end else
  begin
    raise EItemNotFound;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.LastOrDefault: T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[FCount.Native - 1].Field2;
    Exit;
  end else
  begin
    Result := Default(T);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.LastOrDefault(const ADefaultValue: T): T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[FCount.Native - 1].Field2;
    Exit;
  end else
  begin
    Result := ADefaultValue;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetLast(var{out} Value: T): Boolean;
begin
  if (FCount.Native <> 0) then
  begin
    Value := FItems[FCount.Native - 1].Field2;
    Result := True;
    Exit;
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  LIndex: Integer;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  if (FCount.Native <> 0) then
  begin
    LIndex := InternalGetLast(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate);
    if (LIndex >= 0) then
    begin
      Value := FItems[Cardinal(LIndex)].Field2;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function TCustomListCollection<T1,T,T3,T4>.Single: T;
begin
  case FCount.Native of
    0: raise EItemNotFound;
    1:
    begin
      Result := FItems[0].Field2;
      Exit;
    end
  else
    raise EDuplicatesNotAllowed;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.SingleOrDefault: T;
begin
  if (FCount.Native = 1) then
  begin
    Result := FItems[0].Field2;
    Exit;
  end else
  begin
    Result := Default(T);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.SingleOrDefault(const ADefaultValue: T): T;
begin
  if (FCount.Native = 1) then
  begin
    Result := FItems[0].Field2;
    Exit;
  end else
  begin
    Result := ADefaultValue;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetSingle(var{out} Value: T): Boolean;
begin
  if (FCount.Native = 1) then
  begin
    Value := FItems[0].Field2;
    Result := True;
    Exit;
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Single(const APredicate: TFunction<T,Boolean>): T;
var
  LIndex: Integer;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  LIndex := InternalGetSingle(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate, False);
  case (LIndex) of
    -1: raise EItemNotFound;
    -2: raise EDuplicatesNotAllowed;
  else
    Result := FItems[Cardinal(LIndex)].Field2;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.SingleOrDefault(const APredicate: TFunction<T,Boolean>): T;
var
  LIndex: Integer;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  LIndex := InternalGetSingle(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate, False);
  if (LIndex >= 0) then
  begin
    Result := FItems[Cardinal(LIndex)].Field2;
  end else
  begin
    Result := Default(T);
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.SingleOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T;
var
  LIndex: Integer;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  LIndex := InternalGetSingle(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate, False);
  if (LIndex >= 0) then
  begin
    Result := FItems[Cardinal(LIndex)].Field2;
  end else
  begin
    Result := ADefaultValue;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
var
  LIndex: Integer;
begin
  CheckArgumentIsNil(Assigned(APredicate), 'Predicate');

  LIndex := InternalGetSingle(@FItems[0].Field2, SizeOf(TItem), FCount.Int, APredicate, False);
  if (LIndex >= 0) then
  begin
    Value := FItems[Cardinal(LIndex)].Field2;
    Result := True;
    Exit;
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Contains(const AValue: T): Boolean;
begin
  if (FCount.Int <> 0) then
  begin
    Result := (InternalIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      TEqualityComparison<T>(FEqualityComparer)) >= 0);
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Contains(const AValue: T; const AComparer: IEqualityComparer<T>): Boolean;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (FCount.Int <> 0) then
  begin
    Result := (InternalIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      TEqualityComparison<T>(AComparer)) >= 0);
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.Contains(const AValue: T; const AComparer: TEqualityComparison<T>): Boolean;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (FCount.Int <> 0) then
  begin
    Result := (InternalIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      AComparer) >= 0);
  end else
  begin
    Result := False;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.IndexOf(const AValue: T): Integer;
begin
  if (FCount.Int <> 0) then
  begin
    Result := InternalIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      TEqualityComparison<T>(FEqualityComparer));
  end else
  begin
    Result := -1;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.IndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (FCount.Int <> 0) then
  begin
    Result := InternalIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      TEqualityComparison<T>(AComparer));
  end else
  begin
    Result := -1;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (FCount.Int <> 0) then
  begin
    Result := InternalIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      AComparer);
  end else
  begin
    Result := -1;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.LastIndexOf(const AValue: T): Integer;
begin
  if (FCount.Int <> 0) then
  begin
    Result := InternalLastIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      TEqualityComparison<T>(FEqualityComparer));
  end else
  begin
    Result := -1;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.LastIndexOf(const AValue: T; const AComparer: IEqualityComparer<T>): Integer;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (FCount.Int <> 0) then
  begin
    Result := InternalLastIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      TEqualityComparison<T>(AComparer));
  end else
  begin
    Result := -1;
  end;
end;

function TCustomListCollection<T1,T,T3,T4>.LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  CheckArgumentIsNil(Assigned(AComparer), 'Comparer');

  if (FCount.Int <> 0) then
  begin
    Result := InternalLastIndexOf(AValue, @FItems[0].Field2, SizeOf(TItem), FCount.Int,
      AComparer);
  end else
  begin
    Result := -1;
  end;
end;


{ TArrayCollection<T> }

constructor TArrayCollection<T>.Create(const AList: TArray<T>; const AComparer: IComparer<T>;
  const AEqualityComparer: IEqualityComparer<T>);
begin
  inherited Create;

  if (Assigned(AComparer)) then
    FComparer := AComparer;

  if (Assigned(AEqualityComparer)) then
    FEqualityComparer := AEqualityComparer;

  FList := AList;
  FItems := Pointer(AList);
  FCapacity.Native := Length(AList);
  FCount.Native := FCapacity.Native;
end;


{ TConcatedCollection<T> }

constructor TConcatedCollection<T>.Create(const ACollection: TCollection<T>;
  const ASecond: Pointer; const ASecondMode: TSecondMode);
begin
  inherited Create;

  FCollection := ACollection;
  FComparer := ACollection.FComparer;
  FEqualityComparer := ACollection.FEqualityComparer;
  FSecondMode := ASecondMode;
  case (ASecondMode) of
    smInstance:
    begin
      FSecondInstance := TCollection<T>(ASecond);
    end;
    smInterface:
    begin
      FSecondInterface := ICollection<T>(ASecond);
      if (FSecondInterface.Self is TCollection<T>) then
        FSecondInstance := TCollection<T>(FSecondInterface.Self);
    end;
  else
    // smEnumerable:
    FSecondEnumerable := IEnumerable<T>(ASecond);
    if (FSecondEnumerable is TCollection<T>) then
      FSecondInstance := FSecondEnumerable as TCollection<T>;
  end;
end;

function TConcatedCollection<T>.DoGetCount: Integer;
begin
  Result := FCollection.DoGetCount;

  if (Assigned(FSecondInstance)) then
  begin
    Inc(Result, FSecondInstance.DoGetCount);
  end else
  case (FSecondMode) of
    smInterface:
    begin
      Inc(Result, FSecondInterface.Count);
    end;
    smEnumerable:
    begin
      Inc(Result, InternalEnumerableCount);
    end;
  end;
end;

function TConcatedCollection<T>.DoGetIsEmpty: Boolean;
begin
  Result := FCollection.DoGetIsEmpty;
  if (not Result) then
    Exit;

  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.DoGetIsEmpty;
  end else
  case (FSecondMode) of
    smInterface:
    begin
      Result := FSecondInstance.IsEmpty;
    end;
    smEnumerable:
    begin
      Result := InternalEnumerableEmpty;
    end;
  end;
end;

function TConcatedCollection<T>.DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean;
begin
  if (Assigned(FSecondInstance)) and (FCollection.DoTryGetReversedEnumerator(TCollectionEnumerator<T>(nil^))) and
    (FSecondInstance.DoTryGetReversedEnumerator(TCollectionEnumerator<T>(nil^))) then
  begin
    if (Assigned(@Enumerator)) then
    begin
      FSecondInstance.DoTryGetReversedEnumerator(TCollectionEnumerator<T>(Enumerator.InitProxyInteface^));
      Enumerator.Data.Owner := Self;
      Enumerator.DoMoveNext := ReversedEnumeratorMoveNext;
    end;

    Result := True;
    Exit;
  end;

  Result := False;
end;

function TConcatedCollection<T>.DoGetEnumerator: TCollectionEnumerator<T>;
begin
  TCollectionEnumerator<T>(Result.InitProxyInteface^) := FCollection.DoGetEnumerator;
  Result.Data.Owner := Self;
  Result.DoMoveNext := EnumeratorMoveNext;
end;

class function TConcatedCollection<T>.ReversedEnumeratorMoveNext(var AEnumerator: TCollectionEnumerator<T>): Boolean;
var
  LEnumerator: ^TCollectionEnumerator<T>;
begin
  LEnumerator := AEnumerator.GetProxyInteface;
  if (LEnumerator^.DoMoveNext(LEnumerator^)) then
  begin
    AEnumerator.Data.Current := LEnumerator.Data.Current;
    Result := True;
    Exit;
  end;

  if (TConcatedCollection<T>(AEnumerator.Data.Owner).FCollection.DoTryGetReversedEnumerator(AEnumerator)) then
    raise EMethodNotFound(AEnumerator.Data.Owner.ClassType, 'DoTryGetReversedEnumerator');

  Result := AEnumerator.DoMoveNext(AEnumerator);
end;

class function TConcatedCollection<T>.EnumeratorMoveNext(var AEnumerator: TCollectionEnumerator<T>): Boolean;
var
  LEnumerator: ^TCollectionEnumerator<T>;
  {$ifdef AUTOREFCOUNT}[Unsafe]{$endif} LOwner: TConcatedCollection<T>;
begin
  LEnumerator := AEnumerator.GetProxyInteface;
  if (LEnumerator^.DoMoveNext(LEnumerator^)) then
  begin
    AEnumerator.Data.Current := LEnumerator.Data.Current;
    Result := True;
    Exit;
  end;

  LOwner := TConcatedCollection<T>(AEnumerator.Data.Owner);
  if (Assigned(LOwner.FSecondInstance)) then
  begin
    AEnumerator.Init(LOwner.FSecondInstance, False);
  end else
  if (LOwner.SecondMode = smInterface) then
  begin
    AEnumerator.Init(LOwner.FSecondInterface, False);
  end else
  begin
    AEnumerator.Init(LOwner.FSecondEnumerable);
  end;

  Result := AEnumerator.DoMoveNext(AEnumerator);
end;

function TConcatedCollection<T>.InternalEnumerableCount: Integer;
var
  LEnumerator: IEnumerator<T>;
begin
  Result := 0;

  if (Assigned(FSecondEnumerable)) then
  begin
    LEnumerator := FSecondEnumerable.GetEnumerator;
    if (Assigned(LEnumerator)) then
    begin
      while (LEnumerator.MoveNext) do
        Inc(Result);
    end;
  end;
end;

function TConcatedCollection<T>.InternalEnumerableEmpty: Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  if (Assigned(FSecondEnumerable)) then
  begin
    LEnumerator := FSecondEnumerable.GetEnumerator;
    if (Assigned(LEnumerator)) then
    begin
      Result := LEnumerator.MoveNext;
      Exit;
    end;
  end;

  Result := False;
end;

function TConcatedCollection<T>.InternalEnumerableToArray: TArray<T>;
var
  Count, Buffered: NativeUInt;
  LEnumerator: IEnumerator<T>;
begin
  if (Pointer(Result) <> nil) then
  begin
    Result := nil;
  end;

  LEnumerator := FSecondEnumerable.GetEnumerator;
  if (not Assigned(LEnumerator)) then
    Exit;

  Count := 0;
  Buffered := 16;
  SetLength(Result, Buffered);

  while (Enumerator.MoveNext) do
  begin
    if (Count = Buffered) then
    begin
      Buffered := Buffered * 2;
      SetLength(Result, Buffered);
    end;

    Result[Count] := Enumerator.Current;
    Inc(Count);
  end;

  SetLength(Result, Count);
end;

function TConcatedCollection<T>.InternalEnumerableToArray(const APredicate: TFunction<T,Boolean>): TArray<T>;
var
  Count, Buffered: NativeUInt;
  LEnumerator: IEnumerator<T>;
  Temp: T;
begin
  if (Pointer(Result) <> nil) then
  begin
    Result := nil;
  end;

  LEnumerator := FSecondEnumerable.GetEnumerator;
  if (not Assigned(LEnumerator)) then
    Exit;

  Count := 0;
  Buffered := 16;
  SetLength(Result, Buffered);

  while (Enumerator.MoveNext) do
  begin
    Temp := Enumerator.Current;
    if (APredicate(Temp)) then
    begin
      if (Count = Buffered) then
      begin
        Buffered := Buffered * 2;
        SetLength(Result, Buffered);
      end;

      Result[Count] := Temp;
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

function TConcatedCollection<T>.DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean;
var
  LLeftItems, LRightItems: TLinearItems;
begin
  if (Assigned(FSecondInstance)) and (FSecondInstance.DoTryGetLinearItems(LRightItems)) then
  begin
    if (FCollection.DoTryGetLinearItems(LLeftItems)) and (LLeftItems.Offset = LRightItems.Offset) then
    begin
      Result := LLeftItems.Combine(ALinearItems, LRightItems);
      Exit;
    end;
  end;

  Result := False;
end;

function TConcatedCollection<T>.ToArray: TArray<T>;
var
  LinearItems: TLinearItems;
  Temp: TArray<T>;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    LinearItems.ToArray(Result);
    Exit;
  end;

  Result := FCollection.ToArray;

  if (Assigned(FSecondInstance)) then
  begin
    Temp := FSecondInstance.ToArray;
  end else
  if (SecondMode = smInterface) then
  begin
    Temp := FSecondInterface.ToArray;
  end else
  begin
    Temp := InternalEnumerableToArray;
  end;

  if (Pointer(Result) = nil) then
  begin
    Result := Temp;
  end else
  begin
    Insert(Temp, Result, Length(Result));
  end;
end;

function TConcatedCollection<T>.ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>;
var
  LinearItems: TLinearItems;
  Temp: TArray<T>;
begin
  if (DoTryGetLinearItems(LinearItems)) then
  begin
    LinearItems.ToArray(Result, APredicate);
    Exit;
  end;

  Result := FCollection.ToArray(APredicate);

  if (Assigned(FSecondInstance)) then
  begin
    Temp := FSecondInstance.ToArray(APredicate);
  end else
  if (SecondMode = smInterface) then
  begin
    Temp := FSecondInterface.ToArray(APredicate);
  end else
  begin
    Temp := InternalEnumerableToArray(APredicate);
  end;

  if (Pointer(Result) = nil) then
  begin
    Result := Temp;
  end else
  begin
    Insert(Temp, Result, Length(Result));
  end;
end;

function TConcatedCollection<T>.All(const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.All(APredicate);

  if (Result) then
  begin
    if (Assigned(FSecondInstance)) then
    begin
      Result := FSecondInstance.All(APredicate);
    end else
    if (SecondMode = smInterface) then
    begin
      Result := FSecondInterface.All(APredicate);
    end else
    begin
      Result := InternalAll(FSecondEnumerable, APredicate);
    end;
  end;
end;

function TConcatedCollection<T>.Any(const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.Any(APredicate);

  if (not Result) then
  begin
    if (Assigned(FSecondInstance)) then
    begin
      Result := FSecondInstance.Any(APredicate);
    end else
    if (SecondMode = smInterface) then
    begin
      Result := FSecondInterface.Any(APredicate);
    end else
    begin
      Result := InternalAny(FSecondEnumerable, APredicate);
    end;
  end;
end;

function TConcatedCollection<T>.TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean;
var
  LinearItems: TLinearItems;
begin
  Result := FCollection.TryAggregate(Value, AFunc);

  if (Assigned(FSecondInstance)) then
  begin
    if (FSecondInstance.DoTryGetLinearItems(LinearItems)) then
    begin
      Result := LinearItems.TryAggregate(Value, AFunc, Result);
    end else
    begin
      Result := InternalTryAggregate(FSecondInstance, Value, AFunc, Result);
    end;
  end else
  if (SecondMode = smInterface) then
  begin
    Result := InternalTryAggregate(FSecondInterface, Value, AFunc, Result);
  end else
  begin
    Result := InternalTryAggregate(FSecondEnumerable, Value, AFunc, Result);
  end;
end;

function TConcatedCollection<T>.TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
var
  Temp: T;
begin
  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetMin(Value, AComparer);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetMin(Value, AComparer);
  end else
  begin
    CheckArgumentIsNil(Assigned(AComparer), 'Comparer');
    Result := InternalTryGetMin(Value, FSecondEnumerable, AComparer);
  end;

  if (not Result) then
  begin
    Result := FCollection.TryGetMin(Value, AComparer);
  end else
  begin
    if (FCollection.TryGetMin(Temp, AComparer)) then
    begin
      if (AComparer(Temp, Value) < 0) then
        Value := Temp;
    end;

    Result := True;
  end;
end;

function TConcatedCollection<T>.TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
var
  Temp: Integer;
begin
  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetMin(Value, ASelector);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetMin(Value, ASelector);
  end else
  begin
    CheckArgumentIsNil(Assigned(ASelector), 'Selector');
    Result := InternalTryGetMin(Value, FSecondEnumerable, ASelector);
  end;

  if (not Result) then
  begin
    Result := FCollection.TryGetMin(Value, ASelector);
  end else
  begin
    if (FCollection.TryGetMin(Temp, ASelector)) then
    begin
      if (Temp < Value) then
        Value := Temp;
    end;

    Result := True;
  end;
end;

function TConcatedCollection<T>.TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
var
  Temp: T;
begin
  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetMax(Value, AComparer);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetMax(Value, AComparer);
  end else
  begin
    CheckArgumentIsNil(Assigned(AComparer), 'Comparer');
    Result := InternalTryGetMax(Value, FSecondEnumerable, AComparer);
  end;

  if (not Result) then
  begin
    Result := FCollection.TryGetMax(Value, AComparer);
  end else
  begin
    if (FCollection.TryGetMax(Temp, AComparer)) then
    begin
      if (AComparer(Temp, Value) > 0) then
        Value := Temp;
    end;

    Result := True;
  end;
end;

function TConcatedCollection<T>.TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
var
  Temp: Integer;
begin
  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetMax(Value, ASelector);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetMax(Value, ASelector);
  end else
  begin
    CheckArgumentIsNil(Assigned(ASelector), 'Selector');
    Result := InternalTryGetMax(Value, FSecondEnumerable, ASelector);
  end;

  if (not Result) then
  begin
    Result := FCollection.TryGetMax(Value, ASelector);
  end else
  begin
    if (FCollection.TryGetMax(Temp, ASelector)) then
    begin
      if (Temp > Value) then
        Value := Temp;
    end;

    Result := True;
  end;
end;

function TConcatedCollection<T>.Sum: T;
var
  Temp: T;
begin
  Result := FCollection.Sum;

  if (Assigned(FSecondInstance)) then
  begin
    Temp := FSecondInstance.Sum;
  end else
  if (SecondMode = smInterface) then
  begin
    Temp := FSecondInterface.Sum;
  end else
  begin
    Temp := InternalSum(FSecondEnumerable);
  end;

  case {$ifdef SMARTGENERICS}GetTypeKind(T){$else}PTypeInfo(TypeInfo(T)).Kind{$endif} of
    tkInteger:
    begin
      case (GetTypeData(TypeInfo(T)).OrdType) of
        otSByte, otUByte: PByte(@Result)^ := PByte(@Result)^ + PByte(@Temp)^;
        otSWord, otUWord: PWord(@Result)^ := PWord(@Result)^ + PWord(@Temp)^;
        otSLong, otULong: PCardinal(@Result)^ := PCardinal(@Result)^ + PCardinal(@Temp)^;
      end;
    end;
    tkInt64:
    begin
      PInt64(@Result)^ := PInt64(@Result)^ + PInt64(@Temp)^;
    end;
    tkFloat:
    begin
      case (GetTypeData(TypeInfo(T)).FloatType) of
        ftSingle: PSingle(@Result)^ := PSingle(@Result)^ + PSingle(@Temp)^;
        ftDouble: PDouble(@Result)^ := PDouble(@Result)^ + PDouble(@Temp)^;
      ftExtended: PExtended(@Result)^ := PExtended(@Result)^ + PExtended(@Temp)^;
      else
        PInt64(@Result)^ := PInt64(@Result)^ + PInt64(@Temp)^;
      end;
    end;
  end;
end;

function TConcatedCollection<T>.Sum(const ASelector: TFunction<T,Integer>): Integer;
begin
  Result := FCollection.Sum(ASelector);

  if (Assigned(FSecondInstance)) then
  begin
    Result := Result + FSecondInstance.Sum(ASelector);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := Result + FSecondInterface.Sum(ASelector);
  end else
  begin
    Result := Result + InternalSum(FSecondEnumerable, ASelector);
  end;
end;

function TConcatedCollection<T>.Sum(const ASelector: TFunction<T,Int64>): Int64;
begin
  Result := FCollection.Sum(ASelector);

  if (Assigned(FSecondInstance)) then
  begin
    Result := Result + FSecondInstance.Sum(ASelector);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := Result + FSecondInterface.Sum(ASelector);
  end else
  begin
    Result := Result + InternalSum(FSecondEnumerable, ASelector);
  end;
end;

function TConcatedCollection<T>.Sum(const ASelector: TFunction<T,Extended>): Extended;
begin
  Result := FCollection.Sum(ASelector);

  if (Assigned(FSecondInstance)) then
  begin
    Result := Result + FSecondInstance.Sum(ASelector);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := Result + FSecondInterface.Sum(ASelector);
  end else
  begin
    Result := Result + InternalSum(FSecondEnumerable, ASelector);
  end;
end;

procedure TConcatedCollection<T>.ForEach(const AAction: TProcedure<T>);
begin
  FCollection.ForEach(AAction);

  if (Assigned(FSecondInstance)) then
  begin
    FSecondInstance.ForEach(AAction);
  end else
  if (SecondMode = smInterface) then
  begin
    FSecondInterface.ForEach(AAction);
  end else
  begin
    InternalForEach(FSecondEnumerable, AAction);
  end;
end;

function TConcatedCollection<T>.ForEach(const AAction: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.ForEach(AAction);
  if (not Result) then
    Exit;

  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.ForEach(AAction);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.ForEach(AAction);
  end else
  begin
    Result := InternalForEach(FSecondEnumerable, AAction);
  end;
end;

function TConcatedCollection<T>.TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean;
var
  LIndex: Integer;
begin
  if (AIndex >= 0) then
  begin
    Result := FCollection.TryGetElementAt(Value, AIndex);
    if (Result) then
      Exit;

    LIndex := AIndex - FCollection.DoGetCount;
    if (LIndex >= 0) then
    begin
      if (Assigned(FSecondInstance)) then
      begin
        Result := FSecondInstance.TryGetElementAt(Value, LIndex);
      end else
      if (SecondMode = smInterface) then
      begin
        Result := FSecondInterface.TryGetElementAt(Value, LIndex);
      end else
      begin
        Result := InternalTryGetElementAt(FSecondEnumerable, Value, LIndex);
      end;
    end;
  end;

  Result := False;
end;

function TConcatedCollection<T>.TryGetFirst(var{out} Value: T): Boolean;
begin
  Result := FCollection.TryGetFirst(Value);
  if (Result) then
    Exit;

  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetFirst(Value);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetFirst(Value);
  end else
  begin
    Result := InternalTryGetFirst(FSecondEnumerable, Value);
  end;
end;

function TConcatedCollection<T>.TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.TryGetFirst(Value, APredicate);
  if (Result) then
    Exit;

  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetFirst(Value, APredicate);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetFirst(Value, APredicate);
  end else
  begin
    Result := InternalTryGetFirst(FSecondEnumerable, Value, APredicate);
  end;
end;

function TConcatedCollection<T>.TryGetLast(var{out} Value: T): Boolean;
begin
  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetLast(Value);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetLast(Value);
  end else
  begin
    Result := InternalTryGetLast(FSecondEnumerable, Value);
  end;

  if (not Result) then
    Result := FCollection.TryGetLast(Value);
end;

function TConcatedCollection<T>.TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.TryGetLast(Value, APredicate);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.TryGetLast(Value, APredicate);
  end else
  begin
    Result := InternalTryGetLast(FSecondEnumerable, Value, APredicate);
  end;

  if (not Result) then
    Result := FCollection.TryGetLast(Value, APredicate);
end;

function TConcatedCollection<T>.InternalTryGetSingle(var{out} Value: T): Integer;
var
  LinearItems: TLinearItems;
begin
  Result := FCollection.InternalTryGetSingle(Value);

  if (Result <= 1) then
  begin
    if (Assigned(FSecondInstance)) then
    begin
      if (FSecondInstance.DoTryGetLinearItems(LinearItems)) then
      begin
        Result := Result + LinearItems.Count1 + LinearItems.Count2;
        if (Result = 1) then
        begin
          if (LinearItems.Count1 = 1) then
          begin
            Value := LinearItems.Values1^;
          end else
          if (LinearItems.Count2 = 1) then
          begin
            Value := LinearItems.Values2^;
          end;
        end;
      end else
      begin
        Result := InternalTryGetSingle(FSecondInstance, Value, Boolean(Result));
      end;
    end else
    if (SecondMode = smInterface) then
    begin
      Result := InternalTryGetSingle(FSecondInterface, Value, Boolean(Result));
    end else
    begin
      Result := InternalTryGetSingle(FSecondEnumerable, Value, Boolean(Result));
    end;
  end;
end;

function TConcatedCollection<T>.InternalTryGetSingle(var{out} Value: T;
  const APredicate: TFunction<T,Boolean>): Integer;
var
  LinearItems: TLinearItems;
begin
  Result := FCollection.InternalTryGetSingle(Value, APredicate);

  if (Result <= 1) then
  begin
    if (Assigned(FSecondInstance)) then
    begin
      if (FSecondInstance.DoTryGetLinearItems(LinearItems)) then
      begin
        Result := LinearItems.TryGetSingle(Value, APredicate, Boolean(Result));
      end else
      begin
        Result := InternalTryGetSingle(FSecondInstance, Value, APredicate, Boolean(Result));
      end;
    end else
    if (SecondMode = smInterface) then
    begin
      Result := InternalTryGetSingle(FSecondInterface, Value, APredicate, Boolean(Result));
    end else
    begin
      Result := InternalTryGetSingle(FSecondEnumerable, Value, APredicate, Boolean(Result));
    end;
  end;
end;

function TConcatedCollection<T>.IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  Result := FCollection.IndexOf(AValue, AComparer);
  if (Result < 0) then
  begin
    if (Assigned(FSecondInstance)) then
    begin
      Result := FSecondInstance.IndexOf(AValue, AComparer);
    end else
    if (SecondMode = smInterface) then
    begin
      Result := FSecondInterface.IndexOf(AValue, AComparer);
    end else
    begin
      Result := InternalIndexOf(AValue, FSecondEnumerable, AComparer);
    end;

    if (Result >= 0) then
    begin
      Inc(Result, FCollection.DoGetCount);
    end;
  end;
end;

function TConcatedCollection<T>.LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  if (Assigned(FSecondInstance)) then
  begin
    Result := FSecondInstance.LastIndexOf(AValue, AComparer);
  end else
  if (SecondMode = smInterface) then
  begin
    Result := FSecondInterface.LastIndexOf(AValue, AComparer);
  end else
  begin
    Result := InternalLastIndexOf(AValue, FSecondEnumerable, AComparer);
  end;

  if (Result >= 0) then
  begin
    Inc(Result, FCollection.DoGetCount);
  end else
  begin
    Result := FCollection.LastIndexOf(AValue, AComparer);
  end;
end;


{ TCustomProxyCollection<T> }

constructor TCustomProxyCollection<T>.Create(const ACollection: TCollection<T>);
begin
  inherited Create;
  FCollection := ACollection;
  FComparer := ACollection.FComparer;
  FEqualityComparer := ACollection.FEqualityComparer;
end;

function TCustomProxyCollection<T>.DoGetCount: Integer;
begin
  Result := FCollection.DoGetCount;
end;

function TCustomProxyCollection<T>.DoGetIsEmpty: Boolean;
begin
  Result := FCollection.DoGetIsEmpty;
end;

function TCustomProxyCollection<T>.DoGetIsSynchronized: Boolean;
begin
  Result := FCollection.DoGetIsSynchronized;
end;

function TCustomProxyCollection<T>.All(const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.All(APredicate);
end;

function TCustomProxyCollection<T>.Any(const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.Any(APredicate);
end;

function TCustomProxyCollection<T>.TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
begin
  Result := FCollection.TryGetMin(Value, AComparer);
end;

function TCustomProxyCollection<T>.TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
begin
  Result := FCollection.TryGetMin(Value, ASelector);
end;

function TCustomProxyCollection<T>.TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
begin
  Result := FCollection.TryGetMax(Value, AComparer);
end;

function TCustomProxyCollection<T>.TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
begin
  Result := FCollection.TryGetMax(Value, ASelector);
end;

function TCustomProxyCollection<T>.Single: T;
begin
  Result := FCollection.Single;
end;

function TCustomProxyCollection<T>.SingleOrDefault: T;
begin
  Result := FCollection.SingleOrDefault;
end;

function TCustomProxyCollection<T>.SingleOrDefault(const ADefaultValue: T): T;
begin
  Result := FCollection.SingleOrDefault;
end;

function TCustomProxyCollection<T>.TryGetSingle(var{out} Value: T): Boolean;
begin
  Result := FCollection.TryGetSingle(Value);
end;

function TCustomProxyCollection<T>.Single(const APredicate: TFunction<T,Boolean>): T;
begin
  Result := FCollection.Single(APredicate);
end;

function TCustomProxyCollection<T>.SingleOrDefault(const APredicate: TFunction<T,Boolean>): T;
begin
  Result := FCollection.SingleOrDefault(APredicate);
end;

function TCustomProxyCollection<T>.SingleOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T;
begin
  Result := FCollection.SingleOrDefault(APredicate, ADefaultValue);
end;

function TCustomProxyCollection<T>.TryGetSingle(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.TryGetSingle(Value, APredicate);
end;

function TCustomProxyCollection<T>.Sum: T;
begin
  Result := FCollection.Sum;
end;

function TCustomProxyCollection<T>.Sum(const ASelector: TFunction<T,Integer>): Integer;
begin
  Result := FCollection.Sum(ASelector);
end;

function TCustomProxyCollection<T>.Sum(const ASelector: TFunction<T,Int64>): Int64;
begin
  Result := FCollection.Sum(ASelector);
end;

function TCustomProxyCollection<T>.Sum(const ASelector: TFunction<T,Extended>): Extended;
begin
  Result := FCollection.Sum(ASelector);
end;

function TCustomProxyCollection<T>.Contains(const AValue: T; const AComparer: TEqualityComparison<T>): Boolean;
begin
  Result := FCollection.Contains(AValue, AComparer);
end;

function TCustomProxyCollection<T>.Ordered(const AComparer: TComparison<T>): ICollection<T>;
begin
  Result := FCollection.Ordered(AComparer);
end;

function TCustomProxyCollection<T>.Shuffled: ICollection<T>;
begin
  Result := FCollection.Shuffled;
end;


{ TProxyCollection<T> }


function TProxyCollection<T>.DoGetIsOrdered: Boolean;
begin
  Result := FCollection.DoGetIsOrdered;
end;

function TProxyCollection<T>.DoTryGetLinearItems(var ALinearItems: TLinearItems): Boolean;
begin
  Result := FCollection.DoTryGetLinearItems(ALinearItems);
end;

function TProxyCollection<T>.DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean;
begin
  Result := FCollection.DoTryGetReversedEnumerator(Enumerator);
end;

function TProxyCollection<T>.DoGetEnumerator: TCollectionEnumerator<T>;
begin
  Result := FCollection.DoGetEnumerator;
end;

function TProxyCollection<T>.ToArray: TArray<T>;
begin
  Result := FCollection.ToArray;
end;

function TProxyCollection<T>.ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>;
begin
  Result := FCollection.ToArray(APredicate);
end;

function TProxyCollection<T>.TryAggregate(var{out} Value: T; const AFunc: TFunction<T,T,T>): Boolean;
begin
  Result := FCollection.TryAggregate(Value, AFunc);
end;

function TProxyCollection<T>.TryGetMin(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
begin
  Result := FCollection.TryGetMin(Value, AComparer);
end;

function TProxyCollection<T>.TryGetMin(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
begin
  Result := FCollection.TryGetMin(Value, ASelector);
end;

function TProxyCollection<T>.TryGetMax(var{out} Value: T; const AComparer: TComparison<T>): Boolean;
begin
  Result := FCollection.TryGetMax(Value, AComparer);
end;

function TProxyCollection<T>.TryGetMax(var{out} Value: Integer; const ASelector: TFunction<T,Integer>): Boolean;
begin
  Result := FCollection.TryGetMax(Value, ASelector);
end;

procedure TProxyCollection<T>.ForEach(const AAction: TProcedure<T>);
begin
  FCollection.ForEach(AAction);
end;

function TProxyCollection<T>.ForEach(const AAction: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.ForEach(AAction);
end;

function TProxyCollection<T>.TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean;
begin
  Result := FCollection.TryGetElementAt(Value, AIndex);
end;

function TProxyCollection<T>.TryGetFirst(var{out} Value: T): Boolean;
begin
  Result := FCollection.TryGetFirst(Value);
end;

function TProxyCollection<T>.TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.TryGetFirst(Value, APredicate);
end;

function TProxyCollection<T>.TryGetLast(var{out} Value: T): Boolean;
begin
  Result := FCollection.TryGetLast(Value);
end;

function TProxyCollection<T>.TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.TryGetLast(Value, APredicate);
end;

function TProxyCollection<T>.IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  Result := FCollection.IndexOf(AValue, AComparer);
end;

function TProxyCollection<T>.LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  Result := FCollection.LastIndexOf(AValue, AComparer);
end;

function TProxyCollection<T>.EqualsTo(const AValues: array of T; const AComparer: TEqualityComparison<T>): Boolean;
begin
  Result := FCollection.EqualsTo(AValues, AComparer);
end;

function TProxyCollection<T>.EqualsTo(const ACollection: TCollection<T>; const AComparer: TEqualityComparison<T>): Boolean;
begin
  Result := FCollection.EqualsTo(ACollection, AComparer);
end;

function TProxyCollection<T>.EqualsTo(const ACollection: ICollection<T>; const AComparer: TEqualityComparison<T>): Boolean;
begin
  Result := FCollection.EqualsTo(ACollection, AComparer);
end;

function TProxyCollection<T>.EqualsTo(const AEnumerable: IEnumerable<T>; const AComparer: TEqualityComparison<T>): Boolean;
begin
  Result := FCollection.EqualsTo(AEnumerable, AComparer);
end;

function TProxyCollection<T>.Concat(const ACollection: TCollection<T>): ICollection<T>;
begin
  Result := FCollection.Concat(ACollection);
end;

function TProxyCollection<T>.Concat(const ACollection: ICollection<T>): ICollection<T>;
begin
  Result := FCollection.Concat(ACollection);
end;

function TProxyCollection<T>.Concat(const AEnumerable: IEnumerable<T>): ICollection<T>;
begin
  Result := FCollection.Concat(AEnumerable);
end;

function TProxyCollection<T>.Where(const APredicate: TFunction<T,Boolean>): ICollection<T>;
begin
  Result := FCollection.Where(APredicate);
end;

function TProxyCollection<T>.Reversed: ICollection<T>;
begin
  Result := FCollection.Reversed;
end;


{ TReversedCollection<T> }

function TReversedCollection<T>.DoTryGetReversedEnumerator(var Enumerator: TCollectionEnumerator<T>): Boolean;
begin
  Enumerator := FCollection.DoGetEnumerator;
  Result := True;
end;

function TReversedCollection<T>.DoGetEnumerator: TCollectionEnumerator<T>;
begin
  DoTryGetReversedEnumerator(Result);
end;

function TReversedCollection<T>.ToArray: TArray<T>;
begin
  Result := FCollection.ToArray;
  TArray.Reverse<T>(Result);
end;

function TReversedCollection<T>.ToArray(const APredicate: TFunction<T,Boolean>): TArray<T>;
begin
  Result := FCollection.ToArray(APredicate);
  TArray.Reverse<T>(Result);
end;

function TReversedCollection<T>.TryGetElementAt(var{out} Value: T; const AIndex: Integer): Boolean;
var
  Count: Integer;
begin
  Count := FCollection.DoGetCount;
  Result := (Cardinal(AIndex) < Cardinal(Count)) and
    (FCollection.TryGetElementAt(Value, Count - 1 - AIndex));
end;

function TReversedCollection<T>.First: T;
begin
  Result := FCollection.Last;
end;

function TReversedCollection<T>.FirstOrDefault: T;
begin
  Result := FCollection.LastOrDefault;
end;

function TReversedCollection<T>.FirstOrDefault(const ADefaultValue: T): T;
begin
  Result := FCollection.LastOrDefault(ADefaultValue);
end;

function TReversedCollection<T>.TryGetFirst(var{out} Value: T): Boolean;
begin
  Result := FCollection.TryGetLast(Value);
end;

function TReversedCollection<T>.First(const APredicate: TFunction<T,Boolean>): T;
begin
  Result := FCollection.Last(APredicate);
end;

function TReversedCollection<T>.FirstOrDefault(const APredicate: TFunction<T,Boolean>): T;
begin
  Result := FCollection.LastOrDefault(APredicate);
end;

function TReversedCollection<T>.FirstOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T;
begin
  Result := FCollection.LastOrDefault(APredicate, ADefaultValue);
end;

function TReversedCollection<T>.TryGetFirst(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.TryGetLast(Value, APredicate);
end;

function TReversedCollection<T>.Last: T;
begin
  Result := FCollection.First;
end;

function TReversedCollection<T>.LastOrDefault: T;
begin
  Result := FCollection.FirstOrDefault;
end;

function TReversedCollection<T>.LastOrDefault(const ADefaultValue: T): T;
begin
  Result := FCollection.FirstOrDefault(ADefaultValue);
end;

function TReversedCollection<T>.TryGetLast(var{out} Value: T): Boolean;
begin
  Result := FCollection.TryGetFirst(Value);
end;

function TReversedCollection<T>.Last(const APredicate: TFunction<T,Boolean>): T;
begin
  Result := FCollection.First(APredicate);
end;

function TReversedCollection<T>.LastOrDefault(const APredicate: TFunction<T,Boolean>): T;
begin
  Result := FCollection.FirstOrDefault(APredicate);
end;

function TReversedCollection<T>.LastOrDefault(const APredicate: TFunction<T,Boolean>; const ADefaultValue: T): T;
begin
  Result := FCollection.FirstOrDefault(APredicate, ADefaultValue);
end;

function TReversedCollection<T>.TryGetLast(var{out} Value: T; const APredicate: TFunction<T,Boolean>): Boolean;
begin
  Result := FCollection.TryGetFirst(Value, APredicate);
end;

function TReversedCollection<T>.IndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  Result := FCollection.LastIndexOf(AValue, AComparer);
  if (Result >= 0) then
  begin
    Result := FCollection.DoGetCount - 1 - Result;
    if (Result < 0) then
      Result := -1;
  end;
end;

function TReversedCollection<T>.LastIndexOf(const AValue: T; const AComparer: TEqualityComparison<T>): Integer;
begin
  Result := FCollection.IndexOf(AValue, AComparer);
  if (Result >= 0) then
  begin
    Result := FCollection.DoGetCount - 1 - Result;
    if (Result < 0) then
      Result := -1;
  end;
end;

function TReversedCollection<T>.Reversed: ICollection<T>;
begin
  Result := TProxyCollection<T>.Create(FCollection);
end;


{ TCustomDictionary<TKey,TValue>.TPairEnumerator }

{$if CompilerVersion <= 22}
function TCustomDictionary<TKey,TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  Result := Data.Current;
end;
{$ifend}

function TCustomDictionary<TKey,TValue>.TPairEnumerator.MoveNext: Boolean;
var
  N: NativeInt;
  Item: PItem;
begin
  N := Data.Tag + 1;

  with TCustomDictionary<TKey,TValue>(Data.Owner) do
  begin
    if (N < FCount.Native) then
    begin
      Data.Tag := N;

      Item := @FItems[N];
      Data.Current.Key := Item.Key;
      Data.Current.Value := Item.Value;

      Exit(True);
    end;
  end;

  Result := False;
end;

{ TCustomDictionary<TKey,TValue>.TKeyEnumerator }

{$if CompilerVersion <= 22}
function TCustomDictionary<TKey,TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := Data.Current;
end;
{$ifend}

function TCustomDictionary<TKey,TValue>.TKeyEnumerator.MoveNext: Boolean;
var
  N: NativeInt;
begin
  N := Data.Tag + 1;

  with TCustomDictionary<TKey,TValue>(Data.Owner) do
  begin
    if (N < FCount.Native) then
    begin
      Data.Tag := N;
      Data.Current := FItems[N].Key;
      Exit(True);
    end;
  end;

  Result := False;
end;

{ TCustomDictionary<TKey,TValue>.TValueEnumerator }

{$if CompilerVersion <= 22}
function TCustomDictionary<TKey,TValue>.TValueEnumerator: TValue;
begin
  Result := Data.Current;
end;
{$ifend}

function TCustomDictionary<TKey,TValue>.TValueEnumerator.MoveNext: Boolean;
var
  N: NativeInt;
begin
  N := Data.Tag + 1;

  with TCustomDictionary<TKey,TValue>(Data.Owner) do
  begin
    if (N < FCount.Native) then
    begin
      Data.Tag := N;
      Data.Current := FItems[N].Value;
      Exit(True);
    end;
  end;

  Result := False;
end;

{ TCustomDictionary<TKey,TValue>.TKeyCollection }

constructor TCustomDictionary<TKey,TValue>.TKeyCollection.Create(const ADictionary: TCustomDictionary<TKey,TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TCustomDictionary<TKey,TValue>.TKeyCollection.DoGetCount: Integer;
begin
  Result := FDictionary.FCount.Int;
end;

function TCustomDictionary<TKey,TValue>.TKeyCollection.DoGetIsEmpty: Boolean;
begin
  Result := (FDictionary.FCount.Int = 0);
end;

function TCustomDictionary<TKey,TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := FDictionary.FCount.Int;
end;

function TCustomDictionary<TKey,TValue>.TKeyCollection.GetIsEmpty: Boolean;
begin
  Result := (FDictionary.FCount.Int = 0);
end;

function TCustomDictionary<TKey,TValue>.TKeyCollection.DoGetEnumerator: TCollectionEnumerator<TKey>;
begin
  Result.Data.Init(Self);
  Pointer(@Result.DoMoveNext) := @TKeyEnumerator.MoveNext;
end;

function TCustomDictionary<TKey,TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result.Data.Init(Self);
end;

function TCustomDictionary<TKey,TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  i, Count: NativeInt;
  Src: TCustomDictionary<TKey,TValue>.PItem;
  Dest: ^TKey;
begin
  Count := Self.FDictionary.FCount.Native;

  SetLength(Result, Count);
  Src := Pointer(FDictionary.FItems);
  Dest := Pointer(Result);
  for i := 0 to Count - 1 do
  begin
    Dest^ := Src.Key;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TCustomDictionary<TKey,TValue>.TValueCollection }

constructor TCustomDictionary<TKey,TValue>.TValueCollection.Create(const ADictionary: TCustomDictionary<TKey,TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TCustomDictionary<TKey,TValue>.TValueCollection.DoGetCount: Integer;
begin
  Result := FDictionary.FCount.Int;
end;

function TCustomDictionary<TKey,TValue>.TValueCollection.DoGetIsEmpty: Boolean;
begin
  Result := (FDictionary.FCount.Int = 0);
end;

function TCustomDictionary<TKey,TValue>.TValueCollection.GetCount: Integer;
begin
  Result := FDictionary.FCount.Int;
end;

function TCustomDictionary<TKey,TValue>.TValueCollection.GetIsEmpty: Boolean;
begin
  Result := (FDictionary.FCount.Int = 0);
end;

function TCustomDictionary<TKey,TValue>.TValueCollection.DoGetEnumerator: TCollectionEnumerator<TValue>;
begin
  Result.Data.Init(Self);
  Pointer(@Result.DoMoveNext) := @TValueEnumerator.MoveNext;
end;

function TCustomDictionary<TKey,TValue>.TValueCollection.GetEnumerator: TValueEnumerator;
begin
  Result.Data.Init(Self);
end;

function TCustomDictionary<TKey,TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  i, Count: NativeInt;
  Src: TCustomDictionary<TKey,TValue>.PItem;
  Dest: ^TValue;
begin
  Count := Self.FDictionary.FCount.Native;

  SetLength(Result, Count);
  Src := Pointer(FDictionary.FItems);
  Dest := Pointer(Result);
  for i := 0 to Count - 1 do
  begin
    Dest^ := Src.Value;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TCustomDictionary<TKey,TValue> }

function TCustomDictionary<TKey,TValue>.DoGetCount: Integer;
begin
  Result := FCount.Int;
end;

function TCustomDictionary<TKey,TValue>.DoGetEnumerator: TCollectionEnumerator<TPair<TKey,TValue>>;
begin
  Result.Data.Init(Self);
  Pointer(@Result.DoMoveNext) := @TPairEnumerator.MoveNext;
end;

function TCustomDictionary<TKey,TValue>.GetEnumerator: TPairEnumerator;
begin
  Result.Data.Init(Self);
end;

function TCustomDictionary<TKey,TValue>.InitKeyCollection: TKeyCollection;
begin
  FKeyCollection := TKeyCollection.Create(Self);
  {$ifNdef AUTOREFCOUNT}
  FKeyCollection._AddRef;
  {$endif}
  Result := FKeyCollection;
end;

function TCustomDictionary<TKey,TValue>.InitValueCollection: TValueCollection;
begin
  FValueCollection := TValueCollection.Create(Self);
  {$ifNdef AUTOREFCOUNT}
  FValueCollection._AddRef;
  {$endif}
  Result := FValueCollection;
end;

function TCustomDictionary<TKey,TValue>.GetKeys: TKeyCollection;
begin
  if (not Assigned(FKeyCollection)) then
  begin
    Result := InitKeyCollection;
  end else
  begin
    Result := FKeyCollection;
  end;
end;

function TCustomDictionary<TKey,TValue>.GetValues: TValueCollection;
begin
  if (not Assigned(FValueCollection)) then
  begin
    Result := InitValueCollection;
  end else
  begin
    Result := FValueCollection;
  end;
end;

function TCustomDictionary<TKey,TValue>.ToArray: TArray<TPair<TKey,TValue>>;
var
  i, Count: NativeInt;
  Src: PItem;
  Dest: ^TPair<TKey,TValue>;
begin
  Count := Self.FCount.Native;

  SetLength(Result, Count);
  Src := Pointer(FItems);
  Dest := Pointer(Result);
  for i := 0 to Count - 1 do
  begin
    Dest^.Key := Src.Key;
    Dest^.Value := Src.Value;

    Inc(Src);
    Inc(Dest);
  end;
end;

constructor TCustomDictionary<TKey,TValue>.Create(ACapacity: Integer);
begin
  inherited Create;
  TRAIIHelper<TKey>.Create;
  TRAIIHelper<TValue>.Create;

  FDefaultValue := Default(TValue);
  FHashTableMask := -1;
  SetNotifyMethods;

  if (ACapacity > 3) then
  begin
    SetCapacity(ACapacity);
  end else
  begin
    Rehash(4);
  end;
end;

destructor TCustomDictionary<TKey,TValue>.Destroy;
begin
  Clear;
  ReallocMem(FItems, 0);
  {$ifdef AUTOREFCOUNT}
  FKeyCollection.Free;
  FValueCollection.Free;
  {$else}
  if (Assigned(FKeyCollection)) then FKeyCollection._Release;
  if (Assigned(FValueCollection)) then FValueCollection._Release;
  {$endif}
  ClearMethod(FInternalKeyNotify);
  ClearMethod(FInternalValueNotify);
  ClearMethod(FInternalItemNotify);
  inherited;
end;

procedure TCustomDictionary<TKey,TValue>.Rehash(NewTableCount{power of 2}: NativeInt);
var
  NewCapacity: NativeInt;
  NewHashTable: TArray<PItem>;
  {$ifdef WEAKREF}
  WeakItems: PItemList;
  {$endif}

  i, HashTableMask, Index: NativeInt;
  Item: PItem;
  HashList: ^THashList;
begin
  // grow threshold
  NewCapacity := NewTableCount shr 1 + NewTableCount shr 2; // 75%
  if (NewCapacity < Self.FCount.Native) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  // reallocations
  if (NewTableCount < FHashTableMask + 1{Length(FHashTable)}) then
  begin
    ReallocMem(FItems, NewCapacity * SizeOf(TItem));
    SetLength(FHashTable, NewTableCount);
    NewHashTable := FHashTable;
  end else
  begin
    SetLength(NewHashTable, NewTableCount);

    {$ifdef WEAKREF}
    {$ifdef SMARTGENERICS}
    if (TRAIIHelper<TKey>.Weak) or (TRAIIHelper<TValue>.Weak) then
    {$else}
    if (TRAIIHelper<TKey>.FOptions.FWeak) or (TRAIIHelper<TValue>.FOptions.FWeak) then
    {$endif}
    begin
      GetMem(WeakItems, NewCapacity * SizeOf(TItem));

      if (FCount.Native <> 0) then
      begin
        FillChar(WeakItems^, FCount.Native * SizeOf(TItem), #0);
        System.CopyArray(WeakItems, FItems, TypeInfo(TItem), FCount.Native);
        System.FinalizeArray(FItems, TypeInfo(TItem), FCount.Native);
      end;

      FreeMem(FItems);
      FItems := WeakItems;
    end else
    {$endif}
    begin
      ReallocMem(FItems, NewCapacity * SizeOf(TItem));
    end;
  end;

  // apply new
  FillChar(Pointer(NewHashTable)^, NewTableCount * SizeOf(Pointer), #0);
  FHashTable := NewHashTable;
  FCapacity := NewCapacity;
  FHashTableMask := NewTableCount - 1;

  // regroup items
  Item := Pointer(FItems);
  HashList := Pointer(FHashTable);
  HashTableMask := FHashTableMask;
  for i := 1 to Self.FCount.Native do
  begin
    Index := NativeInt(Cardinal(Item.HashCode)) and HashTableMask;
    Item.FNext := HashList[Index];
    HashList[Index] := Item;
    Inc(Item);
  end;
end;

procedure TCustomDictionary<TKey,TValue>.SetCapacity(ACapacity: NativeInt);
var
  Cap, NewTableCount: NativeInt;
begin
  // 75% threshold
  Cap := 3;
  if (Cap < ACapacity) then
  repeat
    Cap := Cap shl 1;
  until (Cap < 0) or (Cap >= ACapacity);

  // power of 2
  NewTableCount := (Cap and (Cap - 1)) shl 1;
  if (NewTableCount = FHashTableMask + 1{Length(FHashTable)}) then
    Exit;
  if (NativeUInt(NewTableCount) > NativeUInt(High(Integer)){Integer(NewTableCount) < 0}) then
    OutOfMemoryError;

  // rehash
  Rehash(NewTableCount);
end;

function TCustomDictionary<TKey,TValue>.Grow: TCustomDictionary<TKey,TValue>;
begin
  Rehash((FHashTableMask + 1){Length(FHashTable)} * 2);
  Result := Self;
end;

procedure TCustomDictionary<TKey,TValue>.TrimExcess;
var
  Capacity: Integer;
begin
  Capacity := FCount.Int;
  SetCapacity(Capacity);
end;

procedure TCustomDictionary<TKey,TValue>.Clear;
begin
  if (FCount.Native <> 0) then
  begin
    Self.DoCleanupItems(Pointer(FItems), FCount.Native);
  end;

  FCount.Native := 0;
  if (FHashTableMask + 1 = 4) then
  begin
    FillChar(Pointer(FHashTable)^, 4 * SizeOf(Pointer), #0);
  end else
  begin
    Rehash(4);
  end;
end;

procedure TCustomDictionary<TKey,TValue>.DoCleanupItems(Item: PItem; Count: NativeInt);
{$ifdef SMARTGENERICS}
var
  i: NativeInt;
  VType: Integer;
{$else}
var
  i: NativeInt;
{$endif}
  StoredItem: PItem;
begin
  // Key/Value notifies (cnRemoved)
  StoredItem := Item;
  if Assigned(FInternalKeyNotify) then
  begin
    if Assigned(FInternalValueNotify) then
    begin
      if (TMethod(FInternalItemNotify).Code = @TCustomDictionary<TKey,TValue>.ItemNotifyCaller) then
      begin
        for i := 1 to Count do
        begin
          Self.KeyNotify(Item.Key, cnRemoved);
          Self.ValueNotify(Item.Value, cnRemoved);
          Inc(Item);
        end;
      end else
      begin
        for i := 1 to Count do
        begin
          FInternalKeyNotify(Self, Item.Key, cnRemoved);
          FInternalValueNotify(Self, Item.Value, cnRemoved);
          Inc(Item);
        end;
      end;
    end else
    begin
      // Key
      if (TMethod(FInternalKeyNotify).Code = @TCustomDictionary<TKey,TKey>.KeyNotifyCaller) then
      begin
        for i := 1 to Count do
        begin
          Self.KeyNotify(Item.Key, cnRemoved);
          Inc(Item);
        end;
      end else
      begin
        for i := 1 to Count do
        begin
          FInternalKeyNotify(Self, Item.Key, cnRemoved);
          Inc(Item);
        end;
      end;
    end;
  end else
  if Assigned(FInternalValueNotify) then
  begin
    // Value
    if (TMethod(FInternalValueNotify).Code = @TCustomDictionary<TKey,TValue>.ValueNotifyCaller) then
    begin
      for i := 1 to Count do
      begin
        Self.ValueNotify(Item.Value, cnRemoved);
        Inc(Item);
      end;
    end else
    begin
      for i := 1 to Count do
      begin
        FInternalValueNotify(Self, Item.Value, cnRemoved);
        Inc(Item);
      end;
    end;
  end;

  // finalize array
  Item := StoredItem;
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TKey)) then
  {$else}
  if Assigned(TRAIIHelper<TKey>.FOptions.ClearProc) then
  {$endif}
  begin
    {$ifdef SMARTGENERICS}
    if (System.IsManagedType(TValue)) then
    {$else}
    if Assigned(TRAIIHelper<TValue>.FOptions.ClearProc) then
    {$endif}
    begin
      // Keys + Values
      for i := 1 to Count do
      begin
        {$ifdef SMARTGENERICS}
        case GetTypeKind(TKey) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (PKeyRec(Item).Native <> 0) then
            case GetTypeKind(TKey) of
              {$ifdef AUTOREFCOUNT}
              tkClass:
              begin
                TRAIIHelper.RefObjClear(@PKeyRec(Item).Native);
              end;
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString:
              begin
                TRAIIHelper.WStrClear(@PKeyRec(Item).Native);
              end;
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString:
              begin
                TRAIIHelper.ULStrClear(@PKeyRec(Item).Native);
              end;
              tkInterface:
              begin
                IInterface(PKeyRec(Item).Native)._Release;
              end;
              tkDynArray:
              begin
                TRAIIHelper.DynArrayClear(@PKeyRec(Item).Native, TypeInfo(TKey));
              end;
            end;
          end;
          {$ifdef WEAKINSTREF}
          tkMethod:
          begin
            if (PKeyRec(Item).Method.Data <> nil) then
              TRAIIHelper.WeakMethodClear(@PKeyRec(Item).Method.Data);
          end;
          {$endif}
          tkVariant:
          begin
            VType := PKeyRec(Item).VarData.VType;
            if (VType and TRAIIHelper.varDeepData <> 0) then
            case VType of
              varBoolean, varUnknown+1..varUInt64: ;
            else
              System.VarClear(Variant(PKeyRec(Item).VarData));
            end;
          end;
        else
          TRAIIHelper<TKey>.FOptions.ClearProc(TRAIIHelper<TKey>.FOptions, @Item.FKey);
        end;
        {$else}
        TRAIIHelper<TKey>.FOptions.ClearProc(TRAIIHelper<TKey>.FOptions, @Item.FKey);
        {$endif}

        {$ifdef SMARTGENERICS}
        case GetTypeKind(TValue) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (PValueRec(Item).Native <> 0) then
            case GetTypeKind(TValue) of
              {$ifdef AUTOREFCOUNT}
              tkClass:
              begin
                TRAIIHelper.RefObjClear(@PValueRec(Item).Native);
              end;
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString:
              begin
                TRAIIHelper.WStrClear(@PValueRec(Item).Native);
              end;
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString:
              begin
                TRAIIHelper.ULStrClear(@PValueRec(Item).Native);
              end;
              tkInterface:
              begin
                IInterface(PValueRec(Item).Native)._Release;
              end;
              tkDynArray:
              begin
                TRAIIHelper.DynArrayClear(@PValueRec(Item).Native, TypeInfo(TValue));
              end;
            end;
          end;
          {$ifdef WEAKINSTREF}
          tkMethod:
          begin
            if (PValueRec(Item).Method.Data <> nil) then
              TRAIIHelper.WeakMethodClear(@PValueRec(Item).Method.Data);
          end;
          {$endif}
          tkVariant:
          begin
            VType := PValueRec(Item).VarData.VType;
            if (VType and TRAIIHelper.varDeepData <> 0) then
            case VType of
              varBoolean, varUnknown+1..varUInt64: ;
            else
              System.VarClear(Variant(PValueRec(Item).VarData));
            end;
          end;
        else
          TRAIIHelper<TValue>.FOptions.ClearProc(TRAIIHelper<TValue>.FOptions, @Item.FValue);
        end;
        {$else}
        TRAIIHelper<TValue>.FOptions.ClearProc(TRAIIHelper<TValue>.FOptions, @Item.FValue);
        {$endif}

        Inc(Item);
      end;
    end else
    begin
      // Keys only
      TRAIIHelper<TKey>.ClearArray(@Item.Key, Count, SizeOf(TItem));
    end;
  end else
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TValue)) then
  {$else}
  if Assigned(TRAIIHelper<TValue>.FOptions.ClearProc) then
  {$endif}
  begin
    // Values only
    TRAIIHelper<TValue>.ClearArray(@Item.Value, Count, SizeOf(TItem));
  end;
end;

function TCustomDictionary<TKey,TValue>.NewItem: Pointer{PItem};
var
  Instance: TCustomDictionary<TKey,TValue>;
  Count: NativeInt;
  Null: NativeUInt;
begin
  Instance := Self;
  repeat
    Count := Instance.FCount.Native;
    if (Count <> Instance.FCapacity) then
    begin
      Instance.FCount.Native := Count + 1;
      Result := Pointer(Instance.FItems);
      Inc(PItem(Result), Count);

      if ((SizeOf(TKey) >= SizeOf(NativeInt)) and (SizeOf(TKey) <= 16)) or
        ((SizeOf(TValue) >= SizeOf(NativeInt)) and (SizeOf(TValue) <= 16)) then
      begin
        {$ifdef SMARTGENERICS}
        if (System.IsManagedType(TItem)) then
        {$endif}
        Null := 0;
      end;

      {$ifdef SMARTGENERICS}
      if (System.IsManagedType(TKey)) then
      {$else}
      if (SizeOf(TKey) >= SizeOf(NativeInt)) then
      {$endif}
      begin
        if (SizeOf(TKey) = SizeOf(NativeInt)){$ifdef SMARTGENERICS}or (GetTypeKind(TKey) = tkVariant){$endif} then
        begin
          PKeyRec(Result).Natives[0] := Null;
        end else
        if ((SizeOf(TKey) >= SizeOf(NativeInt)) and (SizeOf(TKey) <= 16)) then
        begin
          PKeyRec(Result).Natives[0] := Null;
          if (SizeOf(TKey) >= 2 * SizeOf(NativeInt)) then PKeyRec(Result).Natives[1] := Null;
          {$ifdef SMALLINT}
          if (SizeOf(TKey) >= 3 * SizeOf(NativeInt)) then PKeyRec(Result).Natives[2] := Null;
          if (SizeOf(TKey)  = 4 * SizeOf(NativeInt)) then PKeyRec(Result).Natives[3] := Null;
          {$endif}
        end else
        TRAIIHelper<TKey>.Init(@PItem(Result).FKey);
      end;

      {$ifdef SMARTGENERICS}
      if (System.IsManagedType(TValue)) then
      {$else}
      if (SizeOf(TValue) >= SizeOf(NativeInt)) then
      {$endif}
      begin
        if (SizeOf(TValue) = SizeOf(NativeInt)){$ifdef SMARTGENERICS}or (GetTypeKind(TValue) = tkVariant){$endif} then
        begin
          PValueRec(Result)^.Natives[0] := Null;
        end else
        if ((SizeOf(TValue) >= SizeOf(NativeInt)) and (SizeOf(TValue) <= 16)) then
        begin
          PValueRec(Result)^.Natives[0] := Null;
          if (SizeOf(TValue) >= 2 * SizeOf(NativeInt)) then PValueRec(Result)^.Natives[1] := Null;
          {$ifdef SMALLINT}
          if (SizeOf(TValue) >= 3 * SizeOf(NativeInt)) then PValueRec(Result)^.Natives[2] := Null;
          if (SizeOf(TValue)  = 4 * SizeOf(NativeInt)) then PValueRec(Result)^.Natives[3] := Null;
          {$endif}
        end else
        TRAIIHelper<TValue>.Init(@PItem(Result).FValue);
      end;

      Exit;
    end else
    begin
      Instance := Instance.Grow;
    end;
  until (False);
end;

procedure TCustomDictionary<TKey,TValue>.DisposeItem(Item: Pointer{Item});
var
  {$ifdef SMARTGENERICS}
  VType: Integer;
  {$endif}
  Count: NativeInt;
  Parent: Pointer;
  TopItem, Current: PItem;
  Index: NativeInt;
begin
  // top item, count
  Count := Self.FCount.Native;
  Dec(Count);
  Self.FCount.Native := Count;
  TopItem := Pointer(FItems);
  Inc(TopItem, Count);
  if (Item <> TopItem) then
  begin
    // change TopItem.Parent.Next --> Item
    Parent := Pointer(@FHashTable[NativeInt(Cardinal(TopItem.HashCode)) and FHashTableMask]);
    repeat
      Current := PItem(Parent^);
      if (Current = TopItem) then
      begin
        PItem(Parent^) := Item;
        Break;
      end;
      {$if (CompilerVersion >= 22) and (CompilerVersion <= 24)}
        Parent := Pointer(NativeUInt(Current) + (SizeOf(TKey) + SizeOf(TValue)));
      {$else}
        Parent := Pointer(@Current.FNext);
      {$ifend}
    until (False);
  end;

  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<TKey>.Weak) or (TRAIIHelper<TValue>.Weak) then
  {$else}
  if (TRAIIHelper<TKey>.FOptions.FWeak) or (TRAIIHelper<TValue>.FOptions.FWeak) then
  {$endif}
  begin
    // weak case: Copy(TopItem --> Item), Finalize(TopItem)
    PItem(Item)^ := TopItem^;
    System.Finalize(TopItem^);
  end else
  {$endif}
  begin
    // standard case: Finalize(Item) + Move(DestItem, Item)
    // finalize Item.Key
    {$ifdef SMARTGENERICS}
    case GetTypeKind(TKey) of
      {$ifdef AUTOREFCOUNT}
      tkClass,
      {$endif}
      tkWString, tkLString, tkUString, tkInterface, tkDynArray:
      begin
        if (PKeyRec(Item).Native <> 0) then
        case GetTypeKind(TKey) of
          {$ifdef AUTOREFCOUNT}
          tkClass:
          begin
            TRAIIHelper.RefObjClear(@PKeyRec(Item).Native);
          end;
          {$endif}
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            TRAIIHelper.WStrClear(@PKeyRec(Item).Native);
          end;
          {$else}
          tkWString,
          {$endif}
          tkLString, tkUString:
          begin
            TRAIIHelper.ULStrClear(@PKeyRec(Item).Native);
          end;
          tkInterface:
          begin
            IInterface(PKeyRec(Item).Native)._Release;
          end;
          tkDynArray:
          begin
            TRAIIHelper.DynArrayClear(@PKeyRec(Item).Native, TypeInfo(TKey));
          end;
        end;
      end;
      {$ifdef WEAKINSTREF}
      tkMethod:
      begin
        if (PKeyRec(Item).Method.Data <> nil) then
          TRAIIHelper.WeakMethodClear(@PKeyRec(Item).Method.Data);
      end;
      {$endif}
      tkVariant:
      begin
        VType := PKeyRec(Item).VarData.VType;
        if (VType and TRAIIHelper.varDeepData <> 0) then
        case VType of
          varBoolean, varUnknown+1..varUInt64: ;
        else
          System.VarClear(Variant(PKeyRec(Item).VarData));
        end;
      end
    else
      TRAIIHelper<TKey>.Clear(@PItem(Item).FKey);
    end;
    {$else}
    TRAIIHelper<TKey>.Clear(@PItem(Item).FKey);
    {$endif}

    // finalize Item.Value
    {$ifdef SMARTGENERICS}
    case GetTypeKind(TValue) of
      {$ifdef AUTOREFCOUNT}
      tkClass,
      {$endif}
      tkWString, tkLString, tkUString, tkInterface, tkDynArray:
      begin
        if (PValueRec(Item).Native <> 0) then
        case GetTypeKind(TValue) of
          {$ifdef AUTOREFCOUNT}
          tkClass:
          begin
            TRAIIHelper.RefObjClear(@PValueRec(Item).Native);
          end;
          {$endif}
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            TRAIIHelper.WStrClear(@PValueRec(Item).Native);
          end;
          {$else}
          tkWString,
          {$endif}
          tkLString, tkUString:
          begin
            TRAIIHelper.ULStrClear(@PValueRec(Item).Native);
          end;
          tkInterface:
          begin
            IInterface(PValueRec(Item).Native)._Release;
          end;
          tkDynArray:
          begin
            TRAIIHelper.DynArrayClear(@PValueRec(Item).Native, TypeInfo(TValue));
          end;
        end;
      end;
      {$ifdef WEAKINSTREF}
      tkMethod:
      begin
        if (PValueRec(Item).Method.Data <> nil) then
          TRAIIHelper.WeakMethodClear(@PValueRec(Item).Method.Data);
      end;
      {$endif}
      tkVariant:
      begin
        VType := PValueRec(Item).VarData.VType;
        if (VType and TRAIIHelper.varDeepData <> 0) then
        case VType of
          varBoolean, varUnknown+1..varUInt64: ;
        else
          System.VarClear(Variant(PValueRec(Item).VarData));
        end;
      end
    else
      TRAIIHelper<TValue>.Clear(@PItem(Item).FValue);
    end;
    {$else}
    TRAIIHelper<TValue>.Clear(@PItem(Item).FValue);
    {$endif}

    // move TopItem --> Item
    if (Item <> TopItem) then
    begin
      case SizeOf(TItem) of
        1: TRAIIHelper.T1(Pointer(Item)^) := TRAIIHelper.T1(Pointer(TopItem)^);
        2: TRAIIHelper.T2(Pointer(Item)^) := TRAIIHelper.T2(Pointer(TopItem)^);
        3: TRAIIHelper.T3(Pointer(Item)^) := TRAIIHelper.T3(Pointer(TopItem)^);
        4: TRAIIHelper.T4(Pointer(Item)^) := TRAIIHelper.T4(Pointer(TopItem)^);
        5: TRAIIHelper.T5(Pointer(Item)^) := TRAIIHelper.T5(Pointer(TopItem)^);
        6: TRAIIHelper.T6(Pointer(Item)^) := TRAIIHelper.T6(Pointer(TopItem)^);
        7: TRAIIHelper.T7(Pointer(Item)^) := TRAIIHelper.T7(Pointer(TopItem)^);
        8: TRAIIHelper.T8(Pointer(Item)^) := TRAIIHelper.T8(Pointer(TopItem)^);
        9: TRAIIHelper.T9(Pointer(Item)^) := TRAIIHelper.T9(Pointer(TopItem)^);
       10: TRAIIHelper.T10(Pointer(Item)^) := TRAIIHelper.T10(Pointer(TopItem)^);
       11: TRAIIHelper.T11(Pointer(Item)^) := TRAIIHelper.T11(Pointer(TopItem)^);
       12: TRAIIHelper.T12(Pointer(Item)^) := TRAIIHelper.T12(Pointer(TopItem)^);
       13: TRAIIHelper.T13(Pointer(Item)^) := TRAIIHelper.T13(Pointer(TopItem)^);
       14: TRAIIHelper.T14(Pointer(Item)^) := TRAIIHelper.T14(Pointer(TopItem)^);
       15: TRAIIHelper.T15(Pointer(Item)^) := TRAIIHelper.T15(Pointer(TopItem)^);
       16: TRAIIHelper.T16(Pointer(Item)^) := TRAIIHelper.T16(Pointer(TopItem)^);
       17: TRAIIHelper.T17(Pointer(Item)^) := TRAIIHelper.T17(Pointer(TopItem)^);
       18: TRAIIHelper.T18(Pointer(Item)^) := TRAIIHelper.T18(Pointer(TopItem)^);
       19: TRAIIHelper.T19(Pointer(Item)^) := TRAIIHelper.T19(Pointer(TopItem)^);
       20: TRAIIHelper.T20(Pointer(Item)^) := TRAIIHelper.T20(Pointer(TopItem)^);
       21: TRAIIHelper.T21(Pointer(Item)^) := TRAIIHelper.T21(Pointer(TopItem)^);
       22: TRAIIHelper.T22(Pointer(Item)^) := TRAIIHelper.T22(Pointer(TopItem)^);
       23: TRAIIHelper.T23(Pointer(Item)^) := TRAIIHelper.T23(Pointer(TopItem)^);
       24: TRAIIHelper.T24(Pointer(Item)^) := TRAIIHelper.T24(Pointer(TopItem)^);
       25: TRAIIHelper.T25(Pointer(Item)^) := TRAIIHelper.T25(Pointer(TopItem)^);
       26: TRAIIHelper.T26(Pointer(Item)^) := TRAIIHelper.T26(Pointer(TopItem)^);
       27: TRAIIHelper.T27(Pointer(Item)^) := TRAIIHelper.T27(Pointer(TopItem)^);
       28: TRAIIHelper.T28(Pointer(Item)^) := TRAIIHelper.T28(Pointer(TopItem)^);
       29: TRAIIHelper.T29(Pointer(Item)^) := TRAIIHelper.T29(Pointer(TopItem)^);
       30: TRAIIHelper.T30(Pointer(Item)^) := TRAIIHelper.T30(Pointer(TopItem)^);
       31: TRAIIHelper.T31(Pointer(Item)^) := TRAIIHelper.T31(Pointer(TopItem)^);
       32: TRAIIHelper.T32(Pointer(Item)^) := TRAIIHelper.T32(Pointer(TopItem)^);
       33: TRAIIHelper.T33(Pointer(Item)^) := TRAIIHelper.T33(Pointer(TopItem)^);
       34: TRAIIHelper.T34(Pointer(Item)^) := TRAIIHelper.T34(Pointer(TopItem)^);
       35: TRAIIHelper.T35(Pointer(Item)^) := TRAIIHelper.T35(Pointer(TopItem)^);
       36: TRAIIHelper.T36(Pointer(Item)^) := TRAIIHelper.T36(Pointer(TopItem)^);
       37: TRAIIHelper.T37(Pointer(Item)^) := TRAIIHelper.T37(Pointer(TopItem)^);
       38: TRAIIHelper.T38(Pointer(Item)^) := TRAIIHelper.T38(Pointer(TopItem)^);
       39: TRAIIHelper.T39(Pointer(Item)^) := TRAIIHelper.T39(Pointer(TopItem)^);
       40: TRAIIHelper.T40(Pointer(Item)^) := TRAIIHelper.T40(Pointer(TopItem)^);
      else
        System.Move(TopItem^, Item^, SizeOf(TItem));
      end;
    end;
  end;
end;

function TCustomDictionary<TKey,TValue>.ContainsValue(const Value: TValue): Boolean;
{$ifdef SMARTGENERICS}
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  next_item, done;
{$endif}
var
  i: NativeInt;
  Item: PValue;
  {$ifdef SMARTGENERICS}
  Left, Right: PByte;
  Count, Offset: NativeUInt;
  {$else}
  Comparer: Pointer;
  ComparerEquals: function(const Left, Right: TValue): Boolean of object;
  {$endif}
begin
  Item := @Self.FItems[0].FValue;

  {$ifdef SMARTGENERICS}
  begin
    for i := 1 to Self.FCount.Native do
    begin
      if (GetTypeKind(TValue) = tkVariant) then
      begin
        if (not InterfaceDefaults.Equals_Var(nil, PVarData(@Value), PVarData(Item))) then goto next_item;
      end else
      if (GetTypeKind(TValue) = tkClass) then
      begin
        Left := PPointer(@Value)^;
        Right := PPointer(Item)^;

        if (Assigned(Left)) then
        begin
          if (PPointer(Pointer(Left)^)[vmtEquals div SizeOf(Pointer)] = @TObject.Equals) then
          begin
            if (Left <> Right) then goto next_item;
          end else
          begin
            if (not TObject(Left).Equals(TObject(Right))) then goto next_item;
          end;
        end else
        begin
          if (Right <> nil) then goto next_item;
        end;
      end else
      if (GetTypeKind(TValue) = tkFloat) then
      begin
        case SizeOf(TValue) of
          4:
          begin
            if (PSingle(@Value)^ <> PSingle(Result)^) then goto next_item;
          end;
          10:
          begin
            if (PExtended(@Value)^ <> PExtended(Result)^) then goto next_item;
          end;
        else
        {$ifdef LARGEINT}
          if (PInt64(@Value)^ <> PInt64(Result)^) then
        {$else .SMALLINT}
          if ((PPoint(@Value).X - PPoint(Result).X) or (PPoint(@Value).Y - PPoint(Result).Y) <> 0) then
        {$endif}
          begin
            if (TRAIIHelper<TValue>.Options.ItemSize < 0) then goto next_item;
            if (PDouble(@Value)^ <> PDouble(Result)^) then goto next_item;
          end;
        end;
      end else
      if (not (GetTypeKind(TValue) in [tkDynArray, tkString, tkLString, tkWString, tkUString])) and
        (SizeOf(TValue) <= 16) then
      begin
        // small binary
        if (SizeOf(TValue) <> 0) then
        with PData16(@Value)^ do
        begin
          if (SizeOf(TValue) >= SizeOf(Integer)) then
          begin
            if (SizeOf(TValue) >= SizeOf(Int64)) then
            begin
              {$ifdef LARGEINT}
              if (Int64s[0] <> PData16(Item).Int64s[0]) then goto next_item;
              {$else}
              if (Integers[0] <> PData16(Item).Integers[0]) then goto next_item;
              if (Integers[1] <> PData16(Item).Integers[1]) then goto next_item;
              {$endif}

              if (SizeOf(TValue) = 16) then
              begin
                {$ifdef LARGEINT}
                if (Int64s[1] <> PData16(Item).Int64s[1]) then goto next_item;
                {$else}
                if (Integers[2] <> PData16(Item).Integers[2]) then goto next_item;
                if (Integers[3] <> PData16(Item).Integers[3]) then goto next_item;
                {$endif}
              end else
              if (SizeOf(TValue) >= 12) then
              begin
                if (Integers[2] <> PData16(Item).Integers[2]) then goto next_item;
              end;
            end else
            begin
              if (Integers[0] <> PData16(Item).Integers[0]) then goto next_item;
            end;
          end;

          if (SizeOf(TValue) and 2 <> 0) then
          begin
            if (Words[(SizeOf(TValue) and -4) shr 1] <> PData16(Item).Words[(SizeOf(TValue) and -4) shr 1]) then goto next_item;
          end;
          if (SizeOf(TValue) and 1 <> 0) then
          begin
            if (Bytes[SizeOf(TValue) and -2] <> PData16(Item).Bytes[SizeOf(TValue) and -2]) then goto next_item;
          end;
        end;
      end else
      begin
        if (GetTypeKind(TValue) in [tkDynArray, tkString, tkLString, tkWString, tkUString]) then
        begin
          // dynamic size
          if (GetTypeKind(TValue) = tkString) then
          begin
            Left := Pointer(@Value);
            Right := Pointer(Item);
            if (PValue(Left) = {Right}Item) then goto cmp0;
            Count := Left^;
            if (Count <> Right^) then goto next_item;
            if (Count = 0) then goto cmp0;
            // compare last bytes
            if (Left[Count] <> Right[Count]) then goto next_item;
          end else
          // if (GetTypeKind(TValue) in [tkDynArray, tkLString, tkWString, tkUString]) then
          begin
            Left := PPointer(@Value)^;
            Right := PPointer(Item)^;
            if (Left = Right) then goto cmp0;
            if (Left = nil) then
            begin
              {$ifdef MSWINDOWS}
              if (GetTypeKind(TValue) = tkWString) then
              begin
                Dec(Right, SizeOf(Cardinal));
                if (PCardinal(Right)^ = 0) then goto cmp0;
              end;
              {$endif}
              goto next_item;
            end;
            if (Right = nil) then
            begin
              {$ifdef MSWINDOWS}
              if (GetTypeKind(TValue) = tkWString) then
              begin
                Dec(Left, SizeOf(Cardinal));
                if (PCardinal(Left)^ = 0) then goto cmp0;
              end;
              {$endif}
              goto next_item;
            end;

            if (GetTypeKind(TValue) = tkDynArray) then
            begin
              Dec(Left, SizeOf(NativeUInt));
              Dec(Right, SizeOf(NativeUInt));
              Count := PNativeUInt(Left)^;
              if (Count <> PNativeUInt(Right)^) then goto next_item;
              NativeInt(Count) := NativeInt(Count) * TRAIIHelper<TValue>.Options.ItemSize;
              Inc(Left, SizeOf(NativeUInt));
              Inc(Right, SizeOf(NativeUInt));
            end else
            // if (GetTypeKind(TValue) in [tkLString, tkWString, tkUString]) then
            begin
              Dec(Left, SizeOf(Cardinal));
              Dec(Right, SizeOf(Cardinal));
              Count := PCardinal(Left)^;
              if (Cardinal(Count) <> PCardinal(Right)^) then goto next_item;
              Inc(Left, SizeOf(Cardinal));
              Inc(Right, SizeOf(Cardinal));
            end;
          end;

          // compare last (after cardinal) words
          if (GetTypeKind(TValue) in [tkDynArray, tkString, tkLString]) then
          begin
            if (GetTypeKind(TValue) in [tkString, tkLString]) {ByteStrings + 2} then
            begin
              Inc(Count);
            end;
            if (Count and 2 <> 0) then
            begin
              Offset := Count and -4;
              Inc(Left, Offset);
              Inc(Right, Offset);
              if (PWord(Left)^ <> PWord(Right)^) then goto next_item;
              Offset := Count;
              Offset := Offset and -4;
              Dec(Left, Offset);
              Dec(Right, Offset);
            end;
          end else
          // modify Count to have only cardinals to compare
          // if (GetTypeKind(TValue) in [tkWString, tkUString]) {UnicodeStrings + 2} then
          begin
            {$ifdef MSWINDOWS}
            if (GetTypeKind(TValue) = tkWString) then
            begin
              if (Count = 0) then goto cmp0;
            end else
            {$endif}
            begin
              Inc(Count, Count);
            end;
            Inc(Count, 2);
          end;

          {$ifdef LARGEINT}
          if (Count and 4 <> 0) then
          begin
            Offset := Count and -8;
            Inc(Left, Offset);
            Inc(Right, Offset);
            if (PCardinal(Left)^ <> PCardinal(Right)^) then goto next_item;
            Dec(Left, Offset);
            Dec(Right, Offset);
          end;
          {$endif}
        end else
        begin
          // non-dynamic (constant) size binary > 16
          if (SizeOf(TValue) and {$ifdef LARGEINT}7{$else}3{$endif} <> 0) then
          with PData16(@Value)^ do
          begin
            {$ifdef LARGEINT}
            if (SizeOf(TValue) and 4 <> 0) then
            begin
              if (Integers[(SizeOf(TValue) and -8) shr 2] <> PData16(Item).Integers[(SizeOf(TValue) and -8) shr 2]) then goto next_item;
            end;
            {$endif}
            if (SizeOf(TValue) and 2 <> 0) then
            begin
              if (Words[(SizeOf(TValue) and -4) shr 1] <> PData16(Item).Words[(SizeOf(TValue) and -4) shr 1]) then goto next_item;
            end;
            if (SizeOf(TValue) and 1 <> 0) then
            begin
              if (Bytes[SizeOf(TValue) and -2] <> PData16(Item).Bytes[SizeOf(TValue) and -2]) then goto next_item;
            end;
          end;
          Left := Pointer(@Value);
          Right := Pointer(Item);
          Count := SizeOf(TValue);
        end;

        // natives (40 bytes static) compare
        Count := Count shr {$ifdef LARGEINT}3{$else}2{$endif};
        case Count of
        {$ifdef SMALLINT}
         10: goto cmp10;
          9: goto cmp9;
          8: goto cmp8;
          7: goto cmp7;
          6: goto cmp6;
        {$endif}
          5: goto cmp5;
          4: goto cmp4;
          3: goto cmp3;
          2: goto cmp2;
          1: goto cmp1;
          0: goto cmp0;
        else
          repeat
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Dec(Count);
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          until (Count = {$ifdef LARGEINT}5{$else}10{$endif});

          {$ifdef SMALLINT}
          cmp10:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp9:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp8:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp7:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp6:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          {$endif}
          cmp5:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp4:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp3:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp2:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp1:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          cmp0:
        end;
      end;
      goto done;

    next_item:
      Inc(NativeUInt(Item), SizeOf(TItem));
    end;

    Result := False;
    Exit;
  done:
    Result := True;
    Exit;
  end;
  {$else}
  begin
    Comparer := InterfaceDefaults.TDefaultEqualityComparer<TValue>.Create;
    TMethod(ComparerEquals) := IntfMethod(Comparer, 3);
    Result := False;
    for i := 1 to Self.FCount.Native do
    begin
      Result := (ComparerEquals(Value, Item^));
      if (Result) then Break;
      Inc(NativeUInt(Item), SizeOf(TItem));
    end;
    ClearMethod(ComparerEquals);
    Exit;
  end;
  {$endif}
end;

class function TCustomDictionary<TKey,TValue>.IntfMethod(Intf: Pointer; MethodNum: NativeUInt): TMethod;
begin
  Result.Data := Intf;
  Result.Code := PPointer(PNativeUInt(Intf)^ + MethodNum * SizeOf(Pointer))^;
end;

class procedure TCustomDictionary<TKey,TValue>.ClearMethod(var Method);
begin
  {$ifdef WEAKINSTREF}
  TMethod(Method).Data := nil;
  {$endif}
end;

procedure TCustomDictionary<TKey,TValue>.SetKeyNotify(const Value: TCollectionNotifyEvent<TKey>);
begin
  if (TMethod(FOnKeyNotify).Code <> TMethod(Value).Code) or
    (TMethod(FOnKeyNotify).Data <> TMethod(Value).Data) then
  begin
    FOnKeyNotify := Value;
    SetNotifyMethods;
  end;
end;

procedure TCustomDictionary<TKey,TValue>.SetValueNotify(const Value: TCollectionNotifyEvent<TValue>);
begin
  if (TMethod(FOnValueNotify).Code <> TMethod(Value).Code) or
    (TMethod(FOnValueNotify).Data <> TMethod(Value).Data) then
  begin
    FOnValueNotify := Value;
    SetNotifyMethods;
  end;
end;

procedure TCustomDictionary<TKey,TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, Key, Action);
end;

procedure TCustomDictionary<TKey,TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);
end;

procedure TCustomDictionary<TKey,TValue>.KeyNotifyCaller(Sender: TObject; const Item: TKey; Action: TCollectionNotification);
begin
  Self.KeyNotify(Item, Action);
end;

procedure TCustomDictionary<TKey,TValue>.ValueNotifyCaller(Sender: TObject; const Item: TValue; Action: TCollectionNotification);
begin
  Self.ValueNotify(Item, Action);
end;

procedure TCustomDictionary<TKey,TValue>.ItemNotifyCaller(const Item: TItem; Action: TCollectionNotification);
begin
  Self.KeyNotify(Item.Key, Action);
  Self.ValueNotify(Item.Value, Action);
end;

procedure TCustomDictionary<TKey,TValue>.ItemNotifyEvents(const Item: TItem; Action: TCollectionNotification);
begin
  Self.FInternalKeyNotify(Self, Item.Key, Action);
  Self.FInternalValueNotify(Self, Item.Value, Action);
end;

procedure TCustomDictionary<TKey,TValue>.ItemNotifyKey(const Item: TItem; Action: TCollectionNotification);
begin
  Self.FInternalKeyNotify(Self, Item.Key, Action);
end;

procedure TCustomDictionary<TKey,TValue>.ItemNotifyValue(const Item: TItem; Action: TCollectionNotification);
begin
  Self.FInternalValueNotify(Self, Item.Value, Action);
end;

procedure TCustomDictionary<TKey,TValue>.SetNotifyMethods;
var
  VMTKeyNotify: procedure(const Key: TKey; Action: TCollectionNotification) of object;
  VMTValueNotify: procedure(const Value: TValue; Action: TCollectionNotification) of object;
begin
  // FInternalKeyNotify, FInternalValueNotify
  VMTKeyNotify := Self.KeyNotify;
  VMTValueNotify := Self.ValueNotify;
  if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) then
  begin
    // FInternalKeyNotify := Self.KeyNotifyCaller;
    TMethod(FInternalKeyNotify).Data := Pointer(Self);
    TMethod(FInternalKeyNotify).Code := @TCustomDictionary<TKey,TValue>.KeyNotifyCaller;
  end else
  begin
    TMethod(FInternalKeyNotify) := TMethod(Self.FOnKeyNotify);
  end;
  if (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
  begin
    // FInternalValueNotify := Self.ValueNotifyCaller;
    TMethod(FInternalValueNotify).Data := Pointer(Self);
    TMethod(FInternalValueNotify).Code := @TCustomDictionary<TKey,TValue>.ValueNotifyCaller;
  end else
  begin
    TMethod(FInternalValueNotify) := TMethod(Self.FOnValueNotify);
  end;

  // FInternalItemNotify
  if Assigned(FInternalKeyNotify) then
  begin
    // FInternalItemNotify := Self.ItemNotifyKey;
    TMethod(FInternalItemNotify).Data := Pointer(Self);
    TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyKey;

    if Assigned(FInternalValueNotify) then
    begin
      // FInternalItemNotify := Self.ItemNotifyEvents;
      TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyEvents;

      if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) or
        (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
      begin
        // FInternalItemNotify := Self.ItemNotifyCaller;
        TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyCaller;
      end;
    end;
  end else
  if Assigned(FInternalValueNotify) then
  begin
    // FInternalItemNotify := Self.ItemNotifyValue;
    TMethod(FInternalItemNotify).Data := Pointer(Self);
    TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyValue;
  end else
  begin
    // FInternalItemNotify := nil;
    TMethod(FInternalItemNotify).Data := nil;
    TMethod(FInternalItemNotify).Code := nil;
  end;
end;


{ TDictionary }

constructor TDictionary<TKey,TValue>.Create(ACapacity: Integer);
begin
  Create(ACapacity, nil);
end;

constructor TDictionary<TKey,TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

constructor TDictionary<TKey,TValue>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
begin
  if ACapacity < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  // comparer
  FComparer := AComparer;
  if (FComparer = nil) then
    Pointer(FComparer) := InterfaceDefaults.TDefaultEqualityComparer<TKey>.Create;

  // comparer methods
  TMethod(FComparerEquals) := IntfMethod(Pointer(FComparer), 3);
  TMethod(FComparerGetHashCode) := IntfMethod(Pointer(FComparer), 4);

  // initialization
  inherited Create(ACapacity);
end;

constructor TDictionary<TKey,TValue>.Create(const Collection: TEnumerable<TPair<TKey,TValue>>);
begin
  Create(Collection, nil);
end;

constructor TDictionary<TKey,TValue>.Create(const Collection: TEnumerable<TPair<TKey,TValue>>;
  const AComparer: IEqualityComparer<TKey>);
var
  Item: TPair<TKey,TValue>;
begin
  Create(0, AComparer);
  for Item in Collection do
    AddOrSetValue(Item.Key, Item.Value);
end;

destructor TDictionary<TKey,TValue>.Destroy;
begin
  inherited;

  ClearMethod(FComparerEquals);
  ClearMethod(FComparerGetHashCode);
end;

function TDictionary<TKey,TValue>.InternalFindItem(const Key: TKey; const FindMode: Integer): Pointer{PItem};
var
  Parent: Pointer;
  Item: TCustomDictionary<TKey,TValue>.PItem;
  HashCode, Mode: Integer;
  Stored: TInternalFindStored;
begin
  // hash code
  HashCode := Self.FComparerGetHashCode(Key);

  // parent
  Pointer(Item{Parent}) := @FHashTable[NativeInt(Cardinal(HashCode)) and FHashTableMask];
  Dec(NativeUInt(Item{Parent}), SizeOf(TKey) + SizeOf(TValue));

  // find
  Stored.HashCode := HashCode;
  repeat
    // hash code item
    repeat
      Parent := Pointer(@Item.FNext);
      Item := Item.FNext;
    until (Item = nil) or (Stored.HashCode = Item.HashCode);

    if (Item <> nil) then
    begin
      // hash code item found
      Stored.Parent := Parent;

      // keys comparison
      if (not Self.FComparerEquals(Key, Item.Key)) then Continue;

      // found
      Mode := FindMode;
      if (Mode and FOUND_MASK = 0) then Break;
      Cardinal(Mode) := Cardinal(Mode) and FOUND_MASK;
      if (Mode <> FOUND_EXCEPTION) then
      begin
        if (Mode = FOUND_DELETE) then
        begin
          Pointer(Stored.Parent^) := Item.FNext;
          if (not Assigned(Self.FInternalItemNotify)) then
          begin
            Self.DisposeItem(Item);
          end else
          begin
            Self.FInternalItemNotify(Item^, cnRemoved);
            Self.DisposeItem(Item);
          end;
        end else
        // if (Mode = FOUND_REPLACE) then
        begin
          if (not Assigned(Self.FInternalValueNotify)) then
          begin
            Item.FValue := FInternalFindValue^;
          end else
          begin
            Self.FInternalValueNotify(Self, Item.Value, cnRemoved);
            Item.FValue := FInternalFindValue^;
            Self.FInternalValueNotify(Self, Item.Value, cnAdded);
          end;
        end;
      end else
      begin
        raise EListError.CreateRes(Pointer(@SGenericDuplicateItem));
      end;
      Break;
    end;

    // not found (Item = nil)
    Mode := FindMode;
    if (Mode and EMPTY_MASK = 0) then Break;
    if (Mode and EMPTY_EXCEPTION = 0) then
    begin
      // EMPTY_NEW
      Item := Self.NewItem;
      Item.FKey := Key;
      Item.FHashCode := Stored.HashCode;
      Parent := Pointer(@Self.FHashTable[NativeInt(Cardinal(Stored.HashCode)) and Self.FHashTableMask]);
      Item.FNext := Pointer(Parent^);
      Pointer(Parent^) := Item;
      Item.FValue := FInternalFindValue^;
      if (Assigned(Self.FInternalItemNotify)) then
      begin
        Self.FInternalItemNotify(Item^, cnAdded);
      end;
      Break;
    end else
    begin
      raise EListError.CreateRes(Pointer(@SGenericItemNotFound));
    end;
  until (False);

  Result := Item;
end;

function TDictionary<TKey,TValue>.GetItem(const Key: TKey): TValue;
begin
  Result := TCustomDictionary<TKey,TValue>.PItem(Self.InternalFindItem(Key, FOUND_NONE + EMPTY_EXCEPTION)).Value;
end;

procedure TDictionary<TKey,TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_REPLACE + EMPTY_EXCEPTION);
end;

function TDictionary<TKey,TValue>.Find(const Key: TKey): Pointer{PItem};
begin
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
end;

function TDictionary<TKey,TValue>.FindOrAdd(const Key: TKey): Pointer{PItem};
begin
  Self.FInternalFindValue := @Self.FDefaultValue;
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NEW);
end;

procedure TDictionary<TKey,TValue>.Add(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_EXCEPTION + EMPTY_NEW);
end;

procedure TDictionary<TKey,TValue>.Remove(const Key: TKey);
begin
  Self.InternalFindItem(Key, FOUND_DELETE + EMPTY_NONE)
end;

function TDictionary<TKey,TValue>.ExtractPair(const Key: TKey): TPair<TKey,TValue>;
var
  Parent: Pointer;
  Item, Current: TCustomDictionary<TKey,TValue>.PItem;
begin
  Result.Key := Key;
  Item := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
  if (Item = nil) then
  begin
    Result.Value := Default(TValue);
    Exit;
  end;

  Result.Value := Item.Value;
  Parent := Pointer(@Self.FHashTable[NativeInt(Cardinal(Item.HashCode)) and Self.FHashTableMask]);
  repeat
    Current := TCustomDictionary<TKey,TValue>.PItem(Parent^);

    if (Item = Current) then
    begin
      TCustomDictionary<TKey,TValue>.PItem(Parent^) := Item.FNext;

      if (not Assigned(Self.FInternalItemNotify)) then
      begin
        Self.DisposeItem(Item);
      end else
      begin
        Self.FInternalItemNotify(Item^, cnExtracted);
        Self.DisposeItem(Item);
      end;

      Exit;
    end;

    {$if (CompilerVersion >= 22) and (CompilerVersion <= 24)}
      Parent := Pointer(NativeUInt(Current) + (SizeOf(TKey) + SizeOf(TValue)));
    {$else}
      Parent := Pointer(@Current.FNext);
    {$ifend}
  until (False);
end;

function TDictionary<TKey,TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
var
  Item: PItem;
begin
  Item := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
  if Assigned(Item) then
  begin
    Value := Item.Value;
    Result := True;
  end else
  begin
    Value := Default(TValue);
    Result := False;
  end;
end;

procedure TDictionary<TKey,TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_REPLACE + EMPTY_NEW);
end;

function TDictionary<TKey,TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Result := (Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE) <> nil);
end;


{ TRapidDictionary<TKey,TValue> }

constructor TRapidDictionary<TKey,TValue>.Create(ACapacity: Integer);
{$if CompilerVersion <= 28}
var
  Comparer: Pointer;
{$ifend}
begin
  {$if CompilerVersion <= 28}
  Comparer := InterfaceDefaults.TDefaultEqualityComparer<TKey>.Create;
  TMethod(FComparerEquals) := IntfMethod(Pointer(Comparer), 3);
  TMethod(FComparerGetHashCode) := IntfMethod(Pointer(Comparer), 4);
  {$ifend}

  inherited;
end;

constructor TRapidDictionary<TKey,TValue>.Create(const Collection: TEnumerable<TPair<TKey,TValue>>);
var
  Item: TPair<TKey,TValue>;
begin
  inherited Create;
  for Item in Collection do
    AddOrSetValue(Item.Key, Item.Value);
end;

destructor TRapidDictionary<TKey,TValue>.Destroy;
begin
  inherited;

  {$if CompilerVersion <= 28}
  ClearMethod(FComparerEquals);
  ClearMethod(FComparerGetHashCode);
  {$ifend}
end;

{$if CompilerVersion >= 29}
function TRapidDictionary<TKey,TValue>.InternalFindItem(const Key: TKey; const FindMode: Integer): TCustomDictionary<TKey,TValue>.PItem;
label
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10,
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  hash_calculated, next_item, not_found;
var
  Parent: ^PItem;
  HashCode, Mode, M: Integer;
  Stored: TInternalFindStored;
  Left, Right: PByte;
  Count, Offset: NativeUInt;
  _Self1, _Self2: TRapidDictionary<TKey,TValue>;
begin
  // stores, hash code
  Stored.Self := Self;
  if (GetTypeKind(TKey) = tkVariant) then
  begin
    HashCode := InterfaceDefaults.GetHashCode_Var(nil, PVarData(@Key));
  end else
  if (GetTypeKind(TKey) = tkClass) then
  begin
    Left := PPointer(@Key)^;
    if (Assigned(Left)) then
    begin
      if (PPointer(Pointer(Left)^)[vmtGetHashCode div SizeOf(Pointer)] = @TObject.GetHashCode) then
      begin
        {$ifdef LARGEINT}
          HashCode := Integer(NativeInt(Left) xor (NativeInt(Left) shr 32));
        {$else .SMALLINT}
          HashCode := Integer(Left);
        {$endif}
        Inc(HashCode, ((HashCode shr 8) * 63689) + ((HashCode shr 16) * -1660269137) +
            ((HashCode shr 24) * -1092754919));
      end else
      begin
        HashCode := TObject(Left).GetHashCode;
      end;
    end else
    begin
      HashCode := 0;
    end;
  end else
  if (GetTypeKind(TKey) in [tkInterface, tkClassRef, tkPointer, tkProcedure]) then
  begin
    {$ifdef LARGEINT}
      HashCode := Integer(PNativeInt(@Key)^ xor (PNativeInt(@Key)^ shr 32));
    {$else .SMALLINT}
      HashCode := PInteger(@Key)^;
    {$endif}
    Inc(HashCode, ((HashCode shr 8) * 63689) + ((HashCode shr 16) * -1660269137) +
        ((HashCode shr 24) * -1092754919));
  end else
  if (GetTypeKind(TKey) = tkFloat) then
  begin
    HashCode := 0;
    case SizeOf(TKey) of
      4:
      begin
        if (PSingle(@Key)^ = 0) then goto hash_calculated;
        Frexp(PSingle(@Key)^, Stored.SingleRec.Mantissa, Stored.SingleRec.Exponent);
        HashCode := Stored.SingleRec.Exponent + Stored.SingleRec.HighInt * 63689;
      end;
      10:
      begin
        if (PExtended(@Key)^ = 0) then goto hash_calculated;
        Frexp(PExtended(@Key)^, Stored.ExtendedRec.Mantissa, Stored.ExtendedRec.Exponent);
        HashCode := Stored.ExtendedRec.Exponent + Stored.ExtendedRec.LowInt * 63689 +
          Stored.ExtendedRec.HighInt * -1660269137 + Integer(Stored.ExtendedRec.Middle) * -1092754919;
      end;
    else
      if (TRAIIHelper<TKey>.Options.ItemSize < 0) then
      begin
        HashCode := PPoint(@Key).X + PPoint(@Key).Y * 63689;
      end else
      begin
        if (PDouble(@Key)^ = 0) then goto hash_calculated;
        Frexp(PDouble(@Key)^, Stored.DoubleRec.Mantissa, Stored.DoubleRec.Exponent);
        HashCode := Stored.DoubleRec.Exponent + Stored.DoubleRec.LowInt * 63689 +
          Stored.DoubleRec.HighInt * -1660269137;
      end;
    end;

    Inc(HashCode, ((HashCode shr 8) * 63689) + ((HashCode shr 16) * -1660269137) +
        ((HashCode shr 24) * -1092754919));
  end else
  if (not (GetTypeKind(TKey) in [tkDynArray, tkString, tkLString, tkWString, tkUString])) and
    (SizeOf(TKey) <= 16) then
  begin
    // small binary
    if (SizeOf(TKey) >= SizeOf(Integer)) then
    begin
      if (SizeOf(TKey) = SizeOf(Integer)) then
      begin
        HashCode := PInteger(@Key)^;
      end else
      if (SizeOf(TKey) = SizeOf(Int64)) then
      begin
        HashCode := PPoint(@Key).X + PPoint(@Key).Y * 63689;
      end else
      begin
        Left := Pointer(@Key);
        HashCode := Integer(SizeOf(TKey)) + PInteger(Left + (SizeOf(TKey) - SizeOf(Integer)))^ * 63689;
        HashCode := HashCode * 2012804575 + PInteger(Left)[0];
        if (SizeOf(TKey) > SizeOf(Integer) * 2) then
        begin
          HashCode := HashCode * -1092754919 + PInteger(Left)[1];
          if (SizeOf(TKey) > SizeOf(Integer) * 3) then
          begin
            HashCode := HashCode * -1660269137 + PInteger(Left)[2];
          end;
        end;
      end;

      Inc(HashCode, ((HashCode shr 8) * 63689) + ((HashCode shr 16) * -1660269137) +
        ((HashCode shr 24) * -1092754919));
    end else
    if (SizeOf(TKey) <> 0) then
    begin
      Left := Pointer(@Key);
      HashCode := Integer(Left[0]);
      HashCode := HashCode + (HashCode shr 4) * 63689;
      if (SizeOf(TKey) > 1) then
      begin
        HashCode := HashCode + Integer(Left[1]) * -1660269137;
        if (SizeOf(TKey) > 2) then
        begin
          HashCode := HashCode + Integer(Left[2]) * -1092754919;
        end;
      end;
    end else
    begin
      HashCode := 0;
    end;
  end else
  begin
    if (GetTypeKind(TKey) in [tkDynArray, tkString, tkLString, tkWString, tkUString]) then
    begin
      // dynamic size
      if (GetTypeKind(TKey) = tkString) then
      begin
        Left := Pointer(@Key);
        Count := Left^;
        Inc(Count);
      end else
      // if (GetTypeKind(TKey) in [tkDynArray, tkLString, tkWString, tkUString]) then
      begin
        Left := PPointer(@Key)^;
        HashCode := 0;
        if (Left = nil) then goto hash_calculated;

        case GetTypeKind(TKey) of
          tkLString:
          begin
            Dec(Left, SizeOf(Integer));
            Count := PInteger(Left)^;
            Inc(Left, SizeOf(Integer));
          end;
          tkWString:
          begin
            Dec(Left, SizeOf(Integer));
            Count := PInteger(Left)^;
            Inc(Left, SizeOf(Integer));
            {$ifdef MSWINDOWS}if (Count = 0) then goto hash_calculated;{$endif}
            Count := Count {$ifNdef MSWINDOWS}* 2{$endif} + 2;
            Count := Count and -4;
          end;
          tkUString:
          begin
            Dec(Left, SizeOf(Integer));
            Count := PInteger(Left)^;
            Inc(Left, SizeOf(Integer));
            Count := Count * 2 + 2;
            Count := Count and -4;
          end;
        else
        // tkDynArray
          Dec(Left, SizeOf(NativeUInt));
          Count := PNativeInt(Left)^ * TRAIIHelper<TKey>.Options.ItemSize;
          Inc(Left, SizeOf(NativeUInt));
        end;
      end;
    end else
    begin
      // non-dynamic (constant) size binary > 16
      Left := Pointer(@Key);
      Count := SizeOf(TKey);
    end;

    if (not (GetTypeKind(TKey) in [tkWString, tkUString])) then
    begin
      if (Count < SizeOf(Integer)) then
      begin
        if (Count <> 0) then
        begin
          HashCode := Integer(Left[0]);
          HashCode := HashCode + (HashCode shr 4) * 63689;
          if (Count > 1) then
          begin
            HashCode := HashCode + Integer(Left[1]) * -1660269137;
            if (Count > 2) then
            begin
              HashCode := HashCode + Integer(Left[2]) * -1092754919;
            end;
          end;
        end else
        begin
          HashCode := 0;
        end;
        goto hash_calculated;
      end;
    end;

    HashCode := Integer(Count);
    Dec(Count, SizeOf(Integer));
    Inc(Left, Count);
    Inc(HashCode, PInteger(Left)^ * 63689);
    Dec(Left, Count);
    case (Count + (SizeOf(Integer) - 1)) shr 2 of
     10: goto hash10;
      9: goto hash9;
      8: goto hash8;
      7: goto hash7;
      6: goto hash6;
      5: goto hash5;
      4: goto hash4;
      3: goto hash3;
      2: goto hash2;
      1: goto hash1;
      0: goto hash0;
    else
      Inc(Count, SizeOf(Integer) - 1);
      M := -1660269137;
      repeat
        HashCode := HashCode * M + PInteger(Left)^;
        Dec(Count, SizeOf(Integer));
        Inc(Left, SizeOf(Integer));
        M := M * 378551;
        if (NativeInt(Count) <= 43) then Break;
      until (False);

      hash10:
        HashCode := HashCode * 631547855 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash9:
        HashCode := HashCode * -1987506439 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash8:
        HashCode := HashCode * -1653913089 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash7:
        HashCode := HashCode * -186114231 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash6:
        HashCode := HashCode * 915264303 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash5:
        HashCode := HashCode * -794603367 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash4:
        HashCode := HashCode * 135394143 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash3:
        HashCode := HashCode * 2012804575 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash2:
        HashCode := HashCode * -1092754919 + PInteger(Left)^;
        Inc(Left, SizeOf(Integer));
      hash1:
        HashCode := HashCode * -1660269137 + PInteger(Left)^;
      hash0:
    end;

    Inc(HashCode, ((HashCode shr 8) * 63689) + ((HashCode shr 16) * -1660269137) +
      ((HashCode shr 24) * -1092754919));
  end;
hash_calculated:

  // parent
  Pointer(Result{Parent}) := @FHashTable[NativeInt(Cardinal(HashCode)) and FHashTableMask];
  Dec(NativeUInt(Result{Parent}), SizeOf(TKey) + SizeOf(TValue));

  // find
  Stored.HashCode := HashCode;
  repeat
  next_item:
    // hash code item
    repeat
      Parent := Pointer(@Result.FNext);
      Result := Result.FNext;
      if (not Assigned(Result)) then goto not_found;
    until (Stored.HashCode = Result.HashCode);
    NativeUInt(Stored.Parent) := NativeUInt(Parent);

    // default keys comparison
    if (GetTypeKind(TKey) = tkVariant) then
    begin
      if (not InterfaceDefaults.Equals_Var(nil, PVarData(@Key), PVarData(Result))) then goto next_item;
    end else
    if (GetTypeKind(TKey) = tkClass) then
    begin
      Left := PPointer(@Key)^;
      Right := PPointer(Result)^;
      if (Assigned(Left)) then
      begin
        if (PPointer(Pointer(Left)^)[vmtEquals div SizeOf(Pointer)] = @TObject.Equals) then
        begin
          if (Left <> Right) then goto next_item;
        end else
        begin
          if (not TObject(PNativeUInt(@Key)^).Equals(TObject(PNativeUInt(Result)^))) then goto next_item;
        end;
      end else
      begin
        if (Right <> nil) then goto next_item;
      end;
    end else
    if (GetTypeKind(TKey) = tkFloat) then
    begin
      case SizeOf(TKey) of
        4:
        begin
          if (PSingle(@Key)^ <> PSingle(Result)^) then goto next_item;
        end;
        10:
        begin
          if (PExtended(@Key)^ <> PExtended(Result)^) then goto next_item;
        end;
      else
      {$ifdef LARGEINT}
        if (PInt64(@Key)^ <> PInt64(Result)^) then
      {$else .SMALLINT}
        if ((PPoint(@Key).X - PPoint(Result).X) or (PPoint(@Key).Y - PPoint(Result).Y) <> 0) then
      {$endif}
        begin
          if (TRAIIHelper<TKey>.Options.ItemSize < 0) then goto next_item;
          if (PDouble(@Key)^ <> PDouble(Result)^) then goto next_item;
        end;
      end;
    end else
    if (not (GetTypeKind(TKey) in [tkDynArray, tkString, tkLString, tkWString, tkUString])) and
      (SizeOf(TKey) <= 16) then
    begin
      // small binary
      if (SizeOf(TKey) <> 0) then
      with PData16(@Key)^ do
      begin
        if (SizeOf(TKey) >= SizeOf(Integer)) then
        begin
          if (SizeOf(TKey) >= SizeOf(Int64)) then
          begin
            {$ifdef LARGEINT}
            if (Int64s[0] <> PData16(Result).Int64s[0]) then goto next_item;
            {$else}
            if (Integers[0] <> PData16(Result).Integers[0]) then goto next_item;
            if (Integers[1] <> PData16(Result).Integers[1]) then goto next_item;
            {$endif}

            if (SizeOf(TKey) = 16) then
            begin
              {$ifdef LARGEINT}
              if (Int64s[1] <> PData16(Result).Int64s[1]) then goto next_item;
              {$else}
              if (Integers[2] <> PData16(Result).Integers[2]) then goto next_item;
              if (Integers[3] <> PData16(Result).Integers[3]) then goto next_item;
              {$endif}
            end else
            if (SizeOf(TKey) >= 12) then
            begin
              if (Integers[2] <> PData16(Result).Integers[2]) then goto next_item;
            end;
          end else
          begin
            if (Integers[0] <> PData16(Result).Integers[0]) then goto next_item;
          end;
        end;

        if (SizeOf(TKey) and 2 <> 0) then
        begin
          if (Words[(SizeOf(TKey) and -4) shr 1] <> PData16(Result).Words[(SizeOf(TKey) and -4) shr 1]) then goto next_item;
        end;
        if (SizeOf(TKey) and 1 <> 0) then
        begin
          if (Bytes[SizeOf(TKey) and -2] <> PData16(Result).Bytes[SizeOf(TKey) and -2]) then goto next_item;
        end;
      end;
    end else
    begin
      if (GetTypeKind(TKey) in [tkDynArray, tkString, tkLString, tkWString, tkUString]) then
      begin
        // dynamic size
        if (GetTypeKind(TKey) = tkString) then
        begin
          Left := Pointer(@Key);
          Right := Pointer(Result);
          if (PItem(Left) = {Right}Result) then goto cmp0;
          Count := Left^;
          if (Count <> Right^) then goto next_item;
          if (Count = 0) then goto cmp0;
          // compare last bytes
          if (Left[Count] <> Right[Count]) then goto next_item;
        end else
        // if (GetTypeKind(TKey) in [tkDynArray, tkLString, tkWString, tkUString]) then
        begin
          Left := PPointer(@Key)^;
          Right := PPointer(Result)^;
          if (Left = Right) then goto cmp0;
          if (Left = nil) then
          begin
            {$ifdef MSWINDOWS}
            if (GetTypeKind(TKey) = tkWString) then
            begin
              Dec(Right, SizeOf(Cardinal));
              if (PCardinal(Right)^ = 0) then goto cmp0;
            end;
            {$endif}
            goto next_item;
          end;
          if (Right = nil) then
          begin
            {$ifdef MSWINDOWS}
            if (GetTypeKind(TKey) = tkWString) then
            begin
              Dec(Left, SizeOf(Cardinal));
              if (PCardinal(Left)^ = 0) then goto cmp0;
            end;
            {$endif}
            goto next_item;
          end;

          if (GetTypeKind(TKey) = tkDynArray) then
          begin
            Dec(Left, SizeOf(NativeUInt));
            Dec(Right, SizeOf(NativeUInt));
            Count := PNativeUInt(Left)^;
            if (Count <> PNativeUInt(Right)^) then goto next_item;
            NativeInt(Count) := NativeInt(Count) * TRAIIHelper<TKey>.Options.ItemSize;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          end else
          // if (GetTypeKind(TKey) in [tkLString, tkWString, tkUString]) then
          begin
            Dec(Left, SizeOf(Cardinal));
            Dec(Right, SizeOf(Cardinal));
            Count := PCardinal(Left)^;
            if (Cardinal(Count) <> PCardinal(Right)^) then goto next_item;
            Inc(Left, SizeOf(Cardinal));
            Inc(Right, SizeOf(Cardinal));
          end;
        end;

        // compare last (after cardinal) words
        if (GetTypeKind(TKey) in [tkDynArray, tkString, tkLString]) then
        begin
          if (GetTypeKind(TKey) in [tkString, tkLString]) {ByteStrings + 2} then
          begin
            Inc(Count);
          end;
          if (Count and 2 <> 0) then
          begin
            Offset := Count and -4;
            Inc(Left, Offset);
            Inc(Right, Offset);
            if (PWord(Left)^ <> PWord(Right)^) then goto next_item;
            Offset := Count;
            Offset := Offset and -4;
            Dec(Left, Offset);
            Dec(Right, Offset);
          end;
        end else
        // modify Count to have only cardinals to compare
        // if (GetTypeKind(TKey) in [tkWString, tkUString]) {UnicodeStrings + 2} then
        begin
          {$ifdef MSWINDOWS}
          if (GetTypeKind(TKey) = tkWString) then
          begin
            if (Count = 0) then goto cmp0;
          end else
          {$endif}
          begin
            Inc(Count, Count);
          end;
          Inc(Count, 2);
        end;

        {$ifdef LARGEINT}
        if (Count and 4 <> 0) then
        begin
          Offset := Count and -8;
          Inc(Left, Offset);
          Inc(Right, Offset);
          if (PCardinal(Left)^ <> PCardinal(Right)^) then goto next_item;
          Dec(Left, Offset);
          Dec(Right, Offset);
        end;
        {$endif}
      end else
      begin
        // non-dynamic (constant) size binary > 16
        if (SizeOf(TKey) and {$ifdef LARGEINT}7{$else}3{$endif} <> 0) then
        with PData16(@Key)^ do
        begin
          {$ifdef LARGEINT}
          if (SizeOf(TKey) and 4 <> 0) then
          begin
            if (Integers[(SizeOf(TKey) and -8) shr 2] <> PData16(Result).Integers[(SizeOf(TKey) and -8) shr 2]) then goto next_item;
          end;
          {$endif}
          if (SizeOf(TKey) and 2 <> 0) then
          begin
            if (Words[(SizeOf(TKey) and -4) shr 1] <> PData16(Result).Words[(SizeOf(TKey) and -4) shr 1]) then goto next_item;
          end;
          if (SizeOf(TKey) and 1 <> 0) then
          begin
            if (Bytes[SizeOf(TKey) and -2] <> PData16(Result).Bytes[SizeOf(TKey) and -2]) then goto next_item;
          end;
        end;
        Left := Pointer(@Key);
        Right := Pointer(Result);
        Count := SizeOf(TKey);
      end;

      // natives (40 bytes static) compare
      Count := Count shr {$ifdef LARGEINT}3{$else}2{$endif};
      case Count of
      {$ifdef SMALLINT}
       10: goto cmp10;
        9: goto cmp9;
        8: goto cmp8;
        7: goto cmp7;
        6: goto cmp6;
      {$endif}
        5: goto cmp5;
        4: goto cmp4;
        3: goto cmp3;
        2: goto cmp2;
        1: goto cmp1;
        0: goto cmp0;
      else
        repeat
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Dec(Count);
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        until (Count = {$ifdef LARGEINT}5{$else}10{$endif});

        {$ifdef SMALLINT}
        cmp10:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp9:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp8:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp7:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp6:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        {$endif}
        cmp5:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp4:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp3:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp2:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
          Inc(Left, SizeOf(NativeUInt));
          Inc(Right, SizeOf(NativeUInt));
        cmp1:
          if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then goto next_item;
        cmp0:
      end;
    end;

    // found
    Mode := FindMode;
    if (Mode and FOUND_MASK = 0) then Exit;
    Cardinal(Mode) := Cardinal(Mode) and FOUND_MASK;
    if (Mode <> FOUND_EXCEPTION) then
    begin
      if (Mode = FOUND_DELETE) then
      begin
        Pointer(Stored.Parent^) := Result.FNext;

        _Self1 := Stored.Self;
        with _Self1 do
        if (not Assigned(FInternalItemNotify)) then
        begin
          DisposeItem(Result);
        end else
        begin
          FInternalItemNotify(Result^, cnRemoved);
          DisposeItem(Result);
        end;
      end else
      // if (Mode = FOUND_REPLACE) then
      begin
        _Self1 := Stored.Self;
        with _Self1 do
        if (not Assigned(FInternalValueNotify)) then
        begin
          Result.FValue := FInternalFindValue^;
        end else
        begin
          FInternalValueNotify(_Self1, Result.Value, cnRemoved);
          Result.FValue := FInternalFindValue^;
          FInternalValueNotify(_Self1, Result.Value, cnAdded);
        end;
      end;
    end else
    begin
      raise EListError.CreateRes(Pointer(@SGenericDuplicateItem));
    end;
    Exit;

    // not found (Result = nil)
  not_found:
    Mode := FindMode;
    if (Mode and EMPTY_MASK = 0) then Exit;
    if (Mode and EMPTY_EXCEPTION = 0) then
    begin
      // EMPTY_NEW
      _Self2 := Stored.Self;
      with _Self2 do
      begin
        Result := NewItem;
        Result.FKey := Key;
        Result.FHashCode := Stored.HashCode;
        Parent := Pointer(@FHashTable[NativeInt(Cardinal(Stored.HashCode)) and FHashTableMask]);
        Result.FNext := Parent^;
        Parent^ := Result;
        Result.FValue := FInternalFindValue^;
        if (Assigned(FInternalItemNotify)) then
        begin
          FInternalItemNotify(Result^, cnAdded);
        end;
      end;
    end else
    begin
      raise EListError.CreateRes(Pointer(@SGenericItemNotFound));
    end;
    Exit;
  until (False);
end;
{$else XE7-}
function TRapidDictionary<TKey,TValue>.InternalFindItem(const Key: TKey; const FindMode: Integer): Pointer{PItem};
var
  Parent: Pointer;
  Item: TCustomDictionary<TKey,TValue>.PItem;
  HashCode, Mode: Integer;
  Stored: TInternalFindStored;
begin
  // hash code
  HashCode := Self.FComparerGetHashCode(Key);

  // parent
  Pointer(Item{Parent}) := @FHashTable[NativeInt(Cardinal(HashCode)) and FHashTableMask];
  Dec(NativeUInt(Item{Parent}), SizeOf(TKey) + SizeOf(TValue));

  // find
  Stored.HashCode := HashCode;
  repeat
    // hash code item
    repeat
      Parent := Pointer(@Item.FNext);
      Item := Item.FNext;
    until (Item = nil) or (Stored.HashCode = Item.HashCode);

    if (Item <> nil) then
    begin
      // hash code item found
      Stored.Parent := Parent;

      // keys comparison
      if (not Self.FComparerEquals(Key, Item.Key)) then Continue;

      // found
      Mode := FindMode;
      if (Mode and FOUND_MASK = 0) then Break;
      Cardinal(Mode) := Cardinal(Mode) and FOUND_MASK;
      if (Mode <> FOUND_EXCEPTION) then
      begin
        if (Mode = FOUND_DELETE) then
        begin
          Pointer(Stored.Parent^) := Item.FNext;
          if (not Assigned(Self.FInternalItemNotify)) then
          begin
            Self.DisposeItem(Item);
          end else
          begin
            Self.FInternalItemNotify(Item^, cnRemoved);
            Self.DisposeItem(Item);
          end;
        end else
        // if (Mode = FOUND_REPLACE) then
        begin
          if (not Assigned(Self.FInternalValueNotify)) then
          begin
            Item.FValue := FInternalFindValue^;
          end else
          begin
            Self.FInternalValueNotify(Self, Item.Value, cnRemoved);
            Item.FValue := FInternalFindValue^;
            Self.FInternalValueNotify(Self, Item.Value, cnAdded);
          end;
        end;
      end else
      begin
        raise EListError.CreateRes(Pointer(@SGenericDuplicateItem));
      end;
      Break;
    end;

    // not found (Item = nil)
    Mode := FindMode;
    if (Mode and EMPTY_MASK = 0) then Break;
    if (Mode and EMPTY_EXCEPTION = 0) then
    begin
      // EMPTY_NEW
      Item := Self.NewItem;
      Item.FKey := Key;
      Item.FHashCode := Stored.HashCode;
      Parent := Pointer(@Self.FHashTable[NativeInt(Cardinal(Stored.HashCode)) and Self.FHashTableMask]);
      Item.FNext := Pointer(Parent^);
      Pointer(Parent^) := Item;
      Item.FValue := FInternalFindValue^;
      if (Assigned(Self.FInternalItemNotify)) then
      begin
        Self.FInternalItemNotify(Item^, cnAdded);
      end;
      Break;
    end else
    begin
      raise EListError.CreateRes(Pointer(@SGenericItemNotFound));
    end;
  until (False);

  Result := Item;
end;
{$ifend}

function TRapidDictionary<TKey,TValue>.GetItem(const Key: TKey): TValue;
begin
  Result := TCustomDictionary<TKey,TValue>.PItem(Self.InternalFindItem(Key, FOUND_NONE + EMPTY_EXCEPTION)).Value;
end;

procedure TRapidDictionary<TKey,TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_REPLACE + EMPTY_EXCEPTION);
end;

function TRapidDictionary<TKey,TValue>.Find(const Key: TKey): Pointer{PItem};
begin
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
end;

function TRapidDictionary<TKey,TValue>.FindOrAdd(const Key: TKey): Pointer{PItem};
begin
  Self.FInternalFindValue := @Self.FDefaultValue;
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NEW);
end;

procedure TRapidDictionary<TKey,TValue>.Add(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_EXCEPTION + EMPTY_NEW);
end;

procedure TRapidDictionary<TKey,TValue>.Remove(const Key: TKey);
begin
  Self.InternalFindItem(Key, FOUND_DELETE + EMPTY_NONE)
end;

function TRapidDictionary<TKey,TValue>.ExtractPair(const Key: TKey): TPair<TKey,TValue>;
var
  Parent: Pointer;
  Item, Current: TCustomDictionary<TKey,TValue>.PItem;
begin
  Result.Key := Key;
  Item := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
  if (Item = nil) then
  begin
    Result.Value := Default(TValue);
    Exit;
  end;

  Result.Value := Item.Value;
  Parent := Pointer(@Self.FHashTable[NativeInt(Cardinal(Item.HashCode)) and Self.FHashTableMask]);
  repeat
    Current := TCustomDictionary<TKey,TValue>.PItem(Parent^);

    if (Item = Current) then
    begin
      TCustomDictionary<TKey,TValue>.PItem(Parent^) := Item.FNext;

      if (not Assigned(Self.FInternalItemNotify)) then
      begin
        Self.DisposeItem(Item);
      end else
      begin
        Self.FInternalItemNotify(Item^, cnExtracted);
        Self.DisposeItem(Item);
      end;

      Exit;
    end;

    {$if (CompilerVersion >= 22) and (CompilerVersion <= 24)}
      Parent := Pointer(NativeUInt(Current) + (SizeOf(TKey) + SizeOf(TValue)));
    {$else}
      Parent := Pointer(@Current.FNext);
    {$ifend}
  until (False);
end;

function TRapidDictionary<TKey,TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
var
  Item: PItem;
begin
  Item := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
  if Assigned(Item) then
  begin
    Value := Item.Value;
    Result := True;
  end else
  begin
    Value := Default(TValue);
    Result := False;
  end;
end;

procedure TRapidDictionary<TKey,TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_REPLACE + EMPTY_NEW);
end;

function TRapidDictionary<TKey,TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Result := (Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE) <> nil);
end;


{ TCustomList<T>.TEnumerator }

function TCustomList<T>.TEnumerator.MoveNext: Boolean;
var
  N, Cap: NativeInt;
begin
  N := Data.Tag + 1;
  with TCustomList<T>(Data.Owner) do
  begin
    if (N < FCount.Native) then
    begin
      Data.Tag := N;

      Inc(N, FTail);
      Cap := FCapacity.Native;
      if (N > Cap) then Dec(N, Cap);
      Data.Current := FItems[N];

      Exit(True);
    end;
  end;
  Result := False;
end;

{ TCustomList<T> }

constructor TCustomList<T>.Create;
begin
  inherited Create;
  TRAIIHelper<T>.Create;
  SetNotifyMethods;
end;

destructor TCustomList<T>.Destroy;
begin
  Clear;
  ClearMethod(FInternalNotify);
  inherited;
end;

class procedure TCustomList<T>.ClearMethod(var Method);
begin
  {$ifdef WEAKINSTREF}
  TMethod(Method).Data := nil;
  {$endif}
end;

class function TCustomList<T>.EmptyException: Exception;
begin
  Result := EListError.CreateRes(Pointer(@SUnbalancedOperation));
end;

class function TCustomList<T>.OutOfRangeException: Exception;
begin
  Result := EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
end;

procedure TCustomList<T>.SetCapacity(Value: Integer);
var
  Dif, NewTail: NativeInt;
  {$ifdef WEAKREF}
  WeakItems: PItemList;
  {$endif}
begin
  if (Value = FCapacity.Int) then Exit;
  if Value < Count then
    raise OutOfRangeException;

  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<T>.Weak) then
  {$else}
  if (TRAIIHelper<T>.FOptions.FWeak) then
  {$endif}
  begin
    GetMem(WeakItems, Value * SizeOf(T));

    if (FCount.Native <> 0) then
    begin
      FillChar(Pointer(WeakItems)^, FCount.Native * SizeOf(T), #0);

      if (FTail <= FHead) then
      begin
        System.CopyArray(@WeakItems[0], @FItems[FTail], TypeInfo(T), FCount.Native);
        System.FinalizeArray(@FItems[FTail], TypeInfo(T), FCount.Native);
      end else
      begin
        System.CopyArray(@WeakItems[0], @FItems[FTail], TypeInfo(T), FCapacity.Native - FTail);
        System.FinalizeArray(@FItems[FTail], TypeInfo(T), FCapacity.Native - FTail);
        System.CopyArray(@WeakItems[FCapacity.Native - FTail], @FItems[0], TypeInfo(T), FHead);
        System.FinalizeArray(@FItems[0], TypeInfo(T), FHead);
      end;
    end;

    FreeMem(FItems);
    FItems := WeakItems;
    FTail := 0;
    FHead := FCount.Native;
    FCapacity.Native := Value;
  end else
  {$endif}
  if (FCount.Native = 0) then
  begin
    FTail := 0;
    FHead := 0;
    FCapacity.Native := Value;
    ReallocMem(FItems, Value * SizeOf(T));
  end else
  if (FTail <= FHead) then
  begin
    if (FTail <> 0) then
    begin
      System.Move(FItems[FTail], FItems[0], FCount.Native * SizeOf(T));
      Dec(FHead, FTail);
      FTail := 0;
    end;
    FCapacity.Native := Value;
    ReallocMem(FItems, Value * SizeOf(T));
  end else
  begin
    Dif := NativeInt(Value) - FCapacity.Native;
    NewTail := FTail + Dif;

    if (Dif > 0) then
    begin
      ReallocMem(FItems, Value * SizeOf(T));
      System.Move(FItems[FTail], FItems[NewTail], (FCapacity.Native - FTail) * SizeOf(T));
    end else
    //if (Dif < 0) then
    begin
      System.Move(FItems[FTail], FItems[NewTail], (FCapacity.Native - FTail) * SizeOf(T));
      ReallocMem(FItems, Value * SizeOf(T));
    end;

    FCapacity.Native := Value;
    FTail := NewTail;
  end;
end;

procedure TCustomList<T>.Grow;
var
  OldCapacity, NewCapacity: Integer;
begin
  OldCapacity := FCapacity.Int;
  NewCapacity := OldCapacity * 2;
  if (NewCapacity < 0) then
    OutOfMemoryError;
  if (NewCapacity < 4) then
    NewCapacity := 4;

  SetCapacity(NewCapacity);
end;

procedure TCustomList<T>.GrowTo(Value: Integer);
var
  OldCapacity, NewCapacity: Integer;
begin
  OldCapacity := FCapacity.Int;
  NewCapacity := OldCapacity * 2;
  if (NewCapacity < 0) then
    OutOfMemoryError;
  if (NewCapacity < 4) then
    NewCapacity := 4;

  while (NewCapacity < Value) do
  begin
    NewCapacity := NewCapacity * 2;
    if (NewCapacity < 0) then
      OutOfMemoryError;
  end;

  SetCapacity(NewCapacity)
end;

procedure TCustomList<T>.Clear;
var
  i: NativeInt;
  Item: PItem;
begin
  if (FItems = nil) then Exit;

  if (Assigned(FInternalNotify)) then
  begin
    Item := @FItems[FTail];

    if (FTail <= FHead) then
    begin
      if (TMethod(FInternalNotify).Code = @TCustomList<T>.NotifyCaller) then
      begin
        for i := 1 to FCount.Native do
        begin
          Self.Notify(Item^, cnRemoved);
          Inc(Item);
        end;
      end else
      begin
        for i := 1 to FCount.Native do
        begin
          FInternalNotify(Self, Item^, cnRemoved);
          Inc(Item);
        end;
      end;
    end else
    if (TMethod(FInternalNotify).Code = @TCustomList<T>.NotifyCaller) then
    begin
      for i := FTail to FCapacity.Native - 1 do
      begin
        Self.Notify(Item^, cnRemoved);
        Inc(Item);
      end;
      Item := @FItems[0];
      for i := 0 to FHead - 1 do
      begin
        Self.Notify(Item^, cnRemoved);
        Inc(Item);
      end;
    end else
    begin
      for i := FTail to FCapacity.Native - 1 do
      begin
        FInternalNotify(Self, Item^, cnRemoved);
        Inc(Item);
      end;
      Item := @FItems[0];
      for i := 0 to FHead - 1 do
      begin
        FInternalNotify(Self, Item^, cnRemoved);
        Inc(Item);
      end;
    end;
  end;

  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T)) then
  {$else}
  if Assigned(TRAIIHelper<T>.FOptions.ClearProc) then
  {$endif}
  begin
    if (FTail <= FHead) then
    begin
      TRAIIHelper<T>.ClearArray(@FItems[FTail], FCount.Native);
    end else
    begin
      TRAIIHelper<T>.ClearArray(@FItems[FTail], FCapacity.Native - FTail - 1);
      TRAIIHelper<T>.ClearArray(@FItems[0], FHead);
    end;
  end;

  FCount.Native := 0;
  FCapacity.Native := 0;
  FTail := 0;
  FHead := 0;
  ReallocMem(FItems, 0);
end;

procedure TCustomList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

procedure TCustomList<T>.SetOnNotify(const Value: TCollectionNotifyEvent<T>);
begin
  if (TMethod(FOnNotify).Code <> TMethod(Value).Code) or
    (TMethod(FOnNotify).Data <> TMethod(Value).Data) then
  begin
    FOnNotify := Value;
    SetNotifyMethods;
  end;
end;

procedure TCustomList<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

procedure TCustomList<T>.NotifyCaller(Sender: TObject; const Item: T; Action: TCollectionNotification);
begin
  Self.Notify(Item, Action);
end;

procedure TCustomList<T>.SetNotifyMethods;
var
  VMTNotify: procedure(const Item: T; Action: TCollectionNotification) of object;
begin
  VMTNotify := Self.Notify;
  if (TMethod(VMTNotify).Code <> @TCustomList<T>.Notify) then
  begin
    TMethod(FInternalNotify).Data := Pointer(Self);
    TMethod(FInternalNotify).Code := @TCustomList<T>.NotifyCaller;
  end else
  begin
    TMethod(FInternalNotify) := TMethod(Self.FOnNotify);
  end;
end;

function TCustomList<T>.DoGetCount: Integer;
begin
  Result := FCount.Int;
end;

function TCustomList<T>.GetIsEmpty: Boolean;
begin
  Result := (FCount.Int = 0);
end;

function TCustomList<T>.DoGetIsEmpty: Boolean;
begin
  Result := (FCount.Int = 0);
end;

function TCustomList<T>.DoGetEnumerator: TCollectionEnumerator<T>;
begin
  Result.Data.Init(Self);
  Pointer(@Result.DoMoveNext) := @TEnumerator.MoveNext;
end;

function TCustomList<T>.GetEnumerator: TEnumerator;
begin
  Result.Data.Init(Self);
end;

function TCustomList<T>.ToArray: TArray<T>;
var
  Count, TailCount: NativeInt;
begin
  Count := FCount.Native;
  if (Count <> 0) then
  begin
    if (Pointer(Result) <> nil) then Result := nil;
    SetLength(Result, Count);

    if (FTail <= FHead) then
    begin
      {$ifdef SMARTGENERICS}
      if (System.IsManagedType(T)) then
      {$else}
      if Assigned(TRAIIHelper<T>.FOptions.ClearProc) then
      {$endif}
      begin
        System.CopyArray(Pointer(Result), @FItems[FTail], TypeInfo(T), Count);
      end else
      begin
        System.Move(FItems[FTail], Pointer(Result)^, Count * SizeOf(T));
      end;
    end else
    begin
      TailCount := FCapacity.Native - FTail - 1;

      {$ifdef SMARTGENERICS}
      if (System.IsManagedType(T)) then
      {$else}
      if Assigned(TRAIIHelper<T>.FOptions.ClearProc) then
      {$endif}
      begin
        System.CopyArray(Pointer(Result), @FItems[FTail], TypeInfo(T), TailCount);
        System.CopyArray(@Result[TailCount], @FItems[0], TypeInfo(T), FHead);
      end else
      begin
        System.Move(FItems[FTail], Pointer(Result)^, TailCount * SizeOf(T));
        System.Move(FItems[0], Result[TailCount], FHead * SizeOf(T));
      end;
    end;
  end else
  begin
    if (Pointer(Result) <> nil) then Result := nil;
  end;
end;


{ TList<T> }

constructor TList<T>.Create;
begin
  inherited Create;
end;

constructor TList<T>.Create(const AComparer: IComparer<T>);
begin
  Create;
  if (Pointer(AComparer) <> @InterfaceDefaults.TDefaultComparer<T>.Instance) then
    FComparer := AComparer;
end;

constructor TList<T>.Create(const Collection: TEnumerable<T>);
begin
  Create;
  InsertRange(0, Collection);
end;

class procedure TList<T>.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) {$if CompilerVersion >= 23}at ReturnAddress{$ifend};
end;

{$ifNdef NEXTGEN}
class procedure TList<T>.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) {$if CompilerVersion >= 23}at ReturnAddress{$ifend};
end;
{$endif}

procedure TList<T>.SetCount(Value: Integer);
var
  Count: Integer;
begin
  Count := FCount.Int;
  if (Value < 0) then
  begin
    raise OutOfRangeException;
  end else
  if (Value < Count) then
  begin
    DeleteRange(Value, Count - Value);
  end else
  if (Value > Count) then
  begin
    if (Value > FCapacity.Int) then
      GrowTo(Value);

    {$ifdef SMARTGENERICS}
    if (System.IsManagedType(T)) then
    {$else}
    if (Assigned(TRAIIHelper<T>.Options.InitProc)) then
    {$endif}
    begin
      TRAIIHelper<T>.InitArray(@FItems[FCount.Native], Value - FCount.Int);
    end;

    FCount.Int := Value;
  end;
end;

function TList<T>.GetItem(Index: Integer): T;
begin
  if (Cardinal(Index) < Cardinal(FCount.Int)) then
  begin
    Result := FItems[Index];
  end else
  begin
    raise OutOfRangeException;
  end;
end;

procedure TList<T>.ReplaceItemNotify(Index: Integer; const Value: T);
var
  Item: ^T;
begin
  Item := @FItems[Index];

  if (TMethod(FInternalNotify).Code = @TCustomList<T>.NotifyCaller) then
  begin
    Self.Notify(Item^, cnRemoved);
    Item^ := Value;
    Self.Notify(Value, cnAdded);
  end else
  begin
    FInternalNotify(Self, Item^, cnRemoved);
    Item^ := Value;
    FInternalNotify(Self, Value, cnAdded);
  end;
end;

procedure TList<T>.SetItem(Index: Integer; const Value: T);
begin
  if (Cardinal(Index) < Cardinal(FCount.Int)) then
  begin
    if (not Assigned(FInternalNotify)) then
    begin
      FItems[Index] := Value;
    end else
    begin
      ReplaceItemNotify(Index, Value);
    end;
  end else
  begin
    raise OutOfRangeException;
  end;
end;

function TList<T>.First: T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[0];
  end else
  begin
    raise OutOfRangeException;
  end;
end;

function TList<T>.Last: T;
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count <> 0) then
  begin
    Dec(Count);
    Result := FItems[Count];
  end else
  begin
    raise OutOfRangeException;
  end;
end;

function TList<T>.ItemValue(const Item: T): NativeInt;
begin
  case SizeOf(T) of
    1: Result := PByte(@Item)^;
    2: Result := PWord(@Item)^;
    3: Result := PWord(@Item)^ + PByte(@Item)[2] shl 16;
    {$ifdef LARGEINT}
    4: Result := PCardinal(@Item)^;
    5: Result := NativeInt(PCardinal(@Item)^) +
       NativeInt(PByte(@Item)[4]) shl 32;
    6: Result := NativeInt(PCardinal(@Item)^) +
       NativeInt(PWord(PByte(@Item) + SizeOf(Cardinal))^) shl 32;
    7: Result := NativeInt(PCardinal(@Item)^) +
       NativeInt(PWord(PByte(@Item) + SizeOf(Cardinal))^) shl 32 + NativeInt(PByte(@Item)[6]) shl 48;
    {$endif}
  else
    Result := PNativeInt(@Item)^;
  end;
end;

{$ifdef WEAKREF}
class procedure TList<T>.InternalWeakInsert(const Item: Pointer; const ItemsCount, InsertCount: NativeUInt);
var
  Source, Destination: ^T;
begin
  // init appended items
  Source := Item;
  Inc(Source, ItemsCount);
  TRAIIHelper<T>.InitArray(Source, InsertCount);

  // move items
  Destination := Source + InsertCount;
  repeat
    Dec(Source);
    Dec(Destination);
    Destination^ := Source^;
  until (Source = Item);

  // clear + init (Finalization)
  TRAIIHelper<T>.ClearArray(Item, InsertCount);
  TRAIIHelper<T>.InitArray(Item, InsertCount);
end;
{$endif}

function TList<T>.InternalInsert(Index: NativeInt; const Value: T): Integer;
var
  Count, Null: NativeInt;
  Item: ^TRAIIHelper.TData16;
begin
  Result := Index;
  Count := FCount.Native;
  if (NativeUInt(Index) <= NativeUInt(Count)) then
  begin
    repeat
      if (Count <> FCapacity.Native) then
      begin
        Inc(Count);
        FCount.Native := Count;
        Dec(Count);
        if (Index <> Count) then
        begin
          {$ifdef WEAKREF}
          {$ifdef SMARTGENERICS}
          if (TRAIIHelper<T>.Weak) then
          {$else}
          if (TRAIIHelper<T>.FOptions.FWeak) then
          {$endif}
          begin
            InternalWeakInsert(Item, Count - Index, 1);
          end else
          {$endif}
          begin
            Count := (Count - Index) * SizeOf(T);
            Item := Pointer(@FItems[Index]);
            System.Move(Item^, PByte(PByte(Item) + SizeOf(T))^, Count);
          end;

          Index := Result;
        end;
        Item := Pointer(@FItems[Index]);

        {$ifdef SMARTGENERICS}
        if (System.IsManagedType(T)) then
        {$else}
        if (SizeOf(T) >= SizeOf(NativeInt)) then
        {$endif}
        begin
          {$ifdef SMARTGENERICS}
          if (GetTypeKind(T) = tkVariant) then
          begin
            Item.Integers[0] := 0;
          end else
          {$endif}
          if (SizeOf(T) <= 16) then
          begin
            Null := 0;
            {$ifdef SMALLINT}
              if (SizeOf(T) >= SizeOf(Integer) * 1) then Item.Integers[0] := Null;
              if (SizeOf(T) >= SizeOf(Integer) * 2) then Item.Integers[1] := Null;
              if (SizeOf(T) >= SizeOf(Integer) * 3) then Item.Integers[2] := Null;
              if (SizeOf(T)  = SizeOf(Integer) * 4) then Item.Integers[3] := Null;
            {$else .LARGEINT}
              if (SizeOf(T) >= SizeOf(Int64) * 1) then Item.Int64s[0] := Null;
              if (SizeOf(T)  = SizeOf(Int64) * 2) then Item.Int64s[1] := Null;
              case SizeOf(T) of
                 4..7: Item.Integers[0] := Null;
               12..15: Item.Integers[2] := Null;
              end;
            {$endif}
            case SizeOf(T) of
               2,3: Item.Words[0] := 0;
               6,7: Item.Words[2] := 0;
             10,11: Item.Words[4] := 0;
             14,15: Item.Words[6] := 0;
            end;
            case SizeOf(T) of
               1: Item.Bytes[ 1-1] := 0;
               3: Item.Bytes[ 3-1] := 0;
               5: Item.Bytes[ 5-1] := 0;
               7: Item.Bytes[ 7-1] := 0;
               9: Item.Bytes[ 9-1] := 0;
              11: Item.Bytes[11-1] := 0;
              13: Item.Bytes[13-1] := 0;
              15: Item.Bytes[15-1] := 0;
            end;
          end else
          begin
            TRAIIHelper<T>.Init(Pointer(Item));
          end;
        end;

        PItem(Item)^ := Value;
        if Assigned(FInternalNotify) then
          FInternalNotify(Self, Value, cnAdded);
        Exit;
      end else
      begin
        Self.Grow;
        Count := FCount.Native;
        Index := Result;
      end;
    until (False);
  end else
  begin
    raise OutOfRangeException;
  end;
end;

function TList<T>.Add(const Value: T): Integer;
var
  Count, Null: NativeInt;
  Item: TRAIIHelper.PData16;
begin
  Count := FCount.Native;
  if (Count <> FCapacity.Native) and (not Assigned(FInternalNotify)) then
  begin
    Inc(Count);
    FCount.Native := Count;
    Dec(Count);
    Item := Pointer(@FItems[Count]);

    {$ifdef SMARTGENERICS}
    if (System.IsManagedType(T)) then
    {$else}
    if (SizeOf(T) >= SizeOf(NativeInt)) then
    {$endif}
    begin
      {$ifdef SMARTGENERICS}
      if (GetTypeKind(T) = tkVariant) then
      begin
        Item.Integers[0] := 0;
      end else
      {$endif}
      if (SizeOf(T) <= 16) then
      begin
        Null := 0;
        {$ifdef SMALLINT}
          if (SizeOf(T) >= SizeOf(Integer) * 1) then Item.Integers[0] := Null;
          if (SizeOf(T) >= SizeOf(Integer) * 2) then Item.Integers[1] := Null;
          if (SizeOf(T) >= SizeOf(Integer) * 3) then Item.Integers[2] := Null;
          if (SizeOf(T)  = SizeOf(Integer) * 4) then Item.Integers[3] := Null;
        {$else .LARGEINT}
          if (SizeOf(T) >= SizeOf(Int64) * 1) then Item.Int64s[0] := Null;
          if (SizeOf(T)  = SizeOf(Int64) * 2) then Item.Int64s[1] := Null;
          case SizeOf(T) of
             4..7: Item.Integers[0] := Null;
           12..15: Item.Integers[2] := Null;
          end;
        {$endif}
        case SizeOf(T) of
           2,3: Item.Words[0] := 0;
           6,7: Item.Words[2] := 0;
         10,11: Item.Words[4] := 0;
         14,15: Item.Words[6] := 0;
        end;
        case SizeOf(T) of
           1: Item.Bytes[ 1-1] := 0;
           3: Item.Bytes[ 3-1] := 0;
           5: Item.Bytes[ 5-1] := 0;
           7: Item.Bytes[ 7-1] := 0;
           9: Item.Bytes[ 9-1] := 0;
          11: Item.Bytes[11-1] := 0;
          13: Item.Bytes[13-1] := 0;
          15: Item.Bytes[15-1] := 0;
        end;
      end else
      begin
        TRAIIHelper<T>.Init(Pointer(Item));
      end;
    end;

    PItem(Item)^ := Value;
    Result := Count;
    Exit;
  end else
  begin
    Result := InternalInsert(Count, Value);
  end;
end;

procedure TList<T>.Insert(Index: Integer; const Value: T);
begin
  InternalInsert(Index, Value);
end;

procedure TList<T>.AddRange(const Values: array of T);
var
  Count, ValuesCount, i: NativeInt;
  Item, Source, Buffer: PItem;
  Stored: TInternalStored;
begin
  ValuesCount := High(Values);
  if (ValuesCount < 0) then Exit;
  Inc(ValuesCount);

  Stored.Self := Self;
  Stored.InternalNotify := TMethod(FInternalNotify);

  Count := FCount.Native;
  Inc(Count, ValuesCount);
  if (NativeUInt(Count) <= NativeUInt(High(Integer))) then
  begin
    repeat
      if (Count <= FCapacity.Native) then
      begin
        FCount.Native := Count;
        Dec(Count, ValuesCount);
        Buffer{Item} := @FItems[Count];

        {$ifdef SMARTGENERICS}
        if (System.IsManagedType(T)) then
        {$else}
        if (Assigned(TRAIIHelper<T>.Options.InitProc)) then
        {$endif}
        begin
          TRAIIHelper<T>.InitArray(Buffer{Item}, ValuesCount);
        end else
        if (not Assigned(Stored.InternalNotify.Code)) then
        begin
          System.Move(Values[0], Buffer{Item}^, ValuesCount * SizeOf(T));
          Exit;
        end;

        Source := @Values[0];
        Item := Buffer{Item};
        if (not Assigned(Stored.InternalNotify.Code)) then
        begin
          for ValuesCount := ValuesCount downto 1 do
          begin
            Item^ := Source^;
            Inc(Source);
            Inc(Item);
          end;
        end else
        begin
          for ValuesCount := ValuesCount downto 1 do
          begin
            Item^ := Source^;
            TCollectionNotifyEvent<T>(Stored.InternalNotify)(Stored.Self, Source^, cnAdded);
            Inc(Source);
            Inc(Item);
          end;
        end;
        Exit;
      end else
      begin
        Self.GrowTo(Count);
        Count := FCount.Native;
        Inc(Count, ValuesCount);
      end;
    until (False);
  end else
  begin
    OutOfMemoryError;
  end;
end;

procedure TList<T>.InsertRange(Index: Integer; const Values: array of T);
var
  Count, ValuesCount, AIndex: NativeInt;
  Item, Source, Buffer: PItem;
  Stored: TInternalStored;
begin
  ValuesCount := High(Values);
  if (ValuesCount < 0) then Exit;
  Inc(ValuesCount);

  Stored.Self := Self;
  Stored.InternalNotify := TMethod(FInternalNotify);
  Stored.Count := ValuesCount;
  AIndex := Index;

  Count := FCount.Native;
  if (NativeUInt(AIndex) <= NativeUInt(Count)) then
  begin
    Inc(Count, ValuesCount);
    if (NativeUInt(Count) <= NativeUInt(High(Integer))) then
    begin
      repeat
        if (Count <= FCapacity.Native) then
        begin
          FCount.Native := Count;
          Dec(Count, AIndex);
          Dec(Count, ValuesCount);
          Buffer{Item} := @FItems[AIndex];

          {$ifdef SMARTGENERICS}
          if (System.IsManagedType(T)) then
          {$else}
          if (Assigned(TRAIIHelper<T>.Options.InitProc)) then
          {$endif}
          begin
            {$ifdef WEAKREF}
            {$ifdef SMARTGENERICS}
            if (TRAIIHelper<T>.Weak) then
            {$else}
            if (TRAIIHelper<T>.FOptions.FWeak) then
            {$endif}
            begin
              if (Count <> 0) then
              begin
                InternalWeakInsert(Buffer{Item}, Count, Stored.Count);
              end else
              begin
                TRAIIHelper<T>.InitArray(Buffer{Item}, Stored.Count);
              end;
            end else
            {$endif}
            begin
              if (Count <> 0) then
              begin
                Count := Count * SizeOf(T);
                Source := Buffer{Item} + Stored.Count;
                System.Move(Buffer{Item}^, Source^, Count);
              end;

              TRAIIHelper<T>.InitArray(Buffer{Item}, Stored.Count);
            end;
          end else
          begin
            if (Count <> 0) then
            begin
              Count := Count * SizeOf(T);
              Source := Buffer{Item} + Stored.Count;
              System.Move(Buffer{Item}^, Source^, Count);
            end;

            if (not Assigned(Stored.InternalNotify.Code)) then
            begin
              System.Move(Values[0], Buffer{Item}^, Stored.Count * SizeOf(T));
              Exit;
            end;
          end;

          // insertion
          Source := @Values[0];
          Stored.Item := Source + Stored.Count;
          Item := Buffer{Item};
          if (not Assigned(Stored.InternalNotify.Code)) then
          begin
            repeat
              Item^ := Source^;
              Inc(Source);
              Inc(Item);
            until (Source = Stored.Item);
          end else
          begin
            repeat
              Item^ := Source^;
              TCollectionNotifyEvent<T>(Stored.InternalNotify)(Stored.Self, Source^, cnAdded);
              Inc(Source);
              Inc(Item);
            until (Source = Stored.Item);
          end;
          Exit;
        end else
        begin
          Self.GrowTo(Count);
          AIndex := Index;
          ValuesCount := Stored.Count;
          Count := FCount.Native;
          Inc(Count, ValuesCount);
        end;
      until (False);
    end else
    begin
      OutOfMemoryError;
    end;
  end else
  begin
    raise OutOfRangeException;
  end;
end;

procedure TList<T>.AddRange(const Collection: IEnumerable<T>);
var
  Item: T;
  Index: NativeInt;
begin
  if (not Assigned(FInternalNotify)) then
  begin
    for Item in Collection do
    begin
      Add(Item);
    end;
  end else
  begin
    Index := FCount.Native;
    for Item in Collection do
    begin
      InternalInsert(Index, Item);
      Inc(Index);
    end;
  end;
end;

procedure TList<T>.AddRange(const Collection: TEnumerable<T>);
var
  Item: T;
  Index: NativeInt;
begin
  if (not Assigned(FInternalNotify)) then
  begin
    for Item in Collection do
    begin
      Add(Item);
    end;
  end else
  begin
    Index := FCount.Native;
    for Item in Collection do
    begin
      InternalInsert(Index, Item);
      Inc(Index);
    end;
  end;
end;

procedure TList<T>.InsertRange(Index: Integer; const Collection: IEnumerable<T>);
var
  Item: T;
begin
  if (Index = FCount.Int) and (not Assigned(FInternalNotify)) then
  begin
    AddRange(Collection);
    Exit;
  end;

  for Item in Collection do
  begin
    Insert(Index, Item);
    Inc(Index);
  end;
end;

procedure TList<T>.InsertRange(Index: Integer; const Collection: TEnumerable<T>);
var
  Item: T;
begin
  if (Index = FCount.Int) and (not Assigned(FInternalNotify)) then
  begin
    AddRange(Collection);
    Exit;
  end;

  for Item in Collection do
  begin
    Insert(Index, Item);
    Inc(Index);
  end;
end;

procedure TList<T>.InternalDelete(Index: NativeInt; Action: TCollectionNotification);
var
  Count: NativeInt;
  Item: PItem;
  VType: Integer;
  Stored: TInternalStored;
begin
  Count := FCount.Native;
  if (NativeUInt(Index) < NativeUInt(Count)) then
  begin
    Dec(Count);
    FCount.Native := Count;
    Dec(Count, Index);
    Item := @FItems[Index];

    if (Assigned(FInternalNotify)) then
    begin
      Stored.Item := Item;
      Stored.Count := Count;
      FInternalNotify(Self, Stored.Item^, Action);
      Item := Stored.Item;
      Count := Stored.Count;
    end;

    {$ifdef SMARTGENERICS}
    case GetTypeKind(T) of
      {$ifdef AUTOREFCOUNT}
      tkClass,
      {$endif}
      tkWString, tkLString, tkUString, tkInterface, tkDynArray:
      begin
        if (PNativeInt(Item)^ <> 0) then
        case GetTypeKind(T) of
          {$ifdef AUTOREFCOUNT}
          tkClass:
          begin
            TRAIIHelper.RefObjClear(Item);
          end;
          {$endif}
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            TRAIIHelper.WStrClear(Item);
          end;
          {$else}
          tkWString,
          {$endif}
          tkLString, tkUString:
          begin
            TRAIIHelper.ULStrClear(Item);
          end;
          tkInterface:
          begin
            IInterface(PPointer(Item)^)._Release;
          end;
          tkDynArray:
          begin
            TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
          end;
        end;
      end;
      {$ifdef WEAKINSTREF}
      tkMethod:
      begin
        if (PMethod(Item).Data <> nil) then
          TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
      end;
      {$endif}
      tkVariant:
      begin
        VType := PVarData(Item).VType;
        if (VType and TRAIIHelper.varDeepData <> 0) then
        case VType of
          varBoolean, varUnknown+1..varUInt64: ;
        else
          System.VarClear(PVariant(Item)^);
        end;
      end;
    else
      TRAIIHelper<T>.Clear(Item);
    end;
    {$else}
    TRAIIHelper<T>.Clear(Item);
    {$endif}

    if (Count <> 0) then
    begin
      {$ifdef WEAKREF}
      {$ifdef SMARTGENERICS}
      if (TRAIIHelper<T>.Weak) then
      {$else}
      if (TRAIIHelper<T>.FOptions.FWeak) then
      {$endif}
      begin
        System.CopyArray(Item, Item + 1, TypeInfo(T), Count);
        System.Finalize((Item + Count)^);
      end else
      {$endif}
      begin
        System.Move(Pointer(Item + 1)^, Item^, Count * SizeOf(T));
      end;
    end;

    Exit;
  end else
  begin
    raise OutOfRangeException;
  end;
end;

procedure TList<T>.Delete(Index: Integer);
begin
  InternalDelete(Index, cnRemoved);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: Integer);
var
  Count, Index: NativeInt;
  Item: PItem;
  VType: Integer;
  Stored: TInternalStored;
begin
  if (AIndex >= 0) and (ACount >= 0) then
  begin
    Index := NativeInt(AIndex) + NativeInt(ACount);
    Count := FCount.Native;
    if (Index >= 0) and (Index <= Count) then
    begin
      if (ACount = 0) then Exit;
      Dec(Count, ACount);
      Dec(Index, ACount);
      FCount.Native := Count;
      Dec(Count, Index);
      Item := @FItems[Index];

      if (Assigned(FInternalNotify)) then
      begin
        Stored.Self := Self;
        Stored.InternalNotify := TMethod(FInternalNotify);
        Stored.Count := Count;
        Stored.ACount := ACount;
        for ACount := ACount downto 1 do
        begin
          TCollectionNotifyEvent<T>(Stored.InternalNotify)(Stored.Self, Item^, cnRemoved);
          Inc(Item);
        end;
        Count := Stored.Count;
        ACount := Stored.ACount;
        Dec(Item, ACount);
      end;

      {$ifdef SMARTGENERICS}
      if (System.IsManagedType(T)) then
      {$else}
      if (Assigned(TRAIIHelper<T>.Options.ClearProc)) then
      {$endif}
      begin
        Stored.ACount := ACount;
        for ACount := ACount downto 1 do
        begin
          {$ifdef SMARTGENERICS}
          case GetTypeKind(T) of
            {$ifdef AUTOREFCOUNT}
            tkClass,
            {$endif}
            tkWString, tkLString, tkUString, tkInterface, tkDynArray:
            begin
              if (PNativeInt(Item)^ <> 0) then
              case GetTypeKind(T) of
                {$ifdef AUTOREFCOUNT}
                tkClass:
                begin
                  TRAIIHelper.RefObjClear(Item);
                end;
                {$endif}
                {$ifdef MSWINDOWS}
                tkWString:
                begin
                  TRAIIHelper.WStrClear(Item);
                end;
                {$else}
                tkWString,
                {$endif}
                tkLString, tkUString:
                begin
                  TRAIIHelper.ULStrClear(Item);
                end;
                tkInterface:
                begin
                  IInterface(PPointer(Item)^)._Release;
                end;
                tkDynArray:
                begin
                  TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
                end;
              end;
            end;
            {$ifdef WEAKINSTREF}
            tkMethod:
            begin
              if (PMethod(Item).Data <> nil) then
                TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
            end;
            {$endif}
            tkVariant:
            begin
              VType := PVarData(Item).VType;
              if (VType and TRAIIHelper.varDeepData <> 0) then
              case VType of
                varBoolean, varUnknown+1..varUInt64: ;
              else
                System.VarClear(PVariant(Item)^);
              end;
            end;
          else
            TRAIIHelper<T>.Options.ClearProc(TRAIIHelper<T>.Options, Item);
          end;
          {$else}
          TRAIIHelper<T>.Options.ClearProc(TRAIIHelper<T>.Options, Item);
          {$endif}

          Inc(Item);
        end;
        ACount := Stored.ACount;
        Dec(Item, ACount);
      end;

      if (Count <> 0) then
      begin
        {$ifdef WEAKREF}
        {$ifdef SMARTGENERICS}
        if (TRAIIHelper<T>.Weak) then
        {$else}
        if (TRAIIHelper<T>.FOptions.FWeak) then
        {$endif}
        begin
          System.CopyArray(Item, Item + ACount, TypeInfo(T), Count);
          System.FinalizeArray(Item + Count, TypeInfo(T), ACount);
        end else
        {$endif}
        begin
          System.Move(Pointer(Item + ACount)^, Item^, Count * SizeOf(T));
        end;
      end;

      Exit;
    end;
  end;

  raise OutOfRangeException;
end;

function TList<T>.Expand: TList<T>;
begin
  if (FCount.Native = FCapacity.Native) then
    Grow;

  Result := Self;
end;

procedure TList<T>.Exchange(Index1, Index2: Integer);
var
  Count: Cardinal;
  X, Y: Pointer;
begin
  Count := FCount.Int;
  if (Cardinal(Index1) < Count) and (Cardinal(Index2) < Count) then
  begin
    if (Index1 <> Index2) then
    begin
      X := Pointer(FItems);
      Y := Pointer(TCustomList<T>.PItem(X) + Index2);
      X := Pointer(TCustomList<T>.PItem(X) + Index1);

      TArray.Exchange<T>(X, Y);
    end;
  end else
  begin
    raise OutOfRangeException;
  end;
end;

procedure TList<T>.InternalMove(CurIndex, NewIndex: Integer);
var
  Count: Cardinal;
  X, Y: PItem;
  Temp: T;
begin
  Count := FCount.Int;
  if (Cardinal(CurIndex) < Count) and (Cardinal(NewIndex) < Count) then
  begin
    if (CurIndex <> NewIndex) then
    begin
      X := Pointer(FItems);
      Y := X + NewIndex;
      X := X + CurIndex;

      Temp := X^;
      if (X < Y) then
      begin
        System.Move(Pointer(X + 1)^, X^, NativeUInt(Y) - NativeUInt(X));
      end else
      begin
        System.Move(Y^, Pointer(Y + 1)^, NativeUInt(X) - NativeUInt(Y));
      end;
      Y^ := Temp;
    end;
  end else
  begin
    raise OutOfRangeException;
  end;
end;

procedure TList<T>.InternalMove40(CurIndex, NewIndex: Integer);
var
  Count: Cardinal;
  X, Y: PItem;

  Temp1: TRAIIHelper.T1;
  Temp2: TRAIIHelper.T2;
  Temp4: TRAIIHelper.T4;
  Temp8: TRAIIHelper.T8;
  Temp40: TRAIIHelper.TTemp40;
begin
  Count := FCount.Int;
  if (Cardinal(CurIndex) < Count) and (Cardinal(NewIndex) < Count) then
  begin
    if (CurIndex <> NewIndex) then
    begin
      X := Pointer(FItems);
      Y := X + NewIndex;
      X := X + CurIndex;

      case SizeOf(T) of
         1: Temp1 := TRAIIHelper.T1(Pointer(X)^);
         2: Temp2 := TRAIIHelper.T2(Pointer(X)^);
         3: Temp40.V3 := TRAIIHelper.T3(Pointer(X)^);
         4: Temp4 := TRAIIHelper.T4(Pointer(X)^);
         5: Temp40.V5 := TRAIIHelper.T5(Pointer(X)^);
         6: Temp40.V6 := TRAIIHelper.T6(Pointer(X)^);
         7: Temp40.V7 := TRAIIHelper.T7(Pointer(X)^);
         8: Temp8 := TRAIIHelper.T8(Pointer(X)^);
         9: Temp40.V9 := TRAIIHelper.T9(Pointer(X)^);
        10: Temp40.V10 := TRAIIHelper.T10(Pointer(X)^);
        11: Temp40.V11 := TRAIIHelper.T11(Pointer(X)^);
        12: Temp40.V12 := TRAIIHelper.T12(Pointer(X)^);
        13: Temp40.V13 := TRAIIHelper.T13(Pointer(X)^);
        14: Temp40.V14 := TRAIIHelper.T14(Pointer(X)^);
        15: Temp40.V15 := TRAIIHelper.T15(Pointer(X)^);
        16: Temp40.V16 := TRAIIHelper.T16(Pointer(X)^);
        17: Temp40.V17 := TRAIIHelper.T17(Pointer(X)^);
        18: Temp40.V18 := TRAIIHelper.T18(Pointer(X)^);
        19: Temp40.V19 := TRAIIHelper.T19(Pointer(X)^);
        20: Temp40.V20 := TRAIIHelper.T20(Pointer(X)^);
        21: Temp40.V21 := TRAIIHelper.T21(Pointer(X)^);
        22: Temp40.V22 := TRAIIHelper.T22(Pointer(X)^);
        23: Temp40.V23 := TRAIIHelper.T23(Pointer(X)^);
        24: Temp40.V24 := TRAIIHelper.T24(Pointer(X)^);
        25: Temp40.V25 := TRAIIHelper.T25(Pointer(X)^);
        26: Temp40.V26 := TRAIIHelper.T26(Pointer(X)^);
        27: Temp40.V27 := TRAIIHelper.T27(Pointer(X)^);
        28: Temp40.V28 := TRAIIHelper.T28(Pointer(X)^);
        29: Temp40.V29 := TRAIIHelper.T29(Pointer(X)^);
        30: Temp40.V30 := TRAIIHelper.T30(Pointer(X)^);
        31: Temp40.V31 := TRAIIHelper.T31(Pointer(X)^);
        32: Temp40.V32 := TRAIIHelper.T32(Pointer(X)^);
        33: Temp40.V33 := TRAIIHelper.T33(Pointer(X)^);
        34: Temp40.V34 := TRAIIHelper.T34(Pointer(X)^);
        35: Temp40.V35 := TRAIIHelper.T35(Pointer(X)^);
        36: Temp40.V36 := TRAIIHelper.T36(Pointer(X)^);
        37: Temp40.V37 := TRAIIHelper.T37(Pointer(X)^);
        38: Temp40.V38 := TRAIIHelper.T38(Pointer(X)^);
        39: Temp40.V39 := TRAIIHelper.T39(Pointer(X)^);
        40: Temp40.V40 := TRAIIHelper.T40(Pointer(X)^);
      end;

      if (X < Y) then
      begin
        System.Move(Pointer(X + 1)^, X^, NativeUInt(Y) - NativeUInt(X));
      end else
      begin
        System.Move(Y^, Pointer(Y + 1)^, NativeUInt(X) - NativeUInt(Y));
      end;

      case SizeOf(T) of
         1: TRAIIHelper.T1(Pointer(Y)^) := Temp1;
         2: TRAIIHelper.T2(Pointer(Y)^) := Temp2;
         3: TRAIIHelper.T3(Pointer(Y)^) := Temp40.V3;
         4: TRAIIHelper.T4(Pointer(Y)^) := Temp4;
         5: TRAIIHelper.T5(Pointer(Y)^) := Temp40.V5;
         6: TRAIIHelper.T6(Pointer(Y)^) := Temp40.V6;
         7: TRAIIHelper.T7(Pointer(Y)^) := Temp40.V7;
         8: TRAIIHelper.T8(Pointer(Y)^) := Temp8;
         9: TRAIIHelper.T9(Pointer(Y)^) := Temp40.V9;
        10: TRAIIHelper.T10(Pointer(Y)^) := Temp40.V10;
        11: TRAIIHelper.T11(Pointer(Y)^) := Temp40.V11;
        12: TRAIIHelper.T12(Pointer(Y)^) := Temp40.V12;
        13: TRAIIHelper.T13(Pointer(Y)^) := Temp40.V13;
        14: TRAIIHelper.T14(Pointer(Y)^) := Temp40.V14;
        15: TRAIIHelper.T15(Pointer(Y)^) := Temp40.V15;
        16: TRAIIHelper.T16(Pointer(Y)^) := Temp40.V16;
        17: TRAIIHelper.T17(Pointer(Y)^) := Temp40.V17;
        18: TRAIIHelper.T18(Pointer(Y)^) := Temp40.V18;
        19: TRAIIHelper.T19(Pointer(Y)^) := Temp40.V19;
        20: TRAIIHelper.T20(Pointer(Y)^) := Temp40.V20;
        21: TRAIIHelper.T21(Pointer(Y)^) := Temp40.V21;
        22: TRAIIHelper.T22(Pointer(Y)^) := Temp40.V22;
        23: TRAIIHelper.T23(Pointer(Y)^) := Temp40.V23;
        24: TRAIIHelper.T24(Pointer(Y)^) := Temp40.V24;
        25: TRAIIHelper.T25(Pointer(Y)^) := Temp40.V25;
        26: TRAIIHelper.T26(Pointer(Y)^) := Temp40.V26;
        27: TRAIIHelper.T27(Pointer(Y)^) := Temp40.V27;
        28: TRAIIHelper.T28(Pointer(Y)^) := Temp40.V28;
        29: TRAIIHelper.T29(Pointer(Y)^) := Temp40.V29;
        30: TRAIIHelper.T30(Pointer(Y)^) := Temp40.V30;
        31: TRAIIHelper.T31(Pointer(Y)^) := Temp40.V31;
        32: TRAIIHelper.T32(Pointer(Y)^) := Temp40.V32;
        33: TRAIIHelper.T33(Pointer(Y)^) := Temp40.V33;
        34: TRAIIHelper.T34(Pointer(Y)^) := Temp40.V34;
        35: TRAIIHelper.T35(Pointer(Y)^) := Temp40.V35;
        36: TRAIIHelper.T36(Pointer(Y)^) := Temp40.V36;
        37: TRAIIHelper.T37(Pointer(Y)^) := Temp40.V37;
        38: TRAIIHelper.T38(Pointer(Y)^) := Temp40.V38;
        39: TRAIIHelper.T39(Pointer(Y)^) := Temp40.V39;
        40: TRAIIHelper.T40(Pointer(Y)^) := Temp40.V40;
      end;
    end;
  end else
  begin
    raise OutOfRangeException;
  end;
end;

procedure TList<T>.Move(CurIndex, NewIndex: Integer);
begin
  if (SizeOf(T) > 40) then
  begin
    InternalMove(CurIndex, NewIndex);
  end else
  begin
    InternalMove40(CurIndex, NewIndex);
  end;
end;

procedure TList<T>.Reverse;
begin
  TArray.Reverse<T>(Pointer(FItems), FCount.Native);
end;

procedure TList<T>.Sort;
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count > 1) then
  begin
    if (Assigned(FComparer)) then
    begin
      TArray.Sort<T>(FItems[0], Count, FComparer);
    end else
    begin
      TArray.Sort<T>(FItems[0], Count);
    end;
  end;
end;

procedure TList<T>.Sort(Index, Count: Integer);
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) or (Count < 0)
    or (Index + Count < 0) then
    raise OutOfRangeException;
  if Count <= 1 then
    Exit;

  if (Assigned(FComparer)) then
  begin
    TArray.Sort<T>(FItems[Index], Count, FComparer);
  end else
  begin
    TArray.Sort<T>(FItems[Index], Count);
  end;
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count > 1) then
  begin
    TArray.Sort<T>(FItems[0], Count, AComparer);
  end;
end;

procedure TList<T>.Sort(const AComparison: TComparison<T>);
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count > 1) then
  begin
    TArray.Sort<T>(FItems[0], Count, AComparison);
  end;
end;

procedure TList<T>.Sort(Index, Count: Integer; const AComparer: IComparer<T>);
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) or (Count < 0)
    or (Index + Count < 0) then
    raise OutOfRangeException;
  if Count <= 1 then
    Exit;

  TArray.Sort<T>(FItems[Index], Count, AComparer);
end;

procedure TList<T>.Sort(Index, Count: Integer; const AComparison: TComparison<T>);
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) or (Count < 0)
    or (Index + Count < 0) then
    raise OutOfRangeException;
  if Count <= 1 then
    Exit;

  TArray.Sort<T>(FItems[Index], Count, AComparison);
end;

procedure TList<T>.SortDescending;
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count > 1) then
  begin
    if (Assigned(FComparer)) then
    begin
      TArray.SortDescending<T>(FItems[0], Count, FComparer);
    end else
    begin
      TArray.SortDescending<T>(FItems[0], Count);
    end;
  end;
end;

procedure TList<T>.SortDescending(Index, Count: Integer);
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) or (Count < 0)
    or (Index + Count < 0) then
    raise OutOfRangeException;
  if Count <= 1 then
    Exit;

  if (Assigned(FComparer)) then
  begin
    TArray.SortDescending<T>(FItems[Index], Count, FComparer);
  end else
  begin
    TArray.SortDescending<T>(FItems[Index], Count);
  end;
end;

procedure TList<T>.SortDescending(const AComparer: IComparer<T>);
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count > 1) then
  begin
    TArray.SortDescending<T>(FItems[0], Count, AComparer);
  end;
end;

procedure TList<T>.SortDescending(const AComparison: TComparison<T>);
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count > 1) then
  begin
    TArray.SortDescending<T>(FItems[0], Count, AComparison);
  end;
end;

procedure TList<T>.SortDescending(Index, Count: Integer; const AComparer: IComparer<T>);
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) or (Count < 0)
    or (Index + Count < 0) then
    raise OutOfRangeException;
  if Count <= 1 then
    Exit;

  TArray.SortDescending<T>(FItems[Index], Count, AComparer);
end;

procedure TList<T>.SortDescending(Index, Count: Integer; const AComparison: TComparison<T>);
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) or (Count < 0)
    or (Index + Count < 0) then
    raise OutOfRangeException;
  if Count <= 1 then
    Exit;

  TArray.SortDescending<T>(FItems[Index], Count, AComparison);
end;

function TList<T>.BinarySearch(const Item: T; out FoundIndex: Integer): Boolean;
begin
  if (Assigned(FComparer)) then
  begin
    Result := TArray.InternalSearch<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex, Pointer(FComparer));
  end else
  begin
    Result := TArray.InternalSearch<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex);
  end;
end;

function TList<T>.BinarySearch(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>): Boolean;
begin
  Result := TArray.InternalSearch<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex, Pointer(AComparer));
end;

function TList<T>.BinarySearch(const Item: T; out FoundIndex: Integer; const AComparison: TComparison<T>): Boolean;
begin
  Result := TArray.InternalSearch<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex, PPointer(@AComparison)^);
end;

function TList<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Index, Count: Integer): Boolean;
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) {or (Count < 0)}
    or (Index + Count < 0) then
    raise OutOfRangeException;

  if (Assigned(FComparer)) then
  begin
    Result := TArray.InternalSearch<T>(Pointer(FItems), Index, Count, Item, FoundIndex, Pointer(FComparer));
  end else
  begin
    Result := TArray.InternalSearch<T>(Pointer(FItems), Index, Count, Item, FoundIndex);
  end;
end;

function TList<T>.BinarySearch(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>;
  Index, Count: Integer): Boolean;
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) {or (Count < 0)}
    or (Index + Count < 0) then
    raise OutOfRangeException;

  Result := TArray.InternalSearch<T>(Pointer(FItems), Index, Count, Item, FoundIndex, Pointer(AComparer));
end;

function TList<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Index, Count: Integer;
  const AComparison: TComparison<T>): Boolean;
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) {or (Count < 0)}
    or (Index + Count < 0) then
    raise OutOfRangeException;

  Result := TArray.InternalSearch<T>(Pointer(FItems), Index, Count, Item, FoundIndex, PPointer(@AComparison)^);
end;

function TList<T>.BinarySearchDescending(const Item: T; out FoundIndex: Integer): Boolean;
begin
  if (Assigned(FComparer)) then
  begin
    Result := TArray.InternalSearchDescending<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex, Pointer(FComparer));
  end else
  begin
    Result := TArray.InternalSearchDescending<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex);
  end;
end;

function TList<T>.BinarySearchDescending(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex, Pointer(AComparer));
end;

function TList<T>.BinarySearchDescending(const Item: T; out FoundIndex: Integer; const AComparison: TComparison<T>): Boolean;
begin
  Result := TArray.InternalSearchDescending<T>(Pointer(FItems), 0, FCount.Int, Item, FoundIndex, PPointer(@AComparison)^);
end;

function TList<T>.BinarySearchDescending(const Item: T; out FoundIndex: Integer; Index, Count: Integer): Boolean;
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) {or (Count < 0)}
    or (Index + Count < 0) then
    raise OutOfRangeException;

  if (Assigned(FComparer)) then
  begin
    Result := TArray.InternalSearchDescending<T>(Pointer(FItems), Index, Count, Item, FoundIndex, Pointer(FComparer));
  end else
  begin
    Result := TArray.InternalSearchDescending<T>(Pointer(FItems), Index, Count, Item, FoundIndex);
  end;
end;

function TList<T>.BinarySearchDescending(const Item: T; out FoundIndex: Integer; const AComparer: IComparer<T>;
  Index, Count: Integer): Boolean;
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) {or (Count < 0)}
    or (Index + Count < 0) then
    raise OutOfRangeException;

  Result := TArray.InternalSearchDescending<T>(Pointer(FItems), Index, Count, Item, FoundIndex, Pointer(AComparer));
end;

function TList<T>.BinarySearchDescending(const Item: T; out FoundIndex: Integer; Index, Count: Integer;
  const AComparison: TComparison<T>): Boolean;
begin
  if (Index < 0) or ((Index >= FCount.Int) and (Count > 0))
    or (Index + Count - 1 >= FCount.Int) {or (Count < 0)}
    or (Index + Count < 0) then
    raise OutOfRangeException;

  Result := TArray.InternalSearchDescending<T>(Pointer(FItems), Index, Count, Item, FoundIndex, PPointer(@AComparison)^);
end;

function TList<T>.InternalIndexOf(const Value: T): NativeInt;
{$if CompilerVersion >= 29}
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5{$ifdef SMALLINT}, cmp6, cmp7, cmp8, cmp9, cmp10{$endif};
{$ifend}
var
  R: NativeInt;
  Item, TopItem: PItem;
  {$if CompilerVersion >= 29}
  Count: NativeUInt;
  Left, Right: PByte;
  Offset: NativeUInt;
  {$ifend}
  Stored: TInternalStored;
begin
  if (not Assigned(FComparer)) then
  begin
    {$if CompilerVersion >= 29}
      if ((GetTypeKind(T) = tkFloat) and (SizeOf(T) = 8)) or (GetTypeKind(T) = tkDynArray) then
      begin
        if (not TRAIIHelper<T>.Created) then
          TRAIIHelper<T>.InternalCreate;
      end;
    {$else}
      if (not InterfaceDefaults.TDefaultEqualityComparer<T>.Created) then
        InterfaceDefaults.TDefaultEqualityComparer<T>.InternalCreate;
    {$ifend}

    Item := Pointer(FItems);
    Stored.Item := Item;
    Dec(Item);
    TopItem := Item + FCount.Native;

    repeat
      if (Item = TopItem) then Break;
      Inc(Item);
      {$if CompilerVersion >= 29}
      if (GetTypeKind(T) = tkVariant) then
      begin
        if (not InterfaceDefaults.Equals_Var(nil, PVarData(@Value), PVarData(Item))) then Continue;
      end else
      if (GetTypeKind(T) = tkClass) then
      begin
        Left := PPointer(@Value)^;
        Right := PPointer(Item)^;
        if (Assigned(Left)) then
        begin
          if (PPointer(Pointer(Left)^)[vmtEquals div SizeOf(Pointer)] = @TObject.Equals) then
          begin
            if (Left <> Right) then Continue;
          end else
          begin
            if (not TObject(PNativeUInt(@Value)^).Equals(TObject(PNativeUInt(Item)^))) then Continue;
          end;
        end else
        begin
          if (Right <> nil) then Continue;
        end;
      end else
      if (GetTypeKind(T) = tkFloat) then
      begin
        case SizeOf(T) of
          4:
          begin
            if (PSingle(@Value)^ <> PSingle(Item)^) then Continue;
          end;
          10:
          begin
            if (PExtended(@Value)^ <> PExtended(Item)^) then Continue;
          end;
        else
        {$ifdef LARGEINT}
          if (PInt64(@Value)^ <> PInt64(Item)^) then
        {$else .SMALLINT}
          if ((PPoint(@Value).X - PPoint(Item).X) or (PPoint(@Value).Y - PPoint(Item).Y) <> 0) then
        {$endif}
          begin
            if (TRAIIHelper<T>.Options.ItemSize < 0) then Continue;
            if (PDouble(@Value)^ <> PDouble(Item)^) then Continue;
          end;
        end;
      end else
      if (not (GetTypeKind(T) in [tkDynArray, tkString, tkLString, tkWString, tkUString])) and
        (SizeOf(T) <= 16) then
      begin
        // small binary
        if (SizeOf(T) <> 0) then
        with PData16(@Value)^ do
        begin
          if (SizeOf(T) >= SizeOf(Integer)) then
          begin
            if (SizeOf(T) >= SizeOf(Int64)) then
            begin
              {$ifdef LARGEINT}
              if (Int64s[0] <> PData16(Item).Int64s[0]) then Continue;
              {$else}
              if (Integers[0] <> PData16(Item).Integers[0]) then Continue;
              if (Integers[1] <> PData16(Item).Integers[1]) then Continue;
              {$endif}

              if (SizeOf(T) = 16) then
              begin
                {$ifdef LARGEINT}
                if (Int64s[1] <> PData16(Item).Int64s[1]) then Continue;
                {$else}
                if (Integers[2] <> PData16(Item).Integers[2]) then Continue;
                if (Integers[3] <> PData16(Item).Integers[3]) then Continue;
                {$endif}
              end else
              if (SizeOf(T) >= 12) then
              begin
                if (Integers[2] <> PData16(Item).Integers[2]) then Continue;
              end;
            end else
            begin
              if (Integers[0] <> PData16(Item).Integers[0]) then Continue;
            end;
          end;

          if (SizeOf(T) and 2 <> 0) then
          begin
            if (Words[(SizeOf(T) and -4) shr 1] <> PData16(Item).Words[(SizeOf(T) and -4) shr 1]) then Continue;
          end;
          if (SizeOf(T) and 1 <> 0) then
          begin
            if (Bytes[SizeOf(T) and -2] <> PData16(Item).Bytes[SizeOf(T) and -2]) then Continue;
          end;
        end;
      end else
      begin
        if (GetTypeKind(T) in [tkDynArray, tkString, tkLString, tkWString, tkUString]) then
        begin
          // dynamic size
          if (GetTypeKind(T) = tkString) then
          begin
            Left := Pointer(@Value);
            Right := Pointer(Item);
            if (PItem(Left) = {Right}Item) then goto cmp0;
            Count := Left^;
            if (Count <> Right^) then Continue;
            if (Count = 0) then goto cmp0;
            // compare last bytes
            if (Left[Count] <> Right[Count]) then Continue;
          end else
          // if (GetTypeKind(T) in [tkDynArray, tkLString, tkWString, tkUString]) then
          begin
            Left := PPointer(@Value)^;
            Right := PPointer(Item)^;
            if (Left = Right) then goto cmp0;
            if (Left = nil) then
            begin
              {$ifdef MSWINDOWS}
              if (GetTypeKind(T) = tkWString) then
              begin
                Dec(Right, SizeOf(Cardinal));
                if (PCardinal(Right)^ = 0) then goto cmp0;
              end;
              {$endif}
              Continue;
            end;
            if (Right = nil) then
            begin
              {$ifdef MSWINDOWS}
              if (GetTypeKind(T) = tkWString) then
              begin
                Dec(Left, SizeOf(Cardinal));
                if (PCardinal(Left)^ = 0) then goto cmp0;
              end;
              {$endif}
              Continue;
            end;

            if (GetTypeKind(T) = tkDynArray) then
            begin
              Dec(Left, SizeOf(NativeUInt));
              Dec(Right, SizeOf(NativeUInt));
              Count := PNativeUInt(Left)^;
              if (Count <> PNativeUInt(Right)^) then Continue;
              NativeInt(Count) := NativeInt(Count) * TRAIIHelper<T>.Options.ItemSize;
              Inc(Left, SizeOf(NativeUInt));
              Inc(Right, SizeOf(NativeUInt));
            end else
            // if (GetTypeKind(T) in [tkLString, tkWString, tkUString]) then
            begin
              Dec(Left, SizeOf(Cardinal));
              Dec(Right, SizeOf(Cardinal));
              Count := PCardinal(Left)^;
              if (Cardinal(Count) <> PCardinal(Right)^) then Continue;
              Inc(Left, SizeOf(Cardinal));
              Inc(Right, SizeOf(Cardinal));
            end;
          end;

          // compare last (after cardinal) words
          if (GetTypeKind(T) in [tkDynArray, tkString, tkLString]) then
          begin
            if (GetTypeKind(T) in [tkString, tkLString]) {ByteStrings + 2} then
            begin
              Inc(Count);
            end;
            if (Count and 2 <> 0) then
            begin
              Offset := Count and -4;
              Inc(Left, Offset);
              Inc(Right, Offset);
              if (PWord(Left)^ <> PWord(Right)^) then Continue;
              Offset := Count;
              Offset := Offset and -4;
              Dec(Left, Offset);
              Dec(Right, Offset);
            end;
          end else
          // modify Count to have only cardinals to compare
          // if (GetTypeKind(T) in [tkWString, tkUString]) {UnicodeStrings + 2} then
          begin
            {$ifdef MSWINDOWS}
            if (GetTypeKind(T) = tkWString) then
            begin
              if (Count = 0) then goto cmp0;
            end else
            {$endif}
            begin
              Inc(Count, Count);
            end;
            Inc(Count, 2);
          end;

          {$ifdef LARGEINT}
          if (Count and 4 <> 0) then
          begin
            Offset := Count and -8;
            Inc(Left, Offset);
            Inc(Right, Offset);
            if (PCardinal(Left)^ <> PCardinal(Right)^) then Continue;
            Dec(Left, Offset);
            Dec(Right, Offset);
          end;
          {$endif}
        end else
        begin
          // non-dynamic (constant) size binary > 16
          if (SizeOf(T) and {$ifdef LARGEINT}7{$else}3{$endif} <> 0) then
          with PData16(@Value)^ do
          begin
            {$ifdef LARGEINT}
            if (SizeOf(T) and 4 <> 0) then
            begin
              if (Integers[(SizeOf(T) and -8) shr 2] <> PData16(Item).Integers[(SizeOf(T) and -8) shr 2]) then Continue;
            end;
            {$endif}
            if (SizeOf(T) and 2 <> 0) then
            begin
              if (Words[(SizeOf(T) and -4) shr 1] <> PData16(Item).Words[(SizeOf(T) and -4) shr 1]) then Continue;
            end;
            if (SizeOf(T) and 1 <> 0) then
            begin
              if (Bytes[SizeOf(T) and -2] <> PData16(Item).Bytes[SizeOf(T) and -2]) then Continue;
            end;
          end;
          Left := Pointer(@Value);
          Right := Pointer(Item);
          Count := SizeOf(T);
        end;

        // natives (40 bytes static) compare
        Count := Count shr {$ifdef LARGEINT}3{$else}2{$endif};
        case Count of
        {$ifdef SMALLINT}
         10: goto cmp10;
          9: goto cmp9;
          8: goto cmp8;
          7: goto cmp7;
          6: goto cmp6;
        {$endif}
          5: goto cmp5;
          4: goto cmp4;
          3: goto cmp3;
          2: goto cmp2;
          1: goto cmp1;
          0: goto cmp0;
        else
          repeat
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Dec(Count);
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          until (Count = {$ifdef LARGEINT}5{$else}10{$endif});

          {$ifdef SMALLINT}
          cmp10:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp9:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp8:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp7:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp6:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          {$endif}
          cmp5:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp4:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp3:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp2:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp1:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
          cmp0:
        end;
      end;
      {$else}
        if (not TEquals(InterfaceDefaults.TDefaultEqualityComparer<T>.Instance.Equals)(
          @InterfaceDefaults.TDefaultEqualityComparer<T>.Instance, Item^, Value)) then Continue;
      {$ifend}

      R := NativeInt(Item) - NativeInt(Stored.Item);
      case SizeOf(T) of
      0, 1: Exit(R);
         2: Exit(R shr 1);
         4: Exit(R shr 2);
         8: Exit(R shr 3);
        16: Exit(R shr 4);
        32: Exit(R shr 5);
        64: Exit(R shr 6);
       128: Exit(R shr 7);
       256: Exit(R shr 8);
      else
        Exit(Round(R * (1 / SizeOf(T))));
      end;
    until (False);

    Exit(-1);
  end else
  begin
    Exit(InternalIndexOf(Value, FComparer));
  end;
end;

function TList<T>.InternalIndexOf(const Value: T; const Comparer: IComparer<T>): NativeInt;
var
  Count: NativeInt;
  Item: PItem;
  Compare: TMethod;
begin
  Count := FCount.Native;
  Item := Pointer(FItems);
  Compare.Data := Pointer(Comparer);
  Compare.Code := PPointer(PNativeUInt(Comparer)^ + 3 * SizeOf(Pointer))^;
  for Result := 0 to Count - 1 do
  begin
    if (TCompare(Compare.Code)(Compare.Data, Item^, Value) = 0) then Exit;
    Inc(Item);
  end;

  Result := -1;
end;

function TList<T>.InternalIndexOfRev(const Value: T): NativeInt;
{$if CompilerVersion >= 29}
label
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5{$ifdef SMALLINT}, cmp6, cmp7, cmp8, cmp9, cmp10{$endif};
{$ifend}
var
  R: NativeInt;
  Item, LowItem: PItem;
  {$if CompilerVersion >= 29}
  Count: NativeUInt;
  Left, Right: PByte;
  Offset: NativeUInt;
  {$ifend}
  Stored: TInternalStored;
begin
  if (not Assigned(FComparer)) then
  begin
    {$if CompilerVersion >= 29}
      if ((GetTypeKind(T) = tkFloat) and (SizeOf(T) = 8)) or (GetTypeKind(T) = tkDynArray) then
      begin
        if (not TRAIIHelper<T>.Created) then
          TRAIIHelper<T>.InternalCreate;
      end;
    {$else}
      if (not InterfaceDefaults.TDefaultEqualityComparer<T>.Created) then
        InterfaceDefaults.TDefaultEqualityComparer<T>.InternalCreate;
    {$ifend}

    LowItem := Pointer(FItems);
    Stored.Item := LowItem;
    Item := LowItem + FCount.Native;

    repeat
      if (Item = LowItem) then Break;
      Dec(Item);
      {$if CompilerVersion >= 29}
      if (GetTypeKind(T) = tkVariant) then
      begin
        if (not InterfaceDefaults.Equals_Var(nil, PVarData(@Value), PVarData(Item))) then Continue;
      end else
      if (GetTypeKind(T) = tkClass) then
      begin
        Left := PPointer(@Value)^;
        Right := PPointer(Item)^;
        if (Assigned(Left)) then
        begin
          if (PPointer(Pointer(Left)^)[vmtEquals div SizeOf(Pointer)] = @TObject.Equals) then
          begin
            if (Left <> Right) then Continue;
          end else
          begin
            if (not TObject(PNativeUInt(@Value)^).Equals(TObject(PNativeUInt(Item)^))) then Continue;
          end;
        end else
        begin
          if (Right <> nil) then Continue;
        end;
      end else
      if (GetTypeKind(T) = tkFloat) then
      begin
        case SizeOf(T) of
          4:
          begin
            if (PSingle(@Value)^ <> PSingle(Item)^) then Continue;
          end;
          10:
          begin
            if (PExtended(@Value)^ <> PExtended(Item)^) then Continue;
          end;
        else
        {$ifdef LARGEINT}
          if (PInt64(@Value)^ <> PInt64(Item)^) then
        {$else .SMALLINT}
          if ((PPoint(@Value).X - PPoint(Item).X) or (PPoint(@Value).Y - PPoint(Item).Y) <> 0) then
        {$endif}
          begin
            if (TRAIIHelper<T>.Options.ItemSize < 0) then Continue;
            if (PDouble(@Value)^ <> PDouble(Item)^) then Continue;
          end;
        end;
      end else
      if (not (GetTypeKind(T) in [tkDynArray, tkString, tkLString, tkWString, tkUString])) and
        (SizeOf(T) <= 16) then
      begin
        // small binary
        if (SizeOf(T) <> 0) then
        with PData16(@Value)^ do
        begin
          if (SizeOf(T) >= SizeOf(Integer)) then
          begin
            if (SizeOf(T) >= SizeOf(Int64)) then
            begin
              {$ifdef LARGEINT}
              if (Int64s[0] <> PData16(Item).Int64s[0]) then Continue;
              {$else}
              if (Integers[0] <> PData16(Item).Integers[0]) then Continue;
              if (Integers[1] <> PData16(Item).Integers[1]) then Continue;
              {$endif}

              if (SizeOf(T) = 16) then
              begin
                {$ifdef LARGEINT}
                if (Int64s[1] <> PData16(Item).Int64s[1]) then Continue;
                {$else}
                if (Integers[2] <> PData16(Item).Integers[2]) then Continue;
                if (Integers[3] <> PData16(Item).Integers[3]) then Continue;
                {$endif}
              end else
              if (SizeOf(T) >= 12) then
              begin
                if (Integers[2] <> PData16(Item).Integers[2]) then Continue;
              end;
            end else
            begin
              if (Integers[0] <> PData16(Item).Integers[0]) then Continue;
            end;
          end;

          if (SizeOf(T) and 2 <> 0) then
          begin
            if (Words[(SizeOf(T) and -4) shr 1] <> PData16(Item).Words[(SizeOf(T) and -4) shr 1]) then Continue;
          end;
          if (SizeOf(T) and 1 <> 0) then
          begin
            if (Bytes[SizeOf(T) and -2] <> PData16(Item).Bytes[SizeOf(T) and -2]) then Continue;
          end;
        end;
      end else
      begin
        if (GetTypeKind(T) in [tkDynArray, tkString, tkLString, tkWString, tkUString]) then
        begin
          // dynamic size
          if (GetTypeKind(T) = tkString) then
          begin
            Left := Pointer(@Value);
            Right := Pointer(Item);
            if (PItem(Left) = {Right}Item) then goto cmp0;
            Count := Left^;
            if (Count <> Right^) then Continue;
            if (Count = 0) then goto cmp0;
            // compare last bytes
            if (Left[Count] <> Right[Count]) then Continue;
          end else
          // if (GetTypeKind(T) in [tkDynArray, tkLString, tkWString, tkUString]) then
          begin
            Left := PPointer(@Value)^;
            Right := PPointer(Item)^;
            if (Left = Right) then goto cmp0;
            if (Left = nil) then
            begin
              {$ifdef MSWINDOWS}
              if (GetTypeKind(T) = tkWString) then
              begin
                Dec(Right, SizeOf(Cardinal));
                if (PCardinal(Right)^ = 0) then goto cmp0;
              end;
              {$endif}
              Continue;
            end;
            if (Right = nil) then
            begin
              {$ifdef MSWINDOWS}
              if (GetTypeKind(T) = tkWString) then
              begin
                Dec(Left, SizeOf(Cardinal));
                if (PCardinal(Left)^ = 0) then goto cmp0;
              end;
              {$endif}
              Continue;
            end;

            if (GetTypeKind(T) = tkDynArray) then
            begin
              Dec(Left, SizeOf(NativeUInt));
              Dec(Right, SizeOf(NativeUInt));
              Count := PNativeUInt(Left)^;
              if (Count <> PNativeUInt(Right)^) then Continue;
              NativeInt(Count) := NativeInt(Count) * TRAIIHelper<T>.Options.ItemSize;
              Inc(Left, SizeOf(NativeUInt));
              Inc(Right, SizeOf(NativeUInt));
            end else
            // if (GetTypeKind(T) in [tkLString, tkWString, tkUString]) then
            begin
              Dec(Left, SizeOf(Cardinal));
              Dec(Right, SizeOf(Cardinal));
              Count := PCardinal(Left)^;
              if (Cardinal(Count) <> PCardinal(Right)^) then Continue;
              Inc(Left, SizeOf(Cardinal));
              Inc(Right, SizeOf(Cardinal));
            end;
          end;

          // compare last (after cardinal) words
          if (GetTypeKind(T) in [tkDynArray, tkString, tkLString]) then
          begin
            if (GetTypeKind(T) in [tkString, tkLString]) {ByteStrings + 2} then
            begin
              Inc(Count);
            end;
            if (Count and 2 <> 0) then
            begin
              Offset := Count and -4;
              Inc(Left, Offset);
              Inc(Right, Offset);
              if (PWord(Left)^ <> PWord(Right)^) then Continue;
              Offset := Count;
              Offset := Offset and -4;
              Dec(Left, Offset);
              Dec(Right, Offset);
            end;
          end else
          // modify Count to have only cardinals to compare
          // if (GetTypeKind(T) in [tkWString, tkUString]) {UnicodeStrings + 2} then
          begin
            {$ifdef MSWINDOWS}
            if (GetTypeKind(T) = tkWString) then
            begin
              if (Count = 0) then goto cmp0;
            end else
            {$endif}
            begin
              Inc(Count, Count);
            end;
            Inc(Count, 2);
          end;

          {$ifdef LARGEINT}
          if (Count and 4 <> 0) then
          begin
            Offset := Count and -8;
            Inc(Left, Offset);
            Inc(Right, Offset);
            if (PCardinal(Left)^ <> PCardinal(Right)^) then Continue;
            Dec(Left, Offset);
            Dec(Right, Offset);
          end;
          {$endif}
        end else
        begin
          // non-dynamic (constant) size binary > 16
          if (SizeOf(T) and {$ifdef LARGEINT}7{$else}3{$endif} <> 0) then
          with PData16(@Value)^ do
          begin
            {$ifdef LARGEINT}
            if (SizeOf(T) and 4 <> 0) then
            begin
              if (Integers[(SizeOf(T) and -8) shr 2] <> PData16(Item).Integers[(SizeOf(T) and -8) shr 2]) then Continue;
            end;
            {$endif}
            if (SizeOf(T) and 2 <> 0) then
            begin
              if (Words[(SizeOf(T) and -4) shr 1] <> PData16(Item).Words[(SizeOf(T) and -4) shr 1]) then Continue;
            end;
            if (SizeOf(T) and 1 <> 0) then
            begin
              if (Bytes[SizeOf(T) and -2] <> PData16(Item).Bytes[SizeOf(T) and -2]) then Continue;
            end;
          end;
          Left := Pointer(@Value);
          Right := Pointer(Item);
          Count := SizeOf(T);
        end;

        // natives (40 bytes static) compare
        Count := Count shr {$ifdef LARGEINT}3{$else}2{$endif};
        case Count of
        {$ifdef SMALLINT}
         10: goto cmp10;
          9: goto cmp9;
          8: goto cmp8;
          7: goto cmp7;
          6: goto cmp6;
        {$endif}
          5: goto cmp5;
          4: goto cmp4;
          3: goto cmp3;
          2: goto cmp2;
          1: goto cmp1;
          0: goto cmp0;
        else
          repeat
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Dec(Count);
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          until (Count = {$ifdef LARGEINT}5{$else}10{$endif});

          {$ifdef SMALLINT}
          cmp10:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp9:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp8:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp7:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp6:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          {$endif}
          cmp5:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp4:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp3:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp2:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
            Inc(Left, SizeOf(NativeUInt));
            Inc(Right, SizeOf(NativeUInt));
          cmp1:
            if (PNativeUInt(Left)^ <> PNativeUInt(Right)^) then Continue;
          cmp0:
        end;
      end;
      {$else}
        if (not TEquals(InterfaceDefaults.TDefaultEqualityComparer<T>.Instance.Equals)(
          @InterfaceDefaults.TDefaultEqualityComparer<T>.Instance, Item^, Value)) then Continue;
      {$ifend}

      R := NativeInt(Item) - NativeInt(Stored.Item);
      case SizeOf(T) of
      0, 1: Exit(R);
         2: Exit(R shr 1);
         4: Exit(R shr 2);
         8: Exit(R shr 3);
        16: Exit(R shr 4);
        32: Exit(R shr 5);
        64: Exit(R shr 6);
       128: Exit(R shr 7);
       256: Exit(R shr 8);
      else
        Exit(Round(R * (1 / SizeOf(T))));
      end;
    until (False);

    Exit(-1);
  end else
  begin
    Exit(InternalIndexOfRev(Value, FComparer));
  end;
end;

function TList<T>.InternalIndexOfRev(const Value: T; const Comparer: IComparer<T>): NativeInt;
var
  Count: NativeInt;
  Item: PItem;
  Compare: TMethod;
begin
  Count := FCount.Native;
  Dec(Count);
  Item := @FItems[Count];
  Compare.Data := Pointer(Comparer);
  Compare.Code := PPointer(PNativeUInt(Comparer)^ + 3 * SizeOf(Pointer))^;
  for Result := Count downto 0 do
  begin
    if (TCompare(Compare.Code)(Compare.Data, Item^, Value) = 0) then Exit;
    Dec(Item);
  end;

  Result := -1;
end;

function TList<T>.Contains(const Value: T): Boolean;
begin
  Result := (InternalIndexOf(Value) >= 0);
end;

function TList<T>.IndexOf(const Value: T): Integer;
begin
  Result := InternalIndexOf(Value);
end;

function TList<T>.IndexOfItem(const Value: T; Direction: TDirection): Integer;
begin
  if (Direction = FromBeginning) then
  begin
    Result := InternalIndexOf(Value);
  end else
  begin
    Result := InternalIndexOfRev(Value);
  end;
end;

function TList<T>.LastIndexOf(const Value: T): Integer;
begin
  Result := InternalIndexOfRev(Value);
end;

function TList<T>.Remove(const Value: T): Integer;
var
  Index: NativeInt;
begin
  Index := IndexOf(Value);
  if (Index >= 0) then
    InternalDelete(Index, cnRemoved);

  Result := Index;
end;

function TList<T>.RemoveItem(const Value: T; Direction: TDirection): Integer;
var
  Index: NativeInt;
begin
  Index := IndexOfItem(Value, Direction);
  if (Index >= 0) then
    InternalDelete(Index, cnRemoved);

  Result := Index;
end;

function TList<T>.Extract(const Value: T): T;
var
  Index: NativeInt;
begin
  Index := IndexOf(Value);
  if (Index < 0) then
  begin
    Result := Default(T);
  end else
  begin
    Result := FItems[Index];
    InternalDelete(Index, cnExtracted);
  end;
end;

function TList<T>.ExtractItem(const Value: T; Direction: TDirection): T;
var
  Index: NativeInt;
begin
  Index := IndexOfItem(Value, Direction);
  if (Index < 0) then
  begin
    Result := Default(T);
  end else
  begin
    Result := FItems[Index];
    InternalDelete(Index, cnExtracted);
  end;
end;

{$ifdef WEAKREF}
procedure TList<T>.InternalWeakPack;
label
  next_find, next_item, comparer_recall;
var
  R, Index, LastCount: NativeInt;
  Item, TopItem, DestItem: ^TRAIIHelper.TData16;
  VByte: Byte;
  VWord: Word;
  VInteger: Integer;
  VNative, Flags: NativeUInt;
begin
  Item := Pointer(FItems);
  Dec(NativeUInt(Item), SizeOf(T));
  TopItem := Pointer(TCustomList<T>.PItem(Item) + FCount.Native);

  // find first empty
  repeat
  next_find:
    if (Item = TopItem) then Exit;
    Inc(NativeUInt(Item), SizeOf(T));

    if (SizeOf(T) <= 16) then
    begin
      {$ifdef SMALLINT}
        if (SizeOf(T) >= SizeOf(Int64) * 1) then
        begin
          if (Item.Integers[0] or Item.Integers[1] <> 0) then Continue;
        end;
        if (SizeOf(T)  = SizeOf(Int64) * 2) then
        begin
          if (Item.Integers[2] or Item.Integers[3] <> 0) then Continue;
        end;
      {$else .LARGEINT}
        if (SizeOf(T) >= SizeOf(Int64) * 1) then
        begin
          if (Item.Int64s[0] <> 0) then Continue;
        end;
        if (SizeOf(T)  = SizeOf(Int64) * 2) then
        begin
          if (Item.Int64s[1] <> 0) then Continue;
        end;
      {$endif}
      case SizeOf(T) of
         4..7: if (Item.Integers[0] <> 0) then Continue;
       12..15: if (Item.Integers[2] <> 0) then Continue;
      end;
      case SizeOf(T) of
         2,3: if (Item.Words[0] <> 0) then Continue;
         6,7: if (Item.Words[2] <> 0) then Continue;
       10,11: if (Item.Words[4] <> 0) then Continue;
       14,15: if (Item.Words[6] <> 0) then Continue;
      end;
      case SizeOf(T) of
         1: if (Item.Bytes[ 1-1] <> 0) then Continue;
         3: if (Item.Bytes[ 3-1] <> 0) then Continue;
         5: if (Item.Bytes[ 5-1] <> 0) then Continue;
         7: if (Item.Bytes[ 7-1] <> 0) then Continue;
         9: if (Item.Bytes[ 9-1] <> 0) then Continue;
        11: if (Item.Bytes[11-1] <> 0) then Continue;
        13: if (Item.Bytes[13-1] <> 0) then Continue;
        15: if (Item.Bytes[15-1] <> 0) then Continue;
      end;
    end else
    begin
      DestItem := Item;
      Index := 0;
      repeat
        Inc(Index);
        if (PNativeUInt(DestItem)^ <> 0) then goto next_find;
        Inc(NativeUInt(DestItem), SizeOf(NativeUInt));
      until (Index = SizeOf(T) div SizeOf(NativeUInt));

      if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
      begin
        Dec(NativeUInt(DestItem), SizeOf(T) - (SizeOf(NativeUInt) - 1));
        if (PNativeUInt(DestItem)^ <> 0) then goto next_find;
      end;
    end;
    Break;
  until (False);

  // compare empty and move
  DestItem := Item;
  goto next_item;
  repeat
    DestItem^ := Item^;
    Inc(NativeUInt(DestItem), SizeOf(T));
  next_item:
    if (Item = TopItem) then Break;
    Inc(NativeUInt(Item), SizeOf(T));

    case SizeOf(T) of
      1:
      begin
        VByte := Item.Bytes[0];
        if (VByte <> 0) then Continue;
      end;
      2:
      begin
        VWord := Item.Words[0];
        if (VWord <> 0) then Continue;
      end;
      4:
      begin
        VInteger := Item.Integers[0];
        if (VInteger <> 0) then Continue;
      end;
      8:
      begin
        {$ifdef LARGEINT}
          VNative := Item.Int64s[0];
          if (VNative <> 0) then Continue;
        {$else .SMALLINT}
          VInteger := Item.Integers[0];
          VNative := Item.Integers[1];
          if (VInteger or Integer(VNative) <> 0) then Continue;
        {$endif}
      end;
    else
      if (SizeOf(T) < SizeOf(NativeUInt)) then
      begin
        case SizeOf(T) of
          3:
          begin
            Flags := Item.Words[0];
            DestItem.Words[0] := Flags;
            VByte := Item.Bytes[2];
            DestItem.Bytes[2] := VByte;
            Flags := Flags or VByte;
          end;
          5:
          begin
            Flags := Item.Integers[0];
            DestItem.Integers[0] := Flags;
            VByte := Item.Bytes[4];
            DestItem.Bytes[4] := VByte;
            Flags := Flags or VByte;
          end;
          6:
          begin
            Flags := Item.Integers[0];
            DestItem.Integers[0] := Flags;
            VWord := Item.Words[2];
            DestItem.Words[2] := VWord;
            Flags := Flags or VWord;
          end;
          7:
          begin
            Flags := Item.Integers[0];
            DestItem.Integers[0] := Flags;
            VWord := Item.Words[2];
            DestItem.Words[2] := VWord;
            Flags := Flags or VWord;
            VByte := Item.Bytes[6];
            DestItem.Bytes[6] := VByte;
            Flags := Flags or VByte;
          end;
        end;
      end else
      if (SizeOf(T) <= 16) then
      begin
        {$ifdef LARGEINT}
          Flags := Item.Int64s[0];
          DestItem.Int64s[0] := Flags;
        {$else .SMALLINT}
          Flags := Item.Integers[0];
          DestItem.Integers[0] := Flags;
        {$endif}

        {$ifdef SMALLINT}
          if (SizeOf(T) >= SizeOf(Integer) * 2) then
          begin
            VInteger := Item.Integers[1];
            Item.Integers[1] := VInteger;
            Flags := Flags or Cardinal(VInteger);
          end;
          if (SizeOf(T) >= SizeOf(Integer) * 3) then
          begin
            VInteger := Item.Integers[2];
            Item.Integers[2] := VInteger;
            Flags := Flags or Cardinal(VInteger);
          end;
          if (SizeOf(T)  = SizeOf(Integer) * 4) then
          begin
            VInteger := Item.Integers[3];
            Item.Integers[3] := VInteger;
            Flags := Flags or Cardinal(VInteger);
          end;
        {$else .LARGEINT}
          if (SizeOf(T)  = SizeOf(Int64) * 2) then
          begin
            VNative := Item.Int64s[1];
            Item.Int64s[1] := VNative;
            Flags := Flags or VNative;
          end;
        {$endif}

        if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
        begin
          VNative := PNativeUInt(@Item.Bytes[SizeOf(T) - SizeOf(NativeUInt)])^;
          PNativeUInt(@DestItem.Bytes[SizeOf(T) - SizeOf(NativeUInt)])^ := VNative;
          Flags := Flags or VNative;
        end;
      end else
      begin
        Flags := 0;

        Index := 0;
        repeat
          Inc(Index);
          VNative := PNativeUInt(Item)^;
          PNativeUInt(DestItem)^ := VNative;
          Inc(NativeUInt(Item), SizeOf(NativeUInt));
          Inc(NativeUInt(DestItem), SizeOf(NativeUInt));
          Flags := Flags or VNative;
        until (Index = SizeOf(T) div SizeOf(NativeUInt));

        if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
        begin
          Dec(NativeUInt(Item), SizeOf(T) - (SizeOf(NativeUInt) - 1));
          Dec(NativeUInt(DestItem), SizeOf(T) - (SizeOf(NativeUInt) - 1));
          VNative := PNativeUInt(Item)^;
          PNativeUInt(DestItem)^ := VNative;
          Flags := Flags or VNative;
        end;

        Dec(NativeUInt(Item), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
        Dec(NativeUInt(DestItem), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
      end;

      if (Flags <> 0) then Continue;
    end;

    goto next_item;
  until (False);

  LastCount := FCount.Native;
  R := NativeInt(DestItem) - NativeInt(FItems);
  case SizeOf(T) of
  0, 1: FCount.Native := R;
     2: FCount.Native := R shr 1;
     4: FCount.Native := R shr 2;
     8: FCount.Native := R shr 3;
    16: FCount.Native := R shr 4;
    32: FCount.Native := R shr 5;
    64: FCount.Native := R shr 6;
   128: FCount.Native := R shr 7;
   256: FCount.Native := R shr 8;
  else
    FCount.Native := Round(R * (1 / SizeOf(T)));
  end;
  System.FinalizeArray(@FItems[FCount.Native], TypeInfo(T), LastCount - FCount.Native);
end;
{$endif}

{$ifdef SMARTGENERICS}
procedure TList<T>.InternalPackDifficults;
{$if CompilerVersion = 28}
var
  R: NativeInt;
  Item, TopItem, DestItem: PItem;
  VarData: PVarData;
  {$ifNdef CPUX86}
  VSingle, VSingleNull: Single;
  VDouble, VDoubleNull: Double;
  VExtended, VExtendedNull: Extended;
  {$endif}
begin
  Item := Pointer(FItems);
  Dec(Item);
  TopItem := Item + FCount.Native;

  {$ifNdef CPUX86}
  case SizeOf(T) of
    4:
    begin
      VSingleNull := 0;
    end;
    10:
    begin
      VExtendedNull := 0;
    end;
  else
    VDoubleNull := 0;
  end;
  {$endif}

  repeat
    if (Item = TopItem) then Exit;
    Inc(Item);

    if (GetTypeKind(T) = tkVariant) then
    begin
      VarData := Pointer(Item);
      while (VarData.VType = varByRef or varVariant) do
        VarData := PVarData(VarData.VPointer);

      if (VarData.VType > varNull) then Continue;
    end else
    if (GetTypeKind(T) = tkFloat) then
    begin
      case SizeOf(T) of
        4:
        begin
          if (PSingle(Item)^ <> {$ifdef CPUX86}0{$else}VSingleNull{$endif}) then Continue;
        end;
        10:
        begin
          if (PExtended(Item)^ <> {$ifdef CPUX86}0{$else}VExtendedNull{$endif}) then Continue;
        end;
      else
        if (PDouble(Item)^ <> {$ifdef CPUX86}0{$else}VDoubleNull{$endif}) then Continue;
      end;
    end else
    //if (GetTypeKind(T) = tkString) then
    begin
      if (PByte(Item)^ <> 0) then Continue;
    end;
    Break;
  until (False);

  DestItem := Item;
  Inc(TopItem);
  repeat
    Inc(Item);
    if (Item = TopItem) then Break;

    if (GetTypeKind(T) = tkVariant) then
    begin
      VarData := Pointer(Item);
      while (VarData.VType = varByRef or varVariant) do
        VarData := PVarData(VarData.VPointer);

      if (VarData.VType > varNull) then Break;
    end else
    if (GetTypeKind(T) = tkFloat) then
    begin
      case SizeOf(T) of
        4:
        begin
          {$ifdef CPUX86}
            if (PSingle(Item)^ <> 0) then Break;
          {$else}
            VSingle := PSingle(Item)^;
            if (VSingle <> VSingleNull) then Break;
          {$endif}
        end;
        10:
        begin
          {$ifdef CPUX86}
            if (PExtended(Item)^ <> 0) then Break;
          {$else}
            VExtended := PExtended(Item)^;
            if (VExtended <> VExtendedNull) then Break;
          {$endif}
        end;
      else
        {$ifdef CPUX86}
          if (PDouble(Item)^ <> 0) then Break;
        {$else}
          VDouble := PDouble(Item)^;
          if (VDouble <> VDoubleNull) then Break;
        {$endif}
      end;
    end else
    //if (GetTypeKind(T) = tkString) then
    begin
      if (PByte(Item)^ <> 0) then Break;
    end;
  until (False);

  if (Item <> TopItem) then
  repeat
    // DestItem^ := Item^;
    if (GetTypeKind(T) = tkVariant) then
    begin
      PRect(DestItem)^ := PRect(Item)^;
    end else
    if (GetTypeKind(T) = tkFloat) then
    begin
      {$ifdef CPUX86}
        DestItem^ := Item^;
      {$else !CPUX86}
      case SizeOf(T) of
         4: PSingle(DestItem)^ := VSingle;
        10: PExtended(DestItem)^ := VExtended;
      else
        PDouble(DestItem)^ := VDouble;
      end;
      {$endif}
    end else
    begin
      DestItem^ := Item^;
    end;

    Inc(DestItem);
    repeat
      Inc(Item);
      if (Item = TopItem) then Break;

      if (GetTypeKind(T) = tkVariant) then
      begin
        VarData := Pointer(Item);
        while (VarData.VType = varByRef or varVariant) do
          VarData := PVarData(VarData.VPointer);

        if (VarData.VType > varNull) then Break;
      end else
      if (GetTypeKind(T) = tkFloat) then
      begin
        case SizeOf(T) of
          4:
          begin
            {$ifdef CPUX86}
              if (PSingle(Item)^ <> 0) then Break;
            {$else}
              VSingle := PSingle(Item)^;
              if (VSingle <> VSingleNull) then Break;
            {$endif}
          end;
          10:
          begin
            {$ifdef CPUX86}
              if (PExtended(Item)^ <> 0) then Break;
            {$else}
              VExtended := PExtended(Item)^;
              if (VExtended <> VExtendedNull) then Break;
            {$endif}
          end;
        else
          {$ifdef CPUX86}
            if (PDouble(Item)^ <> 0) then Break;
          {$else}
            VDouble := PDouble(Item)^;
            if (VDouble <> VDoubleNull) then Break;
          {$endif}
        end;
      end else
      //if (GetTypeKind(T) = tkString) then
      begin
        if (PByte(Item)^ <> 0) then Break;
      end;
    until (False);
  until (Item = TopItem);

  R := NativeInt(DestItem) - NativeInt(FItems);
  case SizeOf(T) of
  0, 1: FCount.Native := R;
     2: FCount.Native := R shr 1;
     4: FCount.Native := R shr 2;
     8: FCount.Native := R shr 3;
    16: FCount.Native := R shr 4;
    32: FCount.Native := R shr 5;
    64: FCount.Native := R shr 6;
   128: FCount.Native := R shr 7;
   256: FCount.Native := R shr 8;
  else
    FCount.Native := Round(R * (1 / SizeOf(T)));
  end;
end;
{$else}
label
  next_item;
var
  R: NativeInt;
  Item, TopItem, DestItem: PItem;
  VarData: PVarData;
  {$ifNdef CPUX86}
  VSingle, VSingleNull: Single;
  VDouble, VDoubleNull: Double;
  VExtended, VExtendedNull: Extended;
  {$endif}
begin
  Item := Pointer(FItems);
  Dec(Item);
  TopItem := Item + FCount.Native;

  {$ifNdef CPUX86}
  case SizeOf(T) of
    4:
    begin
      VSingleNull := 0;
    end;
    10:
    begin
      VExtendedNull := 0;
    end;
  else
    VDoubleNull := 0;
  end;
  {$endif}

  repeat
    if (Item = TopItem) then Exit;
    Inc(Item);

    if (GetTypeKind(T) = tkVariant) then
    begin
      VarData := Pointer(Item);
      while (VarData.VType = varByRef or varVariant) do
        VarData := PVarData(VarData.VPointer);

      if (VarData.VType > varNull) then Continue;
    end else
    if (GetTypeKind(T) = tkFloat) then
    begin
      case SizeOf(T) of
        4:
        begin
          if (PSingle(Item)^ <> {$ifdef CPUX86}0{$else}VSingleNull{$endif}) then Continue;
        end;
        10:
        begin
          if (PExtended(Item)^ <> {$ifdef CPUX86}0{$else}VExtendedNull{$endif}) then Continue;
        end;
      else
        if (PDouble(Item)^ <> {$ifdef CPUX86}0{$else}VDoubleNull{$endif}) then Continue;
      end;
    end else
    //if (GetTypeKind(T) = tkString) then
    begin
      if (PByte(Item)^ <> 0) then Continue;
    end;
    Break;
  until (False);

  DestItem := Item;
  goto next_item;
  repeat
    // DestItem^ := Item^;
    if (GetTypeKind(T) = tkVariant) then
    begin
      PRect(DestItem)^ := PRect(Item)^;
    end else
    if (GetTypeKind(T) = tkFloat) then
    begin
      {$ifdef CPUX86}
        DestItem^ := Item^;
      {$else !CPUX86}
      case SizeOf(T) of
         4: PSingle(DestItem)^ := VSingle;
        10: PExtended(DestItem)^ := VExtended;
      else
        PDouble(DestItem)^ := VDouble;
      end;
      {$endif}
    end else
    begin
      DestItem^ := Item^;
    end;

    Inc(DestItem);
  next_item:
    if (Item = TopItem) then Break;
    Inc(Item);

    if (GetTypeKind(T) = tkVariant) then
    begin
      VarData := Pointer(Item);
      while (VarData.VType = varByRef or varVariant) do
        VarData := PVarData(VarData.VPointer);

      if (VarData.VType > varNull) then Continue;
    end else
    if (GetTypeKind(T) = tkFloat) then
    begin
      case SizeOf(T) of
        4:
        begin
          {$ifdef CPUX86}
            if (PSingle(Item)^ <> 0) then Continue;
          {$else}
            VSingle := PSingle(Item)^;
            if (VSingle <> VSingleNull) then Continue;
          {$endif}
        end;
        10:
        begin
          {$ifdef CPUX86}
            if (PExtended(Item)^ <> 0) then Continue;
          {$else}
            VExtended := PExtended(Item)^;
            if (VExtended <> VExtendedNull) then Continue;
          {$endif}
        end;
      else
        {$ifdef CPUX86}
          if (PDouble(Item)^ <> 0) then Continue;
        {$else}
          VDouble := PDouble(Item)^;
          if (VDouble <> VDoubleNull) then Continue;
        {$endif}
      end;
    end else
    //if (GetTypeKind(T) = tkString) then
    begin
      if (PByte(Item)^ <> 0) then Continue;
    end;

    goto next_item;
  until (False);

  R := NativeInt(DestItem) - NativeInt(FItems);
  case SizeOf(T) of
  0, 1: FCount.Native := R;
     2: FCount.Native := R shr 1;
     4: FCount.Native := R shr 2;
     8: FCount.Native := R shr 3;
    16: FCount.Native := R shr 4;
    32: FCount.Native := R shr 5;
    64: FCount.Native := R shr 6;
   128: FCount.Native := R shr 7;
   256: FCount.Native := R shr 8;
  else
    FCount.Native := Round(R * (1 / SizeOf(T)));
  end;
end;
{$ifend}

procedure TList<T>.Pack;
label
  next_find, next_item, comparer_recall;
var
  R, Index: NativeInt;
  Item, TopItem, DestItem: ^TRAIIHelper.TData16;
  VByte: Byte;
  VWord: Word;
  VInteger: Integer;
  VNative, Flags: NativeUInt;
begin
  if (Assigned(FComparer)) then goto comparer_recall;

  {$ifdef WEAKREF}
  if (TRAIIHelper<T>.Weak) then
  begin
    Self.InternalWeakPack;
  end else
  {$endif}
  begin
    if (GetTypeKind(T) = tkVariant) or (GetTypeKind(T) = tkString) or
      (GetTypeKind(T) = tkFloat) and
      (
        (SizeOf(T) <> SizeOf(Double)) or (TRAIIHelper<T>.Options.ItemSize >= 0)
      ) then
    begin
      Self.InternalPackDifficults;
      Exit;
    end;

    Item := Pointer(FItems);
    Dec(NativeUInt(Item), SizeOf(T));
    TopItem := Pointer(TCustomList<T>.PItem(Item) + FCount.Native);

    // find first empty
    repeat
    next_find:
      if (Item = TopItem) then Exit;
      Inc(NativeUInt(Item), SizeOf(T));

      if (SizeOf(T) <= 16) then
      begin
        {$ifdef SMALLINT}
          if (SizeOf(T) >= SizeOf(Int64) * 1) then
          begin
            if (Item.Integers[0] or Item.Integers[1] <> 0) then Continue;
          end;
          if (SizeOf(T)  = SizeOf(Int64) * 2) then
          begin
            if (Item.Integers[2] or Item.Integers[3] <> 0) then Continue;
          end;
        {$else .LARGEINT}
          if (SizeOf(T) >= SizeOf(Int64) * 1) then
          begin
            if (Item.Int64s[0] <> 0) then Continue;
          end;
          if (SizeOf(T)  = SizeOf(Int64) * 2) then
          begin
            if (Item.Int64s[1] <> 0) then Continue;
          end;
        {$endif}
        case SizeOf(T) of
           4..7: if (Item.Integers[0] <> 0) then Continue;
         12..15: if (Item.Integers[2] <> 0) then Continue;
        end;
        case SizeOf(T) of
           2,3: if (Item.Words[0] <> 0) then Continue;
           6,7: if (Item.Words[2] <> 0) then Continue;
         10,11: if (Item.Words[4] <> 0) then Continue;
         14,15: if (Item.Words[6] <> 0) then Continue;
        end;
        case SizeOf(T) of
           1: if (Item.Bytes[ 1-1] <> 0) then Continue;
           3: if (Item.Bytes[ 3-1] <> 0) then Continue;
           5: if (Item.Bytes[ 5-1] <> 0) then Continue;
           7: if (Item.Bytes[ 7-1] <> 0) then Continue;
           9: if (Item.Bytes[ 9-1] <> 0) then Continue;
          11: if (Item.Bytes[11-1] <> 0) then Continue;
          13: if (Item.Bytes[13-1] <> 0) then Continue;
          15: if (Item.Bytes[15-1] <> 0) then Continue;
        end;
      end else
      begin
        DestItem := Item;
        Index := 0;
        repeat
          Inc(Index);
          if (PNativeUInt(DestItem)^ <> 0) then goto next_find;
          Inc(NativeUInt(DestItem), SizeOf(NativeUInt));
        until (Index = SizeOf(T) div SizeOf(NativeUInt));

        if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
        begin
          Dec(NativeUInt(DestItem), SizeOf(T) - (SizeOf(NativeUInt) - 1));
          if (PNativeUInt(DestItem)^ <> 0) then goto next_find;
        end;
      end;
      Break;
    until (False);

    // compare empty and move
    DestItem := Item;
    goto next_item;
    repeat
      // DestItem^ := Item^;
      case SizeOf(T) of
        1: DestItem.Bytes[0] := VByte;
        2: DestItem.Words[0] := VWord;
        4: DestItem.Integers[0] := VInteger;
        8:
        begin
          {$ifdef LARGEINT}
            DestItem.Int64s[0] := VNative;
          {$else .SMALLINT}
            DestItem.Integers[0] := VInteger;
            DestItem.Integers[1] := VNative;
          {$endif}
        end;
        3: TRAIIHelper.T3(Pointer(DestItem)^) := TRAIIHelper.T3(Pointer(Item)^);
        5: TRAIIHelper.T5(Pointer(DestItem)^) := TRAIIHelper.T5(Pointer(Item)^);
        6: TRAIIHelper.T6(Pointer(DestItem)^) := TRAIIHelper.T6(Pointer(Item)^);
        7: TRAIIHelper.T7(Pointer(DestItem)^) := TRAIIHelper.T7(Pointer(Item)^);
        9: TRAIIHelper.T9(Pointer(DestItem)^) := TRAIIHelper.T9(Pointer(Item)^);
        10: TRAIIHelper.T10(Pointer(DestItem)^) := TRAIIHelper.T10(Pointer(Item)^);
        11: TRAIIHelper.T11(Pointer(DestItem)^) := TRAIIHelper.T11(Pointer(Item)^);
        12: TRAIIHelper.T12(Pointer(DestItem)^) := TRAIIHelper.T12(Pointer(Item)^);
        13: TRAIIHelper.T13(Pointer(DestItem)^) := TRAIIHelper.T13(Pointer(Item)^);
        14: TRAIIHelper.T14(Pointer(DestItem)^) := TRAIIHelper.T14(Pointer(Item)^);
        15: TRAIIHelper.T15(Pointer(DestItem)^) := TRAIIHelper.T15(Pointer(Item)^);
        16: TRAIIHelper.T16(Pointer(DestItem)^) := TRAIIHelper.T16(Pointer(Item)^);
        17: TRAIIHelper.T17(Pointer(DestItem)^) := TRAIIHelper.T17(Pointer(Item)^);
        18: TRAIIHelper.T18(Pointer(DestItem)^) := TRAIIHelper.T18(Pointer(Item)^);
        19: TRAIIHelper.T19(Pointer(DestItem)^) := TRAIIHelper.T19(Pointer(Item)^);
        20: TRAIIHelper.T20(Pointer(DestItem)^) := TRAIIHelper.T20(Pointer(Item)^);
        21: TRAIIHelper.T21(Pointer(DestItem)^) := TRAIIHelper.T21(Pointer(Item)^);
        22: TRAIIHelper.T22(Pointer(DestItem)^) := TRAIIHelper.T22(Pointer(Item)^);
        23: TRAIIHelper.T23(Pointer(DestItem)^) := TRAIIHelper.T23(Pointer(Item)^);
        24: TRAIIHelper.T24(Pointer(DestItem)^) := TRAIIHelper.T24(Pointer(Item)^);
        25: TRAIIHelper.T25(Pointer(DestItem)^) := TRAIIHelper.T25(Pointer(Item)^);
        26: TRAIIHelper.T26(Pointer(DestItem)^) := TRAIIHelper.T26(Pointer(Item)^);
        27: TRAIIHelper.T27(Pointer(DestItem)^) := TRAIIHelper.T27(Pointer(Item)^);
        28: TRAIIHelper.T28(Pointer(DestItem)^) := TRAIIHelper.T28(Pointer(Item)^);
        29: TRAIIHelper.T29(Pointer(DestItem)^) := TRAIIHelper.T29(Pointer(Item)^);
        30: TRAIIHelper.T30(Pointer(DestItem)^) := TRAIIHelper.T30(Pointer(Item)^);
        31: TRAIIHelper.T31(Pointer(DestItem)^) := TRAIIHelper.T31(Pointer(Item)^);
        32: TRAIIHelper.T32(Pointer(DestItem)^) := TRAIIHelper.T32(Pointer(Item)^);
        33: TRAIIHelper.T33(Pointer(DestItem)^) := TRAIIHelper.T33(Pointer(Item)^);
        34: TRAIIHelper.T34(Pointer(DestItem)^) := TRAIIHelper.T34(Pointer(Item)^);
        35: TRAIIHelper.T35(Pointer(DestItem)^) := TRAIIHelper.T35(Pointer(Item)^);
        36: TRAIIHelper.T36(Pointer(DestItem)^) := TRAIIHelper.T36(Pointer(Item)^);
        37: TRAIIHelper.T37(Pointer(DestItem)^) := TRAIIHelper.T37(Pointer(Item)^);
        38: TRAIIHelper.T38(Pointer(DestItem)^) := TRAIIHelper.T38(Pointer(Item)^);
        39: TRAIIHelper.T39(Pointer(DestItem)^) := TRAIIHelper.T39(Pointer(Item)^);
        40: TRAIIHelper.T40(Pointer(DestItem)^) := TRAIIHelper.T40(Pointer(Item)^);
      else
        System.Move(Item^, DestItem^, SizeOf(T));
      end;
      Inc(NativeUInt(DestItem), SizeOf(T));
    next_item:
      if (Item = TopItem) then Break;
      Inc(NativeUInt(Item), SizeOf(T));

      case SizeOf(T) of
        1:
        begin
          VByte := Item.Bytes[0];
          if (VByte <> 0) then Continue;
        end;
        2:
        begin
          VWord := Item.Words[0];
          if (VWord <> 0) then Continue;
        end;
        4:
        begin
          VInteger := Item.Integers[0];
          if (VInteger <> 0) then Continue;
        end;
        8:
        begin
          {$ifdef LARGEINT}
            VNative := Item.Int64s[0];
            if (VNative <> 0) then Continue;
          {$else .SMALLINT}
            VInteger := Item.Integers[0];
            VNative := Item.Integers[1];
            if (VInteger or Integer(VNative) <> 0) then Continue;
          {$endif}
        end;
      else
        if (SizeOf(T) < SizeOf(NativeUInt)) then
        begin
          case SizeOf(T) of
            3:
            begin
              Flags := Item.Words[0];
              DestItem.Words[0] := Flags;
              VByte := Item.Bytes[2];
              DestItem.Bytes[2] := VByte;
              Flags := Flags or VByte;
            end;
            5:
            begin
              Flags := Item.Integers[0];
              DestItem.Integers[0] := Flags;
              VByte := Item.Bytes[4];
              DestItem.Bytes[4] := VByte;
              Flags := Flags or VByte;
            end;
            6:
            begin
              Flags := Item.Integers[0];
              DestItem.Integers[0] := Flags;
              VWord := Item.Words[2];
              DestItem.Words[2] := VWord;
              Flags := Flags or VWord;
            end;
            7:
            begin
              Flags := Item.Integers[0];
              DestItem.Integers[0] := Flags;
              VWord := Item.Words[2];
              DestItem.Words[2] := VWord;
              Flags := Flags or VWord;
              VByte := Item.Bytes[6];
              DestItem.Bytes[6] := VByte;
              Flags := Flags or VByte;
            end;
          end;
        end else
        if (SizeOf(T) <= 16) then
        begin
          {$ifdef LARGEINT}
            Flags := Item.Int64s[0];
            DestItem.Int64s[0] := Flags;
          {$else .SMALLINT}
            Flags := Item.Integers[0];
            DestItem.Integers[0] := Flags;
          {$endif}

          {$ifdef SMALLINT}
            if (SizeOf(T) >= SizeOf(Integer) * 2) then
            begin
              VInteger := Item.Integers[1];
              Item.Integers[1] := VInteger;
              Flags := Flags or Cardinal(VInteger);
            end;
            if (SizeOf(T) >= SizeOf(Integer) * 3) then
            begin
              VInteger := Item.Integers[2];
              Item.Integers[2] := VInteger;
              Flags := Flags or Cardinal(VInteger);
            end;
            if (SizeOf(T)  = SizeOf(Integer) * 4) then
            begin
              VInteger := Item.Integers[3];
              Item.Integers[3] := VInteger;
              Flags := Flags or Cardinal(VInteger);
            end;
          {$else .LARGEINT}
            if (SizeOf(T)  = SizeOf(Int64) * 2) then
            begin
              VNative := Item.Int64s[1];
              Item.Int64s[1] := VNative;
              Flags := Flags or VNative;
            end;
          {$endif}

          if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
          begin
            VNative := PNativeUInt(@Item.Bytes[SizeOf(T) - SizeOf(NativeUInt)])^;
            PNativeUInt(@DestItem.Bytes[SizeOf(T) - SizeOf(NativeUInt)])^ := VNative;
            Flags := Flags or VNative;
          end;
        end else
        begin
          Flags := 0;

          Index := 0;
          repeat
            Inc(Index);
            VNative := PNativeUInt(Item)^;
            PNativeUInt(DestItem)^ := VNative;
            Inc(NativeUInt(Item), SizeOf(NativeUInt));
            Inc(NativeUInt(DestItem), SizeOf(NativeUInt));
            Flags := Flags or VNative;
          until (Index = SizeOf(T) div SizeOf(NativeUInt));

          if (SizeOf(T) and (SizeOf(NativeUInt) - 1) <> 0) then
          begin
            Dec(NativeUInt(Item), SizeOf(T) - (SizeOf(NativeUInt) - 1));
            Dec(NativeUInt(DestItem), SizeOf(T) - (SizeOf(NativeUInt) - 1));
            VNative := PNativeUInt(Item)^;
            PNativeUInt(DestItem)^ := VNative;
            Flags := Flags or VNative;
          end;

          Dec(NativeUInt(Item), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
          Dec(NativeUInt(DestItem), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
        end;

        if (Flags <> 0) then Continue;
      end;

      goto next_item;
    until (False);

    R := NativeInt(DestItem) - NativeInt(FItems);
    case SizeOf(T) of
    0, 1: FCount.Native := R;
       2: FCount.Native := R shr 1;
       4: FCount.Native := R shr 2;
       8: FCount.Native := R shr 3;
      16: FCount.Native := R shr 4;
      32: FCount.Native := R shr 5;
      64: FCount.Native := R shr 6;
     128: FCount.Native := R shr 7;
     256: FCount.Native := R shr 8;
    else
      FCount.Native := Round(R * (1 / SizeOf(T)));
    end;
  end;
  Exit;

comparer_recall:
  {$ifdef WEAKREF}
  if (TRAIIHelper<T>.Weak) then
  begin
    Self.InternalWeakPackComparer;
  end else
  {$endif}
  Self.InternalPackComparer;
end;

{$else !SMARTGENERICS}
procedure TList<T>.Pack;
var
  R, i: NativeInt;
  Item, TopItem, DestItem: PItem;
  _Self: TList<T>;
begin
  if (not Assigned(FComparer)) then
  begin
    {$ifdef WEAKREF}
    if (TRAIIHelper<T>.FOptions.FWeak) then
    begin
      Self.InternalWeakPack;
      Exit;
    end;
    {$endif}

    if (not InterfaceDefaults.TDefaultEqualityComparer<T>.Created) then
      InterfaceDefaults.TDefaultEqualityComparer<T>.InternalCreate;

    _Self := Self;
    with _Self do
    begin
      Item := Pointer(FItems);
      Dec(Item);
      TopItem := Item + FCount.Native;
    end;

    repeat
      if (Item = TopItem) then Exit;
      Inc(Item);
    until (TEquals(InterfaceDefaults.TDefaultEqualityComparer<T>.Instance.Equals)(
          @InterfaceDefaults.TDefaultEqualityComparer<T>.Instance, Item^, Default(T)));

    DestItem := Item;
    Inc(TopItem);
    repeat
      Inc(Item);
      if (Item = TopItem) then Break;
    until (not TEquals(InterfaceDefaults.TDefaultEqualityComparer<T>.Instance.Equals)(
          @InterfaceDefaults.TDefaultEqualityComparer<T>.Instance, Item^, Default(T)));

    if (Item <> TopItem) then
    repeat
      // DestItem^ := Item^;
      case SizeOf(T) of
        1: TRAIIHelper.T1(Pointer(DestItem)^) := TRAIIHelper.T1(Pointer(Item)^);
        2: TRAIIHelper.T2(Pointer(DestItem)^) := TRAIIHelper.T2(Pointer(Item)^);
        3: TRAIIHelper.T3(Pointer(DestItem)^) := TRAIIHelper.T3(Pointer(Item)^);
        4: TRAIIHelper.T4(Pointer(DestItem)^) := TRAIIHelper.T4(Pointer(Item)^);
        5: TRAIIHelper.T5(Pointer(DestItem)^) := TRAIIHelper.T5(Pointer(Item)^);
        6: TRAIIHelper.T6(Pointer(DestItem)^) := TRAIIHelper.T6(Pointer(Item)^);
        7: TRAIIHelper.T7(Pointer(DestItem)^) := TRAIIHelper.T7(Pointer(Item)^);
        8: TRAIIHelper.T8(Pointer(DestItem)^) := TRAIIHelper.T8(Pointer(Item)^);
        9: TRAIIHelper.T9(Pointer(DestItem)^) := TRAIIHelper.T9(Pointer(Item)^);
        10: TRAIIHelper.T10(Pointer(DestItem)^) := TRAIIHelper.T10(Pointer(Item)^);
        11: TRAIIHelper.T11(Pointer(DestItem)^) := TRAIIHelper.T11(Pointer(Item)^);
        12: TRAIIHelper.T12(Pointer(DestItem)^) := TRAIIHelper.T12(Pointer(Item)^);
        13: TRAIIHelper.T13(Pointer(DestItem)^) := TRAIIHelper.T13(Pointer(Item)^);
        14: TRAIIHelper.T14(Pointer(DestItem)^) := TRAIIHelper.T14(Pointer(Item)^);
        15: TRAIIHelper.T15(Pointer(DestItem)^) := TRAIIHelper.T15(Pointer(Item)^);
        16: TRAIIHelper.T16(Pointer(DestItem)^) := TRAIIHelper.T16(Pointer(Item)^);
        17: TRAIIHelper.T17(Pointer(DestItem)^) := TRAIIHelper.T17(Pointer(Item)^);
        18: TRAIIHelper.T18(Pointer(DestItem)^) := TRAIIHelper.T18(Pointer(Item)^);
        19: TRAIIHelper.T19(Pointer(DestItem)^) := TRAIIHelper.T19(Pointer(Item)^);
        20: TRAIIHelper.T20(Pointer(DestItem)^) := TRAIIHelper.T20(Pointer(Item)^);
        21: TRAIIHelper.T21(Pointer(DestItem)^) := TRAIIHelper.T21(Pointer(Item)^);
        22: TRAIIHelper.T22(Pointer(DestItem)^) := TRAIIHelper.T22(Pointer(Item)^);
        23: TRAIIHelper.T23(Pointer(DestItem)^) := TRAIIHelper.T23(Pointer(Item)^);
        24: TRAIIHelper.T24(Pointer(DestItem)^) := TRAIIHelper.T24(Pointer(Item)^);
        25: TRAIIHelper.T25(Pointer(DestItem)^) := TRAIIHelper.T25(Pointer(Item)^);
        26: TRAIIHelper.T26(Pointer(DestItem)^) := TRAIIHelper.T26(Pointer(Item)^);
        27: TRAIIHelper.T27(Pointer(DestItem)^) := TRAIIHelper.T27(Pointer(Item)^);
        28: TRAIIHelper.T28(Pointer(DestItem)^) := TRAIIHelper.T28(Pointer(Item)^);
        29: TRAIIHelper.T29(Pointer(DestItem)^) := TRAIIHelper.T29(Pointer(Item)^);
        30: TRAIIHelper.T30(Pointer(DestItem)^) := TRAIIHelper.T30(Pointer(Item)^);
        31: TRAIIHelper.T31(Pointer(DestItem)^) := TRAIIHelper.T31(Pointer(Item)^);
        32: TRAIIHelper.T32(Pointer(DestItem)^) := TRAIIHelper.T32(Pointer(Item)^);
        33: TRAIIHelper.T33(Pointer(DestItem)^) := TRAIIHelper.T33(Pointer(Item)^);
        34: TRAIIHelper.T34(Pointer(DestItem)^) := TRAIIHelper.T34(Pointer(Item)^);
        35: TRAIIHelper.T35(Pointer(DestItem)^) := TRAIIHelper.T35(Pointer(Item)^);
        36: TRAIIHelper.T36(Pointer(DestItem)^) := TRAIIHelper.T36(Pointer(Item)^);
        37: TRAIIHelper.T37(Pointer(DestItem)^) := TRAIIHelper.T37(Pointer(Item)^);
        38: TRAIIHelper.T38(Pointer(DestItem)^) := TRAIIHelper.T38(Pointer(Item)^);
        39: TRAIIHelper.T39(Pointer(DestItem)^) := TRAIIHelper.T39(Pointer(Item)^);
        40: TRAIIHelper.T40(Pointer(DestItem)^) := TRAIIHelper.T40(Pointer(Item)^);
      else
        for i := 1 to SizeOf(T) div SizeOf(NativeUInt) do
        begin
          NativeUInt(Pointer(DestItem)^) := NativeUInt(Pointer(Item)^);
          Inc(NativeInt(Item), SizeOf(NativeUInt));
          Inc(NativeInt(DestItem), SizeOf(NativeUInt));
        end;

        case SizeOf(T) and (SizeOf(NativeUInt) - 1) of
          1: TRAIIHelper.T1(Pointer(DestItem)^) := TRAIIHelper.T1(Pointer(Item)^);
          2: TRAIIHelper.T2(Pointer(DestItem)^) := TRAIIHelper.T2(Pointer(Item)^);
          3: TRAIIHelper.T3(Pointer(DestItem)^) := TRAIIHelper.T3(Pointer(Item)^);
        {$ifdef LARGEINT}
          4: TRAIIHelper.T4(Pointer(DestItem)^) := TRAIIHelper.T4(Pointer(Item)^);
          5: TRAIIHelper.T5(Pointer(DestItem)^) := TRAIIHelper.T5(Pointer(Item)^);
          6: TRAIIHelper.T6(Pointer(DestItem)^) := TRAIIHelper.T6(Pointer(Item)^);
          7: TRAIIHelper.T7(Pointer(DestItem)^) := TRAIIHelper.T7(Pointer(Item)^);
        {$endif}
        end;

        Dec(NativeInt(DestItem), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
        Dec(NativeInt(Item), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
      end;

      Inc(DestItem);
      repeat
        Inc(Item);
        if (Item = TopItem) then Break;
      until (not TEquals(InterfaceDefaults.TDefaultEqualityComparer<T>.Instance.Equals)(
        @InterfaceDefaults.TDefaultEqualityComparer<T>.Instance, Item^, Default(T)));
    until (Item = TopItem);

    _Self := Self;
    with _Self do
    begin
      R := NativeInt(DestItem) - NativeInt(FItems);
      case SizeOf(T) of
      0, 1: FCount.Native := R;
         2: FCount.Native := R shr 1;
         4: FCount.Native := R shr 2;
         8: FCount.Native := R shr 3;
        16: FCount.Native := R shr 4;
        32: FCount.Native := R shr 5;
        64: FCount.Native := R shr 6;
       128: FCount.Native := R shr 7;
       256: FCount.Native := R shr 8;
      else
        FCount.Native := Round(R * (1 / SizeOf(T)));
      end;
    end;
    Exit;
  end else
  begin
    {$ifdef WEAKREF}
    if (TRAIIHelper<T>.FOptions.FWeak) then
    begin
      Self.InternalWeakPackComparer;
    end else
    {$endif}
    Self.InternalPackComparer;
  end;
end;
{$endif}

{$ifdef WEAKREF}
procedure TList<T>.InternalWeakPack(const IsEmpty: TEmptyFunc);
var
  R, LastCount: NativeInt;
  Item, TopItem, DestItem: PItem;
  Equals: TMethod;
  _Self: TList<T>;
begin
  Equals.Data := PPointer(@IsEmpty)^;
  Equals.Code := PPointer(PNativeUInt(Equals.Data)^ + 3 * SizeOf(Pointer))^;

  _Self := Self;
  with _Self do
  begin
    Item := Pointer(FItems);
    Dec(Item);
    TopItem := Item + FCount.Native;
  end;

  repeat
    if (Item = TopItem) then Exit;
    Inc(Item);
  until (TEquals(Equals.Code)(Equals.Data, Item^, Default(T)));

  DestItem := Item;
  Inc(TopItem);
  repeat
    Inc(Item);
    if (Item = TopItem) then Break;
  until (not TEquals(Equals.Code)(Equals.Data, Item^, Default(T)));

  if (Item <> TopItem) then
  repeat
    DestItem^ := Item^;
    Inc(DestItem);
    repeat
      Inc(Item);
      if (Item = TopItem) then Break;
    until (not TEquals(Equals.Code)(Equals.Data, Item^, Default(T)));
  until (Item = TopItem);

  _Self := Self;
  with _Self do
  begin
    LastCount := FCount.Native;
    R := NativeInt(DestItem) - NativeInt(FItems);
    case SizeOf(T) of
    0, 1: FCount.Native := R;
       2: FCount.Native := R shr 1;
       4: FCount.Native := R shr 2;
       8: FCount.Native := R shr 3;
      16: FCount.Native := R shr 4;
      32: FCount.Native := R shr 5;
      64: FCount.Native := R shr 6;
     128: FCount.Native := R shr 7;
     256: FCount.Native := R shr 8;
    else
      FCount.Native := Round(R * (1 / SizeOf(T)));
    end;
    System.FinalizeArray(@FItems[FCount.Native], TypeInfo(T), LastCount - FCount.Native);
  end;
end;
{$endif}

procedure TList<T>.Pack(const IsEmpty: TEmptyFunc);
var
  R, i: NativeInt;
  Item, TopItem, DestItem: PItem;
  Equals: TMethod;
  _Self: TList<T>;
begin
  {$ifdef WEAKREF}
  {$ifdef SMARTGENERICS}
  if (TRAIIHelper<T>.Weak) then
  {$else}
  if (TRAIIHelper<T>.FOptions.FWeak) then
  {$endif}
  begin
    InternalWeakPack(IsEmpty);
  end else
  {$endif}
  begin
    Equals.Data := PPointer(@IsEmpty)^;
    Equals.Code := PPointer(PNativeUInt(Equals.Data)^ + 3 * SizeOf(Pointer))^;

    _Self := Self;
    with _Self do
    begin
      Item := Pointer(FItems);
      Dec(Item);
      TopItem := Item + FCount.Native;
    end;

    repeat
      if (Item = TopItem) then Exit;
      Inc(Item);
    until (TEquals(Equals.Code)(Equals.Data, Item^, Default(T)));

    DestItem := Item;
    Inc(TopItem);
    repeat
      Inc(Item);
      if (Item = TopItem) then Break;
    until (not TEquals(Equals.Code)(Equals.Data, Item^, Default(T)));

    if (Item <> TopItem) then
    repeat
      // DestItem^ := Item^;
      case SizeOf(T) of
        1: TRAIIHelper.T1(Pointer(DestItem)^) := TRAIIHelper.T1(Pointer(Item)^);
        2: TRAIIHelper.T2(Pointer(DestItem)^) := TRAIIHelper.T2(Pointer(Item)^);
        3: TRAIIHelper.T3(Pointer(DestItem)^) := TRAIIHelper.T3(Pointer(Item)^);
        4: TRAIIHelper.T4(Pointer(DestItem)^) := TRAIIHelper.T4(Pointer(Item)^);
        5: TRAIIHelper.T5(Pointer(DestItem)^) := TRAIIHelper.T5(Pointer(Item)^);
        6: TRAIIHelper.T6(Pointer(DestItem)^) := TRAIIHelper.T6(Pointer(Item)^);
        7: TRAIIHelper.T7(Pointer(DestItem)^) := TRAIIHelper.T7(Pointer(Item)^);
        8: TRAIIHelper.T8(Pointer(DestItem)^) := TRAIIHelper.T8(Pointer(Item)^);
        9: TRAIIHelper.T9(Pointer(DestItem)^) := TRAIIHelper.T9(Pointer(Item)^);
        10: TRAIIHelper.T10(Pointer(DestItem)^) := TRAIIHelper.T10(Pointer(Item)^);
        11: TRAIIHelper.T11(Pointer(DestItem)^) := TRAIIHelper.T11(Pointer(Item)^);
        12: TRAIIHelper.T12(Pointer(DestItem)^) := TRAIIHelper.T12(Pointer(Item)^);
        13: TRAIIHelper.T13(Pointer(DestItem)^) := TRAIIHelper.T13(Pointer(Item)^);
        14: TRAIIHelper.T14(Pointer(DestItem)^) := TRAIIHelper.T14(Pointer(Item)^);
        15: TRAIIHelper.T15(Pointer(DestItem)^) := TRAIIHelper.T15(Pointer(Item)^);
        16: TRAIIHelper.T16(Pointer(DestItem)^) := TRAIIHelper.T16(Pointer(Item)^);
        17: TRAIIHelper.T17(Pointer(DestItem)^) := TRAIIHelper.T17(Pointer(Item)^);
        18: TRAIIHelper.T18(Pointer(DestItem)^) := TRAIIHelper.T18(Pointer(Item)^);
        19: TRAIIHelper.T19(Pointer(DestItem)^) := TRAIIHelper.T19(Pointer(Item)^);
        20: TRAIIHelper.T20(Pointer(DestItem)^) := TRAIIHelper.T20(Pointer(Item)^);
        21: TRAIIHelper.T21(Pointer(DestItem)^) := TRAIIHelper.T21(Pointer(Item)^);
        22: TRAIIHelper.T22(Pointer(DestItem)^) := TRAIIHelper.T22(Pointer(Item)^);
        23: TRAIIHelper.T23(Pointer(DestItem)^) := TRAIIHelper.T23(Pointer(Item)^);
        24: TRAIIHelper.T24(Pointer(DestItem)^) := TRAIIHelper.T24(Pointer(Item)^);
        25: TRAIIHelper.T25(Pointer(DestItem)^) := TRAIIHelper.T25(Pointer(Item)^);
        26: TRAIIHelper.T26(Pointer(DestItem)^) := TRAIIHelper.T26(Pointer(Item)^);
        27: TRAIIHelper.T27(Pointer(DestItem)^) := TRAIIHelper.T27(Pointer(Item)^);
        28: TRAIIHelper.T28(Pointer(DestItem)^) := TRAIIHelper.T28(Pointer(Item)^);
        29: TRAIIHelper.T29(Pointer(DestItem)^) := TRAIIHelper.T29(Pointer(Item)^);
        30: TRAIIHelper.T30(Pointer(DestItem)^) := TRAIIHelper.T30(Pointer(Item)^);
        31: TRAIIHelper.T31(Pointer(DestItem)^) := TRAIIHelper.T31(Pointer(Item)^);
        32: TRAIIHelper.T32(Pointer(DestItem)^) := TRAIIHelper.T32(Pointer(Item)^);
        33: TRAIIHelper.T33(Pointer(DestItem)^) := TRAIIHelper.T33(Pointer(Item)^);
        34: TRAIIHelper.T34(Pointer(DestItem)^) := TRAIIHelper.T34(Pointer(Item)^);
        35: TRAIIHelper.T35(Pointer(DestItem)^) := TRAIIHelper.T35(Pointer(Item)^);
        36: TRAIIHelper.T36(Pointer(DestItem)^) := TRAIIHelper.T36(Pointer(Item)^);
        37: TRAIIHelper.T37(Pointer(DestItem)^) := TRAIIHelper.T37(Pointer(Item)^);
        38: TRAIIHelper.T38(Pointer(DestItem)^) := TRAIIHelper.T38(Pointer(Item)^);
        39: TRAIIHelper.T39(Pointer(DestItem)^) := TRAIIHelper.T39(Pointer(Item)^);
        40: TRAIIHelper.T40(Pointer(DestItem)^) := TRAIIHelper.T40(Pointer(Item)^);
      else
        for i := 1 to SizeOf(T) div SizeOf(NativeUInt) do
        begin
          NativeUInt(Pointer(DestItem)^) := NativeUInt(Pointer(Item)^);
          Inc(NativeInt(Item), SizeOf(NativeUInt));
          Inc(NativeInt(DestItem), SizeOf(NativeUInt));
        end;

        case SizeOf(T) and (SizeOf(NativeUInt) - 1) of
          1: TRAIIHelper.T1(Pointer(DestItem)^) := TRAIIHelper.T1(Pointer(Item)^);
          2: TRAIIHelper.T2(Pointer(DestItem)^) := TRAIIHelper.T2(Pointer(Item)^);
          3: TRAIIHelper.T3(Pointer(DestItem)^) := TRAIIHelper.T3(Pointer(Item)^);
        {$ifdef LARGEINT}
          4: TRAIIHelper.T4(Pointer(DestItem)^) := TRAIIHelper.T4(Pointer(Item)^);
          5: TRAIIHelper.T5(Pointer(DestItem)^) := TRAIIHelper.T5(Pointer(Item)^);
          6: TRAIIHelper.T6(Pointer(DestItem)^) := TRAIIHelper.T6(Pointer(Item)^);
          7: TRAIIHelper.T7(Pointer(DestItem)^) := TRAIIHelper.T7(Pointer(Item)^);
        {$endif}
        end;

        Dec(NativeInt(DestItem), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
        Dec(NativeInt(Item), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
      end;
      Inc(DestItem);
      repeat
        Inc(Item);
        if (Item = TopItem) then Break;
      until (not TEquals(Equals.Code)(Equals.Data, Item^, Default(T)));
    until (Item = TopItem);

    _Self := Self;
    with _Self do
    begin
      R := NativeInt(DestItem) - NativeInt(FItems);
      case SizeOf(T) of
      0, 1: FCount.Native := R;
         2: FCount.Native := R shr 1;
         4: FCount.Native := R shr 2;
         8: FCount.Native := R shr 3;
        16: FCount.Native := R shr 4;
        32: FCount.Native := R shr 5;
        64: FCount.Native := R shr 6;
       128: FCount.Native := R shr 7;
       256: FCount.Native := R shr 8;
      else
        FCount.Native := Round(R * (1 / SizeOf(T)));
      end;
    end;
  end;
end;

{$ifdef WEAKREF}
procedure TList<T>.InternalWeakPackComparer;
var
  R, LastCount: NativeInt;
  Item, TopItem, DestItem: PItem;
  Compare: TMethod;
  _Self: TList<T>;
begin
  Compare.Data := Pointer(FComparer);
  Compare.Code := PPointer(PNativeUInt(Compare.Data)^ + 3 * SizeOf(Pointer))^;

  _Self := Self;
  with _Self do
  begin
    Item := Pointer(FItems);
    Dec(Item);
    TopItem := Item + FCount.Native;
  end;

  repeat
    if (Item = TopItem) then Exit;
    Inc(Item);
  until (TCompare(Compare.Code)(Compare.Data, Item^, Default(T)) = 0);

  DestItem := Item;
  Inc(TopItem);
  repeat
    Inc(Item);
    if (Item = TopItem) then Break;
  until (TCompare(Compare.Code)(Compare.Data, Item^, Default(T)) <> 0);

  if (Item <> TopItem) then
  repeat
    DestItem^ := Item^;
    Inc(DestItem);
    repeat
      Inc(Item);
      if (Item = TopItem) then Break;
    until (TCompare(Compare.Code)(Compare.Data, Item^, Default(T)) <> 0);
  until (Item = TopItem);

  _Self := Self;
  with _Self do
  begin
    LastCount := FCount.Native;
    R := NativeInt(DestItem) - NativeInt(FItems);
    case SizeOf(T) of
    0, 1: FCount.Native := R;
       2: FCount.Native := R shr 1;
       4: FCount.Native := R shr 2;
       8: FCount.Native := R shr 3;
      16: FCount.Native := R shr 4;
      32: FCount.Native := R shr 5;
      64: FCount.Native := R shr 6;
     128: FCount.Native := R shr 7;
     256: FCount.Native := R shr 8;
    else
      FCount.Native := Round(R * (1 / SizeOf(T)));
    end;
    System.FinalizeArray(@FItems[FCount.Native], TypeInfo(T), LastCount - FCount.Native);
  end;
end;
{$endif}

procedure TList<T>.InternalPackComparer;
var
  R, i: NativeInt;
  Item, TopItem, DestItem: PItem;
  Compare: TMethod;
  _Self: TList<T>;
begin
  Compare.Data := Pointer(FComparer);
  Compare.Code := PPointer(PNativeUInt(Compare.Data)^ + 3 * SizeOf(Pointer))^;

  _Self := Self;
  with _Self do
  begin
    Item := Pointer(FItems);
    Dec(Item);
    TopItem := Item + FCount.Native;
  end;

  repeat
    if (Item = TopItem) then Exit;
    Inc(Item);
  until (TCompare(Compare.Code)(Compare.Data, Item^, Default(T)) = 0);

  DestItem := Item;
  Inc(TopItem);
  repeat
    Inc(Item);
    if (Item = TopItem) then Break;
  until (TCompare(Compare.Code)(Compare.Data, Item^, Default(T)) <> 0);

  if (Item <> TopItem) then
  repeat
    // DestItem^ := Item^;
    case SizeOf(T) of
      1: TRAIIHelper.T1(Pointer(DestItem)^) := TRAIIHelper.T1(Pointer(Item)^);
      2: TRAIIHelper.T2(Pointer(DestItem)^) := TRAIIHelper.T2(Pointer(Item)^);
      3: TRAIIHelper.T3(Pointer(DestItem)^) := TRAIIHelper.T3(Pointer(Item)^);
      4: TRAIIHelper.T4(Pointer(DestItem)^) := TRAIIHelper.T4(Pointer(Item)^);
      5: TRAIIHelper.T5(Pointer(DestItem)^) := TRAIIHelper.T5(Pointer(Item)^);
      6: TRAIIHelper.T6(Pointer(DestItem)^) := TRAIIHelper.T6(Pointer(Item)^);
      7: TRAIIHelper.T7(Pointer(DestItem)^) := TRAIIHelper.T7(Pointer(Item)^);
      8: TRAIIHelper.T8(Pointer(DestItem)^) := TRAIIHelper.T8(Pointer(Item)^);
      9: TRAIIHelper.T9(Pointer(DestItem)^) := TRAIIHelper.T9(Pointer(Item)^);
      10: TRAIIHelper.T10(Pointer(DestItem)^) := TRAIIHelper.T10(Pointer(Item)^);
      11: TRAIIHelper.T11(Pointer(DestItem)^) := TRAIIHelper.T11(Pointer(Item)^);
      12: TRAIIHelper.T12(Pointer(DestItem)^) := TRAIIHelper.T12(Pointer(Item)^);
      13: TRAIIHelper.T13(Pointer(DestItem)^) := TRAIIHelper.T13(Pointer(Item)^);
      14: TRAIIHelper.T14(Pointer(DestItem)^) := TRAIIHelper.T14(Pointer(Item)^);
      15: TRAIIHelper.T15(Pointer(DestItem)^) := TRAIIHelper.T15(Pointer(Item)^);
      16: TRAIIHelper.T16(Pointer(DestItem)^) := TRAIIHelper.T16(Pointer(Item)^);
      17: TRAIIHelper.T17(Pointer(DestItem)^) := TRAIIHelper.T17(Pointer(Item)^);
      18: TRAIIHelper.T18(Pointer(DestItem)^) := TRAIIHelper.T18(Pointer(Item)^);
      19: TRAIIHelper.T19(Pointer(DestItem)^) := TRAIIHelper.T19(Pointer(Item)^);
      20: TRAIIHelper.T20(Pointer(DestItem)^) := TRAIIHelper.T20(Pointer(Item)^);
      21: TRAIIHelper.T21(Pointer(DestItem)^) := TRAIIHelper.T21(Pointer(Item)^);
      22: TRAIIHelper.T22(Pointer(DestItem)^) := TRAIIHelper.T22(Pointer(Item)^);
      23: TRAIIHelper.T23(Pointer(DestItem)^) := TRAIIHelper.T23(Pointer(Item)^);
      24: TRAIIHelper.T24(Pointer(DestItem)^) := TRAIIHelper.T24(Pointer(Item)^);
      25: TRAIIHelper.T25(Pointer(DestItem)^) := TRAIIHelper.T25(Pointer(Item)^);
      26: TRAIIHelper.T26(Pointer(DestItem)^) := TRAIIHelper.T26(Pointer(Item)^);
      27: TRAIIHelper.T27(Pointer(DestItem)^) := TRAIIHelper.T27(Pointer(Item)^);
      28: TRAIIHelper.T28(Pointer(DestItem)^) := TRAIIHelper.T28(Pointer(Item)^);
      29: TRAIIHelper.T29(Pointer(DestItem)^) := TRAIIHelper.T29(Pointer(Item)^);
      30: TRAIIHelper.T30(Pointer(DestItem)^) := TRAIIHelper.T30(Pointer(Item)^);
      31: TRAIIHelper.T31(Pointer(DestItem)^) := TRAIIHelper.T31(Pointer(Item)^);
      32: TRAIIHelper.T32(Pointer(DestItem)^) := TRAIIHelper.T32(Pointer(Item)^);
      33: TRAIIHelper.T33(Pointer(DestItem)^) := TRAIIHelper.T33(Pointer(Item)^);
      34: TRAIIHelper.T34(Pointer(DestItem)^) := TRAIIHelper.T34(Pointer(Item)^);
      35: TRAIIHelper.T35(Pointer(DestItem)^) := TRAIIHelper.T35(Pointer(Item)^);
      36: TRAIIHelper.T36(Pointer(DestItem)^) := TRAIIHelper.T36(Pointer(Item)^);
      37: TRAIIHelper.T37(Pointer(DestItem)^) := TRAIIHelper.T37(Pointer(Item)^);
      38: TRAIIHelper.T38(Pointer(DestItem)^) := TRAIIHelper.T38(Pointer(Item)^);
      39: TRAIIHelper.T39(Pointer(DestItem)^) := TRAIIHelper.T39(Pointer(Item)^);
      40: TRAIIHelper.T40(Pointer(DestItem)^) := TRAIIHelper.T40(Pointer(Item)^);
    else
      for i := 1 to SizeOf(T) div SizeOf(NativeUInt) do
      begin
        NativeUInt(Pointer(DestItem)^) := NativeUInt(Pointer(Item)^);
        Inc(NativeInt(Item), SizeOf(NativeUInt));
        Inc(NativeInt(DestItem), SizeOf(NativeUInt));
      end;

      case SizeOf(T) and (SizeOf(NativeUInt) - 1) of
        1: TRAIIHelper.T1(Pointer(DestItem)^) := TRAIIHelper.T1(Pointer(Item)^);
        2: TRAIIHelper.T2(Pointer(DestItem)^) := TRAIIHelper.T2(Pointer(Item)^);
        3: TRAIIHelper.T3(Pointer(DestItem)^) := TRAIIHelper.T3(Pointer(Item)^);
      {$ifdef LARGEINT}
        4: TRAIIHelper.T4(Pointer(DestItem)^) := TRAIIHelper.T4(Pointer(Item)^);
        5: TRAIIHelper.T5(Pointer(DestItem)^) := TRAIIHelper.T5(Pointer(Item)^);
        6: TRAIIHelper.T6(Pointer(DestItem)^) := TRAIIHelper.T6(Pointer(Item)^);
        7: TRAIIHelper.T7(Pointer(DestItem)^) := TRAIIHelper.T7(Pointer(Item)^);
      {$endif}
      end;

      Dec(NativeInt(DestItem), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
      Dec(NativeInt(Item), (SizeOf(T) div SizeOf(NativeUInt)) * SizeOf(NativeUInt));
    end;

    Inc(DestItem);
    repeat
      Inc(Item);
      if (Item = TopItem) then Break;
    until (TCompare(Compare.Code)(Compare.Data, Item^, Default(T)) <> 0);
  until (Item = TopItem);

  _Self := Self;
  with _Self do
  begin
    R := NativeInt(DestItem) - NativeInt(FItems);
    case SizeOf(T) of
    0, 1: FCount.Native := R;
       2: FCount.Native := R shr 1;
       4: FCount.Native := R shr 2;
       8: FCount.Native := R shr 3;
      16: FCount.Native := R shr 4;
      32: FCount.Native := R shr 5;
      64: FCount.Native := R shr 6;
     128: FCount.Native := R shr 7;
     256: FCount.Native := R shr 8;
    else
      FCount.Native := Round(R * (1 / SizeOf(T)));
    end;
  end;
end;


{ TStack<T> }

constructor TStack<T>.Create;
begin
  inherited Create;
end;

constructor TStack<T>.Create(const Collection: TEnumerable<T>);
var
  Item: T;
begin
  Create;
  for Item in Collection do
    Push(Item);
end;

procedure TStack<T>.InternalPush(const Value: T);
var
  Count, Null: NativeInt;
  Item: ^TRAIIHelper.TData16;
begin
  Count := FCount.Native;
  repeat
    if (Count <> FCapacity.Native) then
    begin
      FCount.Native := Count + 1;
      Item := Pointer(@FItems[Count]);

      {$ifdef SMARTGENERICS}
      if (System.IsManagedType(T)) then
      {$else}
      if (SizeOf(T) >= SizeOf(NativeInt)) then
      {$endif}
      begin
        {$ifdef SMARTGENERICS}
        if (GetTypeKind(T) = tkVariant) then
        begin
          Item.Integers[0] := 0;
        end else
        {$endif}
        if (SizeOf(T) <= 16) then
        begin
          Null := 0;
          {$ifdef SMALLINT}
            if (SizeOf(T) >= SizeOf(Integer) * 1) then Item.Integers[0] := Null;
            if (SizeOf(T) >= SizeOf(Integer) * 2) then Item.Integers[1] := Null;
            if (SizeOf(T) >= SizeOf(Integer) * 3) then Item.Integers[2] := Null;
            if (SizeOf(T)  = SizeOf(Integer) * 4) then Item.Integers[3] := Null;
          {$else .LARGEINT}
            if (SizeOf(T) >= SizeOf(Int64) * 1) then Item.Int64s[0] := Null;
            if (SizeOf(T)  = SizeOf(Int64) * 2) then Item.Int64s[1] := Null;
            case SizeOf(T) of
               4..7: Item.Integers[0] := Null;
             12..15: Item.Integers[2] := Null;
            end;
          {$endif}
          case SizeOf(T) of
             2,3: Item.Words[0] := 0;
             6,7: Item.Words[2] := 0;
           10,11: Item.Words[4] := 0;
           14,15: Item.Words[6] := 0;
          end;
          case SizeOf(T) of
             1: Item.Bytes[ 1-1] := 0;
             3: Item.Bytes[ 3-1] := 0;
             5: Item.Bytes[ 5-1] := 0;
             7: Item.Bytes[ 7-1] := 0;
             9: Item.Bytes[ 9-1] := 0;
            11: Item.Bytes[11-1] := 0;
            13: Item.Bytes[13-1] := 0;
            15: Item.Bytes[15-1] := 0;
          end;
        end else
        begin
          TRAIIHelper<T>.Init(Pointer(Item));
        end;
      end;

      PItem(Item)^ := Value;
      if Assigned(FInternalNotify) then
        FInternalNotify(Self, Value, cnAdded);
      Exit;
    end else
    begin
      Self.Grow;
    end;
  until (False);
end;

procedure TStack<T>.Push(const Value: T);
var
  Count, Null: NativeInt;
  Item: ^TRAIIHelper.TData16;
begin
  Count := FCount.Native;
  if (Count <> FCapacity.Native) and (not Assigned(FInternalNotify)) then
  begin
    Inc(Count);
    FCount.Native := Count;
    Dec(Count);
    Item := Pointer(@FItems[Count]);

    {$ifdef SMARTGENERICS}
    if (System.IsManagedType(T)) then
    {$else}
    if (SizeOf(T) >= SizeOf(NativeInt)) then
    {$endif}
    begin
      {$ifdef SMARTGENERICS}
      if (GetTypeKind(T) = tkVariant) then
      begin
        Item.Integers[0] := 0;
      end else
      {$endif}
      if (SizeOf(T) <= 16) then
      begin
        Null := 0;
        {$ifdef SMALLINT}
          if (SizeOf(T) >= SizeOf(Integer) * 1) then Item.Integers[0] := Null;
          if (SizeOf(T) >= SizeOf(Integer) * 2) then Item.Integers[1] := Null;
          if (SizeOf(T) >= SizeOf(Integer) * 3) then Item.Integers[2] := Null;
          if (SizeOf(T)  = SizeOf(Integer) * 4) then Item.Integers[3] := Null;
        {$else .LARGEINT}
          if (SizeOf(T) >= SizeOf(Int64) * 1) then Item.Int64s[0] := Null;
          if (SizeOf(T)  = SizeOf(Int64) * 2) then Item.Int64s[1] := Null;
          case SizeOf(T) of
             4..7: Item.Integers[0] := Null;
           12..15: Item.Integers[2] := Null;
          end;
        {$endif}
        case SizeOf(T) of
           2,3: Item.Words[0] := 0;
           6,7: Item.Words[2] := 0;
         10,11: Item.Words[4] := 0;
         14,15: Item.Words[6] := 0;
        end;
        case SizeOf(T) of
           1: Item.Bytes[ 1-1] := 0;
           3: Item.Bytes[ 3-1] := 0;
           5: Item.Bytes[ 5-1] := 0;
           7: Item.Bytes[ 7-1] := 0;
           9: Item.Bytes[ 9-1] := 0;
          11: Item.Bytes[11-1] := 0;
          13: Item.Bytes[13-1] := 0;
          15: Item.Bytes[15-1] := 0;
        end;
      end else
      begin
        TRAIIHelper<T>.Init(Pointer(Item));
      end;
    end;

    PItem(Item)^ := Value;
    Exit;
  end else
  begin
    Self.InternalPush(Value);
  end;
end;

function TStack<T>.InternalPop(const Action: TCollectionNotification): T;
var
  Count: NativeInt;
  Item: PItem;
  VType: Integer;
begin
  Count := FCount.Native;
  if (Count <> 0) then
  begin
    Dec(Count);
    FCount.Native := Count;
    Item := @FItems[Count];
    Result := Item^;

    if Assigned(FInternalNotify) then
      Self.FInternalNotify(Self, Item^, Action);

    {$ifdef SMARTGENERICS}
    case GetTypeKind(T) of
      {$ifdef AUTOREFCOUNT}
      tkClass,
      {$endif}
      tkWString, tkLString, tkUString, tkInterface, tkDynArray:
      begin
        if (PNativeInt(Item)^ <> 0) then
        case GetTypeKind(T) of
          {$ifdef AUTOREFCOUNT}
          tkClass:
          begin
            TRAIIHelper.RefObjClear(Item);
          end;
          {$endif}
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            TRAIIHelper.WStrClear(Item);
          end;
          {$else}
          tkWString,
          {$endif}
          tkLString, tkUString:
          begin
            TRAIIHelper.ULStrClear(Item);
          end;
          tkInterface:
          begin
            IInterface(PPointer(Item)^)._Release;
          end;
          tkDynArray:
          begin
            TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
          end;
        end;
      end;
      {$ifdef WEAKINSTREF}
      tkMethod:
      begin
        if (PMethod(Item).Data <> nil) then
          TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
      end;
      {$endif}
      tkVariant:
      begin
        VType := PVarData(Item).VType;
        if (VType and TRAIIHelper.varDeepData <> 0) then
        case VType of
          varBoolean, varUnknown+1..varUInt64: ;
        else
          System.VarClear(PVariant(Item)^);
        end;
      end;
    else
      TRAIIHelper<T>.Clear(Item);
    end;
    {$else}
    TRAIIHelper<T>.Clear(Item);
    {$endif}

    Exit;
  end else
  begin
    raise Self.EmptyException;
  end;
end;

function TStack<T>.Pop: T;
var
  Count: NativeInt;
  Item: PItem;
  VType: Integer;
begin
  Count := FCount.Native;
  if (Count <> 0) and (not Assigned(FInternalNotify)) then
  begin
    Dec(Count);
    FCount.Native := Count;
    Item := @FItems[Count];
    Result := Item^;

    {$ifdef SMARTGENERICS}
    case GetTypeKind(T) of
      {$ifdef AUTOREFCOUNT}
      tkClass,
      {$endif}
      tkWString, tkLString, tkUString, tkInterface, tkDynArray:
      begin
        if (PNativeInt(Item)^ <> 0) then
        case GetTypeKind(T) of
          {$ifdef AUTOREFCOUNT}
          tkClass:
          begin
            TRAIIHelper.RefObjClear(Item);
          end;
          {$endif}
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            TRAIIHelper.WStrClear(Item);
          end;
          {$else}
          tkWString,
          {$endif}
          tkLString, tkUString:
          begin
            TRAIIHelper.ULStrClear(Item);
          end;
          tkInterface:
          begin
            IInterface(PPointer(Item)^)._Release;
          end;
          tkDynArray:
          begin
            TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
          end;
        end;
      end;
      {$ifdef WEAKINSTREF}
      tkMethod:
      begin
        if (PMethod(Item).Data <> nil) then
          TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
      end;
      {$endif}
      tkVariant:
      begin
        VType := PVarData(Item).VType;
        if (VType and TRAIIHelper.varDeepData <> 0) then
        case VType of
          varBoolean, varUnknown+1..varUInt64: ;
        else
          System.VarClear(PVariant(Item)^);
        end;
      end;
    else
      TRAIIHelper<T>.Clear(Item);
    end;
    {$else}
    TRAIIHelper<T>.Clear(Item);
    {$endif}

    Exit;
  end else
  begin
    Result := Self.InternalPop(cnRemoved);
  end;
end;

function TStack<T>.Extract: T;
var
  Count: NativeInt;
  Item: PItem;
  VType: Integer;
begin
  Count := FCount.Native;
  if (Count <> 0) and (not Assigned(FInternalNotify)) then
  begin
    Dec(Count);
    FCount.Native := Count;
    Item := @FItems[Count];
    Result := Item^;

    {$ifdef SMARTGENERICS}
    case GetTypeKind(T) of
      {$ifdef AUTOREFCOUNT}
      tkClass,
      {$endif}
      tkWString, tkLString, tkUString, tkInterface, tkDynArray:
      begin
        if (PNativeInt(Item)^ <> 0) then
        case GetTypeKind(T) of
          {$ifdef AUTOREFCOUNT}
          tkClass:
          begin
            TRAIIHelper.RefObjClear(Item);
          end;
          {$endif}
          {$ifdef MSWINDOWS}
          tkWString:
          begin
            TRAIIHelper.WStrClear(Item);
          end;
          {$else}
          tkWString,
          {$endif}
          tkLString, tkUString:
          begin
            TRAIIHelper.ULStrClear(Item);
          end;
          tkInterface:
          begin
            IInterface(PPointer(Item)^)._Release;
          end;
          tkDynArray:
          begin
            TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
          end;
        end;
      end;
      {$ifdef WEAKINSTREF}
      tkMethod:
      begin
        if (PMethod(Item).Data <> nil) then
          TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
      end;
      {$endif}
      tkVariant:
      begin
        VType := PVarData(Item).VType;
        if (VType and TRAIIHelper.varDeepData <> 0) then
        case VType of
          varBoolean, varUnknown+1..varUInt64: ;
        else
          System.VarClear(PVariant(Item)^);
        end;
      end;
    else
      TRAIIHelper<T>.Clear(Item);
    end;
    {$else}
    TRAIIHelper<T>.Clear(Item);
    {$endif}

    Exit;
  end else
  begin
    Result := Self.InternalPop(cnExtracted);
  end;
end;

function TStack<T>.Peek: T;
var
  Count: NativeInt;
begin
  Count := FCount.Native;
  if (Count <> 0) then
  begin
    Result := FItems[Count - 1];
    Exit;
  end else
  begin
    raise Self.EmptyException;
  end;
end;


{ TQueue<T> }

constructor TQueue<T>.Create;
begin
  inherited Create;
end;

constructor TQueue<T>.Create(const Collection: TEnumerable<T>);
var
  Item: T;
begin
  Create;
  for Item in Collection do
    Enqueue(Item);
end;

procedure TQueue<T>.InternalEnqueue(const Value: T);
var
  Count, Null: NativeInt;
  Item: ^TRAIIHelper.TData16;
begin
  Count := FCount.Native;
  repeat
    if (Count <> FCapacity.Native) then
    begin
      FCount.Native := Count + 1;
      Count := FHead;
      repeat
        if (Count <> FCapacity.Native) then
        begin
          Inc(Count);
          FHead := Count;
          Dec(Count);
          Item := Pointer(@FItems[Count]);

          {$ifdef SMARTGENERICS}
          if (System.IsManagedType(T)) then
          {$else}
          if (SizeOf(T) >= SizeOf(NativeInt)) then
          {$endif}
          begin
            {$ifdef SMARTGENERICS}
            if (GetTypeKind(T) = tkVariant) then
            begin
              Item.Integers[0] := 0;
            end else
            {$endif}
            if (SizeOf(T) <= 16) then
            begin
              Null := 0;
              {$ifdef SMALLINT}
                if (SizeOf(T) >= SizeOf(Integer) * 1) then Item.Integers[0] := Null;
                if (SizeOf(T) >= SizeOf(Integer) * 2) then Item.Integers[1] := Null;
                if (SizeOf(T) >= SizeOf(Integer) * 3) then Item.Integers[2] := Null;
                if (SizeOf(T)  = SizeOf(Integer) * 4) then Item.Integers[3] := Null;
              {$else .LARGEINT}
                if (SizeOf(T) >= SizeOf(Int64) * 1) then Item.Int64s[0] := Null;
                if (SizeOf(T)  = SizeOf(Int64) * 2) then Item.Int64s[1] := Null;
                case SizeOf(T) of
                   4..7: Item.Integers[0] := Null;
                 12..15: Item.Integers[2] := Null;
                end;
              {$endif}
              case SizeOf(T) of
                 2,3: Item.Words[0] := 0;
                 6,7: Item.Words[2] := 0;
               10,11: Item.Words[4] := 0;
               14,15: Item.Words[6] := 0;
              end;
              case SizeOf(T) of
                 1: Item.Bytes[ 1-1] := 0;
                 3: Item.Bytes[ 3-1] := 0;
                 5: Item.Bytes[ 5-1] := 0;
                 7: Item.Bytes[ 7-1] := 0;
                 9: Item.Bytes[ 9-1] := 0;
                11: Item.Bytes[11-1] := 0;
                13: Item.Bytes[13-1] := 0;
                15: Item.Bytes[15-1] := 0;
              end;
            end else
            begin
              TRAIIHelper<T>.Init(Pointer(Item));
            end;
          end;

          PItem(Item)^ := Value;
          if Assigned(FInternalNotify) then
            FInternalNotify(Self, Value, cnAdded);
          Exit;
        end else
        begin
          Count := 0;
        end;
      until (False);
    end else
    begin
      Self.Grow;
    end;
  until (False);
end;

procedure TQueue<T>.Enqueue(const Value: T);
var
  Count, Null: NativeInt;
  Item: ^TRAIIHelper.TData16;
begin
  Count := FCount.Native;
  if (Count <> FCapacity.Native) and (not Assigned(FInternalNotify)) then
  begin
    FCount.Native := Count + 1;
    Count := FHead;
    repeat
      if (Count <> FCapacity.Native) then
      begin
        Inc(Count);
        FHead := Count;
        Dec(Count);
        Item := Pointer(@FItems[Count]);

        {$ifdef SMARTGENERICS}
        if (System.IsManagedType(T)) then
        {$else}
        if (SizeOf(T) >= SizeOf(NativeInt)) then
        {$endif}
        begin
          {$ifdef SMARTGENERICS}
          if (GetTypeKind(T) = tkVariant) then
          begin
            Item.Integers[0] := 0;
          end else
          {$endif}
          if (SizeOf(T) <= 16) then
          begin
            Null := 0;
            {$ifdef SMALLINT}
              if (SizeOf(T) >= SizeOf(Integer) * 1) then Item.Integers[0] := Null;
              if (SizeOf(T) >= SizeOf(Integer) * 2) then Item.Integers[1] := Null;
              if (SizeOf(T) >= SizeOf(Integer) * 3) then Item.Integers[2] := Null;
              if (SizeOf(T)  = SizeOf(Integer) * 4) then Item.Integers[3] := Null;
            {$else .LARGEINT}
              if (SizeOf(T) >= SizeOf(Int64) * 1) then Item.Int64s[0] := Null;
              if (SizeOf(T)  = SizeOf(Int64) * 2) then Item.Int64s[1] := Null;
              case SizeOf(T) of
                 4..7: Item.Integers[0] := Null;
               12..15: Item.Integers[2] := Null;
              end;
            {$endif}
            case SizeOf(T) of
               2,3: Item.Words[0] := 0;
               6,7: Item.Words[2] := 0;
             10,11: Item.Words[4] := 0;
             14,15: Item.Words[6] := 0;
            end;
            case SizeOf(T) of
               1: Item.Bytes[ 1-1] := 0;
               3: Item.Bytes[ 3-1] := 0;
               5: Item.Bytes[ 5-1] := 0;
               7: Item.Bytes[ 7-1] := 0;
               9: Item.Bytes[ 9-1] := 0;
              11: Item.Bytes[11-1] := 0;
              13: Item.Bytes[13-1] := 0;
              15: Item.Bytes[15-1] := 0;
            end;
          end else
          begin
            TRAIIHelper<T>.Init(Pointer(Item));
          end;
        end;

        PItem(Item)^ := Value;
        Exit;
      end else
      begin
        Count := 0;
      end;
    until (False);
  end else
  begin
    Self.InternalEnqueue(Value);
  end;
end;

function TQueue<T>.InternalDequeue(const Action: TCollectionNotification): T;
var
  Count: NativeInt;
  Item: PItem;
  VType: Integer;
begin
  Count := FCount.Native;
  if (Count <> 0) then
  begin
    FCount.Native := Count - 1;
    Count := FTail;
    Item := @FItems[Count];
    Inc(Count);
    repeat
      if (Count <> FCapacity.Native) then
      begin
        FTail := Count;
        Result := Item^;

        Self.FInternalNotify(Self, Item^, Action);

        {$ifdef SMARTGENERICS}
        case GetTypeKind(T) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (PNativeInt(Item)^ <> 0) then
            case GetTypeKind(T) of
              {$ifdef AUTOREFCOUNT}
              tkClass:
              begin
                TRAIIHelper.RefObjClear(Item);
              end;
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString:
              begin
                TRAIIHelper.WStrClear(Item);
              end;
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString:
              begin
                TRAIIHelper.ULStrClear(Item);
              end;
              tkInterface:
              begin
                IInterface(PPointer(Item)^)._Release;
              end;
              tkDynArray:
              begin
                TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
              end;
            end;
          end;
          {$ifdef WEAKINSTREF}
          tkMethod:
          begin
            if (PMethod(Item).Data <> nil) then
              TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
          end;
          {$endif}
          tkVariant:
          begin
            VType := PVarData(Item).VType;
            if (VType and TRAIIHelper.varDeepData <> 0) then
            case VType of
              varBoolean, varUnknown+1..varUInt64: ;
            else
              System.VarClear(PVariant(Item)^);
            end;
          end;
        else
          TRAIIHelper<T>.Clear(Item);
        end;
        {$else}
        TRAIIHelper<T>.Clear(Item);
        {$endif}

        Exit;
      end else
      begin
        Count := 0;
      end;
    until (False);
  end else
  begin
    raise Self.EmptyException;
  end;
end;

function TQueue<T>.Dequeue: T;
var
  Count: NativeInt;
  Item: PItem;
  VType: Integer;
begin
  Count := FCount.Native;
  if (Count <> 0) and (not Assigned(FInternalNotify)) then
  begin
    FCount.Native := Count - 1;
    Count := FTail;
    Item := @FItems[Count];
    Inc(Count);
    repeat
      if (Count <> FCapacity.Native) then
      begin
        FTail := Count;
        Result := Item^;

        {$ifdef SMARTGENERICS}
        case GetTypeKind(T) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (PNativeInt(Item)^ <> 0) then
            case GetTypeKind(T) of
              {$ifdef AUTOREFCOUNT}
              tkClass:
              begin
                TRAIIHelper.RefObjClear(Item);
              end;
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString:
              begin
                TRAIIHelper.WStrClear(Item);
              end;
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString:
              begin
                TRAIIHelper.ULStrClear(Item);
              end;
              tkInterface:
              begin
                IInterface(PPointer(Item)^)._Release;
              end;
              tkDynArray:
              begin
                TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
              end;
            end;
          end;
          {$ifdef WEAKINSTREF}
          tkMethod:
          begin
            if (PMethod(Item).Data <> nil) then
              TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
          end;
          {$endif}
          tkVariant:
          begin
            VType := PVarData(Item).VType;
            if (VType and TRAIIHelper.varDeepData <> 0) then
            case VType of
              varBoolean, varUnknown+1..varUInt64: ;
            else
              System.VarClear(PVariant(Item)^);
            end;
          end;
        else
          TRAIIHelper<T>.Clear(Item);
        end;
        {$else}
        TRAIIHelper<T>.Clear(Item);
        {$endif}

        Exit;
      end else
      begin
        Count := 0;
      end;
    until (False);
  end else
  begin
    Result := Self.InternalDequeue(cnRemoved);
  end;
end;

function TQueue<T>.Extract: T;
var
  Count: NativeInt;
  Item: PItem;
  VType: Integer;
begin
  Count := FCount.Native;
  if (Count <> 0) and (not Assigned(FInternalNotify)) then
  begin
    FCount.Native := Count - 1;
    Count := FTail;
    Item := @FItems[Count];
    Inc(Count);
    repeat
      if (Count <> FCapacity.Native) then
      begin
        FTail := Count;
        Result := Item^;

        {$ifdef SMARTGENERICS}
        case GetTypeKind(T) of
          {$ifdef AUTOREFCOUNT}
          tkClass,
          {$endif}
          tkWString, tkLString, tkUString, tkInterface, tkDynArray:
          begin
            if (PNativeInt(Item)^ <> 0) then
            case GetTypeKind(T) of
              {$ifdef AUTOREFCOUNT}
              tkClass:
              begin
                TRAIIHelper.RefObjClear(Item);
              end;
              {$endif}
              {$ifdef MSWINDOWS}
              tkWString:
              begin
                TRAIIHelper.WStrClear(Item);
              end;
              {$else}
              tkWString,
              {$endif}
              tkLString, tkUString:
              begin
                TRAIIHelper.ULStrClear(Item);
              end;
              tkInterface:
              begin
                IInterface(PPointer(Item)^)._Release;
              end;
              tkDynArray:
              begin
                TRAIIHelper.DynArrayClear(Item, TypeInfo(T));
              end;
            end;
          end;
          {$ifdef WEAKINSTREF}
          tkMethod:
          begin
            if (PMethod(Item).Data <> nil) then
              TRAIIHelper.WeakMethodClear(@PMethod(Item).Data);
          end;
          {$endif}
          tkVariant:
          begin
            VType := PVarData(Item).VType;
            if (VType and TRAIIHelper.varDeepData <> 0) then
            case VType of
              varBoolean, varUnknown+1..varUInt64: ;
            else
              System.VarClear(PVariant(Item)^);
            end;
          end;
        else
          TRAIIHelper<T>.Clear(Item);
        end;
        {$else}
        TRAIIHelper<T>.Clear(Item);
        {$endif}

        Exit;
      end else
      begin
        Count := 0;
      end;
    until (False);
  end else
  begin
    Result := Self.InternalDequeue(cnExtracted);
  end;
end;

function TQueue<T>.Peek: T;
begin
  if (FCount.Native <> 0) then
  begin
    Result := FItems[FTail];
    Exit;
  end else
  begin
    raise Self.EmptyException;
  end;
end;


{ TThreadList<T> }

procedure TThreadList<T>.Add(const Item: T);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
       (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      raise EListError.CreateFmt(SDuplicateItem, [FList.ItemValue(Item)]);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

constructor TThreadList<T>.Create;
begin
  inherited Create;
  FLock := TObject.Create;
  FList := TList<T>.Create;
  FDuplicates := dupIgnore;
end;

destructor TThreadList<T>.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

function TThreadList<T>.LockList: TList<T>;
begin
  TMonitor.Enter(FLock);
  Result := FList;
end;

procedure TThreadList<T>.Remove(const Item: T);
begin
  RemoveItem(Item, TDirection.FromBeginning);
end;

procedure TThreadList<T>.RemoveItem(const Item: T; Direction: TDirection);
begin
  LockList;
  try
    FList.RemoveItem(Item, Direction);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.UnlockList;
begin
  TMonitor.Exit(FLock);
end;

{ TThreadedQueue<T> }
{$if CompilerVersion >= 22}
constructor TThreadedQueue<T>.Create(AQueueDepth: Integer = 10; PushTimeout: LongWord = INFINITE; PopTimeout: LongWord = INFINITE);
begin
  inherited Create;
  SetLength(FQueue, AQueueDepth);
  FQueueLock := TObject.Create;
  FQueueNotEmpty := TObject.Create;
  FQueueNotFull := TObject.Create;
  FPushTimeout := PushTimeout;
  FPopTimeout := PopTimeout;
end;

destructor TThreadedQueue<T>.Destroy;
begin
  DoShutDown;
  FQueueNotFull.Free;
  FQueueNotEmpty.Free;
  FQueueLock.Free;
  inherited;
end;

procedure TThreadedQueue<T>.Grow(ADelta: Integer);
begin
  TMonitor.Enter(FQueueLock);
  try
    SetLength(FQueue, Length(FQueue) + ADelta);
  finally
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.PulseAll(FQueueNotFull);
end;

function TThreadedQueue<T>.PopItem: T;
var
  LQueueSize: Integer;
begin
  PopItem(LQueueSize, Result);
end;

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult;
begin
  AItem := Default(T);
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = 0) and not FShutDown do
      if not TMonitor.Wait(FQueueNotEmpty, FQueueLock, FPopTimeout) then
        Result := wrTimeout;

    if (FShutDown and (FQueueSize = 0)) or (Result <> wrSignaled) then
      Exit;

    AItem := FQueue[FQueueOffset];

    FQueue[FQueueOffset] := Default(T);

    Dec(FQueueSize);
    Inc(FQueueOffset);
    Inc(FTotalItemsPopped);

    if FQueueOffset = Length(FQueue) then
      FQueueOffset := 0;

  finally
    AQueueSize := FQueueSize;
    TMonitor.Exit(FQueueLock);
  end;

  TMonitor.Pulse(FQueueNotFull);
end;

function TThreadedQueue<T>.PopItem(var AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PopItem(LQueueSize, AItem);
end;

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer): T;
begin
  PopItem(AQueueSize, Result);
end;

function TThreadedQueue<T>.PushItem(const AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PushItem(AItem, LQueueSize);
end;

function TThreadedQueue<T>.PushItem(const AItem: T; var AQueueSize: Integer): TWaitResult;
begin
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = Length(FQueue)) and not FShutDown do
      if not TMonitor.Wait(FQueueNotFull, FQueueLock, FPushTimeout) then
        Result := wrTimeout;

    if FShutDown or (Result <> wrSignaled) then
      Exit;

    FQueue[(FQueueOffset + FQueueSize) mod Length(FQueue)] := AItem;
    Inc(FQueueSize);
    Inc(FTotalItemsPushed);

  finally
    AQueueSize := FQueueSize;
    TMonitor.Exit(FQueueLock);
  end;

  TMonitor.Pulse(FQueueNotEmpty);
end;

procedure TThreadedQueue<T>.DoShutDown;
begin
  TMonitor.Enter(FQueueLock);
  try
    FShutDown := True;
  finally
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.PulseAll(FQueueNotFull);
  TMonitor.PulseAll(FQueueNotEmpty);
end;
{$ifend}


{ TObjectList<T> }

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;
  inherited Create(AComparer);
end;

constructor TObjectList<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;
  inherited Create(Collection);
end;

procedure TObjectList<T>.SetOwnsObjects(const Value: Boolean);
begin
  if (FOwnsObjects <> Value) then
  begin
    FOwnsObjects := Value;
    SetNotifyMethods;
  end;
end;

procedure TObjectList<T>.DisposeNotifyEvent(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
begin
  Self.FOnNotify(Sender, Item, Action);
  if (Action = cnRemoved) then
    Item.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectList<T>.DisposeOnly(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Item.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectList<T>.SetNotifyMethods;
var
  VMTNotify: procedure(const Item: T; Action: TCollectionNotification) of object;
begin
  if (not FOwnsObjects) then
  begin
    inherited;
    Exit;
  end;

  TMethod(FInternalNotify).Data := Pointer(Self);
  VMTNotify := Self.Notify;
  if (TMethod(VMTNotify).Code <> @TCustomList<T>.Notify) then
  begin
    TMethod(FInternalNotify).Code := @TCustomList<T>.NotifyCaller;
  end else
  if (Assigned(Self.FOnNotify)) then
  begin
    TMethod(FInternalNotify).Code := @TObjectList<T>.DisposeNotifyEvent;
  end else
  begin
    TMethod(FInternalNotify).Code := @TObjectList<T>.DisposeOnly;
  end;
end;


{ TObjectStack<T> }

constructor TObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;
  inherited Create;
end;

constructor TObjectStack<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;
  inherited Create(Collection);
end;

procedure TObjectStack<T>.Pop;
begin
  inherited Pop;
end;

procedure TObjectStack<T>.SetOwnsObjects(const Value: Boolean);
begin
  if (FOwnsObjects <> Value) then
  begin
    FOwnsObjects := Value;
    SetNotifyMethods;
  end;
end;

procedure TObjectStack<T>.DisposeNotifyEvent(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
begin
  Self.FOnNotify(Sender, Item, Action);
  if (Action = cnRemoved) then
    Item.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectStack<T>.DisposeOnly(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Item.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectStack<T>.SetNotifyMethods;
var
  VMTNotify: procedure(const Item: T; Action: TCollectionNotification) of object;
begin
  if (not FOwnsObjects) then
  begin
    inherited;
    Exit;
  end;

  TMethod(FInternalNotify).Data := Pointer(Self);
  VMTNotify := Self.Notify;
  if (TMethod(VMTNotify).Code <> @TCustomList<T>.Notify) then
  begin
    TMethod(FInternalNotify).Code := @TCustomList<T>.NotifyCaller;
  end else
  if (Assigned(Self.FOnNotify)) then
  begin
    TMethod(FInternalNotify).Code := @TObjectStack<T>.DisposeNotifyEvent;
  end else
  begin
    TMethod(FInternalNotify).Code := @TObjectStack<T>.DisposeOnly;
  end;
end;


{ TObjectQueue<T> }

constructor TObjectQueue<T>.Create(AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;
  inherited Create;
end;

constructor TObjectQueue<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;
  inherited Create(Collection);
end;

procedure TObjectQueue<T>.Dequeue;
begin
  inherited Dequeue;
end;

procedure TObjectQueue<T>.SetOwnsObjects(const Value: Boolean);
begin
  if (FOwnsObjects <> Value) then
  begin
    FOwnsObjects := Value;
    SetNotifyMethods;
  end;
end;

procedure TObjectQueue<T>.DisposeNotifyEvent(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
begin
  Self.FOnNotify(Sender, Item, Action);
  if (Action = cnRemoved) then
    Item.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectQueue<T>.DisposeOnly(Sender: TObject; const Item: TObject; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Item.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectQueue<T>.SetNotifyMethods;
var
  VMTNotify: procedure(const Item: T; Action: TCollectionNotification) of object;
begin
  if (not FOwnsObjects) then
  begin
    inherited;
    Exit;
  end;

  TMethod(FInternalNotify).Data := Pointer(Self);
  VMTNotify := Self.Notify;
  if (TMethod(VMTNotify).Code <> @TCustomList<T>.Notify) then
  begin
    TMethod(FInternalNotify).Code := @TCustomList<T>.NotifyCaller;
  end else
  if (Assigned(Self.FOnNotify)) then
  begin
    TMethod(FInternalNotify).Code := @TObjectQueue<T>.DisposeNotifyEvent;
  end else
  begin
    TMethod(FInternalNotify).Code := @TObjectQueue<T>.DisposeOnly;
  end;
end;


{ TObjectDictionary<TKey,TValue> }

constructor TObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships;
  ACapacity: Integer = 0);
begin
  Create(Ownerships, ACapacity, nil);
end;

constructor TObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships;
  const AComparer: IEqualityComparer<TKey>);
begin
  Create(Ownerships, 0, AComparer);
end;

constructor TObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships;
  ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
begin
  FOwnerships := Ownerships;
  if (Ownerships = []) then
    raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));

  if (doOwnsKeys in Ownerships) then
  begin
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));
  end;

  if (doOwnsValues in Ownerships) then
  begin
    if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));
  end;

  inherited Create(ACapacity, AComparer);
end;

procedure TObjectDictionary<TKey,TValue>.DisposeKeyEvent(Sender: TObject;
  const Key: TObject; Action: TCollectionNotification);
begin
  TOnKeyNotify(TMethod(FOnKeyNotify).Code)(TMethod(FOnKeyNotify).Data, Self, Key, Action);
  if (Action = cnRemoved) then
    Key.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectDictionary<TKey,TValue>.DisposeKeyOnly(Sender: TObject;
  const Key: TObject; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Key.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectDictionary<TKey,TValue>.DisposeValueEvent(Sender: TObject;
  const Value: TObject; Action: TCollectionNotification);
begin
  TOnValueNotify(TMethod(FOnValueNotify).Code)(TMethod(FOnValueNotify).Data, Self, Value, Action);
  if (Action = cnRemoved) then
    Value.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectDictionary<TKey,TValue>.DisposeValueOnly(Sender: TObject;
  const Value: TObject; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Value.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyCaller(const Item: TItem;
  Action: TCollectionNotification);
begin
  Self.KeyNotify(Item.Key, Action);
end;

procedure TObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyEvent(const Item: TItem;
  Action: TCollectionNotification);
begin
  FOnKeyNotify(Self, Item.Key, Action);
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Key)^).Free;
end;

procedure TObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyOnly(const Item: TItem;
  Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Key)^).Free;
end;

procedure TObjectDictionary<TKey,TValue>.DisposeItemNotifyValueCaller(const Item: TItem;
  Action: TCollectionNotification);
begin
  Self.ValueNotify(Item.Value, Action);
end;

procedure TObjectDictionary<TKey,TValue>.DisposeItemNotifyValueEvent(const Item: TItem;
  Action: TCollectionNotification);
begin
  FOnValueNotify(Self, Item.Value, Action);
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Value)^).Free;
end;

procedure TObjectDictionary<TKey,TValue>.DisposeItemNotifyValueOnly(const Item: TItem;
  Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Value)^).Free;
end;

procedure TObjectDictionary<TKey,TValue>.SetNotifyMethods;
var
  VMTKeyNotify: procedure(const Key: TKey; Action: TCollectionNotification) of object;
  VMTValueNotify: procedure(const Value: TValue; Action: TCollectionNotification) of object;
begin
  // FInternalKeyNotify
  TMethod(FInternalKeyNotify).Data := Pointer(Self);
  VMTKeyNotify := Self.KeyNotify;
  if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) then
  begin
    TMethod(FInternalKeyNotify).Code := @TCustomDictionary<TKey,TValue>.KeyNotifyCaller;
  end else
  if (doOwnsKeys in FOwnerships) then
  begin
    if (Assigned(Self.FOnKeyNotify)) then
    begin
      TMethod(FInternalKeyNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeValueEvent;
    end else
    begin
      TMethod(FInternalKeyNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeValueOnly;
    end;
  end else
  begin
    TMethod(FInternalKeyNotify) := TMethod(Self.FOnKeyNotify);
  end;

  // FInternalValueNotify
  TMethod(FInternalValueNotify).Data := Pointer(Self);
  VMTValueNotify := Self.ValueNotify;
  if (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
  begin
    TMethod(FInternalValueNotify).Code := @TCustomDictionary<TKey,TValue>.ValueNotifyCaller;
  end else
  if (doOwnsValues in FOwnerships) then
  begin
    if (Assigned(Self.FOnValueNotify)) then
    begin
      TMethod(FInternalValueNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeValueEvent;
    end else
    begin
      TMethod(FInternalValueNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeValueOnly;
    end;
  end else
  begin
    TMethod(FInternalValueNotify) := TMethod(Self.FOnValueNotify);
  end;

  // FInternalItemNotify
  TMethod(FInternalItemNotify).Data := Self;
  if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) and
    (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
  begin
    TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyCaller;
  end else
  if (Assigned(FInternalKeyNotify)) and (Assigned(FInternalValueNotify)) then
  begin
    TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyEvents;
  end else
  if (Assigned(FInternalKeyNotify)) then
  begin
    if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) then
    begin
      TMethod(FInternalItemNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyCaller;
    end else
    if (Assigned(Self.FOnKeyNotify)) then
    begin
      TMethod(FInternalItemNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyEvent;
    end else
    begin
      TMethod(FInternalItemNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyOnly;
    end;
  end else
  // if (Assigned(FInternalValueNotify)) then
  begin
    if (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
    begin
      TMethod(FInternalItemNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeItemNotifyValueCaller;
    end else
    if (Assigned(Self.FOnValueNotify)) then
    begin
      TMethod(FInternalItemNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeItemNotifyValueEvent;
    end else
    begin
      TMethod(FInternalItemNotify).Code := @TObjectDictionary<TKey,TValue>.DisposeItemNotifyValueOnly;
    end;
  end;
end;


{ TRapidObjectDictionary<TKey,TValue> }

constructor TRapidObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships;
  ACapacity: Integer);
begin
  FOwnerships := Ownerships;
  if (Ownerships = []) then
    raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));

  if (doOwnsKeys in Ownerships) then
  begin
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));
  end;

  if (doOwnsValues in Ownerships) then
  begin
    if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));
  end;

  inherited Create(ACapacity);
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeKeyEvent(Sender: TObject;
  const Key: TObject; Action: TCollectionNotification);
begin
  TOnKeyNotify(TMethod(FOnKeyNotify).Code)(TMethod(FOnKeyNotify).Data, Self, Key, Action);
  if (Action = cnRemoved) then
    Key.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeKeyOnly(Sender: TObject;
  const Key: TObject; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Key.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeValueEvent(Sender: TObject;
  const Value: TObject; Action: TCollectionNotification);
begin
  TOnValueNotify(TMethod(FOnValueNotify).Code)(TMethod(FOnValueNotify).Data, Self, Value, Action);
  if (Action = cnRemoved) then
    Value.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeValueOnly(Sender: TObject;
  const Value: TObject; Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    Value.{$ifdef NEXTGEN}DisposeOf{$else}Free{$endif};
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyCaller(const Item: TItem;
  Action: TCollectionNotification);
begin
  Self.KeyNotify(Item.Key, Action);
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyEvent(const Item: TItem;
  Action: TCollectionNotification);
begin
  FOnKeyNotify(Self, Item.Key, Action);
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Key)^).Free;
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyOnly(const Item: TItem;
  Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Key)^).Free;
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyValueCaller(const Item: TItem;
  Action: TCollectionNotification);
begin
  Self.ValueNotify(Item.Value, Action);
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyValueEvent(const Item: TItem;
  Action: TCollectionNotification);
begin
  FOnValueNotify(Self, Item.Value, Action);
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Value)^).Free;
end;

procedure TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyValueOnly(const Item: TItem;
  Action: TCollectionNotification);
begin
  if (Action = cnRemoved) then
    TObject(Pointer(@Item.Value)^).Free;
end;

procedure TRapidObjectDictionary<TKey,TValue>.SetNotifyMethods;
var
  VMTKeyNotify: procedure(const Key: TKey; Action: TCollectionNotification) of object;
  VMTValueNotify: procedure(const Value: TValue; Action: TCollectionNotification) of object;
begin
  // FInternalKeyNotify
  TMethod(FInternalKeyNotify).Data := Pointer(Self);
  VMTKeyNotify := Self.KeyNotify;
  if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) then
  begin
    TMethod(FInternalKeyNotify).Code := @TCustomDictionary<TKey,TValue>.KeyNotifyCaller;
  end else
  if (doOwnsKeys in FOwnerships) then
  begin
    if (Assigned(Self.FOnKeyNotify)) then
    begin
      TMethod(FInternalKeyNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeValueEvent;
    end else
    begin
      TMethod(FInternalKeyNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeValueOnly;
    end;
  end else
  begin
    TMethod(FInternalKeyNotify) := TMethod(Self.FOnKeyNotify);
  end;

  // FInternalValueNotify
  TMethod(FInternalValueNotify).Data := Pointer(Self);
  VMTValueNotify := Self.ValueNotify;
  if (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
  begin
    TMethod(FInternalValueNotify).Code := @TCustomDictionary<TKey,TValue>.ValueNotifyCaller;
  end else
  if (doOwnsValues in FOwnerships) then
  begin
    if (Assigned(Self.FOnValueNotify)) then
    begin
      TMethod(FInternalValueNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeValueEvent;
    end else
    begin
      TMethod(FInternalValueNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeValueOnly;
    end;
  end else
  begin
    TMethod(FInternalValueNotify) := TMethod(Self.FOnValueNotify);
  end;

  // FInternalItemNotify
  TMethod(FInternalItemNotify).Data := Self;
  if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) and
    (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
  begin
    TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyCaller;
  end else
  if (Assigned(FInternalKeyNotify)) and (Assigned(FInternalValueNotify)) then
  begin
    TMethod(FInternalItemNotify).Code := @TCustomDictionary<TKey,TValue>.ItemNotifyEvents;
  end else
  if (Assigned(FInternalKeyNotify)) then
  begin
    if (TMethod(VMTKeyNotify).Code <> @TCustomDictionary<TKey,TValue>.KeyNotify) then
    begin
      TMethod(FInternalItemNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyCaller;
    end else
    if (Assigned(Self.FOnKeyNotify)) then
    begin
      TMethod(FInternalItemNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyEvent;
    end else
    begin
      TMethod(FInternalItemNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyKeyOnly;
    end;
  end else
  // if (Assigned(FInternalValueNotify)) then
  begin
    if (TMethod(VMTValueNotify).Code <> @TCustomDictionary<TKey,TValue>.ValueNotify) then
    begin
      TMethod(FInternalItemNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyValueCaller;
    end else
    if (Assigned(Self.FOnValueNotify)) then
    begin
      TMethod(FInternalItemNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyValueEvent;
    end else
    begin
      TMethod(FInternalItemNotify).Code := @TRapidObjectDictionary<TKey,TValue>.DisposeItemNotifyValueOnly;
    end;
  end;
end;

{ TSyncYield }

class function TSyncYield.Create: TSyncYield;
begin
  Result.FCount := 0;
end;

procedure TSyncYield.Reset;
begin
  Self.FCount := 0;
end;

procedure TSyncYield.Execute;
var
  LCount: Integer;
begin
  LCount := FCount;
  Inc(LCount);
  FCount := LCount;
  Dec(LCount);

  case (LCount and 7) of
    0..4: System.YieldProcessor;
    5, 6:
    begin
      {$ifdef MSWINDOWS}
        SwitchToThread;
      {$else .POSIX}
        sched_yield;
      {$endif}
    end;
  else
    Sleep(1);
  end;
end;


{ TSyncSpinlock }

class function TSyncSpinlock.Create: TSyncSpinlock;
begin
  Result.FValue := 0;
end;

function TSyncSpinlock.GetLocked: Boolean;
begin
  Result := (FValue <> 0);
end;

function TSyncSpinlock.TryEnter: Boolean;
{$ifdef CPUINTELASM}
asm
  {$ifdef CPUX86}
  xchg eax, ecx
  {$endif}
  mov edx, 1
  xor eax, eax

  {$ifdef CPUX86}
    cmp byte ptr [ECX].TSyncSpinlock.FValue, 0
    jne @done
    lock xchg byte ptr [ECX].TSyncSpinlock.FValue, dl
  {$else .CPUX64}
    cmp byte ptr [RCX].TSyncSpinlock.FValue, 0
    jne @done
    lock xchg byte ptr [RCX].TSyncSpinlock.FValue, dl
  {$endif}
@done:
  sete al
end;
{$else .NEXTGEN}
begin
  Result := (FValue = 0) and
    (AtomicCmpExchange(FValue, 1, 0) = 0);
end;
{$endif}

function TSyncSpinlock.Enter(const ATimeout: Cardinal): Boolean;
var
  Yield: TSyncYield;
  Timeout, TimeStart, TimeFinish, TimeDelta: Cardinal;
begin
  case (ATimeout) of
    0:
    begin
      Result := TryEnter;
    end;
    INFINITE:
    begin
      Enter;
      Result := True;
    end;
  else
    Timeout := ATimeout;
    Yield := TSyncYield.Create;
    TimeStart := TOSTime.TickCount;
    repeat
      Result := TryEnter;
      if (Result) then
        Exit;

      TimeFinish := TOSTime.TickCount;
      TimeDelta := TimeFinish - TimeStart;
      if (TimeDelta >= Timeout) then
        Break;
      Dec(Timeout, TimeDelta);
      TimeStart := TimeFinish;

      Yield.Execute;
    until (False);

    Result := False;
  end;
end;

procedure TSyncSpinlock.Enter;
var
  Yield: TSyncYield;
begin
  if (not TryEnter) then
  begin
    Yield := TSyncYield.Create;
    repeat
      Yield.Execute;
    until (TryEnter);
  end;
end;

procedure TSyncSpinlock.Leave;
begin
  FValue := 0;
end;


{ TSyncLocker }

class function TSyncLocker.Create: TSyncLocker;
begin
  Result.FValue := 0;
end;

function TSyncLocker.GetLocked: Boolean;
begin
  Result := (FValue <> 0);
end;

function TSyncLocker.GetLockedRead: Boolean;
var
  LValue: Integer;
begin
  LValue := FValue;
  Result := (LValue <> 0) and (LValue and 1 = 0);
end;

function TSyncLocker.GetLockedExclusive: Boolean;
begin
  Result := (FValue and 1 <> 0);
end;

function TSyncLocker.TryEnterRead: Boolean;
var
  LValue: Integer;
begin
  LValue := FValue;
  if (LValue and 1 = 0) then
  begin
    LValue := AtomicIncrement(FValue, 2);
    if (LValue and 1 = 0) then
    begin
      Result := True;
      Exit;
    end else
    begin
      AtomicDecrement(FValue, 2)
    end;
  end;

  Result := False;
end;

function TSyncLocker.TryEnterExclusive: Boolean;
var
  LValue: Integer;
  Yield: TSyncYield;
begin
  repeat
    LValue := FValue;
    if (LValue and 1 <> 0) then
      Break;

    if (AtomicCmpExchange(FValue, LValue + 1, LValue) = LValue) then
    begin
      Yield := TSyncYield.Create;

      repeat
        if (FValue and -2 = 0) then
          Break;

        Yield.Execute;
      until (False);

      Result := True;
      Exit;
    end;
  until (False);

  Result := False;
end;

function TSyncLocker.EnterRead(const ATimeout: Cardinal): Boolean;
var
  Yield: TSyncYield;
  Timeout, TimeStart, TimeFinish, TimeDelta: Cardinal;
begin
  case (ATimeout) of
    0:
    begin
      Result := TryEnterRead;
    end;
    INFINITE:
    begin
      EnterRead;
      Result := True;
    end;
  else
    Timeout := ATimeout;
    Yield := TSyncYield.Create;
    TimeStart := TOSTime.TickCount;
    repeat
      Result := TryEnterRead;
      if (Result) then
        Exit;

      TimeFinish := TOSTime.TickCount;
      TimeDelta := TimeFinish - TimeStart;
      if (TimeDelta >= Timeout) then
        Break;
      Dec(Timeout, TimeDelta);
      TimeStart := TimeFinish;

      Yield.Execute;
    until (False);

    Result := False;
  end;
end;

function TSyncLocker.EnterExclusive(const ATimeout: Cardinal): Boolean;
var
  Yield: TSyncYield;
  Timeout, TimeStart, TimeFinish, TimeDelta: Cardinal;
begin
  case (ATimeout) of
    0:
    begin
      Result := TryEnterExclusive;
    end;
    INFINITE:
    begin
      EnterExclusive;
      Result := True;
    end;
  else
    Timeout := ATimeout;
    Yield := TSyncYield.Create;
    TimeStart := TOSTime.TickCount;
    repeat
      Result := TryEnterExclusive;
      if (Result) then
        Exit;

      TimeFinish := TOSTime.TickCount;
      TimeDelta := TimeFinish - TimeStart;
      if (TimeDelta >= Timeout) then
        Break;
      Dec(Timeout, TimeDelta);
      TimeStart := TimeFinish;

      Yield.Execute;
    until (False);

    Result := False;
  end;
end;

procedure TSyncLocker.EnterRead;
var
  Yield: TSyncYield;
begin
  if (not TryEnterRead) then
  begin
    Yield := TSyncYield.Create;
    repeat
      Yield.Execute;
    until (TryEnterRead);
  end;
end;

procedure TSyncLocker.EnterExclusive;
var
  Yield: TSyncYield;
begin
  if (not TryEnterExclusive) then
  begin
    Yield := TSyncYield.Create;
    repeat
      Yield.Execute;
    until (TryEnterExclusive);
  end;
end;

procedure TSyncLocker.LeaveRead;
begin
  AtomicDecrement(FValue, 2);
end;

procedure TSyncLocker.LeaveExclusive;
begin
  AtomicDecrement(FValue, 1);
end;


{ TSyncSmallLocker }

class function TSyncSmallLocker.Create: TSyncSmallLocker;
begin
  Result.FValue := 0;
end;

function TSyncSmallLocker.GetLocked: Boolean;
begin
  Result := (FValue <> 0);
end;

function TSyncSmallLocker.GetLockedRead: Boolean;
var
  LValue: Integer;
begin
  LValue := FValue;
  Result := (LValue <> 0) and (LValue and 1 = 0);
end;

function TSyncSmallLocker.GetLockedExclusive: Boolean;
begin
  Result := (FValue and 1 <> 0);
end;

class function TSyncSmallLocker.InternalCAS(var AValue: Byte; const NewValue, Comparand: Byte): Boolean;
{$ifdef CPUINTELASM}
asm
  {$ifdef CPUX86}
    xchg eax, ecx
    cmp byte ptr [ECX].TSyncSpinlock.FValue, al
    jne @done
    lock xchg byte ptr [ECX].TSyncSpinlock.FValue, dl
  {$else .CPUX64}
    xchg rax, r8
    cmp byte ptr [RCX].TSyncSpinlock.FValue, al
    jne @done
    lock xchg byte ptr [RCX].TSyncSpinlock.FValue, dl
  {$endif}
@done:
  sete al
end;
{$else .NEXTGEN}
begin
  Result := (AValue = Comparand) and (AtomicCmpExchange(AValue, NewValue, Comparand) = Comparand);
end;
{$endif}

function TSyncSmallLocker.TryEnterRead: Boolean;
var
  LValue: Integer;
begin
  repeat
    LValue := FValue;
    if (LValue and 1 <> 0) or (LValue = 254) then
      Break;

    if (InternalCAS(FValue, LValue + 2, LValue)) then
    begin
      Result := True;
      Exit;
    end;
  until (False);

  Result := False;
end;

function TSyncSmallLocker.TryEnterExclusive: Boolean;
var
  LValue: Integer;
  Yield: TSyncYield;
begin
  repeat
    LValue := FValue;
    if (LValue and 1 <> 0) then
      Break;

    if (InternalCAS(FValue, LValue + 1, LValue)) then
    begin
      Yield := TSyncYield.Create;

      repeat
        if (FValue and -2 = 0) then
          Break;

        Yield.Execute;
      until (False);

      Result := True;
      Exit;
    end;
  until (False);

  Result := False;
end;

function TSyncSmallLocker.EnterRead(const ATimeout: Cardinal): Boolean;
var
  Yield: TSyncYield;
  Timeout, TimeStart, TimeFinish, TimeDelta: Cardinal;
begin
  case (ATimeout) of
    0:
    begin
      Result := TryEnterRead;
    end;
    INFINITE:
    begin
      EnterRead;
      Result := True;
    end;
  else
    Timeout := ATimeout;
    Yield := TSyncYield.Create;
    TimeStart := TOSTime.TickCount;
    repeat
      Result := TryEnterRead;
      if (Result) then
        Exit;

      TimeFinish := TOSTime.TickCount;
      TimeDelta := TimeFinish - TimeStart;
      if (TimeDelta >= Timeout) then
        Break;
      Dec(Timeout, TimeDelta);
      TimeStart := TimeFinish;

      Yield.Execute;
    until (False);

    Result := False;
  end;
end;

function TSyncSmallLocker.EnterExclusive(const ATimeout: Cardinal): Boolean;
var
  Yield: TSyncYield;
  Timeout, TimeStart, TimeFinish, TimeDelta: Cardinal;
begin
  case (ATimeout) of
    0:
    begin
      Result := TryEnterExclusive;
    end;
    INFINITE:
    begin
      EnterExclusive;
      Result := True;
    end;
  else
    Timeout := ATimeout;
    Yield := TSyncYield.Create;
    TimeStart := TOSTime.TickCount;
    repeat
      Result := TryEnterExclusive;
      if (Result) then
        Exit;

      TimeFinish := TOSTime.TickCount;
      TimeDelta := TimeFinish - TimeStart;
      if (TimeDelta >= Timeout) then
        Break;
      Dec(Timeout, TimeDelta);
      TimeStart := TimeFinish;

      Yield.Execute;
    until (False);

    Result := False;
  end;
end;

procedure TSyncSmallLocker.EnterRead;
var
  Yield: TSyncYield;
begin
  if (not TryEnterRead) then
  begin
    Yield := TSyncYield.Create;
    repeat
      Yield.Execute;
    until (TryEnterRead);
  end;
end;

procedure TSyncSmallLocker.EnterExclusive;
var
  Yield: TSyncYield;
begin
  if (not TryEnterExclusive) then
  begin
    Yield := TSyncYield.Create;
    repeat
      Yield.Execute;
    until (TryEnterExclusive);
  end;
end;

procedure TSyncSmallLocker.LeaveRead;
{$ifdef CPUINTELASM}
asm
  or edx, -2
  {$ifdef CPUX86}
  lock xadd [EAX].FValue, dl
  {$else .CPUARM}
  lock xadd [RCX].FValue, dl
  {$endif}
end;
{$else .NEXTGEN}
begin
  AtomicDecrement(FValue, 2);
end;
{$endif}

procedure TSyncSmallLocker.LeaveExclusive;
{$ifdef CPUINTELASM}
asm
  or edx, -1
  {$ifdef CPUX86}
  lock xadd [EAX].FValue, dl
  {$else .CPUARM}
  lock xadd [RCX].FValue, dl
  {$endif}
end;
{$else .NEXTGEN}
begin
  AtomicDecrement(FValue, 1);
end;
{$endif}

initialization
  TOSTime.Initialize;
  TCustomObject.CreateIntfTables;
  TLiteCustomObject.CreateIntfTables;
end.


