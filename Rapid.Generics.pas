unit Rapid.Generics;

{******************************************************************************}
{ Copyright (c) 2017 Dmitry Mozulyov                                           }
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
{$else}
  {$ifNdef UNICODE}
    {$MESSAGE ERROR 'Old version compiler not supported'} // CompilerVersion < 20
  {$endif}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$if CompilerVersion >= 24}
    {$LEGACYIFEND ON}
  {$ifend}
  {$if CompilerVersion < 23}
    {$define CPUX86}
  {$ifend}
  {$if CompilerVersion >= 23}
    {$define UNITSCOPENAMES}
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
{$if Defined(CPUX86) or Defined(CPUX64)}
  {$define CPUINTEL}
{$ifend}
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
    {$ifdef WEAKREF}
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
  private
    FTypeInfo: PTypeInfo;
    FSize: NativeInt;

    // initialization/finalization
    procedure Include(AOffset: NativeInt; Value: PTypeInfo);
    procedure Initialize(Value: PTypeInfo);
    class function IsManagedTypeInfo(Value: PTypeInfo): Boolean; static;
    class procedure InitsProcNativeSingle(const Self: TRAIIHelper; P: Pointer); static;
    class procedure InitsProcNatives(const Self: TRAIIHelper; P: Pointer); static;
    class procedure InitsProc(const Self: TRAIIHelper; P: Pointer); static;
    class procedure ClearsProcNativeSingle(const Self: TRAIIHelper; P: Pointer); static;
    class procedure ClearsProcNatives(const Self: TRAIIHelper; P: Pointer); static;
    class procedure ClearsProc(const Self: TRAIIHelper; P: Pointer); static;

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
    {$endif}
    {$ifdef WEAKREF}
    class procedure WeakMethodClear(P: Pointer); static;
    {$endif}
    {$ifdef WEAKINTFREF}
    class procedure WeakIntfClear(P: Pointer); static;
    {$endif}
  public
    {$ifdef WEAKREF}
      InitNatives: TInitNatives;
      ClearNatives: TClearNatives;
    {$else}
      Natives: TNatives;
    {$endif}
    StaticArrays: TStaticArrays;
    InitProc: procedure(const Self: TRAIIHelper; P: Pointer);
    ClearProc: procedure(const Self: TRAIIHelper; P: Pointer);

    property TypeInfo: PTypeInfo read FTypeInfo write Initialize;
    property Size: NativeInt read FSize;
  end;

  TRAIIHelper<T> = record
  public type
    P = ^T;
  private class var
    FOptions: TRAIIHelper;
  public
    class constructor Create;
    class property Options: TRAIIHelper read FOptions;
    class procedure Init(Value: P); static; inline;
    class procedure Clear(Value: P); static; inline;
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
      Instance: IComparerInst;
    public
      class constructor Create;
    end;
    TDefaultEqualityComparer<T> = record
    public class var
      Instance: IEqualityComparerInst;
    public
      class constructor Create;
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
  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  public
    class function Default: IComparer<T>;
    class function Construct(const Comparison: TComparison<T>): IComparer<T>;
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;

  TEqualityComparison<T> = reference to function(const Left, Right: T): Boolean;
  THasher<T> = reference to function(const Value: T): Integer;

  // Abstract base class for IEqualityComparer<T> implementations, and a provider
  // of default IEqualityComparer<T> implementations.
  TEqualityComparer<T> = class(TInterfacedObject, IEqualityComparer<T>)
  public
    class function Default: IEqualityComparer<T>; static;

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

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: Boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: Boolean;
  end;

  TEnumerable<T> = class abstract
  private
  {$HINTS OFF}
    function ToArrayImpl(Count: Integer): TArray<T>; // used by descendants
  {$HINTS ON}
  protected
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>;
    function ToArray: TArray<T>; virtual;
  end;

  TPair<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
    constructor Create(const AKey: TKey; const AValue: TValue);
  end;

  PObject = ^TObject;


{ TCustomDictionary<TKey,TValue> class }

  TCustomDictionary<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
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

    TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
    private
      FDictionary: TCustomDictionary<TKey,TValue>;
      FIndex: NativeInt;
      function GetCurrent: TPair<TKey,TValue>; inline;
    protected
      function DoGetCurrent: TPair<TKey,TValue>; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(const ADictionary: TCustomDictionary<TKey,TValue>);
      property Current: TPair<TKey,TValue> read GetCurrent;
      function MoveNext: Boolean; inline;
    end;

    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FDictionary: TCustomDictionary<TKey,TValue>;
      FIndex: NativeInt;
      function GetCurrent: TKey;
    protected
      function DoGetCurrent: TKey; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(const ADictionary: TCustomDictionary<TKey,TValue>);
      property Current: TKey read GetCurrent;
      function MoveNext: Boolean;
    end;

    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FDictionary: TCustomDictionary<TKey,TValue>;
      FIndex: NativeInt;
      function GetCurrent: TValue;
    protected
      function DoGetCurrent: TValue; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(const ADictionary: TCustomDictionary<TKey,TValue>);
      property Current: TValue read GetCurrent;
      function MoveNext: Boolean;
    end;

    TKeyCollection = class(TEnumerable<TKey>)
    private
      [Weak] FDictionary: TCustomDictionary<TKey,TValue>;
      function GetCount: Integer; inline;
    protected
      function DoGetEnumerator: TEnumerator<TKey>; override;
    public
      constructor Create(const ADictionary: TCustomDictionary<TKey,TValue>);
      function GetEnumerator: TKeyEnumerator; reintroduce;
      function ToArray: TArray<TKey>; override; final;
      property Count: Integer read GetCount;
    end;

    TValueCollection = class(TEnumerable<TValue>)
    private
      [Weak] FDictionary: TCustomDictionary<TKey,TValue>;
      function GetCount: Integer; inline;
    protected
      function DoGetEnumerator: TEnumerator<TValue>; override;
    public
      constructor Create(const ADictionary: TCustomDictionary<TKey,TValue>);
      function GetEnumerator: TValueEnumerator; reintroduce;
      function ToArray: TArray<TValue>; override; final;
      property Count: Integer read GetCount;
    end;
  protected
    FList: PItemList;
    FListCount: NativeInt;
    FHashTable: TArray<PItem>;
    FHashTableMask: NativeInt;
    FCount: packed record
    case Boolean of
     False: (Int: Integer);
      True: (Native: NativeInt);
    end;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;

    // rehash
    procedure Rehash(NewTableCount: NativeInt);
    procedure SetCapacity(ACapacity: NativeInt);
    function Grow: TCustomDictionary<TKey,TValue>;

    // items
    function NewItem: PItem;
    procedure DisposeItem(Item: PItem);
    procedure DoCleanupItems(Item: PItem; Count: NativeInt); virtual;

    // enumarators
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetKeys: TKeyCollection; inline;
    function GetValues: TValueCollection; inline;

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
    procedure SetNotifyMethods;
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

    function GetEnumerator: TPairEnumerator; reintroduce;
    function ToArray: TArray<TPair<TKey,TValue>>; override; final;

    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
  end;


{ TDictionary<TKey,TValue>
  System.Generics.Collections equivalent

  Included 3 members:
    function Find(const Key: TKey): PItem;
    function FindOrAdd(const Key: TKey): PItem;
    property List: PItemList read FList; }

  TDictionary<TKey,TValue> = class(TCustomDictionary<TKey,TValue>)
  public type
    TItem = TCustomDictionary<TKey,TValue>.TItem;
    PItem = TCustomDictionary<TKey,TValue>.PItem;
    PItemList = TCustomDictionary<TKey,TValue>.PItemList;
  protected
    FComparer: IEqualityComparer<TKey>;
    FComparerEquals: function(const Left, Right: TKey): Boolean of object;
    FComparerGetHashCode: function(const Value: TKey): Integer of object;

    function InternalFindItem(const Key: TKey; const FindMode: Integer): PItem;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;

    function Find(const Key: TKey): PItem;
    function FindOrAdd(const Key: TKey): PItem;
    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property List: PItemList read FList;
    property Count: Integer read FCount.Int;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write SetKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write SetValueNotify;
  end;


{ TRapidDictionary<TKey,TValue> class
  Rapid "inline" TDictionary equivalent with default hash code and comparer }

  TRapidDictionary<TKey,TValue> = class(TCustomDictionary<TKey,TValue>)
  public type
    TItem = TCustomDictionary<TKey,TValue>.TItem;
    PItem = TCustomDictionary<TKey,TValue>.PItem;
    PItemList = TCustomDictionary<TKey,TValue>.PItemList;
  protected
    {$ifNdef SMARTGENERICS}
    FDefaultComparer: IEqualityComparer<TKey>;
    FDefaultEquals: function(const Left, Right: TKey): Boolean of object;
    FDefaultGetHashCode: function(const Value: TKey): Integer of object;
    {$endif}
    function InternalFindItem(const Key: TKey; const FindMode: Integer): PItem;
    function GetItem(const Key: TKey): TValue; inline;
    procedure SetItem(const Key: TKey; const Value: TValue); inline;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey,TValue>>); overload;
    destructor Destroy; override;

    function Find(const Key: TKey): PItem; inline;
    function FindOrAdd(const Key: TKey): PItem; inline;
    procedure Add(const Key: TKey; const Value: TValue); inline;
    procedure Remove(const Key: TKey); inline;
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue); inline;
    function ContainsKey(const Key: TKey): Boolean; inline;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property List: PItemList read FList;
    property Count: NativeInt read FCount.Native;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write SetKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write SetValueNotify;
  end;


{ TArray class }

  TArray = class
  private
    class procedure QuickSort<T>(var Values: array of T; const Comparer: IComparer<T>;
      L, R: Integer); static;
    class procedure CheckArrays(Source, Destination: Pointer; SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt); static;
  public
    class procedure Sort<T>(var Values: array of T); overload; static;
    class procedure Sort<T>(var Values: array of T; const Comparer: IComparer<T>); overload; static;
    class procedure Sort<T>(var Values: array of T;
      const Comparer: IComparer<T>; Index, Count: Integer); overload; static;

    class function BinarySearch<T>(const Values: array of T; const Item: T;
      out FoundIndex: Integer; const Comparer: IComparer<T>;
      Index, Count: Integer): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T;
      out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean; overload; static;
    class function BinarySearch<T>(const Values: array of T; const Item: T;
      out FoundIndex: Integer): Boolean; overload; static; static;

    class procedure Copy<T>(const Source: array of T; var Destination: array of T; SourceIndex, DestIndex, Count: NativeInt); overload; static;
    class procedure Copy<T>(const Source: array of T; var Destination: array of T; Count: NativeInt); overload; static;
  end;


  [HPPGEN(HPPGenAttribute.mkFriend, 'DELPHICLASS TList__1<T>')]
  TListHelper = record
  private type
    TInternalNotifyEvent = procedure (const Item; Action: TCollectionNotification) of object;
    TInternalCompareEvent = function (const Left, Right): Integer of object;
    TInternalEmptyFunc = reference to function(const Item): Boolean;
    PInterface = ^IInterface;
    PBytes = ^TBytes;
  private var
    FCount: Integer;
    FTypeInfo: Pointer;
    FNotify: TInternalNotifyEvent;
    FCompare: TInternalCompareEvent;
    function GetFItems: PPointer; inline;
    function GetElType: Pointer; inline;
    function GetElSize: Integer; inline;
    function CheckDeleteRange(AIndex, ACount: Integer): Boolean; inline;
    procedure CheckItemRangeInline(AIndex: Integer); inline;
    procedure CheckInsertRange(AIndex: Integer); inline;
    procedure CheckItemRange(AIndex: Integer);
    function DoIndexOfFwd1(const Value): Integer;
    function DoIndexOfFwd2(const Value): Integer;
    function DoIndexOfFwd4(const Value): Integer;
    function DoIndexOfFwd8(const Value): Integer;
    function DoIndexOfFwdN(const Value): Integer;
    function DoIndexOfFwdMRef(const Value): Integer;
    function DoIndexOfRev1(const Value): Integer;
    function DoIndexOfRev2(const Value): Integer;
    function DoIndexOfRev4(const Value): Integer;
    function DoIndexOfRev8(const Value): Integer;
    function DoIndexOfRevN(const Value): Integer;
    function DoIndexOfRevMRef(const Value): Integer;
    procedure DoExtractItemFwd1(const Value; out Item);
    procedure DoExtractItemFwd2(const Value; out Item);
    procedure DoExtractItemFwd4(const Value; out Item);
    procedure DoExtractItemFwd8(const Value; out Item);
    procedure DoExtractItemFwdN(const Value; out Item);
    procedure DoExtractItemFwdString(const Value; out Item);
    procedure DoExtractItemFwdInterface(const Value; out Item);
    procedure DoExtractItemFwdVariant(const Value; out Item);
{$IF not Defined(NEXTGEN)}
    procedure DoExtractItemFwdAnsiString(const Value; out Item);
    procedure DoExtractItemFwdWideString(const Value; out Item);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoExtractItemFwdObject(const Value; out Item);
{$IFEND}
    procedure DoExtractItemFwdManaged(const Value; out Item);
    procedure DoExtractItemRev1(const Value; out Item);
    procedure DoExtractItemRev2(const Value; out Item);
    procedure DoExtractItemRev4(const Value; out Item);
    procedure DoExtractItemRev8(const Value; out Item);
    procedure DoExtractItemRevN(const Value; out Item);
    procedure DoExtractItemRevString(const Value; out Item);
    procedure DoExtractItemRevInterface(const Value; out Item);
    procedure DoExtractItemRevVariant(const Value; out Item);
{$IF not Defined(NEXTGEN)}
    procedure DoExtractItemRevAnsiString(const Value; out Item);
    procedure DoExtractItemRevWideString(const Value; out Item);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoExtractItemRevObject(const Value; out Item);
{$IFEND}
    procedure DoExtractItemRevManaged(const Value; out Item);
    procedure DoExchangeStringInline(Index1, Index2: Integer); inline;
    procedure DoExchangeInterfaceInline(Index1, Index2: Integer); inline;
    procedure DoExchangeVariantInline(Index1, Index2: Integer); inline;
    procedure DoExchangeDynArrayInline(Index1, Index2: Integer); inline;
{$IF not Defined(NEXTGEN)}
    procedure DoExchangeAnsiStringInline(Index1, Index2: Integer); inline;
    procedure DoExchangeWideStringInline(Index1, Index2: Integer); inline;
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoExchangeObjectInline(Index1, Index2: Integer); inline;
{$IFEND}
    procedure DoExchangeString(Index1, Index2: Integer);
    procedure DoExchangeInterface(Index1, Index2: Integer);
    procedure DoExchangeVariant(Index1, Index2: Integer);
    procedure DoExchangeDynArray(Index1, Index2: Integer);
{$IF not Defined(NEXTGEN)}
    procedure DoExchangeAnsiString(Index1, Index2: Integer);
    procedure DoExchangeWideString(Index1, Index2: Integer);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoExchangeObject(Index1, Index2: Integer);
{$IFEND}
    function DoRemoveFwd1(const Value): Integer;
    function DoRemoveFwd2(const Value): Integer;
    function DoRemoveFwd4(const Value): Integer;
    function DoRemoveFwd8(const Value): Integer;
    function DoRemoveFwdN(const Value): Integer;
    function DoRemoveFwdMRef(const Value): Integer;
    function DoRemoveRev1(const Value): Integer;
    function DoRemoveRev2(const Value): Integer;
    function DoRemoveRev4(const Value): Integer;
    function DoRemoveRev8(const Value): Integer;
    function DoRemoveRevN(const Value): Integer;
    function DoRemoveRevMRef(const Value): Integer;
    procedure SetItem1(const Value; AIndex: Integer);
    procedure SetItem2(const Value; AIndex: Integer);
    procedure SetItem4(const Value; AIndex: Integer);
    procedure SetItem8(const Value; AIndex: Integer);
    procedure SetItemManaged(const Value; AIndex: Integer);
    procedure SetItemN(const Value; AIndex: Integer);
{$IF Defined(AUTOREFCOUNT)}
    procedure DoInsertObject(AIndex: Integer; const Value);
    procedure DoSetItemObject(const Value; AIndex: Integer);
    function DoAddObject(const Value): Integer;
{$IFEND}
{$IF not Defined(NEXTGEN)}
    procedure DoInsertAnsiString(AIndex: Integer; const Value);
    procedure DoInsertWideString(AIndex: Integer; const Value);
    procedure DoSetItemAnsiString(const Value; AIndex: Integer);
    procedure DoSetItemWideString(const Value; AIndex: Integer);
    function DoAddAnsiString(const Value): Integer;
    function DoAddWideString(const Value): Integer;
{$IFEND}
    procedure DoInsertInterface(AIndex: Integer; const Value);
    procedure DoSetItemInterface(const Value; AIndex: Integer);
    procedure DoInsertString(AIndex: Integer; const Value);
    procedure DoSetItemString(const Value; AIndex: Integer);
    procedure DoInsertDynArray(AIndex: Integer; const Value);
    procedure DoSetItemDynArray(const Value; AIndex: Integer);
    procedure SetItemVariant(const Value; AIndex: Integer);
    procedure SetItemMRef(const Value; AIndex: Integer; TypeKind: TTypeKind); inline;
    function DoAddInterface(const Value): Integer;
    function DoAddString(const Value): Integer;
    function DoAddDynArray(const Value): Integer;
    procedure DoReverseMRef(Kind: TTypeKind); inline;
    procedure DoReverseString;
    procedure DoReverseInterface;
    procedure DoReverseVariant;
    procedure DoReverseDynArray;
{$IF not Defined(NEXTGEN)}
    procedure DoReverseAnsiString;
    procedure DoReverseWideString;
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure DoReverseObject;
{$IFEND}
    function InternalAdd1(const Value): Integer;
    function InternalAdd2(const Value): Integer;
    function InternalAdd4(const Value): Integer;
    function InternalAdd8(const Value): Integer;
    function InternalAddN(const Value): Integer;
    function InternalAddVariant(const Value): Integer;
    function InternalAddMRef(const Value; TypeKind: TTypeKind): Integer; inline;
    function InternalAddManaged(const Value): Integer;
    procedure InternalGrow(ANewCount: Integer);
    procedure InternalGrowCheck(ANewCount: Integer);
    procedure InternalDeleteRange1(AIndex, ACount: Integer);
    procedure InternalDeleteRange2(AIndex, ACount: Integer);
    procedure InternalDeleteRange4(AIndex, ACount: Integer);
    procedure InternalDeleteRange8(AIndex, ACount: Integer);
    procedure InternalDeleteRangeN(AIndex, ACount: Integer);
    procedure InternalDeleteRangeMRef(AIndex, ACount: Integer);
    procedure InternalDeleteRangeManaged(AIndex, ACount: Integer);
{$IF Defined(WEAKREF)}
    procedure InternalDeleteRangeWeak(AIndex, ACount: Integer);
{$IFEND}
    procedure InternalDoDelete1(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDelete2(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDelete4(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDelete8(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDeleteN(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDeleteMRef(AIndex: Integer; Action: TCollectionNotification);
    procedure InternalDoDeleteManaged(AIndex: Integer; Action: TCollectionNotification);
{$IF Defined(WEAKREF)}
    procedure InternalDoDeleteWeak(AIndex: Integer; Action: TCollectionNotification);
{$IFEND}
    procedure InternalSetCapacity(Value: NativeInt);
    procedure InternalSetCount1(Value: Integer);
    procedure InternalSetCount2(Value: Integer);
    procedure InternalSetCount4(Value: Integer);
    procedure InternalSetCount8(Value: Integer);
    procedure InternalSetCountN(Value: Integer);
    procedure InternalSetCountMRef(Value: Integer);
    procedure InternalSetCountManaged(Value: Integer);
{$IF Defined(WEAKREF)}
    procedure InternalSetCountWeak(Value: Integer);
{$IFEND}
    procedure InternalClear1;
    procedure InternalClear2;
    procedure InternalClear4;
    procedure InternalClear8;
    procedure InternalClearN;
    procedure InternalClearMRef;
    procedure InternalClearManaged;
{$IF Defined(WEAKREF)}
    procedure InternalClearWeak;
{$IFEND}
    procedure InternalInsert1(AIndex: Integer; const Value);
    procedure InternalInsert2(AIndex: Integer; const Value);
    procedure InternalInsert4(AIndex: Integer; const Value);
    procedure InternalInsert8(AIndex: Integer; const Value);
    procedure InternalInsertN(AIndex: Integer; const Value);
    procedure InternalInsertVariant(AIndex: Integer; const Value);
    procedure InternalInsertMRef(AIndex: Integer; const Value; TypeKind: TTypeKind); inline;
    procedure InternalInsertManaged(AIndex: Integer; const Value);
    procedure InternalInsertRange1(AIndex: Integer; Values: Pointer; ACount: Integer);
    procedure InternalInsertRange2(AIndex: Integer; Values: Pointer; ACount: Integer);
    procedure InternalInsertRange4(AIndex: Integer; Values: Pointer; ACount: Integer);
    procedure InternalInsertRange8(AIndex: Integer; Values: Pointer; ACount: Integer);
    procedure InternalInsertRangeN(AIndex: Integer; Values: Pointer; ACount: Integer);
    procedure InternalInsertRangeManaged(AIndex: Integer; Values: Pointer; ACount: Integer);
    function InternalIndexOf1(const Value; Direction: Byte): Integer; inline;
    function InternalIndexOf2(const Value; Direction: Byte): Integer; inline;
    function InternalIndexOf4(const Value; Direction: Byte): Integer; inline;
    function InternalIndexOf8(const Value; Direction: Byte): Integer; inline;
    function InternalIndexOfN(const Value; Direction: Byte): Integer; inline;
    function InternalIndexOfMRef(const Value; Direction: Byte): Integer; inline;
    procedure InternalExtractItem1(const Value; out Item; Direction: Byte); inline;
    procedure InternalExtractItem2(const Value; out Item; Direction: Byte); inline;
    procedure InternalExtractItem4(const Value; out Item; Direction: Byte); inline;
    procedure InternalExtractItem8(const Value; out Item; Direction: Byte); inline;
    procedure InternalExtractItemN(const Value; out Item; Direction: Byte); inline;
    procedure InternalExtractItemMRef(const Value; Kind: TTypeKind; out Item; Direction: Byte); inline;
    procedure InternalExtractItemVariant(const Value; out Item; Direction: Byte); inline;
    procedure InternalExtractItemManaged(const Value; out Item; Direction: Byte); inline;
    procedure InternalExchange1(Index1, Index2: Integer);
    procedure InternalExchange2(Index1, Index2: Integer);
    procedure InternalExchange4(Index1, Index2: Integer);
    procedure InternalExchange8(Index1, Index2: Integer);
    procedure InternalExchangeN(Index1, Index2: Integer);
    procedure InternalExchangeMRef(Index1, Index2: Integer; Kind: TTypeKind); inline;
    procedure InternalExchangeManaged(Index1, Index2: Integer);
    procedure InternalMove1(CurIndex, NewIndex: Integer);
    procedure InternalMove2(CurIndex, NewIndex: Integer);
    procedure InternalMove4(CurIndex, NewIndex: Integer);
    procedure InternalMove8(CurIndex, NewIndex: Integer);
    procedure InternalMoveN(CurIndex, NewIndex: Integer);
    procedure InternalMoveMRef(CurIndex, NewIndex: Integer);
    procedure InternalMoveManaged(CurIndex, NewIndex: Integer);
    procedure InternalPackInline(const IsEmpty: TInternalEmptyFunc);
    procedure InternalPack1(const IsEmpty: TInternalEmptyFunc);
    procedure InternalPack2(const IsEmpty: TInternalEmptyFunc);
    procedure InternalPack4(const IsEmpty: TInternalEmptyFunc);
    procedure InternalPack8(const IsEmpty: TInternalEmptyFunc);
    procedure InternalPackN(const IsEmpty: TInternalEmptyFunc);
    procedure InternalPackManaged(const IsEmpty: TInternalEmptyFunc);
    function InternalRemove1(const Value; Direction: Byte): Integer; inline;
    function InternalRemove2(const Value; Direction: Byte): Integer; inline;
    function InternalRemove4(const Value; Direction: Byte): Integer; inline;
    function InternalRemove8(const Value; Direction: Byte): Integer; inline;
    function InternalRemoveN(const Value; Direction: Byte): Integer; inline;
    function InternalRemoveMRef(const Value; Direction: Byte): Integer; inline;
    procedure InternalReverse1;
    procedure InternalReverse2;
    procedure InternalReverse4;
    procedure InternalReverse8;
    procedure InternalReverseN;
    procedure InternalReverseMRef(Kind: TTypeKind); inline;
    procedure InternalReverseManaged;
    procedure InternalToArray(var Dest: Pointer);
    procedure InternalToArrayManaged(var Dest: Pointer);
    property FItems: PPointer read GetFItems;
    property ElType: Pointer read GetElType;
    property ElSize: Integer read GetElSize;
  end;

  TList<T> = class(TEnumerable<T>)
  private type
    arrayofT = array of T;
  var
    FListHelper: TListHelper; // FListHelper must always be followed by FItems
    FItems: arrayofT; // FItems must always be preceded by FListHelper
    FComparer: IComparer<T>;
    FOnNotify: TCollectionNotifyEvent<T>;

    function GetCapacity: Integer; inline;
    procedure SetCapacity(Value: Integer); overload; inline;
    procedure SetCount(Value: Integer); inline;
    function GetItem(Index: Integer): T; inline;
    procedure SetItem(Index: Integer; const Value: T); inline;
    procedure GrowCheck(ACount: Integer); inline;
    procedure DoDelete(Index: Integer; Notification: TCollectionNotification); inline;
    procedure InternalNotify(const Item; Action: TCollectionNotification);
    function InternalCompare(const Left, Right): Integer;
    property FCount: Integer read FListHelper.FCount write FListHelper.FCount;
  protected
    function ItemValue(const Item: T): NativeInt;
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
  public
  type
    TDirection = System.Types.TDirection;
    TEmptyFunc = reference to function (const L, R: T): Boolean;
    TListCompareFunc = reference to function (const L, R: T): Integer;

    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(const Collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
{$IFNDEF NEXTGEN}
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
{$ENDIF  NEXTGEN}

    function Add(const Value: T): Integer; inline;

    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload; inline;
    procedure AddRange(const Collection: TEnumerable<T>); overload; inline;

    procedure Insert(Index: Integer; const Value: T); inline;

    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;

    procedure Pack; overload;
    procedure Pack(const IsEmpty: TEmptyFunc); overload;

    function Remove(const Value: T): Integer; inline;
    function RemoveItem(const Value: T; Direction: TDirection): Integer; inline;
    procedure Delete(Index: Integer); inline;
    procedure DeleteRange(AIndex, ACount: Integer); inline;
    function ExtractItem(const Value: T; Direction: TDirection): T; inline;
    function Extract(const Value: T): T; inline;

    procedure Exchange(Index1, Index2: Integer); inline;
    procedure Move(CurIndex, NewIndex: Integer); inline;

    function First: T; inline;
    function Last: T; inline;

    procedure Clear; inline;

    function Expand: TList<T>; inline;

    function Contains(const Value: T): Boolean; inline;
    function IndexOf(const Value: T): Integer; inline;
    function IndexOfItem(const Value: T; Direction: TDirection): Integer; inline;
    function LastIndexOf(const Value: T): Integer; inline;

    procedure Reverse; inline;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;

    procedure TrimExcess; inline;

    function ToArray: TArray<T>; override; final;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FListHelper.FCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List: arrayofT read FItems;

    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;

    type
      TEnumerator = class(TEnumerator<T>)
      private
        FList: TList<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AList: TList<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TEnumerator; reintroduce; inline;
  end;

  TThreadList<T> = class
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

  TQueueHelper = record
  private
    FHead, FTail: Integer;
    FLH: TListHelper;
    procedure DynArraySetLength(Value: NativeInt); inline;
    function GetFItems: PPointer; inline;
    function GetElType: Pointer; inline;
    function GetElSize: Integer; inline;
    function GetNewCap: Integer; inline;
    procedure CheckEmpty; inline;
    procedure DequeueAdjust(Notification: TCollectionNotification; const Item); inline;
    procedure InternalDequeueString(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDequeueInterface(Notification: TCollectionNotification; Peek: Boolean; out Item);
{$IF not Defined(NEXTGEN)}
    procedure InternalDequeueAnsiString(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDequeueWideString(Notification: TCollectionNotification; Peek: Boolean; out Item);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure InternalDequeueObject(Notification: TCollectionNotification; Peek: Boolean; out Item);
{$IFEND}
    procedure InternalDequeue1(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDequeue2(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDequeue4(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDequeue8(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDequeueN(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDequeueMRef(Notification: TCollectionNotification; Peek: Boolean; out Item; Kind: TTypeKind); inline;
    procedure InternalDequeueManaged(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalClearString;
    procedure InternalClearInterface;
{$IF not Defined(NEXTGEN)}
    procedure InternalClearAnsiString;
    procedure InternalClearWideString;
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure InternalClearObject;
{$IFEND}
    procedure InternalClear1;
    procedure InternalClear2;
    procedure InternalClear4;
    procedure InternalClear8;
    procedure InternalClearN;
    procedure InternalClearMRef(Kind: TTypeKind); inline;
    procedure InternalClearManaged;
    procedure EnqueueAdjust(const Value); inline;
    procedure InternalEnqueueString(const Value);
    procedure InternalEnqueueInterface(const Value);
{$IF not Defined(NEXTGEN)}
    procedure InternalEnqueueAnsiString(const Value);
    procedure InternalEnqueueWideString(const Value);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure InternalEnqueueObject(const Value);
{$IFEND}
    procedure InternalEnqueue1(const Value);
    procedure InternalEnqueue2(const Value);
    procedure InternalEnqueue4(const Value);
    procedure InternalEnqueue8(const Value);
    procedure InternalEnqueueN(const Value);
    procedure InternalEnqueueMRef(const Value; Kind: TTypeKind); inline;
    procedure InternalEnqueueManaged(const Value);
    procedure InternalGrow1;
    procedure InternalGrow2;
    procedure InternalGrow4;
    procedure InternalGrow8;
    procedure InternalGrowN;
    procedure InternalGrowMRef;
    procedure InternalGrowManaged;
    procedure InternalSetCapacityInline(Value: Integer; ElemSize: Integer); inline;
    procedure InternalSetCapacity1(Value: Integer);
    procedure InternalSetCapacity2(Value: Integer);
    procedure InternalSetCapacity4(Value: Integer);
    procedure InternalSetCapacity8(Value: Integer);
    procedure InternalSetCapacityN(Value: Integer);
    procedure InternalSetCapacityMRef(Value: Integer);
    procedure InternalSetCapacityManaged(Value: Integer);
    property FItems: PPointer read GetFItems;
    property ElType: Pointer read GetElType;
    property ElSize: Integer read GetElSize;
  end;

  // Queue implemented over array, using wrapping.
  TQueue<T> = class(TEnumerable<T>)
  private type
    arrayOfT = array of T;
  private
    FQueueHelper: TQueueHelper; // FQueueHelper must always be followed by FItems
    FItems: arrayOfT; // FItems must always be preceded by FListHelper
    FOnNotify: TCollectionNotifyEvent<T>;
    procedure SetCapacity(Value: Integer); inline;
    function DoDequeue(Notification: TCollectionNotification): T; inline;
    procedure DoSetCapacity(Value: Integer); inline;
    function GetCapacity: Integer; inline;
    procedure InternalNotify(const Item; Action: TCollectionNotification);
    function InternalCompare(const Left, Right): Integer;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
  public
    constructor Create; overload;
    constructor Create(const Collection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Enqueue(const Value: T); inline;
    function Dequeue: T; inline;
    function Extract: T; inline;
    function Peek: T; inline;
    procedure Clear; inline;
    procedure TrimExcess; inline;
    property Count: Integer read FQueueHelper.FLH.FCount;
    property Capacity: Integer read GetCapacity write DoSetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
    function ToArray: TArray<T>; override; final;

    type
      TEnumerator = class(TEnumerator<T>)
      private
        FQueue: TQueue<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AQueue: TQueue<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TEnumerator; reintroduce;
  end;

  TThreadedQueue<T> = class
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

  TStackHelper = record
  private
    FLH: TListHelper;
    function GetFItems: PPointer; inline;
    function GetElType: Pointer; inline;
    function GetElSize: Integer; inline;
    procedure CheckEmpty; inline;
    procedure CheckGrow; inline;
    procedure InternalGrow;
    procedure InternalSetCapacity(Value: Integer);
    procedure PopAdjust(const Value; Notification: TCollectionNotification); inline;
    procedure InternalDoPopString(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDoPopInterface(Notification: TCollectionNotification; Peek: Boolean; out Item);
{$IF not Defined(NEXTGEN)}
    procedure InternalDoPopAnsiString(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDoPopWideString(Notification: TCollectionNotification; Peek: Boolean; out Item);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure InternalDoPopObject(Notification: TCollectionNotification; Peek: Boolean; out Item);
{$IFEND}
    procedure InternalDoPop1(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDoPop2(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDoPop4(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDoPop8(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDoPopN(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalDoPopMRef(Notification: TCollectionNotification; Peek: Boolean; out Item; Kind: TTypeKind); inline;
    procedure InternalDoPopManaged(Notification: TCollectionNotification; Peek: Boolean; out Item);
    procedure InternalClearString;
    procedure InternalClearInterface;
{$IF not Defined(NEXTGEN)}
    procedure InternalClearAnsiString;
    procedure InternalClearWideString;
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure InternalClearObject;
{$IFEND}
    procedure InternalClear1;
    procedure InternalClear2;
    procedure InternalClear4;
    procedure InternalClear8;
    procedure InternalClearN;
    procedure InternalClearMRef(Kind: TTypeKind); inline;
    procedure InternalClearManaged;
    procedure PushAdjust(const Value); inline;
    procedure InternalPushString(const Value);
    procedure InternalPushInterface(const Value);
{$IF not Defined(NEXTGEN)}
    procedure InternalPushAnsiString(const Value);
    procedure InternalPushWideString(const Value);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    procedure InternalPushObject(const Value);
{$IFEND}
    procedure InternalPush1(const Value);
    procedure InternalPush2(const Value);
    procedure InternalPush4(const Value);
    procedure InternalPush8(const Value);
    procedure InternalPushN(const Value);
    procedure InternalPushMRef(const Value; Kind: TTypeKind); inline;
    procedure InternalPushManaged(const Value);
    property FItems: PPointer read GetFItems;
    property ElType: Pointer read GetElType;
    property ElSize: Integer read GetElSize;
  end;

  TStack<T> = class(TEnumerable<T>)
  private type
    arrayOfT = array of T;
  private
    FStackHelper: TStackHelper;
    FItems: arrayOfT;
    FOnNotify: TCollectionNotifyEvent<T>;
    function DoPop(Notification: TCollectionNotification): T; inline;
    procedure DoSetCapacity(Value: Integer); inline;
    function GetCapacity: Integer; inline;
    procedure InternalNotify(const Item; Action: TCollectionNotification);
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
  public
    constructor Create; overload;
    constructor Create(const Collection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Clear; inline;
    procedure Push(const Value: T); inline;
    function Pop: T; inline;
    function Peek: T; inline;
    function Extract: T; inline;
    procedure TrimExcess; inline;
    function ToArray: TArray<T>; override; final;
    property Count: Integer read FStackHelper.FLH.FCount;
    property Capacity: Integer read GetCapacity write DoSetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;

    type
      TEnumerator = class(TEnumerator<T>)
      private
        FStack: TStack<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AStack: TStack<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TEnumerator; reintroduce;
  end;

  TObjectList<T: class> = class(TList<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TObjectQueue<T: class> = class(TQueue<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Dequeue;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TObjectStack<T: class> = class(TStack<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Pop;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TDictionaryOwnerships = set of (doOwnsKeys, doOwnsValues);

  TObjectDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
  private
    FOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); override;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); override;
  public
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0); overload;
    constructor Create(Ownerships: TDictionaryOwnerships;
      const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer;
      const AComparer: IEqualityComparer<TKey>); overload;
  end;


implementation

// x86 architecture compatibility (Word mode)
{$ifNdef CPUX86}
function Swap(const X: NativeUInt): NativeUInt; inline;
begin
  Result := (Byte(X) shl 8) + Byte(X shr 8);
end;
{$endif}


{ TRAIIHelper.TClearNatives }

procedure TRAIIHelper.TClearNatives.Clear;
begin
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

{$ifdef WEAKREF}
{ TRAIIHelper.TInitNatives }

procedure TRAIIHelper.TInitNatives.Clear;
begin
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
  if (VType and varDeepData <> 0) then
  case VType of
    varBoolean, varUnknown+1..varUInt64: ;
  else
    System.VarClear(PVariant(P)^);
  end;
end;

class procedure TRAIIHelper.DynArrayClear(P, TypeInfo: Pointer);
type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
    {$ifdef LARGEINT}
    _Padding: Integer;
    {$endif}
    RefCnt: Integer;
    Length: NativeInt;
  end;
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
      Inc(PByte(TypeInfo), PDynArrayTypeInfo(TypeInfo).name);
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
var
  L: TObject;
begin
  L := PPointer(P)^;
  PPointer(P)^ := nil;
  L.__ObjRelease;
end;
{$endif}

{$ifdef WEAKINSTREF}
class procedure TRAIIHelper.WeakObjClear(P: Pointer);
var
  L: TObject;
begin
  L := PPointer(P)^;
  PPointer(P)^ := nil;
  UnregisterWeakRef(P, L);
end;
{$endif}

{$ifdef WEAKREF}
class procedure TRAIIHelper.WeakMethodClear(P: Pointer);
var
  L: TObject;
begin
  L := PPointer(P)^;
  PPointer(P)^ := nil;
  Dec(NativeUInt(P), SizeOf(Pointer));
  PPointer(P)^ := nil;
  UnregisterWeakMethodRef(P, L);
end;
{$endif}

{$ifdef WEAKINTFREF}
class procedure TRAIIHelper.WeakIntfClear(P: Pointer);
var
  L: TObject;
begin
  L := IInterface(PPointer(P)^) as TObject;
  PPointer(P)^ := nil;
  UnregisterWeakRef(P, L);
end;
{$endif}

procedure TRAIIHelper.Include(AOffset: NativeInt; Value: PTypeInfo);
var
  i: Cardinal;
  {$ifdef WEAKREF}
  Weak: Boolean;
  {$endif}
  FieldTable: PFieldTable;
  ChildSize, ChildOffset: NativeInt;
begin
  case Value.Kind of
    tkVariant:
    begin
      {$ifdef WEAKREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.VarClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.VarClear));
      {$endif}
    end;
    {$ifdef AUTOREFCOUNT}
    tkClass:
    begin
      {$ifdef WEAKREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.RefObjClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.RefObjClear));
      {$endif}
    end;
    {$endif}
    {$ifdef WEAKREF}
    tkMethod:
    begin
      Self.InitNatives.Add(AOffset);
      Self.InitNatives.Add(AOffset + SizeOf(Pointer));

      Self.ClearNatives.Add(AOffset + SizeOf(Pointer), nil, TClearNativeProc(@TRAIIHelper.WeakMethodClear));
    end;
    {$endif}
    {$ifdef MSWINDOWS}
    tkWString:
    begin
      {$ifdef WEAKREF}
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
      {$ifdef WEAKREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.ULStrClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.ULStrClear));
      {$endif}
    end;
    tkInterface:
    begin
      {$ifdef WEAKREF}
        Self.InitNatives.Add(AOffset);
        Self.ClearNatives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.IntfClear));
      {$else}
        Self.Natives.Add(AOffset, nil, TClearNativeProc(@TRAIIHelper.IntfClear));
      {$endif}
    end;
    tkDynArray:
    begin
      {$ifdef WEAKREF}
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
        Weak := False;
        {$endif}
        for i := 0 to FieldTable.Count - 1 do
        begin
          ChildOffset := AOffset + NativeInt(FieldTable.Fields[i].Offset);
        {$ifdef WEAKREF}
          if FieldTable.Fields[i].TypeInfo = nil then
          begin
            Weak := True;
            Continue;
          end;
          if not Weak then
          begin
          {$endif}
            Self.Include(ChildOffset, FieldTable.Fields[i].TypeInfo^);
          {$ifdef WEAKREF}
          end else
          begin
            case FieldTable.Fields[i].TypeInfo^.Kind of
            {$ifdef WEAKINTFREF}
              tkInterface:
              begin
                {$ifdef WEAKREF}
                  Self.InitNatives.Add(ChildOffset);
                  Self.ClearNatives.Add(ChildOffset, nil, TClearProc(@TRAIIHelper.WeakIntfClear));
                {$else}
                  Self.Natives.Add(ChildOffset, nil, TClearProc(@TRAIIHelper.WeakIntfClear));
                {$endif}
              end;
            {$endif}
            {$ifdef WEAKINSTREF}
              tkClass:
              begin
                {$ifdef WEAKREF}
                  Self.InitNatives.Add(ChildOffset);
                  Self.ClearNatives.Add(ChildOffset, nil, TClearProc(@TRAIIHelper.WeakObjClear));
                {$else}
                  Self.Natives.Add(ChildOffset, nil, TClearProc(@TRAIIHelper.WeakObjClear));
                {$endif}
              end;
            {$endif}
            {$ifdef WEAKREF}
              tkMethod:
              begin
                Self.InitNatives.Add(ChildOffset);
                Self.InitNatives.Add(ChildOffset + SizeOf(Pointer));

                Self.ClearNatives.Add(ChildOffset + SizeOf(Pointer), nil, TClearProc(@TRAIIHelper.WeakMethodClear));
              end;
            {$endif}
            end;
          end;
        {$endif}
        end;
      end;
    end;
  end;
end;

procedure TRAIIHelper.Initialize(Value: PTypeInfo);
var
  FieldTable: PFieldTable;
begin
  // clear
  FTypeInfo := Value;
  {$ifdef WEAKREF}
  InitNatives.Clear;
  ClearNatives.Clear;
  {$else}
  Natives.Clear;
  {$endif}
  StaticArrays.Clear;
  InitProc := nil;
  ClearProc := nil;

  // type kind
  case Value.Kind of
    tkInteger, tkChar, tkEnumeration, tkWChar:
    begin
      case (Value.TypeData.OrdType) of
        otSByte, otUByte: FSize := SizeOf(Byte);
        otSWord, otUWord: FSize := SizeOf(Word);
        otSLong, otULong: FSize := SizeOf(Cardinal);
      end;
      Exit;
    end;
    tkSet:
    begin
      with Value.TypeData.CompType^.TypeData^ do
      begin
        FSize := (((MaxValue + 7 + 1) and ($FF shl 3)) - (MinValue and ($FF shl 3))) shr 3;
        if (FSize = 3) then FSize := 4;
      end;
      Exit;
    end;
    tkFloat:
    begin
      case (Value.TypeData.FloatType) of
        ftSingle: FSize := SizeOf(Single);
        ftDouble: FSize := SizeOf(Double);
      ftExtended: FSize := SizeOf(Extended);
          ftComp: FSize := SizeOf(Comp);
          ftCurr: FSize := SizeOf(Currency);
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
      FSize := Value.TypeData.MaxLength + 1;
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
    tkClass, tkLString, tkWString, tkInterface, tkDynArray, tkUString:
    begin
      FSize := SizeOf(Pointer);
      Self.Include(0, Value);
    end;
    tkArray:
    begin
      FieldTable := PFieldTable(NativeUInt(Value) + PByte(@Value.Name)^);
      FSize := NativeInt(FieldTable.Size) * NativeInt(FieldTable.Count);
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
  end else
  if ({$ifdef WEAKREF}InitNatives{$else}Natives{$endif}.Count <> 0) then
  begin
    if ({$ifdef WEAKREF}InitNatives{$else}Natives{$endif}.Count > 1) then
    begin
      InitProc := Self.InitsProcNatives;
    end else
    begin
      InitProc := Self.InitsProcNativeSingle;
    end;
  end;

  // finalization
  if (StaticArrays.Count <> 0) then
  begin
    ClearProc := Self.ClearsProc;
  end else
  if ({$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.Count <> 0) then
  begin
    if ({$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.Count > 1) then
    begin
      ClearProc := Self.ClearsProcNatives;
    end else
    begin
      ClearProc := Self.ClearsProcNativeSingle;
    end;
  end;
end;

class function TRAIIHelper.IsManagedTypeInfo(Value: PTypeInfo): Boolean;
var
  i: Cardinal;
  {$ifdef WEAKREF}
  Weak: Boolean;
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
    {$ifdef WEAKREF}
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
        Weak := False;
        {$endif}
        for i := 0 to FieldTable.Count - 1 do
        begin
         {$ifdef WEAKREF}
          if FieldTable.Fields[i].TypeInfo = nil then
          begin
            Weak := True;
            Continue;
          end;
          if not Weak then
          begin
          {$endif}
            if (IsManagedTypeInfo(FieldTable.Fields[i].TypeInfo^)) then
              Exit(True);
          {$ifdef WEAKREF}
          end else
          begin
            case FieldTable.Fields[i].TypeInfo^.Kind of
            {$ifdef WEAKINTFREF}
              tkInterface:
              begin
                Exit(True);
              end;
            {$endif}
            {$ifdef WEAKINSTREF}
              tkClass:
              begin
                Exit(True);
              end;
            {$endif}
            {$ifdef WEAKREF}
              tkMethod:
              begin
                Exit(True);
              end;
            {$endif}
            end;
          end;
        {$endif}
        end;
      end;
    end;
  end;
end;

class procedure TRAIIHelper.InitsProcNativeSingle(const Self: TRAIIHelper; P: Pointer);
begin
  Inc(NativeInt(P), Self.{$ifdef WEAKREF}InitNatives{$else}Natives{$endif}.ItemSingle.Offset);
  PNativeInt(P)^ := 0;
end;

class procedure TRAIIHelper.InitsProcNatives(const Self: TRAIIHelper; P: Pointer);
label
  _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11;
var
  Count, Null: NativeInt;
  Item: ^{$ifdef WEAKREF}TInitNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKREF}InitNatives{$else}Natives{$endif}.Items);
  Count := Self.{$ifdef WEAKREF}InitNatives{$else}Natives{$endif}.Count;
  Null := 0;

  case Count of
    11:
    begin
    _11:
      repeat
        PNativeInt(NativeInt(P) + Item.Offset)^ := Null;
        Dec(Count);
        Inc(Item);
      until (Count = 0);
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
end;

class procedure TRAIIHelper.InitsProc(const Self: TRAIIHelper; P: Pointer);
var
  i: NativeInt;
  Item: ^{$ifdef WEAKREF}TInitNativeRec{$else}TNativeRec{$endif};
  StaticArrayRec: ^TStaticArrayRec;
begin
  Item := Pointer(Self.{$ifdef WEAKREF}InitNatives{$else}Natives{$endif}.Items);
  for i := 1 to Self.{$ifdef WEAKREF}InitNatives{$else}Natives{$endif}.Count do
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
end;

class procedure TRAIIHelper.ClearsProcNativeSingle(const Self: TRAIIHelper; P: Pointer);
var
  Value: PNativeInt;
begin
  Value := Pointer(NativeInt(P) + Self.{$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.ItemSingle.Offset);
  if (Value^ <> 0) then
  begin
    Self.{$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.ItemSingle.ClearNativeProc(Value,
      Self.{$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.ItemSingle.DynTypeInfo);
  end;
end;

class procedure TRAIIHelper.ClearsProcNatives(const Self: TRAIIHelper; P: Pointer);
label
  _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11;
var
  Count: NativeInt;
  Value: PNativeInt;
  Item: ^{$ifdef WEAKREF}TClearNativeRec{$else}TNativeRec{$endif};
begin
  Item := Pointer(Self.{$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.Items);
  Count := Self.{$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.Count;

  case Count of
    11:
    begin
    _11:
      repeat
        Value := Pointer(NativeInt(P) + Item.Offset);
        if (Value^ <> 0) then Item.ClearNativeProc(Value, Item.DynTypeInfo);
        Dec(Count);
        Inc(Item);
      until (Count = 0);
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
end;

class procedure TRAIIHelper.ClearsProc(const Self: TRAIIHelper; P: Pointer);
var
  i: NativeInt;
  Value: PNativeInt;
  Item: ^TClearNativeRec;
  StaticArrayRec: ^TStaticArrayRec;
begin
  Item := Pointer(Self.{$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.Items);
  for i := 1 to Self.{$ifdef WEAKREF}ClearNatives{$else}Natives{$endif}.Count do
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
end;

{ TRAIIHelper<T> }

class constructor TRAIIHelper<T>.Create;
begin
  FOptions.TypeInfo := TypeInfo(T);
end;

class procedure TRAIIHelper<T>.Init(Value: P);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T) or System.HasWeakRef(T)) then
  {$else}
  if (Assigned(FOptions.InitProc)) then
  {$endif}
    FOptions.InitProc(FOptions, Value);
end;

class procedure TRAIIHelper<T>.Clear(Value: P);
begin
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(T) or System.HasWeakRef(T)) then
  {$else}
  if (Assigned(FOptions.ClearProc)) then
  {$endif}
    FOptions.ClearProc(FOptions, Value);
end;


{ InterfaceDefaults }

class constructor InterfaceDefaults.TDefaultComparer<T>.Create;
var
  TypeData: PTypeData;
begin
  Instance.Vtable := @Instance.QueryInterface;
  Instance.Size := SizeOf(T);
  Instance.QueryInterface := @InterfaceDefaults.NopQueryInterface;
  Instance.AddRef := @InterfaceDefaults.NopAddRef;
  Instance.Release := @InterfaceDefaults.NopRelease;

  // Compare
  TypeData := PTypeInfo(TypeInfo(T)).TypeData;
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
end;

class constructor InterfaceDefaults.TDefaultEqualityComparer<T>.Create;
var
  TypeData: PTypeData;
begin
  Instance.Vtable := @Instance.QueryInterface;
  Instance.Size := SizeOf(T);
  Instance.QueryInterface := @InterfaceDefaults.NopQueryInterface;
  Instance.AddRef := @InterfaceDefaults.NopAddRef;
  Instance.Release := @InterfaceDefaults.NopRelease;

  // Equals/GetHashCode
  TypeData := PTypeInfo(TypeInfo(T)).TypeData;
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
      Instance.Size := -SizeOf(Int64);
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
{$ifNdef CPUINTEL}
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
{$ifNdef CPUINTEL}
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
{$if Defined(CPUX64)}
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
{$if Defined(CPUX64)}
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
{$if Defined(CPUX86)}
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
{$elseif Defined(CPUX64)}
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
{$if Defined(CPUX86)}
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
{$elseif Defined(CPUX64)}
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
{$if Defined(CPUX86)}
asm
  fld Left
  fld Right
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
{$elseif Defined(CPUX64)}
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
      Result := Ord(Left^ > Right^);
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
          varShortInt, varBoolean, varByte:
          begin
            Result := (Left.VByte = Right.VByte);
          end;
          varSmallint, varWord:
          begin
            Result := (Left.VWord = Right.VWord);
          end;
          varInteger, varLongWord:
          begin
            Result := (Left.VInteger = Right.VInteger);
          end;
          varInt64, varCurrency, varUInt64:
          begin
            {$ifdef LARGEINT}
              Result := (Left.VInt64 = Right.VInt64);
            {$else .SMALLINT}
              Result := ((Left.VLongs[1] - Right.VLongs[1]) or (Left.VLongs[2] - Right.VLongs[2]) = 0);
            {$endif}
          end;
          varSingle:
          begin
            Result := (Left.VSingle >= Right.VSingle) = (Left.VSingle <= Right.VSingle);
          end;
          varDouble, varDate:
          begin
            Result := (Left.VDouble >= Right.VDouble) = (Left.VDouble <= Right.VDouble);
          end;
          varString:
          begin
            Result := InterfaceDefaults.Equals_LStr(nil, Left.VPointer, Right.VPointer);
          end;
          varUString:
          begin
            Result := InterfaceDefaults.Equals_UStr(nil, Left.VPointer, Right.VPointer);
          end;
          varOleStr:
          begin
            Result := InterfaceDefaults.Equals_WStr(nil, Left.VPointer, Right.VPointer);
          end;
        else
        difficult:
          Result := (InterfaceDefaults.Compare_Var_Difficult(True, PVariant(Left), PVariant(Right)) = 0);
        end;
      end else
      begin
        Result := True;
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
      Top := @Buffer[FloatToText(Buffer, VFloat, fvExtended, ffGeneral, 15, 0, FormatSettings)];
      S := @Buffer[0];
      goto write_terminated_string;
    end;
    varDouble:
    begin
      VFloat := Value.VDouble;
      Top := @Buffer[FloatToText(Buffer, VFloat, fvExtended, ffGeneral, 15, 0, FormatSettings)];
      S := @Buffer[0];
      goto write_terminated_string;
    end;
    varCurrency:
    begin
      Top := @Buffer[FloatToText(Buffer, Value.VCurrency, fvCurrency, ffGeneral, 0, 0, FormatSettings)];
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
var
  X, Y: NativeUInt;
  Temp: IComparerInst;
begin
  X := Left^;
  Y := Right^;
  if (Left <> Right) and (X <> 0) and (Y <> 0) then
  begin
    if (Left[1] <> Right[1]) then
    begin
      if (X <= Y) then
      begin
        Y := (-NativeInt(Y - X)) shr {$ifdef SMALLINT}31{$else}63{$endif};
      end else
      begin
        X := Y;
        Y := NativeUInt(-1);
      end;

      Temp.Size := X + 1;
      X := NativeInt(InterfaceDefaults.Compare_Bin(Temp, Left, Right));
      Result := NativeInt(X * 2 - Y);
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
    until (Count > (40 + SizeOf(NativeUInt) - 1));

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
  done;
var
  X, Y: NativeUInt;
  Temp: IComparerInst;
begin
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = nil) or (Right = nil) or (Left = Right) then goto done;

  X := Left^;
  Y := Right^;
  if (X <> Y) then goto done;

  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  X := PInteger(Left)^;
  Y := PInteger(Right)^;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));
  if (X <= Y) then
  begin
    Y := (-NativeInt(Y - X)) shr {$ifdef SMALLINT}31{$else}63{$endif};
  end else
  begin
    X := Y;
    Y := NativeUInt(-1);
  end;

  Temp.Size := X;
  X := NativeInt(InterfaceDefaults.Compare_Bin(Temp, Left, Right));
  Result := NativeInt(X * 2 - Y);
  Exit;
done:
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
    until (Count > (40 + SizeOf(NativeUInt) - 1));

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
  make_result, done;
var
  X, Y, Count: NativeUInt;
  Modify: Integer;
begin
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = nil) or (Right = nil) or (Left = Right) then goto done;

  X := Left^;
  Y := Right^;
  if (X <> Y) then goto done;

  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  X := PInteger(Left)^;
  Y := PInteger(Right)^;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));
  if (X <= Y) then
  begin
    Modify := (-NativeInt(Y - X)) shr {$ifdef SMALLINT}31{$else}63{$endif};
    Count := X * 2 + 2;
  end else
  begin
    Count := Y * 2 + 2;
    Modify := -1;
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
done:
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
    until (Count > (40 + SizeOf(NativeUInt) - 1));

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
  make_result, done;
var
  X, Y, Count: NativeUInt;
  Modify: Integer;
begin
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = Right) then goto done;
  if (Left = nil) then goto left_nil;
  if (Right = nil) then goto right_nil;

  X := Left^;
  Y := Right^;
  if (X <> Y) then goto done;

  Dec(Left, SizeOf(Integer));
  Dec(Right, SizeOf(Integer));
  X := PInteger(Left)^;
  Y := PInteger(Right)^;
  Inc(Left, SizeOf(Integer));
  Inc(Right, SizeOf(Integer));
  if (X <= Y) then
  begin
    Modify := (-NativeInt(Y - X)) shr {$ifdef SMALLINT}31{$else}63{$endif};
    Count := X {$ifNdef MSWINDOWS}* 2{$endif} + 2;
  end else
  begin
    Count := Y {$ifNdef MSWINDOWS}* 2{$endif} + 2;
    Modify := -1;
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
done:
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
    until (Count > (40 + SizeOf(NativeUInt) - 1));

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
  done;
var
  X, Y: NativeUInt;
  Temp: IComparerInst;
begin
  Temp.Size := Inst.Size;
  X := NativeUInt(Left);
  Y := NativeUInt(Right);
  if (Left = nil) or (Right = nil) or (Left = Right) then goto done;

  X := Left^;
  Y := Right^;
  if (X <> Y) then goto done;

  Dec(Left, SizeOf(NativeUInt));
  Dec(Right, SizeOf(NativeUInt));
  X := PNativeUInt(Left)^;
  Y := PNativeUInt(Right)^;
  Inc(Left, SizeOf(NativeUInt));
  Inc(Right, SizeOf(NativeUInt));
  if (X <= Y) then
  begin
    Y := (-NativeInt(Y - X)) shr {$ifdef SMALLINT}31{$else}63{$endif};
  end else
  begin
    X := Y;
    Y := NativeUInt(-1);
  end;

  Temp.Size := Temp.Size * NativeInt(X);
  X := NativeInt(InterfaceDefaults.Compare_Bin(Temp, Left, Right));
  Result := NativeInt(X * 2 - Y);
  Exit;
done:
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
    until (Count > (40 + SizeOf(NativeUInt) - 1));

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
    until (Count > (40 + SizeOf(NativeUInt) - 1));

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
  Result := IComparer<T>(@InterfaceDefaults.TDefaultComparer<T>.Instance);
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
  Result := IEqualityComparer<T>(@InterfaceDefaults.TDefaultEqualityComparer<T>.Instance);
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
{$ifNdef CPUINTEL}
begin
  Result := InterfaceDefaults.Compare_UStr(nil, Pointer(Left), Pointer(Right));
end;
{$else}
asm
  jmp InterfaceDefaults.Compare_UStr
end;
{$endif}

function TOrdinalStringComparer.Equals(const Left, Right: string): Boolean;
{$ifNdef CPUINTEL}
begin
  Result := InterfaceDefaults.Equals_UStr(Left, Right);
end;
{$else}
asm
  jmp InterfaceDefaults.Equals_UStr
end;
{$endif}

function TOrdinalStringComparer.GetHashCode(const Value: string): Integer;
{$ifNdef CPUINTEL}
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


{ TEnumerator<T> }

function TEnumerator<T>.MoveNext: Boolean;
begin
  Result := DoMoveNext;
end;

// The overridden destructor that simply invokes 'inherited' is
// required to instantiate the destructor for C++ code
destructor TEnumerable<T>.Destroy;
begin
  inherited;
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
  Result := nil;
  Count := 0;
  Buffered := 16;

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

function TEnumerable<T>.ToArrayImpl(Count: Integer): TArray<T>;
var
  Value: T;
begin
  // We assume our caller has passed correct Count
  SetLength(Result, Count);
  Count := 0;
  for Value in Self do
  begin
    Result[Count] := Value;
    Inc(Count);
  end;
end;

{ TPair<TKey,TValue> }

constructor TPair<TKey,TValue>.Create(const AKey: TKey; const AValue: TValue);
begin
  Key := AKey;
  Value := AValue;
end;

{ TCustomDictionary<TKey,TValue>.TPairEnumerator }

constructor TCustomDictionary<TKey,TValue>.TPairEnumerator.Create(const ADictionary: TCustomDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TCustomDictionary<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  Result := GetCurrent;
end;

function TCustomDictionary<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TCustomDictionary<TKey,TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
var
  Item: PItem;
begin
  Item := @FDictionary.FList[FIndex];
  Result.Key := Item.Key;
  Result.Value := Item.Value;
end;

function TCustomDictionary<TKey,TValue>.TPairEnumerator.MoveNext: Boolean;
var
  N: NativeInt;
begin
  N := FIndex + 1;
  FIndex := N;
  Result := (N < FDictionary.FCount.Native);
end;

{ TCustomDictionary<TKey,TValue>.TKeyEnumerator }

constructor TCustomDictionary<TKey,TValue>.TKeyEnumerator.Create(const ADictionary: TCustomDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TCustomDictionary<TKey, TValue>.TKeyEnumerator.DoGetCurrent: TKey;
begin
  Result := GetCurrent;
end;

function TCustomDictionary<TKey, TValue>.TKeyEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TCustomDictionary<TKey,TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := FDictionary.FList[FIndex].Key;
end;

function TCustomDictionary<TKey,TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if (FIndex < FDictionary.FCount.Native) then
  begin
    Inc(FIndex);
    Exit(True);
  end;
  Result := False;
end;

{ TCustomDictionary<TKey,TValue>.TValueEnumerator }

constructor TCustomDictionary<TKey,TValue>.TValueEnumerator.Create(const ADictionary: TCustomDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TCustomDictionary<TKey, TValue>.TValueEnumerator.DoGetCurrent: TValue;
begin
  Result := GetCurrent;
end;

function TCustomDictionary<TKey, TValue>.TValueEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TCustomDictionary<TKey,TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := FDictionary.FList[FIndex].Value;
end;

function TCustomDictionary<TKey,TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if (FIndex < FDictionary.FCount.Native) then
  begin
    Inc(FIndex);
    Exit(True);
  end;
  Result := False;
end;

{ TCustomDictionary<TKey, TValue>.TKeyCollection }

constructor TCustomDictionary<TKey, TValue>.TKeyCollection.Create(const ADictionary: TCustomDictionary<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TCustomDictionary<TKey, TValue>.TKeyCollection.DoGetEnumerator: TEnumerator<TKey>;
begin
  Result := GetEnumerator;
end;

function TCustomDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := FDictionary.FCount.Int;
end;

function TCustomDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(FDictionary);
end;

function TCustomDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  i, Count: NativeInt;
  Src: TCustomDictionary<TKey, TValue>.PItem;
  Dest: ^TKey;
begin
  Count := Self.FDictionary.FCount.Native;

  SetLength(Result, Count);
  Src := Pointer(FDictionary.FList);
  Dest := Pointer(Result);
  for i := 0 to Count - 1 do
  begin
    Dest^ := Src.Key;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TCustomDictionary<TKey, TValue>.TValueCollection }

constructor TCustomDictionary<TKey, TValue>.TValueCollection.Create(const ADictionary: TCustomDictionary<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TCustomDictionary<TKey, TValue>.TValueCollection.DoGetEnumerator: TEnumerator<TValue>;
begin
  Result := GetEnumerator;
end;

function TCustomDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := FDictionary.FCount.Int;
end;

function TCustomDictionary<TKey, TValue>.TValueCollection.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(FDictionary);
end;

function TCustomDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  i, Count: NativeInt;
  Src: TCustomDictionary<TKey, TValue>.PItem;
  Dest: ^TValue;
begin
  Count := Self.FDictionary.FCount.Native;

  SetLength(Result, Count);
  Src := Pointer(FDictionary.FList);
  Dest := Pointer(Result);
  for i := 0 to Count - 1 do
  begin
    Dest^ := Src.Value;
    Inc(Src);
    Inc(Dest);
  end;
end;

{ TCustomDictionary<TKey, TValue> }

function TCustomDictionary<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  Result := GetEnumerator;
end;

function TCustomDictionary<TKey, TValue>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TCustomDictionary<TKey, TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(Self);
  Result := FKeyCollection;
end;

function TCustomDictionary<TKey, TValue>.GetValues: TValueCollection;
begin
  if FValueCollection = nil then
    FValueCollection := TValueCollection.Create(Self);
  Result := FValueCollection;
end;

function TCustomDictionary<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
var
  i, Count: NativeInt;
  Src: PItem;
  Dest: ^TPair<TKey, TValue>;
begin
  Count := Self.FCount.Native;

  SetLength(Result, Count);
  Src := Pointer(FList);
  Dest := Pointer(Result);
  for i := 0 to Count - 1 do
  begin
    Dest.Key := Src.Key;
    Dest.Value := Src.Value;

    Inc(Src);
    Inc(Dest);
  end;
end;

constructor TCustomDictionary<TKey, TValue>.Create(ACapacity: Integer);
begin
  inherited Create;
  FDefaultValue := Default(TValue);
  FHashTableMask := -1;

  if (ACapacity > 3) then
  begin
    SetCapacity(ACapacity);
  end else
  begin
    Rehash(4);
  end;
end;

destructor TCustomDictionary<TKey, TValue>.Destroy;
begin
  Clear;
  FKeyCollection.Free;
  FValueCollection.Free;
  ClearMethod(FInternalKeyNotify);
  ClearMethod(FInternalValueNotify);
  ClearMethod(FInternalItemNotify);
  inherited;
end;

procedure TCustomDictionary<TKey,TValue>.Rehash(NewTableCount{power of 2}: NativeInt);
type
  THashList = array[0..0] of Pointer{PItem};
var
  NewListCount: NativeInt;
  NewHashTable: TArray<PItem>;

  i, HashTableMask, Index: NativeInt;
  Item: PItem;
  HashList: ^THashList;
begin
  // grow threshold
  NewListCount := NewTableCount shr 1 + NewTableCount shr 2; // 75%
  if (NewListCount < Self.FCount.Native) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));

  // reallocations
  if (NewTableCount < FHashTableMask + 1{Length(FHashTable)}) then
  begin
    ReallocMem(FList, NewListCount * SizeOf(TItem));
    SetLength(FHashTable, NewTableCount);
    NewHashTable := FHashTable;
  end else
  begin
    SetLength(NewHashTable, NewTableCount);
    ReallocMem(FList, NewListCount * SizeOf(TItem));
  end;

  // apply new
  FillChar(Pointer(NewHashTable)^, NewTableCount * SizeOf(Pointer), #0);
  FHashTable := NewHashTable;
  FListCount := NewListCount;
  FHashTableMask := NewTableCount - 1;

  // regroup items
  Item := Pointer(FList);
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

procedure TCustomDictionary<TKey, TValue>.TrimExcess;
begin
  SetCapacity(FCount.Int);
end;

procedure TCustomDictionary<TKey, TValue>.Clear;
begin
  if (FCount.Native <> 0) then
  begin
    Self.DoCleanupItems(Pointer(FList), FCount.Native);
  end;

  FCount.Native := 0;
  if (FHashTableMask + 1 = 4) then
  begin
    FillChar(Pointer(FHashTable), 4 * SizeOf(Pointer), #0);
  end else
  begin
    Rehash(4);
  end;
end;

procedure TCustomDictionary<TKey, TValue>.DoCleanupItems(Item: PItem; Count: NativeInt);
{$ifdef SMARTGENERICS}
type
  TKeyRec = packed record
  case Integer of
    0: (Native: NativeInt);
    1: (Method: TMethod);
    2: (VarData: TVarData);
  end;
  PKeyRec = ^TKeyRec;
  TValueRec = packed record
    Key: TKey;
  case Integer of
    0: (Native: NativeInt);
    1: (Method: TMethod);
    2: (VarData: TVarData);
  end;
  PValueRec = ^TValueRec;
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
      if (TMethod(FInternalItemNotify).Code = @TDictionary<TKey,TValue>.ItemNotifyCaller) then
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
      if (TMethod(FInternalKeyNotify).Code = @TDictionary<TKey,TKey>.KeyNotifyCaller) then
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
    if (TMethod(FInternalValueNotify).Code = @TDictionary<TKey,TValue>.ValueNotifyCaller) then
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
  if (System.IsManagedType(TKey) or System.HasWeakRef(TKey)) then
  {$else}
  if Assigned(TRAIIHelper<TKey>.FOptions.ClearProc) then
  {$endif}
  begin
    {$ifdef SMARTGENERICS}
    if (System.IsManagedType(TValue) or System.HasWeakRef(TValue)) then
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
          {$ifdef WEAKREF}
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
          {$ifdef WEAKREF}
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
          {$ifdef WEAKREF}
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

        Inc(Item);
      end;
    end;
  end else
  {$ifdef SMARTGENERICS}
  if (System.IsManagedType(TValue) or System.HasWeakRef(TValue)) then
  {$else}
  if Assigned(TRAIIHelper<TValue>.FOptions.ClearProc) then
  {$endif}
  begin
   // Values only
    for i := 1 to Count do
    begin
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
        {$ifdef WEAKREF}
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
  end;
end;

function TCustomDictionary<TKey, TValue>.NewItem: PItem;
label
  start;
type
  TKeyNative = packed record
    X: NativeInt;
  end;
  PKeyNative = ^TKeyNative;
  TKey8 = packed record
    X: Int64;
  end;
  PKey8 = ^TKey8;
  TKey12 = packed record
    X: Int64;
    Y: Integer;
  end;
  PKey12 = ^TKey12;
  TKey16 = packed record
    X: Int64;
    Y: Int64;
  end;
  PKey16 = ^TKey16;
  TValueNative = packed record
    Key: TKey;
    X: NativeInt;
  end;
  PValueNative = ^TValueNative;
  TValue8 = packed record
    Key: TKey;
    X: Int64;
  end;
  PValue8 = ^TValue8;
  TValue12 = packed record
    Key: TKey;
    X: Int64;
    Y: Integer;
  end;
  PValue12 = ^TValue12;
  TValue16 = packed record
    Key: TKey;
    X: Int64;
    Y: Int64;
  end;
  PValue16 = ^TValue16;
var
  Instance: TCustomDictionary<TKey, TValue>;
  Count: NativeInt;
begin
  Instance := Self;
start:
  Count := Instance.FCount.Native;
  if (Count <> Instance.FListCount) then
  begin
    Instance.FCount.Native := Count + 1;
    Result := @Instance.FList[Count];

    {$ifdef SMARTGENERICS}
    if (System.IsManagedType(TKey) or System.HasWeakRef(TKey)) then
    {$else}
    if (SizeOf(TKey) >= SizeOf(NativeInt)) then
    {$endif}
    begin
      if (SizeOf(TKey) = SizeOf(NativeInt)){$ifdef SMARTGENERICS}or (GetTypeKind(TKey) = tkVariant){$endif} then
      begin
        PKeyNative(Result).X := 0;
      end else
      if (SizeOf(TKey) <= 8) then
      begin
        PKey8(Result).X := 0;
      end else
      if (SizeOf(TKey) <= 12) then
      begin
        PKey12(Result).X := 0;
        PKey12(Result).Y := 0;
      end else
      if (SizeOf(TKey) <= 16) then
      begin
        PKey16(Result).X := 0;
        PKey16(Result).Y := 0;
      end else
      TRAIIHelper<TKey>.Init(@Result.FKey);
    end;

    {$ifdef SMARTGENERICS}
    if (System.IsManagedType(TValue) or System.HasWeakRef(TValue)) then
    {$else}
    if (SizeOf(TValue) >= SizeOf(NativeInt)) then
    {$endif}
    begin
      if (SizeOf(TValue) = SizeOf(NativeInt)){$ifdef SMARTGENERICS}or (GetTypeKind(TValue) = tkVariant){$endif} then
      begin
        PValueNative(Result).X := 0;
      end else
      if (SizeOf(TValue) <= 8) then
      begin
        PValue8(Result).X := 0;
      end else
      if (SizeOf(TValue) <= 12) then
      begin
        PValue12(Result).X := 0;
        PValue12(Result).Y := 0;
      end else
      if (SizeOf(TValue) <= 16) then
      begin
        PValue16(Result).X := 0;
        PValue16(Result).Y := 0;
      end else
      TRAIIHelper<TValue>.Init(@Result.FValue);
    end;

    Exit;
  end else
  begin
    Instance := Instance.Grow;
    goto start;
  end;
end;

procedure TCustomDictionary<TKey, TValue>.DisposeItem(Item: PItem);
type
  TKeyRec = packed record
  case Integer of
    0: (Native: NativeInt);
    1: (Method: TMethod);
    2: (VarData: TVarData);
  end;
  PKeyRec = ^TKeyRec;
  TValueRec = packed record
    Key: TKey;
  case Integer of
    0: (Native: NativeInt);
    1: (Method: TMethod);
    2: (VarData: TVarData);
  end;
  PValueRec = ^TValueRec;

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

var
  {$ifdef SMARTGENERICS}
  VType: Integer;
  {$endif}
  Count: NativeInt;
  Parent: ^PItem;
  TopItem, Current: PItem;
begin
  // Key
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
    {$ifdef WEAKREF}
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
    TRAIIHelper<TKey>.Clear(@Item.FKey);
  end;
  {$else}
  TRAIIHelper<TKey>.Clear(@Item.FKey);
  {$endif}

  // Value
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
    {$ifdef WEAKREF}
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
    TRAIIHelper<TValue>.Clear(@Item.FValue);
  end;
  {$else}
  TRAIIHelper<TValue>.Clear(@Item.FValue);
  {$endif}

  Count := Self.FCount.Native;
  Dec(Count);
  Self.FCount.Native := Count;
  TopItem := @FList[Count];
  if (Item <> TopItem) then
  begin
    // change TopItem.Parent.Next --> Item
    Parent := @FHashTable[NativeInt(Cardinal(TopItem.HashCode)) and FHashTableMask];
    repeat
      Current := Parent^;
      if (Current = TopItem) then
      begin
        Parent^ := Item;
        Break;
      end;
      Parent := @Current.FNext;
    until (False);

    // move TopItem --> Item
    case SizeOf(TItem) of
      1: T1(Pointer(Item)^) := T1(Pointer(TopItem)^);
      2: T2(Pointer(Item)^) := T2(Pointer(TopItem)^);
      3: T3(Pointer(Item)^) := T3(Pointer(TopItem)^);
      4: T4(Pointer(Item)^) := T4(Pointer(TopItem)^);
      5: T5(Pointer(Item)^) := T5(Pointer(TopItem)^);
      6: T6(Pointer(Item)^) := T6(Pointer(TopItem)^);
      7: T7(Pointer(Item)^) := T7(Pointer(TopItem)^);
      8: T8(Pointer(Item)^) := T8(Pointer(TopItem)^);
      9: T9(Pointer(Item)^) := T9(Pointer(TopItem)^);
     10: T10(Pointer(Item)^) := T10(Pointer(TopItem)^);
     11: T11(Pointer(Item)^) := T11(Pointer(TopItem)^);
     12: T12(Pointer(Item)^) := T12(Pointer(TopItem)^);
     13: T13(Pointer(Item)^) := T13(Pointer(TopItem)^);
     14: T14(Pointer(Item)^) := T14(Pointer(TopItem)^);
     15: T15(Pointer(Item)^) := T15(Pointer(TopItem)^);
     16: T16(Pointer(Item)^) := T16(Pointer(TopItem)^);
     17: T17(Pointer(Item)^) := T17(Pointer(TopItem)^);
     18: T18(Pointer(Item)^) := T18(Pointer(TopItem)^);
     19: T19(Pointer(Item)^) := T19(Pointer(TopItem)^);
     20: T20(Pointer(Item)^) := T20(Pointer(TopItem)^);
     21: T21(Pointer(Item)^) := T21(Pointer(TopItem)^);
     22: T22(Pointer(Item)^) := T22(Pointer(TopItem)^);
     23: T23(Pointer(Item)^) := T23(Pointer(TopItem)^);
     24: T24(Pointer(Item)^) := T24(Pointer(TopItem)^);
     25: T25(Pointer(Item)^) := T25(Pointer(TopItem)^);
     26: T26(Pointer(Item)^) := T26(Pointer(TopItem)^);
     27: T27(Pointer(Item)^) := T27(Pointer(TopItem)^);
     28: T28(Pointer(Item)^) := T28(Pointer(TopItem)^);
     29: T29(Pointer(Item)^) := T29(Pointer(TopItem)^);
     30: T30(Pointer(Item)^) := T30(Pointer(TopItem)^);
     31: T31(Pointer(Item)^) := T31(Pointer(TopItem)^);
     32: T32(Pointer(Item)^) := T32(Pointer(TopItem)^);
     33: T33(Pointer(Item)^) := T33(Pointer(TopItem)^);
     34: T34(Pointer(Item)^) := T34(Pointer(TopItem)^);
     35: T35(Pointer(Item)^) := T35(Pointer(TopItem)^);
     36: T36(Pointer(Item)^) := T36(Pointer(TopItem)^);
     37: T37(Pointer(Item)^) := T37(Pointer(TopItem)^);
     38: T38(Pointer(Item)^) := T38(Pointer(TopItem)^);
     39: T39(Pointer(Item)^) := T39(Pointer(TopItem)^);
     40: T40(Pointer(Item)^) := T40(Pointer(TopItem)^);
    else
      System.Move(TopItem^, Item^, SizeOf(TItem));
    end;
  end;
end;

function TCustomDictionary<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
label
  {$ifdef SMARTGENERICS}
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  next_item,
  {$endif}
  done;
type
  T16 = packed record
  case Integer of
    0: (Bytes: array[0..15] of Byte);
    1: (Words: array[0..7] of Word);
    2: (Integers: array[0..3] of Integer);
    3: (Int64s: array[0..1] of Int64);
  end;
  P16 = ^T16;
  PValue = ^TValue;
var
  i: NativeInt;
  Item: PValue;
  {$ifdef SMARTGENERICS}
  Left, Right: PByte;
  Count, Offset: NativeUInt;
  {$else}
  Comparer: IEqualityComparer<TValue>;
  ComparerEquals: function(const Left, Right: TValue): Boolean of object;
  {$endif}
begin
  Item := @Self.FList[0].FValue;

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
          if (PPointer(Left^)[vmtEquals div SizeOf(Pointer)] = @TObject.Equals) then
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
            if (InterfaceDefaults.TDefaultEqualityComparer<TValue>.Instance.Size < 0) then goto next_item;
            if (PDouble(@Value)^ <> PDouble(Result)^) then goto next_item;
          end;
        end;
      end else
      if (not (GetTypeKind(TValue) in [tkDynArray, tkString, tkLString, tkWString, tkUString])) and
        (SizeOf(TValue) <= 16) then
      begin
        // small binary
        if (SizeOf(TValue) <> 0) then
        with P16(@Value)^ do
        begin
          if (SizeOf(TValue) >= SizeOf(Integer)) then
          begin
            if (SizeOf(TValue) >= SizeOf(Int64)) then
            begin
              {$ifdef LARGEINT}
              if (Int64s[0] <> P16(Item).Int64s[0]) then goto next_item;
              {$else}
              if (Integers[0] <> P16(Item).Integers[0]) then goto next_item;
              if (Integers[1] <> P16(Item).Integers[1]) then goto next_item;
              {$endif}

              if (SizeOf(TValue) = 16) then
              begin
                {$ifdef LARGEINT}
                if (Int64s[1] <> P16(Item).Int64s[1]) then goto next_item;
                {$else}
                if (Integers[2] <> P16(Item).Integers[2]) then goto next_item;
                if (Integers[3] <> P16(Item).Integers[3]) then goto next_item;
                {$endif}
              end else
              if (SizeOf(TValue) >= 12) then
              begin
                if (Integers[2] <> P16(Item).Integers[2]) then goto next_item;
              end;
            end else
            begin
              if (Integers[0] <> P16(Item).Integers[0]) then goto next_item;
            end;
          end;

          if (SizeOf(TValue) and 2 <> 0) then
          begin
            if (Words[(SizeOf(TValue) and -4) shr 1] <> P16(Item).Words[(SizeOf(TValue) and -4) shr 1]) then goto next_item;
          end;
          if (SizeOf(TValue) and 1 <> 0) then
          begin
            if (Bytes[SizeOf(TValue) and -2] <> P16(Item).Bytes[SizeOf(TValue) and -2]) then goto next_item;
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
              NativeInt(Count) := NativeInt(Count) * InterfaceDefaults.TDefaultEqualityComparer<TKey>.Instance.Size;
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
          with P16(@Value)^ do
          begin
            {$ifdef LARGEINT}
            if (SizeOf(TValue) and 4 <> 0) then
            begin
              if (Integers[(SizeOf(TValue) and -8) shr 2] <> P16(Item).Integers[(SizeOf(TValue) and -8) shr 2]) then goto next_item;
            end;
            {$endif}
            if (SizeOf(TValue) and 2 <> 0) then
            begin
              if (Words[(SizeOf(TValue) and -4) shr 1] <> P16(Item).Words[(SizeOf(TValue) and -4) shr 1]) then goto next_item;
            end;
            if (SizeOf(TValue) and 1 <> 0) then
            begin
              if (Bytes[SizeOf(TValue) and -2] <> P16(Item).Bytes[SizeOf(TValue) and -2]) then goto next_item;
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
  end;
  {$else}
  begin
    Comparer := TEqualityComparer<TValue>.Default;
    TMethod(ComparerEquals) := IntfMethod(Pointer(Comparer), 3);
    for i := 1 to Self.FCount.Native do
    begin
      if (ComparerEquals(Value, Item^)) then goto done;
      Inc(NativeUInt(Item), SizeOf(TItem));
    end;
    ClearMethod(ComparerEquals);
    Result := False;
    Exit;
  end;
  {$endif}

done:
  {$ifNdef SMARTGENERICS}
  ClearMethod(ComparerEquals);
  {$endif}
  Result := True;
end;

class function TCustomDictionary<TKey, TValue>.IntfMethod(Intf: Pointer; MethodNum: NativeUInt): TMethod;
begin
  Result.Data := Intf;
  Result.Code := PPointer(PNativeUInt(Intf)^ + MethodNum * SizeOf(Pointer))^;
end;

class procedure TCustomDictionary<TKey, TValue>.ClearMethod(var Method);
begin
  {$ifdef WEAKREF}
    TMethod(Method).Data := nil;
  {$endif}
end;

procedure TCustomDictionary<TKey,TValue>.SetKeyNotify(const Value: TCollectionNotifyEvent<TKey>);
begin
  if (TMethod(FOnKeyNotify) <> TMethod(Value)) then
  begin
    FOnKeyNotify := Value;
    SetNotifyMethods;
  end;
end;

procedure TCustomDictionary<TKey,TValue>.SetValueNotify(const Value: TCollectionNotifyEvent<TValue>);
begin
  if (TMethod(FOnValueNotify) <> TMethod(Value)) then
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
  if (TMethod(VMTKeyNotify).Code <> @TDictionary<TKey,TValue>.KeyNotify) then
  begin
    // FInternalKeyNotify := Self.KeyNotifyCaller;
    TMethod(FInternalKeyNotify).Data := Pointer(Self);
    TMethod(FInternalKeyNotify).Code := @TDictionary<TKey,TValue>.KeyNotifyCaller;
  end else
  begin
    TMethod(FInternalKeyNotify) := TMethod(Self.FOnKeyNotify);
  end;
  if (TMethod(VMTValueNotify).Code <> @TDictionary<TKey,TValue>.ValueNotify) then
  begin
    // FInternalValueNotify := Self.ValueNotifyCaller;
    TMethod(FInternalValueNotify).Data := Pointer(Self);
    TMethod(FInternalValueNotify).Code := @TDictionary<TKey,TValue>.ValueNotifyCaller;
  end else
  begin
    TMethod(FInternalValueNotify) := TMethod(Self.FOnValueNotify);
  end;

  // FInternalItemNotify
  if Assigned(FInternalKeyNotify) then
  begin
    // FInternalItemNotify := Self.ItemNotifyKey;
    TMethod(FInternalItemNotify).Data := Pointer(Self);
    TMethod(FInternalItemNotify).Code := @TDictionary<TKey,TValue>.ItemNotifyKey;

    if Assigned(FInternalValueNotify) then
    begin
      // FInternalItemNotify := Self.ItemNotifyEvents;
      TMethod(FInternalItemNotify).Code := @TDictionary<TKey,TValue>.ItemNotifyEvents;

      if (TMethod(VMTKeyNotify).Code <> @TDictionary<TKey,TValue>.KeyNotify) or
        (TMethod(VMTValueNotify).Code <> @TDictionary<TKey,TValue>.ValueNotify) then
      begin
        // FInternalItemNotify := Self.ItemNotifyCaller;
        TMethod(FInternalItemNotify).Code := @TDictionary<TKey,TValue>.ItemNotifyCaller;
      end;
    end;
  end else
  if Assigned(FInternalValueNotify) then
  begin
    // FInternalItemNotify := Self.ItemNotifyValue;
    TMethod(FInternalItemNotify).Data := Pointer(Self);
    TMethod(FInternalItemNotify).Code := @TDictionary<TKey,TValue>.ItemNotifyValue;
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
    IInterface(FComparer) := IInterface(@InterfaceDefaults.TDefaultEqualityComparer<TKey>.Instance);

  // comparer methods
  TMethod(FComparerEquals) := IntfMethod(Pointer(FComparer), 3);
  TMethod(FComparerGetHashCode) := IntfMethod(Pointer(FComparer), 4);

  // initialization
  inherited Create(ACapacity);
end;

constructor TDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>);
begin
  Create(Collection, nil);
end;

constructor TDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>;
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

function TDictionary<TKey,TValue>.InternalFindItem(const Key: TKey; const FindMode: Integer): PItem;
label
  next_item, not_found;
var
  Parent: ^PItem;
  HashCode, Mode: Integer;
  Stored: record
    HashCode: Integer;
    Parent: ^PItem;
  end;
begin
  // hash code
  HashCode := Self.FComparerGetHashCode(Key);

  // parent
  Pointer(Result{Parent}) := @FHashTable[NativeInt(Cardinal(HashCode)) and FHashTableMask];
  Dec(NativeUInt(Result{Parent}), SizeOf(TKey) + SizeOf(TValue));

  // find
  Stored.HashCode := HashCode;
  repeat
  next_item:
    // hash code item
    repeat
      Parent := @Result.FNext;
      Result := Result.FNext;
      if (not Assigned(Result)) then goto not_found;
    until (Stored.HashCode = Result.HashCode);
    NativeUInt(Stored.Parent) := NativeUInt(Parent);

    // keys comparison
    if (not Self.FComparerEquals(Key, Result.Key)) then goto next_item;

    // found
    Mode := FindMode;
    if (Mode and FOUND_MASK = 0) then Exit;
    Cardinal(Mode) := Cardinal(Mode) and FOUND_MASK;
    if (Mode <> FOUND_EXCEPTION) then
    begin
      if (Mode = FOUND_DELETE) then
      begin
        Stored.Parent^ := Result.FNext;
        if (not Assigned(Self.FInternalItemNotify)) then
        begin
          Self.DisposeItem(Result);
        end else
        begin
          Self.FInternalItemNotify(Result^, cnRemoved);
          Self.DisposeItem(Result);
        end;
      end else
      // if (Mode = FOUND_REPLACE) then
      begin
        if (not Assigned(Self.FInternalValueNotify)) then
        begin
          Result.FValue := FInternalFindValue^;
        end else
        begin
          Self.FInternalValueNotify(Self, Result.Value, cnRemoved);
          Result.FValue := FInternalFindValue^;
          Self.FInternalValueNotify(Self, Result.Value, cnAdded);
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
      Result := Self.NewItem;
      Result.FKey := Key;
      Result.FHashCode := Stored.HashCode;
      Parent := @Self.FHashTable[NativeInt(Cardinal(Stored.HashCode)) and Self.FHashTableMask];
      Result.FNext := Parent^;
      Parent^ := Result;
      Result.FValue := FInternalFindValue^;
      if (Assigned(Self.FInternalItemNotify)) then
      begin
        Self.FInternalItemNotify(Result^, cnAdded);
      end;
    end else
    begin
      raise EListError.CreateRes(Pointer(@SGenericItemNotFound));
    end;
    Exit;
  until (False);
end;

function TDictionary<TKey,TValue>.GetItem(const Key: TKey): TValue;
begin
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_EXCEPTION).Value;
end;

procedure TDictionary<TKey,TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_REPLACE + EMPTY_EXCEPTION);
end;

function TDictionary<TKey,TValue>.Find(const Key: TKey): PItem;
begin
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
end;

function TDictionary<TKey,TValue>.FindOrAdd(const Key: TKey): PItem;
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
  Parent: ^PItem;
  Item, Current: PItem;
begin
  Result.Key := Key;
  Item := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
  if (Item = nil) then
  begin
    Result.Value := Default(TValue);
    Exit;
  end;

  Result.Value := Item.Value;
  Parent := @Self.FHashTable[NativeInt(Cardinal(Item.HashCode)) and Self.FHashTableMask];
  repeat
    Current := Parent^;

    if (Item = Current) then
    begin
      Parent^ := Item.FNext;

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

    Parent := @Current.FNext;
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
begin
  {$ifNdef SMARTGENERICS}
  FDefaultComparer := TEqualityComparer<TKey>.Default;
  TMethod(FDefaultEquals) := IntfMethod(Pointer(FDefaultComparer), 3);
  TMethod(FDefaultGetHashCode) := IntfMethod(Pointer(FDefaultComparer), 4);
  {$endif}

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

  {$ifNdef SMARTGENERICS}
  ClearMethod(FDefaultEquals);
  ClearMethod(FDefaultGetHashCode);
  {$endif}
end;

function TRapidDictionary<TKey,TValue>.InternalFindItem(const Key: TKey; const FindMode: Integer): PItem;
label
  {$ifdef SMARTGENERICS}
  hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8, hash9, hash10,
  hash_calculated,
  cmp0, cmp1, cmp2, cmp3, cmp4, cmp5, {$ifdef SMALLINT}cmp6, cmp7, cmp8, cmp9, cmp10,{$endif}
  {$endif}
  next_item, not_found;
type
  TSingleRec = packed record
    Exponent: Integer;
    case Integer of
      0: (Mantissa: Single);
      1: (HighInt: Integer);
  end;
  TDoubleRec = packed record
    Exponent: Integer;
    case Integer of
      0: (Mantissa: Double);
      1: (LowInt: Integer; HighInt: Integer);
  end;
  TExtendedRec = packed record
    Exponent: Integer;
    case Integer of
      0: (Mantissa: Extended);
      1: (LowInt: Integer; Middle: Word; HighInt: Integer);
  end;
  T16 = packed record
  case Integer of
    0: (Bytes: array[0..15] of Byte);
    1: (Words: array[0..7] of Word);
    2: (Integers: array[0..3] of Integer);
    3: (Int64s: array[0..1] of Int64);
  end;
  P16 = ^T16;
var
  Parent: ^PItem;
  HashCode, Mode, M: Integer;
  Stored: record
    HashCode: Integer;
    {$ifdef SMARTGENERICS}
    Self: TRapidDictionary<TKey,TValue>;
    {$endif}
    Parent: ^PItem;
  end;
  {$ifdef SMARTGENERICS}
  Left, Right: PByte;
  Count, Offset: NativeUInt;
  _Self1, _Self2: TRapidDictionary<TKey,TValue>;
  SingleRec: TSingleRec;
  DoubleRec: TDoubleRec;
  ExtendedRec: TExtendedRec;
  {$endif}
begin
  // stores, hash code
  {$ifdef SMARTGENERICS}
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
        Frexp(PSingle(@Key)^, SingleRec.Mantissa, SingleRec.Exponent);
        HashCode := SingleRec.Exponent + SingleRec.HighInt * 63689;
      end;
      10:
      begin
        if (PExtended(@Key)^ = 0) then goto hash_calculated;
        Frexp(PExtended(@Key)^, ExtendedRec.Mantissa, ExtendedRec.Exponent);
        HashCode := ExtendedRec.Exponent + ExtendedRec.LowInt * 63689 + ExtendedRec.HighInt * -1660269137 +
          Integer(ExtendedRec.Middle) * -1092754919;
      end;
    else
      if (InterfaceDefaults.TDefaultEqualityComparer<TKey>.Instance.Size < 0) then
      begin
        HashCode := PPoint(@Key).X + PPoint(@Key).Y * 63689;
      end else
      begin
        if (PDouble(@Key)^ = 0) then goto hash_calculated;
        Frexp(PDouble(@Key)^, DoubleRec.Mantissa, DoubleRec.Exponent);
        HashCode := DoubleRec.Exponent + DoubleRec.LowInt * 63689 + DoubleRec.HighInt * -1660269137;
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
          Count := PNativeInt(Left)^ * InterfaceDefaults.TDefaultEqualityComparer<TKey>.Instance.Size;
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
  {$else}
    HashCode := Self.FDefaultGetHashCode(Key);
  {$endif}
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
      Parent := @Result.FNext;
      Result := Result.FNext;
      if (not Assigned(Result)) then goto not_found;
    until (Stored.HashCode = Result.HashCode);
    NativeUInt(Stored.Parent) := NativeUInt(Parent);

    // default keys comparison
    {$ifdef SMARTGENERICS}
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
        if (PPointer(Left^)[vmtEquals div SizeOf(Pointer)] = @TObject.Equals) then
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
          if (InterfaceDefaults.TDefaultEqualityComparer<TKey>.Instance.Size < 0) then goto next_item;
          if (PDouble(@Key)^ <> PDouble(Result)^) then goto next_item;
        end;
      end;
    end else
    if (not (GetTypeKind(TKey) in [tkDynArray, tkString, tkLString, tkWString, tkUString])) and
      (SizeOf(TKey) <= 16) then
    begin
      // small binary
      if (SizeOf(TKey) <> 0) then
      with P16(@Key)^ do
      begin
        if (SizeOf(TKey) >= SizeOf(Integer)) then
        begin
          if (SizeOf(TKey) >= SizeOf(Int64)) then
          begin
            {$ifdef LARGEINT}
            if (Int64s[0] <> P16(Result).Int64s[0]) then goto next_item;
            {$else}
            if (Integers[0] <> P16(Result).Integers[0]) then goto next_item;
            if (Integers[1] <> P16(Result).Integers[1]) then goto next_item;
            {$endif}

            if (SizeOf(TKey) = 16) then
            begin
              {$ifdef LARGEINT}
              if (Int64s[1] <> P16(Result).Int64s[1]) then goto next_item;
              {$else}
              if (Integers[2] <> P16(Result).Integers[2]) then goto next_item;
              if (Integers[3] <> P16(Result).Integers[3]) then goto next_item;
              {$endif}
            end else
            if (SizeOf(TKey) >= 12) then
            begin
              if (Integers[2] <> P16(Result).Integers[2]) then goto next_item;
            end;
          end else
          begin
            if (Integers[0] <> P16(Result).Integers[0]) then goto next_item;
          end;
        end;

        if (SizeOf(TKey) and 2 <> 0) then
        begin
          if (Words[(SizeOf(TKey) and -4) shr 1] <> P16(Result).Words[(SizeOf(TKey) and -4) shr 1]) then goto next_item;
        end;
        if (SizeOf(TKey) and 1 <> 0) then
        begin
          if (Bytes[SizeOf(TKey) and -2] <> P16(Result).Bytes[SizeOf(TKey) and -2]) then goto next_item;
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
            NativeInt(Count) := NativeInt(Count) * InterfaceDefaults.TDefaultEqualityComparer<TKey>.Instance.Size;
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
        with P16(@Key)^ do
        begin
          {$ifdef LARGEINT}
          if (SizeOf(TKey) and 4 <> 0) then
          begin
            if (Integers[(SizeOf(TKey) and -8) shr 2] <> P16(Result).Integers[(SizeOf(TKey) and -8) shr 2]) then goto next_item;
          end;
          {$endif}
          if (SizeOf(TKey) and 2 <> 0) then
          begin
            if (Words[(SizeOf(TKey) and -4) shr 1] <> P16(Result).Words[(SizeOf(TKey) and -4) shr 1]) then goto next_item;
          end;
          if (SizeOf(TKey) and 1 <> 0) then
          begin
            if (Bytes[SizeOf(TKey) and -2] <> P16(Result).Bytes[SizeOf(TKey) and -2]) then goto next_item;
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
    {$else}
      if (not Self.FDefaultEquals(Key, Result.Key)) then goto next_item;
    {$endif}

    // found
    Mode := FindMode;
    if (Mode and FOUND_MASK = 0) then Exit;
    Cardinal(Mode) := Cardinal(Mode) and FOUND_MASK;
    if (Mode <> FOUND_EXCEPTION) then
    begin
      if (Mode = FOUND_DELETE) then
      begin
        Stored.Parent^ := Result.FNext;

        {$ifdef SMARTGENERICS}
        _Self1 := Stored.Self;
        with _Self1 do
        {$endif}
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
        {$ifdef SMARTGENERICS}
        _Self1 := Stored.Self;
        with _Self1 do
        {$endif}
        if (not Assigned(FInternalValueNotify)) then
        begin
          Result.FValue := FInternalFindValue^;
        end else
        begin
          FInternalValueNotify({$ifdef SMARTGENERICS}_Self1{$else}Self{$endif}, Result.Value, cnRemoved);
          Result.FValue := FInternalFindValue^;
          FInternalValueNotify({$ifdef SMARTGENERICS}_Self1{$else}Self{$endif}, Result.Value, cnAdded);
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
      {$ifdef SMARTGENERICS}
      _Self2 := Stored.Self;
      with _Self2 do
      {$endif}
      begin
        Result := NewItem;
        Result.FKey := Key;
        Result.FHashCode := Stored.HashCode;
        Parent := @FHashTable[NativeInt(Cardinal(Stored.HashCode)) and FHashTableMask];
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

function TRapidDictionary<TKey,TValue>.GetItem(const Key: TKey): TValue;
begin
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_EXCEPTION).Value;
end;

procedure TRapidDictionary<TKey,TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  Self.FInternalFindValue := @Value;
  Self.InternalFindItem(Key, FOUND_REPLACE + EMPTY_EXCEPTION);
end;

function TRapidDictionary<TKey,TValue>.Find(const Key: TKey): PItem;
begin
  Result := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
end;

function TRapidDictionary<TKey,TValue>.FindOrAdd(const Key: TKey): PItem;
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
  Parent: ^PItem;
  Item, Current: PItem;
begin
  Result.Key := Key;
  Item := Self.InternalFindItem(Key, FOUND_NONE + EMPTY_NONE);
  if (Item = nil) then
  begin
    Result.Value := Default(TValue);
    Exit;
  end;

  Result.Value := Item.Value;
  Parent := @Self.FHashTable[NativeInt(Cardinal(Item.HashCode)) and Self.FHashTableMask];
  repeat
    Current := Parent^;

    if (Item = Current) then
    begin
      Parent^ := Item.FNext;

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

    Parent := @Current.FNext;
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


{ TArray }

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>; Index,
  Count: Integer): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Count = 0 then
  begin
    FoundIndex := Index;
    Exit(False);
  end;

  Result := False;
  L := Index;
  H := Index + Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := Comparer.Compare(Values[mid], Item);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  FoundIndex := L;
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch<T>(Values, Item, FoundIndex, Comparer,
    Low(Values), Length(Values));
end;

class function TArray.BinarySearch<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer): Boolean;
begin
  Result := BinarySearch<T>(Values, Item, FoundIndex, TComparer<T>.Default,
    Low(Values), Length(Values));
end;

class procedure TArray.Copy<T>(const Source: array of T; var Destination: array of T; SourceIndex, DestIndex, Count: NativeInt);
begin
  CheckArrays(Pointer(@Source[0]), Pointer(@Destination[0]), SourceIndex, Length(Source), DestIndex, Length(Destination), Count);
  if IsManagedType(T) then
    System.CopyArray(Pointer(@Destination[DestIndex]), Pointer(@Source[SourceIndex]), TypeInfo(T), Count)
  else
    System.Move(Pointer(@Source[SourceIndex])^, Pointer(@Destination[DestIndex])^, Count * SizeOf(T));
end;

class procedure TArray.CheckArrays(Source, Destination: Pointer; SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt);
begin
  if (SourceIndex < 0) or (DestIndex < 0) or (SourceIndex >= SourceLength) or (DestIndex >= DestLength) or
     (SourceIndex + Count > SourceLength) or (DestIndex + Count > DestLength) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Source = Destination then
    raise EArgumentException.CreateRes(Pointer(@sSameArrays));
end;

class procedure TArray.Copy<T>(const Source: array of T; var Destination: array of T; Count: NativeInt);
begin
  Copy<T>(Source, Destination, 0, 0, Count);
end;

class procedure TArray.QuickSort<T>(var Values: array of T; const Comparer: IComparer<T>;
  L, R: Integer);
var
  I, J: Integer;
  pivot, temp: T;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1];
    repeat
      while Comparer.Compare(Values[I], pivot) < 0 do
        Inc(I);
      while Comparer.Compare(Values[J], pivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort<T>(Values, Comparer, L, J);
    L := I;
  until I >= R;
end;

class procedure TArray.Sort<T>(var Values: array of T);
begin
  QuickSort<T>(Values, TComparer<T>.Default, Low(Values), High(Values));
end;

class procedure TArray.Sort<T>(var Values: array of T; const Comparer: IComparer<T>);
begin
  QuickSort<T>(Values, Comparer, Low(Values), High(Values));
end;

class procedure TArray.Sort<T>(var Values: array of T; const Comparer: IComparer<T>;
  Index, Count: Integer);
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Count <= 1 then
    Exit;
  QuickSort<T>(Values, Comparer, Index, Index + Count - 1);
end;

{ TListHelper }

type
  TLocalDynArray = packed record
  {$IFDEF CPUX64}
    _Padding: Integer; // Make 16 byte align for payload..
  {$ENDIF}
    RefCnt: Integer;
    Length: NativeInt;
    Data: array[0..1023] of Byte;
  end;

procedure CopyArray(Dest, Source, TypeInfo: Pointer; ElemSize: Integer; Count: NativeInt);
begin
  if Count > 0 then
    if PByte(Dest) > PByte(Source) then
    begin
      Dest := PByte(Dest) + (Count - 1) * ElemSize;
      Source := PByte(Source) + (Count - 1) * ElemSize;
      while Count > 0 do
      begin
        System.CopyArray(Dest, Source, TypeInfo, 1);
        Dec(PByte(Dest), ElemSize);
        Dec(PByte(Source), ElemSize);
        Dec(Count);
      end;
    end else
      System.CopyArray(Dest, Source, TypeInfo, Count);
end;

function TListHelper.GetElSize: Integer;
begin
  Result := PDynArrayTypeInfo(PByte(FTypeInfo) + PDynArrayTypeInfo(FTypeInfo).name).elSize;
end;

function TListHelper.GetElType: Pointer;
begin
  Result := PDynArrayTypeInfo(PByte(FTypeInfo) + PDynArrayTypeInfo(FTypeInfo).name).elType^;
end;

function TListHelper.GetFItems: PPointer;
begin
  Result := PPointer(PByte(@Self) + SizeOf(Self));
end;

function TListHelper.CheckDeleteRange(AIndex, ACount: Integer): Boolean;
begin
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > FCount) or (AIndex + ACount < 0) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  Result := ACount > 0;
end;

procedure TListHelper.CheckItemRangeInline(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
end;

procedure TListHelper.CheckInsertRange(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex > FCount) then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
end;

procedure TListHelper.CheckItemRange(AIndex: Integer);
begin
  CheckItemRangeInline(AIndex);
end;

procedure TListHelper.SetItem1(const Value; AIndex: Integer);
var
  OldItem: Byte;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PByte(FItems^)[AIndex];
  PByte(FItems^)[AIndex] := Byte(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItem2(const Value; AIndex: Integer);
var
  OldItem: Word;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PWord(FItems^)[AIndex];
  PWord(FItems^)[AIndex] := Word(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItem4(const Value; AIndex: Integer);
var
  OldItem: Cardinal;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PCardinal(FItems^)[AIndex];
  PCardinal(FItems^)[AIndex] := Cardinal(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItem8(const Value; AIndex: Integer);
var
  OldItem: UInt64;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PUInt64(FItems^)[AIndex];
  PUInt64(FItems^)[AIndex] := UInt64(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemDynArray(const Value; AIndex: Integer);
type
  PBytes = ^TBytes;
var
  OldItem: Pointer;
begin
  OldItem := nil;
  try
    CheckItemRangeInline(AIndex);

    // Yes, this is "safe" to do without actually knowing the true dynamic array type for the assignments.
    // The first assignment is to a nil reference, so all that happens is the source array's reference count
    // is incremented. At this point I know that there are at least two references to the source array, so I
    // know that the second assignment won't try and deallocate the array. After the second assignment, the
    // old array reference will have at least 1 refcount.
    TBytes(OldItem) := PBytes(FItems^)[AIndex];
    PBytes(FItems^)[AIndex] := TBytes(Value);

    FNotify(OldItem, cnRemoved);
    FNotify(Value, cnAdded);
  finally
    // Here we do care about the type of the dynamic array since it is likely that the array will need to be
    // finalized. If the reference count of the array is 1, it will be properly freed here. If it is > 1, then
    // this will merely drop the local OldItem reference and set it to nil.
    // NOTE:  TBytes(OldItem) := nil; CANNOT be used here since that would pass in the type info for TBytes
    // into _DynArrayClear instead of the actual dynamic array type. Explicitly call DynArrayClear.
    DynArrayClear(OldItem, FTypeInfo);
  end;
end;

procedure TListHelper.DoSetItemInterface(const Value; AIndex: Integer);
var
  OldItem: IInterface;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PInterface(FItems^)[AIndex];
  PInterface(FItems^)[AIndex] := IInterface(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.SetItemManaged(const Value; AIndex: Integer);
var
  SOldItem: array[0..63] of Byte;
  DOldItem: PByte;
  POldItem: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);

  DOldItem := nil;
  POldItem := Pointer(@SOldItem);
  FillChar(SOldItem, SizeOf(SOldItem), 0);
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(SOldItem) then
    begin
      DOldItem := AllocMem(ElemSize);
      POldItem := DOldItem;
    end;
    System.CopyArray(POldItem, PByte(FItems^) + (AIndex * ElemSize), ElType, 1); // oldItem := FItems[Index];
    System.CopyArray(PByte(FItems^) + (AIndex * ElemSize), @Value, ElType, 1); // FItems[Index] := Value;

    FNotify(POldItem[0], cnRemoved);
    FNotify(Value, cnAdded);
  finally
    FinalizeArray(POldItem, ElType, 1);
    FreeMem(DOldItem);
  end;
end;

procedure TListHelper.SetItemN(const Value; AIndex: Integer);
var
  SOldItem: array[0..64] of Byte;
  DOldItem: PByte;
  POldItem: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);

  DOldItem := nil;
  POldItem := @SOldItem[0];
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(SOldItem) then
    begin
      GetMem(DOldItem, ElemSize);
      POldItem := DOldItem;
    end;
    Move(PByte(FItems^)[AIndex * ElemSize], POldItem[0], ElemSize);
    Move(Value, PByte(FItems^)[AIndex * ElemSize], ElemSize);

    FNotify(POldItem[0], cnRemoved);
    FNotify(Value, cnAdded);
  finally
    FreeMem(DOldItem);
  end;
end;

procedure TListHelper.DoSetItemString(const Value; AIndex: Integer);
var
  OldItem: string;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PString(FItems^)[AIndex];
  PString(FItems^)[AIndex] := string(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalExchange1(Index1, Index2: Integer);
var
  Temp: Byte;
begin
  Temp := PByte(FItems^)[Index1];
  PByte(FItems^)[Index1] := PByte(FItems^)[Index2];
  PByte(FItems^)[Index2] := Temp;
end;

procedure TListHelper.InternalExchange2(Index1, Index2: Integer);
var
  Temp: Word;
begin
  Temp := PWord(FItems^)[Index1];
  PWord(FItems^)[Index1] := PWord(FItems^)[Index2];
  PWord(FItems^)[Index2] := Temp;
end;

procedure TListHelper.InternalExchange4(Index1, Index2: Integer);
var
  Temp: Cardinal;
begin
  Temp := PCardinal(FItems^)[Index1];
  PCardinal(FItems^)[Index1] := PCardinal(FItems^)[Index2];
  PCardinal(FItems^)[Index2] := Temp;
end;

procedure TListHelper.InternalExchange8(Index1, Index2: Integer);
var
  Temp: UInt64;
begin
  Temp := PUInt64(FItems^)[Index1];
  PUInt64(FItems^)[Index1] := PUInt64(FItems^)[Index2];
  PUInt64(FItems^)[Index2] := Temp;
end;

procedure TListHelper.InternalExchangeManaged(Index1, Index2: Integer);
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  DTemp := nil;
  PTemp := Pointer(@STemp);
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(STemp) then
    begin
      DTemp := AllocMem(ElemSize);
      PTemp := DTemp;
    end else
      FillChar(STemp, ElemSize, 0);
    System.CopyArray(@PTemp[0], @PByte(FItems^)[Index1 * ElemSize], ElType, 1);
    System.CopyArray(@PByte(FItems^)[Index1 * ElemSize], @PByte(FItems^)[Index2 * ElemSize], ElType, 1);
    System.CopyArray(@PByte(FItems^)[Index2 * ElemSize], @PTemp[0], ElType, 1);
  finally
    FinalizeArray(PTemp, ElType, 1);
    FreeMem(DTemp);
  end;
end;

procedure TListHelper.InternalExchangeMRef(Index1, Index2: Integer; Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: DoExchangeString(Index1, Index2);
    TTypeKind.tkInterface: DoExchangeInterface(Index1, Index2);
    TTypeKind.tkVariant: DoExchangeVariant(Index1, Index2);
    TTypeKind.tkDynArray: DoExchangeDynArray(Index1, Index2);
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: DoExchangeObject(Index1, Index2);
{$IFEND}
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: DoExchangeAnsiString(Index1, Index2);
    TTypeKind.tkWString: DoExchangeWideString(Index1, Index2);
{$IFEND}
  end;
end;

procedure TListHelper.InternalExchangeN(Index1, Index2: Integer);
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  DTemp := nil;
  PTemp := Pointer(@STemp);
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(STemp) then
    begin
      GetMem(DTemp, ElemSize);
      PTemp := DTemp;
    end;
    Move(PByte(FItems^)[Index1 * ElemSize], PTemp[0], ElemSize);
    Move(PByte(FItems^)[Index2 * ElemSize], PByte(FItems^)[Index1 * ElemSize], ElemSize);
    Move(PTemp[0], PByte(FItems^)[Index2 * ElemSize], ElemSize);
  finally
    FreeMem(DTemp);
  end;
end;

procedure TListHelper.InternalExtractItem1(const Value; out Item; Direction: Byte);
begin
  if Direction = Byte(TDirection.FromBeginning) then
    DoExtractItemFwd1(Value, Item)
  else
    DoExtractItemRev1(Value, Item);
end;

procedure TListHelper.InternalExtractItem2(const Value; out Item; Direction: Byte);
begin
  if Direction = Byte(TDirection.FromBeginning) then
    DoExtractItemFwd2(Value, Item)
  else
    DoExtractItemRev2(Value, Item);
end;

procedure TListHelper.InternalExtractItem4(const Value; out Item; Direction: Byte);
begin
  if Direction = Byte(TDirection.FromBeginning) then
    DoExtractItemFwd4(Value, Item)
  else
    DoExtractItemRev4(Value, Item);
end;

procedure TListHelper.InternalExtractItem8(const Value; out Item; Direction: Byte);
begin
  if Direction = Byte(TDirection.FromBeginning) then
    DoExtractItemFwd8(Value, Item)
  else
    DoExtractItemRev8(Value, Item);
end;

procedure TListHelper.InternalExtractItemManaged(const Value; out Item; Direction: Byte);
begin
  if Direction = Byte(TDirection.FromBeginning) then
    DoExtractItemFwdManaged(Value, Item)
  else
    DoExtractItemRevManaged(Value, Item);
end;

procedure TListHelper.InternalExtractItemMRef(const Value; Kind: TTypeKind; out Item; Direction: Byte);
begin
  case Kind of
    TTypeKind.tkUString:
      if Direction = Byte(TDirection.FromBeginning) then
        DoExtractItemFwdString(Value, Item)
      else
        DoExtractItemRevString(Value, Item);
    TTypeKind.tkInterface:
      if Direction = Byte(TDirection.FromBeginning) then
        DoExtractItemFwdInterface(Value, Item)
      else
        DoExtractItemRevInterface(Value, Item);
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkString:
      if Direction = Byte(TDirection.FromBeginning) then
        DoExtractItemFwdAnsiString(Value, Item)
      else
        DoExtractItemRevAnsiString(Value, Item);
    TTypeKind.tkWString:
      if Direction = Byte(TDirection.FromBeginning) then
        DoExtractItemFwdWideString(Value, Item)
      else
        DoExtractItemRevWideString(Value, Item);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass:
      if Direction = Byte(TDirection.FromBeginning) then
        DoExtractItemFwdObject(Value, Item)
      else
        DoExtractItemRevObject(Value, Item);
{$IFEND}
  end;
end;

procedure TListHelper.InternalExtractItemN(const Value; out Item; Direction: Byte);
begin
  if Direction = Byte(TDirection.FromBeginning) then
    DoExtractItemFwdN(Value, Item)
  else
    DoExtractItemRevN(Value, Item);
end;

procedure TListHelper.InternalExtractItemVariant(const Value; out Item; Direction: Byte);
begin
  if Direction = Byte(TDirection.FromBeginning) then
    DoExtractItemFwdVariant(Value, Item)
  else
    DoExtractItemRevVariant(Value, Item);
end;

procedure TListHelper.SetItemVariant(const Value; AIndex: Integer);
var
  OldItem: Variant;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PVariant(FItems^)[AIndex];
  PVariant(FItems^)[AIndex] := Variant(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

{$IF Defined(AUTOREFCOUNT)}
procedure TListHelper.DoSetItemObject(const Value; AIndex: Integer);
var
  OldItem: TObject;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PObject(FItems^)[AIndex];
  PObject(FItems^)[AIndex] := TObject(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoInsertObject(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PObject(FItems^)[AIndex] := TObject(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddObject(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PObject(FItems^)[FCount] := TObject(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;
{$IFEND}

{$IF not Defined(NEXTGEN)}
procedure TListHelper.DoSetItemAnsiString(const Value; AIndex: Integer);
var
  OldItem: AnsiString;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PAnsiString(FItems^)[AIndex];
  PAnsiString(FItems^)[AIndex] := AnsiString(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoSetItemWideString(const Value; AIndex: Integer);
var
  OldItem: WideString;
begin
  CheckItemRangeInline(AIndex);

  OldItem := PWideString(FItems^)[AIndex];
  PWideString(FItems^)[AIndex] := WideString(Value);

  FNotify(OldItem, cnRemoved);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoInsertAnsiString(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Pointer));
  PAnsiString(FItems^)[AIndex] := AnsiString(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoInsertWideString(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Pointer));
  PWideString(FItems^)[AIndex] := WideString(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddAnsiString(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PAnsiString(FItems^)[FCount] := AnsiString(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddWideString(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PWideString(FItems^)[FCount] := WideString(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;
{$IFEND}

function TListHelper.DoAddDynArray(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PBytes(FItems^)[FCount] := TBytes(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddInterface(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PInterface(FItems^)[FCount] := IInterface(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.DoAddString(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PString(FItems^)[FCount] := string(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoExchangeInterfaceInline(Index1, Index2: Integer);
var
  Temp: IInterface;
begin
  Temp := PInterface(FItems^)[Index1];
  PInterface(FItems^)[Index1] := PInterface(FItems^)[Index2];
  PInterface(FItems^)[Index2] := Temp;
end;

procedure TListHelper.DoExchangeStringInline(Index1, Index2: Integer);
var
  Temp: string;
begin
  Temp := PString(FItems^)[Index1];
  PString(FItems^)[Index1] := PString(FItems^)[Index2];
  PString(FItems^)[Index2] := Temp;
end;

procedure TListHelper.DoExchangeVariantInline(Index1, Index2: Integer);
var
  Temp: Variant;
begin
  Temp := PVariant(FItems^)[Index1];
  PVariant(FItems^)[Index1] := PVariant(FItems^)[Index2];
  PVariant(FItems^)[Index2] := Temp;
end;

procedure TListHelper.DoExchangeDynArrayInline(Index1, Index2: Integer);
var
  Temp: TBytes;
begin
  Temp := PBytes(FItems^)[Index1];
  PBytes(FItems^)[Index1] := PBytes(FItems^)[Index2];
  PBytes(FItems^)[Index2] := Temp;
end;

{$IF not Defined(NEXTGEN)}
procedure TListHelper.DoExchangeAnsiStringInline(Index1, Index2: Integer);
var
  Temp: AnsiString;
begin
  Temp := PAnsiString(FItems^)[Index1];
  PAnsiString(FItems^)[Index1] := PAnsiString(FItems^)[Index2];
  PAnsiString(FItems^)[Index2] := Temp;
end;

procedure TListHelper.DoExchangeWideStringInline(Index1, Index2: Integer);
var
  Temp: WideString;
begin
  Temp := PWideString(FItems^)[Index1];
  PWideString(FItems^)[Index1] := PWideString(FItems^)[Index2];
  PWideString(FItems^)[Index2] := Temp;
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TListHelper.DoExchangeObjectInline(Index1, Index2: Integer);
var
  Temp: TObject;
begin
  Temp := PObject(FItems^)[Index1];
  PObject(FItems^)[Index1] := PObject(FItems^)[Index2];
  PObject(FItems^)[Index2] := Temp;
end;
{$IFEND}

procedure TListHelper.DoExchangeInterface(Index1, Index2: Integer);
begin
  DoExchangeInterfaceInline(Index1, Index2);
end;

procedure TListHelper.DoExchangeString(Index1, Index2: Integer);
begin
  DoExchangeStringInline(Index1, Index2);
end;

procedure TListHelper.DoExchangeVariant(Index1, Index2: Integer);
begin
  DoExchangeVariantInline(Index1, Index2);
end;

procedure TListHelper.DoExchangeDynArray(Index1, Index2: Integer);
begin
  DoExchangeDynArrayInline(Index1, Index2);
end;

{$IF not Defined(NEXTGEN)}
procedure TListHelper.DoExchangeAnsiString(Index1, Index2: Integer);
begin
  DoExchangeAnsiStringInline(Index1, Index2);
end;

procedure TListHelper.DoExchangeWideString(Index1, Index2: Integer);
begin
  DoExchangeWideStringInline(Index1, Index2);
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TListHelper.DoExchangeObject(Index1, Index2: Integer);
begin
  DoExchangeObjectInline(Index1, Index2);
end;
{$IFEND}

procedure TListHelper.DoExtractItemFwd1(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwd1(Value);
  if Index < 0 then
    Byte(Item) := 0
  else
  begin
    Byte(Item) := PByte(FItems^)[Index];
    InternalDoDelete1(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRev1(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRev1(Value);
  if Index < 0 then
    Byte(Item) := 0
  else
  begin
    Byte(Item) := PByte(FItems^)[Index];
    InternalDoDelete1(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwd2(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwd2(Value);
  if Index < 0 then
    Word(Item) := 0
  else
  begin
    Word(Item) := PWord(FItems^)[Index];
    InternalDoDelete2(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRev2(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRev2(Value);
  if Index < 0 then
    Word(Item) := 0
  else
  begin
    Word(Item) := PWord(FItems^)[Index];
    InternalDoDelete2(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwd4(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwd4(Value);
  if Index < 0 then
    Cardinal(Item) := 0
  else
  begin
    Cardinal(Item) := PCardinal(FItems^)[Index];
    InternalDoDelete4(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRev4(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRev4(Value);
  if Index < 0 then
    Cardinal(Item) := 0
  else
  begin
    Cardinal(Item) := PCardinal(FItems^)[Index];
    InternalDoDelete4(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwd8(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwd8(Value);
  if Index < 0 then
    UInt64(Item) := 0
  else
  begin
    UInt64(Item) := PUInt64(FItems^)[Index];
    InternalDoDelete8(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRev8(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRev8(Value);
  if Index < 0 then
    UInt64(Item) := 0
  else
  begin
    UInt64(Item) := PUInt64(FItems^)[Index];
    InternalDoDelete8(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwdN(const Value; out Item);
var
  Index, ElemSize: Integer;
begin
  Index := DoIndexOfFwdN(Value);
  ElemSize := ElSize;
  if Index < 0 then
    FillChar(Item, ElemSize, 0)
  else
  begin
    Move(PByte(FItems^)[Index * ElemSize], Item, ElemSize);
    InternalDoDeleteN(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevN(const Value; out Item);
var
  Index, ElemSize: Integer;
begin
  Index := DoIndexOfRevN(Value);
  ElemSize := ElSize;
  if Index < 0 then
    FillChar(Item, ElemSize, 0)
  else
  begin
    Move(PByte(FItems^)[Index * ElemSize], Item, ElemSize);
    InternalDoDeleteN(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwdInterface(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwdMRef(Value);
  if Index < 0 then
    IInterface(Item) := nil
  else
  begin
    IInterface(Item) := PInterface(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevInterface(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRevMRef(Value);
  if Index < 0 then
    IInterface(Item) := nil
  else
  begin
    IInterface(Item) := PInterface(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwdManaged(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwdN(Value);
  if Index < 0 then
    FinalizeArray(@Item, ElType, 1)
  else
  begin
    System.CopyArray(@Item, PByte(FItems^) + (Index * ElSize), ElType, 1);
    InternalDoDeleteManaged(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevManaged(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRevN(Value);
  if Index < 0 then
    FinalizeArray(@Item, ElType, 1)
  else
  begin
    System.CopyArray(@Item, PByte(FItems^) + (Index * ElSize), ElType, 1);
    InternalDoDeleteManaged(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwdString(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwdMRef(Value);
  if Index < 0 then
    string(Item) := ''
  else
  begin
    string(Item) := PString(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevString(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRevMRef(Value);
  if Index < 0 then
    string(Item) := ''
  else
  begin
    string(Item) := PString(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwdVariant(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwdN(Value);
  if Index < 0 then
    VarClear(Variant(Item))
  else
  begin
    Variant(Item) := PVariant(FItems^)[Index];
    InternalDoDeleteManaged(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevVariant(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRevN(Value);
  if Index < 0 then
    VarClear(Variant(Item))
  else
  begin
    Variant(Item) := PVariant(FItems^)[Index];
    InternalDoDeleteManaged(Index, cnExtracted);
  end;
end;

{$IF Defined(NEXTGEN)}
procedure TListHelper.DoExtractItemFwdObject(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwdMRef(Value);
  if Index < 0 then
    TObject(Item) := nil
  else
  begin
    TObject(Item) := PObject(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevObject(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRevMRef(Value);
  if Index < 0 then
    TObject(Item) := nil
  else
  begin
    TObject(Item) := PObject(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;
{$IFEND}

{$IF not Defined(AUTOREFCOUNT)}
procedure TListHelper.DoExtractItemFwdAnsiString(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwdMRef(Value);
  if Index < 0 then
    AnsiString(Item) := ''
  else
  begin
    AnsiString(Item) := PAnsiString(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevAnsiString(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRevMRef(Value);
  if Index < 0 then
    AnsiString(Item) := ''
  else
  begin
    AnsiString(Item) := PAnsiString(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemFwdWideString(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfFwdMRef(Value);
  if Index < 0 then
    WideString(Item) := ''
  else
  begin
    WideString(Item) := PWideString(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;

procedure TListHelper.DoExtractItemRevWideString(const Value; out Item);
var
  Index: Integer;
begin
  Index := DoIndexOfRevMRef(Value);
  if Index < 0 then
    WideString(Item) := ''
  else
  begin
    WideString(Item) := PWideString(FItems^)[Index];
    InternalDoDeleteMRef(Index, cnExtracted);
  end;
end;
{$IFEND}

function TListHelper.DoIndexOfFwd1(const Value): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCompare(PByte(FItems^)[I], Byte(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfFwd2(const Value): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCompare(PWord(FItems^)[I], Word(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfFwd4(const Value): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCompare(PCardinal(FItems^)[I], Cardinal(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfFwd8(const Value): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCompare(PUInt64(FItems^)[I], UInt64(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfFwdMRef(const Value): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCompare(PPointer(FItems^)[I], Value) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfFwdN(const Value): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCompare(PByte(FItems^)[I * ElSize], Byte(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfRev1(const Value): Integer;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if FCompare(PByte(FItems^)[I], Byte(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfRev2(const Value): Integer;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if FCompare(PWord(FItems^)[I], Word(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfRev4(const Value): Integer;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if FCompare(PCardinal(FItems^)[I], Cardinal(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfRev8(const Value): Integer;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if FCompare(PUInt64(FItems^)[I], UInt64(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfRevMRef(const Value): Integer;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if FCompare(PPointer(FItems^)[I], Value) = 0 then
      Exit(I);
  Result := -1;
end;

function TListHelper.DoIndexOfRevN(const Value): Integer;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if FCompare(PByte(FItems^)[I * ElSize], Byte(Value)) = 0 then
      Exit(I);
  Result := -1;
end;

procedure TListHelper.DoInsertDynArray(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PBytes(FItems^)[AIndex] := TBytes(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoInsertInterface(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PInterface(FItems^)[AIndex] := IInterface(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.DoInsertString(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PPointer(FItems^)[AIndex], PPointer(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Pointer));
  PPointer(FItems^)[AIndex] := nil;
  PString(FItems^)[AIndex] := string(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.DoRemoveFwd1(const Value): Integer;
begin
  Result := DoIndexOfFwd1(Value);
  if Result >= 0 then
    InternalDoDelete1(Result, cnRemoved);
end;

function TListHelper.DoRemoveFwd2(const Value): Integer;
begin
  Result := DoIndexOfFwd2(Value);
  if Result >= 0 then
    InternalDoDelete2(Result, cnRemoved);
end;

function TListHelper.DoRemoveFwd4(const Value): Integer;
begin
  Result := DoIndexOfFwd4(Value);
  if Result >= 0 then
    InternalDoDelete4(Result, cnRemoved);
end;

function TListHelper.DoRemoveFwd8(const Value): Integer;
begin
  Result := DoIndexOfFwd8(Value);
  if Result >= 0 then
    InternalDoDelete8(Result, cnRemoved);
end;

function TListHelper.DoRemoveFwdMRef(const Value): Integer;
begin
  Result := DoIndexOfFwdMRef(Value);
  if Result >= 0 then
    InternalDoDeleteMRef(Result, cnRemoved);
end;

function TListHelper.DoRemoveFwdN(const Value): Integer;
begin
  Result := DoIndexOfFwdN(Value);
  if Result >= 0 then
    InternalDoDeleteN(Result, cnRemoved);
end;

function TListHelper.DoRemoveRev1(const Value): Integer;
begin
  Result := DoIndexOfRev1(Value);
  if Result >= 0 then
    InternalDoDelete1(Result, cnRemoved);
end;

function TListHelper.DoRemoveRev2(const Value): Integer;
begin
  Result := DoIndexOfRev2(Value);
  if Result >= 0 then
    InternalDoDelete2(Result, cnRemoved);
end;

function TListHelper.DoRemoveRev4(const Value): Integer;
begin
  Result := DoIndexOfRev4(Value);
  if Result >= 0 then
    InternalDoDelete4(Result, cnRemoved);
end;

function TListHelper.DoRemoveRev8(const Value): Integer;
begin
  Result := DoIndexOfRev8(Value);
  if Result >= 0 then
    InternalDoDelete8(Result, cnRemoved);
end;

function TListHelper.DoRemoveRevMRef(const Value): Integer;
begin
  Result := DoIndexOfRevMRef(Value);
  if Result >= 0 then
    InternalDoDeleteMRef(Result, cnRemoved);
end;

function TListHelper.DoRemoveRevN(const Value): Integer;
begin
  Result := DoIndexOfRevN(Value);
  if Result >= 0 then
    InternalDoDeleteN(Result, cnRemoved);
end;

procedure TListHelper.DoReverseMRef(Kind: TTypeKind);
var
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    case Kind of
      TTypeKind.tkUString: DoExchangeStringInline(b, e);
      TTypeKind.tkInterface: DoExchangeInterfaceInline(b, e);
      TTypeKind.tkDynArray: DoExchangeDynArrayInline(b, e);
      TTypeKind.tkVariant: DoExchangeVariantInline(b, e);
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkLString: DoExchangeAnsiStringInline(b, e);
      TTypeKind.tkWString: DoExchangeWideStringInline(b, e);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass: DoExchangeObjectInline(b, e);
{$IFEND}
    end;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.DoReverseString;
begin
  DoReverseMRef(TTypeKind.tkUString);
end;

procedure TListHelper.DoReverseVariant;
begin
  DoReverseMRef(TTypeKind.tkVariant);
end;

procedure TListHelper.DoReverseDynArray;
begin
  DoReverseMRef(TTypeKind.tkDynArray);
end;

procedure TListHelper.DoReverseInterface;
begin
  DoReverseMRef(TTypeKind.tkInterface);
end;

{$IF not Defined(NEXTGEN)}
procedure TListHelper.DoReverseWideString;
begin
  DoReverseMRef(TTypeKind.tkWString);
end;

procedure TListHelper.DoReverseAnsiString;
begin
  DoReverseMRef(TTypeKind.tkLString);
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TListHelper.DoReverseObject;
begin
  DoReverseMRef(TTypeKind.tkClass);
end;
{$IFEND}

function TListHelper.InternalAdd1(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PByte(FItems^)[FCount] := Byte(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAdd2(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PWord(FItems^)[FCount] := Word(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAdd4(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PCardinal(FItems^)[FCount] := Cardinal(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAdd8(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PUInt64(FItems^)[FCount] := UInt64(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAddManaged(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  System.CopyArray(PByte(FItems^) + (FCount * ElSize), @Value, ElType, 1);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

function TListHelper.InternalAddMRef(const Value; TypeKind: TTypeKind): Integer;
begin
  if IsConstValue(TypeKind) then
  begin
    case TypeKind of
      TTypeKind.tkUString: Result := DoAddString(Value);
      TTypeKind.tkDynArray: Result := DoAddDynArray(Value);
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass: Result := DoAddObject(Value);
{$IFEND}
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkLString: Result := DoAddAnsiString(Value);
      TTypeKind.tkWString: Result := DoAddWideString(Value);
{$IFEND}
    else
      { TTypeKind.tkInterface: } Result := DoAddInterface(Value);
    end;
  end else
  begin
    Result := -1;
    if Result = -1 then System.Error(rePlatformNotImplemented);
  end;
end;

procedure TListHelper.SetItemMRef(const Value; AIndex: Integer; TypeKind: TTypeKind);
begin
  if IsConstValue(TypeKind) then
  begin
    case TypeKind of
      TTypeKind.tkUString: DoSetItemString(Value, AIndex);
      TTypeKind.tkDynArray: DoSetItemDynArray(Value, AIndex);
      TTypeKind.tkInterface: DoSetItemInterface(Value, AIndex);
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass: DoSetItemObject(Value, AIndex);
{$IFEND}
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkLString: DoSetItemAnsiString(Value, AIndex);
      TTypeKind.tkWString: DoSetItemWideString(Value, AIndex);
{$IFEND}
    end;
  end else
    System.Error(rePlatformNotImplemented);
end;

function TListHelper.InternalAddVariant(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  PVariant(FItems^)[FCount] := Variant(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalClear1;
begin
  InternalSetCount1(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClear2;
begin
  InternalSetCount2(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClear4;
begin
  InternalSetCount4(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClear8;
begin
  InternalSetCount8(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClearManaged;
begin
  InternalSetCountManaged(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClearMRef;
begin
  InternalSetCountMRef(0);
  InternalSetCapacity(0);
end;

procedure TListHelper.InternalClearN;
begin
  InternalSetCountN(0);
  InternalSetCapacity(0);
end;

{$IF Defined(WEAKREF)}
procedure TListHelper.InternalClearWeak;
begin
  InternalSetCountWeak(0);
  InternalSetCapacity(0);
end;
{$IFEND}

function TListHelper.InternalAddN(const Value): Integer;
begin
  InternalGrowCheck(FCount + 1);
  Result := FCount;
  Move(Value, PByte(FItems^)[FCount * ElSize], ElSize);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalDeleteRange1(AIndex, ACount: Integer);
var
  SArray: array[0..1023] of Byte;
  DArray: array of Byte;
  PElem: PByte;
  tailCount, Size: NativeInt;
  I: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end else
      PElem := @SArray[0];
    Size := ACount * SizeOf(Byte);
    Move(PByte(FItems^)[AIndex], PElem[0], Size);

    tailCount := FCount - (AIndex + ACount);
    if tailCount > 0 then
    begin
      Move(PByte(FItems^)[AIndex + ACount], PByte(FItems^)[AIndex], tailCount * SizeOf(Byte));
      Inc(AIndex, tailCount);
    end;
    FillChar(PByte(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

    for I := 0 to ACount - 1 do
      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRange2(AIndex, ACount: Integer);
var
  SArray: array[0..511] of Word;
  DArray: array of Word;
  PElem: PWord;
  tailCount, Size: NativeInt;
  I: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end else
      PElem := @SArray[0];
    Size := ACount * SizeOf(Word);
    Move(PWord(FItems^)[AIndex], PElem[0], Size);

    tailCount := FCount - (AIndex + ACount);
    if tailCount > 0 then
    begin
      Move(PWord(FItems^)[AIndex + ACount], PWord(FItems^)[AIndex], tailCount * SizeOf(Word));
      Inc(AIndex, tailCount);
    end;
    FillChar(PWord(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

    for I := 0 to ACount - 1 do
      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRange4(AIndex, ACount: Integer);
var
  SArray: array[0..255] of Cardinal;
  DArray: array of Cardinal;
  PElem: PCardinal;
  tailCount, Size: NativeInt;
  I: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end else
      PElem := @SArray[0];
    Size := ACount * SizeOf(Cardinal);
    Move(PCardinal(FItems^)[AIndex], PElem[0], Size);

    tailCount := (FCount - (AIndex + ACount));
    if tailCount > 0 then
    begin
      Move(PCardinal(FItems^)[AIndex + ACount], PCardinal(FItems^)[AIndex], tailCount * SizeOf(Cardinal));
      Inc(AIndex, tailCount);
    end;
    FillChar(PCardinal(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

    for I := 0 to ACount - 1 do
      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRange8(AIndex, ACount: Integer);
var
  SArray: array[0..127] of UInt64;
  DArray: array of UInt64;
  PElem: PUInt64;
  tailCount, Size: NativeInt;
  I: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    if ACount > Length(SArray) then
    begin
      SetLength(DArray, ACount);
      PElem := @DArray[0];
    end else
      PElem := @SArray[0];
    Size := ACount * SizeOf(UInt64);
    Move(PUInt64(FItems^)[AIndex], PElem[0], Size);

    tailCount := FCount - (AIndex + ACount);
    if tailCount > 0 then
    begin
      Move(PUInt64(FItems^)[AIndex + ACount], PUInt64(FItems^)[AIndex], tailCount * SizeOf(UInt64));
      Inc(AIndex, tailCount);
    end;
    FillChar(PUInt64(FItems^)[AIndex], Size, 0);

    Dec(FCount, ACount);

    for I := 0 to ACount - 1 do
      FNotify(PElem[I], cnRemoved);
  end;
end;

procedure TListHelper.InternalDeleteRangeManaged(AIndex, ACount: Integer);
var
  SArray: array[0..1023] of Byte;
  DArray: Pointer;
  PElem: PByte;
  tailCount, Size: NativeInt;
  I, ElemSize: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    ElemSize := ElSize;
    DArray := nil; // initialize the local dynarray
    PElem := @SArray[0];
    try
      Size := ACount * ElemSize;
      if Size > Length(SArray) then
      begin
        GetMem(DArray, Size);
        PElem := DArray;
      end;
      Move(PByte(FItems^)[AIndex * ElemSize], PElem[0], Size);

      tailCount := (FCount - (AIndex + ACount)) * ElemSize;
      if tailCount > 0 then
      begin
        Move(PByte(FItems^)[(AIndex + ACount) * ElemSize], PByte(FItems^)[AIndex * ElemSize], tailCount);
        FillChar(PByte(FItems^)[(FCount - ACount) * ElemSize], Size, 0);
      end else
        FillChar(PByte(FItems^)[AIndex * ElemSize], Size, 0);

      Dec(FCount, ACount);

      for I := 0 to ACount - 1 do
        FNotify(PElem[I * ElemSize], cnRemoved);
    finally
      if DArray <> nil then
      begin
        FinalizeArray(DArray, ElType, ACount);
        FreeMem(DArray);
      end else
        FinalizeArray(PElem, ElType, ACount);
    end;
  end;
end;

procedure TListHelper.InternalDeleteRangeMRef(AIndex, ACount: Integer);
var
  SArray: array[0..(1024 div SizeOf(Pointer)) - 1] of NativeInt;
  DArray: Pointer;
  PElem: PPointer;
  tailCount, Size: NativeInt;
  I: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    DArray := nil;
    PElem := Pointer(@SArray[0]);
    try
      Size := ACount;
      if Size > Length(SArray) then
      begin
        DynArraySetLength(DArray, FTypeInfo, 1, @Size);
        PElem := DArray;
      end;
      Move(PPointer(FItems^)[AIndex], PElem[0], ACount * SizeOf(Pointer));

      tailCount := FCount - (AIndex + ACount);
      if tailCount > 0 then
      begin
        Move(PPointer(FItems^)[AIndex + ACount], PPointer(FItems^)[AIndex], tailCount * SizeOf(Pointer));
        FillChar(PPointer(FItems^)[FCount - ACount], ACount * SizeOf(Pointer), 0);
      end else
        FillChar(PPointer(FItems^)[AIndex], ACount * SizeOf(Pointer), 0);

      Dec(FCount, ACount);

      for I := 0 to ACount - 1 do
        FNotify(PElem[I], cnRemoved);
    finally
      if DArray = nil then
        FinalizeArray(PElem, ElType, Size)
      else
        DynArrayClear(DArray, FTypeInfo);
    end;
  end;
end;

{$IF Defined(WEAKREF)}
procedure TListHelper.InternalDeleteRangeWeak(AIndex, ACount: Integer);
var
  SArray: TLocalDynArray;
  DArray: Pointer;
  PElem: PByte;
  tailCount, Size: NativeInt;
  I, ElemSize: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    ElemSize := ElSize;
    DArray := nil; // initialize the local dynarray
    PElem := @SArray.Data[0];
    try
      Size := ACount;
      if (Size * ElemSize) > Length(SArray.Data) then
      begin
        DynArraySetLength(DArray, FTypeInfo, 1, @Size);
        PElem := DArray;
      end else
      begin
        FillChar(SArray, SizeOf(SArray), 0);
        SArray.RefCnt := -1;
        SArray.Length := ACount;
      end;
      System.CopyArray(PElem, PByte(FItems^) + (AIndex * ElemSize), ElType, ACount);

      tailCount := FCount - (AIndex + ACount);
      if tailCount > 0 then
      begin
        System.CopyArray(PByte(FItems^) + (AIndex * ElemSize), PByte(FItems^) + ((AIndex + ACount) * ElemSize), ElType, tailCount);
        FinalizeArray(PByte(FItems^) + ((FCount - ACount) * ElemSize), ElType, ACount);
      end else
        FinalizeArray(PByte(FItems^) + (AIndex * ElemSize), ElType, ACount);

      Dec(FCount, ACount);

      for I := 0 to ACount - 1 do
        FNotify(PElem[I * ElemSize], cnRemoved);
    finally
      if DArray = nil then
        FinalizeArray(PElem, ElType, ACount)
      else
        DynArrayClear(DArray, FTypeInfo);
    end;
  end;
end;
{$IFEND}

procedure TListHelper.InternalDeleteRangeN(AIndex, ACount: Integer);
var
  SArray: array[0..1023] of Byte;
  DArray: Pointer;
  PElem: PByte;
  tailCount, I, Size, ElemSize: Integer;
begin
  if CheckDeleteRange(AIndex, ACount) then
  begin
    ElemSize := ElSize;
    DArray := nil; // initialize the local dynarray
    try
      Size := ACount * ElemSize;
      if Size > Length(SArray) then
      begin
        GetMem(DArray, Size);
        PElem := DArray;
      end else
        PElem := @SArray[0];
      Move(PByte(FItems^)[AIndex * ElemSize], PElem[0], Size);

      tailCount := FCount - (AIndex + ACount);
      if tailCount > 0 then
      begin
        Move(PByte(FItems^)[(AIndex + ACount) * ElemSize], PByte(FItems^)[AIndex * ElemSize], tailCount * ElemSize);
        Inc(AIndex, ACount);
      end;
      FillChar(PUInt64(FItems^)[AIndex * ElemSize], Size, 0);

      Dec(FCount, ACount);

      for I := 0 to ACount - 1 do
        FNotify(PElem[I * ElemSize], cnRemoved);
    finally
      FreeMem(DArray);
    end;
  end;
end;

procedure TListHelper.InternalDoDelete1(AIndex: Integer; Action: TCollectionNotification);
var
  oldItem: Byte;
begin
  CheckItemRangeInline(AIndex);
  oldItem := PByte(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex + 1], PByte(FItems^)[AIndex], FCount - AIndex);
  PByte(FItems^)[FCount] := 0;
  FNotify(oldItem, Action);
end;

procedure TListHelper.InternalDoDelete2(AIndex: Integer; Action: TCollectionNotification);
var
  oldItem: Word;
begin
  CheckItemRangeInline(AIndex);
  oldItem := PWord(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PWord(FItems^)[AIndex + 1], PWord(FItems^)[AIndex], (FCount - AIndex) * SizeOf(Word));
  PWord(FItems^)[FCount] := 0;
  FNotify(oldItem, Action);
end;

procedure TListHelper.InternalDoDelete4(AIndex: Integer; Action: TCollectionNotification);
var
  oldItem: Cardinal;
begin
  CheckItemRangeInline(AIndex);
  oldItem := PCardinal(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PCardinal(FItems^)[AIndex + 1], PCardinal(FItems^)[AIndex], (FCount - AIndex) * SizeOf(Cardinal));
  PCardinal(FItems^)[FCount] := 0;
  FNotify(oldItem, Action);
end;

procedure TListHelper.InternalDoDelete8(AIndex: Integer; Action: TCollectionNotification);
var
  oldItem: UInt64;
begin
  CheckItemRangeInline(AIndex);
  oldItem := PUInt64(FItems^)[AIndex];
  Dec(FCount);
  if AIndex <> FCount then
    Move(PUInt64(FItems^)[AIndex + 1], PUInt64(FItems^)[AIndex], (FCount - AIndex) * SizeOf(UInt64));
  PUInt64(FItems^)[FCount] := 0;
  FNotify(oldItem, Action);
end;

procedure TListHelper.InternalDoDeleteManaged(AIndex: Integer; Action: TCollectionNotification);
var
  SOldItem: array[0..63] of Byte;
  DOldItem: PByte;
  OldItemP: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);
  ElemSize := ElSize;
  DOldItem := nil;
  OldItemP := @SOldItem[0];
  try
    if ElemSize > SizeOf(SOldItem) then
    begin
      GetMem(DOldItem, ElemSize);
      OldItemP := DOldItem;
    end;
    Move(PByte(FItems^)[AIndex * ElemSize], OldItemP^, ElemSize);
    Dec(FCount);
    if AIndex <> FCount then
      Move(PByte(FItems^)[(AIndex + 1) * ElemSize], PByte(FItems^)[AIndex * ElemSize], (FCount - AIndex) * ElemSize);
    FillChar(PByte(FItems^)[FCount * ElemSize], ElemSize, 0);
    FNotify(OldItemP^, Action);
  finally
    FinalizeArray(OldItemP, ElType, 1);
    FreeMem(DOldItem);
  end;
end;

procedure TListHelper.InternalDoDeleteMRef(AIndex: Integer; Action: TCollectionNotification);
var
  oldItem: Pointer;
begin
  CheckItemRangeInline(AIndex);
  oldItem := PPointer(FItems^)[AIndex];
  try
    Dec(FCount);
    if AIndex <> FCount then
      Move(PPointer(FItems^)[AIndex + 1], PPointer(FItems^)[AIndex], (FCount - AIndex) * SizeOf(Pointer));
    PPointer(FItems^)[FCount] := nil;
    FNotify(oldItem, Action);
  finally
    FinalizeArray(@oldItem, ElType, 1);
  end;
end;

procedure TListHelper.InternalDoDeleteN(AIndex: Integer; Action: TCollectionNotification);
var
  SOldItem: array[0..63] of Byte;
  DOldItem: PByte;
  OldItemP: PByte;
  ElemSize: Integer;
begin
  CheckItemRangeInline(AIndex);
  DOldItem := nil;
  try
    ElemSize := ElSize;
    if ElemSize > SizeOf(SOldItem) then
    begin
      GetMem(DOldItem, ElemSize);
      OldItemP := DOldItem;
    end else
      OldItemP := @SOldItem[0];
    Move(PByte(FItems^)[AIndex * ElemSize], OldItemP^, ElemSize);
    Dec(FCount);
    if AIndex <> FCount then
      Move(PByte(FItems^)[(AIndex + 1) * ElemSize], PByte(FItems^)[AIndex * ElemSize], (FCount - AIndex) * ElemSize);
    FillChar(PByte(FItems^)[FCount * ElemSize], ElemSize, 0);
    FNotify(OldItemP^, Action);
  finally
    FreeMem(DOldItem);
  end;
end;

{$IF Defined(WEAKREF)}
procedure TListHelper.InternalDoDeleteWeak(AIndex: Integer; Action: TCollectionNotification);
var
  SOldItem: TLocalDynArray;
  DOldItem: Pointer;
  OldItemP: PByte;
  ElemSize: Integer;
  Size: NativeInt;
begin
  CheckItemRangeInline(AIndex);
  ElemSize := ElSize;
  DOldItem := nil;
  OldItemP := @SOldItem.Data[0];
  Size := 1;
  try
    if ElemSize > Length(SOldItem.Data) then
    begin
      DynArraySetLength(DOldItem, FTypeInfo, 1, @Size);
      OldItemP := DOldItem;
    end else
    begin
      FillChar(SOldItem, SizeOf(SOldItem), 0);
      SOldItem.RefCnt := -1;
      SOldItem.Length := 1;
    end;
    System.CopyArray(OldItemP, PByte(FItems^) + AIndex * ElemSize, ElType, 1);
    Dec(FCount);
    if AIndex <> FCount then
      System.CopyArray(PByte(FItems^) + AIndex * ElemSize, PByte(FItems^) + (AIndex + 1) * ElemSize, ElType, FCount - AIndex);
    FinalizeArray(PByte(FItems^) + FCount * ElemSize, ElType, 1);
    FNotify(OldItemP^, Action);
  finally
    if DOldItem = nil then
      FinalizeArray(OldItemP, ElType, 1)
    else
      DynArrayClear(DOldItem, FTypeInfo);
  end;
end;
{$IFEND}

procedure TListHelper.InternalGrow(ANewCount: Integer);
var
  NewCount: Integer;
begin
  NewCount := DynArraySize(FItems^);
  if NewCount = 0 then
    NewCount := ANewCount
  else
    repeat
      NewCount := NewCount * 2;
      if NewCount < 0 then
        OutOfMemoryError;
    until NewCount >= ANewCount;
  InternalSetCapacity(NewCount);
end;

procedure TListHelper.InternalGrowCheck(ANewCount: Integer);
begin
  if ANewCount > DynArraySize(FItems^) then
    InternalGrow(ANewCount)
  else if ANewCount < 0 then
    OutOfMemoryError;
end;

function TListHelper.InternalIndexOf1(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(System.Types.TDirection.FromBeginning) then
    Result := DoIndexOfFwd1(Value)
  else
    Result := DoIndexOfRev1(Value);
end;

function TListHelper.InternalIndexOf2(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(System.Types.TDirection.FromBeginning) then
    Result := DoIndexOfFwd2(Value)
  else
    Result := DoIndexOfRev2(Value);
end;

function TListHelper.InternalIndexOf4(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(System.Types.TDirection.FromBeginning) then
    Result := DoIndexOfFwd4(Value)
  else
    Result := DoIndexOfRev4(Value);
end;

function TListHelper.InternalIndexOf8(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(System.Types.TDirection.FromBeginning) then
    Result := DoIndexOfFwd8(Value)
  else
    Result := DoIndexOfRev8(Value);
end;

function TListHelper.InternalIndexOfMRef(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(System.Types.TDirection.FromBeginning) then
    Result := DoIndexOfFwdMRef(Value)
  else
    Result := DoIndexOfRevMRef(Value);
end;

function TListHelper.InternalIndexOfN(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(System.Types.TDirection.FromBeginning) then
    Result := DoIndexOfFwdN(Value)
  else
    Result := DoIndexOfRevN(Value);
end;

procedure TListHelper.InternalInsert1(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex], PByte(FItems^)[AIndex + 1], FCount - AIndex);
  PByte(FItems^)[AIndex] := Byte(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsert2(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PWord(FItems^)[AIndex], PWord(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Word));
  PWord(FItems^)[AIndex] := Word(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsert4(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PCardinal(FItems^)[AIndex], PCardinal(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Cardinal));
  PCardinal(FItems^)[AIndex] := Cardinal(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsert8(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PUInt64(FItems^)[AIndex], PUInt64(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(UInt64));
  PUInt64(FItems^)[AIndex] := UInt64(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsertManaged(AIndex: Integer; const Value);
var
  ElemSize: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  ElemSize := ElSize;
  if AIndex <> FCount then
    CopyArray(PByte(FItems^) + (AIndex + 1) * ElemSize, PByte(FItems^) + AIndex * ElemSize, ElType, ElemSize, FCount - AIndex);
  System.CopyArray(PByte(FItems^) + AIndex * ElemSize, @Value, ElType, 1);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsertMRef(AIndex: Integer; const Value; TypeKind: TTypeKind);
begin
  if IsConstValue(TypeKind) then
  begin
    case TypeKind of
      TTypeKind.tkUString: DoInsertString(AIndex, Value);
      TTypeKind.tkDynArray: DoInsertDynArray(AIndex, Value);
      TTypeKind.tkInterface: DoInsertInterface(AIndex, Value);
{$IF Defined(AUTOREFCOUNT)}
      TTypeKind.tkClass: DoInsertObject(AIndex, Value);
{$IFEND}
{$IF not Defined(NEXTGEN)}
      TTypeKind.tkLString: DoInsertAnsiString(AIndex, Value);
      TTypeKind.tkWString: DoInsertWideString(AIndex, Value);
{$IFEND}
    end;
  end else
    System.Error(rePlatformNotImplemented);
end;

procedure TListHelper.InternalInsertN(AIndex: Integer; const Value);
var
  ElemSize: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  ElemSize := ElSize;
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex * ElemSize], PByte(FItems^)[(AIndex + 1) * ElemSize], (FCount - AIndex) * ElemSize);
  Move(Value, PByte(FItems^)[AIndex * ElemSize], ElemSize);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalInsertRange1(AIndex: Integer; Values: Pointer; ACount: Integer);
var
  I: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + ACount);
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex], PByte(FItems^)[AIndex + ACount], FCount - AIndex);

  Move(PByte(Values)[0], PByte(FItems^)[AIndex], ACount);

  Inc(FCount, ACount);

  for I := 0 to ACount - 1 do
    FNotify(PByte(Values)[I], cnAdded);
end;

procedure TListHelper.InternalInsertRange2(AIndex: Integer; Values: Pointer; ACount: Integer);
var
  I: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + ACount);
  if AIndex <> FCount then
    Move(PWord(FItems^)[AIndex], PWord(FItems^)[AIndex + ACount], (FCount - AIndex) * SizeOf(Word));

  Move(PWord(Values)[0], PWord(FItems^)[AIndex], ACount * SizeOf(Word));

  Inc(FCount, ACount);

  for I := 0 to ACount - 1 do
    FNotify(PWord(Values)[I], cnAdded);
end;

procedure TListHelper.InternalInsertRange4(AIndex: Integer; Values: Pointer; ACount: Integer);
var
  I: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + ACount);
  if AIndex <> FCount then
    Move(PCardinal(FItems^)[AIndex], PCardinal(FItems^)[AIndex + ACount], (FCount - AIndex) * SizeOf(Cardinal));

  Move(PCardinal(Values)[0], PCardinal(FItems^)[AIndex], ACount * SizeOf(Cardinal));

  Inc(FCount, ACount);

  for I := 0 to ACount - 1 do
    FNotify(PCardinal(Values)[I], cnAdded);
end;

procedure TListHelper.InternalInsertRange8(AIndex: Integer; Values: Pointer; ACount: Integer);
var
  I: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + ACount);
  if AIndex <> FCount then
    Move(PUInt64(FItems^)[AIndex], PUInt64(FItems^)[AIndex + ACount], (FCount - AIndex) * SizeOf(UInt64));

  Move(PUInt64(Values)[0], PUInt64(FItems^)[AIndex], ACount * SizeOf(UInt64));

  Inc(FCount, ACount);

  for I := 0 to ACount - 1 do
    FNotify(PUInt64(Values)[I], cnAdded);
end;

procedure TListHelper.InternalInsertRangeManaged(AIndex: Integer; Values: Pointer; ACount: Integer);
var
  I, ElemSize: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + ACount);
  ElemSize := ElSize;
  if AIndex <> FCount then
  begin
    CopyArray(@PByte(FItems^)[(AIndex + ACount) * ElemSize], @PByte(FItems^)[AIndex * ElemSize], ElType, ElemSize, FCount - AIndex);
    FillChar(PByte(FItems^)[AIndex * ElemSize], ACount * ElemSize, 0);
  end;

  CopyArray(@PByte(FItems^)[AIndex * ElemSize], @PByte(Values)[0], ElType, ElemSize, ACount);

  Inc(FCount, ACount);

  for I := 0 to ACount - 1 do
    FNotify(PByte(Values)[I * ElemSize], cnAdded);
end;

procedure TListHelper.InternalInsertRangeN(AIndex: Integer; Values: Pointer; ACount: Integer);
var
  I, ElemSize: Integer;
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + ACount);
  ElemSize := ElSize;
  if AIndex <> FCount then
    Move(PByte(FItems^)[AIndex * ElemSize], PByte(FItems^)[(AIndex + ACount) * ElemSize], (FCount - AIndex) * ElemSize);

  Move(PByte(Values)[0], PByte(FItems^)[AIndex * ElemSize], ACount * ElemSize);

  Inc(FCount, ACount);

  for I := 0 to ACount - 1 do
    FNotify(PByte(Values)[I * ElemSize], cnAdded);
end;

procedure TListHelper.InternalInsertVariant(AIndex: Integer; const Value);
begin
  CheckInsertRange(AIndex);

  InternalGrowCheck(FCount + 1);
  if AIndex <> FCount then
    Move(PVariant(FItems^)[AIndex], PVariant(FItems^)[AIndex + 1], (FCount - AIndex) * SizeOf(Variant));
  PVariant(FItems^)[AIndex] := Variant(Value);
  Inc(FCount);
  FNotify(Value, cnAdded);
end;

procedure TListHelper.InternalMove1(CurIndex, NewIndex: Integer);
var
  Temp: Byte;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PByte(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PByte(FItems^)[CurIndex + 1], PByte(FItems^)[CurIndex], NewIndex - CurIndex)
    else
      Move(PByte(FItems^)[NewIndex], PByte(FItems^)[NewIndex + 1], CurIndex - NewIndex);

    PByte(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMove2(CurIndex, NewIndex: Integer);
var
  Temp: Word;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PWord(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PWord(FItems^)[CurIndex + 1], PWord(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(Word))
    else
      Move(PWord(FItems^)[NewIndex], PWord(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(Word));

    PWord(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMove4(CurIndex, NewIndex: Integer);
var
  Temp: Cardinal;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PCardinal(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PCardinal(FItems^)[CurIndex + 1], PCardinal(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(Cardinal))
    else
      Move(PCardinal(FItems^)[NewIndex], PCardinal(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(Cardinal));

    PCardinal(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMove8(CurIndex, NewIndex: Integer);
var
  Temp: UInt64;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := PUInt64(FItems^)[CurIndex];
    if CurIndex < NewIndex then
      Move(PUInt64(FItems^)[CurIndex + 1], PUInt64(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(UInt64))
    else
      Move(PUInt64(FItems^)[NewIndex], PUInt64(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(UInt64));

    PUInt64(FItems^)[NewIndex] := Temp;
  end;
end;

procedure TListHelper.InternalMoveManaged(CurIndex, NewIndex: Integer);
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    DTemp := nil;
    PTemp := Pointer(@STemp);
    try
      ElemSize := ElSize;
      if ElemSize > SizeOf(STemp) then
      begin
        DTemp := AllocMem(ElemSize);
        PTemp := DTemp;
      end else
        FillChar(STemp, SizeOf(STemp), 0);
      System.CopyArray(@PTemp[0], @PByte(FItems^)[CurIndex * ElemSize], ElType, 1);
      if CurIndex < NewIndex then
        CopyArray(@PByte(FItems^)[CurIndex * ElemSize], @PByte(FItems^)[(CurIndex + 1) * ElemSize], ElType, ElemSize, NewIndex - CurIndex)
      else
        CopyArray(@PByte(FItems^)[(NewIndex + 1) * ElemSize], @PByte(FItems^)[NewIndex * ElemSize], ElType, ElemSize, CurIndex - NewIndex);
      FinalizeArray(@PByte(FItems^)[NewIndex * ElemSize], ElType, 1);
      System.CopyArray(@PByte(FItems^)[NewIndex * ElemSize], @PTemp[0], ElType, 1);
    finally
      FinalizeArray(PTemp, ElType, 1);
      FreeMem(DTemp);
    end;
  end;
end;

procedure TListHelper.InternalMoveMRef(CurIndex, NewIndex: Integer);
var
  Temp: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    Temp := nil;
    AtomicExchange(Temp, PPointer(FItems^)[CurIndex]); // this sequence "transfers" the current reference to Temp
    PPointer(FItems^)[CurIndex] := nil;
    if CurIndex < NewIndex then
      Move(PPointer(FItems^)[CurIndex + 1], PPointer(FItems^)[CurIndex], (NewIndex - CurIndex) * SizeOf(Pointer))
    else
      Move(PPointer(FItems^)[NewIndex], PPointer(FItems^)[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(Pointer));

    AtomicExchange(PPointer(FItems^)[NewIndex], Temp); // "transfer" the reference to the new location
  end;
end;

procedure TListHelper.InternalMoveN(CurIndex, NewIndex: Integer);
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
  ElemSize: Integer;
begin
  if CurIndex <> NewIndex then
  begin
    CheckItemRangeInline(NewIndex);

    DTemp := nil;
    PTemp := Pointer(@STemp);
    try
      ElemSize := ElSize;
      if ElemSize > SizeOf(STemp) then
      begin
        GetMem(DTemp, ElemSize);
        PTemp := DTemp;
      end;
      Move(PByte(FItems^)[CurIndex * ElemSize], PTemp[0], ElemSize);
      if CurIndex < NewIndex then
        Move(PByte(FItems^)[(CurIndex + 1) + ElemSize], PByte(FItems^)[CurIndex * ElemSize], (NewIndex - CurIndex) * ElemSize)
      else
        Move(PByte(FItems^)[NewIndex + ElemSize], PByte(FItems^)[(NewIndex + 1) * ElemSize], (CurIndex - NewIndex) * ElemSize);

      Move(PTemp[0], PByte(FItems^)[NewIndex + ElemSize], ElemSize);
    finally
      FreeMem(DTemp);
    end;
  end;
end;

procedure TListHelper.InternalPackInline(const IsEmpty: TInternalEmptyFunc);
var
  PackedCount: Integer;
  StartIndex: Integer;
  EndIndex: Integer;
  ElemSize: Integer;
begin
  if FCount = 0 then
    Exit;

  ElemSize := ElSize;
  PackedCount := 0;
  StartIndex := 0;
  repeat
    // Locate the first/next non-nil element in the list
//    while (StartIndex < FCount) and (FComparer.Compare(FItems[StartIndex], Default(T)) = 0) do
    while (StartIndex < FCount) and IsEmpty(PByte(FItems^)[StartIndex * ElemSize]) do
      Inc(StartIndex);

    if StartIndex < FCount then // There is nothing more to do
    begin
      // Locate the next nil pointer
      EndIndex := StartIndex;
//      while (EndIndex < FCount) and (FComparer.Compare(FItems[EndIndex], Default(T)) <> 0) do
      while (EndIndex < FCount) and not IsEmpty(PByte(FItems^)[EndIndex * ElemSize]) do
        Inc(EndIndex);
      Dec(EndIndex);

      // Move this block of non-null items to the index recorded in PackedToCount:
      // If this is a contiguous non-nil block at the start of the list then
      // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
      if StartIndex > PackedCount then
        Move(PByte(FItems^)[StartIndex * ElemSize], PByte(FItems^)[PackedCount * ELemSize], (EndIndex - StartIndex + 1) * ElemSize);

      // Set the PackedToCount to reflect the number of items in the list
      // that have now been packed.
      Inc(PackedCount, EndIndex - StartIndex + 1);

      // Reset StartIndex to the element following EndIndex
      StartIndex := EndIndex + 1;
    end;
  until StartIndex >= FCount;

  // Set Count so that the 'free' item
  FCount := PackedCount;
end;

procedure TListHelper.InternalPack1(const IsEmpty: TInternalEmptyFunc);
begin
  InternalPackInline(IsEmpty);
end;

procedure TListHelper.InternalPack2(const IsEmpty: TInternalEmptyFunc);
begin
  InternalPackInline(IsEmpty);
end;

procedure TListHelper.InternalPack4(const IsEmpty: TInternalEmptyFunc);
begin
  InternalPackInline(IsEmpty);
end;

procedure TListHelper.InternalPack8(const IsEmpty: TInternalEmptyFunc);
begin
  InternalPackInline(IsEmpty);
end;

procedure TListHelper.InternalPackManaged(const IsEmpty: TInternalEmptyFunc);
var
  PackedCount : Integer;
  StartIndex : Integer;
  EndIndex : Integer;
  ElemSize: Integer;
begin
  if FCount = 0 then
    Exit;

  ElemSize := ElSize;
  PackedCount := 0;
  StartIndex := 0;
  repeat
    // Locate the first/next non-nil element in the list
//    while (StartIndex < FCount) and (FComparer.Compare(FItems[StartIndex], Default(T)) = 0) do
    while (StartIndex < FCount) and IsEmpty(PByte(FItems^)[StartIndex * ElemSize]) do
      Inc(StartIndex);

    if StartIndex < FCount then // There is nothing more to do
    begin
      // Locate the next nil pointer
      EndIndex := StartIndex;
//      while (EndIndex < FCount) and (FComparer.Compare(FItems[EndIndex], Default(T)) <> 0) do
      while (EndIndex < FCount) and not IsEmpty(PByte(FItems^)[EndIndex * ElemSize]) do
        Inc(EndIndex);
      Dec(EndIndex);

      // Move this block of non-null items to the index recorded in PackedToCount:
      // If this is a contiguous non-nil block at the start of the list then
      // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
      if StartIndex > PackedCount then
        CopyArray(@PByte(FItems^)[PackedCount * ELemSize], @PByte(FItems^)[StartIndex * ElemSize], ElType, ElemSize, EndIndex - StartIndex + 1);

      // Set the PackedToCount to reflect the number of items in the list
      // that have now been packed.
      Inc(PackedCount, EndIndex - StartIndex + 1);

      // Reset StartIndex to the element following EndIndex
      StartIndex := EndIndex + 1;
    end;
  until StartIndex >= FCount;

  // Set Count so that the 'free' item
  FCount := PackedCount;
end;

procedure TListHelper.InternalPackN(const IsEmpty: TInternalEmptyFunc);
begin
  InternalPackInline(IsEmpty);
end;

function TListHelper.InternalRemove1(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(TDirection.FromBeginning) then
    Result := DoRemoveFwd1(Value)
  else
    Result := DoRemoveRev1(Value);
end;

function TListHelper.InternalRemove2(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(TDirection.FromBeginning) then
    Result := DoRemoveFwd2(Value)
  else
    Result := DoRemoveRev2(Value);
end;

function TListHelper.InternalRemove4(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(TDirection.FromBeginning) then
    Result := DoRemoveFwd4(Value)
  else
    Result := DoRemoveRev4(Value);
end;

function TListHelper.InternalRemove8(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(TDirection.FromBeginning) then
    Result := DoRemoveFwd8(Value)
  else
    Result := DoRemoveRev8(Value);
end;

function TListHelper.InternalRemoveMRef(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(TDirection.FromBeginning) then
    Result := DoRemoveFwdMRef(Value)
  else
    Result := DoRemoveRevMRef(Value);
end;

function TListHelper.InternalRemoveN(const Value; Direction: Byte): Integer;
begin
  if Direction = Byte(TDirection.FromBeginning) then
    Result := DoRemoveFwdN(Value)
  else
    Result := DoRemoveRevN(Value);
end;

procedure TListHelper.InternalReverse1;
var
  tmp: Byte;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PByte(FItems^)[b];
    PByte(FItems^)[b] := PByte(FItems^)[e];
    PByte(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverse2;
var
  tmp: Word;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PWord(FItems^)[b];
    PWord(FItems^)[b] := PWord(FItems^)[e];
    PWord(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverse4;
var
  tmp: Cardinal;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PCardinal(FItems^)[b];
    PCardinal(FItems^)[b] := PCardinal(FItems^)[e];
    PCardinal(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverse8;
var
  tmp: UInt64;
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    tmp := PUInt64(FItems^)[b];
    PUInt64(FItems^)[b] := PUInt64(FItems^)[e];
    PUInt64(FItems^)[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverseManaged;
var
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    InternalExchangeManaged(b, e);
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalReverseMRef(Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: DoReverseString;
    TTypeKind.tkInterface: DoReverseInterface;
    TTypeKind.tkVariant: DoReverseVariant;
    TTypeKind.tkDynArray: DoReverseDynArray;
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: DoReverseAnsiString;
    TTypeKind.tkWString: DoReverseWideString;
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: DoReverseObject;
{$IFEND}
  end;
end;

procedure TListHelper.InternalReverseN;
var
  b, e: Integer;
begin
  b := 0;
  e := FCount - 1;
  while b < e do
  begin
    InternalExchangeN(b, e);
    Inc(b);
    Dec(e);
  end;
end;

procedure TListHelper.InternalSetCapacity(Value: NativeInt);
begin
  DynArraySetLength(FItems^, FTypeInfo, 1, @Value);
end;

procedure TListHelper.InternalSetCount1(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange1(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCount2(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange2(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCount4(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange4(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCount8(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRange8(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCountManaged(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeManaged(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalSetCountMRef(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeMRef(Value, FCount - Value);
  FCount := Value;
end;

{$IF Defined(WEAKREF)}
procedure TListHelper.InternalSetCountWeak(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeWeak(Value, FCount - Value);
  FCount := Value;
end;
{$IFEND}

procedure TListHelper.InternalSetCountN(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  if Value > DynArraySize(FItems^) then
    InternalSetCapacity(Value);
  if Value < FCount then
    InternalDeleteRangeN(Value, FCount - Value);
  FCount := Value;
end;

procedure TListHelper.InternalToArray(var Dest: Pointer);
var
  LSize: NativeInt;
begin
  LSize := FCount;
  DynArraySetLength(Dest, FTypeInfo, 1, @LSize);
  Move(PByte(FItems^)[0], PByte(Dest)[0], LSize * ElSize);
end;

procedure TListHelper.InternalToArrayManaged(var Dest: Pointer);
var
  LSize: NativeInt;
begin
  LSize := FCount;
  DynArraySetLength(Dest, FTypeInfo, 1, @LSize);
  System.CopyArray(Dest, @PByte(FItems^)[0], ElType, LSize);
end;


{ TList<T> }

function TList<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TList<T>.SetCapacity(Value: Integer);
begin
  if Value < Count then
    Count := Value;
  SetLength(FItems, Value);
end;

procedure TList<T>.SetCount(Value: Integer);
begin
  if IsManagedType(T) then
  begin
{$IF Defined(WEAKREF)}
    if System.HasWeakRef(T) then
      FListHelper.InternalSetCountWeak(Value)
    else
{$IFEND}
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalSetCountMRef(Value)
    else
      FListHelper.InternalSetCountManaged(Value);
  end else
  case SizeOf(T) of
    1: FListHelper.InternalSetCount1(Value);
    2: FListHelper.InternalSetCount2(Value);
    4: FListHelper.InternalSetCount4(Value);
    8: FListHelper.InternalSetCount8(Value);
  else
    FListHelper.InternalSetCountN(Value)
  end;
end;

function TList<T>.GetItem(Index: Integer): T;
begin
  FListHelper.CheckItemRange(Index);
  Result := FItems[Index];
end;

procedure TList<T>.SetItem(Index: Integer; const Value: T);
begin
  if IsManagedType(T) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) and not System.HasWeakRef(T) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.SetItemMRef(Value, Index, GetTypeKind(T))
    else if GetTypeKind(T) = TTypeKind.tkVariant then
      FListHelper.SetItemVariant(Value, Index)
    else
      FListHelper.SetItemManaged(Value, Index);
  end else
  case SizeOf(T) of
    1: FListHelper.SetItem1(Value, Index);
    2: FListHelper.SetItem2(Value, Index);
    4: FListHelper.SetItem4(Value, Index);
    8: FListHelper.SetItem8(Value, Index);
  else
    FListHelper.SetItemN(Value, Index);
  end;
end;

//procedure TList<T>.Grow(ACount: Integer);
//begin
//  FListHelper.InternalGrow(Pointer(FItems), TypeInfo(arrayOfT), ACount);
//end;

procedure TList<T>.GrowCheck(ACount: Integer);
begin
  FListHelper.InternalGrowCheck(ACount);
end;

procedure TList<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

procedure TList<T>.Pack;
var
  IsEmpty: TListHelper.TInternalEmptyFunc;
begin
  IsEmpty := function (const Item): Boolean
    begin
      Result := FComparer.Compare(T(Item), Default(T)) = 0;
    end;
  if IsManagedType(T) then
    FListHelper.InternalPackManaged(IsEmpty)
  else
  case SizeOf(T) of
    1: FListHelper.InternalPack1(IsEmpty);
    2: FListHelper.InternalPack2(IsEmpty);
    4: FListHelper.InternalPack4(IsEmpty);
    8: FListHelper.InternalPack8(IsEmpty);
  else
    FListHelper.InternalPackN(IsEmpty);
  end;
end;

procedure TList<T>.Pack(const IsEmpty: TEmptyFunc);
var
  LIsEmpty: TListHelper.TInternalEmptyFunc;
begin
  LIsEmpty := function (const Item): Boolean
    begin
      Result := IsEmpty(T(Item), Default(T));
    end;
  if IsManagedType(T) then
    FListHelper.InternalPackManaged(LIsEmpty)
  else
  case SizeOf(T) of
    1: FListHelper.InternalPack1(LIsEmpty);
    2: FListHelper.InternalPack2(LIsEmpty);
    4: FListHelper.InternalPack4(LIsEmpty);
    8: FListHelper.InternalPack8(LIsEmpty);
  else
    FListHelper.InternalPackN(LIsEmpty);
  end;
end;

constructor TList<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  FListHelper.FNotify := InternalNotify;
  FListHelper.FCompare := InternalCompare;
  FListHelper.FTypeInfo := TypeInfo(arrayOfT);
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TComparer<T>.Default;
end;

constructor TList<T>.Create(const Collection: TEnumerable<T>);
begin
  inherited Create;
  FListHelper.FNotify := InternalNotify;
  FListHelper.FCompare := InternalCompare;
  FListHelper.FTypeInfo := TypeInfo(arrayOfT);
  FComparer := TComparer<T>.Default;
  InsertRange(0, Collection);
end;

destructor TList<T>.Destroy;
begin
  Capacity := 0;
  inherited;
end;

class procedure TList<T>.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
end;

{$IFNDEF NEXTGEN}
class procedure TList<T>.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;
{$ENDIF  NEXTGEN}

function TList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TList<T>.Add(const Value: T): Integer;
begin
  if IsManagedType(T) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      Result := FListHelper.InternalAddMRef(Value, GetTypeKind(T))
    else if GetTypeKind(T) = TTypeKind.tkVariant then
      Result := FListHelper.InternalAddVariant(Value)
    else
      Result := FListHelper.InternalAddManaged(Value);
  end else
  case SizeOf(T) of
    1: Result := FListHelper.InternalAdd1(Value);
    2: Result := FListHelper.InternalAdd2(Value);
    4: Result := FListHelper.InternalAdd4(Value);
    8: Result := FListHelper.InternalAdd8(Value);
  else
    Result := FListHelper.InternalAddN(Value);
  end;
end;

procedure TList<T>.AddRange(const Values: array of T);
begin
  InsertRange(Count, Values);
end;

procedure TList<T>.AddRange(const Collection: IEnumerable<T>);
begin
  InsertRange(Count, Collection);
end;

procedure TList<T>.AddRange(const Collection: TEnumerable<T>);
begin
  InsertRange(Count, Collection);
end;

function TList<T>.BinarySearch(const Item: T; out Index: Integer): Boolean;
begin
  Result := TArray.BinarySearch<T>(FItems, Item, Index, FComparer, 0, Count);
end;

function TList<T>.BinarySearch(const Item: T; out Index: Integer;
  const AComparer: IComparer<T>): Boolean;
begin
  Result := TArray.BinarySearch<T>(FItems, Item, Index, AComparer, 0, Count);
end;

procedure TList<T>.Insert(Index: Integer; const Value: T);
begin
  if IsManagedType(T) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalInsertMRef(Index, Value, GetTypeKind(T))
    else if GetTypeKind(T) = TTypeKind.tkVariant then
      FListHelper.InternalInsertVariant(Index, Value)
    else
      FListHelper.InternalInsertManaged(Index, Value);
  end else
  case SizeOf(T) of
    1: FListHelper.InternalInsert1(Index, Value);
    2: FListHelper.InternalInsert2(Index, Value);
    4: FListHelper.InternalInsert4(Index, Value);
    8: FListHelper.InternalInsert8(Index, Value);
  else
    FListHelper.InternalInsertN(Index, Value);
  end;
end;

procedure TList<T>.InsertRange(Index: Integer; const Values: array of T);
begin
  if IsManagedType(T) then
    FListHelper.InternalInsertRangeManaged(Index, @Values, Length(Values))
  else
  case SizeOf(T) of
    1: FListHelper.InternalInsertRange1(Index, @Values, Length(Values));
    2: FListHelper.InternalInsertRange2(Index, @Values, Length(Values));
    4: FListHelper.InternalInsertRange4(Index, @Values, Length(Values));
    8: FListHelper.InternalInsertRange8(Index, @Values, Length(Values));
  else
    FListHelper.InternalInsertRangeN(Index, @Values, Length(Values));
  end;
end;

procedure TList<T>.InsertRange(Index: Integer; const Collection: IEnumerable<T>);
var
  Item: T;
begin
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
  for Item in Collection do
  begin
    Insert(Index, Item);
    Inc(Index);
  end;
end;

function TList<T>.InternalCompare(const Left, Right): Integer;
begin
  Result := FComparer.Compare(T(Left), T(Right));
end;

procedure TList<T>.InternalNotify(const Item; Action: TCollectionNotification);
begin
  Notify(T(Item), Action);
end;

function TList<T>.ItemValue(const Item: T): NativeInt;
begin
  case SizeOf(T) of
    1: Result := PByte(@Item)[0] shl 0;
    2: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8;
    3: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16;
{$IF SizeOf(Pointer) <= 4}
    4: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24;
{$ELSE}
    4: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24;
    5: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
       NativeInt(PByte(@Item)[4]) shl 32;
    6: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
       NativeInt(PByte(@Item)[4]) shl 32 + NativeInt(PByte(@Item)[5]) shl 40;
    7: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
       NativeInt(PByte(@Item)[4]) shl 32 + NativeInt(PByte(@Item)[5]) shl 40 + NativeInt(PByte(@Item)[6]) shl 48;
  else
    Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
       NativeInt(PByte(@Item)[4]) shl 32 + NativeInt(PByte(@Item)[5]) shl 40 + NativeInt(PByte(@Item)[6]) shl 48 +
       NativeInt(PByte(@Item)[7]) shl 56;
{$IFEND}
  end;
end;

procedure TList<T>.Exchange(Index1, Index2: Integer);
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalExchangeMRef(Index1, Index2, GetTypeKind(T))
    else
      FListHelper.InternalExchangeManaged(Index1, Index2)
  else
  case SizeOf(T) of
    1: FListHelper.InternalExchange1(Index1, Index2);
    2: FListHelper.InternalExchange2(Index1, Index2);
    4: FListHelper.InternalExchange4(Index1, Index2);
    8: FListHelper.InternalExchange8(Index1, Index2);
  else
    FListHelper.InternalExchangeN(Index1, Index2)
  end;
end;

procedure TList<T>.DoDelete(Index: Integer; Notification: TCollectionNotification);
begin
  if IsManagedType(T) then
  begin
{$IF Defined(WEAKREF)}
    if System.HasWeakRef(T) then
      FListHelper.InternalDoDeleteWeak(Index, Notification)
    else
{$IFEND}
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalDoDeleteMRef(Index, Notification)
    else
      FListHelper.InternalDoDeleteManaged(Index, Notification);
  end else
  case SizeOf(T) of
    1: FListHelper.InternalDoDelete1(Index, Notification);
    2: FListHelper.InternalDoDelete2(Index, Notification);
    4: FListHelper.InternalDoDelete4(Index, Notification);
    8: FListHelper.InternalDoDelete8(Index, Notification);
  else
    FListHelper.InternalDoDeleteN(Index, Notification);
  end;
end;

procedure TList<T>.Delete(Index: Integer);
begin
  DoDelete(Index, cnRemoved);
end;

function TList<T>.ExtractItem(const Value: T; Direction: TDirection): T;
var
  Index: Integer;
begin
  if IsManagedType(T) then
  begin
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalExtractItemMRef(Value, GetTypeKind(T), Result, Byte(Direction))
    else if GetTypeKind(T) = TTypeKind.tkVariant then
      FListHelper.InternalExtractItemVariant(Value, Result, Byte(Direction))
    else
      FListHelper.InternalExtractItemManaged(Value, Result, Byte(Direction));
  end else
  case SizeOf(T) of
    1: FListHelper.InternalExtractItem1(Value, Result, Byte(Direction));
    2: FListHelper.InternalExtractItem2(Value, Result, Byte(Direction));
    4: FListHelper.InternalExtractItem4(Value, Result, Byte(Direction));
    8: FListHelper.InternalExtractItem8(Value, Result, Byte(Direction));
  else
    FListHelper.InternalExtractItemN(Value, Result, Byte(Direction))
  end;
end;

function TList<T>.Extract(const Value: T): T;
begin
  Result := ExtractItem(Value, TDirection.FromBeginning);
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.RemoveItem(const Value: T; Direction: TDirection): Integer;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      Result := FListHelper.InternalRemoveMRef(Value, Byte(Direction))
    else
      Result := FListHelper.InternalRemoveN(Value, Byte(Direction))
  else
  case SizeOf(T) of
    1: Result := FListHelper.InternalRemove1(Value, Byte(Direction));
    2: Result := FListHelper.InternalRemove2(Value, Byte(Direction));
    4: Result := FListHelper.InternalRemove4(Value, Byte(Direction));
    8: Result := FListHelper.InternalRemove8(Value, Byte(Direction));
  else
    Result := FListHelper.InternalRemoveN(Value, Byte(Direction))
  end;
end;

function TList<T>.Remove(const Value: T): Integer;
begin
  Result := RemoveItem(Value, TDirection.FromBeginning);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: Integer);
begin
  if IsManagedType(T) then
  begin
{$IF Defined(WEAKREF)}
    if System.HasWeakRef(T) then
      FListHelper.InternalDeleteRangeWeak(AIndex, ACount)
    else
{$IFEND}
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalDeleteRangeMRef(AIndex, ACount)
    else
      FListHelper.InternalDeleteRangeManaged(AIndex, ACount);
  end else
  case SizeOf(T) of
    1: FListHelper.InternalDeleteRange1(AIndex, ACount);
    2: FListHelper.InternalDeleteRange2(AIndex, ACount);
    4: FListHelper.InternalDeleteRange4(AIndex, ACount);
    8: FListHelper.InternalDeleteRange8(AIndex, ACount);
  else
    FListHelper.InternalDeleteRangeN(AIndex, ACount);
  end;
end;

procedure TList<T>.Clear;
begin
  if IsManagedType(T) then
  begin
{$IF Defined(WEAKREF)}
    if System.HasWeakRef(T) then
      FListHelper.InternalClearWeak
    else
{$IFEND}
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalClearMRef
    else
      FListHelper.InternalClearManaged;
  end else
  case SizeOf(T) of
    1: FListHelper.InternalClear1;
    2: FListHelper.InternalClear2;
    4: FListHelper.InternalClear4;
    8: FListHelper.InternalClear8;
  else
    FListHelper.InternalClearN;
  end;
end;

function TList<T>.Expand: TList<T>;
begin
  if FListHelper.FCount = Length(FItems) then
    GrowCheck(FCount + 1);
  Result := Self;
end;

function TList<T>.IndexOfItem(const Value: T; Direction: TDirection): Integer;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      Result := FListHelper.InternalIndexOfMRef(Value, Byte(Direction))
    else
      Result := FListHelper.InternalIndexOfN(Value, Byte(Direction))
  else
  case SizeOf(T) of
    1: Result := FListHelper.InternalIndexOf1(Value, Byte(Direction));
    2: Result := FListHelper.InternalIndexOf2(Value, Byte(Direction));
    4: Result := FListHelper.InternalIndexOf4(Value, Byte(Direction));
    8: Result := FListHelper.InternalIndexOf8(Value, Byte(Direction));
  else
    Result := FListHelper.InternalIndexOfN(Value, Byte(Direction));
  end;
end;

function TList<T>.IndexOf(const Value: T): Integer;
begin
  Result := IndexOfItem(Value, TDirection.FromBeginning);
end;

function TList<T>.Contains(const Value: T): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

function TList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;

function TList<T>.LastIndexOf(const Value: T): Integer;
begin
  Result := IndexOfItem(Value, TDirection.FromEnd);
end;

procedure TList<T>.Move(CurIndex, NewIndex: Integer);
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and not System.HasWeakRef(T) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalMoveMRef(CurIndex, NewIndex)
    else
      FListHelper.InternalMoveManaged(CurIndex, NewIndex)
  else
  case SizeOf(T) of
    1: FLIstHelper.InternalMove1(CurIndex, NewIndex);
    2: FLIstHelper.InternalMove2(CurIndex, NewIndex);
    4: FLIstHelper.InternalMove4(CurIndex, NewIndex);
    8: FLIstHelper.InternalMove8(CurIndex, NewIndex);
  else
    FLIstHelper.InternalMoveN(CurIndex, NewIndex);
  end;
end;

procedure TList<T>.Reverse;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FListHelper.InternalReverseMRef(GetTypeKind(T))
    else
      FListHelper.InternalReverseManaged
  else
  case SizeOf(T) of
    1: FListHelper.InternalReverse1;
    2: FListHelper.InternalReverse2;
    4: FListHelper.InternalReverse4;
    8: FListHelper.InternalReverse8;
  else
    FListHelper.InternalReverseN;
  end;
end;

procedure TList<T>.Sort;
begin
  TArray.Sort<T>(FItems, FComparer, 0, Count);
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  TArray.Sort<T>(FItems, AComparer, 0, Count);
end;

function TList<T>.ToArray: TArray<T>;
begin
  if IsManagedType(T) then
    FListHelper.InternalToArrayManaged(Pointer(Result))
  else
    FListHelper.InternalToArray(Pointer(Result));
end;

procedure TList<T>.TrimExcess;
begin
  FListHelper.InternalSetCapacity(Count);
end;

function TList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TList<T>.TEnumerator }

constructor TList<T>.TEnumerator.Create(const AList: TList<T>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TList<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TList<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  Result := FList[FIndex];
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TQueueHelper }

function TQueueHelper.GetElSize: Integer;
begin
  Result := FLH.ElSize;
end;

function TQueueHelper.GetElType: Pointer;
begin
  Result := FLH.ElType;
end;

function TQueueHelper.GetFItems: PPointer;
begin
  Result := FLH.FItems;
end;

procedure TQueueHelper.CheckEmpty;
begin
  if FLH.FCount = 0 then
    raise EListError.CreateRes(Pointer(@SUnbalancedOperation));
end;

procedure TQueueHelper.DequeueAdjust(Notification: TCollectionNotification; const Item);
begin
  FTail := (FTail + 1) mod DynArraySize(FItems^);
  Dec(FLH.FCount);
  FLH.FNotify(Item, Notification);
end;

procedure TQueueHelper.DynArraySetLength(Value: NativeInt);
begin
  System.DynArraySetLength(FItems^, FLH.FTypeInfo, 1, @Value);
end;

procedure TQueueHelper.EnqueueAdjust(const Value);
begin
  FHead := (FHead + 1) mod DynArraySize(FItems^);
  Inc(FLH.FCount);
  FLH.FNotify(Value, cnAdded);
end;

function TQueueHelper.GetNewCap: Integer;
begin
  Result := DynArraySize(FItems^) * 2;
  if Result = 0 then
    Result := 4
  else if Result < 0 then
    OutOfMemoryError;
end;

procedure TQueueHelper.InternalSetCapacityInline(Value: Integer; ElemSize: Integer);
var
  TailCount, Offset: Integer;
begin
  Offset := Value - DynArraySize(FItems^);
  if Offset = 0 then
    Exit;

  // If head <= tail, then part of the queue wraps around
  // the end of the array; don't introduce a gap in the queue.
  if (FHead < FTail) or ((FHead = FTail) and (FLH.FCount > 0)) then
    TailCount := DynArraySize(FItems^) - FTail
  else
    TailCount := 0;

  if Offset > 0 then
    DynArraySetLength(Value);
  if TailCount > 0 then
  begin
    Move(PByte(FItems^)[FTail * ElemSize], PByte(FItems^)[(FTail + Offset) * ElemSize], TailCount * ElemSize);
    Inc(FTail, Offset);
  end else if FTail > 0 then
  begin
    Move(PByte(FItems^)[FTail * ElemSize], PByte(FItems^)[0], FLH.FCount * ElemSize);
    Dec(FHead, FTail);
    FTail := 0;
  end;
  if Offset < 0 then
  begin
    DynArraySetLength(Value);
    if Value = 0 then
      FHead := 0
    else
      FHead := FHead mod DynArraySize(FItems^);
  end;
end;

procedure TQueueHelper.InternalEnqueueString(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrowMRef;
  PString(FItems^)[FHead] := string(Value);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalEnqueueInterface(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrowMRef;
  TListHelper.PInterface(FItems^)[FHead] := IInterface(Value);
  EnqueueAdjust(Value);
end;

{$IF not Defined(NEXTGEN)}
procedure TQueueHelper.InternalEnqueueAnsiString(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrowMRef;
  PAnsiString(FItems^)[FHead] := AnsiString(Value);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalEnqueueWideString(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrowMRef;
  PWideString(FItems^)[FHead] := WideString(Value);
  EnqueueAdjust(Value);
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TQueueHelper.InternalEnqueueObject(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrowMRef;
  PObject(FItems^)[FHead] := TObject(Value);
  EnqueueAdjust(Value);
end;
{$IFEND}

procedure TQueueHelper.InternalEnqueue1(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrow1;
  PByte(FItems^)[FHead] := Byte(Value);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalEnqueue2(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrow2;
  PWord(FItems^)[FHead] := Word(Value);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalEnqueue4(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrow4;
  PCardinal(FItems^)[FHead] := Cardinal(Value);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalEnqueue8(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrow8;
  PUInt64(FItems^)[FHead] := UInt64(Value);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalEnqueueManaged(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrowManaged;
  System.CopyArray(@PByte(FItems^)[FHead * ElSize], @Value, ElType, 1);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalEnqueueMRef(const Value; Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: InternalEnqueueString(Value);
    TTypeKind.tkInterface: InternalEnqueueInterface(Value);
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: InternalEnqueueAnsiString(Value);
    TTypeKind.tkWString: InternalEnqueueWideString(Value);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: InternalEnqueueObject(Value);
{$IFEND}
  end;
end;

procedure TQueueHelper.InternalEnqueueN(const Value);
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrowN;
  Move(Value, PByte(FItems^)[FHead * ElSize], ElSize);
  EnqueueAdjust(Value);
end;

procedure TQueueHelper.InternalDequeueString(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  string(Item) := PString(FItems^)[FTail];
  if not Peek then
  begin
    PString(FItems^)[FTail] := '';
    DequeueAdjust(Notification, Item);
  end;
end;

procedure TQueueHelper.InternalDequeueInterface(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  IInterface(Item) := TListHelper.PInterface(FItems^)[FTail];
  if not Peek then
  begin
    TListHelper.PInterface(FItems^)[FTail] := nil;
    DequeueAdjust(Notification, Item);
  end;
end;

{$IF not Defined(NEXTGEN)}
procedure TQueueHelper.InternalDequeueAnsiString(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  AnsiString(Item) := PAnsiString(FItems^)[FTail];
  if not Peek then
  begin
    PAnsiString(FItems^)[FTail] := '';
    DequeueAdjust(Notification, Item);
  end;
end;

procedure TQueueHelper.InternalDequeueWideString(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  WideString(Item) := PWideString(FItems^)[FTail];
  if not Peek then
  begin
    PWideString(FItems^)[FTail] := '';
    DequeueAdjust(Notification, Item);
  end;
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TQueueHelper.InternalDequeueObject(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  TObject(Item) := PObject(FItems^)[FTail];
  if not Peek then
  begin
    PObject(FItems^)[FTail] := nil;
    DequeueAdjust(Notification, Item);
  end;
end;
{$IFEND}

procedure TQueueHelper.InternalDequeue1(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Byte(Item) := PByte(FItems^)[FTail];
  if not Peek then
    DequeueAdjust(Notification, Item);
end;

procedure TQueueHelper.InternalDequeue2(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Word(Item) := PWord(FItems^)[FTail];
  if not Peek then
    DequeueAdjust(Notification, Item);
end;

procedure TQueueHelper.InternalDequeue4(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Cardinal(Item) := PCardinal(FItems^)[FTail];
  if not Peek then
    DequeueAdjust(Notification, Item);
end;

procedure TQueueHelper.InternalDequeue8(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  UInt64(Item) := PUInt64(FItems^)[FTail];
  if not Peek then
    DequeueAdjust(Notification, Item);
end;

procedure TQueueHelper.InternalDequeueManaged(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  System.CopyArray(@Item, @PByte(FItems^)[FTail * ElSize], ElType, 1);
  if not Peek then
  begin
    FinalizeArray(@PByte(FItems^)[FTail * ElSize], ElType, 1);
    DequeueAdjust(Notification, Item);
  end;
end;

procedure TQueueHelper.InternalDequeueMRef(Notification: TCollectionNotification; Peek: Boolean; out Item; Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: InternalDequeueString(Notification, Peek, Item);
    TTypeKind.tkInterface: InternalDequeueInterface(Notification, Peek, Item);
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: InternalDequeueAnsiString(Notification, Peek, Item);
    TTypeKind.tkWString: InternalDequeueWideString(Notification, Peek, Item);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: InternalDequeueObject(Notification, Peek, Item);
{$IFEND}
  end;
end;

procedure TQueueHelper.InternalDequeueN(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Move(PByte(FItems^)[FTail * ElSize], Item, ElSize);
  if not Peek then
    DequeueAdjust(Notification, Item);
end;

procedure TQueueHelper.InternalClearString;
var
  Temp: string;
begin
  while FLH.FCount > 0 do
    InternalDequeueString(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;

procedure TQueueHelper.InternalClearInterface;
var
  Temp: IInterface;
begin
  while FLH.FCount > 0 do
    InternalDequeueInterface(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;

{$IF not Defined(NEXTGEN)}
procedure TQueueHelper.InternalClearAnsiString;
var
  Temp: AnsiString;
begin
  while FLH.FCount > 0 do
    InternalDequeueAnsiString(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;

procedure TQueueHelper.InternalClearWideString;
var
  Temp: WideString;
begin
  while FLH.FCount > 0 do
    InternalDequeueWideString(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TQueueHelper.InternalClearObject;
var
  Temp: TObject;
begin
  while FLH.FCount > 0 do
    InternalDequeueObject(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;
{$IFEND}

procedure TQueueHelper.InternalClear1;
var
  Temp: Byte;
begin
  while FLH.FCount > 0 do
    InternalDequeue1(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;

procedure TQueueHelper.InternalClear2;
var
  Temp: Word;
begin
  while FLH.FCount > 0 do
    InternalDequeue2(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;

procedure TQueueHelper.InternalClear4;
var
  Temp: Cardinal;
begin
  while FLH.FCount > 0 do
    InternalDequeue4(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;

procedure TQueueHelper.InternalClear8;
var
  Temp: UInt64;
begin
  while FLH.FCount > 0 do
    InternalDequeue8(cnRemoved, False, Temp);
  FHead := 0;
  FTail := 0;
  FLH.FCount := 0;
end;

procedure TQueueHelper.InternalClearManaged;
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
begin
  PTemp := @STemp[0];
  DTemp := nil;
  try
    if SizeOf(STemp) < ElSize then
    begin
      DTemp := AllocMem(ElSize);
      PTemp := DTemp;
    end else
      FillChar(STemp, SizeOf(STemp), 0);
    while FLH.FCount > 0 do
      InternalDequeueN(cnRemoved, False, PTemp[0]);
    FHead := 0;
    FTail := 0;
    FLH.FCount := 0;
  finally
    FinalizeArray(@PTemp[0], ElType, 1);
    FreeMem(DTemp);
  end;
end;

procedure TQueueHelper.InternalClearMRef(Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: InternalClearString;
    TTypeKind.tkInterface: InternalClearInterface;
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: InternalClearAnsiString;
    TTypeKind.tkWString: InternalClearWideString;
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: InternalClearObject;
{$IFEND}
  end;
end;

procedure TQueueHelper.InternalClearN;
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
begin
  PTemp := @STemp[0];
  DTemp := nil;
  try
    if SizeOf(STemp) < ElSize then
    begin
      GetMem(DTemp, ElSize);
      PTemp := DTemp;
    end;
    while FLH.FCount > 0 do
      InternalDequeueN(cnRemoved, False, PTemp[0]);
    FHead := 0;
    FTail := 0;
    FLH.FCount := 0;
  finally
    FreeMem(DTemp);
  end;
end;

procedure TQueueHelper.InternalGrow1;
begin
  InternalSetCapacity1(GetNewCap);
end;

procedure TQueueHelper.InternalGrow2;
begin
  InternalSetCapacity2(GetNewCap);
end;

procedure TQueueHelper.InternalGrow4;
begin
  InternalSetCapacity4(GetNewCap);
end;

procedure TQueueHelper.InternalGrow8;
begin
  InternalSetCapacity8(GetNewCap);
end;

procedure TQueueHelper.InternalGrowManaged;
begin
  InternalSetCapacityManaged(GetNewCap);
end;

procedure TQueueHelper.InternalGrowMRef;
begin
  InternalSetCapacityMRef(GetNewCap);
end;

procedure TQueueHelper.InternalGrowN;
begin
  InternalSetCapacityN(GetNewCap);
end;

procedure TQueueHelper.InternalSetCapacity1(Value: Integer);
begin
  InternalSetCapacityInline(Value, SizeOf(Byte));
end;

procedure TQueueHelper.InternalSetCapacity2(Value: Integer);
begin
  InternalSetCapacityInline(Value, SizeOf(Word));
end;

procedure TQueueHelper.InternalSetCapacity4(Value: Integer);
begin
  InternalSetCapacityInline(Value, SizeOf(Cardinal));
end;

procedure TQueueHelper.InternalSetCapacity8(Value: Integer);
begin
  InternalSetCapacityInline(Value, SizeOf(UInt64));
end;

procedure TQueueHelper.InternalSetCapacityManaged(Value: Integer);
var
  TailCount, Offset: Integer;
  Items: PByte;
begin
  Offset := Value - DynArraySize(FItems^);
  if Offset = 0 then
    Exit;

  // If head <= tail, then part of the queue wraps around
  // the end of the array; don't introduce a gap in the queue.
  if (FHead < FTail) or ((FHead = FTail) and (FLH.FCount > 0)) then
    TailCount := DynArraySize(FItems^) - FTail
  else
    TailCount := 0;

  if Offset > 0 then
    DynArraySetLength(Value);
  Items := PByte(FItems^);
  if TailCount > 0 then
  begin
    CopyArray(@Items[(FTail + Offset) * ElSize], @Items[FTail * ElSize], ElType, ElSize, TailCount);
    if Offset > 0 then
      FinalizeArray(@Items[FTail * ElSize], ElType, Offset)
    else if Offset < 0 then
      FinalizeArray(@Items[FLH.FCount * ElSize], ElType, (- Offset));
    Inc(FTail, Offset);
  end
  else if FTail > 0 then
  begin
    if FLH.FCount > 0 then
    begin
      CopyArray(@Items[0], @Items[FTail * ElSize], ElType, ElSize, FLH.FCount);
      FinalizeArray(@Items[FLH.FCount * ElSize], ElType, FTail);
    end;
    Dec(FHead, FTail);
    FTail := 0;
  end;
  if Offset < 0 then
  begin
    DynArraySetLength(Value);
    if Value = 0 then
      FHead := 0
    else
      FHead := FHead mod DynArraySize(FItems^);
  end;
end;

procedure TQueueHelper.InternalSetCapacityMRef(Value: Integer);
var
  TailCount, Offset: Integer;
begin
  Offset := Value - DynArraySize(FItems^);
  if Offset = 0 then
    Exit;

  // If head <= tail, then part of the queue wraps around
  // the end of the array; don't introduce a gap in the queue.
  if (FHead < FTail) or ((FHead = FTail) and (FLH.FCount > 0)) then
    TailCount := DynArraySize(FItems^) - FTail
  else
    TailCount := 0;

  if Offset > 0 then
    DynArraySetLength(Value);
  if TailCount > 0 then
  begin
    Move(PByte(FItems^)[FTail * SizeOf(Pointer)], PByte(FItems^)[(FTail + Offset) * SizeOf(Pointer)], TailCount * SizeOf(Pointer));
    if offset > 0 then
      FillChar(PByte(FItems^)[FTail * SizeOf(Pointer)], Offset * SizeOf(Pointer), 0)
    else if offset < 0 then
      FillChar(PByte(FItems^)[FLH.FCount * SizeOf(Pointer)], (-Offset) * SizeOf(Pointer), 0);
    Inc(FTail, Offset);
  end else if FTail > 0 then
  begin
    if FLH.FCount > 0 then
    begin
      Move(PByte(FItems^)[FTail * SizeOf(Pointer)], PByte(FItems^)[0], FLH.FCount * SizeOf(Pointer));
      FillChar(PByte(FItems^)[FLH.FCount * SizeOf(Pointer)], FTail * SizeOf(Pointer), 0);
    end;
    Dec(FHead, FTail);
    FTail := 0;
  end;
  if Offset < 0 then
  begin
    DynArraySetLength(Value);
    if Value = 0 then
      FHead := 0
    else
      FHead := FHead mod DynArraySize(FItems^);
  end;
end;

procedure TQueueHelper.InternalSetCapacityN(Value: Integer);
begin
  InternalSetCapacityInline(Value, ElSize);
end;

{ TQueue<T> }

procedure TQueue<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

constructor TQueue<T>.Create;
begin
  inherited Create;
  FQueueHelper.FLH.FNotify := InternalNotify;
  FQueueHelper.FLH.FCompare := InternalCompare;
  FQueueHelper.FLH.FTypeInfo := TypeInfo(arrayOfT);
end;

destructor TQueue<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TQueue<T>.SetCapacity(Value: Integer);
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FQueueHelper.InternalSetCapacityMRef(Value)
    else
      FQueueHelper.InternalSetCapacityManaged(Value)
  else
  case SizeOf(T) of
    1: FQueueHelper.InternalSetCapacity1(Value);
    2: FQueueHelper.InternalSetCapacity2(Value);
    4: FQueueHelper.InternalSetCapacity4(Value);
    8: FQueueHelper.InternalSetCapacity8(Value);
  else
    FQueueHelper.InternalSetCapacityN(Value);
  end;
end;

function TQueue<T>.DoDequeue(Notification: TCollectionNotification): T;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FQueueHelper.InternalDequeueMRef(Notification, False, Result, GetTypeKind(T))
    else
      FQueueHelper.InternalDequeueManaged(Notification, False, Result)
  else
  case SizeOf(T) of
    1: FQueueHelper.InternalDequeue1(Notification, False, Result);
    2: FQueueHelper.InternalDequeue2(Notification, False, Result);
    4: FQueueHelper.InternalDequeue4(Notification, False, Result);
    8: FQueueHelper.InternalDequeue8(Notification, False, Result);
  else
    FQueueHelper.InternalDequeueN(Notification, False, Result);
  end;
end;

function TQueue<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

procedure TQueue<T>.DoSetCapacity(Value: Integer);
begin
  if Value < Count then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  SetCapacity(Value);
end;

function TQueue<T>.Dequeue: T;
begin
  Result := DoDequeue(cnRemoved);
end;

procedure TQueue<T>.Enqueue(const Value: T);
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FQueueHelper.InternalEnqueueMRef(Value, GetTypeKind(T))
    else
      FQueueHelper.InternalEnqueueManaged(Value)
  else
  case SizeOf(T) of
    1: FQueueHelper.InternalEnqueue1(Value);
    2: FQueueHelper.InternalEnqueue2(Value);
    4: FQueueHelper.InternalEnqueue4(Value);
    8: FQueueHelper.InternalEnqueue8(Value);
  else
    FQueueHelper.InternalEnqueueN(Value);
  end;
end;

function TQueue<T>.Extract: T;
begin
  Result := DoDequeue(cnExtracted);
end;

constructor TQueue<T>.Create(const Collection: TEnumerable<T>);
var
  Item: T;
begin
  inherited Create;
  FQueueHelper.FLH.FNotify := InternalNotify;
  FQueueHelper.FLH.FCompare := InternalCompare;
  FQueueHelper.FLH.FTypeInfo := TypeInfo(arrayOfT);
  for Item in Collection do
    Enqueue(Item);
end;

function TQueue<T>.Peek: T;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FQueueHelper.InternalDequeueMRef(cnRemoved, True, Result, GetTypeKind(T))
    else
      FQueueHelper.InternalDequeueManaged(cnRemoved, True, Result)
  else
  case SizeOf(T) of
    1: FQueueHelper.InternalDequeue1(cnRemoved, True, Result);
    2: FQueueHelper.InternalDequeue2(cnRemoved, True, Result);
    4: FQueueHelper.InternalDequeue4(cnRemoved, True, Result);
    8: FQueueHelper.InternalDequeue8(cnRemoved, True, Result);
  else
    FQueueHelper.InternalDequeueN(cnRemoved, True, Result);
  end;
end;

procedure TQueue<T>.Clear;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FQueueHelper.InternalClearMRef(GetTypeKind(T))
    else
      FQueueHelper.InternalClearManaged
  else
  case SizeOf(T) of
    1: FQueueHelper.InternalClear1;
    2: FQueueHelper.InternalClear2;
    4: FQueueHelper.InternalClear4;
    8: FQueueHelper.InternalClear8;
  else
    FQueueHelper.InternalClearN;
  end;
end;

function TQueue<T>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(Count);
end;

procedure TQueue<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TQueue<T>.InternalCompare(const Left, Right): Integer;
begin
  Result := 0;
end;

procedure TQueue<T>.InternalNotify(const Item; Action: TCollectionNotification);
begin
  Notify(T(Item), Action);
end;

function TQueue<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TQueue<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TQueue<T>.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(const AQueue: TQueue<T>);
begin
  inherited Create;
  FQueue := AQueue;
  FIndex := -1;
end;

function TQueue<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TQueue<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TQueue<T>.TEnumerator.GetCurrent: T;
begin
  Result := FQueue.FItems[(FQueue.FQueueHelper.FTail + FIndex) mod Length(FQueue.FItems)];
end;

function TQueue<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FQueue.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FQueue.Count;
end;

{ TStackHelper }

function TStackHelper.GetElSize: Integer;
begin
  Result := FLH.ElSize;
end;

function TStackHelper.GetElType: Pointer;
begin
  Result := FLH.ElType
end;

function TStackHelper.GetFItems: PPointer;
begin
  Result := FLH.FItems;
end;

procedure TStackHelper.CheckEmpty;
begin
  if FLH.FCount = 0 then
    raise EListError.CreateRes(Pointer(@SUnbalancedOperation));
end;

procedure TStackHelper.CheckGrow;
begin
  if FLH.FCount = DynArraySize(FItems^) then
    InternalGrow;
end;

procedure TStackHelper.PopAdjust(const Value; Notification: TCollectionNotification);
begin
  Dec(FLH.FCount);
  FLH.FNotify(Value, Notification);
end;

procedure TStackHelper.PushAdjust(const Value);
begin
  Inc(FLH.FCount);
  FLH.FNotify(Value, cnAdded);
end;

procedure TStackHelper.InternalDoPopString(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  string(Item) := PString(FItems^)[FLH.FCount - 1];
  if not Peek then
  begin
    PString(FItems^)[FLH.FCount - 1] := '';
    PopAdjust(Item, Notification);
  end;
end;

procedure TStackHelper.InternalDoPopInterface(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  IInterface(Item) := TListHelper.PInterface(FItems^)[FLH.FCount - 1];
  if not Peek then
  begin
    TListHelper.PInterface(FItems^)[FLH.FCount - 1] := nil;
    PopAdjust(Item, Notification);
  end;
end;

{$IF not Defined(NEXTGEN)}
procedure TStackHelper.InternalDoPopAnsiString(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  AnsiString(Item) := PAnsiString(FItems^)[FLH.FCount - 1];
  if not Peek then
  begin
    PAnsiString(FItems^)[FLH.FCount - 1] := '';
    PopAdjust(Item, Notification);
  end;
end;

procedure TStackHelper.InternalDoPopWideString(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  WideString(Item) := PWideString(FItems^)[FLH.FCount - 1];
  if not Peek then
  begin
    PWideString(FItems^)[FLH.FCount - 1] := '';
    PopAdjust(Item, Notification);
  end;
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TStackHelper.InternalDoPopObject(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  TObject(Item) := PObject(FItems^)[FLH.FCount - 1];
  if not Peek then
  begin
    PObject(FItems^)[FLH.FCount - 1] := nil;
    PopAdjust(Item, Notification);
  end;
end;
{$IFEND}

procedure TStackHelper.InternalClearString;
var
  Temp: string;
begin
  while FLH.FCount > 0 do
    InternalDoPopString(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalClearInterface;
var
  Temp: IInterface;
begin
  while FLH.FCount > 0 do
    InternalDoPopInterface(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

{$IF not Defined(NEXTGEN)}
procedure TStackHelper.InternalClearAnsiString;
var
  Temp: AnsiString;
begin
  while FLH.FCount > 0 do
    InternalDoPopAnsiString(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalClearWideString;
var
  Temp: WideString;
begin
  while FLH.FCount > 0 do
    InternalDoPopWideString(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TStackHelper.InternalClearObject;
var
  Temp: TObject;
begin
  while FLH.FCount > 0 do
    InternalDoPopObject(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;
{$IFEND}

procedure TStackHelper.InternalClear1;
var
  Temp: Byte;
begin
  while FLH.FCount > 0 do
    InternalDoPop1(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalClear2;
var
  Temp: Word;
begin
  while FLH.FCount > 0 do
    InternalDoPop2(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalClear4;
var
  Temp: Cardinal;
begin
  while FLH.FCount > 0 do
    InternalDoPop4(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalClear8;
var
  Temp: UInt64;
begin
  while FLH.FCount > 0 do
    InternalDoPop8(cnRemoved, False, Temp);
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalClearManaged;
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
begin
  PTemp := @STemp[0];
  DTemp := nil;
  try
    if SizeOf(STemp) < ElSize then
    begin
      DTemp := AllocMem(ElSize);
      PTemp := DTemp;
    end else
      FillChar(STemp, SizeOf(STemp), 0);
    while FLH.FCount > 0 do
      InternalDoPopManaged(cnRemoved, False, PTemp[0]);
  finally
    FinalizeArray(@PTemp[0], ElType, 1);
    FreeMem(DTemp);
  end;
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalClearMRef(Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: InternalClearString;
    TTypeKind.tkInterface: InternalClearInterface;
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: InternalClearAnsiString;
    TTypeKind.tkWString: InternalClearWideString;
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: InternalClearObject;
{$IFEND}
  end;
end;

procedure TStackHelper.InternalClearN;
var
  STemp: array[0..63] of Byte;
  DTemp: PByte;
  PTemp: PByte;
begin
  PTemp := @STemp[0];
  DTemp := nil;
  try
    if SizeOf(STemp) < ElSize then
    begin
      GetMem(DTemp, ElSize);
      PTemp := DTemp;
    end;
    while FLH.FCount > 0 do
      InternalDoPopN(cnRemoved, False, PTemp[0]);
  finally
    FreeMem(DTemp);
  end;
  DynArrayClear(FItems^, FLH.FTypeInfo);
end;

procedure TStackHelper.InternalDoPop1(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Byte(Item) := PByte(FItems^)[FLH.FCount - 1];
  if not Peek then
    PopAdjust(Item, Notification);
end;

procedure TStackHelper.InternalDoPop2(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Word(Item) := PWord(FItems^)[FLH.FCount - 1];
  if not Peek then
    PopAdjust(Item, Notification);
end;

procedure TStackHelper.InternalDoPop4(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Cardinal(Item) := PCardinal(FItems^)[FLH.FCount - 1];
  if not Peek then
    PopAdjust(Item, Notification);
end;

procedure TStackHelper.InternalDoPop8(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  UInt64(Item) := PUInt64(FItems^)[FLH.FCount - 1];
  if not Peek then
    PopAdjust(Item, Notification);
end;

procedure TStackHelper.InternalDoPopManaged(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  System.CopyArray(@Item, @PByte(FItems^)[(FLH.FCount - 1) * ElSize], ElType, 1);
  if not Peek then
  begin
    FinalizeArray(@PByte(FItems^)[(FLH.FCount - 1) * ElSize], ElType, 1);
    PopAdjust(Item, Notification);
  end;
end;

procedure TStackHelper.InternalDoPopMRef(Notification: TCollectionNotification; Peek: Boolean; out Item; Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: InternalDoPopString(Notification, Peek, Item);
    TTypeKind.tkInterface: InternalDoPopInterface(Notification, Peek, Item);
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: InternalDoPopAnsiString(Notification, Peek, Item);
    TTypeKind.tkWString: InternalDoPopWideString(Notification, Peek, Item);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: InternalDoPopObject(Notification, Peek, Item);
{$IFEND}
  end;
end;

procedure TStackHelper.InternalDoPopN(Notification: TCollectionNotification; Peek: Boolean; out Item);
begin
  CheckEmpty;
  Move(PByte(FItems^)[(FLH.FCount - 1) * ElSize], Item, ElSize);
  if not Peek then
    PopAdjust(Item, Notification);
end;

procedure TStackHelper.InternalGrow;
var
  NewCap: NativeInt;
begin
  NewCap := DynArraySize(FItems^) * 2;
  if NewCap = 0 then
    NewCap := 4
  else if NewCap < 0 then
    OutOfMemoryError;
  DynArraySetLength(FItems^, FLH.FTypeInfo, 1, @NewCap);
end;

procedure TStackHelper.InternalPushString(const Value);
begin
  CheckGrow;
  PString(FItems^)[FLH.FCount] := string(Value);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalPushInterface(const Value);
begin
  CheckGrow;
  TListHelper.PInterface(FItems^)[FLH.FCount] := IInterface(Value);
  PushAdjust(Value);
end;

{$IF not Defined(NEXTGEN)}
procedure TStackHelper.InternalPushAnsiString(const Value);
begin
  CheckGrow;
  PAnsiString(FItems^)[FLH.FCount] := AnsiString(Value);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalPushWideString(const Value);
begin
  CheckGrow;
  PWideString(FItems^)[FLH.FCount] := WideString(Value);
  PushAdjust(Value);
end;
{$IFEND}

{$IF Defined(AUTOREFCOUNT)}
procedure TStackHelper.InternalPushObject(const Value);
begin
  CheckGrow;
  PObject(FItems^)[FLH.FCount] := TObject(Value);
  PushAdjust(Value);
end;
{$IFEND}

procedure TStackHelper.InternalPush1(const Value);
begin
  CheckGrow;
  PByte(FItems^)[FLH.FCount] := Byte(Value);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalPush2(const Value);
begin
  CheckGrow;
  PWord(FItems^)[FLH.FCount] := Word(Value);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalPush4(const Value);
begin
  CheckGrow;
  PCardinal(FItems^)[FLH.FCount] := Cardinal(Value);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalPush8(const Value);
begin
  CheckGrow;
  PUInt64(FItems^)[FLH.FCount] := UInt64(Value);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalPushManaged(const Value);
begin
  CheckGrow;
  System.CopyArray(@PByte(FItems^)[FLH.FCount * ElSize], @Value, ElType, 1);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalPushMRef(const Value; Kind: TTypeKind);
begin
  case Kind of
    TTypeKind.tkUString: InternalPushString(Value);
    TTypeKind.tkInterface: InternalPushInterface(Value);
{$IF not Defined(NEXTGEN)}
    TTypeKind.tkLString: InternalPushAnsiString(Value);
    TTypeKind.tkWString: InternalPushWideString(Value);
{$IFEND}
{$IF Defined(AUTOREFCOUNT)}
    TTypeKind.tkClass: InternalPushObject(Value);
{$IFEND}
  end;
end;

procedure TStackHelper.InternalPushN(const Value);
begin
  CheckGrow;
  Move(Value, PByte(FItems^)[FLH.FCount * ElSize], ElSize);
  PushAdjust(Value);
end;

procedure TStackHelper.InternalSetCapacity(Value: Integer);
var
  Size: NativeInt;
begin
  if Value < FLH.FCount then
    raise EArgumentOutOfRangeException.CreateRes(Pointer(@SArgumentOutOfRange));
  Size := Value;
  DynArraySetLength(FItems^, FLH.FTypeInfo, 1, @Size);
end;


{ TStack<T> }

procedure TStack<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

constructor TStack<T>.Create(const Collection: TEnumerable<T>);
var
  Item: T;
begin
  inherited Create;
  FStackHelper.FLH.FNotify := InternalNotify;
  FStackHelper.FLH.FTypeInfo := TypeInfo(arrayOfT);
  for Item in Collection do
    Push(Item);
end;

constructor TStack<T>.Create;
begin
  inherited Create;
  FStackHelper.FLH.FNotify := InternalNotify;
  FStackHelper.FLH.FTypeInfo := TypeInfo(arrayOfT);
end;

destructor TStack<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TStack<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

procedure TStack<T>.InternalNotify(const Item; Action: TCollectionNotification);
begin
  Notify(T(Item), Action);
end;

procedure TStack<T>.Push(const Value: T);
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FStackHelper.InternalPushMRef(Value, GetTypeKind(T))
    else
      FStackHelper.InternalPushManaged(Value)
  else
  case SizeOf(T) of
    1: FStackHelper.InternalPush1(Value);
    2: FStackHelper.InternalPush2(Value);
    4: FStackHelper.InternalPush4(Value);
    8: FStackHelper.InternalPush8(Value);
  else
    FStackHelper.InternalPushN(Value);
  end;
end;

function TStack<T>.DoPop(Notification: TCollectionNotification): T;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FStackHelper.InternalDoPopMRef(Notification, False, Result, GetTypeKind(T))
    else
      FStackHelper.InternalDoPopManaged(Notification, False, Result)
  else
  case SizeOf(T) of
    1: FStackHelper.InternalDoPop1(Notification, False, Result);
    2: FStackHelper.InternalDoPop2(Notification, False, Result);
    4: FStackHelper.InternalDoPop4(Notification, False, Result);
    8: FStackHelper.InternalDoPop8(Notification, False, Result);
  else
    FStackHelper.InternalDoPopN(Notification, False, Result);
  end;
end;

procedure TStack<T>.DoSetCapacity(Value: Integer);
begin
  FStackHelper.InternalSetCapacity(Value);
end;

function TStack<T>.Extract: T;
begin
  Result := DoPop(cnExtracted);
end;

function TStack<T>.Peek: T;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FStackHelper.InternalDoPopMRef(cnRemoved, True, Result, GetTypeKind(T))
    else
      FStackHelper.InternalDoPopManaged(cnRemoved, True, Result)
  else
  case SizeOf(T) of
    1: FStackHelper.InternalDoPop1(cnRemoved, True, Result);
    2: FStackHelper.InternalDoPop2(cnRemoved, True, Result);
    4: FStackHelper.InternalDoPop4(cnRemoved, True, Result);
    8: FStackHelper.InternalDoPop8(cnRemoved, True, Result);
  else
    FStackHelper.InternalDoPopN(cnRemoved, True, Result);
  end;
end;

function TStack<T>.Pop: T;
begin
  Result := DoPop(cnRemoved);
end;

procedure TStack<T>.Clear;
begin
  if IsManagedType(T) then
    if (SizeOf(T) = SizeOf(Pointer)) and (GetTypeKind(T) <> tkRecord) then
      FStackHelper.InternalClearMRef(GetTypeKind(T))
    else
      FStackHelper.InternalClearManaged
  else
  case SizeOf(T) of
    1: FStackHelper.InternalClear1;
    2: FStackHelper.InternalClear2;
    4: FStackHelper.InternalClear4;
    8: FStackHelper.InternalClear8;
  else
    FStackHelper.InternalClearN;
  end;
end;

function TStack<T>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(Count);
end;

procedure TStack<T>.TrimExcess;
begin
  SetLength(FItems, Count);
end;

function TStack<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TStack<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

constructor TStack<T>.TEnumerator.Create(const AStack: TStack<T>);
begin
  inherited Create;
  FStack := AStack;
  FIndex := -1;
end;

function TStack<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TStack<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TStack<T>.TEnumerator.GetCurrent: T;
begin
  Result := FStack.FItems[FIndex];
end;

function TStack<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FStack.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FStack.Count;
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

{ TObjectList<T> }

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer);
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(Collection);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectList<T>.Notify(const Value: T; Action: TCollectionNotification);
begin
  inherited;
  if OwnsObjects and (Action = cnRemoved) then
    Value.DisposeOf;
end;

{ TObjectQueue<T> }

constructor TObjectQueue<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectQueue<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(Collection);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectQueue<T>.Dequeue;
begin
  inherited Dequeue;
end;

procedure TObjectQueue<T>.Notify(const Value: T; Action: TCollectionNotification);
begin
  inherited;
  if OwnsObjects and (Action = cnRemoved) then
    Value.DisposeOf;
end;

{ TObjectStack<T> }

constructor TObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectStack<T>.Create(const Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(Collection);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectStack<T>.Notify(const Value: T; Action: TCollectionNotification);
begin
  inherited;
  if OwnsObjects and (Action = cnRemoved) then
    Value.DisposeOf;
end;

procedure TObjectStack<T>.Pop;
begin
  inherited Pop;
end;

{ TObjectDictionary<TKey,TValue> }

procedure TObjectDictionary<TKey,TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsKeys in FOwnerships) then
    PObject(@Key)^.DisposeOf;
end;

procedure TObjectDictionary<TKey,TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsValues in FOwnerships) then
    PObject(@Value)^.DisposeOf;
end;

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
  inherited Create(ACapacity, AComparer);
  if doOwnsKeys in Ownerships then
  begin
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));
  end;

  if doOwnsValues in Ownerships then
  begin
    if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(Pointer(@SInvalidCast));
  end;
  FOwnerships := Ownerships;
end;


end.
