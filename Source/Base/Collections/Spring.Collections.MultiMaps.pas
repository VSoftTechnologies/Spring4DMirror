{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2023 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Collections.MultiMaps;

interface

uses
  Classes,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Trees,
  Spring.Events,
  Spring.Events.Base,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  // same layout as TCollectionWrapper<T>
  TCollectionWrapper = class(TEnumerableBase)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as TCollectionWrapper<T>.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TCollectionWrapper;
      fCollection: IEnumerable;
      fEnumerator: IEnumerator;
      procedure ValidateEnumerator;
      function MoveNext: Boolean;
    end;
  protected
    // TEnumerableBase<T>
    fComparer: IInterface;
    fCollection: IEnumerable;
    fOnDestroy: TNotifyEventImpl;
    fUpdateValues: TNotifyEvent;
    procedure RefreshIfEmpty;
    procedure HandleDestroy(Sender: TObject);
    function GetCount: Integer;
    procedure GetEnumerator(enumerator: PPointer; vtable: PEnumeratorVtable;
      typeInfo, getCurrent: Pointer);
    function GetNonEnumeratedCount: Integer;
  end;

  // same layout as TCollectionWrapper
  TCollectionWrapper<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as TCollectionWrapper.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TCollectionWrapper;
      fCollection: ICollection<T>;
      fEnumerator: IEnumerator<T>;
      function GetCurrent: T;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fCollection: ICollection<T>;
    fOnDestroy: TNotifyEventImpl;
    fUpdateValues: TNotifyEvent;
    function GetCount: Integer;
    function GetNonEnumeratedCount: Integer;
  public
    procedure BeforeDestruction; override;

    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  end;

  TCollectionWrapper<TKey, TValue> = class(TCollectionWrapper<TValue>)
  private
    fKey: TKey;
  end;

  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    TItem = packed record
      HashCode: Integer;
      Key: TKey;
      Values: ICollection<TValue>;
    end;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as THashMapInnerCollection.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fHashTable: PHashTable;
      fOffset: Integer;
      fIndex: Integer;
      fVersion: Integer;
      fItem: PItem;
      fEnumerator: IEnumerator<TValue>;
      function GetCurrent: TKeyValuePair;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TKeyCollection = THashMapInnerCollection<TKey>;
    TValueCollection = THashMapInnerCollection<TValue>;
    TCollectionWrapper = TCollectionWrapper<TKey, TValue>;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fCount: Integer;
    fOnDestroy: TNotifyEventImpl;
    function CreateWrappedCollection(const key: TKey): IReadOnlyCollection<TValue>;
    procedure UpdateValues(collection: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetNonEnumeratedCount: Integer;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    procedure CreateCollection(var result: ICollection<TValue>); virtual; abstract;
    procedure DoRemove(const entry: THashTableEntry;
      action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
    procedure DoRemoveValues(item: PItem; action: TCollectionChangedAction);
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean; overload;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;
    function Extract(const key: TKey): ICollection<TValue>; overload;
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;
  {$ENDREGION}
  end;

  TSortedMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    PNode = ^TNode;
    TNode = packed record // same layout as TRedBlackTreeBase<TKey, IInterface>.TNode
      Parent, Right, Left: Pointer;
      Key: TKey;
      Values: ICollection<TValue>;
    end;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      // same layout as TTreeMapInnerCollection.TEnumerator
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fTree: TBinaryTree;
      fOffset: Integer;
      fSourceVersion: PInteger;
      fNode: PNode;
      fVersion: Integer;
      fItem: Pointer;
      fEnumerator: IEnumerator<TValue>;
      function GetCurrent: TKeyValuePair;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TKeyCollection = TTreeMapInnerCollection<TKey>;
    TValueCollection = TTreeMapInnerCollection<TValue>;
    TCollectionWrapper = TCollectionWrapper<TKey, TValue>;
  {$ENDREGION}
  private
    fTree: TRedBlackTreeBase<TKey, IInterface>;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fVersion: Integer;
    fCount: Integer;
    fOnDestroy: TNotifyEventImpl;
    fKeyComparer: IComparer<TKey>;
    fOwnerships: TDictionaryOwnerships;
    function CreateWrappedCollection(const key: TKey): IReadOnlyCollection<TValue>;
    procedure UpdateValues(collection: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetNonEnumeratedCount: Integer;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    procedure CreateCollection(var result: ICollection<TValue>); virtual; abstract;
    procedure DoRemove(const node: PNode; action: TCollectionChangedAction;
      const extractTarget: ICollection<TValue>; deleteNode: Boolean = True);
    procedure DoRemoveValues(node: PNode; action: TCollectionChangedAction);
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); inline;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); inline;
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      ownerships: TDictionaryOwnerships);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TKeyValuePair>;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    function TryAdd(const key: TKey; const value: TValue): Boolean;
    function Remove(const key: TKey): Boolean; overload;
    function Remove(const key: TKey; const value: TValue): Boolean; overload;
    function Extract(const key: TKey; const value: TValue): TKeyValuePair; overload;
    function Contains(const key: TKey; const value: TValue): Boolean; overload;
    function ContainsKey(const key: TKey): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function Add(const key: TKey; const value: TValue): Boolean; overload;
    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;
    function Extract(const key: TKey): ICollection<TValue>; overload;
    function TryGetValues(const key: TKey; var values: IReadOnlyCollection<TValue>): Boolean;
  {$ENDREGION}
  end;

  TListMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  end;

  THashMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IEqualityComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TTreeMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IEqualityComparer<TKey>;
      const valueComparer: IComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TSortedListMultiMap<TKey, TValue> = class(TSortedMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  end;

  TSortedHashMultiMap<TKey, TValue> = class(TSortedMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IEqualityComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      const valueComparer: IEqualityComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TSortedTreeMultiMap<TKey, TValue> = class(TSortedMultiMapBase<TKey, TValue>, IInterface,
    IEnumerable<TPair<TKey, TValue>>, IReadOnlyCollection<TPair<TKey, TValue>>,
    IReadOnlyMap<TKey, TValue>, IReadOnlyMultiMap<TKey, TValue>,
    ICollection<TPair<TKey, TValue>>, IMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    fValueComparer: IComparer<TValue>;
  protected
    procedure CreateCollection(var result: ICollection<TValue>); override;
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
  public
    constructor Create(const keyComparer: IComparer<TKey>;
      const valueComparer: IComparer<TValue>;
      ownerships: TDictionaryOwnerships);
  end;

  TFoldedListMultiMap<TKey, TValue> = class(TListMultiMap<TKey, TValue>)
  private
    fElementType: PTypeInfo;
    fKeyType: PTypeInfo;
    fValueType: PTypeInfo;
  protected
    function GetElementType: PTypeInfo; override;
    function GetKeyType: PTypeInfo; override;
    function GetValueType: PTypeInfo; override;
  public
    constructor Create(keyType, valueType, elementType: PTypeInfo;
      const keyComparer: IEqualityComparer<TKey>;
      ownerships: TDictionaryOwnerships);
  end;

  TCollections = class(Spring.Collections.TCollections);

implementation

uses
  Types,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Collections.Sets,
  Spring.Comparers,
  Spring.ResourceStrings;


{$REGION 'TCollectionWrapper'}

function TCollectionWrapper.GetCount: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.Count;
end;

procedure TCollectionWrapper.GetEnumerator(enumerator: PPointer;
  vtable: PEnumeratorVtable; typeInfo, getCurrent: Pointer);
begin
  _AddRef;
  RefreshIfEmpty;
  with PEnumerator(TEnumeratorBlock.Create(enumerator, vtable,
    typeInfo, getCurrent, @TEnumerator.MoveNext))^ do
  begin
    Parent := Self;
    fCollection := Self.fCollection;
    {$IFDEF MSWINDOWS}
    IEnumerableInternal(fCollection).GetEnumerator(fEnumerator);
    {$ELSE}
    fEnumerator := fCollection.GetEnumerator;
    {$ENDIF}
  end;
end;

function TCollectionWrapper.GetNonEnumeratedCount: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.GetNonEnumeratedCount;
end;

procedure TCollectionWrapper.HandleDestroy(Sender: TObject);
begin
  fOnDestroy := nil;
  fUpdateValues := nil;
end;

procedure TCollectionWrapper.RefreshIfEmpty;
begin
  if fCollection.IsEmpty and Assigned(fUpdateValues) then
    fUpdateValues(Self);
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper.TEnumerator'}

function TCollectionWrapper.TEnumerator.MoveNext: Boolean;
begin
  ValidateEnumerator;
  Result := fEnumerator.MoveNext;
end;

procedure TCollectionWrapper.TEnumerator.ValidateEnumerator;
begin
  Parent.RefreshIfEmpty;
  if Parent.fCollection <> fCollection then
    RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>'}

procedure TCollectionWrapper<T>.BeforeDestruction;
begin
  if Assigned(fOnDestroy) then
    fOnDestroy.Remove(TCollectionWrapper(Self).HandleDestroy);
  inherited;
end;

function TCollectionWrapper<T>.Contains(
  const value: T; const comparer: IEqualityComparer<T>): Boolean;
begin
  TCollectionWrapper(Self).RefreshIfEmpty;
  Result := fCollection.Contains(value, comparer);
end;

function TCollectionWrapper<T>.GetCount: Integer;
begin
  Result := TCollectionWrapper(Self).GetCount;
end;

function TCollectionWrapper<T>.GetEnumerator: IEnumerator<T>; //FI:W521
begin
  TCollectionWrapper(Self).GetEnumerator(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent);
end;

function TCollectionWrapper<T>.GetNonEnumeratedCount: Integer;
begin
  Result := TCollectionWrapper(Self).GetNonEnumeratedCount;
end;

function TCollectionWrapper<T>.ToArray: TArray<T>;
begin
  TCollectionWrapper(Self).RefreshIfEmpty;
  Result := fCollection.ToArray;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>.TEnumerator'}

function TCollectionWrapper<T>.TEnumerator.GetCurrent: T;
begin
  TCollectionWrapper.PEnumerator(@Self).ValidateEnumerator;
  {$IFDEF RSP31615}
  if IsManagedType(T) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result)
  else
  {$ENDIF}
  Result := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>'}

constructor TMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
end;

procedure TMultiMapBase<TKey, TValue>.AfterConstruction;
var
  keyType, valueType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  valueType := GetValueType;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  fHashTable.Initialize(@TComparerThunks<TKey>.Equals, @TComparerThunks<TKey>.GetHashCode, keyType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<TKey>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<TKey>.FindWithComparer;

  fKeys := TKeyCollection.Create(Self, @fHashTable, nil, keyType, 0);
  fValues := TValueCollection.Create(Self, @fHashTable, nil, valueType,
    SizeOf(TKey), @fCount, TCollectionThunks<TValue>.Contains);
  fOnDestroy := TNotifyEventImpl.Create;
  fOnDestroy.UseFreeNotification := False;
end;

procedure TMultiMapBase<TKey, TValue>.BeforeDestruction;
begin
  fOnDestroy.Invoke(Self);
  fOnDestroy.Free;
  Clear;
  fKeys.Free;
  fValues.Free;
  inherited BeforeDestruction;
end;

function TMultiMapBase<TKey, TValue>.CreateWrappedCollection(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  collection: TCollectionWrapper;
begin
  collection := TCollectionWrapper.Create;
  collection.fKey := key;
  collection.fOnDestroy := fOnDestroy;
  fOnDestroy.Add(Spring.Collections.MultiMaps.TCollectionWrapper(collection).HandleDestroy);
  collection.fUpdateValues := UpdateValues;
  CreateCollection(collection.fCollection);

  Result := collection;
end;

function TMultiMapBase<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAdd(key, value);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    TryAdd(key, values[i]);
end;

procedure TMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: IEnumerable<TValue>);
var
  enumerator: IEnumerator<TValue>;
  item: TValue;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    TryAdd(key, item);
  end;
end;

procedure TMultiMapBase<TKey, TValue>.Clear;
var
  i: NativeInt;
begin
  for i := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[i].HashCode >= 0 then
      DoRemoveValues(@TItems(fHashTable.Items)[i], caRemoved);
  fHashTable.Clear;
  fCount := 0;
end;

function TMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if not Assigned(item) then Exit(Boolean(Pointer(item)));
  Result := item.Values.Contains(value);
end;

function TMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  Result := Assigned(item);
end;

function TMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  i: Integer;
begin
  for i := 0 to fHashTable.ItemCount - 1 do
    if TItems(fHashTable.Items)[i].HashCode >= 0 then
      if TItems(fHashTable.Items)[i].Values.Contains(value) then
        Exit(True);
  Result := False;
end;

procedure TMultiMapBase<TKey, TValue>.DoRemove(const entry: THashTableEntry;
  action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
var
  item: PItem;
  value: TValue;
begin
  item := fHashTable.DeleteEntry(entry);
  Dec(fCount, item.Values.Count);
  DoRemoveValues(item, action);
  if action = caExtracted then
    item.Values.MoveTo(extractTarget);
  item.HashCode := RemovedFlag;
  item.Key := Default(TKey);
  item.Values := nil;
end;

procedure TMultiMapBase<TKey, TValue>.DoRemoveValues(item: PItem;
  action: TCollectionChangedAction);
var
  enumerator: IEnumerator<TValue>;
  value: TValue;
begin
  enumerator := item.values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(value)
    else
    {$ENDIF}
    value := enumerator.Current;
    if Assigned(Notify) then
      DoNotify(item.key, value, action);
    if fOnValueChanged.CanInvoke then
      fOnValueChanged.Invoke(Self, value, action);
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
    if (action = caRemoved) and (doOwnsValues in fHashTable.Ownerships) then
      PObject(@value).Free;
  end;
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item.key, action);
  if action = caRemoved then
  begin
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TKey) = tkClass then
    {$ENDIF}
    if doOwnsKeys in fHashTable.Ownerships then
      PObject(@item.key).Free;
    item.values.Clear;
  end;
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey): ICollection<TValue>;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  CreateCollection(Result);
  if fHashTable.FindEntry(key, entry) then
    DoRemove(entry, caExtracted, Result);
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey; const value: TValue): TKeyValuePair;
var
  item: PItem;
  count, newCount: Integer;
begin
  Result.Key := key;
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
  begin
    count := item.Values.Count;
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      TCollectionThunks<TValue>.ICollectionInternal(item.Values).Extract(
        {$IFDEF CPUX64}Result.Value, {$ENDIF}value{$IFDEF CPUX86}, Result.Value{$ENDIF})
    else
    {$ENDIF}
    Result.Value := item.Values.Extract(value);
    newCount := item.Values.Count;
    if newCount < count then
    begin
      Dec(fCount, count);
      if Assigned(Notify) then
        DoNotify(key, value, caExtracted);
      if fOnValueChanged.CanInvoke then
        fOnValueChanged.Invoke(Self, Result.Value, caExtracted);
      if newCount = 0 then
        if fOnKeyChanged.CanInvoke then
          fOnKeyChanged.Invoke(Self, item.Key, caExtracted);
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent,
    @THashMapInnerCollection.TEnumerator.MoveNext_MultiMap))^ do
  begin
    Parent := Self;
    fHashTable := @Self.fHashTable;
    fOffset := KeyOffset + SizeOf(TKey);
    fVersion := Self.fHashTable.Version;
  end;
end;

function TMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
    Result := item.Values as IReadOnlyCollection<TValue>
  else
    Result := CreateWrappedCollection(key);
end;

function TMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TMultiMapBase<TKey, TValue>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  entry: THashTableEntry;
  item: PItem;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := fHashTable.FindEntry(key, entry);
  if not Result then Exit;
  item := @TItems(fHashTable.Items)[entry.ItemIndex];
  Result := item.Values.Remove(value);
  if not Result then Exit;
  Dec(fCount);
  if Assigned(Notify) then
    DoNotify(item.Key, value, caRemoved);
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, value, caRemoved);
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
  {$ENDIF}
  if doOwnsValues in fHashTable.Ownerships then
    PObject(@item).Free;
  if item.Values.Any then
    {$Q-}
    Inc(PInteger(@fHashTable.Version)^)
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
  else
    DoRemove(entry, caRemoved, nil);
  Result := True;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  Result := fHashTable.FindEntry(key, entry);
  if Result then
  begin
    DoRemove(entry, caRemoved, nil);
    Result := True;
  end;
end;

function TMultiMapBase<TKey, TValue>.TryAdd(const key: TKey; const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key, InsertNonExisting);
  if not Assigned(item.Values) then
  begin
    item.Key := key;
    CreateCollection(item.Values);
    if fOnKeyChanged.CanInvoke then
      fOnKeyChanged.Invoke(Self, item.Key, caAdded);
  end;

  Result := item.Values.Add(value);
  if Result then
  begin
    Inc(fCount);
    {$Q-}
    Inc(PInteger(@fHashTable.Version)^);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    if fOnValueChanged.CanInvoke then
      fOnValueChanged.Invoke(Self, value, caAdded);
    if Assigned(Notify) then
    begin
      DoNotify(item.Key, value, caAdded);
      Result := True;
    end;
  end;
end;

procedure TMultiMapBase<TKey, TValue>.UpdateValues(collection: TObject);
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(TCollectionWrapper(collection).fKey);
  if Assigned(item) then
    TCollectionWrapper(collection).fCollection := item.Values;
end;

function TMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  var values: IReadOnlyCollection<TValue>): Boolean;
var
  temp: Pointer;
  item: PItem;
begin
  temp := IHashTable<TKey>(@fHashTable).Find(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  item := temp;
  values := item.Values as IReadOnlyCollection<TValue>;
  Result := True;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fItem.Key;
  {$IFDEF RSP31615}
  if IsManagedType(TValue) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result.Value)
  else
  {$ENDIF}
  Result.Value := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TSortedMultiMapBase<TKey, TValue>'}

constructor TSortedMultiMapBase<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fKeyComparer := keyComparer;
  fOwnerships := ownerships;
end;

procedure TSortedMultiMapBase<TKey, TValue>.AfterConstruction;
var
  keyType, valueType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  valueType := GetValueType;
  fTree := TRedBlackTreeBase<TKey,IInterface>.Create(fKeyComparer);
  fKeys := TKeyCollection.Create(Self, fTree, @fVersion, nil, keyType, 0);
  fValues := TValueCollection.Create(Self, fTree, @fVersion, nil, valueType,
    SizeOf(TKey), @fCount, TCollectionThunks<TValue>.Contains);
  fOnDestroy := TNotifyEventImpl.Create;
  fOnDestroy.UseFreeNotification := False;
end;

procedure TSortedMultiMapBase<TKey, TValue>.BeforeDestruction;
begin
  fOnDestroy.Invoke(Self);
  fOnDestroy.Free;
  Clear;
  fTree.Free;
  fKeys.Free;
  fValues.Free;
  inherited BeforeDestruction;
end;

function TSortedMultiMapBase<TKey, TValue>.CreateWrappedCollection(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  collection: TCollectionWrapper;
begin
  collection := TCollectionWrapper.Create;
  collection.fKey := key;
  collection.fOnDestroy := fOnDestroy;
  fOnDestroy.Add(Spring.Collections.MultiMaps.TCollectionWrapper(collection).HandleDestroy);
  collection.fUpdateValues := UpdateValues;
  CreateCollection(collection.fCollection);

  Result := collection;
end;

procedure TSortedMultiMapBase<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
    PObject(@item).Free;
end;

procedure TSortedMultiMapBase<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
    PObject(@item).Free;
end;

function TSortedMultiMapBase<TKey, TValue>.Add(const key: TKey;
  const value: TValue): Boolean;
begin
  Result := TryAdd(key, value);
end;

procedure TSortedMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: array of TValue);
var
  i: Integer;
begin
  for i := 0 to High(values) do
    TryAdd(key, values[i]);
end;

procedure TSortedMultiMapBase<TKey, TValue>.AddRange(const key: TKey;
  const values: IEnumerable<TValue>);
var
  enumerator: IEnumerator<TValue>;
  item: TValue;
begin
  if not Assigned(values) then RaiseHelper.ArgumentNil(ExceptionArgument.values);

  enumerator := values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(item)
    else
    {$ENDIF}
    item := enumerator.Current;
    TryAdd(key, item);
  end;
end;

procedure TSortedMultiMapBase<TKey, TValue>.Clear;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    DoRemove(PNode(node), caRemoved, nil, False);
    node := node.Next;
  end;
  fTree.Clear;
  fCount := 0;
end;

function TSortedMultiMapBase<TKey, TValue>.Contains(const key: TKey;
  const value: TValue): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  Result := PNode(node).Values.Contains(value);
end;

function TSortedMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  Result := Assigned(node);
end;

function TSortedMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  node: PBinaryTreeNode;
begin
  node := fTree.Root.LeftMost;
  while Assigned(node) do
  begin
    if PNode(node).Values.Contains(value) then Break;
    node := node.Next;
  end;
  Result := Assigned(node);
end;

procedure TSortedMultiMapBase<TKey, TValue>.DoRemove(const node: PNode;
  action: TCollectionChangedAction; const extractTarget: ICollection<TValue>;
  deleteNode: Boolean);
begin
  Dec(fCount, node.Values.Count);
  DoRemoveValues(node, action);
  if action = caExtracted then
    node.Values.MoveTo(extractTarget);
  if deleteNode then
    fTree.DeleteNode(Pointer(node));
end;

procedure TSortedMultiMapBase<TKey, TValue>.DoRemoveValues(node: PNode;
  action: TCollectionChangedAction);
var
  enumerator: IEnumerator<TValue>;
  value: TValue;
begin
  enumerator := node.values.GetEnumerator;
  while enumerator.MoveNext do
  begin
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      IEnumeratorInternal(enumerator).GetCurrent(value)
    else
    {$ENDIF}
    value := enumerator.Current;
    if Assigned(Notify) then
      DoNotify(node.key, value, action);
    if fOnValueChanged.CanInvoke then
      fOnValueChanged.Invoke(Self, value, action);
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TValue) = tkClass then
    {$ENDIF}
    if (action = caRemoved) and (doOwnsValues in fOwnerships) then
      PObject(@value).Free;
  end;
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, node.key, action);
  if action = caRemoved then
  begin
    {$IFDEF DELPHIXE7_UP}
    if GetTypeKind(TKey) = tkClass then
    {$ENDIF}
    if doOwnsKeys in fOwnerships then
      PObject(@node.key).Free;
    node.values.Clear;
    node.values := nil;
  end;
end;

function TSortedMultiMapBase<TKey, TValue>.Extract(
  const key: TKey): ICollection<TValue>;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  CreateCollection(Result);
  if Assigned(node) then
    DoRemove(node, caExtracted, Result);
end;

function TSortedMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  node: PNode;
  count, newCount: Integer;
begin
  Result.Key := key;
  node := Pointer(fTree.FindNode(key));
  if Assigned(node) then
  begin
    count := node.Values.Count;
    {$IFDEF RSP31615}
    if IsManagedType(TValue) then
      TCollectionThunks<TValue>.ICollectionInternal(node.Values).Extract(
        {$IFDEF CPUX64}Result.Value, {$ENDIF}value{$IFDEF CPUX86}, Result.Value{$ENDIF})
    else
    {$ENDIF}
    Result.Value := node.Values.Extract(value);
    newCount := node.Values.Count;
    if newCount < count then
    begin
      Dec(fCount, count);
      if Assigned(Notify) then
        DoNotify(node.Key, value, caExtracted);
      if fOnValueChanged.CanInvoke then
        fOnValueChanged.Invoke(Self, Result.Value, caExtracted);
      if newCount = 0 then
      begin
        KeyChanged(node.Key, caExtracted);
        fTree.DeleteNode(Pointer(node));
      end;
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TSortedMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TSortedMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>; //FI:W521
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent,
    @TTreeMapInnerCollection.TEnumerator.MoveNext_MultiMap))^ do
  begin
    Parent := Self;
    fTree := Self.fTree;
    fOffset := SizeOf(TKey);
    fSourceVersion := @Self.fVersion;
    fVersion := Self.fVersion;
  end;
end;

function TSortedMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  node: PNode;
begin
  node := Pointer(fTree.FindNode(key));
  if Assigned(node) then
    Result := node.Values as IReadOnlyCollection<TValue>
  else
    Result := CreateWrappedCollection(key);
end;

function TSortedMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TSortedMultiMapBase<TKey, TValue>.GetNonEnumeratedCount: Integer;
begin
  Result := fCount;
end;

function TSortedMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TSortedMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  node: Pointer;
begin
  node := fTree.FindNode(key);
  if not Assigned(node) then Exit(Boolean(node));
  DoRemove(node, caRemoved, nil);
  Result := True;
end;

function TSortedMultiMapBase<TKey, TValue>.Remove(const key: TKey;
  const value: TValue): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  temp := fTree.FindNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  node := temp;
  Result := node.Values.Remove(value);
  if Result then
  begin
    Dec(fCount);
    ValueChanged(value, caRemoved);
    if Assigned(Notify) then
      DoNotify(key, value, caRemoved);
    if not node.Values.Any then
      DoRemove(node, caRemoved, nil);
    Result := True;
  end;
end;

function TSortedMultiMapBase<TKey, TValue>.TryAdd(const key: TKey;
  const value: TValue): Boolean;
var
  node: PNode;
begin
  node := Pointer(fTree.AddNode(key, True));
  node := Pointer(IntPtr(node) and not 1);
  if not Assigned(node.Values) then
  begin
    CreateCollection(node.Values);
    with fOnKeyChanged do if CanInvoke then
      Invoke(Self, node.Key, caAdded);
  end;
  Result := node.Values.Add(value);
  if Result then
  begin
    Inc(fCount);
    {$Q-}
    Inc(fVersion);
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    ValueChanged(value, caAdded);
    if Assigned(Notify) then
    begin
      DoNotify(node.Key, value, caAdded);
      Result := True;
    end;
  end;
end;

function TSortedMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  var values: IReadOnlyCollection<TValue>): Boolean;
var
  temp: Pointer;
  node: PNode;
begin
  temp := fTree.FindNode(key);
  if not Assigned(temp) then Exit(Boolean(temp));
  node := temp;
  values := node.Values as IReadOnlyCollection<TValue>;
  Result := True;
end;

procedure TSortedMultiMapBase<TKey, TValue>.UpdateValues(collection: TObject);
var
  node: PNode;
begin
  node := Pointer(fTree.FindNode(TCollectionWrapper(collection).fKey));
  if Assigned(node) then
    TCollectionWrapper(collection).fCollection := node.Values;
end;

{$ENDREGION}


{$REGION 'TSortedMultiMapBase<TKey, TValue>.TEnumerator'}

function TSortedMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result.Key := fNode.Key;
  {$IFDEF RSP31615}
  if IsManagedType(TValue) then
    IEnumeratorInternal(fEnumerator).GetCurrent(Result.Value)
  else
  {$ENDIF}
  Result.Value := fEnumerator.Current;
end;

{$ENDREGION}


{$REGION 'TListMultiMap<TKey, TValue>'}

function TListMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

procedure TListMultiMap<TKey, TValue>.CreateCollection(var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := _LookupVtableInfo(giComparer, valueType, SizeOf(TValue));
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateList_Object(Pointer(comparer), False, Result, valueType);
    tkInterface: TCollections.CreateList_Interface(Pointer(comparer), Result, valueType);
    tkUString: TCollections.CreateList_String(Pointer(comparer), Result, valueType);
    tkMethod: TCollections.CreateList_Method(Pointer(comparer), Result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateList_Int8(Pointer(comparer), Result, valueType);
        2: TCollections.CreateList_Int16(Pointer(comparer), Result, valueType);
        4: TCollections.CreateList_Int32(Pointer(comparer), Result, valueType);
        8: TCollections.CreateList_Int64(Pointer(comparer), Result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<TValue>.Create(IComparer<TValue>(comparer));
  end;
end;

{$ENDREGION}


{$REGION 'THashMultiMap<TKey, TValue>'}

constructor THashMultiMap<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure THashMultiMap<TKey, TValue>.CreateCollection(var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := Pointer(fValueComparer);
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateHashSet_Object(0, nil, result, valueType);
    tkInterface: TCollections.CreateHashSet_Interface(0, nil, result, valueType);
    tkUString: TCollections.CreateHashSet_String(0, nil, result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateHashSet_Int8(0, nil, result, valueType);
        2: TCollections.CreateHashSet_Int16(0, nil, result, valueType);
        4: TCollections.CreateHashSet_Int32(0, nil, result, valueType);
        8: TCollections.CreateHashSet_Int64(0, nil, result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    result := THashSet<TValue>.Create(0, nil);
  end;
end;

function THashMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TTreeMultiMap<TKey, TValue>'}

constructor TTreeMultiMap<TKey, TValue>.Create(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>; ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure TTreeMultiMap<TKey, TValue>.CreateCollection(var result: ICollection<TValue>);
begin
  result := TCollections.CreateSortedSet<TValue>(fValueComparer);
end;

function TTreeMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TSortedListMultiMap<TKey, TValue>'}

function TSortedListMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

procedure TSortedListMultiMap<TKey, TValue>.CreateCollection(
  var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := _LookupVtableInfo(giComparer, valueType, SizeOf(TValue));
  {$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateList_Object(Pointer(comparer), False, Result, valueType);
    tkInterface: TCollections.CreateList_Interface(Pointer(comparer), Result, valueType);
    tkUString: TCollections.CreateList_String(Pointer(comparer), Result, valueType);
    tkMethod: TCollections.CreateList_Method(Pointer(comparer), Result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateList_Int8(Pointer(comparer), Result, valueType);
        2: TCollections.CreateList_Int16(Pointer(comparer), Result, valueType);
        4: TCollections.CreateList_Int32(Pointer(comparer), Result, valueType);
        8: TCollections.CreateList_Int64(Pointer(comparer), Result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<TValue>.Create(IComparer<TValue>(comparer));
  end;
end;

{$ENDREGION}


{$REGION 'TSortedHashMultiMap<TKey, TValue>'}

constructor TSortedHashMultiMap<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fKeyComparer := keyComparer;
  fOwnerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure TSortedHashMultiMap<TKey, TValue>.CreateCollection(
  var result: ICollection<TValue>);
var
  valueType: PTypeInfo;
  comparer: Pointer;
begin
  valueType := GetValueType;
  comparer := Pointer(fValueComparer);
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(TValue) of
    tkClass: TCollections.CreateHashSet_Object(0, nil, result, valueType);
    tkInterface: TCollections.CreateHashSet_Interface(0, nil, result, valueType);
    tkUString: TCollections.CreateHashSet_String(0, nil, result, valueType);
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(TValue) of
        1: TCollections.CreateHashSet_Int8(0, nil, result, valueType);
        2: TCollections.CreateHashSet_Int16(0, nil, result, valueType);
        4: TCollections.CreateHashSet_Int32(0, nil, result, valueType);
        8: TCollections.CreateHashSet_Int64(0, nil, result, valueType);
      end;
  else{$ELSE}begin{$ENDIF}
    result := THashSet<TValue>.Create(0, nil);
  end;
end;

function TSortedHashMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TSortedTreeMultiMap<TKey, TValue>'}

constructor TSortedTreeMultiMap<TKey, TValue>.Create(
  const keyComparer: IComparer<TKey>; const valueComparer: IComparer<TValue>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TKey));

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(TypeInfo(TValue));

  fKeyComparer := keyComparer;
  fOwnerships := ownerships;
  fValueComparer := valueComparer;
end;

procedure TSortedTreeMultiMap<TKey, TValue>.CreateCollection(
  var result: ICollection<TValue>);
begin
  result := TCollections.CreateSortedSet<TValue>(fValueComparer);
end;

function TSortedTreeMultiMap<TKey, TValue>.AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;
begin
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TFoldedListMultiMap<TKey, TValue>'}

constructor TFoldedListMultiMap<TKey, TValue>.Create(keyType,
  valueType, elementType: PTypeInfo; const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships);
begin
  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TKey) <> tkClass then
  {$ELSE}
  if TType.Kind<TKey> <> tkClass then
  {$ENDIF}
    if doOwnsKeys in ownerships then
      RaiseHelper.NoClassType(keyType);

  {$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) <> tkClass then
  {$ELSE}
  if TType.Kind<TValue> <> tkClass then
  {$ENDIF}
    if doOwnsValues in ownerships then
      RaiseHelper.NoClassType(valueType);

  fHashTable.Comparer := keyComparer;
  fHashTable.Ownerships := ownerships;

  fElementType := elementType;
  fKeyType := keyType;
  fValueType := valueType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetElementType: PTypeInfo;
begin
  Result := fElementType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetKeyType: PTypeInfo;
begin
  Result := fKeyType;
end;

function TFoldedListMultiMap<TKey, TValue>.GetValueType: PTypeInfo;
begin
  Result := fValueType;
end;

{$ENDREGION}


end.
