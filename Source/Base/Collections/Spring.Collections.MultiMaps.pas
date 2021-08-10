{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2021 Spring4D Team                           }
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
  Spring.Events,
  Spring.Events.Base,
  Spring.HashTable;

{$IFDEF DELPHIXE6_UP}{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS(FieldVisibility)}{$ENDIF}

type
  TMultiMapItem<TKey, TValue> = packed record
  public
    HashCode: Integer;
    Key: TKey;
    Values: ICollection<TValue>;
  end;

  TValueCollection<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      Parent: TRefCountedObject;
      fHashTable: PHashTable;
      fIndex: Integer;
      fVersion: Integer;
      fEnumerator: IEnumerator<T>;
      fCurrent: T;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fSource: TRefCountedObject;
    fHashTable: PHashTable;
    fCount: PInteger;
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
  {$ENDREGION}
  public
    constructor Create(const source: TRefCountedObject;
      hashTable: PHashTable; count: PInteger);

  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  {$ENDREGION}

  {$REGION 'Implements IEnumerable<T>'}
    function GetEnumerator: IEnumerator<T>;
    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function ToArray: TArray<T>;
  {$ENDREGION}
  end;

  TCollectionWrapper<T> = class(TEnumerableBase<T>,
    IEnumerable<T>, IReadOnlyCollection<T>)
  private type
  {$REGION 'Nested Types'}
    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TCollectionWrapper<T>;
      fCollection: ICollection<T>;
      fEnumerator: IEnumerator<T>;
      procedure ValidateEnumerator;
      function GetCurrent: T;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;
  {$ENDREGION}
  private
    fCollection: ICollection<T>;
    fWrappers: TList;
    fUpdateValues: TNotifyEvent;
    function GetCount: Integer;
    function GetCountFast: Integer;
    procedure RefreshIfEmpty;
  public
    procedure BeforeDestruction; override;

    function Contains(const value: T;
      const comparer: IEqualityComparer<T>): Boolean; overload;
    function GetEnumerator: IEnumerator<T>;
    function ToArray: TArray<T>;
  end;

  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>)
  private type
  {$REGION 'Nested Types'}
    TKeyValuePair = TPair<TKey, TValue>;
    TItem = TMultiMapItem<TKey, TValue>;
    TItems = TArray<TItem>;
    PItem = ^TItem;

    PEnumerator = ^TEnumerator;
    TEnumerator = record
      Vtable: Pointer;
      RefCount: Integer;
      TypeInfo: PTypeInfo;
      fSource: TMultiMapBase<TKey, TValue>;
      fIndex: Integer;
      fVersion: Integer;
      fEnumerator: IEnumerator<TValue>;
      fCurrent: TKeyValuePair;
      function GetCurrent: TKeyValuePair;
      function MoveNext: Boolean;
      class var Enumerator_Vtable: TEnumeratorVtable;
    end;

    TKeyCollection = TInnerCollection<TKey>;
    TValueCollection = TValueCollection<TValue>;

    TCollectionWrapper = class(TCollectionWrapper<TValue>)
    private
      fKey: TKey;
    end;
  {$ENDREGION}
  private
    fHashTable: THashTable;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fCount: Integer;
    fWrappers: TList;
    function CreateWrappedCollection(const key: TKey): IReadOnlyCollection<TValue>;
    procedure DoValueChanged(sender: TObject; const item: TValue;
      action: TCollectionChangedAction);
    procedure UpdateValues(collection: TObject);
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetCountFast: Integer;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetValues: IReadOnlyCollection<TValue>;
  {$ENDREGION}
    procedure CreateCollection(var result: ICollection<TValue>); virtual; abstract;
    procedure DoRemove(const entry: THashTableEntry;
      action: TCollectionChangedAction; const extractTarget: ICollection<TValue>);
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); inline;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); inline;
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

procedure ClearWrappers(const wrappers: TList);
procedure HandleOnChanged(const collection: IInterface;
  const code: Pointer = nil; const data: Pointer = nil);

implementation

uses
  Types,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Collections.Sets,
  Spring.Comparers,
  Spring.ResourceStrings;

procedure ClearWrappers(const wrappers: TList);
var
  i: NativeInt;
begin
  for i := wrappers.Count - 1 downto 0 do
    with TCollectionWrapper<Pointer>(wrappers.List[i]) do
    begin
      fWrappers := nil;
      TMethod(fUpdateValues).Code := nil;
      TMethod(fUpdateValues).Data := nil;
    end;
  wrappers.Free;
end;

procedure HandleOnChanged(const collection: IInterface; const code, data: Pointer);
var
  onChanged: IEvent;
  handler: TMethod;
begin
  onChanged := ICollection<Integer>(collection).OnChanged;
  if Assigned(code) then
  begin
    onChanged.UseFreeNotification := False;
    handler.Code := code;
    handler.Data := data;
    onChanged.Add(handler);
  end
  else
    onChanged.Clear;
end;


{$REGION 'TValueCollection<T>'}

constructor TValueCollection<T>.Create(
  const source: TRefCountedObject; hashTable: PHashTable; count: PInteger);
begin
  fSource := source;
  fHashTable := hashTable;
  fCount := count;
end;

function TValueCollection<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize: Integer;
begin
  hashTable := fHashTable;
  item := hashTable.Items;
  itemCount := hashTable.ItemCount;
  itemSize := hashTable.ItemSize;
  while itemCount > 0 do
  begin
    if PInteger(item)^ >= 0 then
      if ICollection<T>(PPointer(item + itemSize - SizeOf(Pointer))^).Contains(value, comparer) then
        Exit(True);
    Inc(item, itemSize);
    Dec(itemCount);
  end;
  Result := False;
end;

function TValueCollection<T>.GetCount: Integer;
begin
  Result := fCount^;
end;

function TValueCollection<T>.GetCountFast: Integer;
begin
  Result := fCount^;
end;

function TValueCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    Parent := Self.fSource;
    fHashTable := Self.fHashTable;
    fVersion := fHashTable.Version;
  end;
end;

function TValueCollection<T>.ToArray: TArray<T>;
var
  hashTable: PHashTable;
  item: PByte;
  itemCount, itemSize, targetIndex, offset: Integer;
  collection: Pointer;
begin
  hashTable := fHashTable;
  SetLength(Result, fCount^);
  item := hashTable.Items;
  itemCount := hashTable.ItemCount;
  itemSize := hashTable.ItemSize;
  targetIndex := 0;
  while itemCount > 0 do
  begin
    if PInteger(item)^ >= 0 then
    begin
      collection := PPointer(item + itemSize - SizeOf(Pointer))^;
      ICollection<T>(collection).CopyTo(Result, targetIndex);
      Inc(targetIndex, ICollection<T>(collection).Count);
    end;
    Inc(item, itemSize);
    Dec(itemCount);
  end;
end;

function TValueCollection<T>._AddRef: Integer;
begin
  Result := fSource._AddRef;
end;

function TValueCollection<T>._Release: Integer;
begin
  Result := fSource._Release;
end;

{$ENDREGION}


{$REGION 'TValueCollection<T>.TEnumerator'}

function TValueCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TValueCollection<T>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PByte;
begin
  hashTable := fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if Assigned(fEnumerator) and fEnumerator.MoveNext then
      begin
        fCurrent := fEnumerator.Current;
        Exit(True);
      end;

      repeat
        if fIndex < hashTable.ItemCount then
        begin
          item := fHashTable.Items + NativeInt(fIndex) * fHashTable.ItemSize;
          Inc(fIndex);
          if PInteger(item)^ >= 0 then
          begin
            Inc(item, fHashTable.ItemSize - SizeOf(Pointer));
            fEnumerator := ICollection<T>(PPointer(item)^).GetEnumerator;
            Break;
          end;
        end
        else
        begin
          fEnumerator := nil;
          fCurrent := Default(T);
          Exit(False);
        end;
      until False;
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>'}

procedure TCollectionWrapper<T>.BeforeDestruction;
begin
  if Assigned(fWrappers) then
    fWrappers.Remove(Self);
  inherited;
end;

function TCollectionWrapper<T>.Contains(
  const value: T; const comparer: IEqualityComparer<T>): Boolean;
begin
  RefreshIfEmpty;
  Result := fCollection.Contains(value, comparer);
end;

procedure TCollectionWrapper<T>.RefreshIfEmpty;
begin
  if fCollection.IsEmpty and Assigned(fUpdateValues) then
    fUpdateValues(Self);
end;

function TCollectionWrapper<T>.GetCount: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.Count;
end;

function TCollectionWrapper<T>.GetCountFast: Integer;
begin
  RefreshIfEmpty;
  Result := fCollection.Count;
end;

function TCollectionWrapper<T>.GetEnumerator: IEnumerator<T>;
begin
  RefreshIfEmpty;
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
    fCollection := Self.fCollection;
    fEnumerator := fCollection.GetEnumerator;
  end;
end;

function TCollectionWrapper<T>.ToArray: TArray<T>;
begin
  RefreshIfEmpty;
  Result := fCollection.ToArray;
end;

{$ENDREGION}


{$REGION 'TCollectionWrapper<T>.TEnumerator'}

function TCollectionWrapper<T>.TEnumerator.GetCurrent: T;
begin
  ValidateEnumerator;
  Result := fEnumerator.Current;
end;

function TCollectionWrapper<T>.TEnumerator.MoveNext: Boolean;
begin
  ValidateEnumerator;
  Result := fEnumerator.MoveNext;
end;

procedure TCollectionWrapper<T>.TEnumerator.ValidateEnumerator;
begin
  fSource.RefreshIfEmpty;
  if fSource.fCollection <> fCollection then
    RaiseHelper.EnumFailedVersion;
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
  keyType: PTypeInfo;
begin
  inherited AfterConstruction;

  keyType := GetKeyType;
  fHashTable.ItemsInfo := TypeInfo(TItems);
  fHashTable.Initialize(@TComparerThunks<TKey>.Equals, @TComparerThunks<TKey>.GetHashCode, keyType);
  {$IFDEF DELPHIXE7_UP}
  if fHashTable.DefaultComparer then
    fHashTable.Find := @THashTable<TKey>.FindWithoutComparer
  else
  {$ENDIF}
    fHashTable.Find := @THashTable<TKey>.FindWithComparer;

  fKeys := TKeyCollection.Create(Self, @fHashTable, IEqualityComparer<TKey>(fHashTable.Comparer), keyType, 0);
  fValues := TValueCollection.Create(Self, @fHashTable, @fCount);
  fWrappers := TList.Create;
end;

procedure TMultiMapBase<TKey, TValue>.BeforeDestruction;
begin
  ClearWrappers(fWrappers);
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
  collection.fWrappers := fWrappers;
  collection.fWrappers.Add(collection);
  collection.fUpdateValues := UpdateValues;
  CreateCollection(collection.fCollection);

  Result := collection;
end;

procedure TMultiMapBase<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  if fOnKeyChanged.CanInvoke then
    fOnKeyChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsKeys in fHashTable.Ownerships) then
    FreeObject(item);
end;

procedure TMultiMapBase<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
  if (action = caRemoved) and (doOwnsValues in fHashTable.Ownerships) then
    FreeObject(item);
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
    Add(key, values[i]);
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
    item := enumerator.Current;
    Add(key, item);
  end;
end;

procedure TMultiMapBase<TKey, TValue>.Clear;
var
  oldItemCount, i: Integer;
  oldItems: TArray<TItem>;
  oldValue: TValue;
begin
  oldItemCount := fHashTable.ItemCount;
  oldItems := TItems(fHashTable.Items);
  fHashTable.Clear;

  for i := 0 to oldItemCount - 1 do
    if oldItems[i].HashCode >= 0 then
    begin
      HandleOnChanged(oldItems[i].Values);
      for oldValue in oldItems[i].Values do
      begin
        if Assigned(Notify) then
          DoNotify(oldItems[i].Key, oldValue, caRemoved);
        ValueChanged(oldValue, caRemoved);
      end;
      oldItems[i].Values.Clear;
      oldItems[i].Values := nil;
      KeyChanged(oldItems[i].Key, caRemoved);
    end;
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
  item := THashTable(fHashTable).DeleteEntry(entry);
  HandleOnChanged(item.Values);
  for value in item.Values do
  begin
    Dec(fCount);
    if Assigned(Notify) then
      DoNotify(item.Key, value, action);
    ValueChanged(value, action);
  end;
  if action = caRemoved then
    item.Values.Clear
  else
    item.Values.MoveTo(extractTarget);
  KeyChanged(item.Key, action);
  item.HashCode := RemovedFlag;
  item.Key := Default(TKey);
  item.Values := nil;
end;

procedure TMultiMapBase<TKey, TValue>.DoValueChanged(sender: TObject;
  const item: TValue; action: TCollectionChangedAction);
begin
  case action of //FI:W535
    caAdded: Inc(fCount);
    caRemoved, caExtracted: Dec(fCount);
  end;
  if fOnValueChanged.CanInvoke then
    fOnValueChanged.Invoke(Self, item, action);
{$IFDEF DELPHIXE7_UP}
  if GetTypeKind(TValue) = tkClass then
{$ENDIF}
  if (action = caRemoved) and (doOwnsValues in fHashTable.Ownerships) then
    FreeObject(item);
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey;
  const value: TValue): TKeyValuePair;
var
  item: PItem;
  count: Integer;
begin
  Result.Key := key;
  item := IHashTable<TKey>(@fHashTable).Find(key);
  if Assigned(item) then
  begin
    count := item.Values.Count;
    Result.Value := item.Values.Extract(value);
    if item.Values.Count < count then
    begin
      if Assigned(Notify) then
        DoNotify(key, value, caExtracted);
      KeyChanged(item.Key, caExtracted);
    end;
  end
  else
    Result.Value := Default(TValue);
end;

function TMultiMapBase<TKey, TValue>.Extract(const key: TKey): ICollection<TValue>;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  CreateCollection(Result);
  if THashTable(fHashTable).FindEntry(key, entry) then
    DoRemove(entry, caExtracted, Result);
end;

function TMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetCountFast: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TKeyValuePair>;
begin
  _AddRef;
  with PEnumerator(TEnumeratorBlock.Create(@Result, @TEnumerator.Enumerator_Vtable,
    TypeInfo(TEnumerator), @TEnumerator.GetCurrent, @TEnumerator.MoveNext))^ do
  begin
    fSource := Self;
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
  if THashTable(fHashTable).FindEntry(key, entry) then
  begin
    item := @TItems(fHashTable.Items)[entry.ItemIndex];
    if item.Values.Remove(value) then
    begin
      if Assigned(Notify) then
        DoNotify(key, value, caRemoved);
      if not item.Values.Any then
        DoRemove(entry, caRemoved, nil);
      Exit(True);
    end;
  end;
  Result := False;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  entry: THashTableEntry;
begin
  entry.HashCode := IEqualityComparer<TKey>(fHashTable.Comparer).GetHashCode(key);
  if THashTable(fHashTable).FindEntry(key, entry) then
  begin
    DoRemove(entry, caRemoved, nil);
    Result := True;
  end
  else
    Result := False;
end;

function TMultiMapBase<TKey, TValue>.TryAdd(const key: TKey; const value: TValue): Boolean;
var
  item: PItem;
begin
  item := IHashTable<TKey>(@fHashTable).Find(key, OverwriteExisting or InsertNonExisting);
  if item.HashCode >= 0 then
  begin
    item.Key := key;
    CreateCollection(item.Values);
    HandleOnChanged(item.Values, @TMultiMapBase<TKey, TValue>.DoValueChanged, Self);
    KeyChanged(key, caAdded);
  end
  else
    item.HashCode := item.HashCode and not RemovedFlag;

  Result := item.Values.Add(value);
  if Result then
    if Assigned(Notify) then
    begin
      DoNotify(key, value, caAdded);
      Result := True;
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
  if not Assigned(temp) then Exit(Boolean(Pointer(temp)));
  item := temp;
  values := item.Values as IReadOnlyCollection<TValue>;
  Result := True;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TKeyValuePair;
begin
  Result := fCurrent;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
var
  hashTable: PHashTable;
  item: PItem;
begin
  hashTable := @fSource.fHashTable;
  if fVersion = hashTable.Version then
  begin
    repeat
      if Assigned(fEnumerator) and fEnumerator.MoveNext then
      begin
        fCurrent.Value := fEnumerator.Current;
        Exit(True);
      end;

      repeat
        if fIndex < hashTable.ItemCount then
        begin
          item := @TItems(hashTable.Items)[fIndex];
          Inc(fIndex);
          if item.HashCode >= 0 then
          begin
            fCurrent.Key := item.Key;
            fEnumerator := item.Values.GetEnumerator;
            Break;
          end;
        end
        else
        begin
          fEnumerator := nil;
          fCurrent := Default(TKeyValuePair);
          Exit(False);
        end;
      until False;
    until False;
  end
  else
    Result := RaiseHelper.EnumFailedVersion;
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
