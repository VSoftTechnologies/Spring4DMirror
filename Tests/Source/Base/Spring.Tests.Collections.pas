unit Spring.Tests.Collections;

interface

uses
  TestFramework,
  SysUtils,
  Spring.Collections;

type
  TTestCode = reference to procedure;
  TClassOfException = class of Exception;

type
  TTestEmptyHashSet = class(TTestCase)
  private
    fSet: ISet<Integer>;
    fEmpty: ISet<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmpty;
    procedure TestAddDuplications;
    procedure TestExceptWith;
    procedure TestIntesectWith;
    procedure TestUnionWith;
    procedure TestSetEquals;
  end;

  TTestNormalHashSet = class(TTestCase)
  private
    fSet1: ISet<Integer>;
    fSet2: ISet<Integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckSet(const collection: ISet<Integer>; const values: array of Integer);
  published
    procedure TestExceptWith;
    procedure TestIntesectWith;
    procedure TestUnionWith;
    procedure TestSetEquals;
  end;

  TExceptionCheckerTestCase = class(TTestCase)
  protected
    procedure CheckException(aExceptionType: TClassOfException; aCode: TTestCode; const aMessage: String);
    procedure CheckExceptionNotRaised(aExceptionType: TClassOfException; aCode: TTestCode; const aMessage: String);

  end;

  TTestIntegerList = class(TExceptionCheckerTestCase)
  private
    SUT: IList<integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestListIsInitializedEmpty;
    procedure TestListCountWithAdd;
    procedure TestListCountWithInsert;
    procedure TestListInsertBetween;
    procedure TestListInsertBeginning;
    procedure TestListSimpleDelete;
    procedure TestListMultipleDelete;
    procedure TestListSimpleExchange;
    procedure TesListtReverse;
    procedure TestListSort;
    procedure TestListIndexOf;
    procedure TestLastIndexOf;
    procedure TestListMove;
    procedure TestListClear;
    procedure TestListLargeDelete;
    procedure TestListRemove;
  end;

  TTestEmptyStringIntegerDictionary = class(TExceptionCheckerTestCase)
  private
    SUT: IDictionary<string, integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDictionaryIsInitializedEmpty;
    procedure TestDictionaryKeysAreEmpty;
    procedure TestDictionaryValuesAreEmpty;
    procedure TestDictionaryContainsReturnsFalse;
  end;

  TTestStringIntegerDictionary = class(TExceptionCheckerTestCase)
  private
    SUT: IDictionary<string, integer>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDictionaryCountWithAdd;
    procedure TestDictionarySimpleValues;
    procedure TestDictionaryKeys;
    procedure TestDictionaryValues;
    procedure TestDictionaryContainsValue;
    procedure TestDictionaryContainsKey;
  end;

  TTestEmptyStackofStrings = class(TExceptionCheckerTestCase)
  private
    SUT: IStack<string>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStackInitializesEmpty;
    procedure TestEmptyPopPeek;
  end;



implementation

uses
        Classes
      ;

{ TTestEmptyHashSet }

procedure TTestEmptyHashSet.SetUp;
begin
  inherited;
  fSet := TCollections.CreateSet<Integer>;
  fEmpty := TCollections.CreateSet<Integer>;
end;

procedure TTestEmptyHashSet.TearDown;
begin
  inherited;
  fSet := nil;
end;

procedure TTestEmptyHashSet.TestEmpty;
begin
  CheckEquals(0, fSet.Count);
  CheckTrue(fSet.IsEmpty);
end;

procedure TTestEmptyHashSet.TestExceptWith;
begin
  fSet.ExceptWith(fEmpty);
  CheckEquals(0, fSet.Count);
end;

procedure TTestEmptyHashSet.TestIntesectWith;
begin
  fSet.IntersectWith(fEmpty);
  CheckEquals(0, fSet.Count);
end;

procedure TTestEmptyHashSet.TestUnionWith;
begin
  fSet.UnionWith(fEmpty);
  CheckEquals(0, fSet.Count);
end;

procedure TTestEmptyHashSet.TestAddDuplications;
begin
  fSet.Add(2);
  CheckEquals(1, fSet.Count);

  fSet.Add(2);
  CheckEquals(1, fSet.Count);
end;

procedure TTestEmptyHashSet.TestSetEquals;
begin
  CheckTrue(fSet.SetEquals(fEmpty));
end;

{ TTestNormalHashSet }

procedure TTestNormalHashSet.CheckSet(const collection: ISet<Integer>; const values: array of Integer);
var
  value: Integer;
begin
  CheckEquals(Length(values), collection.Count);
  for value in values do
  begin
    CheckTrue(collection.Contains(value));
  end;
end;

procedure TTestNormalHashSet.SetUp;
begin
  inherited;
  fSet1 := TCollections.CreateSet<Integer>;
  fSet2 := TCollections.CreateSet<Integer>;
  fSet1.AddRange([1, 2, 3]);
  fSet2.AddRange([3, 1, 4, 5]);
end;

procedure TTestNormalHashSet.TearDown;
begin
  inherited;
  fSet1 := nil;
  fSet2 := nil;
end;

procedure TTestNormalHashSet.TestExceptWith;
begin
  fSet1.ExceptWith(fSet2);
  CheckSet(fSet1, [2]);
end;

procedure TTestNormalHashSet.TestIntesectWith;
begin
  fSet1.IntersectWith(fSet2);
  CheckSet(fSet1, [1, 3]);
end;

procedure TTestNormalHashSet.TestUnionWith;
begin
  fSet1.UnionWith(fSet2);
  CheckSet(fSet1, [1,2,3,4,5]);
end;

procedure TTestNormalHashSet.TestSetEquals;
begin
  CheckFalse(fSet1.SetEquals(fSet2));
  CheckTrue(fSet1.SetEquals(fSet1));
  CheckTrue(fSet2.SetEquals(fSet2));
end;

{ TTestIntegerList }

procedure TTestIntegerList.SetUp;
begin
  inherited;
  SUT := TCollections.CreateList<integer>;
end;

procedure TTestIntegerList.TearDown;
begin
  inherited;
  SUT := nil;
end;

const
  ListCountLimit = 1000;

procedure TTestIntegerList.TestLastIndexOf;
begin
  SUT.Add(1);
  SUT.Add(1);
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3);

  CheckEquals(2, SUT.LastIndexOf(1));

end;

procedure TTestIntegerList.TestListClear;
var
  i: Integer;
begin
  for i := 0 to ListCountLimit do
  begin
    SUT.Add(i);
  end;

  SUT.Clear;

  CheckEquals(0, SUT.Count, 'List not empty after call to Clear');


end;

procedure TTestIntegerList.TestListCountWithAdd;
var
  i: Integer;
begin
  for i := 1 to ListCountLimit do
  begin
    SUT.Add(i);
    CheckEquals(i, SUT.Count);
  end;
end;


procedure TTestIntegerList.TestListCountWithInsert;
var
  i: Integer;
begin
  for i := 1 to ListCountLimit do
  begin
    SUT.Insert(0, i);
    CheckEquals(i, SUT.Count);
  end;
end;

procedure TTestIntegerList.TestListSimpleDelete;
begin
  SUT.Add(1);
  CheckEquals(1, SUT.Count);
  SUT.Delete(0);
  CheckEquals(0, SUT.Count);
end;

procedure TTestIntegerList.TesListtReverse;
var
  i: Integer;
begin
 for i := 0 to ListCountLimit do
 begin
   SUT.Add(i);
 end;
 CheckEquals(ListCountLimit + 1, SUT.Count, 'TestReverse: List count incorrect after initial adds');

 SUT.Reverse;

 for i := ListCountLimit downto 0 do
 begin
   CheckEquals(i, SUT[ListCountLimit - i]);
 end;
end;

procedure TTestIntegerList.TestListSimpleExchange;
begin
  SUT.Add(0);
  SUT.Add(1);
  CheckEquals(2, SUT.Count);
  SUT.Exchange(0, 1);
  CheckEquals(2, SUT.Count, 'Count wrong after exchange');
  CheckEquals(1, SUT[0]);
  CheckEquals(0, SUT[1]);
end;

procedure TTestIntegerList.TestListSort;
var
  i: Integer;
begin
  SUT.Add(6);
  SUT.Add(0);
  SUT.Add(2);
  SUT.Add(5);
  SUT.Add(7);
  SUT.Add(1);
  SUT.Add(8);
  SUT.Add(3);
  SUT.Add(4);
  SUT.Add(9);
  CheckEquals(10, SUT.Count, 'Test');
  SUT.Sort;
  for i := 0 to 9 do
  begin
    CheckEquals(i, SUT[i], Format('%s: Items not properly sorted at Index %d', ['TestlistSort', i]));
  end;
end;

procedure TTestIntegerList.TestListIndexOf;
var
  i: Integer;
begin
  for i := 0 to ListCountLimit - 1 do
  begin
    SUT.Add(i);
  end;
  CheckEquals(ListCountLimit, SUT.Count, 'TestLimitIndexOf: List count not correct after adding items.');

  for i := 0 to ListCountLimit - 1 do
  begin
    CheckEquals(i, SUT.IndexOf(i));
  end;

  CheckEquals(-1, SUT.IndexOf(ListCountLimit + 100), 'Index of item not in list was not -1');

end;

procedure TTestIntegerList.TestListInsertBeginning;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.Insert(0, 42);
  CheckEquals(3, SUT.Count);
  CheckEquals(42, SUT[0]);
  CheckEquals(0, SUT[1]);
  CheckEquals(1, SUT[2]);
end;

procedure TTestIntegerList.TestListInsertBetween;
begin
  SUT.Add(0);
  SUT.Add(1);
  SUT.Insert(1, 42);
  CheckEquals(3, SUT.Count);
  CheckEquals(0, SUT[0]);
  CheckEquals(42, SUT[1]);
  CheckEquals(1, SUT[2]);
end;


procedure TTestIntegerList.TestListIsInitializedEmpty;
begin
  CheckEquals(SUT.Count, 0);
end;

procedure TTestIntegerList.TestListLargeDelete;
var
  i: Integer;
begin
  for i := 0 to ListCountLimit do
  begin
    SUT.Add(i);
  end;

  for i := 0 to ListCountLimit do
  begin
    SUT.Delete(0);
  end;

  CheckEquals(0, SUT.Count, 'Not all items properly deleted from large delete');
end;

procedure TTestIntegerList.TestListMove;
begin
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3);
  CheckEquals(3, SUT.Count);

  SUT.Move(0, 2);
  CheckEquals(3, SUT.Count, 'List count is wrong after call to Move');

  CheckEquals(2, SUT[0]);
  CheckEquals(3, SUT[1]);
  CheckEquals(1, SUT[2]);
end;

procedure TTestIntegerList.TestListMultipleDelete;
begin
  SUT.Add(1);
  SUT.Add(2);
  SUT.Add(3);
  CheckEquals(3, SUT.Count);
  SUT.Delete(0);
  CheckEquals(2, SUT.Count);
  SUT.Delete(0);
  CheckEquals(1, SUT.Count);
  SUT.Delete(0);
  CheckEquals(0, SUT.Count);

end;

procedure TTestIntegerList.TestListRemove;
begin

end;
//
{ TTestStringIntegerDictionary }

procedure TTestStringIntegerDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDictionary<string, integer>;
  SUT.Add('one', 1);
  SUT.Add('two', 2);
  SUT.Add('three', 3);
end;

procedure TTestStringIntegerDictionary.TearDown;
begin
  inherited;
end;

procedure TTestStringIntegerDictionary.TestDictionaryContainsKey;
begin

  CheckTrue(SUT.ContainsKey('one'), '"one" not found by ContainsKey');
  CheckTrue(SUT.ContainsKey('two'), '"two" not found by ContainsKey');
  CheckTrue(SUT.ContainsKey('three'), '"three" not found by ContainsKey');

end;

procedure TTestStringIntegerDictionary.TestDictionaryContainsValue;
begin

  CheckTrue(SUT.ContainsValue(1), '1 not found by ContainsValue');
  CheckTrue(SUT.ContainsValue(2), '2 not found by ContainsValue');
  CheckTrue(SUT.ContainsValue(3), '3 not found by ContainsValue');
end;

procedure TTestStringIntegerDictionary.TestDictionaryCountWithAdd;
begin
  CheckEquals(3, SUT.Count, 'TestDictionaryCountWithAdd: Count is not correct');

end;

procedure TTestStringIntegerDictionary.TestDictionaryKeys;
var
  Result: ICollection<string>;
begin

  Result := SUT.Keys;
  CheckEquals(3, Result.Count, 'TestDictionaryKeys: Keys call returns wrong count');

  CheckTrue(Result.Contains('one'), 'TestDictionaryKeys: Keys doesn''t contain "one"');
  CheckTrue(Result.Contains('two'), 'TestDictionaryKeys: Keys doesn''t contain "two"');
  CheckTrue(Result.Contains('three'), 'TestDictionaryKeys: Keys doesn''t contain "three"');

end;

procedure TTestStringIntegerDictionary.TestDictionarySimpleValues;
begin
  CheckEquals(3, SUT.Count, 'TestDictionarySimpleValues: Count is not correct');

  CheckEquals(1, SUT['one']);
  CheckEquals(2, SUT['two']);
  CheckEquals(3, SUT['three']);
end;

procedure TTestStringIntegerDictionary.TestDictionaryValues;
var
  Result: ICollection<integer>;
begin

  Result := SUT.Values;
  CheckEquals(3, Result.Count, 'TestDictionaryKeys: Values call returns wrong count');

  CheckTrue(Result.Contains(1), 'TestDictionaryKeys: Values doesn''t contain "one"');
  CheckTrue(Result.Contains(2), 'TestDictionaryKeys: Values doesn''t contain "two"');
  CheckTrue(Result.Contains(3), 'TestDictionaryKeys: Values doesn''t contain "three"');

end;

{ TTestEmptyStringIntegerDictionary }

procedure TTestEmptyStringIntegerDictionary.SetUp;
begin
  inherited;
  SUT := TCollections.CreateDictionary<string, integer>;
end;

procedure TTestEmptyStringIntegerDictionary.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryContainsReturnsFalse;
begin
   CheckFalse(SUT.ContainsKey('blah'));
   CheckFalse(SUT.ContainsValue(42));
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryIsInitializedEmpty;
begin
  CheckEquals(0, SUT.Count);
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryKeysAreEmpty;
var
  Result: ICollection<string>;
begin
  Result := SUT.Keys;
  CheckEquals(0, Result.Count);
end;

procedure TTestEmptyStringIntegerDictionary.TestDictionaryValuesAreEmpty;
var
  Result: ICollection<integer>;
begin
  Result := SUT.Values;
  CheckEquals(0, Result.Count);
end;

{ TTestEmptyStackofStrings }

procedure TTestEmptyStackofStrings.SetUp;
begin
  inherited;
  SUT := TCollections.CreateStack<string>;
end;

procedure TTestEmptyStackofStrings.TearDown;
begin
  inherited;
  SUT := nil;
end;

procedure TTestEmptyStackofStrings.TestEmptyPopPeek;
begin
  CheckException(EListError, procedure() begin SUT.Pop end, 'EListError not raised');
  CheckException(EListError, procedure() begin SUT.Peek end, 'EListError not raised');
end;

procedure TTestEmptyStackofStrings.TestStackInitializesEmpty;
begin
  CheckEquals(0, SUT.Count);
end;

{ TExceptionCheckerTestCase }

procedure TExceptionCheckerTestCase.CheckException(aExceptionType: TClassOfException; aCode: TTestCode; const aMessage: String);
var
  WasException: Boolean;
begin
  WasException := False;
  try
    aCode;
  except
    on E: Exception do
    begin
      if E is aExceptionType then
      begin
        WasException := True;
      end;
    end;
  end;
  Check(WasException, aMessage);
end;

procedure TExceptionCheckerTestCase.CheckExceptionNotRaised(aExceptionType: TClassOfException; aCode: TTestCode; const aMessage: String);
var
  WasException: Boolean;
begin
  WasException := False;
  try
    aCode;
  except
    on E: Exception do
    begin
      if E is aExceptionType then
      begin
        WasException := True;
      end;
    end;
  end;
  Check(WasException, aMessage);
end;

end.
