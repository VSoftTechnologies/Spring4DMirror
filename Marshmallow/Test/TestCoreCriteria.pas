unit TestCoreCriteria;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

{$I sv.inc}

uses
  TestFramework, Spring.Collections, Spring.Persistence.Criteria, Generics.Collections
  , Spring.Persistence.Core.Interfaces, Spring.Persistence.Criteria.Criterion.Abstract
  , Spring.Persistence.Criteria.Abstract, uModels, Spring.Persistence.Criteria.Restrictions
  ,Spring.Persistence.Core.Session
  ;

type
  // Test methods for class TCriteria

  TestTCriteria = class(TTestCase)
  private
    FCriteria: ICriteria<TCustomer>;
    FSession: TSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Add_Eq;
    procedure AddOrder;
    procedure List_Eq_IsNull;
    procedure List_Like;
    procedure List_Ge_Gt;
    procedure List_LEq_Lt;
    procedure List_In_NotIn;
    procedure List_Property_Eq;
    procedure Page_GEq_OrderDesc;
    procedure Page_SubEntities;
    procedure List_Or_And;
    procedure List_Or_Or;
    procedure List_And_And;
    procedure List_Not_Eq;
    procedure List_NeProperty;
    procedure List_Between;
    procedure Add_SubEntity_Criterion;
    procedure Disjunction;
    procedure Conjunction;
  end;

implementation

uses
  Spring.Persistence.Core.ConnectionFactory
  ,Spring.Persistence.Criteria.OrderBy
  ,Spring.Persistence.Criteria.Properties
  ,TestSession
  ,Spring.Persistence.SQL.Types
  ,Spring.Persistence.SQL.Params
  ,TestConsts
  ,SysUtils
  ,Variants
  ,Rtti
  ;


procedure TestTCriteria.SetUp;
begin
  FSession := TSession.Create(TConnectionFactory.GetInstance(dtSQLite, TestDB));
  FCriteria := FSession.CreateCriteria<TCustomer>;


  FSession.Connection.AddExecutionListener(
    procedure(const ACommand: string; const AParams: IList<TDBParam>)
    var
      i: Integer;
    begin
      Status(ACommand);
      for i := 0 to AParams.Count - 1 do
      begin
        Status(Format('%2:D Param %0:S = %1:S', [AParams[i].Name, VarToStrDef(AParams[i].Value, 'NULL'), i]));
      end;
      Status('-----');
    end);
end;

procedure TestTCriteria.TearDown;
begin
  ClearTable(TBL_PEOPLE);
  ClearTable(TBL_ORDERS);
  FCriteria := nil;
  FSession.Free;
end;

procedure TestTCriteria.Add_Eq;
begin
  FCriteria.Add(TRestrictions.Eq('Name', 'Foo'))
    .Add(TRestrictions.Eq('Age', 42));
  CheckEquals(2, FCriteria.Count);
end;


procedure TestTCriteria.Add_SubEntity_Criterion;
var
  LOrders: IList<TCustomer_Orders>;
  Age: IProperty;
  i, LCustId: Integer;
  LCriteria: ICriteria<TCustomer_Orders>;
begin
  LCriteria := FSession.CreateCriteria<TCustomer_Orders>;

  Age := TProperty<TCustomer>.ForName(CUSTAGE);

  for i := 1 to 10 do
  begin
    LCustId := InsertCustomer(i, 'Foo', Abs(i/2));
    InsertCustomerOrder(LCustId, i + 10, -1, i + 100);
  end;

  LOrders := LCriteria.Add(Age.Eq(1)).ToList;
  CheckEquals(1, LOrders.Count);
  CheckEquals(1, LOrders.First.Customer.Age);

  LCriteria.Clear;

  LOrders := LCriteria.Add(Age.GEq(1)).ToList;
  CheckEquals(10, LOrders.Count);
  CheckEquals(1, LOrders.First.Customer.Age);

  LCriteria.Clear;
  LOrders := LCriteria.Add(Age.InInt(TArray<Integer>.Create(1,2,3)))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(3, LOrders.Count);
  CheckEquals(3, LOrders.First.Customer.Age);
end;

procedure TestTCriteria.Conjunction;
var
  LCustomers: IList<TCustomer>;
  Age, Name: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  Name := TProperty.ForName(CUSTNAME);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(
    TRestrictions
      .Conjunction
        .Add(Age.Eq(42))
        .Add(Name.Eq('Foo')))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals('Foo', LCustomers[0].Name);
end;

procedure TestTCriteria.Disjunction;
var
  LCustomers: IList<TCustomer>;
  Age, Name: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  Name := TProperty.ForName(CUSTNAME);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(
    TRestrictions
      .Disjunction
        .Add(Age.Eq(42))
        .Add(Name.Eq('Foo'))
        .Add(Age.Eq(50)))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TestTCriteria.AddOrder;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'foo');
  InsertCustomer(110, 'foo');
  FCriteria.Add(TRestrictions.Eq(CUSTNAME, 'foo'))
    .OrderBy(TOrderBy.Desc(CUSTAGE));
  LCustomers := FCriteria.ToList;
  CheckEquals(110, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TestTCriteria.List_And_And;
var
  LCustomers: IList<TCustomer>;
  Age, Name: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  Name := TProperty.ForName(CUSTNAME);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(TRestrictions.And(Age.Eq(42), Name.Eq('Foo')))
    .Add(Age.GEq(10))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
end;

procedure TestTCriteria.List_Between;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(TRestrictions.Between(CUSTAGE, 42, 50))
    .ToList;

  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Age.Between(43, 50))
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
end;

procedure TestTCriteria.List_NeProperty;
var
  LCustomers: IList<TCustomer>;
  Age, ID: IProperty;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  Age := TProperty<TCustomer>.ForName(CUSTAGE);
  ID := TProperty<TCustomer>.ForName(CUSTID);

  LCustomers := FCriteria.Add(TRestrictions.NeProperty(CUSTAGE, CUSTID))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Age.NeProperty(ID))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  Age := TProperty.ForName(CUSTAGE);
  ID := TProperty.ForName(CUSTID);
  LCustomers := FCriteria.Add(Age.NeProperty(ID)).Add(Age.NeProperty(CUSTNAME))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);
end;

procedure TestTCriteria.List_Eq_IsNull;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := FCriteria.Add(TRestrictions.Eq(CUSTNAME, 'Foo'))
    .Add(TRestrictions.Eq(CUSTAGE, 42)).Add(TRestrictions.IsNull(CUSTAVATAR)).ToList;
  CheckTrue(Assigned(LCustomers));
  CheckEquals(0, LCustomers.Count);
  InsertCustomer(42, 'Foo');
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals('Foo', LCustomers[0].Name);
  CheckEquals(0, LCustomers[0].Orders.Count);
  InsertCustomerOrder(LCustomers[0].ID, 1, 100, 100.59);
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(1, LCustomers[0].Orders.Count);
  CheckEquals(100, LCustomers[0].Orders[0].Order_Status_Code);
end;

procedure TestTCriteria.List_Ge_Gt;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(TRestrictions.GEq(CUSTAGE, 42))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(TRestrictions.Gt(CUSTAGE, 42))
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
end;

procedure TestTCriteria.List_In_NotIn;
var
  LCustomers: IList<TCustomer>;
  LAges: TArray<Integer>;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');
  InsertCustomer(68, 'FooBar');
  InsertCustomer(10, 'Fbar');

  LAges := TArray<Integer>.Create(10, 50);
  LCustomers := FCriteria.Add(TRestrictions.In<Integer>(CUSTAGE, LAges))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(10, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(TRestrictions.NotIn<Integer>(CUSTAGE, LAges))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(68, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(TRestrictions.In<string>(CUSTNAME, TArray<string>.Create('Bar', 'Fbar')))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals('Bar', LCustomers[0].Name);
  CheckEquals('Fbar', LCustomers[1].Name);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(TRestrictions.NotIn<string>(CUSTNAME, TArray<string>.Create('Bar', 'Fbar')))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals('Foo', LCustomers[0].Name);
  CheckEquals('FooBar', LCustomers[1].Name);
end;

procedure TestTCriteria.List_LEq_Lt;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(TRestrictions.LEq(CUSTAGE, 50))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(TRestrictions.Lt(CUSTAGE, 50))
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
end;

procedure TestTCriteria.List_Like;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'FooBar');
  LCustomers := FCriteria.Add(TRestrictions.Like(CUSTNAME, 'Foo'))
    .ToList;
  CheckEquals(0, LCustomers.Count);
  FCriteria.Clear;
  FCriteria.Add(TRestrictions.Like(CUSTNAME, 'Foo', mmAnywhere));
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);

  FCriteria.Clear;
  FCriteria.Add(TRestrictions.Like(CUSTNAME, 'Foo', mmStart));
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);

  FCriteria.Clear;
  FCriteria.Add(TRestrictions.Like(CUSTNAME, 'Bar', mmEnd));
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);
end;

procedure TestTCriteria.List_Not_Eq;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(TRestrictions.Not(Age.Eq(42)))
    .Add(Age.GEq(10))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
end;

procedure TestTCriteria.List_Or_And;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(TRestrictions.Or(Age.Eq(42), age.Eq(50)))
    .Add(Age.GEq(10))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TestTCriteria.List_Or_Or;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');
  //WHERE ((A.CUSTAGE =:CUSTAGE1 OR A.CUSTAGE = :CUSTAGE2) OR A.CUSTAGE >=:CUSTAGE3)
  //ORDER BY A.CUSTAGE DESC
  LCustomers := FCriteria.Add(TRestrictions.Or(TRestrictions.Or(Age.Eq(42), Age.Eq(50)), Age.GEq(10)))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TestTCriteria.List_Property_Eq;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.ForName(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Age.Eq(42))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
end;

procedure TestTCriteria.Page_GEq_OrderDesc;
var
  LPage: IDBPage<TCustomer>;
  Age: IProperty;
  i: Integer;
begin
  Age := TProperty.ForName(CUSTAGE);
  //add 10 customers
  for i := 1 to 10 do
  begin
    InsertCustomer(i, 'Foo', Abs(i/2));
  end;

  LPage := FCriteria.Add(Age.GEq(5))
    .OrderBy(Age.Desc).Page(0, 3);

  CheckEquals(6, LPage.GetTotalItems);
  CheckEquals(2, LPage.GetTotalPages);
  CheckEquals(10, LPage.Items[0].Age);
  CheckEquals(8, LPage.Items[2].Age);
end;

procedure TestTCriteria.Page_SubEntities;
var
  LPage: IDBPage<TCustomer>;
  Age: IProperty;
  i: Integer;
  LCustId: Integer;
begin
  Age := TProperty.ForName(CUSTAGE);
  //add 10 customers
  for i := 1 to 10 do
  begin
    LCustId := InsertCustomer(i, 'Foo', Abs(i/2));
    InsertCustomerOrder(LCustId, i + 10, -1, i + 100);
  end;

  CheckEquals(10, FSession.FindAll<TCustomer_Orders>.Count);

  LPage := FCriteria.Add(Age.GEq(5))
    .OrderBy(Age.Desc).Page(0, 3);

  CheckEquals(6, LPage.GetTotalItems);
  CheckEquals(2, LPage.GetTotalPages);
  CheckEquals(10, LPage.Items[0].Age);
  CheckEquals(8, LPage.Items[2].Age);

  CheckEquals(10, LPage.Items[0].Age);
  CheckEquals(20, LPage.Items[0].OrdersIntf.First.Customer_Payment_Method_Id);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCriteria.Suite);
end.

