unit Customer_Orders_Model;

interface

uses
  Mapping.Attributes, Core.Types, Graphics;

type
  [Entity]   
  [Table('Customer_Orders', '')]
  TCustomer_Orders = class
  private   
    FOrder_Status_Code: Nullable<Integer>; 
    FDate_Order_Placed: Nullable<TDateTime>; 
    FTotal_Order_Price: Nullable<Double>; 
    FORDER_ID: Integer; 
    FCustomer_ID: Integer; 
    FCustomer_Payment_Method_Id: Nullable<Integer>; 
  public
    [Column('Order_Status_Code', [], -1, -1, -1, '')]
    property Order_Status_Code: Nullable<Integer> read FOrder_Status_Code write FOrder_Status_Code; 
    [Column('Date_Order_Placed', [], -1, -1, -1, '')]
    property Date_Order_Placed: Nullable<TDateTime> read FDate_Order_Placed write FDate_Order_Placed; 
    [Column('Total_Order_Price', [], -1, -1, -1, '')]
    property Total_Order_Price: Nullable<Double> read FTotal_Order_Price write FTotal_Order_Price; 
    [AutoGenerated]
    [Column('ORDER_ID', [cpPrimaryKey], -1, -1, -1, '')]
    property ORDER_ID: Integer read FORDER_ID write FORDER_ID; 
    [Column('Customer_ID', [], -1, -1, -1, '')]
    property Customer_ID: Integer read FCustomer_ID write FCustomer_ID; 
    [Column('Customer_Payment_Method_Id', [], -1, -1, -1, '')]
    property Customer_Payment_Method_Id: Nullable<Integer> read FCustomer_Payment_Method_Id write FCustomer_Payment_Method_Id; 
  
  end;
  
implementation    
  
end.


