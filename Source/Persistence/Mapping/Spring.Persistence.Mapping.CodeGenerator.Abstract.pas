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

unit Spring.Persistence.Mapping.CodeGenerator.Abstract;

interface

uses
  Spring,
  Spring.Collections;

type
  TColumnData = class
  private
    fIsAutoGenerated: Boolean;
    fColumnName: string;
    fColumnLength: Nullable<Integer>;
    fColumnPrecision: Nullable<Integer>;
    fColumnScale: Nullable<Integer>;
    fColumnDescription: Nullable<string>;
    fIsRequired: Boolean;
    fIsUnique: Boolean;
    fDontInsert: Boolean;
    fDontUpdate: Boolean;
    FIsPrimaryKey: Boolean;
    fNotNull: Boolean;
    fIsHidden: Boolean;
    fColumnTypeName: string;
  public
    property ColumnName: string read fColumnName write fColumnName;
    property ColumnLength: Nullable<Integer> read fColumnLength write fColumnLength;
    property ColumnPrecision: Nullable<Integer> read fColumnPrecision write fColumnPrecision;
    property ColumnScale: Nullable<Integer> read fColumnScale write fColumnScale;
    property ColumnDescription: Nullable<string> read fColumnDescription write fColumnDescription;
    property ColumnTypeName: string read fColumnTypeName write fColumnTypeName;
    property IsAutogenerated: Boolean read fIsAutoGenerated write fIsAutoGenerated;
    property IsRequired: Boolean read fIsRequired write fIsRequired;
    property IsUnique: Boolean read fIsUnique write fIsUnique;
    property IsPrimaryKey: Boolean read FIsPrimaryKey write FIsPrimaryKey;
    property DontInsert: Boolean read fDontInsert write fDontInsert;
    property DontUpdate: Boolean read fDontUpdate write fDontUpdate;
    property NotNull: Boolean read fNotNull write fNotNull;
    property IsHidden: Boolean read fIsHidden write fIsHidden;
  end;

  TEntityModelData = class
  private
    fTableName: string;
    fSchemaName: string;
    fColumns: IList<TColumnData>;
  public
    constructor Create;
    function ToString: string; override;

    property TableName: string read fTableName write fTableName;
    property SchemaName: string read fSchemaName write fSchemaName;
    property Columns: IList<TColumnData> read fColumns;
  end;

  TAbstractCodeGenerator = class
  protected
    function GetEntityTypePrefix: string; virtual;
    function GetFieldNamePrefix: string; virtual;
    function GetIndent: string; virtual;

    function DoGenerate(const entityData: TEntityModelData): string; virtual; abstract;

    property EntityTypePrefix: string read GetEntityTypePrefix;
    property FieldNamePrefix: string read GetFieldNamePrefix;
    property Indent: string read GetIndent;
  end;

implementation

uses
  SysUtils;


{$REGION 'TEntityModelData'}

constructor TEntityModelData.Create;
begin
  inherited Create;
  fColumns := TCollections.CreateObjectList<TColumnData>(True);
end;

function TEntityModelData.ToString: string;
begin
  Result := SchemaName;
  if Result <> '' then
    Result := Result + '.';
  Result := Format('%0:s%1:s [%2:d]', [Result, TableName, Columns.Count]);
end;

{$ENDREGION}


{$REGION 'TAbstractCodeGenerator'}

function TAbstractCodeGenerator.GetEntityTypePrefix: string;
begin
  Result := 'T';
end;

function TAbstractCodeGenerator.GetFieldNamePrefix: string;
begin
  Result := 'F';
end;

function TAbstractCodeGenerator.GetIndent: string;
begin
  Result := '  ';
end;

{$ENDREGION}


end.
