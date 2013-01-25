(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SQL.Commands.Delete;

interface

uses
  SQL.AbstractCommandExecutor, SQL.Types, SQL.Commands, SQL.Params, Mapping.Attributes
  ,Generics.Collections, Core.Interfaces;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Responsible for building and executing <c>delete</c> statements. 
  ///	</summary>
  {$ENDREGION}
  TDeleteExecutor = class(TAbstractCommandExecutor)
  private
    FTable: TSQLTable;
    FCommand: TDeleteCommand;
    FPrimaryKeyColumnName: string;
  protected
    function GetCommand: TDMLCommand; override;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Build(AClass: TClass); override;
    procedure BuildParams(AEntity: TObject); override;

    procedure Execute(AEntity: TObject); override;
  end;

implementation

uses
  Core.Exceptions
  ,Core.Utils
  ,Mapping.RttiExplorer
  ,Rtti
  ;

{ TDeleteCommand }

procedure TDeleteExecutor.Build(AClass: TClass);
var
  LAtrTable: TableAttribute;
begin
  EntityClass := AClass;
  LAtrTable := TRttiExplorer.GetTable(EntityClass);
  if not Assigned(LAtrTable) then
    raise ETableNotSpecified.CreateFmt('Table not specified for class "%S"', [AClass.ClassName]);

  FTable.SetFromAttribute(LAtrTable);

  FPrimaryKeyColumnName := TRttiExplorer.GetPrimaryKeyColumnName(EntityClass);
   //add fields to tsqltable
  FCommand.PrimaryKeyColumnName := FPrimaryKeyColumnName;
  FCommand.SetTable(nil);

  SQL := Generator.GenerateDelete(FCommand);
end;

procedure TDeleteExecutor.BuildParams(AEntity: TObject);
var
  LParam: TDBParam;
  LVal: TValue;
begin
  Assert(FPrimaryKeyColumnName <> '');
  inherited BuildParams(AEntity);

  LParam := TDBParam.Create;
  LParam.Name := Command.GetExistingParameterName(FPrimaryKeyColumnName);
  LVal := TRttiExplorer.GetPrimaryKeyValue(AEntity);
  LParam.Value := TUtils.AsVariant(LVal);
  SQLParameters.Add(LParam);
end;

constructor TDeleteExecutor.Create();
begin
  inherited Create();
  FTable := TSQLTable.Create;
  FCommand := TDeleteCommand.Create(FTable);
  FPrimaryKeyColumnName := '';
end;

procedure TDeleteExecutor.Execute(AEntity: TObject);
var
  LStmt: IDBStatement;
begin
  Assert(Assigned(AEntity));

  LStmt := Connection.CreateStatement;
  LStmt.SetSQLCommand(SQL);

  BuildParams(AEntity);
  try
    LStmt.SetParams(SQLParameters);

    inherited Execute(AEntity);

    LStmt.Execute();
  finally
    LStmt := nil;
  end;
end;

function TDeleteExecutor.GetCommand: TDMLCommand;
begin
  Result := FCommand;
end;

destructor TDeleteExecutor.Destroy;
begin
  FTable.Free;
  FCommand.Free;
  inherited Destroy;
end;

end.
