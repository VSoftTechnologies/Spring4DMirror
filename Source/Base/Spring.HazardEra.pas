﻿{***************************************************************************}
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

unit Spring.HazardEra;

interface

// implementation based on the paper by Pedro Ramalhete and Andreia Correia
// https://github.com/pramalhe/ConcurrencyFreaks/blob/master/papers/hazarderas-2017.pdf

function AcquireGuard(var p): Pointer;
procedure ReleaseGuard;

procedure EraArraySetLength(var a; count: NativeInt; elemInfo: Pointer);
procedure EraArrayCopy(var target; source: Pointer);
procedure EraArrayClear(var a);
procedure EraArrayDelete(var target; index: NativeInt; count: NativeInt = 1);

implementation

uses
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}
  SyncObjs,
  Spring;


type
  TEra = Int64;

  PEraEntity = ^TEraEntity;
  TEraEntity = packed record
    newEra: TEra;
    delEra: TEra;
  end;

const
  Inactive = 0;
  Active = -1;
  None = 0;


{$REGION 'Utility routines'}

{$IFDEF MSWINDOWS}
function GetCurrentThreadID: TThreadID;
asm
{$IFDEF CPUX86}
  mov eax,fs:[$24]
{$ELSE}
  mov eax,gs:[$48]
{$ENDIF}
end;
{$ENDIF}

function GetMem_Aligned64(size: Integer): Pointer;
const
  Alignment = 64;
  Bits = 6;
var
  p: Pointer;
begin
  GetMem(Result, size + Alignment);
  p := Result;
  UIntPtr(Result) := UIntPtr(Result) + Alignment;
  UIntPtr(Result) := UIntPtr(Result) and -Alignment;
  PPointer(UIntPtr(Result) - SizeOf(Pointer))^ := p;
end;

procedure FreeMem_Aligned64(p: Pointer);
begin
  FreeMem(PPointer(UIntPtr(p) - SizeOf(Pointer))^);
end;

{$ENDREGION}


{$REGION 'TThreadBlockList'}

type
  {$HINTS OFF}
  PHazardEraThreadControlBlock = ^THazardEraThreadControlBlock;
  THazardEraThreadControlBlock = record
  private
    Next: PHazardEraThreadControlBlock; {$IFNDEF CPU64BITS}strict private Padding1: Cardinal;private{$ENDIF}
    ThreadID: TThreadID;                {$IFNDEF POSIX}strict private Padding2: Cardinal;private{$ENDIF}
    Active: Integer;
    RefCount: Integer;
    Era: TEra;
  strict private
    Padding4: array[0..31] of Byte;
  public
    procedure Store(
      {$IFDEF CPUX86}{$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}{$ENDIF}
      const value: TEra); {$IFNDEF CPUX86}inline;{$ENDIF}
    function Load: TEra; {$IFNDEF CPUX86}inline;{$ENDIF}
  end;

  TThreadBlockList = record
  private class var
    blocks: array[0..255] of PHazardEraThreadControlBlock;
  class threadvar
    activeBlock: PHazardEraThreadControlBlock;
  public
    class destructor Destroy;

    class function Acquire: PHazardEraThreadControlBlock; static;
    class procedure Release; static;
  end;

function THazardEraThreadControlBlock.Load: TEra;
{$IFDEF CPUX86}
asm
  movq xmm0,[Self].Era
  movd eax,xmm0
  psrldq xmm0,4
  movd edx,xmm0
end;
{$ELSE}
begin
  Result := Era;
end;
{$ENDIF}

procedure THazardEraThreadControlBlock.Store(
  {$IFDEF CPUX86}{$IFDEF SUPPORTS_CONSTREF}[ref]{$ENDIF}{$ENDIF}
  const value: TEra);
{$IFDEF CPUX86}
asm
{$IFDEF SUPPORTS_CONSTREF}
  movq xmm0,[value]
{$ELSE}
  movq xmm0,value
{$ENDIF}
  movq [Self].Era,xmm0
end;
{$ELSE}
begin
  Era := value;
end;
{$ENDIF}

class destructor TThreadBlockList.Destroy;
var
  p, q: PHazardEraThreadControlBlock;
  i: Integer;
begin
  for i := 0 to High(blocks) do
  begin
    p := blocks[i];
    blocks[i] := nil;
    while Assigned(p) do
    begin
      q := p;
      p := p.Next;
      FreeMem_Aligned64(q);
    end;
  end;
end;

class function TThreadBlockList.Acquire: PHazardEraThreadControlBlock;

  function New(var era: PHazardEraThreadControlBlock; currentThreadId: TThreadID): PHazardEraThreadControlBlock;
  begin
    Result := GetMem_Aligned64(SizeOf(THazardEraThreadControlBlock));
    Result.ThreadID := currentThreadId;
    Result.Active := Active;
    Result.RefCount := 1;
    Result.Era := 1;

    Result.Next := PHazardEraThreadControlBlock(AtomicExchange(Pointer(era), Pointer(Result)));
    activeBlock := Result;
  end;

var
  currentThreadId: TThreadID;
  index: Integer;
begin
  currentThreadId := GetCurrentThreadID;

  Result := activeBlock;
  if Assigned(Result) then
  begin
    if AtomicExchange(Result.Active, Active) = Inactive then
    begin
      Result.ThreadID := currentThreadID;
      Result.RefCount := 1;
      Exit;
    end;
    if Result.ThreadID = currentThreadId then Exit;
  end;

  index := (currentThreadId xor (currentThreadId shr 8)) and High(blocks);

  Result := blocks[index];
  if Assigned(Result) then
  repeat
    if AtomicExchange(Result.Active, Active) = 0 then
    begin
      activeBlock := Result;
      Result.ThreadID := currentThreadId;
      Result.RefCount := 1;
      Exit;
    end;
    Result := Result.Next;
  until Result = nil;

  Result := New(blocks[index], currentThreadId);
end;

class procedure TThreadBlockList.Release;
var
  era: PHazardEraThreadControlBlock;
begin
  era := activeBlock;
  Dec(era.RefCount);
  if era.RefCount = 0 then
  begin
    era.Store(0);
    era.ThreadID := 0;
    era.Active := 0;
  end;
end;

{$ENDREGION}


{$REGION 'THazardEra'}

var
  lock: TCriticalSection;
  eraClock: TEra;
  retiredList: Vector<PEraEntity>;

function AcquireGuard(var p): Pointer;
var
  current: PHazardEraThreadControlBlock;
  prevEra, era: TEra;
begin
  current := TThreadBlockList.Acquire;
  prevEra := current.Load;
  repeat
    Result := Pointer(p);
    era := AtomicLoad(eraClock);
    if era = prevEra then Break;
    current.Store(era);
    prevEra := era;
  until False;
end;

procedure ReleaseGuard;
begin
  TThreadBlockList.Release;
end;

function Delete_Ptr(const obj: PEraEntity): Boolean;
var
  era: TEra;
  info: PHazardEraThreadControlBlock;
  i: Integer;
begin
  for i := 0 to High(TThreadBlockList.blocks) do
  begin
    info := TThreadBlockList.blocks[i];
    while Assigned(info) do
    begin
      if info.Active <> Inactive then
        era := info.Load
      else
        era := None;

      if (era = None) or (era < obj.newEra) or (era > obj.delEra) then
      begin
        info := info.Next;
        Continue;
      end;
      Exit(False);
    end;
  end;
  FreeMem(obj);
  Result := True;
end;

procedure Retire(p: PEraEntity);
var
  currEra, delEra, era: TEra;
  i: Integer;
  info: PHazardEraThreadControlBlock;
begin
  currEra := AtomicLoad(eraClock);

  lock.Acquire;
  try
    if Assigned(p) then
    begin
      p.delEra := currEra;
      retiredList.Add(p);
    end;

    if AtomicLoad(eraClock) = currEra then
      AtomicIncrement(eraClock);

    i := 0;
    while i < retiredList.Count do
      if Delete_Ptr(retiredList[i]) then
        retiredList.Delete(i)
      else
        Inc(i);
  finally
    lock.Release;
  end;
end;

{$ENDREGION}


{$REGION 'EraArray'}

type
  PEraArray = ^TEraArray;
  TEraArray = packed record
    newEra: TEra;
    delEra: TEra;
    elemInfo: PTypeInfo;
    Length: NativeInt;
  end;

procedure EraArraySetLength(var a; count: NativeInt; elemInfo: Pointer);
var
  p: PEraArray;
  oldSize, newSize: Integer;
begin
  if count = 0 then
  begin
    EraArrayClear(a);
    Exit;
  end;

  p := Pointer(a);
  oldSize := 0;
  if Assigned(p) then
  begin
    Dec(UIntPtr(p), SizeOf(TEraArray));
    oldSize := SizeOf(TEraArray) + p.Length * p.elemInfo.TypeSize;
  end;

  newSize := SizeOf(TEraArray) + count * PTypeInfo(elemInfo).TypeSize;
  if Assigned(p) then
    ReallocMem(p, newSize)
  else
    GetMem(p, newSize);
  if newSize > oldSize then
    FillChar((PByte(p) + oldSize)^, newSize - oldSize, 0);
  p.newEra := AtomicLoad(eraClock);
  p.elemInfo := elemInfo;
  p.Length := count;
  Inc(UIntPtr(p), SizeOf(TEraArray));
  Pointer(a) := p;
end;

procedure EraArrayCopy(var target; source: Pointer);
var
  p: PEraArray;
begin
  if source = nil then Exit;
  p := Pointer(UIntPtr(source) - SizeOf(TEraArray));
  Move(source^, Pointer(target)^, p.Length * p.elemInfo.TypeSize);
end;

procedure EraArrayClear(var a);
var
  p: PEraArray;
begin
  if Pointer(a) = nil then Exit;
  p := Pointer(UIntPtr(a) - SizeOf(TEraArray));
  Retire(PEraEntity(p));
  Pointer(a) := nil;
end;

procedure EraArrayDelete(var target; index, count: NativeInt);
var
  p: PEraArray;
  dest: Pointer;
  elemSize: Integer;
  tailCount: Integer;
begin
  if Pointer(target) = nil then Exit;
  p := Pointer(UIntPtr(target) - SizeOf(TEraArray));
  tailCount := p.Length - index - count;
  if tailCount > 0 then
  begin
    elemSize := p.elemInfo.TypeSize;
    dest := Pointer(PByte(target) + index * elemSize);
    Move(Pointer(PByte(target) + (index + count) * elemSize)^, Pointer(dest)^, tailCount * elemSize);
  end;
  EraArraySetLength(target, p.Length - count, p.elemInfo);
end;

{$ENDREGION}


initialization
  lock := TCriticalSection.Create;
  eraClock := 1;

finalization
  Retire(nil);
  lock.Free;

end.
