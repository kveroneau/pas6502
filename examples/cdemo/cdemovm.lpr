program cdemovm;

{$mode objfpc}{$H+}

uses cpu6502;

type
  TTest = packed record
    c: Byte;
    i: Word;
    name: string[20];
    age: Byte;
  end;

var
  CPU: T6502;
  exit_code: Word;

function puts(addr: Word): Boolean;
begin
  Write(CPU.read_string(CPU.getax)); { Grab and write our string pointer. }
  Result:=True; { Handled. }
end;

function cdemo_end(addr: Word): Boolean;
begin
  exit_code:=CPU.getax;
  Result:=CPU.default_BRK(addr);
end;

function handle_exception(addr: Word): Boolean;
begin
  WriteLn('Entered failed state!');
  CPU.ShowStatus;
  CPU.Memory.SaveToFile('memory.dmp');
  Result:=CPU.default_BRK(addr); { End if this occurs. }
end;

function testit(addr: Word): Boolean;
var
  s: TTest;
  ptr: Word;
begin
  ptr:=CPU.getax; { Get the pointer to the passed in C struct }
  WriteLn('Passed struct data...');
  CPU.Memory.Position:=ptr;
  CPU.Memory.Read(s, 24);
  WriteLn(CPU.Memory.getB(ptr));
  WriteLn(s.c);
  Result:=True;
end;

function testme(addr: Word): Boolean;
var
  s: TTest;
  ptr: Word;
begin
  ptr:=CPU.getax;
  s.c:=65;
  s.i:=4200;
  s.name:='Pascal';
  s.age:=70;
  CPU.Memory.Position:=ptr;
  CPU.Memory.Write(s, 24);
  CPU.setax(ptr);
  Result:=True;
end;

function lf(addr: Word): Boolean;
begin
  WriteLn('');
  Result:=True;
end;

procedure DebugStep;
begin
  repeat
    WriteLn('PC=',CPU.PC);
    CPU.RunSlice;
  until not CPU.Running;
end;

begin
  CPU:=T6502.Create;
  CPU.Memory.SetIOCallback($f0, @cdemo_end);
  CPU.Memory.SetIOCallback($f1, @handle_exception);
  CPU.Memory.SetIOCallback($d0, @puts);
  CPU.Memory.SetIOCallback($d1, @testit);
  CPU.Memory.SetIOCallback($d2, @testme);
  CPU.Memory.SetIOCallback($d3, @lf);
  CPU.Memory.Position:=$900;
  CPU.Memory.LoadFromFile('stacktest.bin');
  CPU.Reset($900); { Load address for cdemo programs is $900 }
  CPU.Memory.setW($fffe,$fff1); { Set BRK to our exception handler. }
  CPU.Run;
  CPU.Free;
  WriteLn('Exit Code: ',exit_code);
end.

