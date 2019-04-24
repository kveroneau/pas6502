unit cpu6502;

{$mode objfpc}{$H+}

{
This is a 65c02 CPU Emulator, however since it runs as fast as the host will
allow it, I am classifying this more along the lines of a Virtual Machine not
for running classic 6502 software and binary programs, but for using in more
modern tasks.  If that last line confuses you, worry not, once ArcVM is released
all will be clear as a sunny day in Vancouver...

This is also the initial release of this CPU Emulator code, of which I have not
even tested yet.  So, if you still see this here message when you download or
look through this code, please be warned that I still need to fully test all the
CPU opcodes, although most of them should generally work fine.  There also isn't
an ideal way to load in binary code yet, in a future update, this feature will
become available.

I made my best attempt to use ObjectPascal in the most efficient way I knew
when putting this together and took advantage of how specific data types work,
such as the Byte type.  Did you know that in type-safe compiled langauges like C
and Pascal will wrap-around variables which are native to the host?  This means,
if you increment a Byte past $FF, it will wrap-around to $00?  I used this
knowledge when I developed this CPU Emulator, and used both the Byte and Word
data types to allow the host CPU to perform this wrap-around instantly for me,
as that is what the virtual 6502 CPU will expect.  This allows me to barely use
the "and $ff" against most variables which could potentially go above $ff from
a past calculation.

Another little fun fact is that this is probably the easiest to read 65c02 CPU
Emulator around!  There's no weird C preprocessing, or funky class stuff going
on here.  For the most part, I only decided to use classes in this code as I
wanted to easily encapsulate the CPU's state, such as registers without needing
to either pass them around all the time or have them as globals.  The idea here
is that if you wanted to run multiple 65c02 CPUs at the same time, and in either
a co-operative or fully threaded environment, you can!  This was actually a
requirement when I was designing ArcVM.  Oh, and you should check out that
project if you can:  https://bitbucket.org/kveroneau/arcvm/src/default/

In the near future, all of ArcVM will be written in ObjectPascal to allow for
super easy portability.  Python is good and all, but when you ask someone to
run your Python program on say Windows, it can become a bit of a mess sometimes,
and the Python runtime can be rather large.  With ArcVM in ObjectPascal, it will
be a single EXE/binary file on each OS, and use each OSes native features to get
it's job done.  It will be a cinch to ship an ArcVM powered program, or be able
to deployment it somewhere.  More news on all this will shared soon, for now you
can read the design docs and such in the ArcVM repository linked above.
}

interface

uses
  Classes, SysUtils;

type
  TFlags = bitpacked record
    Carry, Zero, Interrupt, Decimal, Break, NotUsed, Overflow, Negative: Boolean;
  end;
  TCPUFlags = packed record
    case Integer of
      0: (Reg: Byte);
      1: (Flag: TFlags);
  end;
  TIOCallbackFunc = function(addr: Word): Boolean;

  { T6502Memory }

  T6502Memory = class(TStream)
  private
    FMemory: Pointer;
    FPosition: PtrInt;
    FIOMap: Array[0..$ff] of TIOCallbackFunc;
    procedure ResetIOMap;
    function CStackPointer: Word;
  public
    constructor Create;
    destructor Destroy; override;
    procedure setB(addr: Word; value: Byte);
    function getB(addr: Word): Byte;
    procedure setW(addr, value: Word);
    function getW(addr: Word): Word;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): Longint;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    function HandleIOCall(addr: Word): Boolean;
    procedure SetIOCallback(addr: Byte; callback: TIOCallbackFunc);
    property Position: PtrInt read FPosition write FPosition;
    property csp: Word read CStackPointer;
  end;

  T6502 = class(TObject)
  private
    FMemory: T6502Memory;
    regP: TCPUFlags;
    regA, regX, regY, regSP: Byte;
    regPC, regSS: Word;
    FRunning: Boolean;
    intr_tbl: array[0..255] of Word;
    intr_ptr: Byte;
    procedure set_nv_flags(value: Byte);
    procedure setA(value: Byte);
    procedure setX(value: Byte);
    procedure setY(value: Byte);
    procedure set_carry0(value: Byte);
    procedure set_carry7(value: Byte);
    procedure BIT(value: Byte);
    procedure CLC;
    procedure SEC;
    procedure CLV;
    procedure set_overflow;
    procedure DEC(addr: Word);
    procedure INC(addr: Word);
    function overflow: Byte;
    function carry: Byte;
    function negative: Byte;
    function zero: Byte;
    procedure compare(reg, value: Byte);
    procedure test_sbc(value: Byte);
    procedure test_adc(value: Byte);
    function fetch: Byte;
    function fetch16: Word;
    procedure push(value: Byte);
    function pop: Byte;
    procedure branch(offset: Byte);
    procedure process_op;
  public
    constructor Create;
    destructor Destroy; override;
    property Memory: T6502Memory read FMemory;
    property Flags: TCPUFlags read regP;
    property Running: Boolean read FRunning;
    property PC: Word read regPC;
    function default_BRK(addr: Word): Boolean;
    procedure Reset(addr: Word);
    procedure RunSlice;
    procedure Run;
    procedure IRQ(addr: Word);
    procedure BRK;
    procedure NMI;
    procedure ShowStatus;
    { This method names will change in the future... }
    function popax: Word;
    procedure setax(value: Word);
    function getax: Word;
    function cs_popa: Byte;
    function read_string(addr: Word): string;
  end;

implementation

const
  MSize=$10000; { Although this should never change... }

{ T6502 }

procedure T6502.set_nv_flags(value: Byte);
begin
  if value = 0 then
    regP.Flag.Zero:=True
  else
    regP.Flag.Zero:=False;
  if (value and $80) = $80 then
    regP.Flag.Negative:=True
  else
    regP.Flag.Negative:=False;
end;

procedure T6502.setA(value: Byte);
begin
  regA:=value;
  set_nv_flags(regA);
end;

procedure T6502.setX(value: Byte);
begin
  regX:=value;
  set_nv_flags(regX);
end;

procedure T6502.setY(value: Byte);
begin
  regY:=value;
  set_nv_flags(regY);
end;

procedure T6502.set_carry0(value: Byte);
begin
  if (value and $1) = $1 then
    regP.Flag.Carry:=True
  else
    regP.Flag.Carry:=False;
end;

procedure T6502.set_carry7(value: Byte);
begin
  if ((value shr 7) and $1) = $1 then
    regP.Flag.Carry:=True
  else
    regP.Flag.Carry:=False;
end;

procedure T6502.BIT(value: Byte);
begin
  if (value and $80) = $80 then
    regP.Flag.Negative:=True
  else
    regP.Flag.Negative:=False;
  if (regP.Reg and value) = value then
    regP.Flag.Zero:=False
  else
    regP.Flag.Zero:=True;
end;

procedure T6502.CLC;
begin
  regP.Flag.Carry:=False;
end;

procedure T6502.SEC;
begin
  regP.Flag.Carry:=True;
end;

procedure T6502.CLV;
begin
  regP.Flag.Overflow:=False;
end;

procedure T6502.set_overflow;
begin
  regP.Flag.Overflow:=True;
end;

procedure T6502.DEC(addr: Word);
var
  value: Byte; { This lets the host CPU wrap-around 8-bit values for us. }
begin
  value:=FMemory.getB(addr);
  value:=value-1; { Host CPU will automatically take care of $00-$FF for us. }
  FMemory.setB(addr, value);
  set_nv_flags(value);
end;

procedure T6502.INC(addr: Word);
var
  value: Byte;
begin
  value:=FMemory.getB(addr);
  value:=value+1; { Host CPU will automatically take care of $FF-$00 for us. }
  FMemory.setB(addr, value);
  set_nv_flags(value);
end;

function T6502.overflow: Byte;
begin
  Result:=regP.Reg and $40;
end;

function T6502.carry: Byte;
begin
  Result:=regP.Reg and $1;
end;

function T6502.negative: Byte;
begin
  Result:=regP.Reg and $80;
end;

function T6502.zero: Byte;
begin
  Result:=regP.Reg and $2;
end;

procedure T6502.compare(reg, value: Byte);
begin
  if reg >= value then
    SEC
  else
    CLC;
  value:=reg-value;
  set_nv_flags(value);
end;

procedure T6502.test_sbc(value: Byte);
var
  tmp, w: Integer;
begin
  tmp:=0;
  if ((regA xor value) and $80) = $80 then
    set_overflow
  else
    CLV;
  if regP.Flag.Decimal then
  begin
    tmp:=($f + (regA and $f) - (value and $f) + (regP.Reg and $1));
    if tmp < $10 then
    begin
      w:=0;
      tmp:=tmp-$6;
    end
    else
    begin
      w:=$10;
      tmp:=tmp-$10;
    end;
    w:=w+$f0+(regA and $f10) - (value and $f0);
    if w < $100 then
    begin
      CLC;
      if regP.Flag.Overflow and (w < $80) then
        CLV;
      w:=w-$60;
    end
    else
    begin
      SEC;
      if regP.Flag.Overflow and (w >= $180) then
        CLV;
    end;
    w:=w+tmp;
  end
  else
  begin
    w:=$ff+regA-value+carry();
    if w < $100 then
    begin
      CLC;
      if regP.Flag.Overflow and (w < $80) then
        CLV;
    end
    else
    begin
      SEC;
      if regP.Flag.Overflow and (w >= $180) then
        CLV;
    end;
  end;
  regA:=(w and $ff);
  set_nv_flags(regA);
end;

procedure T6502.test_adc(value: Byte);
var
  tmp: Integer;
begin
  if ((regA xor value) and $80) = $80 then
    CLV
  else
    set_overflow;
  if regP.Flag.Decimal then
  begin
    tmp:=(regA and $ff) + (value and $f) + carry();
    if tmp >= 10 then
      tmp:=($10 or ((tmp + 6) and $f0));
    tmp:=tmp+(regA and $f0)+(value and $f0);
    if tmp >= 160 then
    begin
      SEC;
      if regP.Flag.Overflow and (tmp >= $180) then
        CLV;
      tmp:=tmp+$60;
    end
    else
    begin
      CLC;
      if regP.Flag.Overflow and (tmp < $80) then
        CLV;
    end;
  end
  else
  begin
    tmp:=regA+value+carry();
    if tmp >= $100 then
    begin
      SEC;
      if regP.Flag.Overflow and (tmp >= $180) then
        CLV;
    end
    else
    begin
      CLC;
      if regP.Flag.Overflow and (tmp < $80) then
        CLV;
    end;
  end;
  regA:=(tmp and $ff);
  set_nv_flags(regA);
end;

function T6502.fetch: Byte;
begin
  Result:=FMemory.getB(regPC);
  regPC:=regPC+1;
end;

function T6502.fetch16: Word;
begin
  Result:=FMemory.getW(regPC);
  regPC:=regPC+2;
end;

procedure T6502.push(value: Byte);
begin
  FMemory.setB((regSP and $ff)+regSS, value);
  regSP:=regSP-1;
end;

function T6502.pop: Byte;
begin
  regSP:=regSP+1;
  Result:=FMemory.getB(regSP+regSS);
end;

procedure T6502.branch(offset: Byte);
begin
  if offset > $7f then
    regPC:=(regPC - ($100-offset))
  else
    regPC:=(regPC + offset);
end;

procedure T6502.process_op;
var
  addr: Word;
  zp, offset, tmp: Byte;
  value: Word;
  tmpF: Boolean;
begin
  case fetch of
    $0: { BRK }
      begin
        addr:=regPC+1;
        push((addr >> 8) and $ff);
        push((addr and $ff));
        push(regP.Reg);
        regP.Flag.Break:=True;
        regP.Flag.Decimal:=False;
        regPC:=FMemory.getW($fffe);
      end;
    $1: { ORA (Indirect,X) }
      begin
        zp:=fetch+regX;
        addr:=FMemory.getW(zp);
        value:=FMemory.getB(addr);
        regA:=(regA or value);
        set_nv_flags(regA);
      end;
    $4: { TSB ZP }
      begin
        zp:=fetch;
        value:=FMemory.getB(zp);
        BIT(value);
        value:=(value and regA);
        FMemory.setB(zp, value);
      end;
    $5: setA(regA or FMemory.getB(fetch)); { ORA ZP }
    $6: { ASL ZP }
      begin
        zp:=fetch;
        value:=FMemory.getB(zp);
        set_carry7(value);
        value:=(value shl $1);
        FMemory.setB(zp, value);
        set_nv_flags(value);
      end;
    $8: push(regP.Reg or $30); { PHP }
    $9: setA(regA or fetch); { ORA Imm }
    $a: { ASL A }
      begin
        set_carry7(regA);
        regA:=(regA shl $1);
        set_nv_flags(regA);
      end;
    $c: { TSB ABS }
      begin
        addr:=fetch16;
        value:=FMemory.getB(addr);
        BIT(value);
        value:=(value and regA);
        FMemory.setB(addr, value);
      end;
    $d: setA(regA or FMemory.getB(fetch16)); { ORA ABS }
    $e: { ASL ABS }
      begin
        addr:=fetch16;
        value:=FMemory.getB(addr);
        set_carry7(value);
        value:=(value shl $1);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $10: { BPL }
      begin
        offset:=fetch;
        if not regP.Flag.Negative then
          branch(offset);
      end;
    $11: { ORA (Ind),Y }
      begin
        zp:=fetch;
        addr:=FMemory.getW(zp)+regY;
        regA:=(regA or FMemory.getB(addr));
        set_nv_flags(regA);
      end;
    $12: { ORA (ZP) }
      begin
        zp:=fetch;
        value:=FMemory.getW(zp);
        regA:=(regA or FMemory.getB(value));
        set_nv_flags(regA);
      end;
    $14: { TRB ZP }
      begin
        zp:=fetch;
        value:=FMemory.getB(zp);
        BIT(value);
        value:=(value and (regA xor $ff));
        FMemory.setB(zp, value);
      end;
    $15: { ORA ZP,X }
      begin
        zp:=fetch + regX;
        regA:=(regA or FMemory.getB(zp));
        set_nv_flags(regA);
      end;
    $16: { ASL ZP,X }
      begin
        zp:=fetch + regX;
        value:=FMemory.getB(zp);
        set_carry7(value);
        value:=value shl 1;
        FMemory.setB(zp, value);
        set_nv_flags(value);
      end;
    $18: CLC; { CLC }
    $19: { ORA ABS,Y }
      begin
        addr:=fetch16 + regY;
        regA:=(regA or FMemory.getB(addr));
      end;
    $1a: setA(regA+1); { INC A }
    $1c: { TRB ABS }
      begin
        addr:=fetch16;
        value:=FMemory.getB(addr);
        BIT(value);
        value:=(value and (regA xor $ff));
        FMemory.setB(addr, value);
      end;
    $1d: setA(regA or FMemory.getB(fetch16 + regX)); { ORA ABS,X }
    $1e: { ASL ABS,X }
      begin
        addr:=fetch16+regX;
        value:=FMemory.getB(addr);
        set_carry7(value);
        value:=(value shl $1);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $20: { JSR }
      begin
        value:=fetch16;
        addr:=regPC-1;
        push((addr shr 8) and $ff);
        push((addr and $ff));
        regPC:=value;
      end;
    $21: { AND (Ind,X) }
      begin
        zp:=fetch+regX;
        regA:=(regA and FMemory.getB(FMemory.getW(zp)));
        set_nv_flags(regA);
      end;
    $24: BIT(FMemory.getB(fetch)); { BIT ZP }
    $25: setA(regA and FMemory.getB(fetch)); { AND ZP }
    $26: { ROL ZP }
      begin
        addr:=fetch;
        value:=FMemory.getB(addr);
        tmp:=carry;
        set_carry7(value);
        value:=((value shl 1) or tmp);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $28: regP.Reg:=(pop or $30); { PLP }
    $29: setA(regA and fetch); { AND Imm }
    $2a: { ROL A }
      begin
        tmp:=carry;
        set_carry7(regA);
        regA:=((regA shl $1) or tmp);
        set_nv_flags(regA);
      end;
    $2c: BIT(FMemory.getB(fetch16)); { BIT ABS }
    $2d: setA(regA and FMemory.getB(fetch16)); { AND ABS }
    $2e: { ROL ABS }
      begin
        addr:=fetch16;
        value:=FMemory.getB(addr);
        tmp:=carry;
        set_carry7(value);
        value:=((value shl $1) or tmp);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $30: { BMI }
      begin
        offset:=fetch;
        if RegP.Flag.Negative then
          branch(offset);
      end;
    $31: setA(regA and FMemory.getB(FMemory.getW(fetch)+regY)); { AND (Ind),Y }
    $32: setA(regA and FMemory.getB(FMemory.getW(fetch))); { AND (zp) }
    $34: BIT(FMemory.getB((fetch+regX) and $ff)); { BIT ZP,X }
    $35: { AND ZP,X }
      begin
        zp:=fetch+regX;
        regA:=(regA and FMemory.getB(zp));
        set_nv_flags(regA);
      end;
    $36: { ROL ZP,X }
      begin
        addr:=fetch + regX;
        value:=FMemory.getB(addr);
        tmp:=carry;
        set_carry7(value);
        value:=((value shl $1) or tmp);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $38: SEC; { SEC }
    $39: setA(regA and FMemory.getB(fetch16+regY)); { AND ABS,Y }
    $3a: setA(regA-1); { DEC }
    $3c: BIT(FMemory.getB(fetch16+regX)); { BIT ABS,X }
    $3d: setA(regA and FMemory.getB(fetch16+regX)); { AND ABS,X }
    $3e: { ROL ABS,X }
      begin
        addr:=fetch16+regX;
        value:=FMemory.getB(addr);
        tmp:=carry;
        set_carry7(value);
        value:=((value shl $1) or tmp);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $40: { RTI }
      begin
        regP.Reg:=(pop or $30);
        regPC:=(pop or (pop shl 8));
      end;
    $41: { EOR (Ind,X) }
      begin
        zp:=fetch+regX;
        regA:=(regA xor FMemory.getB(FMemory.getW(zp)));
        set_nv_flags(regA);
      end;
    $45: setA(regA xor FMemory.getB(fetch)); { EOR ZP }
    $46: { LSR ZP }
      begin
        zp:=fetch;
        value:=FMemory.getB(zp);
        set_carry0(value);
        value:=(value shr $1);
        FMemory.setB(zp, value);
        set_nv_flags(value);
      end;
    $48: push(regA); { PHA }
    $49: setA(regA xor fetch); { EOR Imm }
    $4a: { LSR A }
      begin
        set_carry0(regA);
        regA:=(regA shr $1);
        set_nv_flags(regA);
      end;
    $4c: regPC:=fetch16; { JMP ABS }
    $4d: setA(regA xor FMemory.getB(fetch16)); { EOR ABS }
    $4e: { LSR ABS }
      begin
        addr:=fetch16;
        value:=FMemory.getB(addr);
        set_carry0(value);
        value:=(value shr $1);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $50: { BVC }
      begin
        offset:=fetch;
        if not regP.Flag.Overflow then
          branch(offset);
      end;
    $51: setA(regA xor FMemory.getB(FMemory.getW(fetch)+regY)); { EOR (Ind),Y }
    $52: setA(regA xor FMemory.getB(FMemory.getW(fetch))); { EOR (Ind) }
    $55: setA(regA xor FMemory.getB(fetch+regX)); { EOR ZP,X }
    $56: { LSR ZP,X }
      begin
        zp:=fetch+regX;
        value:=FMemory.getB(zp);
        set_carry0(value);
        value:=(value shr $1);
        FMemory.setB(zp,value);
        set_nv_flags(value);
      end;
    $58: regP.Flag.Interrupt:=False; { CLI }
    $59: setA(regA xor FMemory.getB(fetch16+regY)); { EOR ABS,Y }
    $5a: push(regY); { PHY }
    $5d: setA(regA xor FMemory.getB(fetch16+regX)); { EOR ABS,X }
    $5e: { LSR ABS,X }
      begin
        addr:=fetch16+regX;
        value:=FMemory.getB(addr);
        set_carry0(value);
        value:=(value shr $1);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $60: regPC:=(pop or (pop shl 8))+1; { RTS }
    $61: test_adc(FMemory.getB(FMemory.getW(fetch+regX))); { ADC (Ind,X) }
    $64: FMemory.setB(fetch, $0); { STZ ZP }
    $65: test_adc(FMemory.getB(fetch)); { ADC ZP }
    $66: { ROR ZP }
      begin
        tmpF:=regP.Flag.Carry;
        zp:=fetch;
        value:=FMemory.getB(zp);
        set_carry0(value);
        value:=(value shr $1);
        if tmpF then
          value:=(value or $80);
        FMemory.setB(zp, value);
        set_nv_flags(value);
      end;
    $68: setA(pop); { PLA }
    $69: test_adc(fetch); { ADC Imm }
    $6a: { ROR A }
      begin
        tmpF:=regP.Flag.Carry;
        set_carry0(regA);
        regA:=(regA shr $1);
        if tmpF then
          regA:=(regA or $80);
        set_nv_flags(regA);
      end;
    $6c: regPC:=FMemory.getW(fetch16); { JMP (Ind) }
    $6d: test_adc(FMemory.getB(fetch16)); { ADC ABS }
    $6e: { ROR ABS }
      begin
        tmpF:=regP.Flag.Carry;
        addr:=fetch16;
        value:=FMemory.getB(addr);
        set_carry0(value);
        value:=(value shr $1);
        if tmpF then
          value:=(value or $80);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $70: { BVS }
      begin
        offset:=fetch;
        if regP.Flag.Overflow then
          branch(offset);
      end;
    $71: test_adc(FMemory.getB(FMemory.getW(fetch)+regY)); { ADC (Ind),Y }
    $72: test_adc(FMemory.getB(FMemory.getW(fetch))); { ADC (Ind) }
    $74: FMemory.setB(fetch+regX,$0); { STZ ZP,X }
    $75: test_adc(FMemory.getB(fetch+regX)); { ADC ZP,X }
    $76: { ROR ZP,X }
      begin
        tmpF:=regP.Flag.Carry;
        zp:=fetch+regX;
        value:=FMemory.getB(zp);
        set_carry0(value);
        value:=(value shr $1);
        if tmpF then
          value:=(value or $80);
        FMemory.setB(zp, value);
        set_nv_flags(value);
      end;
    $78: regP.Flag.Interrupt:=True; { SEI }
    $79: test_adc(FMemory.getB(fetch16+regY)); { ADC ABS,Y }
    $7a: setY(pop); { PLY }
    $7c: regPC:=FMemory.getW(fetch16+regX); { JMP (ABS,X) }
    $7d: test_adc(FMemory.getB(fetch16+regX)); { ADC ABS,X }
    $7e: { ROR ABS,X }
      begin
        tmpF:=regP.Flag.Carry;
        addr:=fetch16+regX;
        value:=FMemory.getB(addr);
        set_carry0(value);
        value:=(value shr $1);
        if tmpF then
          value:=(value or $80);
        FMemory.setB(addr, value);
        set_nv_flags(value);
      end;
    $80: branch(fetch); { BRA }
    $81: FMemory.setB(FMemory.getW(fetch+regX), regA); { STA (Ind,X) }
    $84: FMemory.setB(fetch, regY); { STY ZP }
    $85: FMemory.setB(fetch, regA); { STA ZP }
    $86: FMemory.setB(fetch, regX); { STX ZP }
    $88: setY(regY-1); { DEY }
    $89: BIT(fetch); { BIT Imm }
    $8a: setA(regX); { TXA }
    $8c: FMemory.setB(fetch16, regY); { STY ABS }
    $8d: FMemory.setB(fetch16, regA); { STA ABS }
    $8e: FMemory.setB(fetch16, regX); { STX ABS }
    $90: { BCC }
      begin
        offset:=fetch;
        if not regP.Flag.Carry then
          branch(offset);
      end;
    $91: FMemory.setB(FMemory.getW(fetch)+regY, regA); { STA (Ind),Y }
    $92: FMemory.setB(FMemory.getW(fetch), regA); { STA (Ind) }
    $94: FMemory.setB((fetch+regX) and $ff, regY); { STY ZP,X }
    $95: FMemory.setB((fetch+regX) and $ff, regA); { STA ZP,X }
    $96: FMemory.setB((fetch+regY) and $ff, regX); { STX ZP,Y }
    $98: setA(regY); { TYA }
    $99: FMemory.setB(fetch16+regY,regA); { STA ABS,Y }
    $9a: regSP:=regX; { TXS }
    $9c: FMemory.setB(fetch16, $0); { STZ ABS }
    $9d: FMemory.setB(fetch16+regX, regA); { STA ABS,X }
    $9e: FMemory.setB(fetch16+regX, $0); { STZ ABS,X }
    $a0: setY(fetch); { LDY Imm }
    $a1: setA(FMemory.getB(FMemory.getW(fetch+regX))); { LDA (Ind,X) }
    $a2: setX(fetch); { LDX Imm }
    $a4: setY(FMemory.getB(fetch)); { LDY ZP }
    $a5: setA(FMemory.getB(fetch)); { LDA ZP }
    $a6: setX(FMemory.getB(fetch)); { LDX ZP }
    $a8: setY(regA); { TAY }
    $a9: setA(fetch); { LDA Imm }
    $aa: setX(regA); { TAX }
    $ac: setY(FMemory.getB(fetch16)); { LDY ABS }
    $ad: setA(FMemory.getB(fetch16)); { LDA ABS }
    $ae: setX(FMemory.getB(fetch16)); { LDX ABS }
    $b0: { BCS }
      begin
        offset:=fetch;
        if regP.Flag.Carry then
          branch(offset);
      end;
    $b1: setA(FMemory.getB(FMemory.getW(fetch)+regY)); { LDA (Ind),Y }
    $b2: setA(FMemory.getB(FMemory.getW(fetch))); { LDA (Ind) }
    $b4: setY(FMemory.getB(fetch+regX)); { LDY ZP,X }
    $b5: setA(FMemory.getB(fetch+regX)); { LDA ZP,X }
    $b6: setX(FMemory.getB(fetch+regY)); { LDX ZP,Y }
    $b8: CLV; { CLV }
    $b9: setA(FMemory.getB(fetch16+regY)); { LDA ABS,Y }
    $ba: setX(regSP); { TSX }
    $bc: setY(FMemory.getB(fetch16+regX)); { LDY ABS,X }
    $bd: setA(FMemory.getB(fetch16+regX)); { LDA ABS,X }
    $be: setX(FMemory.getB(fetch16+regY)); { LDX ABS,Y }
    $c0: compare(regY, fetch); { CPY Imm }
    $c1: { CMP (Ind,X) }
      begin
        zp:=fetch+regX;
        compare(regA, FMemory.getB(FMemory.getW(zp)));
      end;
    $c4: compare(regY, FMemory.getB(fetch)); { CPY ZP }
    $c5: compare(regA, FMemory.getB(fetch)); { CMP ZP }
    $c6: DEC(fetch); { DEC ZP }
    $c8: setY(regY+1); { INY }
    $c9: compare(regA, fetch); { CMP Imm }
    $ca: setX(regX-1); { DEX }
    $cc: compare(regY, FMemory.getB(fetch16)); { CPY ABS }
    $cd: compare(regA, FMemory.getB(fetch16)); { CMP ABS }
    $ce: DEC(fetch16);
    $d0: { BNE }
      begin
        offset:=fetch;
        if not regP.Flag.Zero then
          branch(offset);
      end;
    $d1: compare(regA, FMemory.getB(FMemory.getW(fetch)+regY)); { CMP (Ind),Y }
    $d2: compare(regA, FMemory.getB(FMemory.getW(fetch))); { CMP (Ind) }
    $d5: { CMP ZP,X }
      begin
        zp:=fetch+regX;
        compare(regA, FMemory.getB(zp));
      end;
    $d6: DEC((fetch+regX) and $ff); { DEC ZP,X }
    $d8: regP.Flag.Decimal:=False; { CLD }
    $d9: compare(regA, FMemory.getB(fetch16+regY)); { CMP ABS,Y }
    $da: push(regX); { PHX }
    $dd: compare(regA, FMemory.getB(fetch16+regX)); { CMP ABS,X }
    $de: DEC(fetch16+regX);
    $e0: compare(regX, fetch); { CPX Imm }
    $e1: { SBC (Ind,X) }
      begin
        zp:=fetch+regX;
        test_sbc(FMemory.getB(FMemory.getW(zp)));
      end;
    $e4: compare(regX, FMemory.getB(fetch)); { CPX ZP }
    $e5: test_sbc(FMemory.getB(fetch)); { SBC ZP }
    $e6: INC(fetch); { INC ZP }
    $e8: setX(regX+1); { INX }
    $e9: test_sbc(fetch); { SBC Imm }
    $ec: compare(regX, FMemory.getB(fetch16)); { CPX ABS }
    $ed: test_sbc(FMemory.getB(fetch16)); { SBC ABS }
    $ee: INC(fetch16); { INC ABS }
    $f0: { BEQ }
      begin
        offset:=fetch;
        if regP.Flag.Zero then
          branch(offset);
      end;
    $f1: test_sbc(FMemory.getB(FMemory.getW(fetch)+regY)); { SBC (Ind),Y }
    $f2: test_sbc(FMemory.getB(FMemory.getW(fetch))); { SBC (Ind) }
    $f5: { SBC ZP,X }
      begin
        zp:=fetch+regX;
        test_sbc(FMemory.getB(zp));
      end;
    $f6: INC((fetch+regX) and $ff); { INC ZP,X }
    $f8: regP.Flag.Decimal:=True; { SED }
    $f9: test_sbc(FMemory.getB(fetch16+regY)); { SBC ABS,Y }
    $fa: setX(pop); { PLX }
    $fd: test_sbc(FMemory.getB(fetch16+regX)); { SBC ABS,X }
    $fe: INC(fetch16+regX); { INC ABS,X }
  end;
end;

function T6502.default_BRK(addr: Word): Boolean;
begin
  FRunning:=False; { Let the main CPU instance know it's time to end. }
  Result:=True; { We handled the IO call. }
end;

constructor T6502.Create;
begin
  FMemory:=T6502Memory.Create;
end;

destructor T6502.Destroy;
begin
  FMemory.Free;
  FMemory:=nil;
  inherited Destroy;
end;

procedure T6502.Reset(addr: Word);
begin
  regA:=0;
  regX:=0;
  regY:=0;
  regP.Reg:=0;
  regPC:=addr;
  regSP:=$ff;
  regSS:=$100;
  FRunning:=False;
  intr_ptr:=0;
  FMemory.setW($fffa, $fff0); { Default NMI handler. }
  FMemory.setW($fffc, addr); { Set the RESET vector to allow for soft restarts. }
  FMemory.setW($fffe, $fff0); { Default BRK/IRQ handler. }
  {FMemory.SetIOCallback($f0, @default_BRK); { Set our IO callback! }}
end;

procedure T6502.RunSlice;
var
  addr: Word;
begin
  FRunning:=True;
  if (intr_ptr>0) and (not regP.Flag.Interrupt) then
  begin
    { An external interrupt is waiting in our queue... }
    intr_ptr:=intr_ptr-1;
    addr:=intr_tbl[intr_ptr];
    regP.Flag.Break:=False; { BRK Flag isn't set for external interrupts. }
    push((regPC shr 8) and $ff); { Push the stack for RTI }
    push(regPC and $ff);
    push(regP.Reg);
    regP.Flag.Interrupt:=True; { Set interrupt disable only after pushing P }
    regPC:=addr;
  end;
  if FMemory.HandleIOCall(regPC) then
    regPC:=popax+1 { Simulate RTS }
  else
    process_op;
end;

procedure T6502.Run;
begin
  Repeat
    RunSlice;
  until not FRunning;
end;

procedure T6502.IRQ(addr: Word);
begin
  { This is only useful if you have an IRQ table or know what you are doing! }
  if not regP.Flag.Interrupt then
  begin
    intr_tbl[intr_ptr]:=addr;
    intr_ptr:=intr_ptr+1;
  end;
end;

procedure T6502.BRK;
begin
  { This can be triggered from the outside application for a hw BRK. }
  IRQ(FMemory.getW($fffe)) { Call the standard ISR }
end;

procedure T6502.NMI;
begin
  { Non-maskable Interrupt, this cannot be blocked via SEI! }
  push((regPC shr 8) and $ff); { Push the stack for RTI }
  push(regPC and $ff);
  push(regP.Reg);
  regPC:=FMemory.getW($fffa); { $FFFA is the normal NMI vector on 6502 }
end;

procedure T6502.ShowStatus;
var
  tmp: Integer;
begin
  Write(LineEnding,'A=',regA);
  Write(', X=',regX);
  Write(', Y=',regY);
  tmp:=regSP;
  Write(', S=',IntToHex(tmp,2));
  tmp:=pop;
  tmp:=popax;
  WriteLn(', PC=',IntToHex(tmp,4));
end;

function T6502.popax: Word;
begin
  Result:=(pop or (pop shl 8));
end;

procedure T6502.setax(value: Word);
begin
  regA:=value and $ff;
  regX:=value shr 8;
end;

function T6502.getax: Word;
begin
  Result:=(regX shl 8)+regA;
end;

function T6502.cs_popa: Byte;
var
  sp: Word;
begin
  { Pops a single Byte from the C runtime stack. }
  sp:=FMemory.getW($80);
  Result:=FMemory.getB(sp);
  sp:=sp+1;
  FMemory.setW($80,sp);
end;

function T6502.read_string(addr: Word): string;
var
  b: Byte;
begin
  { This function reads a null-terminated string from 6502 memory. }
  Result:='';
  b:=FMemory.getB(addr);
  while b <> 0 do
  begin
    Result:=Result+chr(b);
    addr:=addr+1;
    b:=FMemory.getB(addr);
  end;
end;

{ T6502Memory }

procedure T6502Memory.ResetIOMap;
var
  i: Byte;
begin
  for i := 0 to $ff do
    FIOMap[i]:=nil; { Ensure all pointers are nil! }
end;

function T6502Memory.CStackPointer: Word;
begin
  Result:=getW($80); { Standard C stack pointer location for cc65 runtime. }
end;

constructor T6502Memory.Create;
begin
  FMemory:=AllocMem(MSize);
  ResetIOMap;
end;

destructor T6502Memory.Destroy;
begin
  ResetIOMap; { Nil all function pointers before we destrory instance. }
  Freemem(FMemory);
  FMemory:=nil;
  inherited Destroy;
end;

procedure T6502Memory.setB(addr: Word; value: Byte);
begin
  Byte((FMemory+addr)^):=value;
end;

function T6502Memory.getB(addr: Word): Byte;
begin
  Result:=Byte((FMemory+addr)^);
end;

procedure T6502Memory.setW(addr, value: Word);
begin
  setB(addr, value and $ff);
  setB(addr+1, (value shr 8) and $ff);
end;

function T6502Memory.getW(addr: Word): Word;
begin
  Result:=getB(addr)+(getB(addr+1) shl 8);
end;

function T6502Memory.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=0;
  if (FPosition<MSize) and (FPosition>=0) then
  begin
    Result:=Count;
    if (Result>(MSize-FPosition)) then
      Result:=MSize-FPosition;
    Move((FMemory+FPosition)^, Buffer, Result);
    FPosition:=FPosition+Result;
  end;
end;

function T6502Memory.Write(const Buffer; Count: Longint): Longint;
begin
  Result:=0;
  if (FPosition<MSize) and (FPosition>=0) then
  begin
    Result:=Count;
    if (Result>(MSize-FPosition)) then
      Result:=MSize-FPosition;
    Move(Buffer, (FMemory+FPosition)^, Result);
    FPosition:=FPosition+Result;
  end;
end;

function T6502Memory.Seek(const Offset: int64; Origin: TSeekOrigin): Longint;
begin
  case Word(Origin) of
    soFromBeginning : FPosition:=Offset;
    soFromEnd       : FPosition:=MSize+Offset;
    soFromCurrent   : FPosition:=FPosition+Offset;
  end;
  Result:=FPosition;
end;

procedure T6502Memory.LoadFromStream(Stream: TStream);
var
  Count: Longint;
begin
  Count:=Stream.Size;
  Stream.Position:=0;
  if Stream.Size > (MSize-FPosition) then
    Count:=MSize-FPosition;
  Stream.ReadBuffer((FMemory+FPosition)^, Count);
end;

procedure T6502Memory.LoadFromFile(const FileName: string);
var
  S: TFileStream;
begin
  S:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure T6502Memory.SaveToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FMemory^, MSize);
end;

procedure T6502Memory.SaveToFile(const FileName: string);
var
  S: TFileStream;
begin
  S:=TFileStream.Create (FileName,fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

function T6502Memory.HandleIOCall(addr: Word): Boolean;
var
  v: Byte;
begin
  v:=addr and $ff;
  Result:=False;
  if Assigned(FIOMap[v]) then
    Result:=FIOMap[v](addr);
end;

procedure T6502Memory.SetIOCallback(addr: Byte; callback: TIOCallbackFunc);
begin
  FIOMap[addr]:=callback;
end;

end.

