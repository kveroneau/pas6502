# Basic Usage document

While this ObjectPascal Unit isn't entirely finished yet, I think it's still a good time to start writing up it's
documentation for eager 65c02 developers who want to start getting down and dirty with this emulator.  So lets begin!

## Basic ObjectPascal program example

Currently this example code will be pretty bare, as there isn't yet too much which can be done without additional methods:

    program my6502;
    
    {$mode objfpc}{$H+}
    
    uses cpu6502;
    
    var
      CPU: T6502;
    
    begin
      CPU:=T6502.Create;
      CPU.Memory.LoadFromFile('6502_functional_test.bin');
      CPU.Reset($400); { $400 is the entry point for the above binary code. }
      { Here would be the soon to be created code to bind I/O addresses and other things. }
      CPU.RunSlice; { This will run a single operation on the virtual CPU and immediately return. }
      if CPU.Running then { Checking CPU.Running after either a RunSlice or Run will let you know if the CPU has halted or not. }
        CPU.Run; { This will run the 65c02 machine code until something switched off the Running flag. }
      CPU.Free; { Finally, free the allocated object. }
    end.

This example will expand as the CPU emulator code is expanded.  For now this example should give you a general idea on how it
works currently.

## Classes and Methods available

**TCPUFlags**: This type is how the internal CPU flags are set, you can either access it as a single byte or through Boolean values.

**TIOCallbackFunc**: This is the function signature for writing your own IO callbacks.

**T6502Memory**: This is a TStream compatible class, you can write external streams to it, but you can't currently exported it to
a new stream as a memory core dump.  This will be an option at a future date.

**T6502**: This is the main 65c02 CPU Emulator class which you will use in your own programs to create and run a 6502 CPU.

### T6502Memory class

This class is very similar to a standard TStream you might expect in Delphi/ObjectPascal.  It has the required methods to read in
buffers from other streams you might have lying around in your program code.  However, there are a few differences you should know
about.  Firstly, when you use either the *LoadFromStream* or *LoadFromFile* method calls, it will not reset the internal Position.
This is done purposely, as program code rarely loads directly into the zero-page of the 65c02 Memory.  Before calling this, if your
binary code expects to be loaded into a very specific address, be sure to either use *Seek* or set the *Position* property directly.

**setB** (addr: Word, value: Byte) -- Set a single Byte at a very specific memory address.  This does not move any internal positional
pointer.

**getB** (addr: Word): Byte -- Reads a single Byte from a very specific memory address.  Again, this does not move any internal
positional pointer.

**setW** (addr: Word, value: Word) -- Sets a Word(16-bit integer) to a specific memory address.  This does not move any internal
positional pointer.

**getW** (addr: Word): Word -- Reads a Word(16-bit integer) from a specific memory address.  This does not move any internal positional pointer.

**HandleIOCall** (addr: Word): Boolean -- This is called by *RunSlice* to check the function pointer table for a potential callback.

**SetIOCallback** (addr: Word; callback: TIOCallbackFunc) -- Call this to set-up an IO callback into your application code.

All these above methods were written mainly for the 65c02 OpCodes, but can be used externally by your application if need be.

### T6502 class

This class is the heart of the 65c02, it is the actual emulator and operation code parser to allow real 65c02 machine code to run.

*Memory*: T6502Memory - This is an externally exposed property which allows your own application code to directly access the 65c02
memory.  Here you can either load in local binary files from disk, or hand assemble opcodes using *setB* and *setW* method calls.

*Flags*: TCPUFlags - This is an externally exposed property which allows your own application code to access the CPU flags, if
creating a GUI application for example, you could use this to display the various CPU flags in real-time for debugging purposes.

*Running*: Boolean - This is a read-only property you can read to determine if the code has completed and the CPU has reached a halt,
in the initial implementation the **BRK** opcode will set this to *False*, however it will be configurable through application code.

**Reset** (PC: Word) -- Resets the 65c02 CPU core, it does not clear the memory, but does clear all the registers and sets the program
counter to what you specify as the *PC* parameter.  You should always call this after initializing the class before running the
emulator as well.  Remember, this is a compiled language, so uninitialized variables can contain garbage values.

**RunSlice** -- This allows you to have the CPU run just a single operation and return back to your application code.  This is very
useful if you want to run the CPU within a GUI program, but do not wish to run the CPU in a separate thread and instead plan on using
**Application.Process** messages within your main application loop.  This is also very useful if you want to run more than one CPU
core at once and avoid using threads.  You can place all the T6502 Classes into a loop, and call **RunSlice** on each one of them in a loop until they have all completed or whatever condition you are seeking.

**Run** -- This runs the CPU core in a dedicated loop, you should not call this in a GUI application as it will lock the GUI unless
you run this in a background thread.  Use this if you plan on using threads or are writing a CLI-based application.

**IRQ** (addr: Word) -- This incredibly useful method allows you to trigger an interrupt request in the virtual 65c02.  This is useful
if you plan on emulating a C64, Apple 2, etc Keyboard controller, or other hardware.  It can be used for an asortment of fun things.

**BRK** -- This triggers a hardware level BRK, it will run the same code as when the BRK opcode is parsed, but will report itself to
the 65c02 program as a hardware interrupt.  You can check this through the Break CPU flag.  See 6502.org for documentation.

**NMI** -- This triggers a non-maskable interrupt in the 6502, it cannot be masked by the Interrupt bit.

The methods below will be renamed to better suite their function, currently their names are mainly based on the cc65 runtime library
names...

**popax**: Word -- This pops the 6502 machine stack and returns back a 16-bit Word.  This is very useful when simulating an RTS.

**setax** (value: Word) -- This sets the A an X registers to a 16-bit Word, this is useful when integrating with cc65 programs when
you need to return a value from a non-void function call, such as a pointer to a string or an int.

**getax**: Word -- Does the opposite of setax, also very useful during cc65 API calls, it will usually contain the first C function
parameter if using __fastcall__ calling method.  Very useful for simple functions sending in a single integer or a string pointer.

**cs_popa**: Byte -- This is called *popa* in the cc65 runtime, it pops a single Byte off the C stack, not the 6502 machine stack.

**read_string** (addr: Word) -- Does exactly what it does, it reads a null-terminated string from the 6502 memory into a standard
ObjectPacal ansistring.  Very helpful when used with *getax* above to create a basic **puts()** routine for example.

It's starting to get late, so this is all I can write at the moment.
