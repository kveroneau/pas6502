SOURCES = hellovm.c advdemo.c

%.bin: %.o
	ld65 -m $@.map -C clib/cdemo.cfg -o $@ $^ clib/cdemo.lib advlib.o

%.o: %.c
	cl65 -c -t none $<

advlib.o: advlib.s
	ca65 $<

all: $(SOURCES:.c=.bin)

clean:
	rm $(SOURCES:.c=.bin) *.map
