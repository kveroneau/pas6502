SOURCES = crt0.s puts.s

LIBRARY = cdemo.lib

all: $(LIBRARY)

$(LIBRARY): $(SOURCES:.s=.o)
	ar65 a $(LIBRARY) $^

%.o: %.s
	ca65 $<

clean:
	rm -rf $(SOURCES:.s=.o)
