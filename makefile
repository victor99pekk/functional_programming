# Makefile for Haskell program

# Compiler
HC = ghc

SOURCE = file.hs
OUTPUT = myprogram

.PHONY: all clean run

all: $(OUTPUT)

$(OUTPUT): $(SOURCE)
	$(HC) -o $(OUTPUT) $(SOURCE)

run: $(OUTPUT)
	./$(OUTPUT)

clean:
	rm -f $(OUTPUT) *.o *.hi
