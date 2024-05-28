# Compiler options
GHC = ghc
GHC_FLAGS = -O2 -Wall -package parsec

# Source file
SRC = Main.hs

# Output file
OUT = flp-fun

all: $(OUT)

$(OUT): $(SRC)
	$(GHC) $(GHC_FLAGS) -o $(OUT) $(SRC)

clean:
	rm -f $(OUT)
