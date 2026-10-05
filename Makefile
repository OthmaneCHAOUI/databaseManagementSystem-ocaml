EXEC = exec
BIN = bin
SRC = src

# Default target: build the executable in bin/
$(BIN)/$(EXEC): $(SRC)/projet.cmo | $(BIN)
	ocamlc -g $(SRC)/projet.cmo -o $@

# Compile .ml to .cmo (object files stay in src/)
$(SRC)/%.cmo : $(SRC)/%.ml
	ocamlc -g -c $<

# Compile .mli to .cmi (not used here, but kept for completeness)
$(SRC)/%.cmi : $(SRC)/%.mli
	ocamlc -g -c $<

# Create bin/ directory if it doesn't exist
$(BIN):
	mkdir -p $@

# Clean all build artifacts
clean:
	rm -f $(SRC)/*.cmo $(SRC)/*.cmi $(BIN)/$(EXEC)

# Optional: run the executable
run: $(BIN)/$(EXEC)
	./$(BIN)/$(EXEC)

.PHONY: clean run