build:
	ghc -o diff-HA.exe diff-HA.hs

# Run
run: build
	./diff-HA.exe
