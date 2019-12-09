all: sph

sph:
	stack build

run: sph
	stack exec sph-exe

clean:
	stack clean
