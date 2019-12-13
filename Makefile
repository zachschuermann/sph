all: sph

sph:
	stack build

run: sph
	stack exec sph-exe

test: sph
	stack exec -- sph-exe -t

time: sph
	time ./.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/sph-exe/sph-exe -t +RTS -N4 -ls

timelx: sph
	time ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/sph-exe/sph-exe -t +RTS -N4 -ls

tscope: sph
	stack exec -- sph-exe -t +RTS -N4 -ls

clean:
	stack clean
