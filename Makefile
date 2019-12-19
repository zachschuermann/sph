all: sph

sph:
	stack build

run: sph
	stack exec sph

test: sph
	stack exec -- sph -t

time: sph
	time ./.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/sph/sph -t +RTS -N4 -ls

timelx: sph
	time ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/sph/sph -t +RTS -N4 -ls

tscope: sph
	stack exec -- sph -t +RTS -N4 -ls

clean:
	stack clean
