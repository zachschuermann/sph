# SPH
2D smoothed-particle hydrodynamics simulation implemented in haskell.

## Getting Started
Requires `OpenGL`, `stack`.  

Simple: 
``` bash
make run
```

Using stack: 
``` bash
stack build
stack exec sph
```
or
``` bash
stack install
sph -t -n 1000 -c 150 +RTS -N8 -s
sph --help #print usage
```


## Log
Created with `stack`.  
```bash
stack new sph
cd sph
stack setup
stack build
```

On Ubuntu:
``` bash
apt install freeglut3 freeglut3-dev
```


## Ref
- https://github.com/cerrno/mueller-sph/blob/master/src/main.cpp
- https://www.cs.hs-rm.de/~panitz/hopengl/skript.pdf
- https://www.arcadianvisions.com/blog/2013/introducing-vinyl-gl.html
- https://github.com/haskell-opengl/GLUT/blob/master/examples/BOGLGP/Chapter03/Points.hsu
- https://twoguysarguing.wordpress.com/2010/02/20/opengl-and-haskell/
- https://www.arcadianvisions.com/blog/2011/modern-opengl-with-haskell.html
- https://wiki.haskell.org/OpenGLTutorial2
- https://www.cs.hs-rm.de/~panitz/hopengl/skript.pdf
- https://www.fpcomplete.com/blog/2017/09/all-about-strictness
