library(randtoolbox)

#produce the same outputs since the same seed is used

setSeed(1234)
SFMT(5, mexp=607, usepset=FALSE)
setSeed(1234)
SFMT(5, mexp=607, usepset=FALSE)

#only first call produce the same outputs since time machine is used otherwise

setSeed(1234)
SFMT(5, mexp=607, usepset=FALSE)
SFMT(5, mexp=607, usepset=FALSE)
setSeed(1234)
SFMT(5, mexp=607, usepset=FALSE)
SFMT(5, mexp=607, usepset=FALSE)
