library(randtoolbox)
RNGkind()

#see e.g. https://en.wikipedia.org/wiki/Linear_congruential_generator

if(FALSE)
{
  
  
  #Park Miller congruential generator
  set.generator(name="congruRand", mod=2^31-1, mult=16807, incr=0, seed=12345)
  get.description()
  runif(5)
  setSeed(12345)
  congruRand(5, dim=1, mod=2^31-1, mult=16807, incr=0)
  RNGkind()
  
  # the Knuth Lewis RNG
  4294967296 == 2^32
  set.generator(name="congruRand", mod="4294967296", mult="1664525", incr="1013904223", seed=1)
  runif(5)
  setSeed(1)
  congruRand(5, dim=1, mod=4294967296, mult=1664525, incr=1013904223)
  
  
  # the POSIX rand48 
  281474976710656 == 2^48
  set.generator(name="congruRand", mod="281474976710656", mult="25214903917", incr="11", seed=1)
  runif(5)
  setSeed(1)
  congruRand(5, dim=1, mod=281474976710656, mult=25214903917, incr=11)
  
  
  
  # the NMIX RNG by Donald Knuth => produce two different result after the second term
  18446744073709551616 == 2^64
  set.generator(name="congruRand", mod="18446744073709551616", mult="1442695040888963407", incr="1013904223", seed=1)
  runif(5)
  #first value is
  (1442695040888963407 * 1 + 1013904223) / 2^64
  setSeed(1)
  congruRand(5, dim=1, mod=18446744073709551616, mult=1442695040888963407, incr=1013904223)
  
  
  #Haynes RNG
  set.generator(name="congruRand", mod="18446744073709551616", mult="636412233846793005", incr="1", seed=1)
  res <- get.description()
  runif(1)
  setSeed(1)
  congruRand(2, dim=1, mod=18446744073709551616, mult=636412233846793005, incr=1, echo = TRUE)
  
}
