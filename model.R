set.seed(1234)
n <- 100000                         # pocet simulacii
R <- R0                             # parameter: zakladne reprodukcne cislo R0
r <- est_r                          # disperzny parameter
Z <- rnbinom(n, mu = R, size = r)   # pseudonahodne generovanie poctu potomkov, resp. prvej generacie
Ztab <- as.data.frame(table(Z))
z <- as.integer(as.vector(Ztab$Z))
pZ <- as.vector(Ztab$Freq/sum(Ztab$Freq))
for (i in 1:3)
{
  cat("generating generation",i+1,"\n")         #vypise kolku generaciu pocita
  Z <- replicate(n,sum(rnbinom(sample(z,1,prob=pZ,replace=TRUE),mu=R,size=r)))
  Ztab <- as.data.frame(table(Z))
  z <- as.integer(as.vector(Ztab$Z))
  pZ <- as.vector(Ztab$Freq/sum(Ztab$Freq))     # G-W proces
}
FZ <- numeric(length(pZ))    
FZ[1] <- pZ[1]
for (i in 2:length(pZ))   #cyklus
{
  FZ[i] <- FZ[i-1]+pZ[i]  # vypocet distribucnej funkcie
}
Ztab <- cbind(Ztab,pZ,FZ) 
Ztab                      # vysledna tabulka
hist(Z)                   # histogram prislusnej generaciu ktoru sme vypocitali


