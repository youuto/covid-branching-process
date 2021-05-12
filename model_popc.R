install.packages("rootSolve")
library(rootSolve)

eq2 <- function(c)
{
  (1 + ((1-c)*R0/est_r)*0.08)^(-est_r) - 0.92
}
c_pop <- uniroot.all(eq2, interval = c(0,2),
                     lower = 0,
                     upper = 2,
                     tol = 0.00000000001) # vypocet faktora c pre celoplosnu kontrolu


set.seed(4356)
n <- 100000                              # pocet simulacii
Rpc <- (1-c_pop)*R0                      # reprodukcne cislo po celoplosnej kontrole
rpc <- est_r                             # disperzny parameter
Zpc <- rnbinom(n, mu = Rpc, size = rpc)  # pseudonahodne generovanie poctu potomkov, resp. prvej generacie
Ztabpc <- as.data.frame(table(Zpc))
zpc <- as.integer(as.vector(Ztabpc$Zpc))
pZpc <- as.vector(Ztabpc$Freq/sum(Ztabpc$Freq))
for (i in 1:20)
{
  cat("generating generation",i+1,"\n")             #vypise kolku generaciu pocita
  Zpc <- replicate(n,sum(rnbinom(sample(zpc,1,prob=pZpc,replace=TRUE),mu=Rpc,size=rpc)))
  Ztabpc <- as.data.frame(table(Zpc))
  zpc <- as.integer(as.vector(Ztabpc$Zpc))
  pZpc <- as.vector(Ztabpc$Freq/sum(Ztabpc$Freq))   
  if (pZpc[1] >= 0.9) { break }                     # kriterium na ukoncenie G-W procesu
}                           
FZpc <- numeric(length(pZpc))    
FZpc[1] <- pZpc[1]
for (i in 2:length(pZpc))
{
  FZpc[i] <- FZpc[i-1]+pZpc[i]  # vypocet distribucnej funkcie
}
Ztabpc <- cbind(Ztabpc,pZpc,FZpc)
Ztabpc                             # vysledna tabulka
hist(Zpc)                          # histogram prislusnej generacie ktoru sme vypocitali
