install.packages("rootSolve")
library(rootSolve)

eq3 <- function(c)
{
  c+(1-c)*(1+(R0/est_r)*0.08)^(-est_r) - 0.92
}
c_ind <- uniroot.all(eq3, interval = c(0,2),
                     lower = 0,
                     upper = 2,
                     tol = 0.00000000001)        # vypocet faktora c pre individ.-spec. kontrolu

library(actuar)
set.seed(4598)
n <- 100000                                       # pocet simulacii
psc <- est_p                                      # parameter p NBi
rsc <- est_r                                      # parameter r NBi
p0m <- pnbi[1] + c_ind*(1 - pnbi[1])              # parameter p0
Zsc <- rzmnbinom(n, size=rsc, prob=psc, p0=p0m )  # pseudonahodne generovanie poctu potomkov, resp. prvej generacie
Ztabsc <- as.data.frame(table(Zsc))
zsc <- as.integer(as.vector(Ztabsc$Zsc))
pZsc <- as.vector(Ztabsc$Freq/sum(Ztabsc$Freq))
for (i in 1:20)
{
  cat("generating generation",i+1,"\n")            # vypise kolku generaciu pocita
  Zsc <- replicate(n,sum(rzmnbinom(sample(zsc,1,prob = pZsc,replace=TRUE),size = rsc,prob = psc,p0 = p0m)))
  Ztabsc <- as.data.frame(table(Zsc))
  zsc <- as.integer(as.vector(Ztabsc$Zsc))
  pZsc <- as.vector(Ztabsc$Freq/sum(Ztabsc$Freq)) 
  if (pZsc[1] >= 0.9) { break }                    # kriterium na ukoncenie G-W procesu
}



FZsc <- numeric(length(pZsc))    
FZsc[1] <- pZsc[1]
for (i in 2:length(pZsc))
{
  FZsc[i] <- FZsc[i-1]+pZsc[i]           # vypocet distribucnej funkcie
}
Ztabsc <- cbind(Ztabsc,pZsc,FZsc)       
ZItab                                    # vysledna tabulka
hist(ZI)                                 # histogram prislusnej generacie ktoru sme vypocitali