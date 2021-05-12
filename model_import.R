import <- read.csv("import.csv", header = FALSE)
import <- as.matrix(import)
imp_tab <- as.data.frame(table(import))
days <- as.integer(as.vector(imp_tab$Freq))        # nacitanie a spracovanie udajov

set.seed(2345)
n <- 100000                # pocet simulacii
RI <- R0                   # parameter: zakladne reprodukcne cislo R0
rI <- est_r                # disperzny parameter
zI <- imp_tab$import       # pocet importovanych pripadov
pZI <- days/sum(days)      # pravdepodobnosti poctu import. prip.
for (i in 1:4)
{
  cat("generating generation",i,"\n")               #vypise kolku generaciu pocita
  ZI <- replicate(n,sum(rnbinom(sample(zI,1,prob=pZI,replace=TRUE),mu=RI,size=rI)))
  ZItab <- as.data.frame(table(ZI))
  zI <- as.integer(as.vector(ZItab$ZI))
  pZI <- as.vector(ZItab$Freq/sum(ZItab$Freq))    # G-W proces
}
FZI <- numeric(length(pZI))    
FZI[1] <- pZI[1]
for (i in 2:length(pZI))
{
  FZI[i] <- FZI[i-1]+pZI[i]  # distribucna funkcia poctu nakazencych 4-tej generacie
}
ZItab <- cbind(ZItab,pZI,FZI) 
ZItab                          # vysledna tabulka
hist(ZI)                      # histogram prislusnej generacie ktoru sme vypocitali



