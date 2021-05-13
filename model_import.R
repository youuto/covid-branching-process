import <- read.csv("import.csv", header = FALSE)
import <- as.matrix(import)
imp_tab <- as.data.frame(table(import))
days <- as.integer(as.vector(imp_tab$Freq))       

set.seed(2345)
n <- 100000               
RI <- R0                  
rI <- est_r                
zI <- imp_tab$import      
pZI <- days/sum(days)      
for (i in 1:4)
{
  cat("generating generation",i,"\n")              
  ZI <- replicate
          (n, sum( rnbinom
                   ( sample(zI, 1, prob = pZI,
                                replace = TRUE),
                                mu = RI,size = rI)))
  ZItab <- as.data.frame(table(ZI))
  zI <- as.integer(as.vector(ZItab$ZI))
  pZI <- as.vector(ZItab$Freq/sum(ZItab$Freq))    
}
FZI <- numeric(length(pZI))    
FZI[1] <- pZI[1]
for (i in 2:length(pZI))
{
  FZI[i] <- FZI[i-1]+pZI[i]  
}
ZItab <- cbind(ZItab,pZI,FZI) 
                      



