                                        
                                        #pripad Z0 = 1
set.seed(1234)                          
n <- 100000                             #pocet simulacii
R <- R0                                 #reprodukcne cislo
r <- est_r                              #disperzny parameter
Z <- rnbinom(n, mu = R, size = r)       #pseudonahodne generovanie poctu potomkov X, Z1 ~ X
                                        #z negativne binomickeho rozdelenia
Ztab <- as.data.frame(table(Z))         
z <- as.integer(as.vector(Ztab$Z))          #usporiadane hodnoty poctu potomkov
pZ <- as.vector(Ztab$Freq/sum(Ztab$Freq))   #hodnoty pravdepodobnostnej funkcie poctu potomkov

for (i in 1:3)                              #vetviaci proces pre gen. Z4
{                            
 cat("generating generation", i+1,"\n")         
  Z <- replicate                            #vypocet diskretneho zlozeneho rozdelenia gen. n
        (n, sum( rnbinom                    #metodou Monte Carlo simulacie
                  ( sample(z, 1, prob = pZ,       
                           replace = TRUE),
                           mu = R, size = r) ) )
                                                        
  Ztab <- as.data.frame( table(Z))                                             
  z <- as.integer( as.vector(Ztab$Z))             #hodnoty generacie n na vypocet hodnot gen. n+1
  pZ <- as.vector( Ztab$Freq / sum(Ztab$Freq))    #pravdepodobnosti generacie n na vypocet pravd. gen. n+1 
}                                           
FZ <- numeric(length(pZ))                     
FZ[1] <- pZ[1]
for (i in 2:length(pZ))       
{
  FZ[i] <- FZ[i-1]+pZ[i]   #vypocet distribucnej funkcie generacie n
}
Ztab <- cbind(Ztab,pZ,FZ) 
Ztab                      #vysledna tabulka
hist(Z)                   #histogram prislusnej generacie ktoru sme vypocitali





