data <- read.csv("offspring.csv", header=FALSE)
x <- as.matrix(data)
R0 <- mean(x)                                  # stredna hodnota poctu potomkov
disp <- var(x)                                 # disperzia poctu potomkov

est_r <- as.numeric(R0^2/(disp - R0))          # odhad parametra r NBi
est_p <- as.numeric(R0/disp)                   # odhad parametra p NBi

tab <- as.data.frame(table(x))
k <- as.integer(as.vector(tab$x))
nk <- as.integer(as.vector(tab$Freq))

pnbi <- dnbinom(k, size = est_r, prob = est_p) #vypocet hodnot pf NBi 
pnbi_resc <- pnbi*1/sum(pnbi)                  #uprava hodnot
Enbi <- pnbi_resc*sum(nk)                      #vypocet ocak.pocet. podla NBi


chisq <- sum((nk - Enbi)^2 / Enbi)    #vypocet statistiky
df <- length(k) - 3                   # stupne volnosti
p_value <- 1 - pchisq(q = chisq, df)  # p-hodnota
p_value
