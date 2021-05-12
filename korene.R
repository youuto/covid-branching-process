install.packages("rootSolve")
library(rootSolve)
eq <- function(t) {
  (1 + (R0/est_r)*(1-t))^(-est_r) - t
}
korene <- uniroot.all(eq, interval = c(0,2),
                      lower = 0,
                      upper = 2,
                      tol = 0.00000000001)


eq2 <- function(c) {
  (1 + ((1-c)*R0/est_r)*0.08)^(-est_r) - 0.92
}
c_pop <- uniroot.all(eq2, interval = c(0,2),
                   lower = 0,
                   upper = 2,
                   tol = 0.00000000001)

korene2

eq3 <- function(c) {
  c+(1-c)*(1+(R0/est_r)*0.08)^(-est_r) - 0.92
}
c_ind <- uniroot.all(eq3, interval = c(0,2),
                       lower = 0,
                       upper = 2,
                       tol = 0.00000000001)
