

# library(MASS)
dane <- c(483, 705, 2623, 347, 620, 2719, 1035, 421)

# lambda daszek nw
eest <- fitdistr(dane, "exponential")$est

est <- 1 / mean(dane)

print(eest - est)

#ENW(EX) = ENW(1/lambda) = 1/ENW(lambda) = X daszek
1 / eest

# daszek(P(X < 1000)) = F(1000, daszek(lambda))
# = 1 - exp(-daszek(lambda)x) =

pexp(1000, eest)

