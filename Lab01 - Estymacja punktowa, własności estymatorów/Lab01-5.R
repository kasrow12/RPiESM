N <- 100

a = 3
b = 2

X = rgamma(N, a, b)

fit <- fitdistr(X, "gamma")
# fita <- fit$estimate[1]
# fitb <- fit$estimate[2]

print(fit$estimate)

