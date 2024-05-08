N <- 10000
n <- 1:N
p = 1/4
set.seed(100)

X <- rbinom(N, 1, p)
S <- cumsum(X)
M <- S / n
print(M[N])
plot(1:N, M, type = "l", xlab = "n", ylab = "Średnia", las=1,
     main = "Wykres ciągu X1, X2, ..., XN")
abline(h = p, col = "red")

p <- 1/3
X <- rexp(N, p)
S <- cumsum(X)
M <- S / n
print(M[N])
plot(1:N, M, type = "l", xlab = "n", ylab = "Średnia", las=1,
     main = "Wykres ciągu X1, X2, ..., XN")
abline(h = 1/p, col = "red")

X <- rcauchy(N, 0, 1)
S <- cumsum(X)
M <- S / n
print(M[N])
plot(1:N, M, type = "l", xlab = "n", ylab = "Średnia", las=1,
     main = "Wykres ciągu X1, X2, ..., XN")
abline(h = 0, col = "red")

