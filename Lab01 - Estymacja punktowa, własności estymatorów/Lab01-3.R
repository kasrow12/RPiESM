N <- 500
n <- 1:N
n2 = 2:N
a = 20
d = 1
#set.seed(201)

Y <- rcauchy(N, a, d)
means <- cumsum(Y) / n

medians <- c(N)
sds <- c(N)
iqrs <- c(N)

for (i in n) {
  y <- 1:i
  medians[i] <- median(Y[y])
  sds[i] <- sd(Y[y])

  iqrs[i] <- IQR(Y[y]) / 2
}

par(mfrow = c(1,2))

# mediany i iqr lepsze
plot(n, means, type = "l", xlab = "i", ylab = "",  las = 1,
     main = "Ciągi średnich i median", col = "orange")
lines(n, medians, type = "l", col = "blue", lty = 2)
legend("bottomleft", legend = c("Średnia", "Mediana"), col = c("orange", "blue"), lty = 1)
abline(h = a, col = "red")

plot(n2, sds[n2], type = "l", xlab = "i", ylab = "", las = 1,
     main = "Ciąg odchyleń standardowych SDi i IQRi", col = "green")
lines(n2, iqrs[n2], type = "l", col = "red",  lty = 2)
legend("bottomleft", legend = c("Odchylenie std", "IQR"), col = c("green", "red"), lty = 1)
abline(h = d, col = "blue")
par(mfrow = c(1,1))
