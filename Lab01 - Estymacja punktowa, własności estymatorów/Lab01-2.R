N <- 500
n <- 1:N
n2 = 2:N
ex = 4
sd = 2
set.seed(201)

Y <- rnorm(N, ex, sd)
means <- cumsum(Y) / n

# odchylenia i średnie lepsze
medians <- c(N)
sds <- c(N)
iqrs <- c(N)

for (i in n) {
  y <- 1:i
  medians[i] <- median(Y[y])
  sds[i] <- sd(Y[y])
  
  # qnorm(.75) - qnorm(.25) = 1.35
  iqrs[i] <- IQR(Y[y]) / 1.35
}

plot(n, means, type = "l", xlab = "i", ylab = "",  las = 1,
     main = "Ciągi średnich i median", col = "orange")
lines(n, medians, type = "l", col = "blue", lty = 2)
legend("bottomleft", legend = c("Średnia", "Mediana"), col = c("orange", "blue"), lty = 1)
abline(h = ex, col = "red")

plot(n2, sds[n2], type = "l", xlab = "i", ylab = "", las = 1,
     main = "Ciąg odchyleń standardowych SDi i IQRi", col = "green")
lines(n2, iqrs[n2], type = "l", col = "red",  lty = 2)
legend("bottomleft", legend = c("Odchylenie std", "IQR"), col = c("green", "red"), lty = 1)
abline(h = sd, color = "blue")
