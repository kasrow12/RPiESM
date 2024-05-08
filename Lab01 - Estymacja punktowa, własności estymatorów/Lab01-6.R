N = 10000
teta = 2

k = 20

# estnw teta X_n:n
# estmm = 2 X kreska

esty = replicate(N, {
  x = runif(k, 0, teta)
  c(2 * mean(x), max(x))
})

#esty[,1:5]

mean(esty[1,]) - teta
mean(esty[2,]) - teta

par(mfrow = c(2, 1))
plot(1:N, esty[1,], las = 1, xlab = "", ylab = "EMM")
plot(1:N, esty[2,], las = 1, xlab = "", ylab = "ENW")

# est teta 3 = a * teta NW
# żeby est3 nie był obciążony
# trzeba znać EX teta NW żeby wyznaczyć