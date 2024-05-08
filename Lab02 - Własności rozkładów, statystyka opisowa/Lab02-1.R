# 1. Wygenerować dwie próby losowe: 20 i 100 elementową z rozkładu standardowego
# normalnego. Narysować dla obu prób dystrybuanty empiryczne i porównać je
# z odpowiednią dystrybuantą teoretyczną.

n = 1000
ex = 2;
sd = 4;
dane = rnorm(n, ex, sd)
dyst = ecdf(dane) # dystrybuanta empiryczna

plot(dyst)
curve(pnorm(x, ex, sd), add=T, col="blue") # tutaj x zawsze