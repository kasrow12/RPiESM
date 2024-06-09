# Niech Gamma(a, β) oznacza rozkład gamma z parametrem kształtu a i drugim
# parametrem β, tzn. rozkład o gęstości [..,]

# Wygenerować n = 100 obserwacji z rozkładu Gamma(3, 2).
# Następnie przyjąć, że zapomnieliśmy wartości parametrów rozkładu gamma,
# z którego wygenerowaliśmy dane i, używając R, oszacować te parametry stosując
# metodę największej wiarygodności.

N <- 100
a = 3
b = 2

X = rgamma(N, a, b)

fit <- fitdistr(X, "gamma")
# fita <- fit$estimate[1]
# fitb <- fit$estimate[2]

print(fit$estimate)

