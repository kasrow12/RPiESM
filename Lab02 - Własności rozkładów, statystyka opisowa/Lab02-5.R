# 5. Utworzyć wykresy gęstości zmiennych losowych o rozkładzie chi-kwadrat o 5,
# 10 oraz 40 stopniach swobody. Przeanalizować, jak zmienia się gęstoś¢ rozkładu
# chi-kwadrat wraz ze wzrostem liczby stopni swobody

curve(dchisq(x, 5), xlim=c(0,100))
curve(dchisq(x, 10), add=T, col="red")
curve(dchisq(x, 40), add=T, col="blue")
curve(dchisq(x, 80), add=T, col="blue")
curve(dnorm(x, 80, sqrt(2*80)), add=T, col="green")

# EX = n, Var = 2n
# N(u = n, sd = sqrt(2n))
# podobnie jak normalny, ale ucieka

