# 4. Utworzyć wykresy gęstości zmiennych losowych o rozkładzie t-Studenta o 5,
# 10 oraz 40 stopniach swobody. Przeanalizować, jak zmienia się gęstość rozkładu
# t-Studenta wraz ze wzrostem liczby stopni swobody.

# t student o n=1 = Cauchy

curve(dt(x, 5), xlim=c(-5,5))
curve(dt(x, 10), add=T, col="red")
curve(dt(x, 40), add=T, col="blue")
# czym wyższe n, ściska się bardziej do normalnego

# kwantyle
print(qnorm(0.12, 0, 1))
print(qt(0.12, 1000000))