# Wygenerować po N = 100 liczb z następujących rozkładów:
# (i) normalnego o średniej = 20 i odchyleniu standardowym = 5,
# (ii) jednostajnego na przedziale (−1, 1),
# (iii) wykładniczego o średniej = 5,
# (iv) Poissona o średniej = 3.

# H0: próba pochodzi z rozkładu normalnego
# H1: !H0

# a) Dla każdej wygenerowanej próbki sporządzić wykres normalności
#    i wykresy te wyświetlić w jednym oknie. Przeanalizować ich kształt.

dane = infolinia$czas
#dane = rnorm(100, 0.2, 10)
#dane = rgamma(100, 0.2, 10)
#dane = rcauchy(100) fajna linia pozioma
#dane = runif(100, 0, 110)

qqnorm(dane) # wykres kwantylowy
qqline(dane)

# infolinia -> raczej nie są z rozkładu normalnego, bo jest takie uśmiechnięte
# ma się pokrywać z linią przy normalnym (bo jest z gamma)


# b) Dla każdej wygenerowanej próbki sporządzić wykres skrzynkowy
#    i wykresy te wyświetlić w jednym oknie. Przeanalizować ich kształt.
# c) Dla każdej wygenerowanej próbki sporządzić histogram częstości i nanieść
#    na niego jądrowy estymator gęstości. Przeanalizować kształty tych wykresów.


# d) Dla danych wygenerowanych w pkt. (i) i (ii) przeprowadzić, na poziomie
# istotności 0,05, test normalności Shapiro-Wilka.

# dane z infolinii
wynik = shapiro.test(dane)$p.value
cat('p-value =', wynik, '< 0.05 => odrzucamy H0')
