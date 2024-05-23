# Pełnomocnik rządu Alfalandii d/s równego statusu kobiet i mężczyzn podejrzewa,
# że udział mężczyzn wśród pracowników przedszkoli jest niższy niż minimum
# przewidziane w ustawie wynoszące 35%.

# rozkład dwupunktowy

# X_i = { 1 gdy mężczyzna; 0 gdy kobieta }

# X_i ~ binom(1,p), p=P(X=1)
# H0: p = 0.35
# H1: p < 0.35

# a) Czy na poziomie istotności 0,05 można uznać to stwierdzenie za uzasadnione,
# jeśli wśród losowo zbadanych 400 pracowników przedszkoli było 128 mężczyzn?

# n=400
# sumaXi = K -> k=128
# p z daszkiem = p/n (we wzorze na piechotę z kartki)
#         k
wynik = prop.test(x=128, n=400, p=0.35, alt='less')$p.value
cat('a) p-value =', wynik, '> alfa=0.05 -> nie ma podstaw do odrzucenia H')


# b) Utworzyć 95% przedział ufności dla odsetka mężczyzn wśród pracowników
# przedszkoli wykorzystując wyniki badania z punktu a).
wynik = prop.test(x=128, n=400, conf.level=0.95)$conf
cat('\nb) [', wynik, ']')


# c) Czy odpowiedź uzyskana w pkt. a). zmieniłaby się, gdyby pełnomocnik pobrał
# reprezentatywną próbkę 10 pracowników przedszkoli i 3 z nich okazałoby się
# mężczyznami?
wynik = prop.test(x=3, n=10, p=0.35, alt='less')$p.value
cat('\nc) prop.test: p-value =', wynik)
wynik = binom.test(x=3, n=10, p=0.35, alt='less')$p.value
cat('\nc) binom.test: p-value =', wynik)

# d) Utworzyć 95% przedział ufności dla odsetka mężczyzn wśród pracowników
# przedszkoli dla danych z punktu c).
wynik = binom.test(x=3, n=10)$conf
cat('\nd) [', wynik, ']', '\n')



  