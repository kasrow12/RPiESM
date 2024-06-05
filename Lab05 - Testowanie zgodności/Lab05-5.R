# W kolumnie czas w pliku infolinia.txt zapisano czasy oczekiwania (w min.)
# na połączenia z pewną infolinią. Używając testu Kołmogorowa-Smirnowa,
# sprawdzić czy można uznać, że prezentowane czasy pochodzą z rozkładu gamma
# Gamma(a, β) z parametrem kształtu a = 4,5 i drugim parametrem β = 4.
# Przyjąć poziom istotności α = 0,05.

# H0: Próba pochodzi z rozkładu Gamma(a=4.5, b=4)
# H1: !H1

dane = infolinia$czas
n = length(dane) # 100

wynik = ks.test(dane, y="pgamma", shape=4.5, rate=4)$p.value
cat('p-value =', wynik, '> 0.05 => brak podstaw do odrzucenia H0')
