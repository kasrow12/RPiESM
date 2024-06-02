# Zbadano grupę krwi 100 osób. Grupę 0 miało 36 osób, A - 42 osoby, B - 14 osób
# i grupę AB - 8 osób. Zweryfikować hipotezę że prawdopodobieństwa wystąpienia
# grup krwi 0, A, B, AB w populacji są równe odpowiednio: 0.4; 0.4; 0.1; 0.1.
# Przyjąć poziom istotności 0,05.

dane = c(36, 42, 14, 8)
prob = c(0.4, 0.4, 0.1, 0.1)

n = sum(dane)
zal = n*prob
# założenie >= 10 (git)

# H0: p1=0.4, p2=0.4, p3=0.1, p4=0.1
# H1: ~H0

wynik = chisq.test(dane, p=prob)$p.value

cat('pvalue =', wynik, '> 0.05 => nie ma podstaw do odrzucenia H0')
