# Badano staż pracy osób zatrudnionych w pewnej dużej sieci handlowej. Na 150
# losowo wybranych pracowników, 118 pracowało w tej sieci mniej niż 5 lat.
# Czy na tej podstawie można twierdzić, że 80% pracowników tej sieci legitymuje
# się stażem pracy mniejszym niż 5 lat? Zweryfikować odpowiednią hipotezę
# przyjmując poziom istotności 0,05.

# rozkład dwupunktowy
# X_i = { 1 gdy < 5 lat; 0 wpp }

# H0: p = 0.8
# H1: p != 0.8

k = 118
n = 150
# założenia
cat('np =', 150*118/150, ' >= 5 , nq =', 150*(1-118/150), '>= 5 => git')
wynik = prop.test(x=118, n=150, p=0.8, alt='two.sided')$p.value
cat('\np-value =', wynik, '> 0.05 => brak podstaw do odrz. H0')