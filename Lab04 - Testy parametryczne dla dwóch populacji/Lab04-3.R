# Do badania wybrano w sposób losowy 15 dzieci chorych na cukrzycę. Poddano
# ich kuracji podając nowo opracowany lek. W pliku hemoglobina zapisano poziom
# hemoglobiny glikowanej (w %) u tych dzieci przed (zmienna przed) oraz po
# kuracji (zmienna po). Wiadomo, że poziomy te mają łączny rozkład normalny.


# a) Czy dane te potwierdzają, że nowy lek obniża poziom hemoglobiny glikowanej?
# Przyjąć poziom istotności 0,05.

# Model III
# próba z (X,Y) ~ wielowymiarowy normalny
# Z = X - Y

# H0: mi_przed = mi_po (mi_przed - mi_po = 0)
# H1: mi_przed > mi_po
# H0: mi_z = 0
# H1: mi_z > 0

wynik = t.test(hemoglobina$przed, hemoglobina$po, paired=T,
               alt="greater")
cat('a) p-value =', wynik$p.value, '< 0.05 => odrzucamy H0')


# b) Zakładając, że nowy lek obniża poziom hemoglobiny glikowanej o średnio 1,5%,
# wyznaczyć moc testu z pkt a) i podać interpretację otrzymanego wyniku.

# sd = sd(x-y), bo Z = X-Y
wynik = power.t.test(n=length(hemoglobina$przed), delta=1.5,
                     sd=sd(hemoglobina$przed - hemoglobina$po), 
                     sig.level=0.05, type="paired", alt="one.sided")
cat('\nb) power =', wynik$power)





