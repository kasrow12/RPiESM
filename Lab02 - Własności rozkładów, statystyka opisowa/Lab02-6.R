# 6. Zbiór Cars93, znajdujący się w bibliotece MASS, zawiera dane dotyczące
# różnych modeli samochodów osobowych.

library(MASS)
dim(Cars93)
head(Cars93)

summary(Cars93)
# Cars93$MPG.city


# a) Utworzyć nową zmienną o nazwie zp.m opisującą zużycie paliwa (mierzone w litrach na
# 100 km) podczas jazdy samochodu w mieście. Przyjąć, że 1 mila to 1,6 km; 1 galon
# amerykański to 3,8 litra. Odpowiednie dane wyrażone w milach na galon znajdują się
# w zmiennej MPG.city.

Cars93$zp.m = 1 / Cars93$MPG.city * 3.8 * 100 / 1.6
x = Cars93$zp.m


# b) Wyznaczyć podstawowe statystyki próbkowe dla danych w zmiennej zp.m.
# Obliczy¢ kwantyl rzędu 0,95 dla tych danych i podać jego interpretację.

mean(x)
var(x)
sd(x)
quantile(x, 0.95)
median(x)
IQR(x)
min(x)
max(x)
diff(range(x)) # rozstęp


# c) Sporządzić wykresy skrzynkowe dla zmiennej zp.m osobno dla samochodów
# amerykńskich i nieamerykańskich. Powtórzyć to samo dla zmiennej MPG.city.

b = boxplot(x~Cars93$Origin)
b = boxplot(Cars93$MPG.city~Cars93$Origin)
# coś z points(mean(dane))

#b $out ma outliarów w kolejności pliku, $group ma z której kartki
# stats od dolnego wąsa w górę
#'*komentarz*


# d) Narysować wykres słupkowy i kołowy dla zmiennej Type. Ile, spośród
# badanych samochodów, zaliczono do kategorii sportowe?
par(mfrow=c(1,2))
typ = table(Cars93$Type)
barplot(typ, col='salmon')

pie(typ) #labels
# paste()
par(mfrow=c(1,1))