# Zbiór nlschools, znajdujący się w bibliotece MASS, zawiera dane dotyczące
# wybranych uczniów szkół holenderskich kończących ósmą klasę:
# IQ - wynik testu na IQ werbalne (w pkt.),
# SES - społeczno-ekonomiczny status rodziny ucznia.

# Czy na podstawie powyższych danych możemy stwierdzić na poziomie istotności
# 0,05, że wśród uczniów kończących ósmą klasę, ci pochodzący z domów
# o społeczno-ekonomicznym statusie powyżej mediany, mają wyższy poziom
# inteligencji werbalnej niż pozostali?

library(MASS)
#dim(nlschools)

# Model IV (kartka pozioma)
# x - iq tych co mają status społ-ekon powyżej mediany
# y - pozostali

# H0: mi_x = mi_y
# H1: mi_x > mi_y

# T = ułamek z karty

x = nlschools$IQ[nlschools$SES > median(nlschools$SES)]
y = nlschools$IQ[nlschools$SES <= median(nlschools$SES)]

t = (mean(x) - mean(y)) / (sqrt( var(x)/length(x) + var(y)/length(y) ))

pv = pnorm(t, lower.tail=F)
# albo pv = 1 - pnorm(t)
cat('t =', t, ', pvalue =', pv, '< 0.05 odrzucamy z hukiem, czyli majątek
    robi różnicę')







