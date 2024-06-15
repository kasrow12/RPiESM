set.seed(123)
dane = rexp(25, 6)

library(MASS)
(est = fitdistr(dane, "exponential")$est)
(quantile(dane, 0.9))



dane = crabs$CW
(mean(dane))
(var(dane))
(median(dane))
quantile(dane, 0.8)


wykres = hist(dane)

shapiro.test(dane)$p.value

n = length(dane)
licznik = (n-1)*var(dane)
mian.lewy = qchisq(1-0.95/2, n-1)^2
mian.prawy = qchisq(0.95/2, n-1)^2

(licznik/mian.lewy)
(licznik/mian.prawy)


prop.test(x=24,n=500,p=0.04,alt="greater")$p.value
prop.test(x=24,n=500,p=0.04,alt="greater")$stat

prop.test(x=24, n=500, conf.level=0.99)$conf.int
#binom.test(x=24, n=500, conf.level=0.99)$stat


dane = c(22,30,22,16,10)
prob = c(dpois(0:3, 2), ppois(3, 2, lower.tail=F))

(pval = chisq.test(dane, p=prob)$p.value)
(pval = chisq.test(dane, p=prob)$stat)



x = subset(anorexia, Treat == "CBT")$Prewt
y = subset(anorexia, Treat == "CBT")$Postwt

t.test(x, y, paired=T, alt="less")$p.value
t.test(x, y, paired=T, alt="less")$stat

wynik = power.t.test(n=length(x), delta=3,
                     sd=sd(x - y), 
                     type="paired", alt="one.sided")
cat('\nb) power =', 1- wynik$power)
