#caso di studio iris

summary(iris)
#ascissa lunghezza sepalo
#ordinata lunghezza petalo
attach(iris)
plot(Sepal.Length, Petal.Length, col = as.numeric(Species), pch = 19)

modregs = lm(Petal.Length ~ Sepal.Length, data = iris)
modregs
abline(-7.101, 1.858, col = 4, lwd = 2)

modr2 = lm(Petal.Length ~ Sepal.Length + Species, data = iris)
modr2
valoridibeta = modr2$coefficients
abline(valoridibeta[1], valoridibeta[2], col = 1, lwd = 2)
abline(valoridibeta[1] + valoridibeta[3], valoridibeta[2], col = 1, lwd = 2)
abline(valoridibeta[1] + valoridibeta[4], valoridibeta[2], col = 1, lwd = 2)

modr3 = lm(Petal.Length ~ Sepal.Length*Species, data = iris)
modr3senzaper = lm(Petal.Length ~ Sepal.Length + Species + Sepal.Length : Species, data = iris)
modr3
vb2 = modr3$coefficients
vb2
abline(vb2[1], vb2[2], col = 1, lwd = 2)
abline(vb2[1] + vb2[3], vb2[2] + vb2[5], col = 2, lwd = 2)
abline(vb2[1] + vb2[4], vb2[2] + vb2[6], col = 3, lwd = 2)

BIC(modr3, modr2, modregs)
#il migliore è il 3 perchè ha il BIC più basso

model.matrix(modr3)[c(1,2,51,52,101,102),]