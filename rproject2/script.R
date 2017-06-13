#DATI FORNITI
Y <- c(7, 8, 9, 4, 7, 1, 8, 5, 6, 7, 0, 1, 6, 7, 9)
N <- sum(xtabs(~Y))

#DEFINISCO LE VARIABILI PER LAMBDA (parametri e iperparametri)
alpha0 <- 10 #totale mail arrivate in 
beta0 <- 3 #settimane
alpha1 <- alpha0 + sum(Y)
beta1 <- beta0 + N

# 1) Implementazione del modello
library(rstan)
data <- list(N, Y)

modPG <- '
data {
int N;
int Y[N];
int alpha0;
int beta0;
}
transformed data{

}

parameters {
real<lower=0> Lambda;

}
transformed parameters {

}
model {
Lambda ~ gamma(alpha0,beta0);
Y ~ poisson(Lambda);
}
'

# 2) Ottieni un campione simulato dalla a-posteriori

require(rstan)
iniTime <- date()
mod <- stan_model(model_code = modPG)
endTime <- date();c(iniTime = iniTime, endTime = endTime)

#SAMPLING E GENERAZIONE DELLE CATENE 
library(rstudioapi)
system.time(
  fit <<- sampling(mod,
                  data = data,
                  pars = c("Lambda"), #a noi interessa capire la distribuzione di Lambda, non di Y. 
#Perchè dopo che si campiona lambda si capisce come distribuisce la Y
                  chains = 4,
                  iter = 25000,
                  warmup = 10000,
                  thin = 5,
                  cores = 4,
                  seed = 1997,
                  control = list(max_treedepth = 10,
                                adapt_delta = 0.8)
  )
)

testModel <- stan(model_code = modPG, data = data, iter = 100, chains = 4)

# 3) Esplora le diagnostiche di outlput e discuti i risultati

require(ggmcmc)
outSim <- ggs(fit)
print(outSim)

# creiamo un file pdf di tutto l'output
ggmcmc(outSim)

###traceplots
library(ggthemes)
ggs_traceplot(outSim) + theme_fivethirtyeight()

### Densita
ggs_density(outSim) + theme_solarized(light = TRUE)

### R cappello ovvero diagnostica di Gelman
ggs_Rhat(outSim) + xlab("R_hat")

### diagnostica di Geweke (solo per normali standardizzate)
ggs_geweke(outSim)

### diagnostica con Caterpillar
ggs_caterpillar(outSim)

#intervalli di credibilit?
ci(outSim)

# sintesi per coppie di parametri
ggs_pairs(outSim, lower = list(continuous = "density"))
str(outSim)

# riassunti numeri e diagnostica
summary(fit)

# 4) Riassumi le caratteristiche principali della distribuzione a-posteriori

#                Commentare

# 5) Ottieni la distribuzione prededittiva della futura osservazione con un campione di dimensione 10k. 
#    Riassumi le sue caratteristiche.

modPGp <- '
data {
int N;
int Y[N];
}
parameters {
real<lower=0> Lambda;

}
transformed parameters {

}
model {
Lambda ~ gamma(95,18);
Y ~ poisson(Lambda);
}
generated quantities{
real Y_predict;
Y_predict = poisson_rng(Lambda);
}
'
iniTime <- date()
mod <- stan_model(model_code = modPGp)
endTime <- date();
c(iniTime = iniTime, endTime = endTime)

#campionamento
system.time(
  fit <<- sampling(mod,
                  data = data,
                  pars = c("Lambda", "Y_predict"),
                  chains = 4,
                  iter = 25000,
                  warmup = 10000,
                  thin = 5,
                  cores = 6,
                  seed = 1997,
                  control = list(max_treedepth = 10,
                  adapt_delta = 0.8)
  )
)

require(ggmcmc)

outSim = ggs(fit)
MailsOsservate <- c(mean(Y))
ggs_density(outSim, family = "Y_predict") + geom_vline(xintercept = MailsOsservate, color = "red")

# 6) Confronta i risultati ottenuti via simulazione MCMC con i risultati esatti sopra riportati

#    Commentare
