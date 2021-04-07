library(readxl)

mango <- read_excel("C:/Users/Javi3/Downloads/mango.xlsx")

m1l <- lm(Largo ~ Ambiente, mango) #estimacion de los residuos por minimos cuadrados, no opera bien sin homocedasticidad
m1l <- lmer(Largo ~ (Ambiente * Genotipo) / (1|Hoja/Pseudoreplica), mango)
summary(modelo1l)
confint(modelo1l)
anova(modelo1l)

par(mfrow = c(2,2)) 
plot(modelo1l)
install.packages(c("ez","lmerTest"))
library(ez)
mj <- ezANOVA(dv = Largo, within = Ambiente, wid = Genotipo, data = mango) #Friedman test
mj$Mauchly#si hay significancia toca hacer una coreccion
mj$sphericity$GGe.DFn = mj$sphericity$GGe * mj$ANOVA$DFn[pos] #Greenhouse-Geisser
mj$sphericity$GGe.DFd = mj$sphericity$GGe * mj$ANOVA$DFd[pos]
mj$sphericity$HFe.DFn = mj$sphericity$HFe * mj$ANOVA$DFn[pos] #Huynh-Feldt
mj$sphericity$GGe.DFd = mj$sphericity$HFe * mj$ANOVA$DFd[pos]
mj$sphericity

library("nlme")
m2l <- gls(Largo ~ Ambiente, mango) #Estimacion de los residuos por cuadrados minimos generalizados la varianza es una funcion lineal

r2<-residuals(m2l,type="p") 
pred2l <- fitted(m2l)
plot(pred2l, r2l, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED")
boxplot(r2l ~ mango$Ambiente, xlab = "Ambiente", ylab = "Residuos estandarizados")
qqnorm(m2l, abline = c(0,1))

m3l <- gls(Largo ~ Ambiente, weights = varIdent(form=~1|Ambiente), mango) #Modela la varianza cada grupo tiene su propia varianza

m4l <- gls(Largo ~ Ambiente, weights = varComb(varPower()), data = mango) #La varianza es una potencia de alguna covariable no recomendable cuando la covariable toma valores de 0

m5l <- gls(Largo ~ Ambiente, weights = varComb(varExp()), data = mango) #La varianza es un exponente de alguna covarianza si puede tomar valores de cero, pero no deben ser muy altos

m6l <- gls(Largo ~ Ambiente, correlation = corCompSymm(form = ~ 1 | Genotipo), data = mango) #Factor aleatorio marginal

m7l <- gls(Largo ~ Ambiente, correlation = corCompSymm(form = ~ 1 |Genotipo), mango) #misma covariable a todas la observaciones

m8l <- gls(Largo ~ Ambiente, correlation = corCompSymm(form = ~ 1 |Genotipo), weights = varIdent(form=~1|Ambiente), mango) #lo mismo pero con diferentes varibles

m9l <- gls(Largo ~ Ambiente, correlation = corAR1(form = ~ 1 |Genotipo), mango)#autoregresiva de primer orden

m10l <- gls(Largo ~ Ambiente, correlation = corAR1(form = ~ 1 |Genotipo), weights = varIdent(form=~1|Ambiente), mango)

m11l <- gls(Largo ~ Ambiente, correlation = corSymm(form = ~ 1 |Genotipo), mango)

m12l<- lme(Largo ~ Ambiente, random = ~ 1 | Genotipo, correlation = corAR1(form = ~ 1 | Genotipo), mango) 

m12l<- lme(Largo ~ Ambiente, random = ~ 1 + Ambiente | Genotipo/ Hoja, correlation = corCAR1(form = ~ 1 | Genotipo), mango) 

m17l<- lme(Largo ~ Ambiente, random = ~ 1 + Ambiente | Genotipo/ Hoja, correlation = corCAR1(form = ~ 1 | Genotipo / Hoja), mango)



AIC(modelo1l, modelo2l, modelo3l, modelo4l, modelo5l)
anova(modelo4l, modelo5l)

library("multcomp") #Comparaciones multiples
library(lme4)
library(lmerTest)

m13l <- lmer(Largo ~ Ambiente + (1|Genotipo), mango) #Estimacion de los residuos por maxima verosimilitud factror aleatorio condicional
anova(m16l, type = 3, test.statistic = "F")
drop(m13l)

m14l <- lmer(Largo ~ Ambiente + (1|Genotipo/Hoja), REML = FALSE,  mango) #Modelo anidado

m15l <- lmer(Largo ~ Ambiente + (1 + Ambiente|Genotipo), mango) #intercepto y pendiente variable

m16l <- lmer(Largo ~ Ambiente + (1 + Ambiente|Genotipo/Hoja), mango) #Intercepto y pendiente variable

confint.merMod(m16l)
confint.merMod(m16a)

m1l <- lmer(pn ~ tratamiento + repeticion + f + c + (1|repeticion), REML = FALSE, p)



#Bayes

mod_string = "model {
for(i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]], prec)
}
for(j in 1:3) { 
mu[j] ~ dnorm(0.0, 1.0/1.0e6)
}
prec ~ dgamma(5/2.0, 5*1.0/20)
sig = sqrt(1.0/prec)
}"


mod_string1 = "model {
for(i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
}
for(j in 1:3) { 
mu[j] ~ dnorm(0.0, 1.0/1.0e6)
}
for(k in 1:3) {
prec[k] ~ dgamma(5/2.0, 5*1.0/20)
sig = sqrt(1.0/prec)}}"

set.seed(82)
data_jags <- list(y = PlantGrowth$weight, grp = as.numeric(PlantGrowth$group))
params <- c("mu","sig")
inits <- function() {
        inits <- list("mu" = rnorm(3, 0.0, 100.0), "prec" = rgamma(1, 1.0, 1.0))
}

mod <- jags.model(textConnection(mod_string), data = data_jags, inits = inits, n.chains = 3)

update(mod, 1e3)
mod_sim <- coda.samples(model = mod,variable.names = params, n.iter = 5e3)
mod_csim <- as.mcmc(do.call(rbind, mod_sim))
plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)
pm_params = colMeans(mod_csim)
yhat <- pm_params[1:3][data_jags$grp]
resid <- data_jags$y - yhat
plot(resid)
plot(yhat, resid)
summary(mod_sim)
HPDinterval(mod_csim)
HPDinterval(mod_csim, .90)
HPDinterval(mod_csim, .99)
mean(mod_csim[,3] > mod_csim[,1]) #la posterior comparacion de un tratamiento con el control