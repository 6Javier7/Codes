library(readr)
#file.path(datapath, "urban.csv.gz") la ruta para llegar al archivo que se quiere abrir, este puede estar comprimido
#urban <- read_csv(datafile) #lectura del archivo
#hay que esoecificar cada columna
#cols(
#        colname1 = col_character(),
#        colname2 = col_double(),
#        colname3 = col_character(),
#        colname4 = col_double(),
#        colname5 = col_character()
#)
# read_csv(datafile, col_types = "cccdc", n_max = 1000) mas facil con col_types = c = character, i = integer, n = number, d = double, l = logical, f = factor, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.las 5 columnas se especifican 



#Visual

ggplot(data = data, mapping = aes(x = displ, y = hwy, color = drv)) + 
        geom_point() + 
        geom_smooth(se = FALSE)

ggplot(mpg) +  geom_point(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy, linetype = drv)) #Lineas y puntos diferentes 

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") #Barras con probabilidad en una tercera variable fill


#Tamaño muestral

alfa <- 0.1
ceiling((4 * (qnorm(1 - alfa/2) - qnorm(alfa)))^2)

alfa <- 0.05
ceiling((4 * (qnorm(1 - alfa/2) - qnorm(alfa)))^2)


ceiling((4 * (qnorm(0.95) - qnorm(0.1)))^2)

#power
mu0 = 30
mua = 32
sigma = 4
n = 16
alpha = 0.05
z <- qnorm (1 - alpha)
pnorm (mu0 + z * sigma / sqrt(n), mean =  mu0, sd = sigma/sqrt(n), lower.tail = FALSE)
# ahora el power
pnorm (mu0 + z * sigma / sqrt(n), mean =  mua, sd = sigma/sqrt(n), lower.tail = FALSE)

power.t.test(n = 16, delta = 2, sd = 4, type = "one.sample", alternative = "one.sided" )$power 
power.t.test(power = .8, delta = 2, sd = 4, type = "one.sample", alternative = "one.sided" ) 


#Power

pnorm(qnorm(.95) * sd, mean = m, sd = sd, lower.tail = FALSE)

pnorm(1.645 * 0.004, mean = 0.01, sd = 0.004, lower.tail = FALSE)

#t Student interval

med_muestra + c(-1, 1) * qt(0.975, df) * sd/sqrt(n)

1100 + c(-1, 1) * qt(0.975, 8) * 30/sqrt(9)

#diferencia entre media

n1 <- n2 <- 9
x1 <- -3 ##treated
x2 <- 1 ##placebo
s1 <- 1.5 ##treated
s2 <- 1.8 ##placebo
s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
ts <- (x1 - x2)/(s * sqrt(1/n1 + 1/n2))
2 * pt(ts, n1 + n2 - 2)

#likelihood in poisson

ppois(10, lambda = 0.01 * 1787) #cual es la probabilidad de que la tasa de infecion sea mayor a 0.01 al encontrar 10 infectados en 1787 personas


# la minima diferencia que puede detectar
power.t.test(power = .8, n = 27, sd = 4, type = "one.sample", alternative = "one.sided" )$delta

#montecarlo power con la t student
nosim <- 100000
mu0 <- 30
mua <- 32
sigma <- 4
n <- 16
alpha <- 0.05
z <- rnorm (nosim) # simulaciones normales
xsq <- rchisq (nosim, 15) # simulaciones chi cuadrado
t <- qt(.95,15)
mean (z + sqrt(n) * (mua - mu0) / sigma > t / sqrt(n - 1) * sqrt(xsq)) 

# 7) Estimate by simulation: draw 1,000 samples from each and see how often 
#    we observe theta1>theta2

theta1=rbeta(1000,41,11)
theta2=rbeta(1000,32,20)
mean(theta1>theta2)

#intervalos
t
mu0 = mean 
sd = s
n
mu0 + c(1, -1) * qt(.975, n-1) * s/sqrt(n)
mu0 + c(1, -1) * qnorm(.975) * s/sqrt(n)
#dos colas, una cola con .95
n <- ((qnorm(.975)* sigma) / 2 * s) ^ 2
# Confidence intervals
z_star_95 <- qnorm(0.975)
sample1 %>%
  summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(n)))
#intervalo de confianza de la puntuación de Wilson
# cuando p es menor que .25 en muestras pequeñas y mayor que .85 intermedio usar el normal
p <-  0.1
n <-  5
z <-  qnorm(.975)
z1 <- qnorm(.975) ^ 2
n2 <- n * 2
p1 <- p + (z1 / n2)
q <-  1 - p
s <-  p * q
n4 <- 4 * n
j <-  z1 / n4
s1 <- s + j
e <-  z * sqrt(s1 / n)
c1 <- z1/n
c <-  1 + c1 
e1 <- c(1, -1) * e
b <- p1 + e1 
wilson <- b / c


#Logistic regression models are fit in R using the glm function with the argument family=binomial. 
#The logit link is the default and doesn't have to be specified. For true binomial (grouped binary) data the response "variable" 
#is a matrix constructed with the cbind function in which the first column is the number of successes and the second column is the number of failures.


out1 <- glm(cbind(y, n - y) ~ land.use + sewer, data = wells, family = binomial) #with the cbind function in which the first column is the number of successes and the second column is the number of failures.
summary(out1)

library(logistf)
out2d <- glm(cbind(y, n - y) ~ land.use4 + sewer, data = wells, family = binomial)
binom.test(2, 76)#The interval that we've calculated is called the Clopper-Pearson interval. 

Ei <- cbind(fitted(out2d)*wells$n, wells$n - fitted(out2d)*wells$n)
Oi <- cbind(wells$y, wells$n - wells$y)
#too many predicted counts are small for chi-squared distribution to hold
sum(Ei <= 5)/length(Ei)
sum((Oi - Ei)^2/Ei)#I carry out the Pearson test anyway.
1-pchisq(sum((Oi - Ei)^2/Ei), nrow(wells) - length(coef(out2d)))# p-value for test H0 the fit is good, H1 the fit is bad
df.residual(out2d)#degrees of freedom
deviance(out2d)
1-pchisq(deviance(out2d), df.residual(out2d)) #deviance analysis p-value for test H0 the fit is good, H1 the fit is bad
res.dev <- sum(ifelse(Oi == 0, 0, 2*Oi*log(Oi/Ei)))#count of zeros
1-pchisq(res.dev,out2d$df.residual)#p-value

sim.func <- function() { #A parametric bootstrap implementation of the residual deviance test
obs <- sapply(1:nrow(wells), function(x) rbinom(1, prob = fitted(out2d)[x], size = wells$n[x]))
my.glm <- glm(cbind(obs, n - obs) ~ land.use4 + sewer, data = wells, family = binomial)
ei <- fitted(my.glm)*wells$n
Ei <- cbind(ei, wells$n - ei)
Oi <- cbind(obs, wells$n - obs)
# pearson test
pearson <- sum((Oi - Ei)^2/Ei)
# G-test
gtest <- sum(ifelse(Oi == 0, 0, 2*Oi*log(Oi/Ei)))
# residual deviance (same as G-test)
dev <- deviance(my.glm)
# return residual deviance
dev}
# obtain 999 residual deviances from model
sims <- replicate(999, sim.func())
# append actual residual deviance to make 1000
sims <- c(sims, deviance(out2d))
# proportion of simulated values that exceed actual value
sum(sims >= deviance(out2d))/length(sims)

out2d$deviance/out2d$df.residual#If φ > 1 we say the data are overdispersed relative to a binomial distribution. If φ < 1 we say the data are underdispersed relative to a binomial distribution. 
pearson.deviance <- sum((Oi-Ei)^2/Ei)
pearson.deviance/out2d$df.residual#another form of calculate the dispertion of the data


#If the data are overdispersed or underdispersed relative to a binomial distribution then the rule says we need to correct for this in some way.
#One approach is to multiply the estimated binomial variance by φ yielding what's called a quasi-binomial model. 
out2d.quasi <- glm(cbind(y, n - y) ~ land.use4 + sewer, data = wells, family = quasibinomial)
summary(out2d.quasi)
#That's because we no longer have a likelihood-based model, hence no log-likelihood and no AIC are calculated (but see Burnham and Anderson 2002 for something they call QAIC). 
#he quasi-binomial model is not an actual probability model. It is just a post hoc adjustment to the variance of the binomial distribution.
phi <- pearson.deviance/out2d$df.residual
out2d.summary <- summary(out2d)$coefficients
sqrt(phi*out2d.summary[,2]^2)
#The overdispersion parameter has been used to increase the standard errors of the parameter estimates. This in turn has decreased the z-statistics and increased the p-values making it harder to achieve statistical significance.
#So, is fitting a quasi-binomial a good idea? Generally speaking I would say no.
#Still, fitting a quasi-binomial model may be preferable to doing nothing. 
#Because it inflates the reported standard errors (when there is overdispersion) the result is that all of your test results are made more conservative making it less likely for them to achieve statistical significance. 

#If I've exhausted all other possibilities—I can't find any more predictors to add to the model and a mixed effects model does not adequately address the problem
#then and only then would I consider fitting a quasi-binomial model and using it as my final analysis.

#Underdispersion with binomial data also means that the current binomial model is inappropriate but for different reasons. Once again there is heterogeneity in the success probabilities 
#within the sets of Bernoulli trials but of a very limited form yielding "binomial" count distributions that have a far more limited range of possible outcomes.
#If it turns out that the quasi-binomial model for underdispersion is correct but you instead choose to fit a binomial model anyway, no real damage is done. It just means that your reported statistical tests are too conservative.

exp(-coef(out2d)[3])
exp(-coef(out2d)[2])
# OR for contamination: mixed density versus rural
exp(coef(out2d)[2]-coef(out2d)[3])
out2d.conf <- confint(out2d)
out2d.conf

#A generalized additive model replaces the linear regression model with a sum of nonparametric smoothers. 
#GAMs are stand-alone models and so can be used as a replacement for generalized linear models. 
#i view GAMs as exploratory tools and generally will use them to help suggest a parametric form when biological theory is not very helpful. 

library(mgcv)
out2.gam <- gam(infection ~ sex + s(age) + s(weight), data = parasites, family = binomial)
#The functions s are nonparametric smooths and in the context of the model represent partial regression functions.
summary(out2.gam)
#Useful information can be obtained from the column labeled edf (estimated degrees of freedom) in the table of the smooth terms. 
#Values very different from 1 in this column indicate possible deviations from linearity. A better assessment is provided by plots of the smooths. 
par(mfrow = c(1, 2))
plot(out2.gam)
par(mfrow = c(1, 1))

#We can examine the plots individually by adding the select argument to the plot function and choosing by number which smooth to display. I plot the smooth of age and add a horizontal line at log odds = 0.
plot(out2.gam, select=1)
abline(h=0, col=2)#laces where the confidence bands enclose the horizontal line indicate age values where the overall pattern is not significant. 
out3 <- update(out1, .~. + I(age^2))#To carry out arithmetic on model terms within the regression model itself requires the use of the I function, I for identity. 
anova(out1, out3, test='Chisq')# Without the I function, R would ignore the arithmetic and just treat the term age as appearing in the model twice. 
c <- 6
out4 <- glm(infection~sex + age + I(age^2) + I((weight>=c)*(weight-c)), data=parasites, family=binomial)
summary(out4)

#Various functions in the ROCR package can be used to compute model sensitivity and specificity as well as plot them as a function of c. 
#The process begins by creating what's called a prediction object.
#To do this the prediction function in the ROCR package is supplied the fitted values from the model and a vector of presence/absence information.
library(ROCR)
pred1 <- prediction(fitted(out6), parasites$infection)
#Having constructed the prediction object the performance function is then used to extract the statistics of interest. 
stats1 <- performance(pred1 ,'tpr', 'tnr')#To obtain sensitivity and specificity, respectively, we need to specify 'tpr', for true positive rate, and 'tnr', for true negative rate.
str(stats1)
#From the printout we see the numeric slots are called @x.values, @y.values, and @alpha.values referring respectively to "True negative rate", "True positive rate", and "Cutoff".
stats1@alpha.values[[1]]#you need to use double bracket notation. So to access
plot(stats1@alpha.values[[1]], stats1@x.values[[1]], xlab=stats1@alpha.name, ylab='Classification rate', type='s')
#add sensitivity
lines(stats1@alpha.values[[1]], stats1@y.values[[1]], type='s', col=2)#The following code plots specificity and sensitivity against the cutoffs. I use type='s' to obtain a step function (because the confusion matrix does not change except at the reported values of c).
legend('right', c('specificity', 'sensitivity', 'MDT', 'MST'), col=c(1,2,4,'seagreen'), lty=c(1,1,2,3), lwd=c(1,1,1,2), bty='n', cex=.9)
#Minimized difference threshold (MDT)
precision(stats1@alpha.values[[1]][26])
#Maximized sum threshold (MST)
plot(stats1@alpha.values[[1]], stats1@x.values[[1]]+stats1@y.values[[1]], ylab=expression('Sensitivity'+'Specificity'), xlab='cutoff', type='s')
abline(v=stats1@alpha.values[[1]][37], lty=3, col='seagreen', lwd=2)
legend('topright', 'MST', lty=3, lwd=2, col='seagreen', bty='n', cex=.9)

#obtain statistics for first model
pred1 <- prediction(fitted(out6), parasites$infection)
stats1a <- performance(pred1, 'tpr', 'fpr')
#obtain statistics for second model
pred2 <- prediction(fitted(out1), parasites$infection)
stats2 <- performance(pred2, 'tpr', 'fpr')
#formatted labels for the models
mod1.lab <- expression('sex'+'age'^2+'weight'>12)
mod2.lab <- expression('sex'+'age'+'weight')
#draw graphs
plot(stats1a@x.values[[1]], stats1a@y.values[[1]], type='s', ylab=stats1a@y.name, xlab=stats1a@x.name, col='grey70', lwd=2)
lines(stats2@x.values[[1]], stats2@y.values[[1]], type='s', col=2, lty=2)
legend('bottomright', c(mod1.lab, mod2.lab), col=c('grey70',2), lwd=c(2,1), lty=1:2, cex=.9, bty='n')






#The dplyr and tidyr packages have numerous functions (sometimes referred to as “verbs”) for cleaning up data. We’ll start with the functions to summarize data.
#tidyr

#Reshape Data 
gather(data, key, value, ..., na.rm = FALSE,  convert = FALSE, factor_key = FALSE) #moves column names into a key column, gathering the column values into a single value column.

gather(table4a, `1999`, `2000`,  key = "year", value = "cases")#los nombre se ponen ente este simbolo '', key es el nombre de la columna que va a combinar las columnas existentes, 
#y en values se pone el nombre a la columna con los valores

spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) #hace lo contrario esta funcion separa una columna en dos o mas
#moves the unique values of a key column into the column names, spreading the values of a value column across the new columns.
spread(table2, type, count)#type es la columna que se quiere separar, y count es la columna con los valores que se van a poner en cada columna

#Handle Missing Values
drop_na(data, ...)# Drop rows containing NA’s in … columns.
drop_na(x, x2)#Quita los NA
fill(data, ..., .direction = c("down", "up")) #Fill in NA’s in … columns with most recent non-NA values.
fill(x, x2)#cambia los NA por el valor anterior
replace_na(data,  replace = list(), ...) #Replace NA’s by column.
replace_na(x, list(x2 = 2))#Reemplaza los NA por el numero 2


 

#dplyr

data(mtcars) # para desagrupar se tiene la funcion ungroup(x, …) 
g_mtcars <-mtcars %>%  #Primero se pone la matriz seguigo del  simpolo %>%
group_by(cyl) %>% # Luego se indica con que variable(Columna) se quiere Agrupar 
summarise(avg = mean(mpg)) #Summarize te muestra un resumen de los datos pero hay que indicarle que datos se quieren

#Group Cases
group_by(.data, ..., add = FALSE)
ungroup(x, …) 

#Summarise Cases
#. Summary functions take vectors as input and return one value
#summarise(Nombre de la Columna en el summary = funcion ejemplo :mean(mpg))
summarise(avg = mean(mpg))
count(x, ..., wt = NULL, sort = FALSE)  #Count number of rows in each group 
count(iris, Species)

summarise_all() # Apply funs to every column. 
summarise_at() # Apply funs to specific columns.  
summarise_if() # Apply funs to all cols of one type.

#RANK 
quantile() # nth quantile  
min() # minimum value 
max() # maximum value 
#SPREAD 
IQR() # Inter-Quartile Range  
mad() # median absolute deviation 
sd() # standard deviation 
var() # variance

#https://mauricioanderson.com/curso-r-loop-functions/
#https://mauricioanderson.com/curso-r-debugging/
#Manipulate Cases
#When cleaning up data, you will need to be able to create subsets of the data, by selecting certain columns or filtering down to certain rows.
filter(.data, …) #Extract rows that meet logical criteria.
filter(iris, Sepal.Length > 7)

distinct(.data, ..., .keep_all = FALSE) #Remove rows with duplicate values.   
distinct(iris, Species)  

sample_frac(tbl, size = 1, replace = FALSE,  weight = NULL, .env = parent.frame()) #Randomly select fraction of rows.   
sample_frac(iris, 0.5, replace = TRUE)  #Selecciona una matriz con la mitad de los datos
sample_n(tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) # Randomly select size rows. 
sample_n(iris, 10, replace = TRUE)  
slice(.data, …) #Select rows by position.  
slice(iris, 10:15)
top_n(x, n, wt) #Select and order top n entries (by group if grouped data). 
top_n(iris, 5, Sepal.Width)
arrange(.data, …) #Order rows by values of a column or columns (low to high), use with desc() to order from high to low. 
arrange(mtcars, mpg) 
arrange(mtcars, desc(mpg))
add_row(.data, ..., .before = NULL, .after = NULL) #Add one or more rows to a table. 
add_row(faithful, eruptions = 1, waiting = 1)

#Manipulate Variables

pull(.data,  var = -1) #Extract column values as a vector.  Choose by name or index. 
pull(iris, Sepal.Length)
select(.data, …) #Extract columns as a table. Also select_if(). 
select(iris, Sepal.Length, Species)
#Use these helpers with select ()
select(iris, starts_with("Sepal"))
helpers
contains(match)  
ends_with(match)  
matches(match)
num_range(prefix, range) 
one_of(…)  
starts_with(match) 
ends_with()#Select all columns that end with a certain string (for example, select(ext_tracks, ends_with("ne")) to get all the wind radii for the northeast quadrant of a storm for the hurricane example data)

contains()#Select all columns that include a certain string (select(ext_tracks, contains("34")) to get all wind radii for 34-knot winds)


mutate(.data, …)   #Compute new column(s). 
mutate(mtcars, gpm = 1/mpg)
mutate(age = row.names(VADeaths)) #los nombres de las columna en una nueva columna
transmute(.data, …)  #Compute new column(s), drop others. 
transmute(mtcars, gpm = 1/mpg) 
mutate_all(.tbl, .funs, …) #Apply funs to every column. Use with funs(). Also mutate_if().  
mutate_all(faithful, funs(log(.), log2(.))) 
mutate_if(iris, is.numeric, funs(log(.)))
mutate_at(.tbl, .cols, .funs, …) #Apply funs to specific columns. Use with funs(), vars() and the helper functions for select().  
mutate_at(iris, vars( -Species), funs(log(.))) 

#50 repeticiones aleatoreas de los intervalos de confianza
ci <- ames %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(n)))
#los primero 5 intervalos de la repeticion
ci %>%
  slice(1:5)
#separar variableas categoricas y encontrar la edia de otra variable en cada grupo
nc %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight))

#tamaño poblacional
sum(dbinom(1:4, size = 20, p = 0.5 ))
#verosimilitud de proporciones con hipotesis nula .5 y con una muestra de 4 exitos
#para hacer la prueba de hipostesis en proporciones se debe analizar cual es la probabilidad de tener ese numero de exitos o mas
choose(5,1) * .10 * .9 ^ 4
choose(5,1) * .20 * .8 ^ 4
#el area en una beta(1,5) es 1 -(x - 1) ^ 4
qbeta(0, 1, 5)
qbeta(0.95, 1, 5)
#We can find the critical value for a 95% confidence interal using
z_star_95 <- qnorm(0.975)
#As a first step in the analysis, we should take a look at the variables in the dataset. This can be done using the `str` command
#Using visualization and summary statistics, describe the distribution of weight gained by mothers during pregnancy. The `summary` function can also be useful.
summary(nc$gained)
#Next, we introduce a new function, `inference`, that we will use for conducting hypothesis tests and constructing confidence intervals. 
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")


sum(dbinom(0:4,20,.5)) # valor p de una binomial de 4 exitos y n = 20 y p= .5 

1 - dbinom(0,5,.1) # valor de pobabilidad de 1 o mas x

#estadisticos
sample1 %>%
  +   summarise(mu = mean(area), pop_med = median(area), 
                +             sigma = sd(area), pop_iqr = IQR(area),Z
                +             pop_min = min(area), pop_max = max(area),
                +             pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
                +             pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

#distribucion muestral
sample_means50 <- ames %>%
                              rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
                              summarise(x_bar = mean(area))


wald



p0  # Hipotesis nula   

p1 = x / n #proporcion muestral

W = ( p1 - p0) / sqrt((p0 * ( 1 - p0) ) / n)

S = ( p1 - p0) / sqrt((p1 * ( 1 - p1) ) / n) #Score

#agresti / coull interval

#cuando p es menor que .25 en muestras pequeñas y menor que .85 intermedio usar el normal en otro caso usar wald agregando 2 suscesos y dos fracasos

p <-  0.1
n <-  20
q <- 1 - p
va1 <- p * q
z <-  qnorm(.975)
z1 <- qnorm(.975) ^ 2
n1 <- n + z1
n2 <- n / n1
z3 <- z1 / n1
inv1 <- n / (n + z1)
pn <- p * n2
p1 <- pn + (.5 * z3)
z4 <- 1 / n1
inv <- va1 * inv1
sq <- sqrt(z4 * (inv + (.25 * z3)))
e <- (z * sq)
a1 <- p1 +  (c(1 , -1) * e)



#cuando los datos son pareados se toman ambos grupos como uno diferencia entre las muestras (solo una muestra)

#Simulacion
n <- 10000
pvals <- seq(.1, .9, by = .05)
nosim <- 100000
coverage <- sapply(pvals, function(p) {
  phats <- rbinom(nosim, prob = p, size = n) / n
  ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
  lu <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
  mean(ll < p & lu > p)
})

plot(coverage)
abline(h = .95)

#agresti

n <- 10000
pvals <- seq(.1, .9, by = .05)
nosim <- 100000
q <- 1 - pvals
va1 <- pvals * q
z <-  qnorm(.975)
z1 <- qnorm(.975) ^ 2
n1 <- n + z1
n2 <- n / n1
z3 <- z1 / n1
inv1 <- n / (n + z1)
pn <- pvals * n2
p1 <- pn + (.5 * z3)
z4 <- 1 / n1
inv <- va1 * inv1
sq <- sqrt(z4 * (inv + (.25 * z3)))
e <- (z * sq)

coverage <- sapply(pvals, function(p) {
  phats <- rbinom(nosim, prob = p, size = n) / n
  ll <- phats - e
  lu <- phats + e
  mean(ll < p & lu > p)
})

plot(coverage)
abline(h = .95)

#wilson

n <- 10000
pvals <- seq(.1, .9, by = .05)
nosim <- 100000

z <-  qnorm(.975)
z1 <- qnorm(.975) ^ 2
n2 <- n * 2
p1 <- pvals + (z1 / n2)
q <-  1 - pvals
s <-  pvals * q
n4 <- 4 * n
j <-  z1 / n4
s1 <- s + j
e <-  z * sqrt(s1 / n)
c1 <- z1/n
c <-  1 + c1 


coverage <- sapply(pvals, function(p) {
  phats <- rbinom(nosim, prob = p, size = n) / n
  ll <- phats - e / c
  lu <- phats + e / c
  mean(ll < p & lu > p)
})

plot(coverage)
abline(h = .95)

#agresti

n <- 10000
pvals <- seq(.1, .9, by = .05)
nosim <- 100000
coverage <- sapply(pvals, function(p) {
  phats <- (rbinom(nosim, prob = p, size = n) + 2)/ (n + 4)
  ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) /n)
  lu <- phats + qnorm(.975) * sqrt(phats * (1 - phats) /n)
  mean(ll < p & lu > p)
})

plot(coverage)
abline(h = .95)


#bootstrap
#Was invented in 1979 by a famous statistician named Ephron who's in the Stanford Statistics Department, easily one of the top statisticians in history.

#"pulling oneselves up from ones own bootstrap" ~Baron Machausen
set.seed(950728)

group1 <- matrix$group1
group2 <- matrix$group2
estadistico <- diff(mean(group1), mean(group2))
B <- 10000
n <- length(matrix$variable)
variable1 <- variable a remuestrear group1
variable2 <- variable a remuestrear group2

sampleMean <- rep(NA, 10000)
sampleMedian <- rep(NA, 10000)
sampleVar <- rep(NA, 10000)

BootstrapSamplesgroup1 <- matrix(sample(variable1, size = n*B, replace = T), nrow = n, ncol = B)
BootstrapSamplesgroup2 <- matrix(sample(variable2, size = n*B, replace = T), nrow = n, ncol = B)

for(i in 1:1000) {sampleMean[i] <- abs(mean(BootstrapSamplesm[1:length(manu$herb), i]) - mean(BootstrapSamples[1:length(digi$herb), i]))
sampleMedian[i] <- abs(median(BootstrapSamplesm[1:length(manu$herb), i]) - median(BootstrapSamples[1:length(digi$herb), i]))
sampleVar[i] <- abs(var(BootstrapSamplesm[1:length(manu$herb), i]) + var(BootstrapSamples[1:length(digi$herb), i]))}


sampleMean[i] <- abs(diff(mean(BootstrapSamplesgroup1[length(variable1)], i), mean(BootstrapSamplesgroup2[length(variable2)], i)))

sampleMean <- rep(NA, 10000)
sampleVar <- rep(NA, 10000)
for(i in 1:1000) {sampleMean [i] <-mean(rnorm(x, mean(x), sd(x)))
sampleVar [i] <- var(rnorm(x,mean(x), sd(x)))}

x <- rnorm(30)
sampleMean <- rep(NA, 1000)
sampleVar <- rep(NA, 1000)
for(i in 1:1000) {sampleMean [i] <-mean(rnorm(30))
                  sampleVar [i] <- var(rnorm(30))}
mediaFunc <- function(x, i)  { return(c(mean(x[i]), var(x[i])))}
bootMedia <- boot(x1, mediaFunc, 10000)
plot(bootMedia)
boot.ci(bootMedia)

resamples1 <- rep(NA, 1000) # vectores para que el boot 
resamples2 <- rep(NA, 1000)
for(i in 1:1000) {resamples1[i] <- lapply(1:100, function(i) sample(exl, replace = T))

                   resamples2[i] <- lapply(1:100, function(i) sample(inl, replace = T))
}    
r.mean1 <- sapply(resamples1, mean)
r.mean2 <- sapply(resamples2, mean)

x <- r.mean1 - r.mean2
mediaFunc <- function(x, i) { return(c(mean(x[i]), var(x[i])))} 
bootMedia <- boot(x, mediaFunc, 15000) # boot con 15000 replicas

ft <- as.data.frame(cbind(exf,inf))
bootMedia1 <- boot(ft, mediaFunc1, 100000)
valor_p <- sum(bootMedia$t>=bootMedia$t0)/bootMedia$R

mediaFunc1 <- function(x, i) { y <- tapply(x[i,1], x[i,2], mean)
c(y[1]-y[2], var(y))} # La funcion del boot


bs <- function(data, indices, formula) { d <- data[indices,]; fit <- lme(formula, data = d); return(coef(fit))}
results <- boot(data = mango, statistic = bs, R = 1000, formula = Largo ~ Ambiente, random = ~ 1 + Ambiente | Genotipo/ Hoja, correlation = corCAR1(form = ~ 1 | Genotipo / Hoja))
resultados <- boot(availableSample, Mediaboot, R = 10000)
boot.ci(results)

bs <- function(data,i){
        d<-data[i,]
        mod<-lme(Largo ~ Ambiente, random = ~ 1 + Ambiente | Genotipo/ Hoja, correlation = corCAR1(form = ~ 1 | Genotipo / Hoja),d)
        fixef(mod)
}


#monte carlo

#Using Markov chain Monte Carlo to examine the sex effect
# refit model using lmer and carry out mcmc
detach(package:nlme)
library(lme4)
turtle2a <- lmer(plasma~treat+sex+(1|subject), data=turtles)
# Markov chain Monte Carlo
out.mc <- mcmcsamp(turtle2a, n=10000)
# highest probability density intervals
HPDinterval(out.mc)


jav <- read_excel("C:/Users/Javi3/Downloads/irga.xlsx")
library(rjags)
see <- rt(100, 47, mean(exe))
sie <- rt(100, 47, mean(ine))
mc <- see - sie
model_string <- "model{
   for(i in 1:length(y)) {
     y[i] ~ dnorm(mu, tau)
   }
   mu ~ dnorm(0, 0.0001)
   sigma ~ dlnorm(0, 0.0625)
   tau <- 1 / pow(sigma, 2)
 }"

model <- jags.model(textConnection(model_string), data = list(y = mc), n.chains = 3, n.adapt = 10000)
update(model, 10000)
mcmc_samples <- coda.samples(model, variable.names=c("mu", "sigma"), n.iter = 20000)
plot(mcmc_samples)
summary(mcmc_samples)


#Bayesian
#codicional probability p(A|B) = p(A n B)/P(B)
#Bayes theorem is used to reves the direction of conditioning
#Bayes rule p(A|B) = p(B|A)p(A)/p(B|A)p(A) + P(B|Acomplemento)p(Acomplemento)
#como regla general las funcines con p como pbeta, pnorm son para ver la posterior y las funciones como d com dbinom o dpois son para
#la verosimilitud

#The typical situations where the Bayesian perspective may be necessary are the following:
#1.Models with many random effects
#2.Models using exotic probability distributions. Keep in mind that most probability models become exotic when coupled with random effects. For instance there currently are limited frequentist tools available for fitting negative binomial regressions with random effects.
#3.There are many parameters to estimate
#4.Models in which the estimated variances of some of the random effects are small and/or the number of replicates is small

#The only real problem from a practical standpoint with using Markov chain Monte Carlo in model estimation is that it exacerbates the temptation to toss parsimony out the window
#and to fit extremely complicated models. This temptation should be resisted at all costs!

#mean and Variance
#Each summary has its own interpretation: for example, the mean is the posterior expectation of the parameter, and the mode may be
#interpreted as the single ‘most likely’ value, given the data (and the model).

#We consider two basic interpretations that can be given to prior distributions.
#in the population interpretation, the prior distribution represents a population of possible parameter values, from which the θ of current interest has been drawn.

#In the more subjective state of knowledge interpretation, the guiding principle is that we must express our knowledge
#(and uncertainty) about θ as if its value could be thought of as a random realization from the prior distribution.

#

p <- seq(.1,.9, by = .01)
likelihood <- dbinom(4,20,p)
prior <- c(rep(.06,4),.52,rep(.06,4))
numerator <- prior * likelihood
denominator <- sum(numerator)
posterior <- numerator / denominator

#Valor p

x_1 <- x - 1 #para encontar los x mas extremos
pbinom(x_1,n,p, lower.tail = FALSE)

#MLE
#Binomial

L <- function(n, y, theta) {return(theta^y*(1 - theta)^(n - y))}
theta <- seq(.01, .99, by = .01)
plot(theta,L(400,72,theta))
LME <- L(400,72,theta)
LL <- function(n, y, theta) {return(y * log(theta) + (n - y) * log(1 - theta))}
LLME <- LL(400,72,theta)
LP1 <- function(n, x, theta) {return(-n * theta + log(theta) * x -  log(factorial(x)))}
LP <- function(n, x, theta) {return(-n * theta + log(theta) * x )}# es esta
LP <- LP(400,72,theta)
LG <- function(x, alfa, beta) return {alfa - 1 * x - (1 / beta) * x - n * alpha * log(beta) - n * log(gamma(alpha))}


#Elicitacion

#*Andrei Andreevitch Markov (1856-1922) pasó la mayor parte de su vida en San Petersburgo, ya que su pa-
#dre trabajaba en el departamento ruso de silvicultura. Fue estudiante y luego profesor en la Universidad de
#San Petersburgo. Político liberal, participó en las protestas contra el régimen zarista en la primera década del
#siglo XX . Aunque estaba interesado en muchos aspectos del análisis matemático, su trabajo más importante
#fue contribuir a establecer las bases de la teoría moderna de la probabilidad. Sus ideas, que darían lugar a lo
#que hoy conocemos como procesos de Markov, fueron motivadas por el deseo de dar una demostración rigu-
#rosa de la ley de los grandes números, y ampliar el campo de aplicaciones de esta ley. Tales ideas fueron pu-
#blicadas en una serie de artículos entre 1906 y 1912.


#Reference Prior
#We may be concerned that our prior has too much influence on the posterior distribution, and want to explore how sensitive the results are to our prior assumptions. 
#There may be cases where prior information is not available, 
#or you wish to present an objective analysis where minimal prior information is used. 
#Or perhaps you want to use the Bayesian paradigm to make probability statements about parameters without incorporating prior information.
#when priors goes to zero, prior sample size goes to zero, if we have no data prior variance goes to zero, the prior degrees of freedom goes to -1
#the posterior mean goes to the sample mean
#the posterior sample size goes to the observed sample size
#the posterior degrees of freedom goes to the sample degrees of freedom
#the posterior variance paremeter goes to the sample parameter
#this NormalGamma(0, 0, 0, -1) conjugate "prior" 
#if a prior for mu that is proportional to a constant or flat in the whole ral line
#and the prior of the variance is proportional to a 1/sig2
#However, would you use a uniform distribution for sigma squared, or a uniform distribution for the precision, which is 1 over sigma squared?
#Or perhaps a uniform distribution for sigma?
#These would all lead to different posteriors with little justification for any of them. 
#This ambiguity led Sir Harold Jeffreys to propose reference distributions for the mean and variance for situations where prior information was limited.
#These priors are invariant to the units of the data.
#Informative priors can provide more accurate information when data are limited 
#and the transparency of explicitly laying out prior assumptions is an important of reproducible research.

#Cauchy distribution
#we'll describe priors that are constructed as a mixture of conjugate priors. In particular, the Cauchy distribution
#In many situations, we may have reasonable prior information about the mean from mu, but are less confident in how many observations our prior beliefs are equivalent to. 
#when you have an idea for the prior mean but you don´t sure for the prior sample size 
#We can address this uncertainty in the prior sample size, through an additional prior distribution on a n_0 via a hierarchical prior. 
mu|sig2 ~ N(m0, sig2/n0)
n0|sig2 ~ Gamma(1/2, r2/2)
#the only way to obten the Cauchy distribution if by monte carlo simulations
mu|sig2 ~ C(m0, sig2*r2)
C(0, sig2*1) #Jeffrey-Zellner-Siow or JZS prior la mejor para hacer inferencia con la distribucion normal

#The Cauchy distribution does not have a mean or standard deviation,
#but the center or location and the scale play a similar role to the mean and standard deviation of the normal distribution.
#Cauchy priors were recommended by Sir Harold Jeffreys as a default objective prior for both estimation and testing. 

# initialize MCMC for Cauchy
sigma2[1] = 1# el unico que cambia
n_0 = 1
mu[1] = 1

#draw from full conditional distributions
for(1 in 1:S) {
	mu[i] = p_mu(sig2[i-1], n_0[i-1], m0, r, data)
	sig2[i] = p_sig2(mu[i], n_0[i-1], m0, r, data) #draw sig2 with the mu
	n_0[i] = p_sig2(mu[i], sig2[i], m0, r, data) #draw no with mu and sig2
}


#when the variance are not equal is the Behren-Fisher problem

#Bartlet paradox 
#The takeaway from this is that we cannot use improper priors with nr equal to zero, if we're going to test our hypothesis that mu equal n0.
#Similarly, vague priors that use a small value of n0 are not recommended due to the sensitivity of the results to the choice of an arbitrarily small value of n0. 
#This problem that arises with vague priors where the base factor favors the null model H 1 even when the data are far away from the value under the null are known as Bartlets or the Jeffres-Lindley's paradox.
n <- 300
lamda <- 1.5 # media de poisson
x <- 2 #numero de exitos
k <- 1.3 # gama
theta <- .5 #gama

#"Once again, our choice of the prior parameters ($a$ and $b$) should reflect our 
#prior beliefs about the parameter $\lambda$. In the case of the Gamma-Poisson 
#conjugacy, we can view $a$ as the number of total counts and $b$ as the prior 
#number of observations. For example, setting $a = 12$ and $b = 3$ reflects a 
#belief based on data that 3 respondents on average consume a total of 12 fruits 
#per day. At a first glance, this might sound equivalent to setting $a = 4$ and 
#$b = 1$ or $a = 120$ and $b = 30$, however these three distributions, 
#$Gamma(a = 4, b = 1)$, $Gamma(a = 12, b = 3)$, and $Gamma(a = 120, b = 30)$, 
#while they all have the same expected value 4, differ in their spreads which 
#indicates a different degree of belief about the parameter $\lambda$."

#gama(alfa, beta)
#alfa y beta se haya igualando la media alfa/ beta = al numero medio de poisson, igualmente con la desviasion raiz de alfa / beta con la desviasion poisson
#otra forma es escogiendo una vague prior gama(epsiolon,epsilon) siendo epsilon lo mas pequño posible
#Combinacion gamma-poisson

#para los de duke  la media poisson es mean = k*theta y las desviacion es igual a sd = sqrt(theta)*k, para los de duke alfa = k y lamda = theta

alfa = k + x
beta = beta/beta*n+1

k1 <- alfa + x #puede ser tambien alfa + x
theta <- lamda / (n * lamda + 1)#puede ser beta + n
#gamma(alfa + x, beta + n)
beta #efective sample size of prior
beta / beta + n # prior weight
n / n + beta #dta weight

media <- lamda * k # media de gamma o alfa + x/beta + n
varianza <- lamda^2 * k # var gamma

#qgamma(.975,alfa + x, beta + n) superior ...highest posterior density intervals
#qgamma(.025,alfa + x, beta + n) inferior ... of credible interval
#pgamma(cualquier numero i,alfa + x, beta + n) posterior 


#Exponencial- gamma
#exponenciall como cuando se quiere medir el tiempo promedio en que llega una autobus
#o el area promedio afectada por una bomba
#verosimilitud exponencia y prior gamma tiene una familia conjugada
#los para metros de la prior se sacan igualando 
#la razon entre ellos con la media prior  e.g: si se cree que el bus se demora en promedio 10 minutos
#la media lamda seria 1/10 y esta la igualamos con alfa/beta, 1/10 = alfa/beta 
#el par de numeros que escojamos tienen que cumplir la ecuacion
#la desviacion estadar de la prior es alfa/beta * 10
# alfa es el prior sample size
# gamma(a + 1, b + y) cuando solo se hace una observacion 
# gamma(a + n, b + y) la posterior donde "y" es la sumatoria de las observa
#a+1/b+y es la posterior mean

#Normal

# se pueden escoger otras prioris the Jeffreys-Zellner-Siow prior which is the Jeffreys prior for the unknown variance and a Cauchy prior for the mean

#Varianza Conocida
s <- 1 # aunque puse s es realmente sigma
mu <-rnorm(mean = 10, sd = 2, 5) #N ~ (v,t)
med_muestra <- 10.5
t <- 2
v <- 0
n <- 5
v1 <- ((v * s^2) + ((n * med_muestra) * t^2)) / (s^2 + (n * t^2)) #media de la posterior cuando w es  1
# s = sigma, mu ~ N(v, t), v = hyperparametro media de la media, t = hyperparametro varianza de la media, n = tamaño muestral y med_muestra = media muestral
# si algo antes para mi la prior es ~ N(m, s) pero lo cambia a la idea que es ~ N(v, t)



#tengo dos ecuaciones de la posterior mean
#posterior mean1 = (med_muestra *(n / n + (s^2/t^2))) + v * ((s^2/t^2) / n + (s^2/t^2))) 
#posterior mean2 = (((n*med_muestra)/s^2) + (v/t^2))/ ((n/s^2) + (1/t^2))

t1 <- sqrt(((s^2) * (t^2)) / (s^2 + n * t^2))  #varianza posterior
#tengo dos ecuaciones de la posterior mean
#desviacion posterior1 = sqrt(((s^2) * (t^2)) / (s^2 + n * t^2))
#desviacion posterior2 = sqrt(1 / ((n/s^2) + (1/t^2)))

#N ~ (v, t + s) predictive distribution


#Normal
m <- ((n * med_muestra / s^2) + (v / t^2))/ ((n/s^2) + (1 / t^2))
s1 <- 1 / ((n / s^2) + (1 / t^2))
#posterior mean = ((n / (n + (s^2 / t^2))) * med_muestra) + ((s^2 / t^2)/ (n + (s^2 / t^2)) * v)
#sample size effect = s^2 / t^2 

#Normal desconocida
# x|sigma^2,mu ~ N(mu,sigma^2)
# mu|sigma^2 ~   N(m, sigma^2/w)
#efective sample size = w
# w es una radio entre las variaciones s^2/su^2 donde s es la varicion de y y su es la variocion de la media
#por lo que es preciso elegir cuanta varicion queremos que tenga la muestra por sobre la muestra mu
#entonces de la ecuacion de mu ~ N(m, s^2/w)
#teniendo w = s^2/su^2, podemos decir que s^2/w es igual a su^2 *esto ultimi es deduccion mia puede estar errado


#sigma^2 ~ invgamma(alfa, beta)

#posterior

#p es parametros de la posterior

#Sigma Posterior = gamma(alphap, betap)
alphap = alfa + n/2 #alfa se puede ver como los grados de libertad v0 de duke y tiene que ver con el prior sample size equal to 2*alfa por eso se divide por 2
betap = beta + (.5 * sum((x - mean(x))^2)) + (((n * w)/(2*(n+w)))*((mean(x) - m)^2)) #beta es el prior guess the la varianza


#mu Posterior = norm(mup, s^2p)
mup = ((mean(x)*(n/ (n + w))) + m*(w/(n + w)))
s2P= sigma^2 / (n + w)

#en duke cuando se desconoce la varianza
#prior es NormalGamma(m0, n0, s02, v0)
# posterios es NormalGamma(m, nn, sigma2, v)
# x|sigma^2,mu ~ N(mu,s2)
# mu|sigma^2 ~   N(m, s2/n0) donde n0 se interpreta como the prior sample size
# sigma^2 ~ Gamma(v0/2, s02*v0/2)

#ng(0,0,0,-1) jefryes prior
m0 = 35
n0 = 25
s02 = 156.26
v0 = 24
med_muestra = 55.5
s2 = 540.7
n = 28 

nn <- n0 + n #Posterior sample size sum of no prior and the sample sample size
m = ((n*med_muestra) + (n0*m0))/nn  #posterio_mean weigthed average between the prior mena and the sample mean
# rnorm(100, mean = posterio_mean, sigma = s2/nn)


v = v0 + n #degrees of freedom increase with the sample size
sigma2 = (1/v) * (s02 * v0 + s2*(n - 1) + (n0*n/nn)*((med_muestra-m0)^2)) #posterior variace parameter combine tree sources of infromation about sigma
#in terms of sum square deviation
#the first term is the sample variance time the sample degrees of freedom s02 * v0
#the second term represents the prior sum squres s2*(n - 1)
#the third term is base on the square difference of the sample mean and the prior mean (n0*n/nn)*((med_muestra-m)^2)
#we then divide by posterior degrees of freedom 1/v
# rgama(100, v2, sigma2/2)

#Posterio distribution
phi <- rgamma(1000, shape = v/2, rate = sigma2*v/2)
sigma <- 1/sqrt(phi)
mean(sigma) #posterior mena of sigma
quantile(sigma, c(.975, .025))
mu <- rnorm(1000, m, sigma/sqrt(nn))
dis <- rnorm(1000, mu, sigma)

#the marginal ditribution of mu unconditional is a t-Student
media <- dt(x, v, m, sigma2/n) #lo mismo que una dt(v, 0, 1) = (mu - m)/ (sigma/sqrt(nn))

credible_interval = m + c(1, -1) * sqrt(sigma2/nn)*qt(.975, v)

#cachy prior que es la t student con un grado de libertad Herald Jeffreys
#bratet paradox as n0 rends to 0 the bayes factor favor the H1
#information paradox as the sample mean of diferrence are futher to . the bayes factor becomes constant malo para H2
phi = rgamma(10000, n-1/2, s2*(n-1)/2)
sigma = 1/sqrt(phi)
post_mu = rnorm(10000, mean = med_muestra, sd = sigma/sqrt(n)) #cuando la hipotesis es de la diferenica entre medias toca poner mu = 0
post_y = rnorm(10000, mean = post_mu, sd = sigma)

#ecuaciones finales
#posterior sigma = 1 / gamma(alfa + (n/2), beta + (.5 * sum((mean(x) - x)^2)) + (((n * w)/2(n+w)) + ((mean(x) - m)^2))
#mu = norm(((n * mean(x))+ w * m)) / (n + w)), (sigma^2 / (n + w))

#se puede usar un modelo jerarquico para ponerles prior a m, w, alfa y beta
#igualmente se puede extender a distribuciones multinomiales solo que seria con calculos vectoriales

z <- rgamma(1000, shape = 16.5, rate = 6022.9)# desviacion estandar de la prior s~gamma(alpha', beta')
sig2 <- 1/z #deve ser inverse gamma
mu <- rnorm(1000, mean = 609.3, sd = sqrt(sig2/27.1))#distribucion de la media mu~N(mu',s^2')


#non informative prior
beta(.5,.5) #efect in sample size is just one
beta(.001,.001)
beta(0,0)#es el limete, no es una porper density porque al integrag de 0 a 1 no tendriamos 1 sino infinito
#las prior como beta(0,0) se conocen como impropias
#una vague prio es N(0,1000000^2)
#cuando no se conoce la varianza se utiliza invg(0,0) como prior impropia
#posterior de la varainza cuando usas una prior impropia =gamma(a = (n-1)/2, b = .5*sum((y - mean(y))^2)

# prediccion

h <- .7
h1 <- .4
p <- .5

#beta-binomial
#la probabilidad posterior que el esyadistico sea mayor o igual a otro numero se saca asi pbeta(x, alfa*, beta*, lower.tail = F)
beta + alfa # efective sample size of  prior
alfa / (alfa + beta) # media prior
alfa + x / (alfa + beta +n) #media posterior
beta + alfa / (alfa + beta + n) #prior weight
alfa / (alfa + beta) # media prior
n / (alfa + beta + n) #data weight
x / n #data mean
# media posterior # prior weight * prior mean + data weight * data mean
#dos priors pueden tener las misma prior mena pero diferente pror weght like beta(2, 2) y beta(500, 500)


#la probabilidad posterior que el estadistico sea mayor o igual a otro numero se saca asi pbeta(x, alfa*, beta*, lower.tail = F), con
#la prior es lo mismo pero con los parametros del la prior
#exponencial - gamma
prior_mean <- 1/10 # se podria comparar con gamma(100,1000)
#posterior gamma(alfa +1, beta + x)
#alfa es el peso de la prior

#Posterior odds
#(p(Data|H1)/p(Data|H1)) * (p(H1)/p(H2)) 
#posterior odds = bayes factor * prior odds

#Inference
#H0: mu = m
#H1: mu ~ N(mu, s^2/n0)

#In the limit as n0(prior sample size) goes to 0 under this noninformative prior, the base factor paradoxically ends up favoring H1 regardless of the value of y bar.
#The takeaway from this is that we cannot use improper priors with nr equal to zero, if we're going to test our hypothesis that mu equal n0.

#Similarly, vague priors that use a small value of n0 
#are not recommended due to the sensitivity of the results to the choice of an arbitrarily small value of n0.

#This problem that arises with vague priors where the base factor favors the null model H1 even when the data are far away 
#from the value under the null are known as "Bartlets or the Jeffres-Lindley's paradox".



#factor[H1:H0] =  (med_muestra - mu)/(s^2/sqrt(n)),  (n + n0)/ n0 si n0 tiende a cero el cociente tiende a infinito
#dandole mayor soporte a n de la H0 lo que se conoce como Jefreys-Lindley paradox
#n0 para las pruebas serio 1 pero puede incrementar si opinas que el efecto es mayor
#bias in the ramdon numbers are a problem

#una solucion a la paradoja de informacion es la cauchy C(0, r2sigma2) default r is 1 but can be less if you espect less efect
#r < 1 if less efect if expected  

#Modelos Lineares

#segun duke

Y = alfa + beta*x + epsilon
#epsilon ~ N(0, s2)
#alfa|s2 ~ N(a0, s2*sa)
#beta|s2 ~ N(b0, s2*sb)
#1/s2 ~ G(v0/2, v0s02/2)

marginal
beta|y ~ t(mean(beta), sd(beta)^2)
alfa|y ~ t(mean(alfa), sd(alfa)^2)
alfa + beta*x|y ~ t((maen(alfa) + mean(beta)*x, sy2)
#the posterior probability seria pendiente*qt(.978, df) los df lo muestran al poner summary(modelo)
#la prediccion se puede hacer con la funcion predict(model, data.frame(X = x, Y = y,...), interval = "predict")

#BIC = -2*log(likelihood) + log(n) * parameters in the modelsmaller BIC is better
#BIC tends to selec parsimonius model and AIC might include terms that are not statistically significant

library(MASS)
library(BAS)
data(UScrime)
UScrime[,-2] <- log(UScrime[,-2])
crime.zs <- bas.lm(y ~., 
	               data = UScrime,
	               prior = "ZS-null",
	               modelprior = uniform(),
	               method = "MCMC")# BMA Bayesian MOdel Averaging
#MPM Median Probability Model includ all predctors with posterior > .5

diagnostics(crime.zs)
plot(crime.zs, wich = 1, add.smooth = F) #la diferencia es que se obtinen del ressiadual averaging
plot(crime.zs, wich = 2, add.smooth = F) #cumulative model probability adding up the model probabilities each time a new model is sampled
plot(crime.zs, wich = 3, add.smooth = F) #model size versus the log of marginal likelihood
plot(crime.zs, wich = 4, add.smooth = F) #importance of the each predictor
image(crime.zs, rotate = F) #muestra los 20 modelos con mayor soporte y los predictores 
coef.zs <- coef(crime.zs)
plot(coef.zs, subset = 5:6) #cuando tengas dudas entre predictores se escoge el que tenga menos dencidad en 0 en este plot


2*pnorm(-4)^n probabilidad de encontrar puntos mayores de 4sd de la media

While frequentist confidence intervals are often used in hypothesis tests, Bayesian credible intervals indicate a high probability region of the parameter space. Rejecting or accepting a hypothesis is done using a loss function or Bayes factor.

2*L + log(n)*k BIC

2^k numero de modelos por factores

Since the Zellner g-prior has a data-dependent covariance matrix, it helps put the prior variance on the same scale as the data, which is useful in calculating the posterior and eliciting the prior.

will also use the `broom` package to turn regression outputs to tidy data frames 
to help with diagnostic plotting.

We will use the `stepAIC` function from the 
`MASS` package for model selection using step-wise selection using BIC.

The 
`bas.lm` function from the `BAS` package later in the lab to implement Bayesian 
Model Averaging.

tidy(m1)

ggplot(data = wage, aes(x = iq, y = wage)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

confint(m1)

The `augment` function in the `broom` 
package is going to come in handy here as it takes in a model object (the output 
of an `lm`) and returns a data frame with columns correspinding to variables 
in the model as well as predicted values (`.fitted`), residuals (`.resid`), and 
standardized residuals (`.std.resid`), along with a few others.

maun <- augment(m2)
ggplot(data = maun, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals")


We set the `alpha` level 
of our points to a value lower than 1 (`0.6` to be precise) in order to add 
plot the points with some transparency. This will allow us to more easily 
identify where the points are more dense vs. more sparse. Then, we overlay a 
horizontal dashed line at $y = 0$ (to help us check whether residuals are 
distributed evenly around 0 at each fitted value), and we also adjust the axis 
labels to be more informative.

ggplot(data = maun, aes(x = .resid)) +
  geom_histogram(binwidth = 100) +
  xlab("Residuals")

ggplot(maun) +
  geom_qq(aes(sample = .std.resid)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals")

One way to accommodate the right-skewness in the residuals is to (natural) 
log-transform the dependent variable. Note that this is only possible if the 
variable is strictly positive, since the log of negative value is not defined 
and log(0) = -inf

Let's use the `Bayes.outlier` function from `BAS`, to calculate 
these probabilities for the model  `m_lwage_iq` and plot them against the 
case number.

outliers <- Bayes.outlier(m2)

outliers_df <- data.frame(probability = outliers$prob.outlier,
                          case = 1:length(outliers$prob.outlier))
ggplot(outliers_df, aes(ymax = probability, x = case)) +
  geom_linerange(ymin = 0) +
  labs(y = "Probability")

To identify which cases have probabilities greater than 0.50 of being an outlier, 
we can use the `filter` function to return which cases have `probability > 0.50`.

outliers_df %>%
  filter(probability > 0.50)

While being $3$ standard deviations seems like an extremely unlikely event for 
a single observation, for large sample sizes, there is often a rather high 
probability that there will be at least one error

# prob of a case being an outlier:
#   being below or above 3 standard deviations from 0
(prob_outlier <- pnorm(-3) + pnorm(3, lower.tail = FALSE))

# probability of a signle case not being an outler is therefore the complement 
(prob_not_outlier <- 1 - prob_outlier)

# probability of no outliers in the sample of n assuming errors are independent a priori
n <- nrow(wage)
(prob_no_outliers <- prob_not_outlier^n)

# probability of no outliers in the sample of n
1 - prob_no_outliers

So instead of fixing the number of standard deviations to k = 3
an alternative 
is fix the prior probability of there being no outliers in the sample,

n <- nrow(wage)
(prob_obs_not_outlier <- 0.95^(1/n))
(newk <- qnorm(0.5 + 0.5 * prob_obs_not_outlier))

mfull <- lm(lwage ~ . - wage, data = wage) #con ese punto se analizan todos los factores

The use of `. - wage` in the `lm` function tells R to include all covariates in 
the model except the `wage` variable from the data set.

However, running this full model has a cost: we will remove observations from 
our data if some measurements in the variables (e.g. birth order, mother's 
education, and father's education) are missing. By default, the `lm` function 
does a complete-case analysis. So it removes any observations with a missing 
(`NA`) value in one or more of the predictor variables. 

BIC(mfull)

Bayesian model averaging (BMA), 
in which multiple models are averaged to obtain posteriors of coefficients and 
predictions from new data. Dr. Merlise Clyde is the primary author of the R 
package `BAS`, which implements BMA [@Clyde2018]. We can use this for either 
implementing BMA or selecting models.

bm <- bas.lm(lwage ~ . -wage, data = na.omit(wage),
                   prior = "BIC", 
                   modelprior = uniform())

summary(bma) #mejores 5 modelos

It is also possible to visualize the posterior distribution of the coefficients 
under the model averaging approach. We graph the posterior distribution of the 
coefficients of `iq` and `sibs` below. Note that the subset command dictates 
which variable is plotted.

coef <- coefficients(bma)

plot(coef, subset = c(3,13), ask = FALSE) #plot distribution per factor
confint(coef)

For Questions 9-10, we'll use a reduced data set which excludes wage, number 
of siblings, birth order, and parental education.

wage_red <- wage %>%
  select(-wage, -sibs, -brthord, -meduc, -feduc)

Let's use BMA with the Zellner-Siow prior on the regression coefficients:

bmared <- bas.lm(lwage ~ ., data = wage_red,  
                        prior = "ZS-null",
                        modelprior = uniform())

coef(bmared)
coef(bmared) %>%
  confint()

round((exp(confint(coef(bmared), parm='urban1'))[1] - 1)*100, 2)

Similar to last week's lab, we will be using Bayesian predictive distribution 
for predictions and interpretation of predictions. Simulation is used in `BAS` 
to construct predictive intervals with Bayesian Model Averaging, while exact 
inference is often possible with predictive intervals under model selection.

BPM <- predict(bma, estimator = "BPM", se.fit = TRUE)
variable.names(BPM)

*Median Probability Model* (`MPM`)
MPM <- predict(bma, estimator = "MPM")
variable.names(MPM)

[1] "Intercept" "hours"     "iq"        "kww"       "educ"      "exper"    
 [7] "tenure"    "age"       "married1"  "urban1"    "meduc" 

 variable.names(MPM_pred_lwage)
 [1] "Intercept" "hours"     "iq"        "educ"      "exper"     "tenure"   
 [7] "age"       "married1"  "urban1"    "meduc"    

# Find the index of observation with the largest fitted value
opt <- which.max(BPM$fit)

# Extract the row with this observation and glimpse at the row
wage %>% na.omit() %>%
  slice(opt) %>%
  glimpse()

A 95% credible interval for predicting log wages can be obtained by

ci <- confint(BPM, parm = "pred")
ci[opt,]

To translate this back to `wage` (recall that we regress `lwage`), we may 
exponentiate the interval to obtain a 95% prediction interval for the wages of 
an individual with covariates at the levels of the individual specified by `opt`.

exp(ci[opt,])


#Linear Model



#las´priors para las pendientes betas son generalmente la distribucion normal pero aveces se escoge la distribucion de laplace
#la priori de laplace tiene la distribucion igual beta = 1/2*exp(-abs(beta)) le dicen doble exponelcial porque su grafica se parece
#a la distribucion exponencia pero con el reflejo en la parte negativa de los x entonces esta en la parte postiva y la parte negativa
# tiene los picos alrededor del cero por lo que favorecera valores cercanos a cero se relaciona con una tecnica de regresion conocida como LASSO
library(car)
data(Leinhardt)
Leinhardt$loginfant <- log(Leinhardt$infant)
Leinhardt$logincome <- log(Leinhardt$income)
lmod <- lm(loginfant ~ logincome, Leinhardt)
summary(lmod)#posterior mean stimates
dat <- na.omit(Leinhardt)
#jags
library(rjags)

#Steps in running a model

1. Definition of the model
2. Compilation
3. Initialization
4. Adaptation and burning
5. Monitoring

#Compilation
line_data <- list("x" = c(1, 2, 3, 4, 5),
"Y" = c(1, 3, 3, 3, 5),
"N" = 5)
#In this example, data values are also supplied for the outcome variables Y[1] to Y[5].
#The parameters of the model are the three stochastic nodes for which no data are supplied:
#alpha, beta and tau. JAGS will generate samples from the posterior distribution of these
#parameters given Y.

#modelo base del manual
"model {
for (i in 1:N) {
Y[i] ~ dnorm(mu[i], tau)
mu[i] <- alpha + beta * (x[i] - x.bar)
}
x.bar <- mean(x)
alpha ~ dnorm(0.0, 1.0E-4)
beta ~ dnorm(0.0, 1.0E-4)
sigma <- 1.0/sqrt(tau)
tau ~ dgamma(1.0E-3, 1.0E-3)
}"

#Initialization
#Before a model can be run, it must be initialized. There are three steps in the initialization of a model
1. The initial values of the model parameters are set.
2. A Random Number Generator (RNG) is chosen for each parallel chain, and its seed is
set.
3. The Samplers are chosen for each parameter in the model.

#The format for initial values is the same as the one used for data (i.e. a list using the R interface or a separate file when using the command line interface).

#Random Number Generators
There are four RNGs supplied by the base module in JAGS with the following names:
"base::Wichmann-Hill"
"base::Marsaglia-Multicarry"
"base::Super-Duper"
"base::Mersenne-Twister"

list(".RNG.name" = "base::Wichmann-Hill",
".RNG.seed" = 314159,
...)

Here we use ... to denote that the same list includes initial values for the parameters (not
shown).

If no RNG names are supplied, then RNGs will be chosen automatically so that each
chain has its own independent random number stream. The exact behaviour depends on
which modules are loaded. 




#we use mcmc when is dificult to simulate independent drows
#when the acceptance ratio(good is between 0.23 and 0.51) is to low you should decrease the deviation

#defoult for JAGS

#1. Specify the model
library(rjags)

#we specify the hieraquical structure of the model to varible tha generaly call "mod_string"

mod_string = "model {
	for (i in 1:n){
	y[i] ~ dnorm(mu, 1.0/sig2) # in JAGS we use the precition wich is the reciprocal of the variace
	}
	mu ~ dt(0, 1.0/1.0, 1)
	sig2 = 1.0 
}"

#2. Set up the model
set.seed(50)

#what the data are and what parameters are
##data
y <- c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
n <- length(n)

##and provide intial values whit the same name for the model specification
data_jags <- list(y = y, n = n)

##and params for the model
params <- c("mu")

##to git JAGS initial values we write a function like below

inits <- function() {
	inits = list("mu" = 0) #ramdon initial value
}

##finaly we compile the model itself

mod <- jags.model(textConnection(mod_string), data = data_jags, inits = inits) #initialize the model

#3. Run the MCMC sampler

update(mod, 500) #runs MCMC sampler for 500 iterations without saving

mod_sim <- coda.samples(model = mod, variable.names = c("mu"), n.iter = 1000) #runs the model and keep our simulations

#before fit the model is necesary to check de parameters of the model
#q es igual a 1 porque solo hay dos modelos fair or load


#4. Post processing
library(coda)
plot(mod_sim)
summary(mod_sim)

#https://d18ky98rnyall9.cloudfront.net/_adadc80290e52a99b282ca9d7c1a41ee_background_MarkovChains.html?Expires=1592265600&Signature=GOwT05BPPrc06y6NRKWZPIC5O8bKpte36WRd288IrrOsbdvnRo-aVLhJwqA-kAiI5Kmhn-wh7ZbVCcATOmVYIH0nrTLd8whaOSM8-kPsWdk8Ts8LT7Y3ZNHZPOmSED8Rua6HqlZPQICWhr9jsqECegN3OdWTLrkBPMlDeOwqDLk_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A
##Markov Chainr

#how to know if the chain converge to a stationary distribution?
#is dicult to answer

#Auticorrelation is a number between -1, 1
#dependent of how lineary dependent the current value of the chain is to past values
#we can see the autocorrelation wit 
autocorr.plot() #function in coda package
#and we can see the values with 
autocorr.diag() 

autocorr.diag(mod, lag.max = 500) 
#Auticorrelation is important becouse tell us how much information is availible on markov chain

 
effectiveSize() #with function in coda pakage we can know the effective sample size

#the number we need to produce a confident interval can calculate we the Raftery and Luis diagnostic
raftery.diag(mod)

#https://d18ky98rnyall9.cloudfront.net/_dd6d312e631a80339ba1627e1d72b42d_Autocorrelation.pdf?Expires=1592265600&Signature=I62AW6uTjXioFVB5woCfp7bVEsEr9RNK2oS6tgrHX33c1aPCMlQNX-gNEnzmpXrD3FcFbxb3dhmiMXUywzOdSimCF4fEjx3ldmLOsBjd27iIE6DEOSx6GjiLmu~RWkr8qPMY1siPD-EyN1c7R-tKo4DGzVrOlb3NbBX~FndQNyI_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A

#gelman and ruben diagnostic tell us if the chains converge
#calculate the variability within chains and compares that with variability between de chains

gelman.diag() #with function if the variability between de chain is similar to the variability within echa chain the C.I is near to 1 and the diferent chain converge

gelman.plot() #tell us how change the vaule of statistic with the number of iterations

#start with the model string
mod1_string <- "model {
  for(i in seq_along(n)) {#for para la media
    y[i] = dnorm(mu[i], prec) $prec = precision
    mu[i] = b[1] + b[2]*log_income[i]
  }
  for(j in 1:2){ #para cada parametro
  b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
  sig2 = 1/prec #Desviacion
  sig = sqrt(sig2)

}"
#con las distribucion t student para modelar mejor los datos extremos
mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dt( mu[i], tau, df )
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    df = nu + 2.0 # we want degrees of freedom > 2 to guarantee existence of mean and variance
    nu ~ dexp(1.0)
    
    tau ~ dgamma(5/2.0, 5*10.0/2.0) # tau is close to, but not equal to the precision
    sig = sqrt( 1.0 / tau * df / (df - 2.0) ) # standard deviation of errors
} "

mod2_string = " model {
    for (i in 1:length(y)) {
        s[i] ~ dt( mu[i], tau, df )
        mu[i] = b[1] + b[2]*a[i] 
    }
    
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    df = nu + 2.0 # we want degrees of freedom > 2 to guarantee existence of mean and variance
    nu ~ dexp(1.0)
    
    tau ~ dgamma(5/2.0, 5*10.0/2.0) # tau is close to, but not equal to the precision
    sig = sqrt( 1.0 / tau * df / (df - 2.0) ) # standard deviation of errors
} "

set.seed(72)
data1_jags <- list(y = dat$loginfant, n = nrow(dat), log_income = dat$logincome)
params <- c("b", "sig") #parametros para beta y la desviacion
inits1 <- function() {
      inits = list("b" = rnorm(2, 0.0, 100.0), "prec" = rgamma(1, 1.0, 1.0))
}
#To run a model with the rjags package, we first create a model object with the jags.model()function.
mod1 <- jags.model(textConnection(mod1_string), data = data1_jags, inits = inits1, n.chains = 3)
#When a JAGS model is compiled, the model description is combined with the data to create 
#a virtual graphical model, which is a representation of the model as a directed acyclic graph (DAG) in computer memory.
update(mod1, 1000) #quemar

#The object created by jags.model is of class “jags”. It is an R interface to the virtual
#graphical model created by JAGS and does not contain any samples.
#To get samples from the posterior distribution of the parameters, we use the coda.samples
#function after first using the update function to run the Markov Chain for a burn-in period
#of 1000 iterations.

mod1_sim <- coda.samples(model = mod1, variable.names = params, n.iter = 5e3) #simulaciones
mod1_csim <- do.call(rbind, mod1_sim) #combinar las cadenas
plot(mod1_sim)
gelman.diag(mod1_sim) #te dice cuantos paramtros se deberia tener si hay convergencia
autocorr.diag(mod1_sim)# si hay auto correlacion entre las simulaciones
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)#numero efectivo de simulaciones si quieres tener un posterior interval se debe tener un tamaño efectivo grande 3000 de cada parametro

#The samples object is of class mcmc.list defined by the coda package [Plummer et al., 2006].

#It contains monitored samples of the variables alpha, beta, and sigma for the two parallel

#chains.
summary(mod1_sim)#posteror means and posterior interval for each parameter
summary(lmod)#Para comparar con el metodo frecuentista
dic.samples(mod1, n.iter = 1e3)#mientras mas pequeña mas soporte el modelo deviance information criterion

###Residual
X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income)
head(X)
(pm_params1 = colMeans(mod1_csim)) # posterior mean
yhat1 = drop(X %*% pm_params1[1:2]) #Predictores
resid1 = data1_jags$y - yhat1 #Residuals
plot(resid1) # against data index
plot(yhat1, resid1) # against predicted values
qqnorm(resid1) # checking normality of residuals
shapiro.test(resid1)

#Anova
data("PlantGrowth")
mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]]) #grp es importante
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
        prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
        sig[j] = sqrt(1.0 / prec[j])
    }
    
    
}"


set.seed(82)
str(PlantGrowth)
data_jags = list(y = PlantGrowth$weight, 
              grp = as.numeric(PlantGrowth$group)) #toca ponerlo numerico

params = c("mu", "sig")

inits = function() {
    inits = list("mu" = rnorm(3,0.0,100.0), "prec" = rgamma(3,1.0,1.0))
}

mod <- jags.model(textConnection(mod_string), data = data_jags, inits = inits, n.chains = 3)
update(mod, 1e3)

mod_sim <- coda.samples(model = mod,
                        variable.names = params,
                        n.iter = 5e3)
mod_csim <- as.mcmc(do.call(rbind, mod_sim)) # combined chains
plot(mod_sim)
gelman.diag(mod_sim) #te dice cuantos paramtros se deberia tener si hay convergencia
autocorr.diag(mod_sim)# si hay auto correlacion entre las simulaciones
autocorr.plot(mod_sim)
dic.samples(mod, n.iter = 1e5)#mientras mas pequeña mas soporte el modelo deviance information criterion

effectiveSize(mod_sim)#numero efectivo de simulaciones si quieres tener un posterior interval se debe tener un tamaño efectivo grande 3000 de cada parametro
pm_params <- colMeans(mod_csim)
pm_params
muh <- pm_params[1:3][data_jags$grp]
sdh <- pm_params[4:6][data_jags$grp]
resid <- data_jags$y - muh
qqnorm(resid)
qqline(resid)
plot(muh, resid,xlab = "Predicted", ylab = "Residual")
HPDinterval(mod_csim)
HPDinterval(mod_csim, 0.9)
mean(mod_csim[, 3] > 1.1*mod_csim[, 1]) #probabilidad que el grupo 3 cresca 10% mas que el grupo 1


mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec) #grp es importante
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
        
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt(1.0 / prec)
    
}"

inits2 = function() {
    inits = list("mu" = rnorm(3,0.0,100.0), "prec" = rgamma(1,1.0,1.0))
}

mod2 <- jags.model(textConnection(mod2_string), data = data_jags, inits = inits2, n.chains = 3)
update(mod2, 1e3)

mod2_sim <- coda.samples(model = mod2,
                        variable.names = params,
                        n.iter = 5e3)

mod2_csim <- as.mcmc(do.call(rbind, mod_sim)) # combined chains
summary(mod2_sim)
summary(mod_sim)

plot(mod2_sim)
gelman.diag(mod2_sim) #te dice cuantos paramtros se deberia tener si hay convergencia
autocorr.diag(mod2_sim)# si hay auto correlacion entre las simulaciones
autocorr.plot(mod2_sim)
dic1 <- dic.samples(mod, n.iter = 1e5)#mientras mas pequeña mas soporte el modelo deviance information criterion
dic2 <- dic.samples(mod2, n.iter = 1e5)#mientras mas pequeña mas soporte el modelo deviance information criterion
dic2 - dic1
HPDinterval(mod2_csim)

mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)



#Logistic

#Bernuli
library(boot)
data(urine)
str(urine)
dat <- na.omit(urine) #es necesarios quitar los NAs
#variable selection
#one way is choose aprior tha favoirs the values near 0 (Lapplace prior or doble exponetial) so the coeficients near cero indicate a weak relashionship (LASSO)

pairs(dat)
X <- scale(dat[, -1], center = T, scale = T) # centra los datos continuos restando por la media y dividiendo por la sd solo se puede con varibles cuantitativas
colMeans(X)
apply(X, 2, sd)

mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i]) 

        logit(p[i]) = int + b[1]*gravity[i] + b[2]*ph[i] + b[3]*osmo[i] + b[4]*cond[i] + b[5]*urea[i] + b[6]*calc[i]
    }
    
    for (j in 1:6) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
        
    }
    
    int ~ dnorm(0.0, 1.0/25.0)
    for (j in 1:6) {
    	b[j] ~ ddexp(0.0, sqrt(2.0)) #has variance 1.0
    }
    
}"



params <- c("int", "b") #parametros para beta y la desviacion

data_jags1 <- list(y = dat$r, gravity = X[,"gravity"], ph = X[,"ph"], osmo = X[,"osmo"], cond = X[,"cond"], urea = X[,"urea"], calc = X[,"calc"])

mod1 <- jags.model(textConnection(mod1_string), data = data_jags1, n.chains = 3)
update(mod1, 1e3)

mod1_sim <- coda.samples(model = mod1,
                        variable.names = params,
                        n.iter = 5e3)

mod1_csim <- as.mcmc(do.call(rbind, mod1_sim)) # combined chains
summary(mod1_sim)
plot(mod1_sim)
gelman.diag(mod1_sim) #te dice cuantos paramtros se deberia tener si hay convergencia
autocorr.diag(mod1_sim)# si hay auto correlacion entre las simulaciones
autocorr.plot(mod1_sim)
dic1 <- dic.samples(mod1, n.iter = 1e3)#mientras mas pequeña mas soporte el modelo deviance information criterion

par(mfrow = c(3, 2))
densplot(mod1_csim[, c(1:6)], xlimit = c(-3, 3))

mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i]) 

        logit(p[i]) = int + b[1]*gravity[i] + b[2]*cond[i] +  b[3]*calc[i]
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
        
    }
    
    int ~ dnorm(0.0, 1.0/25.0)
    for (j in 1:3) {
    	b[j] ~ ddexp(0.0, sqrt(2.0)) #has variance 1.0
    }
    
}"



params <- c("int", "b") #parametros para beta y la desviacion

data_jags1 <- list(y = dat$r, gravity = X[,"gravity"], cond = X[,"cond"], calc = X[,"calc"])

mod1 <- jags.model(textConnection(mod1_string), data = data_jags1, n.chains = 3)
update(mod1, 1e3)

mod1_sim <- coda.samples(model = mod1,
                        variable.names = params,
                        n.iter = 5e3)

mod1_csim <- as.mcmc(do.call(rbind, mod1_sim)) # combined chains
summary(mod1_sim)
plot(mod1_sim)
gelman.diag(mod1_sim) #te dice cuantos paramtros se deberia tener si hay convergencia
autocorr.diag(mod1_sim)# si hay auto correlacion entre las simulaciones
autocorr.plot(mod1_sim)
dic1 <- dic.samples(mod1, n.iter = 1e3)#mientras mas pequeña mas soporte el modelo deviance information criterion

densplot(mod1_csim[, c(1:3)], xlimit = c(-3, 3))

pm_params <- colMeans(mod1_csim)
pm_params #como escalamos valores de 0 corresponden a valores promedio

#por lo que cuando los valores de las xi son cero o estan en el promedio el valor sera igual al del intercepto
1.0/1.0 + exp(pm_params[4]) #prediccion de la probabilidad para valores medios en bernuli

prediccion <- c(0, -1, 1, 1) # esta persona primero tiene un valor estandar, el segun es una desviacion estardar abajo del promedio 
#y el tercero es una desviacion arriba del promedio de la tercera variable

#prediccion de la probabilidad para valores medios en bernuli

pm <- pm_params["int"] + X[, c(1, 4, 6)] %*% pm_params[1:3] #Modelo para predicciones
phat <- 1.0/(1.0 + exp(-pm)) #predicciones

plot(phat, jitter(dat$r))

t0.5 <- table(phat > .5, dat$r) #modelo donde mayores que .5 son un 1

sum(diag(t0.5))/sum(t0.5) #probabilidada de aciertos

t0.3 <- table(phat > .3, dat$r) #modelo donde mayores que .3 son un 1

sum(diag(t0.3))/sum(t0.3) #probabilidada de aciertos

library(MASS)
data(OME)
# Fit logistic curve from p = 0.5 to p = 1.0
     fp1 <- deriv(~ 0.5 + 0.5/(1 + exp(-(x-L75)/scal)),
                  c("L75", "scal"),
                  function(x,L75,scal)NULL)
     nls(Correct/Trials ~ fp1(Loud, L75, scal), data = OME,
         start = c(L75=45, scal=3))
      OMEi.nls <- nlsList(Correct/Trials ~ fp2(Loud, L75) | UIDn,
        data = OMEi, start = list(L75=45), control = list(maxiter=100))
     options(show.error.messages = TRUE, OutDec=dec)
     tmp <- sapply(OMEi.nls, function(X)
                   {if(is.null(X)) NA else as.vector(coef(X))})
     OMEif <- data.frame(UID = round(as.numeric((names(tmp)))),
              Noise = rep(c("coherent", "incoherent"), 110),
              L75 = as.vector(tmp), stringsAsFactors = TRUE)
     OMEif$Age <- OME$Age[match(OMEif$UID, OME$UID)]
     OMEif$OME <- OME$OME[match(OMEif$UID, OME$UID)]
     OMEif <- OMEif[OMEif$L75 > 30,]
     summary(lm(L75 ~ Noise/Age, data = OMEif, na.action = na.omit))
     summary(lm(L75 ~ Noise/(Age + OME), data = OMEif,
                subset = (Age >= 30 & Age <= 60),
                na.action = na.omit), cor = FALSE)


#We must also indicate how many trials were run for each experiment using the \tt weightsweights argument.
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)

plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)

X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)


mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

#Herarchical linear model
mod_string = "model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	for (j in 1:max(ID)) {
		a[j] ~ dnorm(mu0, tau0)

	}
	
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}

mu0 ~ dnorm(0, 1.0/1.0e2)
tau0 ~ dgamma(1.0/2.0, 1.0/2.0)	
sigID = sqrt(1.0/tau0)
}"

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID))
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
data_jags$ID = dat$ID
str(data_jags) # make sure that all variables have the same number of observations (712).

params <- c("a", "sigID","b") #parametros para beta y la desviacion


mod <- jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod, 1e3)

mod_sim <- coda.samples(model = mod,
                        variable.names = params,
                        n.iter = 5e3)

mod_csim <- as.mcmc(do.call(rbind, mod_sim)) # combined chains
summary(mod_sim)
raftery.diag(mod_csim) #In the Gelman and Rubin diagnostic, large scale reduction factors indicate that the different chains are not exploring the same space yet and that we need more burn-in time. This question is about interpreting the Raftery and Lewis diagnostic.


#poisson
#como no se puede coger el log de 0 toca eliminar los ceros antes de hacer el modelo les puedes suma 0.1 a todos los datos de la variable de respuesta
library(COUNT)
data(badhealth)
mod_string = " model {
	for (i in 1:length(numvisit)) {
		numvisit[i] ~ dpois(lam[i])
		log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
	}
	
	int ~ dnorm(0.0, 1.0/1e6)
	b_badh ~ dnorm(0.0, 1.0/1e4)
	b_age ~ dnorm(0.0, 1.0/1e4)
	b_intx ~ dnorm(0.0, 1.0/1e4)

}"

set.seed(102)
data_jags <- as.list(badhealth)
params <- c("int", "b_badh","b_age", "b_intx")
mod <- jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod1, 1e3)

mod_sim <- coda.samples(model = mod,
                        variable.names = params,
                        n.iter = 5e3)

mod_csim <- as.mcmc(do.call(rbind, mod_sim)) # combined chains
summary(mod_sim)
raftery.diag(mod_csim) #In the Gelman and Rubin diagnostic, large scale reduction factors indicate that the different chains are not exploring the same space yet and that we need more burn-in time. This question is about interpreting the Raftery and Lewis diagnostic.

##Residuals
X <- as.matrix(badhealth[,-1])
X <- cbind(X, with(badhealth, age*badh))
para <- apply(mod_csim, 2, median) #in this case we choose the median
predicciones <- para["int"] + X %*% para[c("b_badh", "b_age", "b_intx")]
lam <- exp(predicciones)
resid <- badhealth$numvisit - lam
plot(resid)
var(resid[badhealth$badh == 0])
var(resid[badhealth$badh == 1])

#overdispersed data we negative binomial distribution
x1 <- c(0, 35, 0)
x2 <- c(1, 35, 35)

loglam1 <- mod_csim[,"int"] + mod_csim[,c("b_badh", "b_age", "b_intx")] %*% x1 #montecarlo samplem for  the data
loglam2 <- mod_csim[,"int"] + mod_csim[,c("b_badh", "b_age", "b_intx")] %*% x2
lam1 <- exp(loglam1)
lam2 <- exp(loglam2)
plot(density(lam1))
plot(density(lam2))

#para compara un poco de mas simulaciones
y1 <- rpois(15000, lam1)
y2 <- rpois(15000, lam2)
mean(y2 > y1)

plot(table(factor(y1, levels = 0:18))/15000)
points(table((y2 + 0.1))/15000, col = "red")

dic <- dic.samples(mod, n.iter = 1e3)#mientras mas pequeña mas soporte el modelo deviance information criterion

mod_string1 = " model {
	for (i in 1:length(numvisit)) {
		numvisit[i] ~ dpois(lam[i])
		log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] 
	}
	
	int ~ dnorm(0.0, 1.0/1e6)
	b_badh ~ dnorm(0.0, 1.0/1e4)
	b_age ~ dnorm(0.0, 1.0/1e4)
	

}"

set.seed(102)
data_jags1 <- as.list(badhealth)
params1 <- c("int", "b_badh","b_age")
mod1 <- jags.model(textConnection(mod_string1), data = data_jags1, n.chains = 3)
update(mod1, 1e3)

mod_sim <- coda.samples(model = mod1,
                        variable.names = params1,
                        n.iter = 5e3)

mod_csim <- as.mcmc(do.call(rbind, mod_sim)) # combined chains
dic1 <- dic.samples(mod1, n.iter = 1e3)#mientras mas pequeña mas soporte el modelo deviance information criterion


data_jags1 <- read.csv("calls.csv")
data_jags1 <- as.list(data_jags1)
mod_string1 = " model {
for (i in 1:length(calls)) {
		calls[i] ~ dpois(days_active[i] * lam[i])
		log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
	}
	b0 ~ dnorm(0.0, 1.0/10e2)
	for (j in 1:2) {
		b[j] ~ dnorm(0.0, 1.0/1e2)
	}
}"

params1 <- c("b0", "b")
mod1 <- jags.model(textConnection(mod_string1), data = data_jags1, n.chains = 3)
update(mod1, 1e3)

para <- colMeans(mod_csim)
predicciones <- para[3] + 3 %*% para[c(1:2)]
lam <- exp(-predicciones)


mod_sim <- coda.samples(model = mod1,
                        variable.names = params1,
                        n.iter = 5e3)

mod_csim <- as.mcmc(do.call(rbind, mod_sim)) # combined chains
dic1 <- dic.samples(mod1, n.iter = 1e3)#mientras mas pequeña mas soporte el modelo deviance information criterion

#prior predictive

n_sim <- 500
alfapri <- rpois(n_sim, rate = 1.0/2.0) #alfa simulation
betapri <- rpois(n_sim, rate = 5.0) #beta simulation
mupri <- alfapri/betapri #prior mean simulation
sigpri <- sqrt(alfapri/betapri^2) #prior sd simulation
summary(mupri)
summary(sigpri)

lampri <- rgama(n_sim, alpri, betapri) #simulation of the lamda
summary(lampri)

ypri <- rpois(n_sim, lampri) #simulation of the y
summary(ypri)


library(rjags)
mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec) #grp es importante
    }
    
    for (j in 1:5) {
        mu[j] ~ dnorm(mu0, 1)
        
    }
    
    mu0 ~ dnorm(0, 1.0/1.0e6)
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt(1.0 / prec)
    
}"



mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec) #grp es importante
    }
    
    for (j in 1:5) {
        mu[j] ~ dnorm(mu0, tau0)
        
        sig0 = sqrt(1.0/tau0)
    }
    
    mu0 ~ dnorm(0, 1.0/1.0e6)
    tau0 ~ dnorm(0, 1.0/1.0e6)
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt(1.0 / prec)
    
}"


params <- c("mu", "mu0", "prec", "sig0") #parametros para beta y la desviacion
data_jags = as.list(dat)
mod <- jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod, 1e3)

mod_sim <- coda.samples(model = mod,
                        variable.names = params,
                        n.iter = 5e3)
mod_csim <- as.mcmc(do.call(rbind, mod_sim)) # combined chains
plot(mod_sim)
gelman.diag(mod_sim) #te dice cuantos paramtros se deberia tener si hay convergencia
autocorr.diag(mod_sim)# si hay auto correlacion entre las simulaciones
autocorr.plot(mod_sim)
dic.samples(mod, n.iter = 1e5)#mientras mas pequeña mas soporte el modelo deviance information criterion

effectiveSize(mod_sim)#numero efectivo de simulaciones si quieres tener un posterior interval se debe tener un tamaño efectivo grande 3000 de cada parametro


#Randomness


#Jaw data
M   <- 4
N   <- 20
age <- c(8.0, 8.5, 9.0, 9.5)
Y   <- c(47.8, 48.8, 49.0, 49.7,
         46.4, 47.3, 47.7, 48.4,
         46.3, 46.8, 47.8, 48.5,
         45.1, 45.3, 46.1, 47.2,
         47.6, 48.5, 48.9, 49.3,
         52.5, 53.2, 53.3, 53.7,
         51.2, 53.0, 54.3, 54.5,
         49.8, 50.0, 50.3, 52.7,
         48.1, 50.8, 52.3, 54.4,
         45.0, 47.0, 47.3, 48.3,
         51.2, 51.4, 51.6, 51.9,
         48.5, 49.2, 53.0, 55.5,
         52.1, 52.8, 53.7, 55.0,
         48.2, 48.9, 49.3, 49.8,
         49.6, 50.4, 51.2, 51.8,  
         50.7, 51.7, 52.7, 53.3,
         47.2, 47.7, 48.4, 49.5,
         53.3, 54.6, 55.1, 55.3,
         46.2, 47.5, 48.1, 48.4,
         46.3, 47.6, 51.3, 51.8)

Y <- matrix(Y,20,4,byrow=TRUE)

plot(NA,xlim=range(age),ylim=range(Y),xlab="Age",ylab="Bone height")
for(j in 1:N){
  lines(age,Y[j,])
  points(age,Y[j,]) 
}

library(rjags)

model_string <- "model{

  # Likelihood
  for(i in 1:N){
    for(j in 1:M){
      Y[i,j]   ~ dnorm(mu[i,j],inv.var)
      mu[i,j] <- alpha[i] + beta[i]*age[j]
    }
  }

  # Random effects distributions
  for(i in 1:N){
    alpha[i] ~ dnorm(mu.alpha,inv.var.alpha)
    beta[i]  ~ dnorm(mu.beta,inv.var.beta)
  }

  # Prior
  inv.var       ~ dgamma(0.1, 0.1)
  inv.var.alpha ~ dgamma(0.1, 0.1)
  inv.var.beta  ~ dgamma(0.1, 0.1)
  mu.alpha      ~ dnorm(0,0.001)
  mu.beta       ~ dnorm(0,0.001)

}"



model <- jags.model(textConnection(model_string), 
                    data = list(Y=Y,N=N,M=M,age=age),
                    n.chains=3)

# Generation of simulated data
set.seed(123)
varY <- rnorm(100, 0, 1)
facX <- gl(n = 4, k = 25, labels = c("A", "B", "C", "D"))
block <- factor(rep(x = paste("B",1:25, sep = ""), times = 4))
df <- data.frame(varY, facX, block)

library('runjags')
template.jags(varY ~ facX + (1|block), data = df)

#North Carolina

ipo.1.jags <- jags(ipo.data, ipo.inits, ipo.parms, "ipomodel.txt", n.chains=3, n.iter=10000)
jagsfit.mcmc <- as.mcmc(ipo.1.jags)
xyplot(jagsfit.mcmc)
densityplot(bugsfit.mcmc, layout=c(3,2))#Displaying the posterior distributions
#The mcmc object created from a bugs or jags object is a list of matrices. We can rbind the individual list elements together and plot the result.
bugsfit.matrix <- rbind(bugsfit.mcmc[[1]], bugsfit.mcmc[[2]], bugsfit.mcmc[[3]])
#This can be done more efficiently with the do.call function of R.
bugsfit.matrix <- do.call(rbind, bugsfit.mcmc)
#The do.call function resembles sapply in that it can be used to apply a function to a list of elements.
#Unlike sapply which applies a function separately to each component of the list, do.call applies the function to all list components simultaneously. 

densityplot(as.mcmc(do.call(rbind, bugsfit.mcmc)))
#Bayesians calculate a statistic DIC (deviance information criterion) that is analogous to AIC. It is defined as follows.

#Separate intercepts model (model2.txt)
separate_intercept = "model{
for(i in 1:n) {
y[i]~dpois(mu.hat[i])
log.mu[i] <- a[patch[i]] + b1*year2[i] + b2*year3[i]
mu.hat[i] <- exp(log.mu[i])
}
#priors
for(j in 1:J){
a[j]~dnorm(0,.000001)
}
b1~dnorm(0,.000001)
b2~dnorm(0,.000001)
}"

#In the separate intercepts model we assigned separate uninformative priors to each intercept.
#To a Bayesian the random intercepts model differs because each intercept is given an informative prior so that the intercepts are now modeled parameters.
Random_intercepts = "model{
for(i in 1:n) {
y[i]~dpois(mu.hat[i])
log.mu[i] <- a[patch[i]] + b1*year2[i] + b2*year3[i]
mu.hat[i] <- exp(log.mu[i])
}
#level-2 model
for(j in 1:J){
   a[j]~dnorm(a.hat[j],tau.a)
   a.hat[j] <- mu.a
}
#priors
mu.a~dnorm(0,.000001)
tau.a <- pow(sigma.a,-2)
sigma.a~dunif(0,10000)
b1~dnorm(0,.000001)
b2~dnorm(0,.000001)
}"

# separate intercepts model
model2.glm <- glm(S~factor(patch)+factor(year), data=birds.short, family=poisson)
# random intercepts model
library(lme4)
model3.lmer <- lmer(S~factor(year) + (1|patch), data=birds.short, family=poisson)


#The parameters that appear in the priors for the intercepts, the mean mu.a and the standard deviation sigma.a,
#are in turn given their own priors, now called hyperpriors. In a similar vein the parameters mu.a and sigma.a are sometimes called hyperparameters. 

#Checking the summary object we find that all of the Rhats are less than 1.1 and the effective sample sizes are large.
#Likelihood

Random_intercepts = "model{
for(i in 1:n) {
y[i]~dpois(mu.hat[i])
log.mu[i] <- a[patch[i]] + b1*year2[i] + b2*year3[i]
mu.hat[i] <- exp(log.mu[i])
}
#level-2 model
for(j in 1:J){
   a[j]~dnorm(mu.a,tau.a)
}
#priors
mu.a~dnorm(0,.000001)
tau.a <- pow(sigma.a,-2)
sigma.a~dunif(0,10000)
b1~dnorm(0,.000001)
b2~dnorm(0,.000001)
}"


Random_effects = "model{
for(i in 1:n) {
y[i]~dpois(mu.hat[i])
log.mu[i] <- a[patch[i]] + b1*year2[i] + b2*year3[i]
mu.hat[i] <- exp(log.mu[i])
}
#level-2 model
for(j in 1:J){
   a[j] <- mu.a + u0[j]
   u0[j]~dnorm(0,tau.a)
}
#priors
mu.a~dnorm(0,.000001)
tau.a <- pow(sigma.a,-2)
sigma.a~dunif(0,10000)
b1~dnorm(0,.000001)
b2~dnorm(0,.000001)
}"

#We've already seen that the lmer function does not report a log-likelihood for Poisson models that is comparable to what is reported for other Poisson models (lecture 22).
#Even if we adjust for this and also take into account the difference between Dhat and Dbar in the Bayesian formulation, 
#the Bayesian and frequentist deviances with mixed effects models are not directly comparable. 
#In the Bayesian calculations the reported deviance is based on the joint likelihood of the data and the random effects, an expression we would write as follows.

#In the frequentist approach the reported log-likelihood is based on the marginal likelihood of y in which the random effects have been integrated out.
#These two likelihoods are not the same nor do they contain the same information about model fit.

#Random intercepts model with level-2 predictors 

random_level2 = "model{
for(i in 1:n) {
y[i]~dpois(mu.hat[i])
log.mu[i] <- a[patch[i]] + b1*year2[i] + b2*year3[i]
mu.hat[i] <- exp(log.mu[i])
}
#level-2 model
for(j in 1:J){
a[j]~dnorm(a.hat[j],tau.a)
a.hat[j]<-mu.a + g1*L.area[j] + g2*Lscape2[j] + g3*Lscape3[j] + g4*Lscape4[j]
}
g1~dnorm(0,.000001)
g2~dnorm(0,.000001)
g3~dnorm(0,.000001)
g4~dnorm(0,.000001)
b1~dnorm(0,.000001)
b2~dnorm(0,.000001)
mu.a~dnorm(0,.000001)
tau.a<-pow(sigma.a,-2)
sigma.a~dunif(0,10000)
}"



#Bayesian Mixture Model

#Multiple probability distribution in a linear combination
#X = F(x) = p(X <= x) = F(x) = sumatoria omeagai * G(x)
#e.g mixture of normals with the same varince f(x) = sumatoria omeagai * Ni(x)
#in the previous example only the mean of each one of the normals depends on the componend index in this case is a LOCATION MIXTURES OF NORMALS
#in a Location-ScaLe Mixture of Normals(multinormals distributions) when both parameters depend on the index componend
#the location-scale mixture of normals is usfull when we have multiple populations or when you try to get the skewness(population if contries, land size if different countries)
#when means are different but not too different
#when all the distributions comes from the same family we write like this F(x) = sumatoriak G(x|thetai)

E(x) = sum(x*f(x)) = sum(x*(w*gk(x))) = sum(w)*sum(x*gk(x)) or sum(w)*Eg(x)
Var(x) = E(x^2) - [E(x)]^2
E(x^2) = sum(w) * sum(x^2*gk(x)) = sum(w)*Egk(x^2))

Var(x) = (sum(wk) * sum(vargk(x) + Egk(x)^2)) - (sum(wk)*Egk(x))^2

#hierarchical representation of Mixtures models
#introduce an indicator  c = Discrete Random Variable
#so if in a simple model is x~f in a herarchical representation is x|c
x|c ~ gc(x)
c ~  p(c) = wk #c is just and index
p(x) = sum(p(x|c)p(c = k))

#two different types of likelihood functions.
#observed data likelihood: that just uses the original representation of the mixture model as a weighted sum.
x1, ..., xm #observation 

#Note: remenber likelihood function is just a function that is proportional to the joint distribution of the data.
likelihood = L(w1,....wk, tetha1,...,tethak) = prod(sum(wk*gk(xi|thetak))) #the product is over observations and the sum is over the components
#Note: you have to be carefull always the product is first and then is the sum

#complete data likelihood: that relies on the hierarchical representation 
#That will allow us to have likelihood function that depends on more parameters because it will depend also on the c's. 
#But where the structure that you get on the right is much simpler, and easier to work with. 

#the idea now is that we are going to augment our parameter space by also considering 
#the later observation C that we discussed before another Heracles representation, that just tell us to which component each observation belongs.

xi|ci ~ gci(xi) p(c = K) = wk
L(w1,...,wk,theta1,...,thetak,c1,...,ck) = prod 1,..,n * prod 1,...,k (wk*gk(xi))

#identifability
#You may recall that the statistical model is identifiable if two different values for the parameters,
#any two different values fro the parameters, always raise two different probability distributions for the data.
#unfortunately, mixture models are not fully identifiable.

#label switching
#Essentially, if you have more components in the mixture, 
#you're going to have many more permutations that you could do of the terms that, at the end of the day, represent the same mixture. 
#So the fact that you can switch this order that translates into basically switching what names you use to label the components, is what we call label switching.



#Lilkelihood

#WinBUGS and JAGS and for that matter Bayesians in general use the term deviance to mean –2 times the log-likelihood of the model.
deviance = –2 × log-likelihood

#This differs from the classical definition of deviance used in generalized linear models where the deviance (scaled) was defined as twice the difference in 
#the log-likelihoods between the current model and the so-called saturated model—a model in which there is a separate parameter estimated for every observation.
#The deviance defined in this way can be used as a goodness-of-fit statistic for Poisson or grouped binary data, but only when the expected cell counts meet 
#certain minimum cell-size criteria.

pD = effective number of estimated parameters
#Dmin is the deviance calculated at the modes of the posterior distributions of all parameters in the model and K is number of parameters estimated in fitting the model.

AIC = Dmin + 2K
DIC = Dhat + 2pD

#where Dhat is the deviance calculated at the means of the individual parameter posterior distributions and pD is as defined above. 
pD = Dline - Dhat

#where Dline is the mean of posterior distribution of the deviance.

#Because DIC and AIC in mixed effects models work with different log-likelihoods, they are not directly comparable. Whether AIC is an appropriate way to compare mixed effects models is a matter of debate. 
#An alternative definition of AIC has been proposed by Vaida and Blanchard (2005) for mixed effects models. 

#logLik extracts the log-likelihood of lm, glm, glm.nb, and gamlss models.

#Binomial
h1 <- dbinom(1, 5, 1/3) #Verosimilitud hipotesis 1 
h2 <- dbinom(1, 5, 1/2) 
h1/h2 #força de evidência em favor da hipótese mais plausível
#Grafica
curve(dbinom(1, 5, p = x), from = 0, to = 1)
theta <- seq(0,1,length = 100) 
y <- dbinom(1, 5, p = theta) 
theta.mle <- theta[ y == max(y) ] #PONTO de MÁXIMO da FUNÇÃO de VEROSSIMILHANÇA
abline(v = theta.mle, lty = 2, col = "orange")
yrel <- y / max(y) #VEROSSIMILHANÇA RELATIVA
plot(theta, yrel, type = "l", xlab = "Probabilidade p", col = "blue") 
abline( v = theta.mle, lty = 2, col = "orange")
theta.interv <- theta[ max(y)/y <= 8 ] #INTERVALO DE VEROSSIMILHANÇA 
lines(theta.interv, rep(1/8, length(theta.interv)), col = "red", lwd = 3)#Limites de confianza
#verossimilhança da amostra é o produto das funções de verossimilhança das observações independentes
x <- seq(0,30, by = 0.1) 
y1 <- dpois(5, lambda = x) 
y2 <- dpois(8, lambda = x) 
y3 <- dpois(9, lambda = x) 
y4 <- dpois(11, lambda = x) 
y <- y1 * y2 * y3 * y4 
plot(x, y, type = "l", col = "red")
curve(dpois(5, x)*dpois(8, x)*dpois(9, x)*dpois(11, x), 0, 30, xlab = expression(lambda), ylab = "Verossimilhança", col = "red")
range(y)

#Constructing the negative binomial log-likelihood
NB.LL<-function(mu, theta) sum(log(dnbinom(aphid.data, mu = mu, size = theta)))#negative binomial log-likelihood
NB.LL(3,4)
negNB.LL<-function(p) -NB.LL(p[1],p[2]) #negative log-likelihood for nlm
out.NB <- nlm(negNB.LL, c(3,4), hessian=TRUE)#Obtaining the MLE
out.NB

#It turns out there is a function in the Bhat package that can be used to calculate profile likelihood confidence intervals directly. The function is called plkhci. 

library(Bhat)
?plkhci 
#The first argument is a list consisting of four components: a label containing the names of the parameters as used in the log-likelihood function, 
#initial estimates for the MLEs, an estimated lower bound for the confidence interval, and an estimated upper bound for the confidence interval. 
#The second argument is the name of the negative log-likelihood function. (Note: Like nlm the plkhci function requires a negative log-likelihood!)
#The third argument is the name of the parameter for which to calculate the confidence interval and must match a name that appears in the label component of the first argument to the plkhci function.
x.in<-list(label='lambda',est=3.4, low=2, up=5)
plkhci(x.in, poisson.negloglik, 'lambda')


#Bolker
binomNLL1 = function(p, k, N) {

-sum(dbinom(k, prob = p, size = N, log = TRUE))
 }

O1 = optim(fn = binomNLL1, par = c(p = 0.5), N = 10, k = k, method = "BFGS")

#The mle2 function in the bbmle package provides a “wrapper” for optim that gives prettier output and makes standard tasks easier ∗
library(bbmle)
m1 = mle2(minuslogl = binomNLL1, start = list(p = 0.5), data = list(N = 10, k = k))

#The mle2 package has a shortcut for simple likelihood functions. In-
#stead of writing an R function to compute the negative log-likehood, you
#can specify a formula:

mle2(k ~ dbinom(prob = p, size = 10), start = list(p = 0.5))

#gives exactly the same answer as the previous commands.

gammaNLL1 = function(shape, scale) {
-sum(dgamma(myxdat$titer, shape = shape, scale = scale,
log = TRUE))
}

#It’s harder to find starting parameters for the Gamma distribution.
#to determine reasonable starting values for the scale (=variance/mean=coefficient of variation [CV]) and shape(=variance/mean 2 =mean/CV) parameters ∗ .

#Poisson

#log
library(MASS)# carrega o pacote "MASS" args(dpois)

args(dpois) # verifica os argumentos da função "dpois" lpois = function(x,lambda) -dpois(x,lambda,log=TRUE)  
lpois <- function(x,lambda) -dpois(x, lambda, log = TRUE)  # cria uma função da log-veros. neg. llikpois = Vectorize(lpois, c("x","lambda"))# cria uma função
llikpois <- Vectorize(lpois, c("x","lambda"))# cria uma função "vetorizada" da log-veros. neg.
x <- c(5, 8, 9, 11) #Datos
lambda <- seq(0, 30, by = 0.1) #hipotesis
lver.mat <- outer(x, lambda, "llikpois")# uma matrix a logveros. neg. de cada observação
lver <- apply(lver.mat, 2, sum)  # um vetor com a logveros. neg da amostra
plot(lambda, lver, type="l", col="red", xlab=expression(lambda))
lambda.min = lambda[ min(lver) == lver ] #O ponto que maximiza a verossimilhança é o mesmo que minimiza a log-verossimilhança negativa
abline(v = lambda.min, lty = 2, col = "purple")
#Intervalo
lambda.int <- lambda[lver - min(lver) <= log(8)] 
lines(lambda.int, rep(log(8), length(lambda.int)), col="purple", lwd=3 )
# mle2, do pacote bbmle1 soma a log-verossimilhança negativa para cada valor dos parâmetros para todas observações na amostra.

#mle
euplant <- c(5, 23, 23, 17, 0, 4, 1, 0, 0, 1, 0, 2, 26, 65, 34, 14, 18, 13, 19, 7) #Datos
library(bbmle)
nvl <- function(lambda) -sum(stats::dpois(euplant, lambda, log=TRUE))
euplant.mle = mle2(nvl, start = list(lambda = 10))  # gera um objeto "mle"
summary(euplant.mle) 
logLik(euplant.mle) # valor da logverossimilhança negativa do objeto
coef(euplant.mle) 
mean(euplant)
#Para visualizar a curva da log-verossimilhança negativa use a função plotprofmle, do pacote sads. 
library(sads) 
plotprofmle(profile(euplant.mle) )

#Poisson2
num.stems<-c(6,8,9,6,6,2,5,3,1,4)
aphid.data<-rep(0:9, num.stems) #Datos
barplot(num.stems)
names(num.stems)<- 0:9
dpois(aphid.data, lambda = 1) #Verosimilitud de los datos individuales
sum(log(dpois(aphid.data,lambda = 1))) #log like de la sumatoria de los datos
poisson.LL<-function(lambda) sum(log(dpois(aphid.data, lambda))) #Funcion de verosimilitud para póisson

#Graficar
poisson.L<-function(lambda) prod(dpois(aphid.data, lambda))
plot(seq(2, 5, .01), sapply(seq(2, 5, .01), poisson.L), type='l', xlab = expression(lambda), ylab = 'likelihood')
#R provides two numerical optimization functions, nlm and optim
#of which I focus on nlm
#From the help screen we see that there are only two arguments that are required, f, the function to be minimized, and p, a vector of initial estimates of the parameter values. 
poisson.negloglik <- function(lambda) -poisson.LL(lambda)
out.pois <- nlm(poisson.negloglik, 3)#function uses a method similar to the Newton-Raphson method
out.pois
#The value of the gradient at the MLE is very small. This is what we want. 
#The gradient is the same as the score, the derivative of the log-likelihood, which from calculus should be zero at a maximum or a minimum. 
#Thus the fact that the value of the gradient is very close to zero encourages us to believe that nlm has found a solution (although there's no guarantee that it's the global minimum that we seek).
#The help screen tell us that code=1 means "relative gradient is close to zero, current iterate is probably solution."
#It took 4 iterations of the numerical algorithm to converge.

#By adding the argument hessian=T to nlm we can get it to return the value of the Hessian evaluated at the maximum likelihood estimate of λ.

#data
num.stems <- c(6, 8, 9, 6, 6, 2, 5, 3, 1, 4)
# data frame of tabulated data
aphids <- data.frame(aphids = 0:9, counts = num.stems)

# refit model to return Hessian
out.pois <- nlm(pois.negloglike, 2, hessian = T)
# standard error of the mean
sqrt(1/out.pois$hessian)

#We can use this to calculate 95% Wald confidence intervals for λ (assuming that the sample size is large enough that we can assume normality for the distribution of the MLE).
# Wald confidence intervals
out.pois$estimate + c(1, -1)*qnorm(.975)*sqrt(1/out.pois$hessian)

poisson.LL <- function(lambda) sum(log(dpois(aphid.data, lambda)))
poisson.negloglik <- function(lambda) -poisson.LL(lambda)
out.pois <- nlm(poisson.negloglik, 3)

#chi test
poisson.p <- c(dpois(0:8, out.pois$estimate), 1-ppois(8, out.pois$estimate))
chisq.test(num.stems, p=poisson.p, simulate.p.value=TRUE, B=9999)


#Likelihood ratio test of whether the Poisson means are the same in the field types
LRstat <- 2*(out.pois1$minimum - out.pois2$minimum)
1-pchisq(LRstat,df=1)

#Ejemplo
library(bbmle)
library(car)
library(sads)#No lo pude descargar en la version 3.3.3
set.seed(1234)
nplan <-1000
lambda <-5
frutos <- rpois(nplan, lambda)
dpois(0, lambda) #probabilidade teórica de encontrar uma planta sexualmente madura sem frutos
sum(frutos == 0)/length(frutos)#Compare com a proporção de plantas sem frutos na amostra simulada
#gráfico de barras das proporções
mfrut <- max(frutos)
fa <- factor(frutos, levels = 0:mfrut)
prob.obs <- table(fa)/nplan
par(las = 1)
plot(0:mfrut, prob.obs, xlab = "Numero de frutos", 
     ylab = "Probabilidade", type = "h", lwd = 5)

prob.tr<-dpois(0:mfrut, lambda)
points(0:mfrut, prob.tr, pch = 21, col = "red") # pontos com os valores teóricos esperados

x<-frutos
poisNLL<-function(lambda){
        -sum(dpois(x, lambda, log=TRUE))
} # função de máxima verossimilhança do parâmetro lambda da distribuição Poisson
xvec <- seq(4.85, 5.3, length = 1000) #  sequência de valores na vizinhança da média amostral
LLest <- sapply(xvec, poisNLL)

minLLest<-min(LLest) # buscamos o valor mínimo
lambdaLL.fb <- xvec[LLest == min(LLest)]

#mle
lambdaLL.nm <- mle2(poisNLL, start = list(lambda = 4))#Faça a minimização numérica com a função mle2
lambdaLL.fb
lambdaLL.nm
mean(frutos)

#vamos fazer a comparação gráfica, plotando a função de verosimilhança e os pontos obtidos

mfrutos <- mean(frutos)
LLest2 <- LLest - min(LLest)
plot(xvec, LLest2, typ = "l", xlab = "frutos", ylab = "loglik")
abline(v = mfrutos, col = "blue", lwd = 3)

abline(v = coef(lambdaLL.nm), col ="darkgray")
abline(v = lambdaLL.fb, col = "red")

#ecundidade das plantas poderia incrementar-se com um aumento da concentração de fósforo

set.seed(1234)
phos <-runif(100, 0, 10)
a <- 1
b <- 0.3
x <-phos

ydet <- exp(a + b*x)#funcão de ligação logarítmica.
plot(x, ydet)

#sorteamos amostras Poisson com parâmetro lambda igual a estes valores esperados

fec <-rpois(100, ydet)
par(las = 1)
plot(phos, fec, xlab = "Fósforo mg/Kg", ylab = "Número de frutos")

#Definimos a função de verosimilhança para este modelo

poisglmNLL <- function(a, b) {
        ypred = exp(a + b*x)
        -sum(dpois(fec, lambda = ypred, log = TRUE))
}

chute <- list(a = 2.5, b = 0.33) #Valores iniciales
mod.pois<-mle2(poisglmNLL, start= chute) #Estimacion
summary(mod.pois)
mod.pois.prof <- profile(mod.pois)
plot(mod.pois.prof)
confint(mod.pois)

#Por fim, plotamos as curvas dos valores esperados de frutos por planta com os parâmetros e suas estimativas
par(las = 1)
plot(phos, fec, xlab = "Fósforo mg/Kg", ylab = "Número de frutos")
a.est <- coef(mod.pois)[1]
b.est <- coef(mod.pois)[2]
curve(exp(a + b*x), add = TRUE, col = "red")
curve(exp(a.est + b.est*x), add = TRUE, col = "blue", lty = 2) #estimada
legend("topleft", c("Parâmetro","Estimativa"),col=c("red","blue"), lty=c(1,2))

#O que aconteceria se usássemos outro modelo para descrever estes dados? Neste caso sabemos qual é o modelo correto
#mas vamos simular esta situação, imaginando que o pesquisador está experimentando diferentes modelos. Usaremos a função de verosimilhança de uma binomial negativa, que permite agregação. 

negbinNLL<- function(a, b, k){
        ypred <- exp(a + b*x)
        -sum(dnbinom(fec, mu = ypred, size = k, log = TRUE))
}


#Tentamos um valor inicial de k usando o método dos momentos

med <- mean(fec)
vari <- var(fec)
k.init <- med^2/(vari - med)
k.init

mod.negbin<- mle2(negbinNLL, start = list(a = 2.5, b = 0.33, k = k.init))
summary(mod.negbin)

#AIC
AICtab(mod.pois,mod.negbin, delta=T, sort=T, weights = TRUE)

#O modelo com melhor suporte é o modelo com distribuição Poisson, porém a diferença nos valores do AIC é menor que 2.

#obtain data 
slugs <- read.table( 'http://www.bio.ic.ac.uk/research/mjcraw/statcomp/data/slugsurvey.txt', header = TRUE)
names(slugs)
dim(slugs)
table(slugs$field)
#stacked bar plot
barplot(table(slugs$slugs, slugs$field))
#separate bar plots
barplot(table(slugs$slugs, slugs$field), beside=T)
t(table(slugs$slugs, slugs$field))
#Lattice
slugtable <- data.frame(table(slugs$slugs, slugs$field))
slugtable$Var1 <- as.numeric(as.character(slugtable$Var1))
library(lattice)
barchart(Freq ~ Var1|Var2, data = slugtable, xlab = 'Count category', horizontal = F)
barchart(Freq ~ factor(Var1)|Var2, data = slugtable, xlab = 'Count category', horizontal = F, origin = 0, col = 'grey')#Muestra el 0
xyplot(Freq ~ Var1|Var2, data = slugtable, xlab = 'Count category', type = 'h', lineend = 1, lwd = 10, col = 'grey')
#negative LL for a Poisson model--common mean
negpois1.LL <- function(p){
        mu <- p[1]
        LL <- sum(log(dpois(slugs$slugs, lambda = mu)))
        -LL
}
mean(slugs$slugs)
nlm(negpois1.LL, 2) -> out.pois1

#negative LL for a Poisson model--separate means model
negpois2.LL <- function(p){
        mu <- p[1] + p[2]*z
        LL <- sum(log(dpois(slugs$slugs, lambda = mu)))
        -LL
}

tapply(slugs$slugs, slugs$field, mean) #Para ver los valores iniciales
nlm(negpois2.LL, c(1.2, 1)) -> out.pois2
out.pois2

LRstat <- 2*(out.pois1$minimum - out.pois2$minimum)#Likelihood ratio test
#This statistic has an asymptotic chi-squared distribution with degrees of freedom equal to the difference 
#in the number of parameters estimated in the two models, which is 1 for these two models.
length(out.pois2$estimate) - length(out.pois1$estimate)

#Likelihood ratio test of whether the Poisson means are the same in the field types
1-pchisq(LRstat,df = 1)
#We reject the null hypothesis at ?? = .05


#Negative binomial regression functions
library(MASS)
out.nb <- glm.nb(aphid.data ~ 1)
summary(out.nb)
exp(coef(out.nb))#mean
out.nb$theta#dispertion parameter

out.bar <- barplot(num.stems, ylim = c(0, max(c(exp.NB, exp.pois, num.stems))))
#add negative binomial
points(out.bar, exp.NB, col = 2, pch = 22, cex = .9, type = 'o')
#add poisson
points(out.bar, exp.pois, col = 1, pch = 16, cex = .9, type = 'o')
legend('topright', c('negative binomial', 'Poisson'), col = c(2, 1), lty = 1, pch = c(22, 16), bty = 'n', cex = .8)

#Simulation-based goodness of fit test
NB.p <- c(dnbinom(0:8, mu = out.NB$estimate[1], size = out.NB$estimate[2]), 1 - pnbinom(8, mu = out.NB$estimate[1], size = out.NB$estimate[2]))
chisq.test(num.stems, p = NB.p, simulate.p.value = TRUE, B = 9999)

slugs <- read.table('http://www.bio.ic.ac.uk/research/mjcraw/statcomp/data/slugsurvey.txt', header = TRUE)
#invert Hessian to get standard errors
se2 <- sqrt(diag(solve(out.pois2$hessian)))
anova(out1, out2, test = 'Chisq')
#To get predictions on the scale of the response variable we need to add the type='response' argument.
predict(out2, type = 'response')
#https://sakai.unc.edu/access/content/group/3d1eb92e-7848-4f55-90c3-7c72a54e7e43/public/docs/lectures/lecture16.htm
#leer la 17
#https://sakai.unc.edu/access/content/group/3d1eb92e-7848-4f55-90c3-7c72a54e7e43/public/docs/lectures/lecture17.htm


out2 <- glm(slugs ~ field, data = slugs, family = poisson)# use glm to obtain estimates
slugtable$mu <- predict(out2, type = 'response', newdata = data.frame(field = slugtable$Var2))# predicted means
slugtable$p <- dpois(slugtable$Var1.num, lambda = slugtable$mu)# Poisson probabilities
# add tail probabilities
slugtable$p2 <- dpois(as.numeric(as.character(slugtable$Var1)), lambda = slugtable$mu) + (slugtable$Var1 == 10) * ppois(as.numeric(as.character(slugtable$Var1)), lambda = slugtable$mu, lower.tail = F)
n <- table(slugs$field)# count the number of observations in each field
slugtable$exp <- slugtable$p2 * n[as.numeric(slugtable$Var2)]# calculate predicted counts under Poisson model
# add predicted counts to the bar plot of the observed counts
library(lattice)
xyplot(Freq ~ Var1.num|Var2, data = slugtable, xlab = 'Count category', panel = function(x, y, subscripts) {
panel.xyplot(x, y, type = 'h', lineend = 1, col = 'grey', lwd = 10)
panel.points(x, slugtable$exp[subscripts], pch = 16, cex = .6, col = 1, type = 'o')
})

#multinomial distributions
slug.p <- split(slugtable$p2, slugtable$Var2)# split probabilities by field type
?rmultinom

n <- table(slugs$field)
rmultinom(1, size = n[1], prob = slug.p[[1]])# simulate data for field=Nursery
rmultinom(1, size = n[2], prob = slug.p[[2]])# simulate data for field=rokery
set.seed(10)
sapply(1:2, function(x) rmultinom(1, n[x], slug.p[[x]]))#We can obtain both of these at once using sapply to generate all 80 observations (40 from each field type)

out.obs <- as.vector(sapply(1:2, function(x) rmultinom(1, n[x], slug.p[[x]])))
sum((out.obs - slugtable$exp)^2/slugtable$exp)# calculate Pearson statistic using both field results

#The result should be a typical value of a Pearson statistic when the model actually fits the data.
#To get a full sense of what such typical values look like we need to repeat these steps enough times 
#to get a good estimate of the null distribution of Pearson statistics. 

# write things as a function that can be replicated
myfunc <- function() {
out.obs <- as.vector(sapply(1:2, function(x) rmultinom(1, size = n[x], prob = slug.p[[x]])))
sum((out.obs - slugtable$exp)^2/slugtable$exp)
}

# Now do this 9999 times to yield 10,000 Pearson statistics
set.seed(23)
sim.data <- replicate(9999, myfunc())
max(sim.data)

#Pearson statistic using the observed data 
#and then append the result to the vector of simulated Pearson statistics for a total of 10,000 Pearson statistics.
actual <- sum((slugtable$Freq - slugtable$exp)^2/slugtable$exp)
pearson <- c(sim.data, actual)# add actual value to the list of simulated Pearson statistics
pval <- sum(pearson >= actual)/length(pearson)#P-value
#H0: fit is adequate
#H1: fit is inadequate
#Thus we reject the null hypothesis and conclude there is a significant lack of fit.
round((slugtable$Freq - slugtable$exp)^2/slugtable$exp, 3)

#R provides a number of functions for generating a grid of which expand.grid is perhaps the most useful. 
#The expand.grid function takes as its arguments two vectors and then generates all possible combinations of the components of these two vectors.
out.pois2 <- nlm(negpois2.LL, c(1.2,1))
g <- expand.grid(b0 = seq(0.5, 2, .05), b1 = seq(0.5, 2, .05))#generating a grid of points in the plane near MLE
g[1:8,]
g$z <- apply(g, 1, pois2.LL) #create and append the z-coordinate, the log-likelihood


#The persp function of R can be used to make a serviceable surface graph. 
#persp expects the z-coordinates to be arranged in a matrix whose dimensions match the dimensions of the grid we've created.
#The rows of the matrix should correspond to the x-values and the columns to the y-values. We can obtain this format using the matrix command. 
#The first argument to matrix is the vector that is to be converted to a matrix.
z.matrix <- matrix(g$z, nrow = length(seq(0.5, 2, .05)))#we construct a matrix of z-values to match the grid
#The persp function does not support mathematical characters.
persp(seq(0.5, 2, .05), seq(0.5, 2, .05), z.matrix, xlab = "b0", ylab = "b1", zlab = "log-likelihood")
persp(seq(0.5, 2, .05), seq(0.5, 2, .05), z.matrix, xlab = "b0", ylab = "b1", zlab = "log-likelihood", ticktype = "detailed")#add ticks using ticktype option
persp(seq(0.5, 2, .05), seq(0.5, 2, .05), z.matrix, xlab = "b0", ylab = "b1", zlab = "log-likelihood", ticktype = 'detailed', theta = 30, phi = 30)#change viewing position

#We can add the location of the maximum likelihood estimate to the surface.
#For this we need to use the trans3d function that converts the three-dimensional coordinates of the point to a 2-dimensional representation 
#that is consistent with what was used to produce the figure. 
out.persp <- persp(seq(0.5, 2, .05), seq(0.5, 2, .05), z.matrix, xlab = "b0", ylab = "b1", zlab = "log-likelihood", ticktype = 'detailed', theta = 30, phi = 30)
trans3d(out.pois2$estimate[1], out.pois2$estimate[2], -out.pois2$minimum, out.persp)
points(trans3d(out.pois2$estimate[1], out.pois2$estimate[2], -out.pois2$minimum, out.persp), pch = 16, col = 2, cex = 1.5)

#The lattice graph equivalent to persp is a function called wireframe. First we need to load the lattice package.
#wireframe expects the z-coordinates to fill a vector, not a matrix. 
wireframe(z ~ b0*b1, data = g)#wireframe just takes ordinary vectors
wireframe(z ~ b0*b1, data = g, screen = list(z = 20, x = -70, y = 3))#can also get different viewpoint
wireframe(z ~ b0*b1, data = g, xlab = expression(b[0]), ylab = expression(b[1]), zlab = "log\nlikelihood", scales = list(arrows = FALSE))#add tick marks and nice labels
wireframe(z ~ b0*b1, data = g, xlab = expression(b[0]), ylab = expression(b[1]), zlab = "log\nlikelihood", scales = list(arrows = FALSE), drape = TRUE)#add color with drape argument

#logLik extracts the log-likelihood of lm, glm, glm.nb, and gamlss models.

negNB.LL3 <- function(p){#Common mean, separate dispersions model
mu <- p[1]
theta <- p[2] + p[3]*(slugs$field == "Rookery")
LL <- sum(log(dnbinom(slugs$slugs, mu = mu, size = theta)))
-LL
}
out.NB3 <- nlm(negNB.LL3, c(2, 1, 1))

negNB4.LL <- function(p) {# model 4: two means, two dispersions negative binomial model
mu <- p[1] + p[2]*(slugs$field == "Rookery")
theta <- p[3] + p[4]*(slugs$field == "Rookery")
LL <- sum(log(dnbinom(slugs$slugs, mu=mu, size=theta)))
-LL
}
# obtain estimates
out.NB4 <- nlm(negNB4.LL, c(2,1,1,1))

LRstat4 <- 2*(out.NB3$minimum - out.NB4$minimum)#Likelihood ratio test
1-pchisq(LRstat4, df=1)#test

#The gamlss package is a comprehensive package for fitting regression models using a vast array of probability models. 
#The gamlss package allows the user to model any of the parameters of these distributions, not just the mean.
#The gamlss package has its own web site with extensive documentation (www.gamlss.org).

library(gamlss)
glm.NB3 <- gamlss(slugs ~ 1, sigma.formula = ~field, data = slugs, family = NBI)#Common mean, separate dispersions model
summary(glm.NB3)
#The gamlss package includes a special method for coef that permits the inclusion of a what argument to extract the estimates for the dispersion part of the model.

#To match the overall mean returned by nlm we need to exponentiate the value returned by gamlss.
exp(coef(glm.NB3))
1/exp(coef(glm.NB3, what = 'sigma')[1])#to obtain the dispersion of the "Nursery" habitat we exponentiate the first estimate 

LRfunc <- function(x, y) {#likelihood ratio test function
LR <- 2*(logLik(y) - logLik(x))
df <- attr(logLik(y), 'df') - attr(logLik(x), 'df')
p <- 1 - pchisq(LR, df)[1]
out <- data.frame(LR = LR, df = df, p = p)
print(out, row.names = F)
}
LRfunc(glm.pois1, glm.pois2)

library(gamlss)
LR.test(glm.NB3, glm.NB4)#another likelihood ratio test

model3 <- nls(Species ~ b0*Area^b1, data = gala, start = list(b0 = 1, b1 = .5))#(an acronym for nonlinear least squares)  These get specified by name in the start argument of nls.

lognorm.LL <- function(y, model, c = 0) {
sigma2 <- sum(residuals(model)^2)/length(residuals(model))
prob <- ifelse(y == 0, pnorm(log(y + c + 0.5), mean = predict(model), sd = sqrt(sigma2)), pnorm(log(y + c + 0.5), mean = predict(model),
               sd = sqrt(sigma2)) - pnorm(log(y + c -0.5), mean = predict(model), sd = sqrt(sigma2)))
LL <- sum(log(prob))
df <- length(coef(model)) + 1
AIC <- -2*LL + 2*df
out <- data.frame(LL = LL, df = df, AIC = AIC)
out}

upper.p <- 1 - pnbinom(gala$Species - 1, mu = gala$mu, size = out.NB2$theta)
lower.p <- pnbinom(gala$Species, mu = gala$mu, size = out.NB2$theta)
pval.dat <- data.frame(pvalue = c(lower.p, upper.p), island = rep(gala$Island,2), label = rep(c('lower', 'upper'), each = nrow(gala)))
dotplot(island~pvalue|factor(label, levels = levels(label), labels = c('Lower-tailed', 'Upper-tailed')), data = pval.dat, xlab = 'p-value', panel = function(x, y) {
panel.dotplot(x, y)
panel.abline(v = .05, col = 2,lty = 2)})

p.to.plot <- ifelse(pnbinom(gala$Species, mu=gala$mu, size=out.NB2$theta) < .5, pnbinom(gala$Species, mu=gala$mu, size=out.NB2$theta), pnbinom(gala$Species-1, mu=gala$mu, size=out.NB2$theta))

library(grid)
dotplot(gala$Island ~ p.to.plot, xlab = ' ', xlim = c(-0.03, 1.03), panel = function(x, y){
panel.dotplot(x, y)
panel.abline(v=c(.025, .975), col = 2, lty = 2)
panel.abline(v = 0.5, col = 'grey70', lwd = 2)},
scales = list(x = list(at = seq(0, 1, .2), labels = c(0, 0.2, 0.4, 0.4, 0.2, 0))), page = function(n) {
grid.text("upper-tailed p-value", x = .85, y = 0.025, default.units = "npc", just = c("right", "bottom"))
grid.text("lower-tailed p-value", x = .45, y = 0.025, default.units = "npc", just = c("right", "bottom"))})

#A saturated model is one in which one parameter is estimated for each observation.

sum(residuals(out.pois, type = 'deviance')^2)#We can extract the deviance residuals from a Poisson regression model with the residuals function by specifying type = 'deviance'.

#It turns out that the residual deviance for a Poisson regression model is equal to the G-statistic (in the notation of Sokal and Rohlf, 1995).
2*sum(gala$Species * log(gala$Species/fitted(out.pois)))
out.pois$deviance/out.pois$df.residual
anova(out.pois, test='Chisq')
sum(fitted(out.S)<5)/length(fitted(out.S))#must be less than 0.2
out.S$deviance/out.S$df.residual
1-pchisq(out.S$deviance, out.S$df.residual)
confint(out.S1)#We previously fit the analysis of covariance model so I use the confint function to obtain 95% confidence intervals for the regression parameters.




#Normal

nllikGauss <- function(m = 90, s = 80) -sum(stats::dnorm(esa$total, m, s,
log = TRUE))
gauss01 <- mle2(nllikGauss) #Mesmo sendo um modelo simples, podemos ajustá-lo utilizando a função mle (Maximum Likelihood
#Estimator). Para isso é bom definirmos uma função que calcula a log-verossimilhança negativa da
#distribuição Gaussiana (Normal) 
summary(gauss01) #O objeto gerado pela função “mle” é um objeto de classe “mle” e pode ser utilizado diretamente em algumas funções como: “summary” e “logLik”

par(mfrow = c(1,2))
plot( profile(gauss01)) #A função “profile” gera a verosimilhança perfilhada para os parâmetros do modelo, mas utiliza uma transformação da verossimilhança.
par(mfrow = c(1,1))

libary(sads) # primeiro instale o pacote se não o tiver
par(mfrow = c(1,2))
plotprofmle(profile(gauss01)) #Assim, fizemos a função “plotprofmle”, que está no pacote sads, para construir as curvas de verossimilhança perfilhada
par(mfrow = c(1,1))

nllikGauss2 <- function(b0 = 0, b1 = 1, s = 80){ #Para podermos ajustar um modelo onde a média é uma função linear, precisamos definir uma nova função de Log-Verossimilhança Negativa
m <- b0 + b1*(esa$dap^2*esa$ht) # Note que temos agora três parâmetros: e definem a função linear da média, como função da variável combinada “(esa$dap^2*esa$ht)”, enquanto que o desvio padrão
-sum(stats::dnorm(x = esa$total, mean = m, sd = s, log = T))}

gauss02 <- mle2(nllikGauss2)
summary(gauss02) #Podemos notar que os coeficientes de regressão são semelhantes aos obtidos pelo forma clássica de ajuste de modelos lineares de regressão.
AICtab(gauss01, gauss02, base = TRUE, logLik = TRUE) #Comparação

nllikGauss3 <- function(b0 = 11.5, b1 = 0.0264, a0 = 1, a1 = 10){ #Assim podemos ajustar um modelo onde média e desvio padrão são funçôes do tamanho da árvore.

m <- b0 + b1*(esa$dap^2*esa$ht)
s <- exp(a0)*(esa$dap^2*esa$ht)^a1
-sum(stats::dnorm(x = esa$total, mean = m, sd = s, log = T))}

gauss03 <- mle2(nllikGauss3)
summary(gauss03)

par(mfrow = c(2, 2))
plotprofmle(profile(gauss03))
par(mfrow = c(1, 1))

nllikGauss3b <- function(b0 = 11.5, b1 = 0.0264, a0 = 1, a1 = 0.5){ #Uma outra Função para o Desvio Padrão
m <- b0 + b1*(esa$dap^2*esa$ht)
s <- a0 + a1*(esa$dap^2*esa$ht)^(1/2)
-sum(stats::dnorm(x = esa$total, mean = m, sd = s, log = T))}

gauss03b = mle2(nllikGauss3b)
summary(gauss03b)

nllikGauss3b <- function(b0=11.5, b1 = 0.0264, a0 = 1, a1 = 0.5){
m <- b0 + b1*(esa$dap^2*esa$ht)
s <- a0 + a1*(esa$dap^2*esa$ht)^(1/2)
-sum(stats::dnorm(x = esa$total, mean = m, sd = s,log = T))}
gauss03b = mle2(nllikGauss3b)
summary(gauss03b)

nllikGauss4 <- function(b0 = -2.4, b1 = 0.86, a0 = 1, a1 = 10){ #Função Não-Linear para Média
m <- exp(b0) *(esa$dap^2*esa$ht)^b1
s <- exp(a0)*(esa$dap^2*esa$ht)^a1
-sum(stats::dnorm(x = esa$total, mean = m, sd = s, log = T))}
summary(gauss04)
AIC(gauss04)

nllikGauss5 <- function(b0 = -1.7, b1 = 2.44, b2 = -0.097 , a0 = 1, a1 = 10){ #NOVA Função Não-Linear para Média
m <- exp(b0) * esa$dap^b1 * esa$ht^b2
s <- exp(a0)*(esa$dap^2*esa$ht)^a1
-sum(stats::dnorm(x = esa$total, mean = m, sd = s,log = T))}
gauss05 = mle2( nllikGauss5 )
summary(gauss05)

nllikLGauss <- function(b0 = 0, b1 = 1, slog = 1){ #Ajuste do Modelos Log-Normais
mlog <- b0 + b1*log(esa$dap^2*esa$ht)
-sum(stats::dlnorm(esa$total,mlog,slog,log=T))}
lgauss01 <- mle2(nllikLGauss)
summary(lgauss01)

llikLGauss2 <- function(b0 = 0, b1 = 2, b2 = 1, slog = 1){
mlog <- b0 + b1*log(esa$dap) +b2*log(esa$ht)
-sum(stats::dlnorm(esa$total, mlog, slog, log = T))}

#Trabalhamos em todos os modelos até esse ponto com duas variáveis preditoras: DAP (dap) e altura
#(ht). Mas qual o desempenho de um modelo mais simples com apenas o DAP como variável preditora.
nllikGauss6 <- function(b0 = -1.7, b1 = 2.44, a0 = 1, a1 = 10){
m <- exp(b0) * esa$dap^b1
s <- exp(a0)*esa$dap^a1
-sum(stats::dnorm(x = esa$total, mean = m, sd = s,log = T))}



dados = rnorm(1000, mean = 150, sd = 20 ) # gerando os dados
m = seq(125, 175, length = 50) # vetor de médias 
s = seq(10, 60, length = 50)# vetor de desvio padrão 
lgauss = function(m, s, dados) -sum(stats::dnorm(dados, mean = m, sd = s, log = T))  # função de log-ver. neg. 
llikgauss = Vectorize(lgauss, c("m","s")) # vetorização da função
sup.mat = outer(m, s, llikgauss, dados) # cálculo da superfície 
contour(m, s, sup.mat, xlab = "Média", ylab = "Desvio Padrão", nlevels = 20) # gráfico de contorno
abline(v = 150, col = "red", lty = 2) # linha da média 
abline(h = 20, col = "red", lty = 2) # linha do desvio padrão 
persp(m, s, sup.mat, theta = 135, phi = 10, col = "purple") # gráfico de perspectiva

#Coda para comparar modelos
library(car) 
par(mfrow=c(1,2)) 
cont2 <- rbinom(1000, 10, 10)
qqPlot(cont2, distribution="pois", lambda = 10, line ="robust") 
qqPlot(cont2, distribution="nbinom", mu = 10, size = 100, line="robust") 
par(mfrow=c(1,1))
?rbind
?pbinom()

#Anova
#Este experimento tem dois fatores (sexo e tratamento com hormônio), cada um com dois níveis.
#uma ANOVA é usada para testar as seguintes hipóteses nulas a respeito dos efeitos dos fatores sobre a variável resposta, que é a concentração de cálcio plasmático:
#1. Não há efeito do sexo da ave;
#2. Não há efeito do tratamento com o hormônio;
#3. Não há interação entre os dois fatores acima.
calcium <- c(16.5,18.4,12.7,14,12.8,
14.5,11,10.8,14.3,10.0,
39.1,26.2,21.3,35.8,40.2,
32.0,23.8,28.8,25.0,29.3)
hormonio <- factor(rep(c("NO","YES"),each = 10))
sexo <- factor(rep(c("F","M","F","M"),c(5,5,5,5)))

#Uma das maneira de representar o efeito dos fatores sobre o valor esperado da variável-resposta é substituir na expressão acima pela média do grupo experimental de cada observação.
intval <- interaction(hormonio,sexo)
medias <- tapply(calcium,intval,mean)

#Onde é o valor esperado, ou a média, de em cada grupo. Podemos definir uma função de verossimilhança para este modelo da seguinte forma:
LL.m3a <- function(NO.F,YES.F,NO.M,YES.M,sigma){
intval <- interaction(hormonio,sexo)
media <- c(NO.F,YES.F,NO.M,YES.M)[intval]
-sum(dnorm(calcium, mean = media, sd = sigma, log = T))}

#Agora podemos minimizar esta função com o mle2, usando como valores iniciais as médias dos grupos e o desvio-padrão geral
library("bbmle") # carrega o pacote, desde que instalado
m3a <- mle2(LL.m3a, start = c(as.list(medias), sigma = sd(calcium)))
coef(m3a)
medias

