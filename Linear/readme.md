---
title: "Modeling and prediction for movies"
output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages11, message = FALSE}
library(data.table)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
```

### Load data


```{r load-data10}
load("movies.Rdata")
```



* * *

## Part 1: Data

the data was ramdoly selected from a movies data base 

```{r load-data9}
movies1 <- data.table(movies)
```

* * *

## Part 2: Research question

Factors such as audience scores or IMDB ratings are the same, but IMDB rating has less variation, so it is the best factor to model popularity,
And the factors that matter are the factors that we know before the movie is released, as factors like whether the director was an academy award winner or the number of votes are later.

more basics like runtime or genre so my question is:

Can the genre or the runtime of a movie help us to predict its popularity?

firs some exploratory data for the runtime and the popularity with the IMDB raiting score 

```{r load-data8}
ggplot(movies1, aes(imdb_rating)) + geom_density(kernel = "gaussian") + 
 theme_bw() + xlab("IMDB Rating")

 ggplot(movies1, aes(runtime)) + geom_density(kernel = "gaussian") + 
 theme_bw() + xlab("Runtime")

```
so the mean of runtime is near 105 and the IMDB rating is near 6.5

now we will see the average runtime and IMDB rating by genre

```{r load-data7}
movies1[ , lapply(.SD, mean, na.rm = T), .SDcols = c("imdb_rating", "runtime"), by = "genre"]
```
We se some diference between the average  by genre

* * *

## Part 3: Exploratory data analysis


for EDA first i show you the distribution of the runtime and the IMDB rating by genre

```{r load-data6}
ggplot(movies1, aes(imdb_rating, genre, col = genre)) + 
 geom_violin(scale = "area" ) +
  theme_minimal()

ggplot(movies1, aes(runtime, genre, col = genre)) + 
 geom_violin(scale = "area" ) +
  theme_minimal()
```
fFor the IMDB rating, the genre with the highest average is documentary and comedy the lowest IMDB rating on average,
the runtime of documentary films is the greatest variation between genres and animation is the genre with the least runtime on average

now see the interation between the factors

```{r load-data5}
m3 <- with(movies1, lm(imdb_rating ~ runtime + genre))
ggplot(movies1, aes(runtime, imdb_rating, fill = genre, col = genre)) + 
 geom_point() +
  ylim(0, 11) +
  geom_smooth(model = m3) +
   facet_wrap(~ genre) +
    theme_bw()
```

generally the rating increases with runtime but thats not the case of musicals


* * *

## Part 4: Modeling


for the models we have two simple models of one variable,  one with  the genre and another model with the runtime
and finally compare the models with a full model with both factors by its r square

```{r load-data4}
m1 <- lm(imdb_rating ~ genre, movies1)
m2 <- lm(imdb_rating ~ runtime, movies1)
m3 <- lm(imdb_rating ~ runtime + genre, movies1)
summary(m1)
summary(m2)
summary(m3)
```
the full model have the greatest adjusted r square 

```{r load-data3}
qqnorm(resid(m3))
qqline(resid(m3))
```

for the analysis of the model i only choose two movies "Memento" and "Cloud Atlas" each one with a diferent genre


```{r load-data2}
movies2 <- movies1[,c(1,3,4, 13)]
fit <- movies2[title %in% c("Memento", "Cloud Atlas"),]
fit[, model := ifelse( genre == "Mystery & Suspense", 4.596099 + runtime*0.013243 + 0.424538, ifelse(genre == "Drama", 4.596099 + runtime*0.013243 + 0.609926, 0))]
fit[, diff := imdb_rating - model]

fit[,c(1,4,5, 6)]
```
like we see, its good for just a simple model with only two factors, runtime and genre, the diference between the fit and the data can be as low as 0.0012 like in "Benda Bilili!" and as hight as 3.9 like in "Viva Knievel!"

* * *

## Part 5: Prediction

i choose two movies "Mulan" and "Devil All The Time" for the prediction evaluation 
* * *

```{r load-data1}
devil_all_the_time <- predict(m3, data.frame(runtime = 139, genre = "Drama")) + c(1, -1)*(0.127896 + 0.237685) * qt(.975, 638)
mulan <- predict(m3, data.frame(runtime = 115, genre = "Action & Adventure")) + c(1, -1)*(0.237685) * qt(.975, 638)
mulan
devil_all_the_time
```
the IMDB rating for "Mulan" is 5.4 less than 6.11 for the fit and is less than the minimun for the confidence interval 5.65, but for the movie "Devill All The Time" the IMDB ratin is 7.2 near the 7.04 for the fit and in the confidence intervall (7.764752, 6.328977).


## Part 6: Conclusion

the genre and the runtime of a mive can help us to predict the IMDB raiting of a movie, the popularity in in the IMDB raiting depends on the genre of the movie but documentaries genre in general have the highest reating on average, and the musicals with a long runtime will have less raiting