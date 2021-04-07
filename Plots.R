#Plots
require(tigerstats)

#Basic Scatterplot

xyplot(fastest~GPA,data=m111survey,
       xlab="grade point average",
       ylab="speed (mph)",
       main="Fastest Speed Ever Driven,\nby Grade Point Average")

#Including Smoothers
##Adding a Regression Line
xyplot(fastest~GPA,data=m111survey,
       xlab="grade point average",
       ylab="speed (mph)",
       main="Fastest Speed Ever Driven,\nby Grade Point Average",
       type=c("p","r"))

##Adding a Loess Curve
xyplot(fastest~GPA,data=m111survey,
       xlab="grade point average",
       ylab="speed (mph)",
       main="Fastest Speed Ever Driven,\nby Grade Point Average",
       type=c("p","smooth"))

##Grouping

xyplot(fastest~GPA,data=m111survey,
       groups = sex,
       auto.key = TRUE,
       xlab="grade point average",
       ylab="speed (mph)",
       main="Fastest Speed Ever Driven,\nby Grade Point Average",
       type=c("p","r"))

##Types of Points

xyplot(fastest~GPA,data=m111survey,
       xlab="grade point average",
       ylab="speed (mph)",
       main="Fastest Speed Ever Driven,\nby Grade Point Average",
       pch=19)

#Colors
##Single Color
xyplot(fastest~GPA,data=m111survey,
       xlab="grade point average",
       ylab="speed (mph)",
       main="Fastest Speed Ever Driven,\nby Grade Point Average",
       pch = 19,
       col = "red")

##Color Schemes
xyplot(fastest~GPA,data=m111survey,
       groups = sex,
       auto.key = TRUE,
       par.settings = list(superpose.symbol = list(col = c("blue","red"),
                                                   pch = 19),
                           superpose.line = list(col = c("blue","red"),
                                                 lwd = 2)),
       xlab="grade point average",
       ylab="speed (mph)",
       main="Fastest Speed Ever Driven,\nby Grade Point Average",
       type=c("p","smooth"))


xyplot(mpg~disp|transmission,data=mtcars,
 scales=list(cex=.8, col="red"),
 panel=panel.smoother,
 xlab="Displacement", ylab="Miles per Gallon",
 main="MPG vs Displacement by Transmission Type",
 sub = "Dotted lines are Group Means", aspect=1) 

library(lattice)
colors <- "darkgreen"
symbols <- c(1:12)
linetype <- c(1:3)
key.species <- list(title="Plant",
 space="right",
 text=list(levels(CO2$Plant)),
 points=list(pch=symbols, col=colors))
xyplot(uptake~conc|Type*Treatment, data=CO2,
 group=Plant,
 type="o",
 pch=symbols, col=colors, lty=linetype,
 main="Carbon Dioxide Uptake\nin Grass Plants",
 ylab=expression(paste("Uptake ",
 bgroup("(", italic(frac("umol","m"^2)), ")"))),
 xlab=expression(paste("Concentration ",
 bgroup("(", italic(frac(mL,L)), ")"))),
 sub = "Grass Species: Echinochloa crus-galli",
 key=key.species) 

graph1 <- histogram(~height | voice.part, data = singer,
 main = "Heights of Choral Singers by Voice Part")
graph2 <- bwplot(height~voice.part, data = singer)
plot(graph1, position=c(0, .3, 1, 1))
plot(graph2, position=c(0, 0, 1, .3), newpage=FALSE) 


##From and To

densityplot(~kkardashtemp,data=imagpop,
            plot.points=FALSE)

library(lattice)
mtcars$transmission <- factor(mtcars$am, levels=c(0, 1),
 labels=c("Automatic", "Manual")

colors = c("red", "blue")
lines = c(1,2) #1
points = c(16,17)

key.trans <- list(title="Trasmission",
 space="bottom", columns=2, #2
 text=list(levels(mtcars$transmission)),
 points=list(pch=points, col=colors),
 lines=list(col=colors, lty=lines),
 cex.title=1, cex=.9)

densityplot(~mpg, data=mtcars,
 group=transmission,
 main="MPG Distribution by Transmission Type",
 xlab="Miles per Gallon",
 pch=points, lty=lines, col=colors,
 lwd=2, jitter=.005,      #3
 key=key.trans) 

histogram( ~ height | voice.part, data = singer,
    xlab = "Height (inches)", type = "density",
    scales = list(x = list(at = seq(60, 80, by = 2), rot = 45))) #Cambia la escala y rota los numeros

#Barras

# Simple Bar Plot 
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", 
   xlab="Number of Gears")

# Simple Horizontal Bar Plot with Added Labels 
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", horiz=TRUE,
  names.arg=c("3 Gears", "4 Gears", "5 Gears"))

# Stacked Bar Plot with Colors and Legend
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
  xlab="Number of Gears", col=c("darkblue","red"),
  legend = rownames(counts)) #muestra las proporciones de los grupos

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
  xlab="Number of Gears", col=c("darkblue","red"),
  legend = rownames(counts), beside=TRUE)#Barras de diferentes colores segun el grupo

# plot
barplot( c(2,5,4,6) , density=c(5,10,20,30) , angle=c(0,45,90,11) , col="brown" , names.arg=c("A","B","C","D")  )#Cambia la textura

#Para agregarle texto 
# Add the text 
text(my_bar, data$average+0.4 , paste("n = ",data$number,sep="") ,cex=1)  
 
#Legende
legend("topleft", legend = c("Alone","with Himself","With other genotype" ) , 
     col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
     bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))

#Linear model

sp <- read.delim('http://www.bio.ic.ac.uk/research/mjcraw/statcomp/data/splityield.txt')

with(sp, interaction.plot(density, fertilizer, yield))
mtext(expression("fertilizer"%*%"density interaction"), side=3, line=.25)

xyplot(yield~density|fertilizer + irrigation, data=sp, groups=block, type='o')
xyplot(yield~density|fertilizer + irrigation, data=sp, groups=block, type='o', panel=function(x, y, subscripts, groups, ...){
panel.superpose(x, y, subscripts, groups, col='grey70', ...)})#lineas grises

#I obtain predictions of the marginal means from the lme model using the predict function with the level=0 argument.
sp$pred <- predict(out1a,level=0)

#I add the subscripts argument to the panel function and then plot the means with the panel.xyplot 
#function using subscripts to select only those predicted means that should appear in the panel currently being drawn.

xyplot(yield~density|fertilizer + irrigation, data=sp, groups=block, type='o', panel=function(x, y, subscripts, groups, ...){
panel.superpose(x, y, subscripts, groups, col='grey70', ...)
panel.xyplot(x[1:3], sp$pred[subscripts][1:3], col=2, pch=8, type='o')})

#Finally I assign the graph to an object and use the useOuterStrips function from the latticeExtra package to place panel labels on the left and on the top.
xyplot(yield~density|fertilizer + irrigation, data=sp, groups=block, type='o', panel=function(x, y, subscripts, groups, ...){
panel.superpose(x, y, subscripts, groups, col='grey70', ...)
panel.xyplot(x[1:3], sp$pred[subscripts][1:3], col=2, pch=8, type='o')}) -> g1
library(latticeExtra)
useOuterStrips(g1)

turtles <- read.table('ecol 563/turtles.txt', header=T)
xyplot(plasma~treat|sex, groups=subject, type='o', data=turtles)
turtles$treat <- factor(turtles$treat, levels=c('fed', 'fast10', 'fast20'))
xyplot(plasma~treat|sex, groups=subject, type='o', data=turtles)

xyplot(plasma~treat|sex, groups=subject, data=turtles,type='o', panel=function(x, y, subscripts, groups, ...){
panel.superpose(x, y, subscripts, groups, col='grey70',...)
# identify the sex that is currently being plotted 
sex <- turtles$sex[subscripts][1]
# choose different means depending on the sex
if(sex=='male') panel.lines(1:3, males, type='o', pch=8, lwd=2, col=1) else  panel.lines(1:3, fems, type='o', pch=8, lwd=2, col=1)})


xyplot(plasma~treat|sex+factor(subject2), data=turtles, type='o', layout=c(4,2), panel=function(x, y, subscripts, ...){
panel.xyplot(x, y, type='o', col='grey60')
sex <- turtles$sex[subscripts][1]
# marginal means separately by sex
if(sex=='male') panel.lines(1:3, males, pch=8, lwd=2, col=1) else  panel.lines(1:3, fems, pch=8, lwd=2, col=1)
# conditional means
panel.lines(x, turtles$pred[subscripts], col=4, lty=2)
})

xyplot(plasma~factor(treat, levels=levels(treat), labels=c('well\nfed', '10-day\nfast', '20-day\nfast'))| factor(subject2)+sex, data=turtles, xlab='Treatment', ylab='Plasma protein (mg/l)', type='o', layout=c(4,2), panel=function(x, y, subscripts, ...){
panel.xyplot(x,y,col='grey30')
# plot marginal means
sex <- turtles$sex[subscripts][1]
if(sex=='male') panel.lines(1:3, males, lwd=2, col=2) else panel.lines(1:3, fems, lwd=2, col=2)
# conditional means (include random effects)
panel.lines(x, turtles$pred[subscripts], col=4, lty=2)
}, key=list(x=.78, y=-.2, corner=c(0,0), lines=list(col=c('grey30',4,2), pch=c(1,NA,NA), lty=c(1,2,1), lwd=c(1,1,2), size=2, type=c('p','l','l'), divide=2, cex=.85), text=list(c('observed', 'conditional mean', 'marginal mean'), cex=.75), border=T))

slugs <- read.table( 'http://www.bio.ic.ac.uk/research/mjcraw/statcomp/data/slugsurvey.txt', header=TRUE)
#stacked bar plot
barplot(table(slugs$slugs, slugs$field))
#separate bar plots
barplot(table(slugs$slugs, slugs$field), beside=T)

barplot(t(table(slugs$slugs, slugs$field)), beside=T, legend.text=T)

barchart(Freq~Var1|Var2, data=slugtable, xlab='Count category', origin=0, col='grey')

#We can get almost the same display with the xyplot function by using the type='h' 
#option to draw vertical lines from the plotted points to the x-axis with lwd (line width) set to a large value to produce bars. 
#To get square ends on the bars I use lineend=1. For xyplot we need the numerical version of Var1 rather than the factor version. Fig. 10 shows the result.

xyplot(Freq~Var1.num|Var2, data=slugtable, xlab='Count category', type='h', lineend=1, lwd=10, col='grey')




#ggplot

#Puntos

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) #le cambia el tono a los puntos

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")#todos los puntos azules

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)#dvide el plot

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)#los divide en esas variables

#Lineas

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))#con area

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
              
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
    
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE) #solo muestra la linea de subcompact

#barras

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut)) #cAMBIAN DE COLOR LOS MARGENES de las barras

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) #Cambia de color el fondo de las barras

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity)) #muestra las barras con subbarras segun el fill que se tenga

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity") #Muy bacano

  ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "stack") #Muy bacano

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")#muestra las proporciones de cada subclase sin importar el valor absoluto

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")#Como un Histograma cada subclase es una barra aparte

ggplot(data = Generos) + geom_bar(mapping = aes(x = Factores, fill = Especies), alpha = 1/5, width = 0.8, position = "fill") + theme_classic() + scale_fill_grey()  

data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5),
  sd=c(1,0.2,3,2,4)
)

ggplot(data) +
    geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)#Pones la desviacion en las barras


#Otras Barras

# 1: uniform color. Color is for the border, fill is for the inside
ggplot(mtcars, aes(x=as.factor(cyl) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )
 
# 2: Using Hue
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + geom_bar( ) +
  scale_fill_hue(c = 40)
 
# 3: Using RColorBrewer
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + geom_bar( ) +
  scale_fill_brewer(palette = "Set1")
 
# 4: Using greyscale:
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + geom_bar( ) +
  scale_fill_grey(start = 0.25, end = 0.75)
 
# 5: Set manualy
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue") )



#Mapas

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

#Coordenadas polares 

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip() #TE mueve la barra
bar + coord_polar()