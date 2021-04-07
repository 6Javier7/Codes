znum <- 37

if (num > 100) {
	print("Greater")
} else {
	print("Smaler")
}
print("done")

sing <- function(num) {
	 	if (num > 0) {
          print("Positive")
	 	} else if (num < 0) {

	 		print("Negative")

	 	} else if (num == 0 ) {

	 		print("Is cero")
	 	} else if (is.character(num)) {
	 		print("is a character")
	 	} else{
	 		print("no se")
	 	}

	 	
}

if (3 > 2 & 3 < 10) { #se puede poner preposiciones logicas

	print("Both part are true")
} else {

	print("at least one part is false")
}


print_word <- function(sentence) {

	for (word in sentence){
		print(word)
	}
}

for (variable in collection) {
  do things with variable
}

len <- 0

vowels <- c("a", "e", "i", "o", "u")

for (v in vowels) {
	 len <- len + 1
}

#Note that a loop variable is just a variable that’s being used to record progress in a loop. 
#It still exists after the loop is over, and we can re-use variables previously defined as loop variables as well:

letter <- "z"
for (letter in c("a", "b", "c")) {
  print(letter)
}

for (item in list_of_items) {
  do_something(item)
}

#Can have many rows in a loop body
volumes = c(1.6, 3, 8)
for (volume in volumes){
   mass <- 2.65 * volume ^ 0.9
   mass_lb <- mass * 2.2
   print(mass_lb)
}

#Loop over values only let’s you access values from a single list
#Loop over index let’s you access values from multiple lists

b0 <- c(2.65, 1.28, 3.29)
b1 <- c(0.9, 1.1, 1.2)
for (i in seq_along(volumes)){
   mass <- b0[i] * volumes[i] ^ b1[i]
   print(mass)


 get_counts <- function(data_file_name){
    file <- read.csv(data_file_name)
    count <- nrow(file)
    return(count)
  }
	
  results <- vector(length = length(collar_data_files)) #Vector para guardar los valores
  for (i in seq_along(collar_data_files){
    results[i] <- get_numbers(collar_data_files[i]) #Quedan guardados los valores en el vector
  }

  mass <- function(length) {
  	  mass <- 0.73 * length ^ 3.63
  }

  results <- c()

  for (i in seq_along(theropoda_lengths)) {
            results[i] <- mass(theropoda_lengths[i])
  }

  #Loops Fucntions
  x <- list(a = 1:5, b = rnorm(10))
  #lapply(x, fun, ...otros argumentos de la funcion)
  lapply(x, mean) #Always return a list
  sapply(x, mean) #like lapply but returns a vector
  lapply(1:4, runif) #devuelve una lista con 1, 2, 3 y 4 variables uniformes
  lapply(1:4, runif, min = 0, max = 10) #se pueden cambiar los argumentos de la funcion

  #applay(array, Margun, fun, ...otros argumentos de la funcion) 
  m <- matrix(rnorm(200), 20, 10)
  apply(m, 1, quantile, probs = c(.25, .75)) #muestra los cuantiles de cada columna
  apply(m, 1, mean) #saca la media a cada columna
  colMeans(m) #lo mismo mas corto
  apply(m, 2, sum) #la sumatoria a las filas
  rowSums(m) #lo mismo mas cort
o
  #mapply es para ver los argumentos de mas de una lista 
  #mapply(fun, x, y, ... o mas listas, ...otros argumentos de la funcion)
  mapply(rnorm, n = 1:5, mean = 1:5, sd = 2) # una distribucion normal con media 2, 3 con media 3 y...

  # tapply(x, index, fun, ...otros argumentos de la funcion) index son los grupos dentro de x
  # se usa cuando tienes vectores con valores de diferentes grupos
  v <- c(runif(10), rnorm(10), rgamma(10, 3, 5))# este vector tiene variables de tres grupos 10 uniformes, 10 normales y 10 gammas
  f <- gl(3, 10) #te crea los factores que dividen los grupos de el vector
  tapply(v, f, mean)

  # split divide el vector x en grupos
  split(v, f) # te devuelve una lista que se puede usar con lapply o spply
  lapply(split(v, f), mean)

