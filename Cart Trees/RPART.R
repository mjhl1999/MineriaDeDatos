#install.packages("tidyverse") 
library(tidyverse)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("caret")
library(caret)
#install.packages("caTools")
library(caTools)
library(dplyr)
#install.packages("e1071")

# Datos

readLines("../Data/wine.data", n = 10)

# Información

readLines("../Data/wine.names", n = 10)


vino <- read.table("../Data/wine.data" , sep = ",", header = FALSE)

vino

readLines("../Data/wine.names", n = 10)

file.copy(from = "../Data/wine.names", to = "../Data/wine_names.txt")

file.show("../Data/wine_names.txt")

summary(vino)

#install.packages("magrittr") 
#library(magrittr)
 
  nombres <- 
  readLines("../Data/wine_names.txt")[58:70] %>% 
  gsub("[[:cntrl:]].*\\)", "", .) %>% 
  trimws() %>% 
  tolower() %>% 
  gsub(" |/", "_", .) %>% 
  # Agregamos el nombre "tipo", para nuestra primera columna con los tipos de vino
  c("tipo", .)
  

nombres


names(vino) <- nombres 

install.packages("dplyr")
library(dplyr)

vino <- vino %>% 
  mutate_at("tipo", factor) 
  

set.seed(1649)

vino_entrenamiento <- sample_frac(vino, .7)

vino_prueba <- setdiff(vino, vino_entrenamiento)

vino_prueba

summary(vino)
summary(vino_prueba)

library(rpart)
rpart(paid ~ ., data=....)

arbol_1 <- rpart(formula = tipo ~ ., data = vino_entrenamiento)

arbol_1

library(rpart.plot)

rpart.plot(arbol_1)

prediccion_1 <- predict(arbol_1, newdata = vino_prueba, type = "class")


install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
library(caret)

confusionMatrix(prediccion_1, vino_prueba[["tipo"]])

set.seed(7439)

vino_entrenamiento_2 <- sample_frac(vino, .7)

vino_prueba_2 <- setdiff(vino, vino_entrenamiento)

arbol_2 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_2)

arbol_2

rpart.plot(arbol_2)

prediccion_2 <- predict(arbol_2, newdata = vino_prueba_2, type = "class")

confusionMatrix(prediccion_2, vino_prueba_2[["tipo"]])

set.seed(8476)
vino_entrenamiento_3 <- sample_frac(vino, .7)

vino_prueba_3 <- setdiff(vino, vino_entrenamiento)

arbol_3 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_3)

prediccion_3 <- predict(arbol_3, newdata = vino_prueba_3, type = "class")

rpart.plot(arbol_3)


confusionMatrix(prediccion_3, vino_prueba_3[["tipo"]])



set.seed(476)
vino_entrenamiento_4 <- sample_frac(vino, .7)

vino_prueba_4 <- setdiff(vino, vino_entrenamiento)

arbol_4 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_4)

prediccion_4 <- predict(arbol_4, newdata = vino_prueba_4, type = "class")

rpart.plot(arbol_4)


confusionMatrix(prediccion_4, vino_prueba_4[["tipo"]])



set.seed(1)
vino_entrenamiento_5 <- sample_frac(vino, .7)

vino_prueba_5 <- setdiff(vino, vino_entrenamiento)

arbol_5 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_5)

prediccion_5 <- predict(arbol_5, newdata = vino_prueba_5, type = "class")

rpart.plot(arbol_5)


confusionMatrix(prediccion_5, vino_prueba_5[["tipo"]])

