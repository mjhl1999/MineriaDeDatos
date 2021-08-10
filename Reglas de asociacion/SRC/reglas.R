library(tidyverse)
datos <- read_csv(file = "../../Data/datos_groceries.csv", col_names = TRUE)
head(datos)
datos %>% filter(id_compra == 14) %>% pull(item)

# IMPORTACIÓN DIRECTA DE LOS DATOS A UN OBJETO TIPO TRANSACTION
# ==============================================================================
library(arules)
transacciones <- read.transactions(file = "../../Data/datos_groceries.csv",
              format = "single", sep = ",",  header = TRUE,
              cols = c("id_compra", "item"),  rm.duplicates = TRUE)
transacciones
colnames(transacciones)[1:5]
rownames(transacciones)[1:5]

# CONVERSIÓN DE UN DATAFRAME A UN OBJETO TIPO TRANSACTION
# ==============================================================================
# 

datos_split <- split(x = datos$item, f = datos$id_compra)
transacciones <- as(datos_split, Class = "transactions")
transacciones


# CONVERSIÓN DE UNA MATRIZ A UN OBJETO TIPO TRANSACTION
# ==============================================================================

datos_matriz <- datos %>%
    as.data.frame() %>%
    mutate(valor = 1) %>%
    spread(key = item, value = valor, fill = 0) %>%
    column_to_rownames(var = "id_compra") %>%
    as.matrix()



transacciones <- as(datos_matriz, Class = "transactions")
transacciones

inspect(transacciones[1:5])
df_transacciones <- as(transacciones, Class = "data.frame")


# Para que el tamaño de la tabla se ajuste mejor, se convierte el dataframe a tibble

as_tibble(df_transacciones) %>% head()
tamanyos <- size(transacciones)
summary(tamanyos)
data.frame(tamanyos) %>%
  ggplot(aes(x = tamanyos)) +
  geom_histogram() +
  labs(title = "Distribución del tamaño de las transacciones",
       x = "Tamaño") +  theme_bw()

frecuencia_items <- itemFrequency(x = transacciones, type = "relative")
frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)

frecuencia_items <- itemFrequency(x = transacciones, type = "absolute")
frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)

soporte <- 30 / dim(transacciones)[1]
itemsets <- apriori(data = transacciones,
            parameter = list(support = soporte, minlen = 1,
            maxlen = 20,  target = "frequent itemset"))

summary(itemsets)

#  
top_20_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_itemsets)

#  
as(top_20_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +   coord_flip() +
  labs(title = "Itemsets más frecuentes", x = "itemsets") +
  theme_bw()

#  

inspect(sort(itemsets[size(itemsets) > 1], decreasing = TRUE)[1:20])

itemsets_filtrado <- arules::subset(itemsets,
                  subset = items %in% "newspapers")
itemsets_filtrado

#  
inspect(itemsets_filtrado[1:10])


itemsets_filtrado <- arules::subset(itemsets,
                  subset = items %ain% c("newspapers", "whole milk"))
itemsets_filtrado

#  
inspect(itemsets_filtrado[1:10])

#  
#  
subsets <- is.subset(x = itemsets, y = itemsets, sparse = FALSE)

#  
sum(subsets)

soporte <- 30 / dim(transacciones)[1]
reglas <- apriori(data = transacciones,  parameter = list(support = soporte,
                  confidence = 0.70,    # Se especifica que se creen reglas
                  target = "rules"))

summary(reglas)

inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))