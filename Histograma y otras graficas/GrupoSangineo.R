#Se busca crear un dataset sobre grupos sanguineos y mostrar su distribución mediante
#una gráfica de pastel. Y posteriormente sobre el archivo grupoA.csv

#Creamos nuestro dataset en la variable  en la variableGr_Sang
Sexo_A <- factor(c("Mujer","Hombre","Mujer","Mujer","Mujer","Hombre","Mujer","Hombre","Hombre","Mujer","Mujer","Hombre","Hombre","Mujer","Mujer","Hombre","Mujer","Mujer","Mujer","Mujer"), levels = c("Mujer", "Hombre"))
Edad_A <- c(25, 30, 28, 20, 23, 22, 22, 22, 21, 21, 22, 20, 22, 29, 29, 21, 30, 21, 22, 23)
Estatura_A <- c(1.82, 1.83, 1.78, 1.79, 1.80, 1.90, 1.79, 1.83, NA, 1.65, 1.73, 1.79, 1.80, 1.77, 1.69, 1.75, 1.66, NA, 1.79, 1.80)
Gr_Sang_A <- c("A","B","A","AB","O","A","B","A","B","AB","A","B","O","O","A","B","AB","B","B","B")
GrA <- data.frame (Sexo_A,Edad_A, Estatura_A, Gr_Sang_A)
GrA

pie(table(GrA$Gr_Sang_A),
    col = c("blue", "yellow", "pink", "green"),
    main = "GrÃ¡fica de grupos Sanguineo (Grupo A)")

install.packages("readr") #para poder leer los csv

library(readr) 

file.choose()

ruta_grupoA <- "/home/miren/Documentos/MineriaDeDatos/Histograma y Grafica de Barras/grupoA.csv"

grupoA <- read_csv(ruta_grupoA)

grupoA$Gr_Sang

install.packages("RColorBrewer") #paletas de colores 
library("RColorBrewer")
display.brewer.all()             #visualizar paletas


pie(table(grupoA$Gr_Sang),
    col=brewer.pal(n = 4, name = "Pastel1"),
    main = "Grafica de grupos Sanguineo (Grupo A)",)


ruta_grupoB <- "/home/miren/Documentos/MineriaDeDatos/Histograma y Grafica de Barras/grupoB.csv"

grupoB <- read_csv(ruta_grupoB)

pie(table(grupoB$Gr_Sang),
    col=brewer.pal(n = 4, name = "Pastel1"),
    main = "Grafica de grupos Sanguineo (Grupo B)")