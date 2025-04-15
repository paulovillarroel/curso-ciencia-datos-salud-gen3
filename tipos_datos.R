## Tipos y estructura de Datos
# Tipos primitivos de datos

# Enteros
x <- 10
y <- -5

# Flotantes (decimales)
z <- 3.14

# Caracteres
nombre <- "Juan"

# Lógicos
es_mayor = TRUE
es_menor = FALSE

# Factores (útiles para variables categóricas)
factor <- factor(c("A", "B", "C"))
# Ajustar los niveles
factor <- factor(c("A", "B", "C"), levels = c("C", "A", "B"))
# Ejemplo con los dias de la semana
dias_semana <- c(
  "Lunes",
  "Martes",
  "Miércoles",
  "Jueves",
  "Viernes",
  "Sábado",
  "Domingo"
)
# Ordenar dias
sort(dias_semana)
# Orden usando factores
dias_ordenados <- factor(
  dias_semana,
  levels = c(
    "Lunes",
    "Martes",
    "Miércoles",
    "Jueves",
    "Viernes",
    "Sábado",
    "Domingo"
  )
)
sort(dias_ordenados)


# Tipos de datos compuestos
# Listas
lista <- c(1, 2.5, "Hola")

# DataFrames
dataframe <- data.frame(nombre = nombre, edad = x)
print(dataframe)

# Diccionarios (R no los tiene de forma nativa)
mi_diccionario <- list(nombre = "Juan", edad = 30, ciudad = "Chile")
# agregar elementos
mi_diccionario$profesion <- "Ingeniero"
# Eliminar elementos
mi_diccionario$profesion <- NULL
# Verificar si un elemento existe en el diccionario
exists("edad", where = mi_diccionario)
names(mi_diccionario)
unlist(mi_diccionario)

# Agregar un nuevo elemento al diccionario
lista_personas <- list(
  list(nombre = "Juan", edad = 30, ciudad = "Chile"),
  list(nombre = "Maria", edad = 25, ciudad = "Argentina")
)

lista_personas <- list(
  Juan = list(nombre = "Juan", edad = 30, ciudad = "Chile"),
  Maria = list(nombre = "Maria", edad = 25, ciudad = "Argentina")
)

lista_personas$Juan

# Ejemplos de Coerción
# Coerción automática
valores <- c(1, "2", TRUE) # R convierte los números y lógicos a caracteres
print(valores)
print(paste("Tipo de datos:", class(valores)))

# Coerción explícita
numero <- 42.5
print(paste("Número original:", numero))
print(paste("Como entero:", as.integer(numero))) # Convierte a entero

factor_nombres <- factor(c("rojo", "verde", "azul"))
print(factor_nombres)
print(paste("Como carácter:", as.character(factor_nombres))) # Convierte factor a carácter
print(paste("Como número:", as.numeric(factor_nombres))) # Convierte factor a número (niveles)

# Estructuras de Datos
# 1. Vectores
vector_logico <- c(TRUE, FALSE, TRUE)
print(vector_logico)

vector_numerico <- c(1, 2, 3, 4, 5)
# Agregar un elemento al final del vector
vector_numerico <- append(vector_numerico, 6)
#Agregar un elemento al principio del vector
vector_numerico <- append(vector_numerico, 0, after = 0)
# Agregar un elemento en una posición específica del vector
vector_numerico <- append(vector_numerico, 3, after = 4)

# Eliminar un elemento del vector
vector_numerico <- vector_numerico[-1]
# Eliminar un elemento en una posición específica del vector
vector_numerico <- vector_numerico[-4]
# Eliminar el útlimo valor del vector
vector_numerico <- vector_numerico[-length(vector_numerico)]
vector_numerico <- head(vector_numerico, -1)

# Vectores de caracteres
vector_caracteres <- c("Paulo", "Maria", "Jaime")
append(vector_caracteres, "Juan")
append(vector_caracteres, c("Pedro", "Josefina"))
append(vector_caracteres, "Ana", 2)
vector_caracteres[vector_caracteres != "Paulo"]

# Agregar 2 vectores
vector_1 <- c(1, 2, 3)
vector_2 <- c(4, 5, 6)
append(vector_1, vector_2)
vector_1 + vector_2 # Ojo con las operaciones metemáticas


# 2. Matrices
matriz_ejemplo <- matrix(1:6, nrow = 2, byrow = TRUE)
print(matriz_ejemplo)

# 3. Data Frames
df_usuarios <- data.frame(
  usuario = c("Alice", "Bob", "Carlos"),
  edad = c(23, 35, 28),
  activo = c(TRUE, FALSE, TRUE)
)
print(df_usuarios)

# 4. Listas
lista_mixta <- list(
  nombre = "ChatGPT",
  puntuacion = 9.8,
  caracteristicas = c("rápido", "preciso", "versátil")
)
print(lista_mixta)

lista_mixta$nombre # Acceder a un dato interno
lista_mixta[1]

# Operadores
# Operadores Aritméticos
x <- 10
y <- 3
print(paste("Suma:", x + y))
print(paste("Resta:", x - y))
print(paste("Multiplicación:", x * y))
print(paste("División:", x / y))
print(paste("Módulo:", x %% y)) # Resto de la división
print(paste("Potencia:", x^y))

# Operadores Relacionales
print(paste("x es igual a y:", x == y))
print(paste("x es mayor que y:", x > y))
print(paste("x es menor o igual que y:", x <= y))

# Operadores Lógicos
a <- TRUE
b <- FALSE
print(paste("a AND b:", a & b))
print(paste("a OR b:", a | b))
print(paste("NOT a:", !a))

# Tidy Data con data frames
# Data frame original
df_tidy <- data.frame(
  id = 1:3,
  nombre = c("Ana", "Juan", "Lucía"),
  valor = c(5.2, 6.1, 7.3)
)
print(df_tidy)

# Manipulación de datos
# Instalara librería Tidyverse
# install.packages("tidyverse")

library(tidyverse) # Cargar librería

# Filtrar usuarios mayores de 30 años
usuarios_filtrados <- subset(df_usuarios, edad > 30)
print(usuarios_filtrados)

# Crear una nueva columna
df_usuarios$salario <- c(2000, 3000, 2500)
print(df_usuarios)

# Ordenar por edad
usuarios_ordenados <- df_usuarios[order(df_usuarios$edad), ]
print(usuarios_ordenados)
