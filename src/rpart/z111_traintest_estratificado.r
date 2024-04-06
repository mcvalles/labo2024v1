rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")

PARAM <- list()
PARAM$semilla <- 110023
# 999961,999979,999983,999991,999997
#------------------------------------------------------------------------------

# Subir en la jerarquia de carpetas
navigate_up <- function(directory, levels = 1) {
  dir <- directory
  for (i in 1:levels) {
    dir <- dirname(dir)
  }
  return(dir)
}

# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa

# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(
    data, division, agrupa = "",
    campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# Aqui se debe poner la carpeta de la computadora local
# Establezco el Working Directory
#setwd("X:\\gdrive\\austral2023r\\")
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
work_dir <- navigate_up(script_dir, levels = 2)
dataset_dir <- paste(navigate_up(script_dir, levels = 3),"/Datasets/dataset_pequeno.csv", sep = "") 
setwd(work_dir)# Establezco el Working Directory

# cargo los datos
dataset <- fread(dataset_dir)

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

setorder(dataset, mrentabilidad )

# particiono estratificadamente el dataset
# Cambiar por la primer semilla de cada uno !
particionar(dataset, division = c(7, 3), 
  agrupa = "clase_ternaria", seed = PARAM$semilla) # aqui se usa SU semilla


param_basicos <- list(
  "cp" = -1, # complejidad minima
  "minsplit" = 600, # minima cantidad de regs en un nodo para hacer el split
  "minbucket" = 300, # minima cantidad de regs en una hoja
  "maxdepth" = 8 # profundidad máxima del arbol
)

# genero el modelo
# quiero predecir clase_ternaria a partir del resto
# fold==1  es training,  el 70% de los datos
modelo <- rpart("clase_ternaria ~ .",
  data = dataset[fold == 1],
  xval = 0,
  control = param_basicos # aqui van los parametros
)


# aplico el modelo a los datos de testing
prediccion <- predict(modelo, # el modelo que genere recien
  dataset[fold == 2], # fold==2  es testing, el 30% de los datos
  type = "prob"
) # type= "prob"  es que devuelva la probabilidad

# prediccion es una matriz con TRES columnas,
#  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego una columna que es la de las ganancias
dataset[, ganancia := ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]

# para testing agrego la probabilidad
dataset[fold == 2, prob_baja2 := prediccion[, "BAJA+2"]]

# calculo la ganancia en testing  qu es fold==2
ganancia_test <- dataset[fold == 2 & prob_baja2 > 0.025, sum(ganancia)]

# escalo la ganancia como si fuera todo el dataset
ganancia_test_normalizada <- ganancia_test / 0.3

estimulos <- dataset[fold == 2 & prob_baja2 > 0.025, .N]
aciertos <- dataset[fold == 2 & prob_baja2 > 0.025 & clase_ternaria == "BAJA+2", .N]


cat("Testing total: ", dataset[fold == 2, .N], "\n")
cat("Testing BAJA+2: ", dataset[fold == 2 & clase_ternaria == "BAJA+2", .N], "\n")

cat("Estimulos: ", estimulos, "\n")
cat("Aciertos (BAJA+2): ", aciertos, "\n")

cat("Ganancia en testing (normalizada): ", ganancia_test_normalizada, "\n")

#---------------------------------------------------------------------------
dataset <- fread(dataset_dir)
dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

modelo <- rpart("clase_ternaria ~ .",
                data = dtrain,
                xval = 0,
                control = param_basicos # aqui van los parametros
)


prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
  #type = "class"
)

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
accuracy(dapply$prob_baja2, prediccion)

# Kaggle __________________________________________________________
# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

kaggle = dapply[, list(numero_de_cliente, Predicted)]
nrow(kaggle)
bajas2 = kaggle %>% filter(Predicted == 1)
nrow(bajas2)

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_001.csv",
       sep = ","
)

