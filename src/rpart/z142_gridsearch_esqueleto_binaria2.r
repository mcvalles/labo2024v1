# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(999961,999979,999983,999991,999997, 100019, 110023, 120041, 130043, 140053, 100023, 100037, 100073, 100097, 116959, 101599, 388177, 254747, 379289, 154027)


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
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria", seed = semilla)

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_binaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing que es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "suma"] > 0.025,
      ifelse(clase_binaria == "suma", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 1 # en Windows este valor debe ser 1 - original es 5
  )

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
work_dir <- navigate_up(script_dir, levels = 2)
dataset_dir <- paste(navigate_up(script_dir, levels = 3),"/Datasets/dataset_pequeno.csv", sep = "") 
setwd(work_dir)# Establezco el Working Directory

# cargo los datos
dataset <- fread(dataset_dir)

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]
# agrego la clase_binaria y elimino la clase_ternaria
dataset[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "suma", "resta")]
dataset[, clase_ternaria := NULL]


# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_binaria.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( max_depth = integer(),
                              min_split = integer(),
                              ganancia_promedio = numeric() )


# itero por los loops anidados para cada hiperparametro

for (v_cp in c(-1, -0.75, -0.5, -0.25)) {
  for (vmax_depth in c(4, 6, 8, 10, 12, 14)) {
    for (vmin_bucket in c(500, 400, 300, 200)) {
    # notar como se agrega

    # vminsplit  minima cantidad de registros en un nodo para hacer el split
    param_basicos <- list(
      "cp" = v_cp, # complejidad minima
      "minsplit" = vmin_bucket * 2,
      "minbucket" = vmin_bucket, # minima cantidad de registros en una hoja
      "maxdepth" = vmax_depth
    ) # profundidad máxima del arbol

    # Un solo llamado, con la semilla 17
    ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

    # agrego a la tabla
    tb_grid_search <- rbindlist( 
      list( tb_grid_search, 
            list( vmax_depth, vmin_bucket, ganancia_promedio) ) )

  }

  # escribo la tabla a disco en cada vuelta del loop mas externo
  Sys.sleep(2)  # espero un par de segundos

  fwrite( tb_grid_search,
          file = archivo_salida,
          sep = "\t" )
  }
  }
