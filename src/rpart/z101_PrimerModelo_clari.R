# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# Librerias _______________________________________________________
require("data.table")
require("rpart")
require("rpart.plot")

library(data.table)
library(dplyr) # to perform some data wrangling tasks
library(rpart) # to fit decision trees without tuning
library(rpart.plot) # to plot our decision trees
library(Metrics) #to assess the performance of our models
library(mlr) # to train our model’s hyperparameters
library(ggplot2) # for general plots we will do
library(plotly) # for 3-D plots
library(sampling)
library(survey)
#__________________________________________________________________
# Funciones _______________________________________________________

# Subir en la jerarquia de carpetas
navigate_up <- function(directory, levels = 1) {
  dir <- directory
  for (i in 1:levels) {
    dir <- dirname(dir)
  }
  return(dir)
}

# Función para identificar columnas con valores nulos y contarlos
analizar_nulos <- function(dt) {
  # Asegurarse de que el input es un data.table
  if (!is.data.table(dt)) {
    stop("El input debe ser un data.table")
  }
  
  # Paso 1: Encontrar columnas con valores nulos y contarlos
  nulos_df <- sapply(dt, function(x) sum(is.na(x))) # Cuenta los NAs por columna
  nulos_df <- data.frame(columna = names(nulos_df), n_nulos = nulos_df) # Convertir a data.frame
  nulos_df <- nulos_df[nulos_df$n_nulos > 0, ] # Filtrar columnas con al menos un NA
  
  # Paso 2: Obtener los nombres de las columnas con valores nulos
  columnas_con_nulos <- names(dt)[colSums(is.na(dt)) > 0]
  
  # Devolver una lista con ambos componentes
  list(
    columnas_nulos_df = nulos_df,
    columnas_con_nulos = columnas_con_nulos
  )
}

# Función para identificar columnas con valores infinitos y contarlos
analizar_inf <- function(dt) {
  # Asegurarse de que el input es un data.table
  if (!is.data.table(dt)) {
    stop("El input debe ser un data.table")
  }
  
  # Paso 1: Encontrar columnas con valores infinitos y contarlos
  inf_df <- sapply(dt, function(x) sum(is.infinite(x))) # Cuenta los infs por columna
  inf_df <- data.frame(columna = names(inf_df), n_inf = inf_df) # Convertir a data.frame
  inf_df <- inf_df[inf_df$n_inf > 0, ] # Filtrar columnas con al menos un inf
  
  # Paso 2: Obtener los nombres de las columnas con valores infinitos
  columnas_con_infinitos <- inf_df$column_name[inf_df$n_inf > 0]
  
  # Devolver una lista con ambos componentes
  list(
    columnas_inf_df = inf_df,
    columnas_con_infinitos = columnas_con_infinitos
  )
}

#__________________________________________________________________
# Working Directory________________________________________________
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
work_dir <- navigate_up(script_dir, levels = 2)
dataset_dir <- paste(navigate_up(script_dir, levels = 3),"/Datasets/dataset_pequeno.csv", sep = "") 

setwd(work_dir)# Establezco el Working Directory
#__________________________________________________________________
# Dataset__________________________________________________________

dataset <- fread(dataset_dir)
ncol(dataset)
#__________________________________________________________________
# Limpieza Datos __________________________________________________

# Eliminar Nulos --------------- 

nulos = analizar_nulos(dataset)
nulos$columnas_nulos_df %>% arrange(n_nulos)
nulos$columnas_con_nulos


dataset[,-(nulos$columnas_con_nulos), with = F] %>% dim()
zero_columns = dataset[,nulos$columnas_con_nulos, with = F] %>% colnames()

# Eliminar columnas nulas
for (col in zero_columns) {
  dataset[, (col) := NULL]
}

ncol(dataset)

# Eliminar Infinitos --------------- 

infinitos = analizar_inf(dataset)
infinitos$columnas_inf_df %>% arrange(n_nulos)
infinitos$columnas_con_infinitos

dataset[,-(infinitos$columnas_con_infinitos), with = F] %>% dim()
inf_columns = dataset[,infinitos$columnas_con_infinitos, with = F] %>% colnames()

# Eliminar columnas nulas
for (col in inf_columns) {
  dataset[, (col) := NULL]
}

ncol(dataset)

# Combinar Columnas similares -

dataset[, cseguros := as.integer(rowSums(.SD) > 0), .SDcols = c("cseguro_accidentes_personales", "cseguro_auto", "cseguro_vida", "cseguro_vivienda")]
dataset[, c("cseguro_accidentes_personales", "cseguro_auto", "cseguro_vida", "cseguro_vivienda") := NULL]

#dataset[, numero_de_cliente := NULL]
baja1 <- dataset %>% filter(clase_ternaria == 1)
baja2 <- dataset %>% filter(clase_ternaria == 2)
continua <- dataset %>% filter(clase_ternaria == 0)

# Muestra ---------------

# Calculate the number of rows for training and test data
data_julio <- dataset[foto_mes == 202107]
total_rows <- nrow(data_julio)
#train_size <- round(0.7 * total_rows)  # 70% for training
#test_size <- total_rows - train_size   # Remaining 30% for testing

strata_counts <- table(data_julio$clase_ternaria)
weights <- 1 / strata_counts[data_julio$clase_ternaria]
weights <- as.numeric(weights)

# Create a survey design object with weights
survey_design <- svydesign(ids = data_julio$numero_de_cliente, strata = ~clase_ternaria, data = data_julio, weights = weights)

# Set the seed for reproducibility
set.seed(123)

# Sample 70% of the data for training using the existing survey design object
train_indices <- survey_design$variables$numero_de_cliente[sample(nrow(data_julio), size = 0.7 * nrow(data_julio))]

# Create training dataset
train_data <- data_julio %>% filter(numero_de_cliente %in% train_indices)

# Create testing dataset
test_data <- data_julio %>% filter(!numero_de_cliente %in% train_indices)

# Display the sizes of train and test data
cat("Train data size:", nrow(train_data), "\n")
cat("Test data size:", nrow(test_data), "\n")

# Perform stratified sampling for training data
#train_data <- strata(data_julio, stratanames = c("clase_ternaria"), size = strata_sizes_train, method = "srswor")

# Perform stratified sampling for test data
#test_data <- strata(data_julio, stratanames = c("clase_ternaria"), size = strata_sizes_test, method = "srswor")

# Display the first few rows of the sampled training and test data
#head(train_data)
#head(test_data)

# ------------------------

#__________________________________________________________________

# Modelo __________________________________________________________
id_clientes_train <- dataset[foto_mes == 202107]$numero_de_cliente
id_clientes_apply <- dataset[foto_mes == 202109]$numero_de_cliente
dataset[, numero_de_cliente := NULL]
dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo
#dtrain <- as.data.frame(train_data)
#dapply <- as.data.table(test_data)

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -1.5, # Original -0.3 esto significa no limitar la complejidad de los splits
        minsplit = 600, # Original 0 - minima cantidad de registros para que se haga el split
        minbucket = 200, # Original 1 - tamaño minimo de una hoja
        maxdepth = 6 # Original 3
) # profundidad maxima del arbol

# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
        #type = "class"
)
head(prediccion)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
accuracy(dapply$prob_baja2, prediccion)
#accuracy(dapply$clase_ternaria, prediccion)

#__________________________________________________________________
#Optimizacion Hiperparametros _____________________________________
getParamSet("classif.rpart")

d.tree.params <- makeClassifTask(
  data=dtrain, 
  target='clase_ternaria',
)

modelo <- makeLearner("classif.rpart")
modelo <- train(modelo, task = d.tree.params)

# Define a function to calculate accuracy
#acc_function <- function(task, model, pred, extra.args) {
#  prediccion <- predict(object = modelo, newdata = dapply, type = "prob")
#  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
  # Calculate accuracy
#  accuracy <- accuracy(dapply$prob_baja2, prediccion)
#  return(accuracy)
#}

#custom_acc <- makeMeasure(id = "custom_acc", 
#                         name = "Custom Accuracy", 
#                         minimize = TRUE,
#                         properties = c("regr", "response"),
#                         fun = acc_function)

#f = function(task, model, pred, extra.args) {
#  sum((pred$data$response - pred$data$truth)^2)
#}

# Parametro unico
#param_grid <- makeParamSet(makeDiscreteParam('maxdepth', values=1:30))

# Varios Parametros
param_grid_multi <- makeParamSet( 
  makeDiscreteParam('maxdepth', values=1:20),
  makeDiscreteParam('minbucket', values=1:20),
  makeDiscreteParam('minsplit', values=1:20)
)

control_grid = makeTuneControlGrid()
resample = makeResampleDesc("CV", iters = 3L)

set.seed(123)
dt_tuneparam <- tuneParams(learner='classif.rpart', 
                           task=d.tree.params, 
                           resampling = resample,
                           #measures = custom_acc,
                           measures = bac,
                           par.set=param_grid_multi, 
                           control=control_grid, 
                           show.info = TRUE)

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam, partial.dep = TRUE)
print(result_hyperparam)

ggplot(
  data = result_hyperparam$data,
  aes(x = maxdepth, y=acc.test.mean)
) + geom_line(color = 'darkblue')

best_parameters = setHyperPars(
  makeLearner('classif.rpart'), 
  par.vals = dt_tuneparam$x
)

print(best_parameters)
best_model = train(best_parameters, d.tree.params)

results <- predict(best_model, task = d.tree.params)$data
accuracy(results$truth, results$response)
#__________________________________________________________________
# Kaggle __________________________________________________________
# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, numero_de_cliente := id_clientes_apply]
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

#__________________________________________________________________
