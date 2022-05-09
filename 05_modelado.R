# librerias y funciones
source('libraries_and_functions.R')

# carga de datos
path_dataset <- c('result/dt_train3.RDS', 'result/dt_test3.RDS')

dt_sets <- lapply(X = path_dataset, FUN = readRDS)
names(dt_sets) <- c('dt_train', 'dt_test')

# frecuencia y prop. de las clases de la variable respuesta status
lapply(X = dt_sets, 
       function(x){
           with(
               x, {
                   status <- table(status)
                   # frecuencia de clases
                   print(status)
                   # prop de clases
                   round(prop.table(status), 3)
               })
           }
       )

# balanceo de clases del conjunto de entrenamiento
# (prop. de buenos y malos igual a 80%-20%)
ROSE(
    status ~ ., 
    data = dt_sets$dt_train, 
    N = nrow(dt_sets$dt_train), 
    p = .2, seed = 123
    )$data |> 
    as.data.table() -> dt_balanced

# frec y prop. de las clases de la variable status en dt_balanced
with(
    dt_balanced,{
        p <-table(status)
        
        # frecuencias de clases
        print(p)
        
        # prop de clases
        round(prop.table(p), 3)
    })

# convertimos a valores woe
dt_sets$dt_balanced <- dt_balanced

# bins variables predictivas
lapply(
    X = dt_sets$dt_train[, !'status'], FUN = levels
    ) -> breaks_list

# woe values
woebin(
    dt = dt_sets$dt_train, 
    y = 'status',
    breaks_list = breaks_list
    ) -> bins

# pegamos los woe values a cada dataset
lapply(
    X = dt_sets, 
    FUN = woebin_ply, 
    bins = bins
    ) -> dt_woe

# construccion del modelo logistico
set.seed(123)
glm(
    status ~ ., 
    family = binomial(link = 'logit'), 
    data = dt_woe$dt_balanced
    ) -> logit_model

summary(logit_model)

# predicciones
lapply(
    X = dt_woe, 
    FUN = predict, 
    object =logit_model, 
    type = 'response'
    ) -> predict_model

mapply(
    function(x,y){
        x[
            , prob := y  
        ]},
    dt_woe, predict_model, SIMPLIFY = F
    ) -> dt_woe

# gini model
lapply(
    X = predict_model, 
    FUN = quantile, 
    probs = seq(0,1,.1)
    ) -> breaks

mapply(
    FUN = gini, 
    dt = dt_woe, 
    x = 'prob', 
    y = 'status', 
    breaks = breaks, 
    SIMPLIFY = F
    ) -> gini_model

# otras metricas 
mapply(
    function(x,y){
        perf_eva(
            pred = x,
            label = as.numeric(as.character(y$status)), 
            confusion_matrix = TRUE,
            threshold = 0.181
            )
        }, 
    predict_model, 
    dt_woe, 
    SIMPLIFY = F
    ) -> metrics

# guardamos las modificaciones
path_dataset <- c('result/dt_train_woe.RDS', 
                  'result/dt_test_woe.RDS', 
                  'result/dt_balanced_woe.RDS')

test <- file.exists(
    path_dataset
    ) |> all()

if(!test){
    mapply(FUN = saveRDS, dt_woe, path_dataset)
}



