# librerias y funciones
source('libraries_and_functions.R')

# carga de datos de entrenamiento
dt_train <- readRDS('result/dt_train2.RDS')

# variables categoricas sexo, edo_civil y tipo_res
var_categoricas <- c('sexo', 'edo_civil', 'tipo_res')

mapply(
    FUN = gini,
    list(dt_train),
    var_categoricas, 
    y = 'status', 
    SIMPLIFY = F
    ) -> gini_list

names(gini_list) <- var_categoricas
gini_list

# reagrupacion tipo_res
dt_train[
    , tipo_res2 := {tmp <- ifelse(
        as.character(tipo_res) %in% c('ARRENDADA', 'FAMILIAR', NA),
        as.character(tipo_res),
        'OTRO');
    as.factor(tmp)}
    ] |> gini(x = 'tipo_res2', y = 'status')

# Iv and woe values
with(
    dt_train, 
    list(tipo_res2 = levels(tipo_res2))
    ) -> level_tipo_res2

woebin(
    dt = dt_train, 
    y = 'status', 
    x = 'tipo_res2', 
    breaks_list = level_tipo_res2
    )

# variables numericas discretas
# edad
with(
    dt_train[edad <= 120],
    quantile(x = edad, probs = seq(0, 1, .2), na.rm = T)
    ) -> break_age


gini(
    dt = dt_train, 
    x = 'edad', 
    y = 'status', 
    breaks = break_age
    )

# Iv and woe values
dt_train[
    , edad2 := cut(x = edad,
                   breaks = break_age,
                   include.lowest = T,
                   right = F)
    ]

woebin(
    dt = dt_train, 
    y = 'status', 
    x = 'edad2', 
    breaks_list = list(edad2 = levels(dt_train$edad2))
    )

# antiguedad_tdc
# with(
#     dt_train,
#     quantile(x = antiguedad_tdc, probs = seq(0, 1, .25), na.rm = T)
#     ) -> break_tdc
# 
# gini(
#     dt = dt_train, 
#     x = 'antiguedad_tdc', 
#     y = 'status', 
#     breaks = break_tdc
#     )

# Iv and woe values
# dt_train[
#     , antiguedad_tdc2 := cut(x = antiguedad_tdc,
#                              breaks = break_tdc,
#                              include.lowest = T,
#                              right = F)
# ]
# 
# woebin(
#     dt = dt_train, 
#     y = 'status', 
#     x = 'antiguedad_tdc2', 
#     breaks_list = list(
#         antiguedad_tdc2 = levels(dt_train$antiguedad_tdc2)
#         )
#     )

# nro_moras_obs
break_nro_mora <- c(0,1,2,100)

gini(
    dt = dt_train,
    x = 'nro_moras_obs', 
    y = 'status',
    breaks = break_nro_mora
    )

# Iv and woe values
dt_train[
    , nro_moras_obs2 := cut(x = nro_moras_obs,
                            breaks = break_nro_mora,
                            include.lowest = T,
                            right = F)
    ]

woebin(
    dt = dt_train, 
    y = 'status', 
    x = 'nro_moras_obs2', 
    breaks_list = list(
        nro_moras_obs2 = levels(dt_train$nro_moras_obs2)
        )
    )

# avg_saldo
with(
    dt_train,
    quantile(x = avg_saldo, probs = seq(0, 1, .2), na.rm = T)
    ) -> break_saldo

gini(
    dt = dt_train, 
    x = 'avg_saldo', 
    y = 'status', 
    breaks = break_saldo
    )

# Iv and woe values
dt_train[
    , avg_saldo2 := cut(x = avg_saldo,
                        breaks = break_saldo,
                        include.lowest = T,
                        right = F)
    ]

woebin(
    dt = dt_train, 
    y = 'status', 
    x = 'avg_saldo2', 
    breaks_list = list(
        avg_saldo2 = levels(dt_train$avg_saldo2)
        )
    )

# promedio de suma total depositos en cuenta (avg_dep)
with(
    dt_train,
    quantile(x = avg_dep, probs = seq(0, 1, .2), na.rm = T)
    ) |> round(digits = 4) -> break_dep

gini(
    dt = dt_train, 
    x = 'avg_dep', 
    y = 'status', 
    breaks = break_dep
    )

# Iv and woe values
dt_train[
    , avg_dep2 := cut(x = avg_dep,
                      breaks = break_dep,
                      include.lowest = T,
                      right = F)
    ]

woebin(
    dt = dt_train, 
    y = 'status', 
    x = 'avg_dep2', 
    breaks_list = list(
        avg_dep2 = levels(dt_train$avg_dep2)
        )
    )

# iv and gini summary
vars <- names(dt_train)[ grep(pattern = '2$', x = names(dt_train)) ]
vars2 <- c(vars, 'status')

# levels variables predictivas
lapply(
    X = dt_train[,..vars], FUN = levels
    ) -> breaks_list

woebin(
    dt = dt_train[, ..vars2], 
    y = 'status',
    breaks_list = breaks_list
    ) |> rbindlist() -> total_iv

total_iv[
    ,.(variable, breaks, woe, total_iv)
    ][
        order(-total_iv)
    ] -> total_iv

# total_iv[
#     , lapply(.SD, FUN = unique), 
#     .SDcols = c('variable', 'total_iv')
#     ][
#         order(-total_iv)
#         ] -> total_iv

# creacion de bins en dataset test
dt_test <- readRDS('result/dt_test2.RDS')

dt_test[
    , `:=` (tipo_res2 = {tmp <- ifelse(
            as.character(tipo_res) %in% c('ARRENDADA', 'FAMILIAR', NA),
            as.character(tipo_res),
            'OTRO'); 
            as.factor(tmp)},
            edad2 = cut(x = edad,
                        breaks = break_age,
                        include.lowest = T,
                        right = F),
            # antiguedad_tdc2 = cut(x = antiguedad_tdc,
            #                       breaks = break_tdc,
            #                       include.lowest = T,
            #                       right = F),
            nro_moras_obs2 = cut(x = nro_moras_obs,
                                 breaks = break_nro_mora,
                                 include.lowest = T,
                                 right = F),
            avg_saldo2 = cut(x = avg_saldo,
                             breaks = break_saldo,
                             include.lowest = T,
                             right = F),
            avg_dep2 = cut(x = avg_dep,
                           breaks = break_dep,
                           include.lowest = T,
                           right = F)
            )
    ] -> dt_test

# guardamos las modificaciones
dt_sets <- list(
    dt_train[, ..vars2], dt_test[, ..vars2], total_iv
    )

path_dataset <- c('result/dt_train3.RDS', 
                  'result/dt_test3.RDS', 
                  'result/total_iv.RDS')

test <- file.exists(
    path_dataset
    ) |> all()

if(!test){
    mapply(FUN = saveRDS, dt_sets, path_dataset)
    }
