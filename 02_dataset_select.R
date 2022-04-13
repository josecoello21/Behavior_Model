# librerias y funciones
source('libraries_and_functions.R')

# carga de base tdc
base_tdc <- readRDS(file = 'data_sets_rds/tdc_abr21.RDS')

# variables de interes
base_tdc[
    , cedula := paste0(.SD, collapse = ''),
    by = seq_along(T5CDTI),
    .SDcols = c('T5CDTI', 'T5UNNB')
][
    , .('cedula' = cedula, 'nro_tarjeta' = T5NRTA, 'bine' = T5BINE,
        'sexo' = T5CDSE, 'edo_civil' = T5DSEC, 'tipo_res' = T5DSTR,
        'fecha_nac' = T5PWNB, 'fecha_emi' = T5MQNB, 'nro_dep' = T5T3NB, 
        'vc_2020' = H3HSNB, 'vc_2021' = H3HTNB)
] -> base_tdc

# vectores de comp 12 caracteres 2020 y 4 caracteres 2021, solo tdc
base_tdc[
    , `:=` (vc_2020 = gsub(pattern = '\\s',
                           replacement = '',
                           x = vc_2020
                           ),
            vc_2021 = gsub(pattern = '\\s',
                           replacement = '',
                           x = vc_2021
                           )
            )
    ][
        nchar(vc_2020) == 12 & nchar(vc_2021) == 4
    ] -> base_tdc

# reformateo de datos
base_tdc[
    , `:=` (sexo = factor(sexo),
            edo_civil = factor(edo_civil),
            tipo_res = factor(tipo_res),
            fecha_nac = ymd(fecha_nac),
            fecha_emi = ymd(fecha_emi)
            )
]

# Exclusion de clientes oct20 (fecha divisoria) que se encuentran incumplidos
incumplido <- '[3456789GHIJKLM]'

base_tdc[
    , mora_oct_2020 := substr(x = vc_2020, start = 10, stop = 10)
][
    !grepl(pattern = incumplido, x = mora_oct_2020)
] -> base_tdc

# Etiqueta malos/buenos en el periodo de comportamiento nov20-abr21
base_tdc[
    , `:=` (mora_nov20_dic20 = {tmp <- substr(x = vc_2020, 
                                              start = 11, 
                                              stop = 12); tmp},
            mora_nov20_abr21 = {tmp2 <- paste0(tmp, vc_2021); tmp2},
            status = {tmp3 <- grepl(pattern = incumplido, x = tmp2); 
                      factor(as.integer(tmp3))
                      }
            )
]

# Edad y tiempo de la tarjeta
fecha_obs <- ymd('2020-04-30')

base_tdc[
    , `:=` (edad = {tmp_age <- interval(start = fecha_nac, end = fecha_obs);
                    tmp_age2 <- as.period(tmp_age); 
                    lubridate::year(tmp_age2)
                    },
            antiguedad_tdc = {tmp_age_tdc <- interval(start = fecha_emi, end = fecha_obs);
                              tmp_age_tdc2 <- as.period(tmp_age_tdc); 
                              lubridate::year(tmp_age_tdc2)
                              }
            )
]


# Particion de datos entrenamiento 70% y prueba 30%
size <- prod(nrow(base_tdc), .7) |> trunc()

# datos de entrenamiento
set.seed(123)
# sample rows train data
train_index <- sample(x = 1:nrow(base_tdc), size = size)

# sample rows test data
test_index <- c(1:nrow(base_tdc))[
    !(1:nrow(base_tdc) %in% train_index)
    ]

# dt train
base_tdc[ 
    train_index 
    ] -> dt_train

# dt test
base_tdc[
    test_index
] -> dt_test

# guardamos la data tdc final, datos de entrenamiento y prueba
if(!dir.exists('result')){
    dir.create('result')
}

files <- c('base_tdc.RDS','dt_test.RDS','dt_train.RDS')

test_file <- all(files %in% dir('result'))

if(!test_file){
    paths <- file.path('result', files)
    mapply(FUN = saveRDS, list(base_tdc, dt_test, dt_train), paths)
}
