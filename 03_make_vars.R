# librerias y funciones
source('libraries_and_functions.R')

dt_train <- readRDS('result/dt_train.RDS')

# variables de interes
vrs <- c('bine','fecha_nac','fecha_emi','mora_oct_2020',
         'mora_nov20_dic20','mora_nov20_abr21')

# numero de veces en mora durante el periodo de observacion
dt_train[
    ,!..vrs
][
    , `:=` (mora_may20_oct20 = {tmp <- substr(x = vc_2020, 
                                              start = 5, 
                                              stop = 10)},
            nro_moras_obs = {tmp2 <- strsplit(x = tmp, split = '')
                             tmp3 <- unlist(tmp2)
                             tmp4 <- grepl(pattern = '[^NS]', x = tmp3)
                             tmp5 <- sum(tmp4)}
            ),
    by = nro_tarjeta
] -> dt_train

# saldos promedios en cuenta
avg_saldo <- readRDS('data_sets_rds/avg_saldos.RDS')

# conversion de saldos de txt a numero
avg_saldo[
    , lapply(.SD, function(x) {
        gsub(pattern = ',',
             replacement = '.',
             x = x) -> str_num
        
        as.numeric(str_num)
    } ),
    .SDcols = saldo_may_20:avg_may_dic_20,
    by = cedula
] -> avg_saldo


# seleccion de saldo unico en cuenta por cada cliente
avg_saldo[
    , lapply(.SD, unique),
    .SDcols = saldo_may_20:saldo_oct_20,
    by = cedula
] -> avg_saldo


# filtro saldos mayo_20 a oct_20 NA
avg_saldo[
    , lapply(.SD, function(x) {
        # exclusion de clientes sin informacion de saldos
        x[!is.na(x)] -> saldo_no_na
        # exclusion de saldos negativos
        saldo_no_na[saldo_no_na >= 0]
        }),
    .SDcols = saldo_may_20:saldo_oct_20,
    by = cedula
] -> avg_saldo


# unificacion
merge.data.table(
    x = dt_train, 
    y = avg_saldo, 
    by = 'cedula', 
    all.x = T
    ) -> dt_train


# referencia de control cambiario BCV
precio_dolar <- read_rds('data_sets_rds/precio_dolar.RDS')

fechas <- ymd(c('2020-05-29','2020-06-30','2020-07-31',
                '2020-08-31','2020-09-30','2020-10-30'))

# formato fecha variable fecha y formato numero a variable tipo_cambio
precio_dolar[
    , `:=` (Fecha = dmy(Fecha),
            Tipo_Cambio = {tmp <- gsub(pattern = ',', 
                                       replacement = '.', 
                                       x = Tipo_Cambio)
                           as.numeric(tmp)}
            )
][ # filtro de fechas de interes
    Fecha %in% fechas, .(fecha = Fecha, dolar = Tipo_Cambio)
] -> precio_dolar


# conversion de saldos bs a dolar y promedio de saldos dolarizados may20-oct20
col_saldo <- names(dt_train)[
    grepl(pattern = 'saldo', x = names(dt_train))
]

avg_dolar(
    dt_bs = dt_train,
    dt_dolar = precio_dolar,
    cols_saldos = col_saldo,
    name_var = 'avg_saldo'
    ) -> dt_train

# base depositos en cuenta financiera
dep_cuenta <- read_rds('data_sets_rds/dep_cuenta.RDS')

# conversion de depositos en cuenta de txt a numero
dep_cuenta[
    , lapply(.SD, function(x){
        gsub(pattern = ',', 
             replacement = '.', 
             x = x) -> str_num
        
        as.numeric(str_num)
        }),
    .SDcols = total_dep_may_20:total_dep_dic_20,
    by = CEDULACOMPLETA
] -> dep_cuenta

# seleccion de deposito unico en cuenta por cada cliente
dep_cuenta[
    , lapply(.SD, unique),
    by = CEDULACOMPLETA,
    .SDcols = total_dep_may_20:total_dep_oct_20
] -> dep_cuenta


# filtro de depositos mayo_20 a oct_20 iguales a NA
dep_cuenta[
    , lapply(.SD, function(x) {
        # exclusion de clientes sin informacion de depositos
        x[!is.na(x)] -> dep_no_na
        }),
    .SDcols = total_dep_may_20:total_dep_oct_20,
    by = CEDULACOMPLETA
] -> dep_cuenta

# unificacion
merge.data.table(
    x = dt_train,
    y = dep_cuenta,
    by.x = 'cedula',
    by.y = 'CEDULACOMPLETA',
    all.x = T
    ) -> dt_train

# depositos en cuenta dolarizado may20-oct20
col_dep <- names(dt_train)[
    grepl(pattern = '_dep_', x = names(dt_train))
]

avg_dolar(
    dt_bs = dt_train,
    dt_dolar = precio_dolar,
    cols_saldos = col_dep,
    name_var = 'avg_dep'
    ) -> dt_train

# guardamos las modificaciones
if(!file.exists('result/dt_train2.RDS')){
    saveRDS(object = dt_train, file = 'result/dt_train2.RDS')
}
