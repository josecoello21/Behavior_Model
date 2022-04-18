# librerias y funciones
source('libraries_and_functions.R')

path_dataset <- c('result/dt_train.RDS', 'result/dt_test.RDS')

dt_sets <- lapply(X = path_dataset, FUN = readRDS)
names(dt_sets) <- c('dt_train', 'dt_test')

# variables de interes
vrs <- c('bine','fecha_nac','fecha_emi','mora_oct_2020',
         'mora_nov20_dic20','mora_nov20_abr21')

# numero de veces en mora durante el periodo de observacion
lapply(X = dt_sets, 
       function(x){
           x[
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
           ]
       }) -> dt_sets

# saldos promedios en cuenta
avg_saldo <- readRDS('data_sets_rds/avg_saldos.RDS') |> 
    as.data.table()

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
lapply(X = dt_sets, 
       function(x){
           merge.data.table(
               x = x,
               y = avg_saldo,
               by = 'cedula',
               all.x = T
               )
       }) -> dt_sets

# referencia de control cambiario BCV
precio_dolar <- readRDS('data_sets_rds/precio_dolar.RDS') |> 
    as.data.table()

fechas <- ymd(c('2020-05-29','2020-06-30','2020-07-31',
                '2020-08-31','2020-09-30','2020-10-30'))

# formato fecha variable fecha y formato numero a variable tipo_cambio
precio_dolar[
    , Fecha := dmy(Fecha)
][  # filtro de fechas de interes
    Fecha %in% fechas, .(fecha = Fecha, dolar = Tipo_Cambio)
] -> precio_dolar


# conversion de saldos bs a dolar y promedio de saldos dolarizados may20-oct20
col_saldo <- names(dt_sets$dt_train)[
    grepl(pattern = 'saldo', x = names(dt_sets$dt_train))
]

lapply(
    X = dt_sets, 
    FUN = avg_dolar,
    dt_dolar = precio_dolar,
    cols_saldos = col_saldo,
    name_var = 'avg_saldo'
    ) -> dt_sets

# base depositos en cuenta financiera
dep_cuenta <- readRDS('data_sets_rds/dep_cuenta.RDS') |> 
    as.data.table()

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
lapply(X = dt_sets, 
       function(x){
           merge.data.table(
               x = x,
               y = dep_cuenta,
               by.x = 'cedula',
               by.y = 'CEDULACOMPLETA',
               all.x = T
               )
           }) -> dt_sets

# depositos en cuenta dolarizado may20-oct20
col_dep <- names(dt_sets$dt_train)[
    grepl(pattern = '_dep_', x = names(dt_sets$dt_train))
]

lapply(
    X = dt_sets, 
    FUN = avg_dolar,
    dt_dolar = precio_dolar,
    cols_saldos = col_dep,
    name_var = 'avg_dep'
    ) -> dt_sets

# guardamos las modificaciones
path_dataset2 <- c('result/dt_train2.RDS', 'result/dt_test2.RDS')

test <- file.exists(
    path_dataset2
    ) |> all()

if(!test){
    mapply(FUN = saveRDS, dt_sets, path_dataset2)
}
