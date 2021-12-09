# librerias y funciones
source('libraries_and_functions.R')

train <- read_rds('result/train.RDS')

# variables de interes
vrs <- c('bine','fecha_nac','fecha_emi','mora_oct_2020',
         'mora_nov20_dic20','mora_nov20_abr21')

var_select <- names(train)[-matches(match = vrs, vars = names(train))]

train <- train %>% 
    select(all_of(var_select))

# numero de veces en mora durante el periodo de observacion
train <- train %>% 
    mutate(mora_may20_oct20 = str_sub(string = vc_2020, 
                                      start = 5, 
                                      end = 10),
           nro_moras_obs = str_count(string = mora_may20_oct20, 
                                     pattern = '[^NS]')
           )

# saldos promedios en cuenta
avg_saldo <- read_rds('data_sets_RDS/avg_saldos.RDS')
avg_saldo <- data.table(avg_saldo)

# seleccion del maximo saldo en cuenta por cada cliente
saldo_unique <- avg_saldo[,lapply(.SD, max, na.rm=TRUE),
                          by = cedula, 
                          .SDcols = saldo_may_20:saldo_oct_20] %>% 
    suppressWarnings %>% 
    .[.[, Reduce('&', lapply(.SD, '>=', 0)),
        .SDcols = saldo_may_20:saldo_oct_20], 
      cedula:saldo_oct_20]

# unificacion
train <- train %>% 
    left_join(y = saldo_unique, by = 'cedula')

# referencia de control cambiario BCV
precio_dolar <- read_rds('data_sets_RDS/precio_dolar.RDS')

fechas <- ymd(c('2020-05-29','2020-06-30','2020-07-31',
                '2020-08-31','2020-09-30','2020-10-30'))

precio_dolar <- precio_dolar %>% 
    mutate(Fecha = dmy(Fecha)) %>% 
    filter(Fecha %in% fechas) %>% 
    rename(fecha = Fecha,
           dolar = Tipo_Cambio
           )

# saldos dolarizados/saldo promedio dolarizado may20-oct20
col_saldo <- contains(match = 'saldo', 
                      vars = names(train)
                      )

train <- avg_dolar(df_bs = train,
                   df_dolar = precio_dolar,
                   cols_saldos = col_saldo
                   )

# suma de depositos en cuenta financiera
dep_cuenta <- read_rds('data_sets_RDS/dep_cuenta.RDS')
dep_cuenta <- data.table(dep_cuenta)

# suma total de depositos en cuenta por cliente
dep_total <- dep_cuenta[,lapply(.SD, sum, na.rm=TRUE),
                        by = CEDULACOMPLETA,
                        .SDcols = total_dep_may_20:total_dep_oct_20]

# unificacion
train <- train %>% 
    left_join(y = dep_total, by = c('cedula' = 'CEDULACOMPLETA'))

# depositos en cuenta dolarizado may20-oct20
col_dep <- contains(match = '_dep_', 
                    vars = names(train)
                    )


train <- avg_dolar(df_bs = train,
                   df_dolar = precio_dolar,
                   cols_saldos = col_dep
                   )

train <- train %>% 
    rename(avg_saldo = avg_saldo...20,
           avg_dep = avg_saldo...27
           )

# guardamos las modificaciones
if(!file.exists('result/train_df.RDS')){
    saveRDS(object = train, file = 'result/train_df.RDS')
}
