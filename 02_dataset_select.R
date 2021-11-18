# librerias y funciones
source('libraries_and_functions.R')

# carga de base tdc
base_tdc <- read_rds(file = 'data_sets_rds/tdc_abr21.RDS')

# variables de interes
columns <- c('T5NRTA','T5BINE','T5CDTI','T5UNNB','T5CDSE','T5DSEC',
             'T5DSTR','T5PWNB','T5MQNB','T5T3NB','H3HSNB','H3HTNB')

base_tdc <- base_tdc %>% 
    select(contains(match = columns, vars = names(.) 
                    ) 
           ) %>% 
    unite(col = 'cedula', c(T5CDTI,T5UNNB), sep = '') %>% 
    rename(nro_tarjeta = T5NRTA,
           bine        = T5BINE,
           sexo        = T5CDSE,
           edo_civil   = T5DSEC,
           tipo_res    = T5DSTR,
           fecha_nac   = T5PWNB,
           fecha_emi   = T5MQNB,
           nro_dep     = T5T3NB,
           vc_2020     = H3HSNB,
           vc_2021     = H3HTNB
           )

# vectores de comp 12 caracteres 2020 y 4 caracteres 2021, solo tdc
base_tdc <- base_tdc %>% 
    mutate(vc_2020 = str_replace_all(string = vc_2020, 
                                     pattern = '\\s', 
                                     replacement = ''),
           vc_2021 = str_replace_all(string = vc_2021, 
                                     pattern = '\\s', 
                                     replacement = '')
           ) %>% 
    filter(nchar(vc_2020) == 12, 
           nchar(vc_2021) == 4, 
           bine != '422169'
           )

# reformateo de datos
base_tdc <- base_tdc %>% 
    mutate(sexo      = factor(sexo),
           edo_civil = factor(edo_civil),
           tipo_res  = factor(tipo_res),
           fecha_nac = ymd(fecha_nac),
           fecha_emi = ymd(fecha_emi)
           )

# Exclusion de clientes oct20 (fecha divisoria) que se encuentran incumplidos
incumplido <- '[3456789GHIJKLM]'
base_tdc <- base_tdc %>% 
    mutate(mora_oct_2020 = str_sub(string = vc_2020, 
                                   start = 10, 
                                   end = 10)
           ) %>% 
    filter(str_detect(string = mora_oct_2020, 
                      pattern = incumplido, 
                      negate = T)
           )
# Etiqueta malos/buenos en el periodo de comportamiento nov20-abr21
base_tdc <- base_tdc %>% 
    mutate(mora_nov20_dic20 = str_sub(string = vc_2020, 
                                      start = 11, 
                                      end = 12),
           mora_nov20_abr21 = paste0(mora_nov20_dic20, 
                                     vc_2021),
           status = str_detect(string = mora_nov20_abr21,
                               pattern = incumplido),
           status = factor(as.integer(status))
           )

# Edad y tiempo de la tarjeta
fecha_obs <- ymd('2020-04-30')
base_tdc <- base_tdc %>% 
    mutate(edad = interval(start = fecha_nac, end = fecha_obs) %>% 
               as.period %>% 
               lubridate::year(),
           antiguedad_tdc = interval(start = fecha_emi, end = fecha_obs) %>% 
               as.period %>% 
               lubridate::year()
           )

# Particion de datos entrenamiento 70% y prueba 30%
size <- prod(nrow(base_tdc), .7) %>% trunc

# datos de entrenamiento
set.seed(123)
train <- sample_n(tbl = base_tdc, 
                  size = size, 
                  replace = F
                  )

# datos de prueba
test <- base_tdc %>% 
    anti_join(y = train, 
              by = 'nro_tarjeta'
              )
# guardamos la data tdc final, datos de entrenamiento y prueba
if(!dir.exists('result')){
    dir.create('result')
}

files <- c('base_tdc.RDS','test.RDS','train.RDS')

test_file <- all(files %in% dir('result'))

if(!test_file){
    paths <- file.path('result', files)
    mapply(FUN = saveRDS, list(base_tdc, test, train), paths)
}
