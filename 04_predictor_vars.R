# librerias y funciones
source('libraries_and_functions.R')

# carga de datos de entrenamiento
train <- read_rds('result/train_df.RDS')

# variables categoricas sexo, edo_civil y tipo_res
var_categoricas <- c('sexo', 'edo_civil', 'tipo_res')

gini_list <- mapply(FUN = gini, 
                    list(train), 
                    var_categoricas, 
                    y = 'status', 
                    SIMPLIFY = F
                    )

names(gini_list) <- var_categoricas
gini_list

# variables numericas discretas
# nro_dep
train %>% 
    gini(df = ., 
         x = 'nro_dep', 
         y = 'status', 
         breaks = c(0,3,100)
         )

# edad
break_edad <- quantile(x = train$edad[train$edad <= 120], 
                       probs = seq(0,1,.2),
                       na.rm = T)
train %>% 
    filter(edad <= 120) %>% 
    gini(df = ., 
         x = 'edad', 
         y = 'status', 
         breaks = break_edad
         )

# antiguedad_tdc
break_ages_tdc <- quantile(x = train$antiguedad_tdc,
                           probs = seq(0,1,.25),
                           na.rm = T)
train %>% 
    gini(df = ., 
         x = 'antiguedad_tdc', 
         y = 'status', 
         breaks = unique(break_ages_tdc)
         )

# nro_moras_obs
train %>% 
    gini(df = ., 
         x = 'nro_moras_obs', 
         y = 'status', 
         breaks = c(0,1,3,max(train$nro_moras_obs))
         )

# avg_saldo
break_saldo <- train$avg_saldo %>% 
    quantile(x = ., probs = seq(0,1,.2), na.rm = T) %>% 
    round(x = ., digits = 4)

train %>% 
    filter(!is.na(avg_saldo)) %>% 
    gini(df = ., 
         x = 'avg_saldo', 
         y = 'status', 
         breaks = break_saldo
         )

# promedio de suma total depositos en cuenta (avg_dep)
break_avg_dep <- train$avg_dep %>% 
    quantile(probs = seq(0, 1, .2), na.rm = T) %>% 
    round(digits = 4)

train %>% 
    filter(!is.na(avg_dep)) %>% 
    gini(df = ., 
         x = 'avg_dep', 
         y = 'status', 
         breaks = break_avg_dep
    )
