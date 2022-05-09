---
title: "Desarrollo de Modelo Behavior para TDC"
date: 'May 09, 2022'
output:
  html_document:
    toc: true
    toc_float: true
    toc_depth: 3
    number_sections: true
    code_folding: hide
    keep_md: yes
---

**Sinopsis**

El Modelo de behavior de tarjetas de crédito tiene como objetivo estimar la probabilidad de incumplimiento de los tarjetahabientes en un período de tiempo, según su propensión de poder registrar una mora de noventa días o más en los próximos seis meses, lo cual permite optimizar la gestión de cobro, a su vez este modelo suele ser una herramienta útil en los aumentos de límite de tarjetas de crédito debido a que nos ayuda a mitigar el riesgo de perdida por incumplimiento.

**Flujo de trabajo para el desarrollo del modelo**

Para la realización del modelo se tomarán en cuenta los siguiente pasos:

1. Delimitación de los períodos de observación y desempeño, siendo estos, período de observación: de mayo 2020 a octubre 2020 y período de desempeño: de noviembre 2020 a abril 2021

2. Se excluyen los clientes que a la fecha octubre 2020 (fecha divisoria) se encontraban incumplidos.

3. Etiquetas de buenos/malos clientes basado en la altura máxima de mora alcanzada que se considera como incumplimiento (noventa días) en el período de desempeño (noviembre 2020 a abril 2021).

4. Partición de los datos en un subconjunto de entrenamiento (70%) y un subconjunto de prueba (30%).

5. Con los datos de entrenamiento se construyen posibles variables predictoras en el período de observación (mayo 2020 a octubre 2020).

6. Se evalúa la potencia discriminatoria de cada variable predictora construida en el paso anterior mediante el cálculo del coeficiente gini, luego se seleccionan las variables predictivas y se ajusta el modelo de regresión logística.

7. Con los datos de prueba (30%), y el modelo final, se calculan las probabilidades de incumplimiento y se determina un gini de prueba.

**Fuente de datos**

Los datos utilizados para la elaboración del modelo pertenecen a la cartera de TDC y datos de interes suministrados por el area de inteligencia competitiva, para más información, en el siguiente enlace encontrará cómo se construyen y definen algunas de las variables y bases utilizadas [Data Documentation](https://github.com/josecoello21/Behavior_Model/blob/main/CodeBook.md)<base target="_top"/>.

**Librerias**


```r
library(purrr)
library(lubridate)
library(data.table)
library(scorecard)
library(ROSE)
```



# Carga y preparación de los datos


```r
# carga de base tdc
base_tdc <- readRDS(file = 'data_sets_rds/tdc_abr21.RDS') |> 
    as.data.table()

dim(base_tdc)
```

```
## [1] 213296     78
```

La cartera de tarjeta de créditos cuenta con un total de 213,296 registros y 78 columnas.

Una vez cargados los datos realizamos los siguientes pasos:

1. Selección de variables de interes: `cedula`, `nro_tarjeta`, `bine`, `sexo`, `edo_civil`, `tipo_res`, `fecha_nac`, `fecha_emi`, `nro_dep`, `vc_2020`, `vc_2021`.


```r
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
```

2. Selección de registros con información del comportamiento de pago entre las fechas enero 2020 a abril 2021.


```r
# vectores de comportamiento de 12 caracteres 2020 y 4 caracteres 2021
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
```

3. Exclusión de registros en estado de incumplimiento (90 días o más) al cierre del período de observación  octubre 2020. 


```r
# Exclusión de clientes oct20 (fecha divisoria) que se encuentran incumplidos
incumplido <- '[3456789GHIJKLM]'

base_tdc[
    , mora_oct_2020 := substr(x = vc_2020, start = 10, stop = 10)
    ][
        !grepl(pattern = incumplido, x = mora_oct_2020)
        ] -> base_tdc
```

4. Se detecta la altura de mora alcanzada durante el período de desempeño (noviembre 2020 a abril 2021) y creamos la variable `Status` que nos indica si el cliente alcanzó un estado de mora menor a 90 días, si este es el caso se etiqueta como bueno = 0 ó si el cliente obtuvo una mora de 90 días o más, estos casos serán considerados como incumplido = 1.


```r
# Etiqueta buenos/malos en el período de comportamiento nov20-abr21
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
```

5. Obtenemos la edad y antiguedad de la tarjeta de crédito para cada uno de los registros al inicio del período de observación.


```r
# Edad y antiguedad de la tarjeta
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
```

6. Partición de los datos en un subconjunto de entrenamiento (70%) y un subconjunto de prueba (30%).


```r
# Partición de datos entrenamiento 70% y prueba 30%
size <- prod(nrow(base_tdc), .7) |> trunc()

# datos de entrenamiento
set.seed(123)
# subconjunto datos de entrenamiento
train_index <- sample(x = 1:nrow(base_tdc), size = size)

# subconjunto datos de prueba
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
```

# Construcción de variables

Una vez obtenidos los registros de interes y algunas variables necesarias para el desarrollo del modelo, procedemos a construir posibles variables predictoras en el período de observación.

1. Obtenemos el número de moras que los tarjetahabientes obtuvieron durante el período de observación.


```r
# variables a excluir
vrs <- c('bine','fecha_nac','fecha_emi','mora_oct_2020',
         'mora_nov20_dic20','mora_nov20_abr21')

# número de veces en mora durante el período de observación
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
```

2. Calculo del saldo promedio dolarizado en cuenta financiera durante el período de observación (mayo 2020 a octubre 2020).


```r
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
```

3. Calculo de depósito promedio dolarizado en cuenta financiera durante el período de observación (mayo 2020 a octubre 2020).


```r
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
```

# Construcción de bins sobre variables predictoras

En esta etapa se generaron los distintos intervalos sobre las variables predictoras, tomando en consideración la lógica que se esperaría observar en cada una de ellas.

Para ello se utilizarón las métricas coeficiente de Gini, peso de la evidencia conocido también como (WOE) por sus siglas en inglés y el valor de la información (IV), estas proporcionan un marco para el análisis exploratorio y recategorización de variables de clasificadores binarios, han sido utilizado ampliamente en el mundo del riesgo de crédito durante varias décadas.

- **Peso de la evidencia (WOE)**: muestra el poder predictivo de una variable independiente en relación con la variable dependiente. Evolucionó con  el puntaje de crédito para magnificar el poder de separación entre un buen cliente y un mal cliente, se define como:

\begin{equation}
WOE\ =\ ln( \frac{Distribucion\ de\ malos}{Distribucion\ de\ buenos} )
\end{equation}

Valores WOE positivos significa que la distribución de malos es mayor a la distribución de buenos.

- **Valor de la información (IV)**: ayuda a seleccionar las variables utilizando su orden de importancia y el valor de la información después de la agrupación.

\begin{equation}
IV\ =\ \sum (\%\ de\ buenos\ - \%\ de\ malos) * WOE
\end{equation}




```r
# resumen coeficiente de gini por variable
lapply(
    X = vars, 
    FUN = gini,
    dt = dt_sets$dt_train, 
    y = 'status'
    ) -> gini_summ

names(gini_summ) <- vars

lapply(X = gini_summ, FUN = kable, digits = 3)
```

```
## $tipo_res2
## 
## 
## |var_x     |     0|   1| total| acu_buenos| acu_malos| acu_total|     x|     y|  gini|
## |:---------|-----:|---:|-----:|----------:|---------:|---------:|-----:|-----:|-----:|
## |NA        | 36689| 163| 36852|      0.248|     0.233|     0.248| 0.248| 0.233| 0.047|
## |ARRENDADA |  7363|  45|  7408|      0.297|     0.297|     0.297| 0.545| 0.064| 0.047|
## |FAMILIAR  | 37232| 227| 37459|      0.549|     0.621|     0.549| 0.846| 0.324| 0.047|
## |OTRO      | 66902| 265| 67167|      1.000|     1.000|     1.000| 1.549| 0.379| 0.047|
## 
## $edad2
## 
## 
## |var_x    |     0|   1| total| acu_buenos| acu_malos| acu_total|     x|     y|  gini|
## |:--------|-----:|---:|-----:|----------:|---------:|---------:|-----:|-----:|-----:|
## |NA       |  1319|   2|  1321|      0.009|     0.003|     0.009| 0.009| 0.003| 0.099|
## |[19,34)  | 28085| 153| 28238|      0.198|     0.221|     0.199| 0.207| 0.219| 0.099|
## |[34,39)  | 27213| 151| 27364|      0.382|     0.437|     0.382| 0.580| 0.216| 0.099|
## |[39,46)  | 31287| 181| 31468|      0.593|     0.696|     0.594| 0.975| 0.259| 0.099|
## |[46,55)  | 29092| 114| 29206|      0.790|     0.859|     0.790| 1.383| 0.163| 0.099|
## |[55,120] | 31190|  99| 31289|      1.000|     1.000|     1.000| 1.790| 0.141| 0.099|
## 
## $nro_moras_obs2
## 
## 
## |var_x   |     0|   1| total| acu_buenos| acu_malos| acu_total|     x|     y|  gini|
## |:-------|-----:|---:|-----:|----------:|---------:|---------:|-----:|-----:|-----:|
## |[0,1)   | 85588| 214| 85802|      0.578|     0.306|     0.576| 0.578| 0.306| 0.276|
## |[1,2)   |  7911|  55|  7966|      0.631|     0.384|     0.630| 1.209| 0.079| 0.276|
## |[2,100] | 54687| 431| 55118|      1.000|     1.000|     1.000| 1.631| 0.616| 0.276|
## 
## $avg_saldo2
## 
## 
## |var_x           |     0|   1| total| acu_buenos| acu_malos| acu_total|     x|     y| gini|
## |:---------------|-----:|---:|-----:|----------:|---------:|---------:|-----:|-----:|----:|
## |NA              |  3960|  76|  4036|      0.027|     0.109|     0.027| 0.027| 0.109| 0.68|
## |[0,1.05)        | 28441| 528| 28969|      0.219|     0.863|     0.222| 0.245| 0.754| 0.68|
## |[1.05,3.48)     | 28930|  41| 28971|      0.414|     0.921|     0.416| 0.633| 0.059| 0.68|
## |[3.48,7.72)     | 28933|  36| 28969|      0.609|     0.973|     0.611| 1.023| 0.051| 0.68|
## |[7.72,18.9)     | 28960|  11| 28971|      0.805|     0.989|     0.805| 1.414| 0.016| 0.68|
## |[18.9,2.56e+04] | 28962|   8| 28970|      1.000|     1.000|     1.000| 1.805| 0.011| 0.68|
## 
## $avg_dep2
## 
## 
## |var_x          |     0|   1| total| acu_buenos| acu_malos| acu_total|     x|     y|  gini|
## |:--------------|-----:|---:|-----:|----------:|---------:|---------:|-----:|-----:|-----:|
## |NA             | 18538| 437| 18975|      0.125|     0.624|     0.127| 0.125| 0.624| 0.663|
## |[0,28.5)       | 25818| 164| 25982|      0.299|     0.859|     0.302| 0.424| 0.234| 0.663|
## |[28.5,81.1)    | 25939|  43| 25982|      0.474|     0.920|     0.476| 0.774| 0.061| 0.663|
## |[81.1,182)     | 25956|  26| 25982|      0.650|     0.957|     0.651| 1.124| 0.037| 0.663|
## |[182,453)      | 25966|  16| 25982|      0.825|     0.980|     0.825| 1.474| 0.023| 0.663|
## |[453,3.59e+05] | 25969|  14| 25983|      1.000|     1.000|     1.000| 1.825| 0.020| 0.663|
```

```r
# total peso de evidencia y valor de informacion por variable
kable(x = total_iv, digits = 3)
```



|variable       |breaks          |    woe| total_iv|
|:--------------|:---------------|------:|--------:|
|avg_saldo2     |missing         |  1.402|    2.216|
|avg_saldo2     |1.05            |  1.369|    2.216|
|avg_saldo2     |3.48            | -1.204|    2.216|
|avg_saldo2     |7.72            | -1.334|    2.216|
|avg_saldo2     |18.9            | -2.521|    2.216|
|avg_saldo2     |[18.9,2.56e+04] | -2.839|    2.216|
|avg_dep2       |missing         |  1.607|    1.801|
|avg_dep2       |28.5            |  0.296|    1.801|
|avg_dep2       |81.1            | -1.047|    1.801|
|avg_dep2       |182             | -1.551|    1.801|
|avg_dep2       |453             | -2.037|    1.801|
|avg_dep2       |[453,3.59e+05]  | -2.170|    1.801|
|nro_moras_obs2 |1               | -0.636|    0.309|
|nro_moras_obs2 |2               |  0.386|    0.309|
|nro_moras_obs2 |[2,100]         |  0.512|    0.309|
|edad2          |missing         | -1.136|    0.059|
|edad2          |34              |  0.143|    0.059|
|edad2          |39              |  0.161|    0.059|
|edad2          |46              |  0.203|    0.059|
|edad2          |55              | -0.187|    0.059|
|edad2          |[55,120]        | -0.398|    0.059|
|tipo_res2      |missing         | -0.061|    0.036|
|tipo_res2      |ARRENDADA       |  0.258|    0.036|
|tipo_res2      |FAMILIAR        |  0.255|    0.036|
|tipo_res2      |OTRO            | -0.176|    0.036|

# Modelo de regresión logística

En los problemas de clasificación, una desigualdad en las frecuencias de las clases observadas puede tener un impacto negativo en el ajuste del modelo. Una técnica para resolver este desequilibrio de clases es crear una muestra de datos sintéticos ampliando el espacio de características de la clase minoritaria, de esta manera poder mitigar este problema [Subsampling Techniques](https://topepo.github.io/caret/subsampling-for-class-imbalances.html)<base target="_top"/>.


```r
# frecuencia de las clases de la variable respuesta status
lapply(
    X = dt_sets, 
    function(x){
        kable(table(x$status))
        }
    )
```

```
## $dt_train
## 
## 
## |Var1 |   Freq|
## |:----|------:|
## |0    | 148186|
## |1    |    700|
## 
## $dt_test
## 
## 
## |Var1 |  Freq|
## |:----|-----:|
## |0    | 63475|
## |1    |   334|
```

```r
# prop. de las clases de la variable respuesta status
lapply(
    X = dt_sets, 
    function(x){
        kable(prop.table(table(x$status)))
        }
    )
```

```
## $dt_train
## 
## 
## |Var1 |      Freq|
## |:----|---------:|
## |0    | 0.9952984|
## |1    | 0.0047016|
## 
## $dt_test
## 
## 
## |Var1 |      Freq|
## |:----|---------:|
## |0    | 0.9947656|
## |1    | 0.0052344|
```

Para no modificar en gran medida la naturaleza del problema en lo que refiere al desbalanceo de clases, se decidió configurar el conjunto de datos de entrenamiento en una relación de 80% de registros con características de clientes al día y un 20% de registros de características de clientes incumplidos.


```r
# balanceo de clases del conjunto de entrenamiento
# (prop. de buenos y malos igual a 80%-20%)
ROSE(
    status ~ ., 
    data = dt_sets$dt_train, 
    N = nrow(dt_sets$dt_train), 
    p = .2, seed = 123
    )$data |> 
    as.data.table() -> dt_balanced

# prop. de las clases de la variable status en los datos balanceados
prop.table(table(dt_balanced$status)) |> 
    round(digits = 3)
```

```
## 
##   0   1 
## 0.8 0.2
```



La regresión logística mide la relación entre una o más variables independientes y la variable dependiente categórica mediante la estimación de probabilidades a través de una función logística, que es la distribución logística acumulativa, su fórmula viene dada por:

\begin{equation}
\ln\left(\frac{P\left(X\right)}{1-P\left(X\right)}\right) = intercept + \beta_{1}x +...+ \beta_{n}x
\end{equation}


```r
# construccion del modelo logistico
set.seed(123)
glm(
    status ~ .,
    family = binomial(link = 'logit'),
    data = dt_woe$dt_balanced
    ) -> logit_model

summ(logit_model, model.info = FALSE)
```

```
## MODEL FIT:
## χ²(5) = 42585.49, p = 0.00
## Pseudo-R² (Cragg-Uhler) = 0.39
## Pseudo-R² (McFadden) = 0.29
## AIC = 106387.41, BIC = 106446.88 
## 
## Standard errors: MLE
## -------------------------------------------------------
##                             Est.   S.E.   z val.      p
## ------------------------ ------- ------ -------- ------
## (Intercept)                -0.74   0.01   -74.34   0.00
## tipo_res2_woe               0.80   0.04    21.26   0.00
## edad2_woe                   0.75   0.04    19.38   0.00
## nro_moras_obs2_woe          0.81   0.01    57.70   0.00
## avg_saldo2_woe              0.69   0.01   100.91   0.00
## avg_dep2_woe                0.25   0.01    22.10   0.00
## -------------------------------------------------------
```

Evaluamos el rendimiento del modelo, para ello observamos las métricas de rendimiento tales como:

- Gini general del modelo.
- Matriz de confusión.
- Precisión, recall o sensibilidad y media armónica.
- AUC, KS.

**Gini general del modelo**




```r
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

kable(x = gini_model$dt_test, digits = 3)
```



|bins            |    0|   1| total| acu_buenos| acu_malos| acu_total|     x|     y|  gini|
|:---------------|----:|---:|-----:|----------:|---------:|---------:|-----:|-----:|-----:|
|[0.00878,0.024) | 6333|   0|  6333|      0.100|     0.000|     0.099| 0.100| 0.000| 0.704|
|[0.024,0.0366)  | 6286|   5|  6291|      0.199|     0.015|     0.198| 0.299| 0.015| 0.704|
|[0.0366,0.0537) | 6478|   4|  6482|      0.301|     0.027|     0.299| 0.500| 0.012| 0.704|
|[0.0537,0.07)   | 6331|   9|  6340|      0.401|     0.054|     0.399| 0.701| 0.027| 0.704|
|[0.07,0.0868)   | 6432|   8|  6440|      0.502|     0.078|     0.500| 0.903| 0.024| 0.704|
|[0.0868,0.121)  | 6381|   5|  6386|      0.602|     0.093|     0.600| 1.104| 0.015| 0.704|
|[0.121,0.186)   | 6373|  14|  6387|      0.703|     0.135|     0.700| 1.305| 0.042| 0.704|
|[0.186,0.398)   | 6356|  22|  6378|      0.803|     0.201|     0.800| 1.506| 0.066| 0.704|
|[0.398,0.598)   | 6265|  63|  6328|      0.902|     0.389|     0.899| 1.705| 0.189| 0.704|
|[0.598,0.802]   | 6240| 204|  6444|      1.000|     1.000|     1.000| 1.902| 0.611| 0.704|

**Matriz de confusión**


```r
# otras metricas
threshold = 0.181
mapply(
    function(x,y){
        perf_eva(
            pred = x,
            label = as.numeric(as.character(y$status)), 
            confusion_matrix = TRUE,
            threshold = threshold,
            show_plot = FALSE
            )
        }, 
    predict_model, dt_woe, SIMPLIFY = F
    ) -> metrics
```

```
## [INFO] The threshold of confusion matrix is 0.1810.
## [INFO] The threshold of confusion matrix is 0.1810.
## [INFO] The threshold of confusion matrix is 0.1810.
```

```r
# matriz de confusión
metrics$dt_test$confusion_matrix
```

```
## $dat
##    label pred_0 pred_1     error
## 1:     0  44434  19041 0.2999764
## 2:     1     44    290 0.1317365
## 3: total  44478  19331 0.2990957
```

**Precisión, recall o sensibilidad y media armónica**


```r
# precisión, recall, media armónica entre presición y recall
accuracy.meas(
    response = dt_woe$dt_test$status,
    predicted =  predict_model$dt_test,
    threshold = threshold
    )
```

```
## 
## Call: 
## accuracy.meas(response = dt_woe$dt_test$status, predicted = predict_model$dt_test, 
##     threshold = threshold)
## 
## Examples are labelled as positive when predicted is greater than 0.181 
## 
## precision: 0.015
## recall: 0.868
## F: 0.015
```

**AUC, KS**


```r
# otras métricas
metrics$dt_test$binomial_metric
```

```
## $dat
##           MSE      RMSE   LogLoss        R2        KS       AUC      Gini
## 1: 0.08437658 0.2904765 0.2693769 -15.20454 0.6084199 0.8592149 0.7184298
```


Este documento contiene un resumen general y resultados más relevantes del desarrollo del modelo behavior para TDC, para mayor información sobre flujo de trabajo y código general del proyecto [Github](https://github.com/josecoello21/Behavior_Model.git)<base target="_top"/>.  

<div class = "tocify-extend-page" data-unique = "tocify-extend-page" style = "height: 0;"> </div>
