library(purrr)
library(lubridate)
library(data.table)
library(scorecard)
library(ROSE)
library(knitr)
library(jtools)

# Saldo promedio dolarizado (semestral) 
avg_dolar <- function(dt_bs, 
                      dt_dolar, 
                      cols_saldos,
                      name_var = 'avg'){
    # seleccion de columnas saldos
    dt_bs[
        , ..cols_saldos
    ] -> dt_saldo_bs
    
    # re-arrange de base precio dolar
    dcast(
        data = dt_dolar,
        . ~ fecha,
        value.var = 'dolar'
        )[,-c('.')] -> dt_dolar
    
    # saldos dolarizados
    map2_df(.x = dt_saldo_bs,
            .y = dt_dolar, 
            function(x,y){x/y}
            ) |> 
        as.data.table() -> dt_saldo_dolar
    
    # saldo promedio dolarizado
    dt_saldo_dolar[
        , c(name_var) := apply(X = .SD, MARGIN = 1, FUN = mean, na.rm = T)
    ]
    
    cbind(dt_bs[,-..cols_saldos], dt_saldo_dolar)
}


# Gini
gini <- function(dt, x, y, breaks = NULL){
    # check breaks, si grupos es nulo o se desea una agrupacion
    if(is.null(breaks)){
        # variables por agrupar
        by_vars <- c(x,y)
        dt[
            , .N, by = by_vars
        ] -> dt_gini
        
        # re-arrange
        var_x <- x
        var_y <- y
        dcast(
            data = dt_gini, 
            get(var_x) ~ get(var_y),
            value.var = 'N', 
            fill = 0
            ) -> dt_gini
        
        # calculo del gini
        dt_gini[
            , `:=` (total = {tmp <- `0` + `1`},
                    acu_buenos = {tmp_good <- cumsum(`0`)/sum(`0`)},
                    acu_malos = {tmp_bad <- cumsum(`1`)/sum(`1`)},
                    acu_total = cumsum(tmp)/sum(tmp),
                    x = {tmp_good2 <- c(0, tmp_good[ 1:(length(tmp_good)-1) ]);
                         tmp_x <- tmp_good + tmp_good2},
                    y = {tmp_y <- c(tmp_bad[1], diff(tmp_bad))},
                    gini = abs(1 - sum(tmp_x * tmp_y))
                    )
        ]
    }else{
        # creacion de bins por agrupar
        var_x <- x
        var_y <- y
        dt[
            , bins := cut(x = get(var_x),
                          breaks = breaks,
                          include.lowest = T, right = F)
        ][
            , .N, by = .(bins, get(var_y))
        ] -> dt_gini
        
        # re-arrange
        dcast(
            data = dt_gini,
            bins ~ get,
            value.var = 'N',
            fill = 0
        ) -> dt_gini
        
        # calculo del gini
        dt_gini[
            , `:=` (total = {tmp <- `0` + `1`},
                    acu_buenos = {tmp_good <- cumsum(`0`)/sum(`0`)},
                    acu_malos = {tmp_bad <- cumsum(`1`)/sum(`1`)},
                    acu_total = cumsum(tmp)/sum(tmp),
                    x = {tmp_good2 <- c(0, tmp_good[ 1:(length(tmp_good)-1) ]);
                         tmp_x <- tmp_good + tmp_good2},
                    y = {tmp_y <- c(tmp_bad[1], diff(tmp_bad))},
                    gini = abs(1 - sum(tmp_x * tmp_y))
                    )
            ]
    }
    return(dt_gini[1:nrow(dt_gini)])
}
