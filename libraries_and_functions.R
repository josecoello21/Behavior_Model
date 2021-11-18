library(tidyverse)
library(readr)
library(lubridate)
library(data.table)

# Saldo promedio dolarizado (semestral) 
avg_dolar <- function(df_bs, df_dolar, cols_saldos){
    
    df_saldo_bs <- df_bs %>% 
        select(all_of(cols_saldos))
    
    df_dolar <- precio_dolar %>% 
        spread(fecha, value = dolar)
    
    df_saldo_dolar <- map2_df(.x = df_saldo_bs, 
                              .y = df_dolar, 
                              function(x,y){x/y}
                              ) %>% 
        mutate(avg_saldo = apply(X = ., 
                                 MARGIN = 1, 
                                 function(x) mean(x, na.rm = T) 
                                 ) 
               )
    
    df_final <- bind_cols(df_bs %>% select(!all_of(cols_saldos)), 
                          df_saldo_dolar)
}

# Gini 
gini <- function(df, x, y, breaks = NULL){
    if(is.null(breaks)){
        table_gini <- df %>% 
            count(df[[x]], df[[y]]) %>% 
            rename(status = `df[[y]]`) %>% 
            spread(key = y, value = n) %>% 
            mutate(across(2:3, ~ replace_na(., 0)),
                   total = `0` + `1`,
                   acu_buenos = cumsum(`0`)/sum(`0`),
                   acu_malos = cumsum(`1`)/sum(`1`),
                   x = c(acu_buenos[1], 
                         acu_buenos %>% 
                             map_at(.at = seq(1,length(unique(df[[x]])),2), 
                                    function(x){x*-1}) %>% 
                             unlist %>% 
                             diff %>% 
                             map_if(function(x){x<0}, 
                                    function(y){y*-1}) %>% 
                             unlist
                         ),
                   y = c(acu_malos[1], 
                         acu_malos %>% 
                             diff
                         ),
                   gini = abs(1 - sum(x*y)) 
                   )
        names(table_gini)[1] <- names(df[x])
    }else{
        table_gini <- df %>% 
            mutate(bins = cut(x = df[[x]],
                              breaks = breaks,
                              include.lowest = T, right = F)
                   ) %>% 
            count(bins, df[[y]]) %>% 
            rename(status = `df[[y]]`) %>% 
            spread(key = y, value = n) %>% 
            mutate(across(2:3, ~ replace_na(., 0)),
                   total = `0` + `1`,
                   acu_buenos = cumsum(`0`)/sum(`0`),
                   acu_malos = cumsum(`1`)/sum(`1`),
                   x = c(acu_buenos[1], 
                         acu_buenos %>% 
                             map_at(.at = seq(1,length(breaks),2), 
                                    function(x){x*-1}) %>% 
                             unlist %>% 
                             diff %>% 
                             map_if(function(x){x<0}, 
                                    function(y){y*-1}) %>% 
                             unlist
                         ),
                   y = c(acu_malos[1], 
                         acu_malos %>% 
                             diff
                         ),
                   gini = abs(1 - sum(x*y))
                   )
        names(table_gini)[1] <- names(df[x])
    }
    return(table_gini)
}