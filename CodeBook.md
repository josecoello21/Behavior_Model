---
title: "CodeBook"
output: github_document
---

# Conjunto de datos

Los datos disponibles para la construcción del modelo se encuentran en el directorio llamado data_sets, en el se encuentran las siguientes bases de datos:

1. avg_saldos.csv: saldos promedios en cuenta financiera para cada uno de los meses en el período comprendido de mayo 2020 a Diciembre 2020.

2. base_nov20.csv: cartera tdc hasta la fecha noviembre 2020.

3. compra_mes.csv: compras de la TDC para cada uno de los meses en el período comprendido de mayo 2020 a Diciembre 2020.

4. dep_cuenta.csv: suma de depósitos en cuenta financiera para cada uno de los meses en el período comprendido de mayo 2020 a Diciembre 2020. 

5. pago_mes.csv: pago total realizado por el cliente a la TDC para cada uno de los meses en el periodo comprendido de mayo 2020 a Diciembre 2020.

6. pago_min.csv: cuota o pago mínimo correspondiente a la TDC que debe efectuar el cliente, para cada uno de los meses en el período comprendido de mayo 2020 a Diciembre 2020.

7. precio_dolar.csv: referencia del tipo de cambio precio dolar emitida por el Banco Central de Venezuela.

8. saldo_adeudado.csv: saldo adeudado de la TDC para cada uno de los meses en el período comprendido de mayo 2020 a Diciembre 2020.

9. tdc_abr21.txt: cartera total de tarjeta de crédito correspondiente a la fecha de abril 2021.


# Cartera Tarjeta de Crédito

La cartera total de tarjeta de crédito correspondiente a la fecha abril 2021 contiene 213296 registros y 78 variables, sin embargo, solo serán seleccionadas las siguientes variables de interes:

- T5NRTA: número de tarjeta. 
- T5BINE: bine.       
- T5CDSE: sexo.       
- T5DSEC: estado civil.  
- T5DSTR: tipo de residencia.   
- T5PWNB: fecha de nacimiento.  
- T5MQNB: fecha de emisión.  
- T5T3NB: número de dependientes.    
- H3HSNB: vector de comportamiento año 2020.    
- H3HTNB: vector de comportamiento año 2021.    
