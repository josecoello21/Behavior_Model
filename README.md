# Behavior_Model

El Modelo de behavior de tarjetas de crédito tiene como objetivo estimar la probabilidad de incumplimiento de los tarjetahabientes en un período de tiempo, según su propensión de poder registrar una mora de noventa días o más en los próximos seis meses, lo cual permite optimizar la gestión de cobro, también este modelo suele ser una herramienta útil en los aumentos de límite de las tarjetas debido a que nos permite mitigar el riesgo de perdida debido al  incumplimiento.   

Para la realización del modelo se tomarán en cuenta los siguiente pasos:

1. Delimitación de los períodos de observación y desempeño, siendo estos, período de observación: de mayo 2020 a octubre 2020 y período de desempeño: de noviembre 2020 a abril 2021

2. Se excluyen los clientes que a la fecha octubre 2020 (fecha divisoria) se encontraban incumplidos.

3. Etiquetas de buenos/malos clientes basado en la altura máxima de mora alcanzada que se considera como incumplimiento (noventa días) en el período de desempeño (noviembre 2020 a abril 2021).

4. Partición de los datos en un subconjunto de entrenamiento (70%) y un subconjunto de prueba (30%).

5. Con los datos de entrenamiento se construyen posibles variables predictoras en el período de observación (mayo 2020 a octubre 2020).

6. Se evalúa la potencia discriminatoria de cada variable predictora construida en el paso anterior mediante el cálculo del coeficiente gini, luego se seleccionan las variables predictivas y se ajusta el modelo de regresión logística.

7. Con los datos de prueba (30%), y el modelo final, se calculan las probabilidades de incumplimiento y se determina un gini de prueba.

Documentación de los resultados generales [Rpubs](http://rpubs.com/josecoello21/901188)<base target="_top"/>