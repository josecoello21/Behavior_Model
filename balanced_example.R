# example how ROSE function works
# toy data
data.table(
    a = sample(1:30, 15, replace = T),
    b = sample(41:70, 15, replace = T),
    c = factor(
        sample(x = letters, size = 15, replace = T)
        ),
    response = factor(
        sample(0:1, size = 15, replace = T, prob = c(.9,.1) ), 
        levels = c(0,1)
        )
) -> dt_example

# generation of synthetic data by randomly over sampling examples
ROSE(
    response ~ ., data = dt_example, N = 10, p = .4, seed = 123
)$data -> dt_rose

# clase = 1 in dt_example and clases in dt_rose
dt_example[response == 1]
dt_rose

