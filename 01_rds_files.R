# librerias y funciones
source('libraries_and_functions.R')

# workspace RDS data sets
if(!dir.exists('data_sets_rds')){
    dir.create('data_sets_rds')
    }

# csv and txt files
csv_file <- dir('data_sets')[grepl(pattern = 'csv', x = dir('data_sets'))]
txt_file <- dir('data_sets')[grepl(pattern = 'txt', x = dir('data_sets'))]

# reading csv files 
csv_path <- file.path('data_sets', csv_file)
datos_csv <- lapply(X = csv_path, FUN = read.csv2)
names(datos_csv) <- csv_file

# reading txt files
txt_path <- file.path('data_sets', txt_file)
datos_txt <- lapply(X = txt_path, FUN = read_delim)
names(datos_txt) <- txt_file

# save rds files in data_sets_rds workspace
files <- file.path('data_sets_rds',
                   gsub(pattern = 'csv|txt', 
                        replacement = 'RDS',
                        x = c(csv_file,txt_file) 
                        )
                   ) 
files_rds <- gsub(pattern = 'csv|txt', 
                  replacement = 'RDS',
                  x = c(csv_file,txt_file) )

test <- files_rds %in% dir('data_sets_rds')

if(!all(test)){
    mapply(saveRDS, c(datos_csv, datos_txt), file = files)
    }
