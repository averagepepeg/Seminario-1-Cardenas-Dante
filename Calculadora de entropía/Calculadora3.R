C:/Users/Dante/Desktop/tesis/test

#Esta librería será necesaria para posteriormente poder generar un archivo Excel con el nombre de los archivos
#y su respectiva entropía~

library(openxlsx)

#Esta librería se utiliza por la funcion str_extract() la cual permite obtener el tipo de archivo
library(stringr)

#En esta sección se definen las variables donde se capturará la carpeta que contendrá los archivos de prueba
#y ademas los archivos que esta contiene a los cuales se les calculará la entropía
carpeta <- "C:/Users/Dante/Desktop/tesis/test"

archivos <- list.files(carpeta, full.names = TRUE)

#Aqui generamos un dataframe para almacenar los resultados que se obtendrán y luego poder copiar su información en 
#un documento excel
resultados <- data.frame(nombre_archivo = character(),
                         tipo_archivo = character(),
                         peso_archivo = numeric(),
                         min_entropy = numeric(),
                         markov = numeric(),
                         stringsAsFactors = FALSE)

#Aqui definimos la función de "minima entropía". Este sera uno de nuestros indicadores para el entrenamiento del modelo.
funcion_ent <- function(p) {
  -log2(max(p))
}

#Aqui definimos la funcion para obtener el valor de Markov. Este sera otro indicador.
funcion_markov <- function(p) {
  -1/length(p) * log2(max(p))
}

#En esta sección, se utiliza la logica "for" para recorrer cada archivo de la carpeta de pruebas que se definió arriba.
#Dentro del "for" se lleva a cabo la captura de información correspondiente para guardar en el documento excel 
#Ademas, se utilizará las funciones para realizar el calculo de minima entropía y el valor de Markov en cada archivo.
for (archivo in archivos) {
  #Aqui obtenemos el nombre del archivo
  nombre_archivo <- basename(archivo)
  
  #Con esta formula fue obtenida de internet, y utilizando la librería stringr, se logra extraer el "tipo" de archivo
  #sea este .docx, .pdf, .xls, etc.
  tipo_archivo <- str_to_lower(str_extract(nombre_archivo, "\\.[^.]+$"))
  
  #Esta linea obtiene el peso de cada archivo en kilobytes
  peso_archivo <- file.size(archivo) / 1024
  
  #Aqui se lleva a cabo la descomposición de cada archivo en bytes. Adicionalmente se convierte estos valores binarios
  #a valores enteros puesto que de esta forma se puede tratar cada byte de manera individual y posteriormente calcular
  #su frecuencia.
  contenido_binario <- readBin(archivo, "raw", file.size(archivo))
  contenido_enteros <- as.integer(contenido_binario)
  
  #Aqui se calcula la frecuencia en que aparece cada byte individual en el archivo. La frecuencia se utilizará posteriormente
  #en las formulas para calcular los indicadores actuando como la variable "p"
  frecuencia <- table(contenido_enteros) / length(contenido_enteros)
  
  #Aqui se ejecutan las funciones para calcular la minima entropía y el valor de markov
  min_entropy <- funcion_ent(frecuencia)
  markov <- funcion_markov(frecuencia)
  
  #Finalmente guardamos los resultados en el dataframe creado mas arriba~
  resultados <- rbind(resultados, c(nombre_archivo, tipo_archivo, peso_archivo,
                                    min_entropy, markov))
}

#Renombramos los nombres de las columnas del dataframe para que se vea mejor en el excel
colnames(resultados) <- c("Nombre", "Tipo", "Peso(kb)",
                          "Minima Entropía", "Valor de Markov")

#Se genera una hoja de excel con los resultados guardados del dataframe anteriormente mencionado~
write.xlsx(resultados, "resultados_entropia.xlsx", rowNames = FALSE)