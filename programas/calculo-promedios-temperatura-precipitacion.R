#
# CÁLCULO DE PROMEDIOS ANUALES DE TEMPERATURA Y PRECIPITACIÓN
# DE ESPECIES ARBÓREAS IDENTIFICADAS EN PARCELAS DE MUESTREO
#

# Este programa calcula los promedios anuales de temperatura y 
# precipitación de registros de presencia de un conjunto de especies arbóreas
# identificadas en parcelas de muestreo. Para cada especie se obtiene también
# su categoría en la Lista Roja de la UICN.

# Entradas:
# 1. Lista de especies arbóreas identificadas en parcelas de muestreo.
# 2. Lista de especies arbóreas de Costa Rica y sus categorías en la 
#    Lista Roja de la UICN.
# 3. Registros de presencia de especies arbóreas.

# Procesamiento:
# 1. Cálculo de los promedios anuales de temperatura y precipitación 
#    de cada registro de presencia, con base en los datos de 
#    WorldClim (https://www.worldclim.org/).
# 2. Cálculo de los promedios anuales de temperatura y precipitación
#    de cada especie.
# 3. Obtención de la categoría en la Lista Roja de cada especie.

# Salidas:
# 1. Lista de especies arbóreas identificadas en parcelas de muestreo con sus 
#    respectivos promedios de temperatura y precipitación y su categoría en la 
#    Lista Roja de la UICN.
# 2. Listas de registros de presencia para cada especie arbórea identificada 
#    en parcelas de muestreo.



# Paquetes
library(here)
library(readr)
library(readxl)
library(dplyr)
library(sf)
library(terra)
library(geodata)



# PARÁMETROS GENERALES

# Directorio de datos originales
DIRECTORIO_DATOS_ORIGINALES <- here("datos", "originales")

# Directorio de archivos de WorldClim
DIRECTORIO_WORLDCLIM <- here(DIRECTORIO_DATOS_ORIGINALES, "worldclim")


# Especies identificadas en parcelas de muestreo
ARCHIVO_XLSX_ESPECIES_PARCELAS <- here(
  DIRECTORIO_DATOS_ORIGINALES, 
  "Lista Spp Únicas.xlsx"
)

# Especies en Lista Roja
ARCHIVO_XLSX_ESPECIES_LISTAROJA <- here(
  DIRECTORIO_DATOS_ORIGINALES, 
  "Costa Rica Analysis 2024.xlsx"
)

# Registros de presencia de especies
ARCHIVO_ZIP_REGISTROS_PRESENCIA <- here(
  DIRECTORIO_DATOS_ORIGINALES, 
  "points_data.zip"
)
ARCHIVO_CSV_REGISTROS_PRESENCIA <- "points_data.csv"


# Directorio de salidas
DIRECTORIO_SALIDAS <- here("salidas")

# Promedios de temperatura y precipitación y categoría en lista roja para cada especie
ARCHIVO_CSV_PROMEDIOS_CATEGORIAS <- here(
  DIRECTORIO_SALIDAS, 
  "especies-promedios-temperatura-precipitacion-categorias-listaroja.csv"
)

# Directorio de archivos CSV de registros de presencia para cada especie
DIRECTORIO_CSV_REGISTROS_PRESENCIA <- here(
  DIRECTORIO_SALIDAS, 
  "registros-presencia"
)


# Resolución de las caoas WorldClim
RESOLUCION = 0.5



# CARGA DE DATOS
cat("Cargando datos ...\n\n")

# Especies en parcelas de muestreo
especies_parcelas <- 
  suppressMessages(read_xlsx(ARCHIVO_XLSX_ESPECIES_PARCELAS)) |>
  select(family = Familia, species = Accepted_name) |>
  arrange(species)

# Especies en Lista Roja
especies_lista_roja <- 
  suppressMessages(read_xlsx(ARCHIVO_XLSX_ESPECIES_LISTAROJA)) |>
  select(species = Check_TaxonName, category_iucn_redlist = `IUCN_Red_List`)

# Registros de presencia
# Descompresión del archivo ZIP con registros de presencia
unzip(ARCHIVO_ZIP_REGISTROS_PRESENCIA, exdir = tempdir())

# Ruta del archivo descomprimido
archivo_csv_registros_presencia <- file.path(tempdir(), ARCHIVO_CSV_REGISTROS_PRESENCIA)

# Carga de registros de presencia
registros_presencia <- 
  st_read(
    dsn = archivo_csv_registros_presencia,
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude",
      "KEEP_GEOM_COLUMNS=YES"
    ),
    quiet = TRUE
  ) |>
  mutate(ID = row_number()) |>
  select(ID, species = sci_name)

# Asignación del CRS WGS84 a los registros de presencia
st_crs(registros_presencia) <- 4326

# Creación de columnas x e y
registros_presencia <- 
  registros_presencia |>
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2])



# CÁLCULO DE LA TEMPERATURA PROMEDIO ANUAL PARA CADA REGISTRO DE PRESENCIA
cat("Calculando temperatura promedio anual de registros de presencia ...\n\n")

# Obtención de raster de temperatura promedio mensual
temperatura <- worldclim_global(var="tavg", res = RESOLUCION, path=tempdir())

# Almacenamiento del raster
# writeRaster(
#   temperatura, 
#   here(DIRECTORIO_WORLDCLIM, "temperatura.tif"), 
#   overwrite=TRUE
# )

# Extracción de la temperatura promedio de cada mes para cada registro de presencia
temperatura_registros <- terra::extract(temperatura, registros_presencia)

# Cambio de nombres de columnas
colnames(temperatura_registros) <- 
  c("ID", "temp_01", "temp_02", "temp_03", "temp_04", "temp_05", "temp_06", 
          "temp_07", "temp_08", "temp_09", "temp_10", "temp_11", "temp_12")

# Calculo de la temperatura promedio anual para cada registro de presencia
temperatura_registros <- 
  temperatura_registros |>
  mutate(
    temperatura_promedio_anual = rowMeans(
      select(temperatura_registros, starts_with("temp")), 
      na.rm = TRUE
    )
  )

# Unión (join) de registros de presencia y temperatura promedio
registros_presencia <- 
  registros_presencia |>
  left_join(
    select(temperatura_registros, ID, temperatura_promedio_anual), 
    by = "ID"
  )



# CÁLCULO DE LA PRECIPITACIÓN PROMEDIO ANUAL PARA CADA REGISTRO DE PRESENCIA
cat("Calculando precipitación promedio anual de registros de presencia ...\n\n")

# Obtención de raster de precipitación total mensual
precipitacion <- worldclim_global(var="prec", res = RESOLUCION, path=tempdir())

# Almacenamiento del raster
# writeRaster(
#   precipitacion, 
#   here(DIRECTORIO_WORLDCLIM, "precipitacion.tif"), 
#   overwrite=TRUE
# )

# Extracción de la precipitación total de cada mes para cada registro de presencia
precipitacion_registros <- terra::extract(precipitacion, registros_presencia)

# Cambio de nombres de columnas
colnames(precipitacion_registros) <- 
  c("ID", "prec_01", "prec_02", "prec_03", "prec_04", "prec_05", "prec_06", 
          "prec_07", "prec_08", "prec_09", "prec_10", "prec_11", "prec_12")

# Calculo de la precipitación promedio anual para cada registro de presencia
precipitacion_registros <- 
  precipitacion_registros |>
  mutate(
    precipitacion_promedio_anual = rowMeans(
      select(precipitacion_registros, starts_with("prec")), 
      na.rm = TRUE
    )
  )

# Unión (join) de registros de presencia y precipitacion promedio
registros_presencia <- 
  registros_presencia |>
  left_join(
    select(precipitacion_registros, ID, precipitacion_promedio_anual), 
    by = "ID"
  )



# CÁLCULO DE LOS PROMEDIOS ANUALES DE TEMPERATURA Y PRECIPITACIÓN POR ESPECIE
# Y OBTENCIÓN DE CATEGORÍAS EN LA LISTA ROJA
cat("Calculando promedios anuales de temperatura y precipitación por especie ...\n\n")

# Inicialización del data frame de promedios y categorías
promedios_categorias <- data.frame(
  familia = character(),
  especie = character(),
  temperatura_promedio_anual = numeric(),
  precipitacion_promedio_anual = numeric(),
  stringsAsFactors = FALSE
)

# Procesamiento de cada especie
for (i in 1:nrow(especies_parcelas)) {
  especie <- especies_parcelas$species[i]
  familia <- especies_parcelas$family[i]
  
  cat("Procesando", especie, "...\n")
  
  # Registros de presencia de la especie
  registros_especie <- 
    registros_presencia |>
    st_drop_geometry() |>
    filter(species == especie) |>
    select(
      species, 
      x, y, 
      temperatura_promedio_anual, 
      precipitacion_promedio_anual
    ) |>
    rename(especie = species) |>
    mutate(familia = familia, .before = especie)
  
  if (nrow(registros_especie) > 0) {
    temperatura_promedio_anual <- 
      mean(registros_especie$temperatura_promedio_anual, na.rm = TRUE)
    precipitacion_promedio_anual <- 
      mean(registros_especie$precipitacion_promedio_anual, na.rm = TRUE)
  } else {
    temperatura_promedio_anual <- NA
    precipitacion_promedio_anual <- NA    
  }
  
  # Archivos de registros de presencia de la especie
  archivo_csv <- here(
    DIRECTORIO_CSV_REGISTROS_PRESENCIA, 
    paste0(gsub(" ", "_", especie), ".csv")
  )
  write_csv(registros_especie, archivo_csv)  
  
  # Promedios de temperatura y precipitación por especie
  promedios_categorias <- rbind(
    promedios_categorias, 
    data.frame(
      familia = familia,
      especie = especie,
      temperatura_promedio_anual = temperatura_promedio_anual,
      precipitacion_promedio_anual = precipitacion_promedio_anual
    )
  )
}



# OBTENCIÓN DE CATEGORÍAS DE LA LISTA ROJA
cat("Obteniendo categorías de la Lista Roja ...\n\n")

promedios_categorias <-
  promedios_categorias |>
  left_join(
    especies_lista_roja, 
    by = c("especie" = "species")
  ) |>
  select(
    familia, 
    especie,
    categoria_listaroja = category_iucn_redlist,
    temperatura_promedio_anual,
    precipitacion_promedio_anual
  )



# GENERACIÓN DEL ARCHIVO DE SALIDA CON LOS PROMEDIOS Y CATEGORÍAS 
# PARA CADA ESPECIE
cat("Generando archivo de salida de promedios y categorías ...\n\n")

write_csv(promedios_categorias, na = "", ARCHIVO_CSV_PROMEDIOS_CATEGORIAS)

cat("FIN\n")