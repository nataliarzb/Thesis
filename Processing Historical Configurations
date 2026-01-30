# ===================================================================================
# PROJECT: Undergraduate Thesis - Processing Historical Configurations
# SCRIPT: 03_Processing_and_Past_Metrics.R
# AUTHOR: Natalia Ramírez Bedoya
#
# OBJECTIVE:
# Preparation and standardization of historical spatial layers.

# Calculation of area and connectivity metrics for past scenarios.

# WORKFLOW:
# 1. Masking/Croping historical rasters according to the study area.

# 2. Reclassification and preparation of coverage layers.

# # 3. Patch Identification and Area and Connectivity Calculation
#
# TOOLS: R (terra, dplyr)

# =================================================================================

# ANALISIS DE DATOS CONFIGURACIONES PASADAS 

library(terra)
library(tools)
library(dplyr)
library(openxlsx)
library(readxl)

# CALCULO PARCHES Y ÁREA -----

# Recortar configuraciones a paramo estricto -------

input_dir <- "01_PARAMOS"   # carpeta con shapefiles (subpáramo a páramo)
output_dir <- "D:/Tesis/Configuraciones_paramo_estricto"  # carpeta de salida
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# DEM base (asegúrate de que tenga mismo CRS que tus shapefiles)
dem <- rast("dem/dem30m_9377.tif")

# Si tienes poco espacio en disco C, usa un temporal alterno:
terraOptions(tempdir = "D:/Tesis/temp_terra")

# Listar shapefiles de entrada
shp_files <- list.files(input_dir, pattern = "\\.shp$", full.names = TRUE)

# BUCLE PRINCIPAL
for (shp_path in shp_files) {
  nombre <- file_path_sans_ext(basename(shp_path))
  message("\n=== Procesando: ", nombre, " ===")
  
  # Cargar shapefile
  ufl <- tryCatch(vect(shp_path), error = function(e) { warning(e); return(NULL) })
  if (is.null(ufl)) next
  
  # Reproyectar si es necesario
  if (is.na(crs(ufl))) crs(ufl) <- crs(dem)
  else ufl <- project(ufl, dem)
  
  # Extraer límites altitudinales desde el nombre
  nums <- unlist(regmatches(nombre, gregexpr("[0-9]+", nombre)))
  if (length(nums) < 2) {
    warning("No se pudieron extraer altitudes de: ", nombre)
    next
  }
  alt_min <- as.numeric(nums[1])
  alt_max <- as.numeric(nums[2])
  
  # Calcular el rango estricto de páramo para esa configuración
  paramo_min <- alt_min + 300
  paramo_max <- alt_max - 200
  message("Rango altitudinal: ", alt_min, "-", alt_max,
          " → páramo estricto: ", paramo_min, "-", paramo_max)
  
  # Recortar DEM a la extensión del shapefile
  dem_crop <- crop(dem, ufl)
  
  # Enmascarar el DEM dentro del área de la configuración
  dem_mask <- mask(dem_crop, ufl)
  
  # Crear máscara altitudinal para el páramo estricto
  mask_paramo <- dem_mask >= paramo_min & dem_mask <= paramo_max
  mask_paramo[mask_paramo == 0] <- NA 
  
  # Convertir la máscara a polígonos
  paramo_estricto_vect <- as.polygons(mask_paramo, dissolve = TRUE, na.rm = TRUE)
  
  # Recortar el shapefile original con la máscara (recorte real)
  shp_recortado <- intersect(ufl, paramo_estricto_vect)
  
  # Guardar shapefile de salida
  out_path <- file.path(output_dir, paste0(nombre, "_estricto.shp"))
  writeVector(shp_recortado, out_path, overwrite = TRUE)
  message("→ Guardado: ", out_path)
  
  # Liberar memoria
  rm(ufl, dem_crop, dem_mask, mask_paramo, paramo_estricto_vect, shp_recortado)
  gc()
  terra::tmpFiles(current = TRUE, remove = TRUE)
}



#Rasterizar shapefiles de páramo estricto (30 m) ----------

# Configuración de rutas 
# Carpeta donde están tus shapefiles resultantes del recorte del páramo estricto
shp_dir <- "D:/Tesis/Configuraciones_paramo_estricto"  

# Carpeta donde se guardarán los rasters binarios (uno por shapefile)
out_rast_dir <- "D:/Tesis/Paramo_estricto_rasters_30m"  
dir.create(out_rast_dir, recursive = TRUE, showWarnings = FALSE)

# DEM 30 m (plantilla base)
dem30 <- rast("dem/dem30m_9377.tif")

# Ajustes de memoria temporales (opcional pero recomendado)
terraOptions(tempdir = "D:/Tesis/temp_terra", memfrac = 0.8)

# Listar shapefiles de páramo estricto 
shps <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)

# Función para rasterizar cada shapefile 
for (sh in shps) {
  name <- file_path_sans_ext(basename(sh))
  message("\n🌱 Rasterizando: ", name)
  
  # Leer el shapefile
  v <- tryCatch(vect(sh), error = function(e) {
    warning("Error al leer ", sh, ": ", e$message)
    return(NULL)
  })
  if (is.null(v)) next
  
  # Asegurar que tenga CRS y coincida con el DEM
  if (is.na(crs(v))) crs(v) <- crs(dem30)
  v <- project(v, dem30)
  
  # Rasterizar: 1 = páramo, NA = fuera
  r <- rasterize(v, dem30, field = 1, background = NA)
  
  # Guardar raster binario
  out_file <- file.path(out_rast_dir, paste0(name, "_30m.tif"))
  writeRaster(r, out_file, overwrite = TRUE)
    
  # Liberar memoria temporal
  rm(v, r); gc()
  terra::tmpFiles(current = TRUE, remove = TRUE)
}




#Agregar páramo estricto a 1 km y generar parches --------

# CONFIGURACIÓN GENERAL

input_folder  <- "D:/Tesis/Paramo_estricto_rasters_30m"  # rasters 30 m
output_folder <- file.path(input_folder, "paramo_1km_parches")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Opcional: evitar llenar C:
terraOptions(tempdir = "D:/Tesis/temp_terra", memfrac = 0.7, todisk = TRUE)

# Listar rasters
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# FUNCIÓN DE MODA (para agregar)
calc_moda <- function(x, ...) {
  ux <- na.omit(unique(x))
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}


# BUCLE PRINCIPAL
for (tif_path in tif_files) {
  nombre <- file_path_sans_ext(basename(tif_path))
  message("\n=== Procesando: ", nombre, " ===")
  
  # Cargar raster 30 m
  r30 <- rast(tif_path)
  
  # Determinar factor de agregación (de 30 m a 1 km ≈ 33.33)
  fact <- round(1000 / res(r30)[1])
  
  # Agregar a 1 km (usando moda)
  r1km <- aggregate(r30, fact = c(fact, fact), fun = calc_moda, na.rm = TRUE)
  
  # Binarizar
  r1km_bin <- ifel(r1km == 1, 1, NA)
  
  # Identificar parches (directions = 8)
  parches_r <- patches(r1km_bin, directions = 8)
  
  # Convertir parches a polígonos
  vect_parches <- as.polygons(parches_r, dissolve = TRUE)
  vect_parches$id_patch <- paste0(nombre, "_P", seq_len(nrow(vect_parches)))
  
  # Guardar resultados
  out_tif <- file.path(output_folder, paste0(nombre, "_1km_bin.tif"))
  out_shp <- file.path(output_folder, paste0(nombre, "_parches_1km.shp"))
  
  writeRaster(r1km_bin, out_tif, overwrite = TRUE)
  writeVector(vect_parches, out_shp, overwrite = TRUE)
  
  
  # Limpieza
  rm(r30, r1km, r1km_bin, parches_r, vect_parches)
  gc(full = TRUE)
  terra::tmpFiles(current = TRUE, remove = TRUE)
}



#Calcular áreas topográficas de parches --------------

library(terra)
library(tools)
library(dplyr)


# CONFIGURACIÓN GENERAL

input_folder  <- "D:/Tesis/paramo_1km_parches"
output_folder <- file.path(input_folder, "areas_topograficas")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

terraOptions(tempdir = "D:/Tesis/temp_terra", memfrac = 0.7, todisk = TRUE)

# DEM base
dem <- rast("D:/Documents/11 semestre/Tesis/dem/dem30m_9377.tif")

# Calcular raster de área topográfica
message("\nCalculando raster de área topográfica...")

area_plana_raster <- cellSize(dem, unit = "m")
pendiente_rad <- terrain(dem, v = "slope", unit = "radians")
factor_jenness <- 1 / cos(pendiente_rad)
area_topo_raster <- factor_jenness * area_plana_raster
names(area_topo_raster) <- "area_topo_m2"

area_topo_raster <- rast("D:/Documents/11 semestre/Tesis/area_topo_raster.tif")

# Listar shapefiles de parches
shp_files <- list.files(input_folder, pattern = "_parches_1km\\.shp$", full.names = TRUE)

# FUNCIÓN PRINCIPAL

procesar_areas <- function(shp_path, area_topo_raster) {
  nombre <- file_path_sans_ext(basename(shp_path))
  message("\n=== Calculando áreas: ", nombre, " ===")
  
  parches <- tryCatch(vect(shp_path), error = function(e) { warning(e); return(NULL) })
  if (is.null(parches)) return(NULL)
  
  # Extraer área topográfica por parche
  area_topo <- extract(area_topo_raster, parches, fun = sum, na.rm = TRUE, ID = FALSE)
  
  df_area <- data.frame(
    id_patch = parches$id_patch,
    area_topo_km2 = area_topo[, 1] / 1e6
  )
  
  # Guardar CSV individual
  out_csv <- file.path(output_folder, paste0(nombre, "_areas.csv"))
  write.csv(df_area, out_csv, row.names = FALSE)
  
  message("✅ Guardado: ", out_csv)
  return(df_area)
}


# Aplicar a todos los parches -----

resultados_area_list <- lapply(shp_files[1], procesar_areas, area_topo_raster = area_topo_raster)
resultados_df <- bind_rows(resultados_area_list, .id = "nombre")

# Guardar consolidado
write.csv(resultados_df,
          file.path(output_folder, "areas_topo_consolidado.csv"),
          row.names = FALSE)

message("\n✅ Proceso completado. Resultados guardados en: ", output_folder)





# CALCULAR CONECTIVIDAD A CADA CONFIGURACION ------

#Matriz de resistencia  -----

library(terra)

# --- 1️⃣ Cargar DEM y recortar a área de estudio ---
dem <- rast("dem/dem_continuo_1km.tif")
dem <- crop(dem, area_estudio, mask=TRUE)

# 2️⃣ Generar 19 configuraciones
elev_min_seq <- seq(2200, 2200 + 18*100, by = 100)  # min de cada config
elev_max_seq <- seq(2900, 2900 + 18*100, by = 100)  # max de cada config

configuraciones <- data.frame(
  nombre = paste0("CFG_", sprintf("%02d", 1:19)),
  min = elev_min_seq,
  max = elev_max_seq
)

# 3️⃣ Función segura para calcular matriz de resistencia
calc_resistencia_paramo <- function(dem, elev_min, elev_max){
  
  dem_vals <- values(dem)                   # vector de valores del DEM
  dem_min <- min(dem_vals, na.rm = TRUE)
  dem_max <- max(dem_vals, na.rm = TRUE)
  
  # Inicializar vector de resistencia
  res_vals <- rep(NA, length(dem_vals))
  
  # --- Dentro del páramo ---
  idx_dentro <- !is.na(dem_vals) & dem_vals >= elev_min & dem_vals <= elev_max
  res_vals[idx_dentro] <- 0
  
  # --- Por debajo del páramo ---
  idx_bajo <- !is.na(dem_vals) & dem_vals < elev_min
  if(any(idx_bajo)){
    dist_abajo <- elev_min - dem_vals[idx_bajo]
    limite_abajo <- elev_min - dem_min
    mult_bajo <- 0.0012 + (dist_abajo / limite_abajo) * (0.0012 - 0.0003)
    mult_bajo[dist_abajo > limite_abajo] <- 0.0003
    res_vals[idx_bajo] <- dist_abajo * mult_bajo
  }
  
  # --- Por encima del páramo ---
  idx_arriba <- !is.na(dem_vals) & dem_vals > elev_max
  if(any(idx_arriba)){
    dist_arriba <- dem_vals[idx_arriba] - elev_max
    limite_arriba <- dem_max - elev_max
    mult_arriba <- 0.0005 + (dist_arriba / limite_arriba) * (0.0015 - 0.0005)
    mult_arriba[dist_arriba > limite_arriba] <- 0.0015
    res_vals[idx_arriba] <- dist_arriba * mult_arriba
  }
  
  # Asignar valores al raster
  resistencia <- dem
  values(resistencia) <- res_vals
  return(resistencia)
}

# 4️⃣ Crear carpeta de salida
dir.create("matriz_resistencia_config", showWarnings = FALSE)

# 5️⃣ Bucle para generar y guardar las 19 matrices
for(i in 1:nrow(configuraciones)){
  
  nombre_cfg <- configuraciones$nombre[i]
  elev_min   <- configuraciones$min[i]
  elev_max   <- configuraciones$max[i]
  
  message("\nProcesando configuración ", nombre_cfg,
          ": min=", elev_min, " / max=", elev_max)
  
  r_res <- calc_resistencia_paramo(dem, elev_min, elev_max)
  
  writeRaster(
    r_res,
    filename = file.path("matriz_resistencia_config",
                         paste0("resistencia_", nombre_cfg, ".tif")),
    overwrite = TRUE,
    datatype = "FLT4S",
    filetype = "GTiff"
  )
}

message("\n✅ Todas las matrices de resistencia generadas correctamente.")


#GRAFICAR

library(ggplot2)

altitudes <- seq(0, 6000, by = 10)  # rango de altitudes

# Función que calcula el costo teórico
calc_costo_alt <- function(alt, elev_min, elev_max){
  costo <- ifelse(
    alt >= elev_min & alt <= elev_max,
    0,
    ifelse(
      alt < elev_min,
      {
        dist_abajo <- elev_min - alt
        mult <- 0.0003 + (dist_abajo / elev_min) * (0.0012 - 0.0003)
        mult[dist_abajo > elev_min] <- 0.0012
        dist_abajo * mult
      },
      {
        dist_arriba <- alt - elev_max
        mult <- 0.0005 + (dist_arriba / (6000 - elev_max)) * (0.0015 - 0.0005)
        mult[dist_arriba > (6000 - elev_max)] <- 0.0015
        dist_arriba * mult
      }
    )
  )
  return(costo)
}

# Bucle para graficar de a una configuración
for(i in 1:nrow(configuraciones)){
  
  elev_min <- configuraciones$min[i]
  elev_max <- configuraciones$max[i]
  nombre_cfg <- configuraciones$nombre[i]
  
  costo <- calc_costo_alt(altitudes, elev_min, elev_max)
  df <- data.frame(Altitud = altitudes, Costo = costo)
  
  p <- ggplot(df, aes(x = Altitud, y = Costo)) +
    geom_line(color = "darkblue", size = 1) +
    geom_vline(xintercept = c(elev_min, elev_max),
               linetype = "dashed", color = "darkgreen", size = 0.8) +
    labs(
      title = paste("Configuración", nombre_cfg),
      subtitle = paste("Alt min =", elev_min, "m / Alt max =", elev_max, "m"),
      x = "Altitud (m)",
      y = "Costo"
    ) +
    theme_minimal(base_size = 13)
  
  print(p)  # imprime la gráfica
  
  # Pausa para ver la gráfica antes de pasar a la siguiente
  readline(prompt = "Presiona [Enter] para ver la siguiente configuración...")
}

#Conectividad -----

library(sf)
library(terra)
library(Makurhini)

# --- 1️⃣ Listar shapefiles de parches y raster de resistencias ---
shapefiles_parches <- list.files("paramo_1km_parches", pattern = "_parches_1km\\.shp$", full.names = TRUE)
rasters_resistencia <- list.files("matriz_resistencia_config", pattern = "\\.tif$", full.names = TRUE)

# Ordenar para que coincidan
shapefiles_parches <- sort(shapefiles_parches)
rasters_resistencia <- sort(rasters_resistencia)

# --- 2️⃣ Inicializar lista para resultados ---
PC_list <- vector("list", length = length(shapefiles_parches))
names(PC_list) <- paste0("CFG_", sprintf("%02d", 1:length(shapefiles_parches)))

# --- 3️⃣ Bucle para calcular PC por configuración ---
for(i in seq_along(shapefiles_parches)) {
  
  nombre_cfg <- names(PC_list)[i]  # toma el nombre ya asignado
  
  # --- Cargar shapefile de parches ---
  nodes <- st_read(shapefiles_parches[i], quiet = TRUE)
  geom_active <- attr(nodes, "geometry")
  nodes <- nodes[, c("id_patch", geom_active)]
  
  # --- Crear atributo constante ---
  nodes$atributo_constante <- rep(1, nrow(nodes))
  
  # --- Cargar raster de resistencia ---
  resistencia_r <- rast(rasters_resistencia[i])
  
  message("\nCalculando PC para ", nombre_cfg)
  
  # --- Calcular PC ---
  PC_result <- MK_dPCIIC(
    nodes = nodes,
    attribute = "atributo_constante",
    distance = list(
      type = "least-cost",
      resistance = resistencia_r,
      distance_unit = "m"
    ),
    metric = "PC",
    probability = 0.05,
    overall = FALSE,
    distance_thresholds = 20000
  )
  
  # --- Guardar resultado en la lista ---
  PC_list[[nombre_cfg]] <- PC_result
}

# ✅ Ahora cada elemento tiene nombre y puedes acceder:
names(PC_list)
# Ejemplo: PC_list[["CFG_01"]] o PC_list$CFG_01



#Guardar resultados
library(writexl)

dir.create("PC_resultados", showWarnings = FALSE)

for(cfg in names(PC_list)) {
  
  # Tomar el dataframe de la configuración
  df <- PC_list[[cfg]]
  
  # Crear ruta del archivo Excel
  file_path <- file.path("resultados_configuraciones/PC_resultados", paste0(cfg, "_PC.xlsx"))
  
  # Guardar en Excel
  write_xlsx(df, path = file_path)
  
  message("Guardado: ", file_path)
}


#guardar en un mismo documento
library(openxlsx)
dir.create("PC_resultados", showWarnings = FALSE)

wb <- createWorkbook()

for(cfg in names(PC_list)) {
  
  # Convertir sf a dataframe sin geometría
  df <- st_set_geometry(PC_list[[cfg]], NULL)
  
  # Crear hoja con nombre de la configuración
  addWorksheet(wb, sheetName = cfg)
  
  # Escribir datos
  writeData(wb, sheet = cfg, x = df)
}

# Guardar workbook
saveWorkbook(wb, "resultados_configuraciones/PC_resultados/PC_todas_configuraciones.xlsx", overwrite = TRUE)

#UNIR TODO (AREA Y CONECTIVIDAD DE TODAS LAS CONFIG)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)


areas_todas <- read.csv("resultados_configuraciones/areas_topograficas/areas_topo_consolidado.csv")
file_pc  <- "resultados_configuraciones/PC_resultados/PC_todas_configuraciones.xlsx"

# 3. Obtener nombres de hojas (hojas = cfg)
hojas <- excel_sheets(file_pc)

# 4. Leer todas las hojas y agregarlas a un solo dataframe
pc_todas <- map_df(hojas, function(cfg_name) {
  
  df <- read_excel(file_pc, sheet = cfg_name)
  
  df %>% mutate(cfg = cfg_name)
})

# 5. Unir PC + Áreas
df_final <- areas_todas %>%
  left_join(pc_todas, by = "id_patch")


# 6. Guardar
write.xlsx(df_final, "resultados_configuraciones/Configuraciones_Area_PC_FINAL.xlsx", row.names = FALSE)

# Ver
View(df_final)



# ORGANIZAR DATOS DE TIEMPOS UFL ----

#Datos UFL
df <- read.xlsx("D:/Documents/11 semestre/Tesis/ParamosFlantua_2023/UpperForestLine-JvB_adjusted.xlsx", sheet = 7)

#Pasar de ancho a largo las columnas de UFL
UFL <- df %>%
  tidyr::pivot_longer(
    cols = `1900`:`3700`,  # Ajusta al rango de columnas de UFL en tu hoja
    names_to = "UFL",
    values_to = "Duracion"
  ) %>%
  # 3. Filtrar donde el valor no sea 0 (presencia o duración positiva)
  dplyr::filter(Duracion != 0) %>%
  # 4. Seleccionar solo las columnas que te interesan
  dplyr::select(Age, UFL, Duracion)


#Filtrar solo datos del último millón de años
UFL <- UFL %>% 
  filter(Age <= 1000)

# Agregar columna cfg
UFL <- UFL %>%
  mutate(
    UFL = as.numeric(UFL),
    cfg = sprintf("CFG_%02d", (UFL - 1900) / 100 + 1)
  )


#Crear identificadores de episodios (bloques consecutivos del mismo cfg)
UFL <- UFL %>%
  mutate(
    cambio = cfg != lag(cfg, default = first(cfg)),
    episodio = cumsum(cambio)
  )


# Resultado
View(UFL)

