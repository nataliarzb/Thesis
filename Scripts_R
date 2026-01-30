# ===================================================================================
# PROJECT: Undergraduate Thesis - Effects of area and connectivity in the present and in the last million years on the bird richness of the páramo in the Northern Andes
# SCRIPT: 01_Determinacion_Parches_Area_Conectividad.R
# AUTHOR: Natalia Ramírez Bedoya
# DATE: 2025
#
# OBJECTIVE:
# This script processes raster layers to identify habitat patches,
# calculate area metrics, and determine current connectivity indices


# TOOLS: R (terra, dplyr, tidyterra, ggspatial, mapview)
# =================================================================================


library(terra)
library(openxlsx)
library(readxl)
library(sf)
library(readr)
library(ggplot2)
library(ggspatial) 
library(tidyterra)
library(mapview)


setwd("D:/Documents/11 semestre/Tesis")

# ----------- CARGAR MAPA PARAMO Y ESTABLECER PARCHES -------------
#Cargar DEM de 30m
dem_30m_rast <- rast("dem30m_Andes_Norte.tif")
#CRS destino
crs_destino_9377 <- "EPSG:9377" 

# reproyectar con interpolación bilineal (adecuada para valores continuos como elevación)
dem30m_9377 <- project(dem_30m_rast, crs_destino_9377, method = "bilinear", 
                       filename = "D:/Documents/11 semestre/Tesis/dem30m_Andes.tif",
                       overwrite = TRUE)
writeRaster(dem30m_9377, "dem30m_9377.tif")
dem30m_9377 <- rast("D:/Documents/11 semestre/Tesis/dem/dem30m_9377.tif")

#Crear raster binario de páramo a 30m
paramo_binario_30m <- ifel(dem30m_9377 >= 3500 & dem30m_9377 <= 4200, 1, NA)
writeRaster(paramo_binario_30m, "paramo_binario_30m.tif")
paramo_binario_30m <- rast("resultados_paramo_ecorr/paramo_binario_30m.tif")


#Establecer binario a 1km (moda)
res_dem30m <- res(dem30m_9377)
factor_x <- round(1000 / res_dem30m[1])
factor_y <- round(1000 / res_dem30m[2])

# Definir función de moda
calc_moda <- function(x, ...) {
  ux <- na.omit(unique(x))
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

paramo_1km_9377 <- aggregate(paramo_binario_30m, fact = c(factor_x, factor_y), fun = calc_moda, na.rm = TRUE, filename = "D:/paramo_1km.tif",  # escribe directo al disco
                             overwrite = TRUE)
paramo_bin_1km_9377 <- ifel(paramo_1km_9377 == 1, 1, NA)
writeRaster(paramo_1km_9377, "paramo_1km_9377.tif")
paramo_1km_9377 <- rast("resultados_paramo_ecorr/paramo_1km_9377.tif")

# Identificar parches de páramo
parches_1km_9377 <- patches(paramo_bin_1km_9377, directions = 8)

library(plotly)
library(tidyterra)
plot_dinamico <- ggplot() +
  geom_spatvector(data = parches_1km_3200)
ggplotly(plot_dinamico)

writeRaster(parches_1km_9377, "D:/Documents/11 semestre/Tesis/parches_1km_9377.tif")
parches_1km_9377 <- rast("D:/Documents/11 semestre/Tesis/parches_1km_9377.tif")

# Convertir parches a polígonos
vectores_parches_1km <- as.polygons(parches_1km_9377, dissolve = TRUE)

#guardar todos los parches
writeVector(vectores_parches_1km, "D:/Documents/11 semestre/Tesis/vectores_parches_1km.shp")

#--------------- DEFINIR AREA DE ESTUDIO CON ECOREGIONES--------------------------
ecorregiones_sel <- vect("resultados_paramo_ecorr/paramos_ecorregiones.shp")

#Reproyectar mapas ecorregiones al mismo CRS de parches 
ecorregiones_sel  <- project(ecorregiones_sel,  crs(vectores_parches_1km))

#Calcular buffer de 5km para evitar que quede muy cerca a bordes 
#Crear polígono único con el borde externo
v_union <- aggregate(ecorregiones_sel)  # disuelve en una sola geometría
#Hacer buffer 
eco_buf <- buffer(v_union, width = 5000)
plot(eco_buf)

area_eco <- area_estudio[1,]
plot(area_eco)
writeVector(area_eco, "area_eco.shp")

plot(area_estudio)
#ID unico de parches para enlaces de info futura
vectores_parches_1km$id_patch <- paste0("P", 1:nrow(vectores_parches_1km))

#algunos parches quedan fuera del area de estudio, eliminarlos con interseccion
vectores_parches_1km_df <- intersect(vectores_parches_1km, eco_buf)

#Solo con bufer sin ecorregiones especificas
todo_buf <- rbind(eco_buf, vectores_parches_1km_df)

#area estudio 
area_estudio <- vect("parches/area_estudio_buf.shp")
#quitar primer parche 
area_estudio <- area_estudio[-1,]

#------------------AJUSTE MAPAS DE DISTRIBUCION Y CALCULO DE RIQUEZA POR PARCHE ------------------------------------------------------------------------

#Mapas de distribucion Birdlife
especies <- vect("Lista de especies/mapas_distribucion_andes.shp")

#reproyectar
especies <- project(especies, crs(area_estudio))

#Cargar el raster base con resolución y extensión del área de estudio ---
r_ref <- rast("D:/Documents/11 semestre/Tesis/parches/parches_1km_9377.tif")

#Crear carpeta de salida
dir.create("D:/Documents/11 semestre/Tesis/Lista de especies/Aves_paramo_paises", showWarnings = FALSE)

#Lista de especies
especies_lista <- unique(especies$sci_name)

#Loop para generar un raster por especie
for (sp in especies_lista) {
  cat("Procesando:", sp, "\n")
  
  # Filtrar los polígonos de esa especie
  sub <- especies[especies$sci_name == sp, ]
  
  # Rasterizar con referencia a parches_1km_9377
  r_sp <- rasterize(sub, r_ref, field = 1)
  
  # Convertir NA a 0 (fuera del rango de la especie)
  r_sp[is.na(r_sp)] <- 0
  
  # Recortar al área de estudio
  r_sp <- mask(r_sp, area_estudio)
  
  # Crear nombre limpio del archivo
  nombre <- paste0(
    "D:/Documents/11 semestre/Tesis/Lista de especies/Aves_paramo_paises/",
    gsub(" ", "_", sp),
    ".tif"
  )
  
  # Guardar el raster
  writeRaster(r_sp, nombre, overwrite = TRUE)  
}

#####STACK DE MAPAS 

#Carpeta donde están los .tif de distribución de BirdLife
carpeta_BL_in <- "D:/Documents/11 semestre/Tesis/Lista de especies/Aves_paramo_paises"

#Listar archivos .tif
archivos_BL <- list.files(carpeta_BL_in, pattern = "\\.tif$", full.names = TRUE)
length(archivos_BL)  # número de especies cargadas

#Crear lista de rasters (sin stack para ahorrar memoria)
BL_stack_1km <- lapply(as.list(archivos_BL), rast)

#Guardar en un archivo RData
save(BL_stack_1km, file = "D:/Documents/11 semestre/Tesis/Lista de especies/BL_stack_1km_9377.RData")
load("D:/Documents/11 semestre/Tesis/Lista de especies/BL_stack_1km_9377.RData")


#Extraer valores (presencia por parche)
detalle_extraccion <- lapply(
  BL_stack_1km,
  FUN = extract,
  y   = area_estudio,
  fun = sum,
  na.rm = TRUE
)

#Revisar si cada elemento tiene la misma cantidad de filas que parches
lapply(detalle_extraccion, dim)

# Extraer solo la columna de valores
valores <- lapply(detalle_extraccion, function(df) df[, ncol(df)])

# Combinar todo en un data.frame
detalle_extraccion_df <- do.call("cbind", valores)
detalle_extraccion_df <- data.frame(
  id_patch = area_estudio$id_patch,
  detalle_extraccion_df
)

# Renombrar columnas con los nombres de las especies
nombres_rasters <- tools::file_path_sans_ext(basename(archivos_BL))
colnames(detalle_extraccion_df)[-1] <- nombres_rasters

#Calcular riqueza por parche
detalle_extraccion_df$riqueza <- apply(
  detalle_extraccion_df[, -1],
  1,
  function(fila) sum(!is.na(fila) & fila != 0)
)

tabla_riqueza_andes <- detalle_extraccion_df

#Guardar
write.xlsx(detalle_extraccion_df, "D:/Documents/11 semestre/Tesis/Lista de especies/tabla_riqueza_andes_nobuf.xlsx")
tabla_riqueza_andes <- read_excel("D:/Documents/11 semestre/Tesis/Lista de especies/tabla_riqueza_andes_nobuf.xlsx")

# ---------------- CÁLCULO DE AREA PARCHE ----------------

#Recortar dem a area de estudio
dem30m_9377 <- crop(dem30m_9377, area_estudio, mask=TRUE)
plot(dem30m_9377)
config <- vect("01_PARAMOS/Paramo3700_4900.shp")

#AREA CON DEM 30M
# Raster de área plana real de cada celda (usa cellSize, es robusto)
area_plana_raster <- cellSize(dem30m_9377, unit="m")  # m²
names(area_plana_raster) <- "area_plana_m2"

# Raster de área topográfica (factor Jenness * área por celda)
#terrain usa diferencias con celdas vecinas, por eso necesita un DEM continuo.
pendiente_grados <- terrain(dem30m_9377, v="slope", unit="degrees")
pendiente_radianes <- pendiente_grados * pi / 180 #la funcion cos espera radianes
factor_jenness <- 1 / cos(pendiente_radianes)

area_topo_raster <- factor_jenness * area_plana_raster
names(area_topo_raster) <- "area_topo_m2"

writeRaster(area_plana_raster, "area_plana_raster_recortado.tif", overwrite = TRUE)
writeRaster(area_topo_raster, "area_topo_raster_recortado.tif", overwrite = TRUE)

area_topo_raster <- rast("area_topo_raster.tif")

dem <- crop(dem30m_9377, area_estudio, mask = TRUE)

# 3. Extraer suma por parche (en m²)
area_plana_parche <- extract(area_plana_raster, area_estudio, fun=sum, na.rm=TRUE)
names(area_plana_parche)[2] <- "area_plana_m2"
area_plana_parche$id_patch <- area_estudio$id_patch

area_topo_parche <- extract(area_topo_raster, area_estudio, fun=sum, na.rm=TRUE)
names(area_topo_parche)[2] <- "area_topo_m2"
area_topo_parche$id_patch <- area_estudio$id_patch

# Conversión a hectáreas y km²
area_topo_parche$area_topo_ha   <- area_topo_parche$area_topo_m2 / 1e4
area_topo_parche$area_topo_km2  <- area_topo_parche$area_topo_m2 / 1e6
area_plana_parche$area_plana_ha <- area_plana_parche$area_plana_m2 / 1e4
area_plana_parche$area_plana_km2  <- area_plana_parche$area_plana_m2 / 1e6


#  Guardar resultados
tabla_areas_andes <- as.data.frame(area_topo_parche)
write.xlsx(tabla_areas_andes, "D:/Documents/11 semestre/Tesis/tabla_areas_andes.xlsx")
tabla_areas_andes <- read_excel("D:/Documents/11 semestre/Tesis/resultados_matriz_1/tabla_areas_andes_no_buf.xlsx")

# 4. Comparar
comparacion <- merge(area_plana_parche[,c("id_patch","area_plana_m2")],
                     area_topo_parche[,c("id_patch","area_topo_m2")],
                     by="id_patch")

# Conversión a hectáreas y km²
comparacion$area_plana_ha  <- comparacion$area_plana_m2 / 1e4
comparacion$area_topo_ha   <- comparacion$area_topo_m2 / 1e4
comparacion$area_plana_km2 <- comparacion$area_plana_m2 / 1e6
comparacion$area_topo_km2  <- comparacion$area_topo_m2 / 1e6
comparacion$factor_pendiente <- comparacion$area_topo_m2 / comparacion$area_plana_m2

summary(comparacion$factor_pendiente)

# Factor de pendiente (≥1 si está todo bien)
comparacion$factor_pendiente <- comparacion$area_topo_m2 / comparacion$area_plana_m2
#sacar de tabla guardada
tabla_areas_andes$factor_pendiente <- tabla_areas_andes$area_topo_m2 / area_plana_parche$area_plana_m2

write.xlsx(tabla_areas_andes, "D:/Documents/11 semestre/Tesis/areas_30m_factorpendiente.xlsx")

#revisar pendientes mayores a 2 y menores a 1 (posibles errores)
subset(comparacion, factor_pendiente > 2)
#los parches de estas pendientes son muy pequeños pueden dar resultados “raros” por redondeos.
subset(comparacion, factor_pendiente < 1)


# FORMA 2 CÁLCULO ÁREA: #Hipótesis cantidad de hábitat (Fahrig, 2013) ------------------

area_estudio$area_m2 <- tabla_areas_andes$area_topo_m2

centroides <- centroids(area_estudio)
radios <- 20000

# Cargar dem usado para area 1 
dem <- rast("dem/dem30m_9377.tif")

for (r in radios) {
  message("\n=== radio ", r/1000, " km ===")
  buffs_cent <- buffer(centroides, width = r)
  buffs_cent$ID_buff <- centroides$id_patch
  
  suma_area_m2 <- numeric(nrow(buffs_cent))
  names(suma_area_m2) <- buffs_cent$ID_buff
  
  for (i in seq_len(nrow(buffs_cent))) {
    buff_i <- buffs_cent[i, ]
    id_i <- buff_i$ID_buff
    
    # 1) identificar índices de parches que intersectan (relate evita listas raras)
    rel_vec <- relate(area_estudio, buff_i, relation = "intersects")
    idx <- which(rel_vec)
    if (length(idx) == 0) {
      suma_area_m2[i] <- 0
      next
    }
    
    area_total_m2 <- 0
    # 2) Para cada parche identificado, recortar solo la PORCION dentro del buffer
    for (k in idx) {
      parche_k <- area_estudio[k, ]
      solape <- try(intersect(parche_k, buff_i), silent = TRUE)
      
      # normalizar salida de intersect()
      if (inherits(solape, "try-error") || is.null(solape)) next
      if (is.list(solape)) {
        solape <- solape[sapply(solape, inherits, "SpatVector")]
        if (length(solape) == 0) next
        solape <- do.call(rbind, solape)
      }
      if (!inherits(solape, "SpatVector") || nrow(solape) == 0) next
      
      # 3) recortar raster al solape y sumar área topográfica (m2)
      rc <- try(crop(area_topo_raster, solape), silent = TRUE)
      if (inherits(rc, "try-error")) next
      rmask <- mask(rc, solape)
      if (is.null(rmask) || all(is.na(values(rmask)))) next
      
      v <- global(rmask, "sum", na.rm = TRUE)[1,1]
      if (!is.na(v)) area_total_m2 <- area_total_m2 + v
    }
    
    suma_area_m2[i] <- area_total_m2
    message("ID ", id_i, ": sum_m2 = ", round(area_total_m2), " (", round(area_total_m2/1e6,3), " km2).")
  }
  
  col_m2 <- paste0("hab_", r/1000, "km_m2")
  col_km2 <- paste0("hab_", r/1000, "km_km2")
  area_estudio[[col_m2]] <- suma_area_m2[area_estudio$id_patch]
  area_estudio[[col_km2]] <- (suma_area_m2/1e6)[area_estudio$id_patch]
  
  message("Guardadas columnas: ", col_m2, " y ", col_km2)
}

areas_hab <- as.data.frame(area_estudio[,c("id_patch", "hab_20km_km2")])

write.xlsx(areas_hab, "areas_hab.xlsx")
areas_hab <- read.xlsx("areas_hab.xlsx")
print(areas_hab)

#-----------VISUALIZAR

visualizar_parche <- function(id_patch, area_estudio, radios = c(5000, 10000, 20000)) {
  
  # Seleccionar parche objetivo
  target_patch <- area_estudio[area_estudio$id_patch == id_patch, ]
  if (nrow(target_patch) == 0) stop("ID no encontrado en area_estudio")
  
  target_centroid <- centroids(target_patch)
  
  # Preparar visualización
  vista <- NULL
  
  for (r in radios) {
    message("Procesando radio ", r/1000, " km...")
    
    #  Crear buffer
    buff <- buffer(target_centroid, width = r)
    buff$ID_buff <- id_patch
    buff$radio_km <- r/1000
    
    #  Intersecciones con todos los parches
    inters <- intersect(buff, area_estudio)
    if (!inherits(inters, "SpatVector")) {
      inters <- NULL
    }
    
    # Recorte del entorno para ver solo lo relevante
    extent_view <- ext(buff)
    area_focus <- crop(area_estudio, extent_view)
    
    # Construcción del mapa
    capa_base <- mapview(area_focus,
                         col.regions = "lightgreen",
                         alpha.regions = 0.3,
                         layer.name = paste0("Parches cercanos (", r/1000, " km)"))
    
    capa_buffer <- mapview(buff,
                           color = "blue", lwd = 2,
                           alpha.regions = 0,
                           layer.name = paste0("Buffer ", r/1000, " km"))
    
    capa_inters <- if (!is.null(inters)) {
      mapview(inters,
              col.regions = "orange",
              alpha.regions = 0.5,
              layer.name = paste0("Intersecciones ", r/1000, " km"))
    } else NULL
    
    capa_patch <- mapview(target_patch,
                          col.regions = "red",
                          alpha.regions = 0.8,
                          layer.name = paste0("Parche ", id_patch))
    
    capa_cent <- mapview(target_centroid,
                         col.regions = "black",
                         cex = 2,
                         layer.name = "Centroide")
    
    # Mezcla de capas
    mapa_r <- capa_base + capa_buffer + capa_patch + capa_cent
    if (!is.null(capa_inters)) mapa_r <- mapa_r + capa_inters
    
    # Acumular mapas por radio
    if (is.null(vista)) {
      vista <- mapa_r
    } else {
      vista <- vista + mapa_r
    }
  }
  
  return(vista)
}

library(mapview)

visualizar_parche("P243", area_estudio, radios = c(20000))


p24 <- area_estudio %>% 
  filter(id_patch == "P24" | id_patch == 24)   # cubre ambos casos (texto o número)

mapview(p24, col = "orange")
mapview(p24, col.regions = "orange",   # color del polígono
                alpha.regions = 0.6)   # transparencia
mapview(p24,
        map.types = "CartoDB.DarkMatter",
        col.regions = "red",
        alpha.regions = 0.7) 


#PREDICCIÓN DE DISTANCIA DE DISPERSIÓN ----------------------

#Sacar HWI de especies 
especies_lista <- read_csv("especies_CVE.csv")
HWI <- read_excel("D:/Documents/11 semestre/Tesis/Capacidad_dispersion/Dataset HWI 2020-04-10 (2).xlsx")

names(especies_lista)
names(HWI)

#Poner mismo nombre para nombre
names(HWI)[names(HWI) == "Species name"] <- "species"

#Filtrar el Excel para quedarte solo con las especies que están en el CSV
datos_filtrados <- HWI %>%
  filter(species %in% especies_lista$species) %>%
  select(species, HWI)  # sólo conservar estas columnas

#Guardar el resultado en un nuevo archivo
write.xlsx(datos_filtrados, "HWI_especies_filtradas.xlsx")

##### aqui filtré la tabla anterior con la lista real final de especies (las 95)
datos_filtrados <- read.xlsx("HWI_def.xlsx")

# CONSIDERANDO CLARAMUNT (2021)
library(ggplot2)

#Intercepto y coeficientes del modelo HWI segun Claramunt (2021)
#ln(Dkm)=−0.77+1.00× logHWI
Intercepto <- -0.77   
Coeficiente <- 1.00 

datos_filtrados$logHWI <- log(datos_filtrados$HWI)
#Calcular log(distancia)
datos_filtrados$lnD_pred <- Intercepto + Coeficiente * datos_filtrados$logHWI
#Calcular distancia predicha en km, aplicar exponencial para tener distancias reales
datos_filtrados$dist_pred_km <- exp(datos_filtrados$lnD_pred)

ggplot(datos_filtrados, aes(x = logHWI, y = dist_pred_km)) +
  geom_point(color = "blue", size = 2) +          # puntos
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # línea de tendencia
  labs(x = "HWI", y = "Distancia de dispersión natal km") +
  theme_minimal()

mean(datos_filtrados$dist_pred_km)
#media: 15.074

# CONSIDERANDO WEEK
#HWI sin escalar
HWI_original <- datos_filtrados$HWI  

# transformar igual que ellos
media_articulo <- 35.23545
sd_articulo <- 11.57403

datos_filtrados$HWI_scaled_Week <- (HWI_original - media_articulo)/(2*sd_articulo)

# aplicar el modelo
Intercepto <- 2.697  # ejemplo, revisa el del paper
Coeficiente <- 0.858 # el del paper

datos_filtrados$logDist_pred_Week <- Intercepto + Coeficiente * datos_filtrados$HWI_scaled_Week
datos_filtrados$dist_pred_km_Week <- exp(datos_filtrados$logDist_pred_Week)

mean(datos_filtrados$dist_pred_km_Week)
# media: 17.1196




#CALCULO DE CONECTIVIDAD ACTUAL ------------------------

library(sf)
library(devtools)
library(remotes)
library(igraph)
install_github("connectscape/Makurhini" , dependencies =  TRUE , upgrade =  "never", force = TRUE)
install.packages("dPCIC", lib = "C:/R/win-library")
library(Makurhini)
library(future)
install.packages(c("future", "furrr"))
remotes::install_github("OscarGOGO/Makurhini")


# Matriz de resistencia ------------------------------------


#Cargar dem 30m a 1km
#Establecer binario a 1km (moda)
#res_dem30m <- res(dem30m_9377)
#factor_x <- round(1000 / res_dem30m[1])
#factor_y <- round(1000 / res_dem30m[2])
#dem_continuo_1km <- aggregate(dem30m_9377, fact = c(factor_x, factor_y), fun = mean, na.rm = TRUE)

dem_continuo_1km <- rast("dem/dem_continuo_1km.tif")

#Rangos de elevación del páramo
elevacion_paramo_min <- 3500
elevacion_paramo_max <- 4200

#distancias abajo y arriba
dist_abajo <- elevacion_paramo_min - dem_continuo_1km
dist_arriba <- dem_continuo_1km - elevacion_paramo_max

#recortar dem a area de estudio 
dem_continuo_1km <- crop(dem_continuo_1km, area_estudio, mask=TRUE)

resistencia_matriz <- ifel(
  #Dentro del páramo → costo 0
  dem_continuo_1km >= elevacion_paramo_min & dem_continuo_1km <= elevacion_paramo_max,
  0,
  
  #Por debajo del páramo
  ifel(
    dem_continuo_1km < elevacion_paramo_min,
    {
      dist_abajo <- elevacion_paramo_min - dem_continuo_1km
      # Escala lineal desde 0 en 3500 m hasta 1 en 500 m (3 km de rango)
      # multiplicador total lineal: desde 0 hasta 0.003
      mult <- 0.0012 + (dist_abajo / 3500) * (0.0012 - 0.0003)
      mult[dist_abajo > 3500] <- 0.0003  # saturar abajo de 1500
      mult * dist_abajo
    },
    
    # Por encima del páramo
    {
      dist_arriba <- dem_continuo_1km - elevacion_paramo_max
      # Escala lineal desde 0 en 4700 m hasta 1 en 6700 m (2 km de rango)
      mult <- 0.0005 + (dist_arriba / 2000) * (0.0015 - 0.0005)
      mult[dist_arriba > 2000] <- 0.0015  # saturar arriba de 6700
      mult * dist_arriba
    }
  )
)

plot(resistencia_matriz,
     col = hcl.colors(100, "viridis"),
     main = "Matriz de resistencia",
     asp = 1)



#GRAFICAR ---------------------------------------- 

altitudes <- seq(0, 6000, by = 10)


dist_abajo_g <- elevacion_paramo_min - altitudes
dist_arriba_g <- altitudes - elevacion_paramo_max

# Función continua lineal para el costo
costo <- ifelse(
  #Dentro del páramo: costo 0
  altitudes >= elevacion_paramo_min & altitudes <= elevacion_paramo_max,
  0,
  
  #Por debajo del páramo
  ifelse(
    altitudes < elevacion_paramo_min,
    {
      dist_abajo_g <- elevacion_paramo_min - altitudes
      # Escala lineal desde 0 en 3500 m hasta 1 en 3500 m (3 km de rango)
      # multiplicador total lineal: desde 0 hasta 0.003
      mult <- 0.0003 + (dist_abajo_g / 3500) * (0.0012 - 0.0003)
      mult[dist_abajo_g > 3500] <- 0.0012  # saturar abajo de 1500
      mult * dist_abajo_g 
    },
    
    #Por encima del páramo
    {
      dist_arriba_g <- altitudes - elevacion_paramo_max
      # Escala lineal desde 0 en 4700 m hasta 1 en 6700 m (2 km de rango)
      mult <- 0.0005 + (dist_arriba_g / 2000) * (0.0015 - 0.0005)
      mult[dist_arriba_g > 2000] <- 0.0015  # saturar arriba de 6700
      mult * dist_arriba_g
    }
  )
)

df <- data.frame(Altitud = altitudes, Costo = costo)


ggplot(df, aes(x = altitudes, y = costo)) +
  geom_line(color = "darkblue", size = 1) +
  geom_vline(xintercept = c(elevacion_paramo_min, elevacion_paramo_max),
             linetype = "dashed", color = "darkgreen", size = 0.8) +
  labs(
    title = "Comportamiento de la matriz de resistencia",
    subtitle = "Costo medio según la altitud",
    x = "Altitud (m)",
    y = "Costo"
  ) +
  theme_minimal(base_size = 13)



# PREPARAR PARA MAKURHINI


# Reemplazar NAs por costo muy alto para definir areas intransitables
res_calc <- resistencia_matriz
res_calc[is.na(res_calc)] <- 10

library(raster)
# Convertir a RasterLayer para Makurhini
resistencia_r <- raster(res_calc)
crs(resistencia_r) <- crs(area_estudio)

writeRaster(resistencia_r, "resistencia_r_dem30.tif")

plot(res_calc, 
     col = hcl.colors(100, "viridis"),
     main = "Matriz de resistencia")

#Preparar parches

#Preparar nodos de area de estudio 
library(sf)
#quitar primer parche (buffer de ecorregiones)
area_estudio <- area_estudio[-1,]
#area estudio buffer convertir SpatVector a sf
area_estudio_sf <- st_as_sf(area_estudio)
#para que quede solo una geometria
geom_active <- attr(area_estudio_sf, "geometry")
area_estudio_sf <- area_estudio_sf[, c("id_patch", geom_active)] 


#----Conectividad sin efecto de area ---------------------------------------------- 

#poner mismo peso a todos los parches como atributo de reemplazo del área (para attribute)
n_patches <- nrow(area_estudio_sf)      
atributo_constante <- rep(1, n_patches)
#Añadir el atributo constante a los parches para que Makurhini reconozca
area_estudio_sf$atributo_constante <- atributo_constante

PC_Sin_Area <- MK_dPCIIC(nodes = area_estudio_sf,
                         attribute = "atributo_constante",
                         distance = list(type = "least-cost", 
                                         resistance = resistencia_r, distance_unit = "m"),
                         metric = "PC", probability = 0.05,
                         overall = FALSE,
                         distance_thresholds = 20000)


#Extraer conectividad como tabla
PC_df_NoArea <- as.data.frame(PC_Sin_Area, xy = FALSE, na.rm = TRUE)

# Renombrar columnas para evitar conflictos
names(PC_df_NoArea)[3:6] <- c("dPC_NoArea", "dPCintra_NoArea", "dPCflux_NoArea", "dPCconnector_NoArea")

#Union con tabla area y riqueza
tabla_PC_NoArea <- merge(tabla_completa_nobuf, PC_df_NoArea, by = "id_patch")
#Eliminar la columna 'geometry' del data.frame
tabla_PC_NoArea$geometry <- NULL
names(tabla_PC_NoArea)
#eliminar columnas de especies
tabla_PC_NoArea <- tabla_PC_NoArea[,-(6:101)]
tabla_PC_NoArea <- tabla_PC_NoArea[,-c((3:4),7)]

View(tabla_PC_NoArea)
#Guardar
write.xlsx(tabla_PC_NoArea, "tabla_PC_NoArea.xlsx")
tabla_PC_NoArea <- read_excel("resultados_matriz_1/tabla_PC_NoArea.xlsx")


# VARIABLES ALEATORIAS -----------

#VARIABLE ALEATORIA: ECORREGIONES DE CADA PARCHE -----

area_estudio <- area_estudio[-1,]
#CORDILLERAS------
cordilleras_andes <- vect("cordilleras_andes_combinadas.shp")
cordilleras_andes <- project(cordilleras_andes, crs(area_estudio))
names(cordilleras_andes)

#intersectar parches y ecorregiones
pc <- intersect(area_estudio, cordilleras_andes)
pc <- vect(pc)

library(sf)

# Convertir a sf
area_sf <- st_as_sf(area_estudio)
cord_sf <- st_as_sf(cordilleras_andes)

# Intersección conservando atributos de ambos
pc_sf <- st_intersection(area_sf, cord_sf)

# Volver a SpatVector si quieres
pc <- vect(pc_sf)

# Calcular área
pc$area_intersect <- expanse(pc, unit = "km")

# convertir a data.frame para manipulación
pc_df <- as.data.frame(pc)

# para cada parche, quedarnos con la cordillera que tenga mayor área superpuesta
pc_dom <- pc_df %>%
  group_by(id_patch) %>%
  slice_max(area_intersect, n = 1, with_ties = FALSE)
View(pc_dom)
parches_cordilleras <- pc_dom[,c("id_patch", "cordillera")]
parches_cordilleras <- data.frame(parches_cordilleras)

#quedan solo 267 parches, faltan dos, cuales?
# IDs originales
ids_original <- area_estudio$id_patch

# IDs que quedaron después del proceso (pc_dom o parches_cordilleras)
ids_presentes <- unique(pc_dom$id_patch)

# IDs que NO quedaron
ids_faltantes <- ids_original[!ids_original %in% ids_presentes]
ids_faltantes

#quedaron fuera de poligonos de cordilleras, son de la oriental col

#agregarlos manualmente (por ahora)
faltantes_df <- data.frame(
  id_patch = ids_faltantes,
  cordillera = "oriental"
)

pc_final <- dplyr::bind_rows(pc_dom, faltantes_df)

parches_cordilleras <- pc_final[,c("id_patch", "cordillera")]
parches_cordilleras <- data.frame(parches_cordilleras)


library(dplyr)
parches_cordilleras <- parches_cordilleras|>
  mutate(cordillera = recode(cordillera,
                             "Central" = "Cordillera central",
                             "Merida" = "Cordillera de Mérida",
                             "occidental" = "Cordillera occidental Ecuador",
                  "occidental_col"="Cordillera occidental Colombia",
    "oriental" = "Cordillera oriental Colombia",
    "oriental_real" = "Cordillera oriental real Ecuador",
    "SNSM" = "Sierra Nevada de Santa Marta"
  ))

write.xlsx(parches_cordilleras, "parches_cordilleras.xlsx")
unique(parches_cordilleras$cordillera)
parches_cordilleras <- read.xlsx("parches_cordilleras.xlsx")


#VARIABLE ALEATORIA: CLUSTER DE PARCHE -----
#Agregar un efecto aleatorio cluster por distancias (correcion autocorrelacion espacial)

#hallar coords 
area_estudio <- area_estudio[-1,]
centroides <- centroids(area_estudio)        
coords <- crds(centroides)
tabla_todo_sin_buff$x <- coords[, 1]   # Easting
tabla_todo_sin_buff$y <- coords[, 2]   # Northing

vector_dist <- vect(tabla_todo_sin_buff, geom=c("x", "y"), crs="epsg:9377")

library(terra)

#Calcular matriz de distancias entre centroides

X0 <- distance(vector_dist)           # matriz de distancias en metros
distancias <- as.dist(X0)            # objeto tipo dist para clustering

#Clustering jerárquico
hc <- hclust(distancias, method = "complete")  # clustering jerárquico

#Definir grupos/categorías de parches
umbral_km <- 150
clusters <- cutree(hc, h = umbral_km * 1000) 

# Agregar cluster a tabla_dist
tabla_todo_sin_buff$cluster <- clusters

X1 <- X0[lower.tri(X0, diag = FALSE)] / 1000  # pasar a km
pares <- combn(nrow(vector_dist), 2)

X <- data.frame(
  punto1 = pares[2,],
  punto2 = pares[1,],
  dist.km = X1,
  cluster1 = clusters[pares[2,]],
  cluster2 = clusters[pares[1,]]
)

#Filtrar solo pares dentro del mismo cluster
X_mismo_grupo <- X[X$cluster1 == X$cluster2, ]

#Visualización rápida
plot(vector_dist, col=tabla_todo_sin_buff$cluster, main="Clusters de parches por distancia ≤ 100 km")


