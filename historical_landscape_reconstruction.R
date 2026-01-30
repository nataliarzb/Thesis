# ====================================================================================
# PROJECT: Undergraduate Thesis - Historical Landscape Reconstruction
# SCRIPT: 02_Historical_Spatial_Reconstruction.R
# AUTHOR: Natalia Ramírez Bedoya
#
# OBJECTIVE:
# Processing of historical spatial configurations for the identification of changes in the structure of patches over time.

# METHODOLOGY:
# - Pre-processing of historical layers with Terra.

# # - Past patch correspondence by probabilistic overlap
# - Calculation of historical average area and connectivity
# =================================================================================

# RUTAS Y CONFIGURACIONES

ruta_cfg <- "resultados_configuraciones/paramo_1km_parches/"

shp_cfg <- list(
  "CFG_01" = paste0(ruta_cfg, "Paramo1900_3100_estricto_30m_parches_1km.shp"),
  "CFG_02" = paste0(ruta_cfg, "Paramo2000_3200_estricto_30m_parches_1km.shp"),
  "CFG_03" = paste0(ruta_cfg, "Paramo2100_3300_estricto_30m_parches_1km.shp"),
  "CFG_04" = paste0(ruta_cfg, "Paramo2200_3400_estricto_30m_parches_1km.shp"),
  "CFG_05" = paste0(ruta_cfg, "Paramo2300_3500_estricto_30m_parches_1km.shp"),
  "CFG_06" = paste0(ruta_cfg, "Paramo2400_3600_estricto_30m_parches_1km.shp"),
  "CFG_07" = paste0(ruta_cfg, "Paramo2500_3700_estricto_30m_parches_1km.shp"),
  "CFG_08" = paste0(ruta_cfg, "Paramo2600_3800_estricto_30m_parches_1km.shp"),
  "CFG_09" = paste0(ruta_cfg, "Paramo2700_3900_estricto_30m_parches_1km.shp"),
  "CFG_10" = paste0(ruta_cfg, "Paramo2800_4000_estricto_30m_parches_1km.shp"),
  "CFG_11" = paste0(ruta_cfg, "Paramo2900_4100_estricto_30m_parches_1km.shp"),
  "CFG_12" = paste0(ruta_cfg, "Paramo3000_4200_estricto_30m_parches_1km.shp"),
  "CFG_13" = paste0(ruta_cfg, "Paramo3100_4300_estricto_30m_parches_1km.shp"),
  "CFG_14" = paste0(ruta_cfg, "Paramo3200_4400_estricto_30m_parches_1km.shp"),
  "CFG_15" = paste0(ruta_cfg, "Paramo3300_4500_estricto_30m_parches_1km.shp"),
  "CFG_16" = paste0(ruta_cfg, "Paramo3400_4600_estricto_30m_parches_1km.shp"),
  "CFG_17" = paste0(ruta_cfg, "Paramo3500_4700_estricto_30m_parches_1km.shp"),
  "CFG_18" = paste0(ruta_cfg, "Paramo3600_4800_estricto_30m_parches_1km.shp"),
  "CFG_19" = paste0(ruta_cfg, "Paramo3700_4900_estricto_30m_parches_1km.shp")
)

cfg_list <- lapply(shp_cfg, terra::vect)

# OBJETOS BASE

ID_COL <- "id_patch"
parches_actual <- vect("parches/area_estudio_buf.shp")[-1, ]
UFL_rastreo_full <- read.xlsx("UFL_rastreo_full.xlsx")

# OBJETO DE RESULTADOS

trayectorias <- tibble(
  Linaje_ID = character(),
  Edad = integer(),
  Parche_Correspondiente = character(),
  Metodo = character(),       # probabilistico | detenido
  Mecanismo = character(),    # solape | distancia | sin_vecinos | inicial
  N_candidatos = integer(),
  Area_elegida = numeric(),
  Prop_solape_elegido = numeric(),
  Area_dom = numeric(),
  Prop_solape_dom = numeric(),
  Ratio_dom_elegida = numeric()
)

# ============================
# BUCLE PRINCIPAL
# ============================

for (i in seq_len(nrow(parches_actual))) {
  
  linaje_id <- values(parches_actual)[i, ID_COL]
  geom_actual <- parches_actual[i]
  activo <- TRUE
  
  # Estado inicial
  trayectorias <- add_row(
    trayectorias,
    Linaje_ID = linaje_id,
    Edad = 0,
    Parche_Correspondiente = linaje_id,
    Metodo = "inicial",
    Mecanismo = "inicial",
    N_candidatos = NA,
    Area_elegida = NA,
    Prop_solape_elegido = NA,
    Area_dom = NA,
    Prop_solape_dom = NA,
    Ratio_dom_elegida = NA
  )
  
  # BUCLE TEMPORAL

  for (e in seq_len(nrow(UFL_rastreo_full))) {
    
    if (!activo) break
    
    cfg_name <- UFL_rastreo_full$cfg[e]
    edad_actual <- e
    cfg_sv <- cfg_list[[cfg_name]]
    
    # BUFFER FIJO
    
    buffer_geom <- terra::buffer(geom_actual, width = 10000)
    rel <- terra::relate(cfg_sv, buffer_geom, "intersects")
    candidatos <- cfg_sv[rel, ]
    
    if (nrow(candidatos) == 0) {
      
      trayectorias <- add_row(
        trayectorias,
        Linaje_ID = linaje_id,
        Edad = edad_actual,
        Parche_Correspondiente = NA,
        Metodo = "detenido",
        Mecanismo = "sin_vecinos",
        N_candidatos = 0,
        Area_elegida = NA,
        Prop_solape_elegido = NA,
        Area_dom = NA,
        Prop_solape_dom = NA,
        Ratio_dom_elegida = NA
      )
      
      activo <- FALSE
      next
    }
    
    # CÁLCULO DE SOLAPE

    inter <- suppressWarnings(terra::intersect(geom_actual, candidatos))
    
    if (!is.null(inter) && nrow(inter) > 0) {
      
      inter$area_frag <- expanse(inter, unit = "km")
      
      solape <- inter |>
        as.data.frame() |>
        group_by(id_patch_2) |>
        summarise(area_solape = sum(area_frag), .groups = "drop")
      
      solape_total <- candidatos |>
      as.data.frame() |>
      select(id_patch) |>
      left_join(solape, by = c("id_patch" = "id_patch_2")) |>
      mutate(area_solape = replace_na(area_solape, 0))
    
    mecanismo <- "solape"
    probs <- solape_total$area_solape / sum(solape_total$area_solape)
    area_dom <- max(solape_total$area_solape)
    
    # PROBABILIDADES Y MUESTREO
    
    } else {
    
        # intersección entre candidatos y buffer
        inter_buf <- suppressWarnings(
          terra::intersect(candidatos, buffer_geom)
        )
        
        inter_buf_df <- as.data.frame(inter_buf)
        
        inter_buf$area_buffer <- expanse(inter_buf, unit = "km")
        
        inter_buf_df <- inter_buf_df |>
          mutate(area_buffer = expanse(inter_buf, unit = "km"))
        
        area_buf <- inter_buf_df |>
          as.data.frame() |>
          group_by(id_patch_1) |>
          summarise(area_buffer = sum(area_buffer), .groups = "drop") |>
          rename(id_patch = id_patch_1)
        
        solape_total <- candidatos |>
          as.data.frame() |>
          select(id_patch) |>
          left_join(area_buf, by = "id_patch") |>
          mutate(area_buffer = replace_na(area_buffer, 0))
        
        mecanismo <- "buffer"
        
        probs <- solape_total$area_buffer / sum(solape_total$area_buffer)
        area_dom <- max(solape_total$area_buffer)

    } 
    
    parche_sel <- sample(solape_total$id_patch, 1, prob = probs)
    
    
    area_parche_actual <- expanse(geom_actual, unit = "km")
    
    area_elegida <- if (mecanismo == "solape") {
      solape_total$area_solape[solape_total$id_patch == parche_sel]
    } else {
      solape_total$area_buffer[solape_total$id_patch == parche_sel]
    }
    
    prop_solape <- area_elegida / area_parche_actual
    
    ratio_dom_elegida <- area_elegida / area_dom
    
    prop_solape_dom <- area_dom / area_parche_actual 
    
    trayectorias <- add_row(
      trayectorias,
      Linaje_ID = linaje_id,
      Edad = edad_actual,
      Parche_Correspondiente = parche_sel,
      Metodo = "probabilistico",
      Mecanismo = mecanismo,
      N_candidatos = nrow(solape_total),
      Area_elegida = area_elegida,
      Prop_solape_elegido = prop_solape,
      Area_dom = area_dom,
      Prop_solape_dom = prop_solape_dom,
      Ratio_dom_elegida = ratio_dom_elegida
    )
    
    geom_actual <- cfg_sv[cfg_sv[[ID_COL]] == parche_sel, ]
  }
}

View(trayectorias)
write.xlsx(trayectorias, "trayectorias_final.xlsx")




#-----------ANIMACIÓN-------------------------


## SELECCIONAR LINAJE A ANIMAR
linaje_anim <- "P2"   # cambiar para ver historia por parche

## TRAYECTORIA DEL LINAJE

tray_linaje <- trayectorias %>%
  filter(
    Linaje_ID == linaje_anim,
    Metodo != "detenido"
  ) %>%
  select(
    Episodio = Edad,
    Parche_Correspondiente
  )

## MOSTRAR TODOS LOS EPISODIOS 

episodios_totales <- seq_len(nrow(UFL_rastreo_full))

tray_completa <- tray_linaje %>%
  complete(Episodio = episodios_totales) %>%
  arrange(Episodio) %>%
  fill(Parche_Correspondiente, .direction = "down")


## CONSTRUIR GEOMETRÍAS POR EPISODIO

sf_list <- list()

for (i in seq_len(nrow(tray_completa))) {
  
  ep_i     <- tray_completa$Episodio[i]
  parche_i <- tray_completa$Parche_Correspondiente[i]
  
  if (is.na(parche_i)) next
  
  ## Episodio 0: parche actual
  
  if (ep_i == 0) {
    
    geom_i <- parches_actual[
      parches_actual$id_patch == parche_i
    ]
    
  } else {
    
    cfg_name <- UFL_rastreo_full$cfg[ep_i]
    
    if (is.na(cfg_name) || !cfg_name %in% names(cfg_list)) next
    
    cfg_sv <- cfg_list[[cfg_name]]
    
    geom_i <- cfg_sv[
      cfg_sv$id_patch == parche_i
    ]
  }
  
  if (is.null(geom_i) || nrow(geom_i) == 0) next
  
  sf_i <- st_as_sf(geom_i)
  sf_i$Episodio <- ep_i
  
  sf_list[[length(sf_list) + 1]] <- sf_i
}

sf_anim <- do.call(rbind, sf_list)

sf_anim <- sf_anim %>%
  mutate(label_ep = paste("Episodio:", Episodio))


## EXTENSIÓN ESPACIAL FIJA

bbox <- st_bbox(sf_anim)

bbox_zoom <- st_bbox(
  c(
    xmin = bbox["xmin"] - 20000,
    xmax = bbox["xmax"] + 20000,
    ymin = bbox["ymin"] - 20000,
    ymax = bbox["ymax"] + 20000
  ),
  crs = st_crs(sf_anim)
)

## ANIMACIÓN (EPISODIO POR EPISODIO)

p <- ggplot(sf_anim) +
  geom_sf(
    fill  = "steelblue",
    color = "black",
    alpha = 0.85
  ) +
  coord_sf(
    xlim = c(bbox_zoom["xmin"], bbox_zoom["xmax"]),
    ylim = c(bbox_zoom["ymin"], bbox_zoom["ymax"])
  ) +
  transition_manual(Episodio) +
  labs(
    title = paste("Trayectoria espacial del linaje", linaje_anim),
    subtitle = "Episodio: {current_frame}",
    caption = "Seguimiento espacio-temporal"
  ) +
  theme_minimal()

## EJECUTAR Y GUARDAR GIF

n_ep <- length(unique(sf_anim$Episodio))

animate(
  p,
  nframes = n_ep,
  fps = 1,
  width = 800,
  height = 600,
  renderer = gifski_renderer(
    paste0("trayectoria_", linaje_anim, ".gif")
  )
)




# ------ UNIR INFO DE AREAS Y CONECTIVIDAD  --------------------

areas_configuraciones <- read.xlsx("resultados_configuraciones/Configuraciones_Area_PC_FINAL.xlsx")

historia_parches <- trayectorias %>%
 left_join(
    areas_configuraciones,
    by = c("Parche_Correspondiente" = "id_patch")
  )


#agregar a la tabla la duracion de cada episodio

#primero se debe empezar a numerar los episodios en UFL_rastreo desde 1 y no desde 0
UFL_rastreo_full <- UFL_rastreo_full %>%
  mutate(episodio_corr = episodio + 1)

#excluir el presente
historia_integrada <- historia_parches %>%
  filter(Edad > 0)

historia_integrada <- historia_integrada %>%
  left_join(
    UFL_rastreo_full %>%
      select(episodio_corr, Duracion_Episodio),
    by = c("Edad" = "episodio_corr")
 )


##eliminar las ultimas filas (no se solapan con ninguna configuracion porque va un poco más allá de ecuador)
#ids_eliminar <- paste0("P", 249:269)
#historia_integrada <- historia_integrada %>%
#  filter(!Linaje_ID %in% ids_eliminar)

View(historia_integrada)
write.xlsx(historia_integrada, "historia_integrada_prob2.xlsx")




# ----- AREA PONDERADA EN EL TIEMPO -------

#Calcular area parche en cada edad x duracion 
historia_integrada_pond_prob <- historia_integrada %>%
  mutate(area_x_t = area_topo_km2 * Duracion_Episodio)


#area en el tiempo de cada parche
area_tiempo_parche_pond_prob <- historia_integrada_pond_prob %>%
  group_by(Linaje_ID) %>%
  summarise(
    area_tiempo_total = sum(area_x_t, na.rm = TRUE),
    tiempo_total = sum(Duracion_Episodio, na.rm = TRUE),
    area_media_tiempo = area_tiempo_total / tiempo_total
  )

View(area_tiempo_parche_pond_prob)

write.xlsx(area_tiempo_parche_pond_prob, "area_tiempo_parche_pond_prob.xlsx")
area_tiempo_parche_pond_prob <- read.xlsx("area_tiempo_parche_pond_prob.xlsx")



area_hist <- historia_integrada %>%
  group_by(Linaje_ID) %>%
  summarise(
    area_media_tiempo = sum(area_topo_km2 * Duracion_Episodio, na.rm = TRUE) /
      sum(Duracion_Episodio, na.rm = TRUE)
  )

area_hist_media

#---- CONECTIVIDAD EN EL TIEMPO------

#PC MEDIO HISTORICO POR LINAJE

pc_hist <- historia_integrada %>%
  group_by(Linaje_ID) %>%
  summarise(
    PC_media = sum(dPC * Duracion_Episodio, na.rm = TRUE) /
      sum(Duracion_Episodio, na.rm = TRUE))

pc_hist$PC_media

historia_pc_var <- historia_integrada %>%
  left_join(pc_hist, by = "Linaje_ID")

PC_var <- historia_pc_var %>%
  group_by(Linaje_ID) %>%
  summarise(
    PC_media = first(PC_media),
    dPC_sd = sqrt(
      sum(
        Duracion_Episodio * (dPC - PC_media)^2,
        na.rm = TRUE
      ) / sum(Duracion_Episodio, na.rm = TRUE)
    ),
    dPC_CV = ifelse(PC_media > 0, dPC_sd / PC_media, NA),
    .groups = "drop"
  )
