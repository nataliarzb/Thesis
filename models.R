library(openxlsx)
library(glmmTMB)
library(AICcmodavg)
library(psych)
library(performance)
library(dplyr)

tabla_todo_transf_sc2 <- read.xlsx("tabla_todo_transf_sc2_1.xlsx")
#Eliminar parche P63 porque tiene valores en 0 de la historia y esto genera conflicto en los modelos
borrar <- paste0("P", 63)
tabla_todo_transf_sc2 <- tabla_todo_transf_sc2[!tabla_todo_transf_sc2$id_patch %in% borrar, ]


# CORRELACIÓN ENTRE VARIABLES

vars_cor <- tabla_todo_transf_sc2[, c(
  "logareakm2_sc",
  "logdPC_NA_sc",
  "logarea_media_historia",
  "logPC_media_historia",
  "logPC_CV_historia"
)]

cor_test <- psych::corr.test(
  vars_cor,
  use = "pairwise",
  method = "pearson"
)

# Extraer solo las correlaciones únicas
cor_resultados <- as.data.frame(as.table(cor_test$r)) %>%
  rename(
    variable_1 = Var1,
    variable_2 = Var2,
    r = Freq
  ) %>%
  left_join(
    as.data.frame(as.table(cor_test$p)) %>%
      rename(
        variable_1 = Var1,
        variable_2 = Var2,
        p = Freq
      ),
    by = c("variable_1", "variable_2")
  ) %>%
  filter(
    variable_1 != variable_2,
    as.numeric(factor(variable_1)) < as.numeric(factor(variable_2))
  ) %>%
  mutate(
    abs_r = abs(r)
  ) %>%
  arrange(desc(abs_r))

cor_resultados

# Correlaciones definitivamente muy fuertes:
# Area histórica y PC histórico

# Correlaciones que requieren evaluación de colinealidad VIF:
# PC hist y PC CV hist
# Area hist y PC CV hist 
# PC actual y area hist
# PC actual y PC hist 
# PC actual y PC CV hist

# MODELOS 

# MODELOS ACTUALIDAD 
area_actual <- glmmTMB(riqueza ~ logareakm2_sc + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
PC_actual <- glmmTMB(riqueza ~ logdPC_NA_sc + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
hab_actual <-  glmmTMB(riqueza ~ logarea_hab20km_sc + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
area_PC_actual <- glmmTMB(riqueza ~ logareakm2_sc + logdPC_NA_sc + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
area_hab_actual <- glmmTMB(riqueza ~ logareakm2_sc + logarea_hab20km_sc + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())

modelos_actual <- list(area_actual, PC_actual, hab_actual, 
                       area_PC_actual)
tabla_aic_actual <- aictab(cand.set = modelos_actual, modnames = c("Área actual", "Conectividad actual", "Cantidad de hábitat actual",
                                                                   "Área y conectividad actual"), sort = FALSE)
tabla_aic_actual <- as.data.frame(tabla_aic_actual)

tabla_r2_actual <- bind_rows(lapply(modelos_actual, performance::r2_nakagawa))
tabla_final <- bind_cols(tabla_aic_actual, tabla_r2_actual)

tabla_final <- tabla_final %>%
  arrange(AICc)

# la historia explica también la riqueza? 
#MODELOS ACTUALIDAD + HISTORIA 
area_actual_hist <- glmmTMB(riqueza ~ logareakm2_sc + logarea_media_historia + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
area_actual_PC_hist <- glmmTMB(riqueza ~ logareakm2_sc + logPC_media_historia + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
area_actual_PC_CV_hist <- glmmTMB(riqueza ~ logareakm2_sc * logPC_CV_historia + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
PC_actual_area_hist <- glmmTMB(riqueza ~ logdPC_NA_sc + logarea_media_historia + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
PC_actual_PC_CV_hist <- glmmTMB(riqueza ~ logdPC_NA_sc + logPC_CV_historia + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
PC_actual_hist <- glmmTMB(riqueza ~ logdPC_NA_sc + logPC_CV_historia +(1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2,family = nbinom2())
area_PC_actual_area_hist <- glmmTMB(riqueza ~ logareakm2_sc + logdPC_NA_sc + logarea_media_historia  + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
area_PC_actual_PC_hist <- glmmTMB(riqueza ~ logareakm2_sc + logdPC_NA_sc + logPC_media_historia + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())
area_PC_actual_CV_PC_hist <- glmmTMB(riqueza ~ logareakm2_sc + logdPC_NA_sc + logPC_CV_historia + (1 | cordilleras_real) + (1 | cluster), data = tabla_todo_transf_sc2, family = nbinom2())


# VERIFICAR COLINEALIDAD CON VIF 

modelos_candidatos_vif <- list(
  #actual
  area_PC_actual,
  
  #actual e hist
  area_actual_hist,
  area_actual_PC_hist,
  area_actual_PC_CV_hist,
  PC_actual_area_hist,
  PC_actual_PC_CV_hist,
  PC_actual_hist,
  area_PC_actual_area_hist,
  area_PC_actual_PC_hist,
  area_PC_actual_CV_PC_hist
)

vif_resultados <- lapply(
  modelos_candidatos_vif,
  performance::check_collinearity
)


# AIC DE TODOS LOS MODELOS ELEGIDOS 

modelos_finales <- list(
  # Estado actual
  area_PC_actual,
  hab_actual,
  
  # Actual + histórico
  area_actual_hist,
  PC_actual_area_hist,
  PC_actual_PC_CV_hist,
  PC_actual_hist,
  area_PC_actual_area_hist,
  area_PC_actual_PC_hist,
  area_PC_actual_CV_PC_hist
)



# Cálculo AIC y R2
tabla_aic_completo <- aictab(cand.set = modelos_finales, modnames = c("Área y dPC actual", "Cantidad de hábitat actual",
                                                                      "Área actual y área hist", "dPC actual y área hist",
                                                                      "dPC actual y CV dPC hist", "dPC actual y dPC hist",
                                                                      "Área actual, dPC actual y área hist", "Área actual, dPC actual y dPC hist", 
                                                                      "Área actual, dPC actual, CV dPC hist"), sort = TRUE)


tabla_aic_completo <- as.data.frame(tabla_aic_completo)

performance::r2_nakagawa(area_PC_actual)
performance::r2_nakagawa(hab_actual)
performance::r2_nakagawa(area_actual_hist)
performance::r2_nakagawa(PC_actual_area_hist)
performance::r2_nakagawa(PC_actual_PC_CV_hist)
performance::r2_nakagawa(PC_actual_hist)
performance::r2_nakagawa(area_PC_actual_area_hist)
performance::r2_nakagawa( area_PC_actual_PC_hist)
performance::r2_nakagawa(area_PC_actual_CV_PC_hist)


# VERIFICAR SUPUESTOS 

# Utilizando el mejor modelo
# Diagnosticos de sobredispersion y residuales con nb
sim <- simulateResiduals(area_PC_actual_area_hist)
plot(sim, quantreg = FALSE)
testDispersion(sim)       # p>0.05 NO hay sobredispersión en DHARMa, se cumple
testUniformity(sim)       # p>0.05 hay uniformidad residuos: se cumple
testZeroInflation(sim)    # p>0.05, se cumple

residuales <- residuals(area_PC_actual_area_hist)
plot(residuales, main = "Residuales del mejor modelo")

# Colinealidad 
check_collinearity(area_PC_actual_area_hist)
#No hay colinealidad en ninguna variable, son menores a 3
#no se roban efecto entre ellas, cada una aporta informacion unica

#R2 de Nakagawa (equivalente Pseudo R2 para modelos mixtos)
#R² marginal (explica efectos fijos) y condicional (efectos fijos + aleatorios)
performance::r2_nakagawa(area_PC_actual_CV_PC_hist)
exp(fixef(modelo_completo)$cond)

# Evaluar singularidad
performance::check_singularity(area_PC_actual_area_hist)
VarCorr(area_PC_actual_area_hist)
# No hay singularidad

# Normalidad del efecto aleatorio
library(lme4) # getME funciona con glmmTMB
ranefs <- unlist(ranef(area_PC_actual_area_hist))
hist(ranefs)
qqnorm(ranefs)
qqline(ranefs)
# Se cumple normalidad

# Autocorrelacion espacial
area_estudio <- vect("area_estudio_real.shp")
#eliminar parche 1 que es las biorregiones alrededor 
area_estudio <- area_estudio[-1,]
centroides <- centroids(area_estudio)        # SpatVector de puntos
coords <- crds(centroides)                   # matriz n x 2: x,y
# test espacial con coordenadas (x = easting, y = northing)
testSpatialAutocorrelation(sim, x = coords[,1], y = coords[,2])
#p valor 0.11 > 0.05, No hay autocorrelación espacial, cumple supuesto 




