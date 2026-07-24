# ------------------------------------------------------------------------------
# Paquetes
# ------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(units)

library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)

library(ggplot2)
library(plotly)
library(scales)

# ------------------------------------------------------------------------------
# Parámetros
# ------------------------------------------------------------------------------

# Sistema de coordenadas métrico
crs_metrico <- 6372

# Tamaño de la cuadrícula hexagonal
tamano_celda_m <- 8000

# Número de brigadas que se desea instalar
numero_brigadas <- 6

# Tiempo máximo para cubrir las celdas con riesgo
tiempo_maximo_riesgo_min <- 60

# Tiempo máximo deseable para cubrir las comunidades prioritarias
tiempo_maximo_comunidad_min <- 45

# Parámetros preliminares de desplazamiento
velocidad_promedio_kmh <- 45
factor_sinuosidad <- 1.35

# Pesos del índice histórico de riesgo
peso_frecuencia <- 0.50
peso_superficie <- 0.35
peso_persistencia <- 0.15

# Importancia de las comunidades dentro de la función objetivo
# 0.40 significa que el 40 % del objetivo corresponde
# a la cobertura de las comunidades prioritarias.
peso_objetivo_comunidades <- 0.40


# ------------------------------------------------------------------------------
# Comunidades prioritarias de la costa de Chiapas
# ------------------------------------------------------------------------------

comunidades_prioritarias <- tribble(
  ~Estado,   ~Municipio,            ~Comunidad,            ~Latitud,  ~Longitud,
  "Chiapas", "Acacoyagua",          "Acacoyagua",           15.342372, -92.671686,
  "Chiapas", "Arriaga",             "Arriaga",              16.233297, -93.902150,
  "Chiapas", "Mapastepec",          "Ejido Mapastepec",     15.432031, -92.907292,
  "Chiapas", "Frontera Comalapa",   "Frontera Comalapa",    15.656714, -92.141786,
  "Chiapas", "Suchiate",            "Ciudad Hidalgo",       14.687417, -92.163569,
  "Chiapas", "Huehuetán",           "Huehuetán",            15.012397, -92.404836,
  "Chiapas", "Huixtla",             "Huixtla",              15.135539, -92.466089,
  "Chiapas", "Mapastepec",          "Mapastepec",            15.443872, -92.891944,
  "Chiapas", "Mazatán",             "Mazatán",              14.860278, -92.449536,
  "Chiapas", "Pijijiapan",          "Pijijiapan",           15.688208, -93.207583,
  "Chiapas", "Tapachula",           "Tapachula",            14.913869, -92.255003,
  "Chiapas", "Tonalá",              "Tonalá",               16.087639, -93.758944,
  "Chiapas", "Villa Comaltitlán",   "Villacomaltitlán",     15.212108, -92.576731
) %>%
  mutate(
    comunidad_id = row_number()
  )

comunidades_prioritarias


# ------------------------------------------------------------------------------
# Comunidades como puntos espaciales
# ------------------------------------------------------------------------------

comunidades_sf <- comunidades_prioritarias %>%
  st_as_sf(
    coords = c("Longitud", "Latitud"),
    crs = 4326,
    remove = FALSE
  )

comunidades_m <- comunidades_sf %>%
  st_transform(crs_metrico)

# ------------------------------------------------------------------------------
# Función de normalización
# ------------------------------------------------------------------------------

normalizar_nombre <- function(x) {
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x <- toupper(x)
  
  x <- gsub(
    "[^A-Z0-9 ]",
    "",
    x
  )
  
  x <- gsub(
    "\\s+",
    " ",
    x
  )
  
  trimws(x)
}

comunidades_prioritarias <- comunidades_prioritarias %>%
  mutate(
    municipio_key = normalizar_nombre(Municipio)
  )

municipios_costa_nombre <- comunidades_prioritarias %>%
  distinct(municipio_key) %>%
  pull(municipio_key)

municipios_chiapas_modelo <- municipios_chiapas %>%
  mutate(
    municipio_key = normalizar_nombre(NAME_2)
  )

municipios_no_encontrados <- setdiff(
  municipios_costa_nombre,
  municipios_chiapas_modelo$municipio_key
)

municipios_no_encontrados

# ------------------------------------------------------------------------------
# Seleccionar municipios costeros prioritarios
# ------------------------------------------------------------------------------

zona_costa <- municipios_chiapas_modelo %>%
  filter(
    municipio_key %in% municipios_costa_nombre
  ) %>%
  st_make_valid()

if (nrow(zona_costa) == 0) {
  stop(
    "No fue posible identificar los municipios de la zona costera."
  )
}

zona_costa_m <- zona_costa %>%
  st_transform(crs_metrico)

zona_costa_union <- st_union(
  zona_costa_m
)

plot(
  st_geometry(zona_costa_m),
  col = "gray95",
  border = "gray50",
  main = "Zona prioritaria de la costa de Chiapas"
)

plot(
  st_geometry(comunidades_m),
  add = TRUE,
  pch = 19,
  col = "red"
)

# ------------------------------------------------------------------------------
# Preparar incendios
# ------------------------------------------------------------------------------

fires_costa <- fires_all %>%
  mutate(
    incendio_id = row_number(),
    Lat = as.numeric(Lat),
    Lon = as.numeric(Lon),
    Superficie = as.numeric(Superficie),
    year = as.character(year)
  ) %>%
  filter(
    !is.na(Lat),
    !is.na(Lon),
    !is.na(Superficie),
    Superficie > 0
  )

incendios_costa_sf <- fires_costa %>%
  st_as_sf(
    coords = c("Lon", "Lat"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(crs_metrico)

# Mantener únicamente incendios dentro de la zona seleccionada
incendios_costa_sf <- st_filter(
  incendios_costa_sf,
  zona_costa_m,
  .predicate = st_intersects
)

if (nrow(incendios_costa_sf) == 0) {
  stop(
    "No se encontraron incendios dentro de la zona costera."
  )
}

cat(
  "Incendios costeros utilizados:",
  nrow(incendios_costa_sf),
  "\n"
)

cat(
  "Superficie total:",
  round(
    sum(
      incendios_costa_sf$Superficie,
      na.rm = TRUE
    ),
    1
  ),
  "ha\n"
)

# ------------------------------------------------------------------------------
# Cuadrícula hexagonal
# ------------------------------------------------------------------------------

rejilla_costa <- st_make_grid(
  zona_costa_union,
  cellsize = tamano_celda_m,
  square = FALSE
) %>%
  st_as_sf()

rejilla_costa <- suppressWarnings(
  st_intersection(
    rejilla_costa,
    zona_costa_union
  )
)

rejilla_costa <- st_make_valid(
  rejilla_costa
)

rejilla_costa <- suppressWarnings(
  st_collection_extract(
    rejilla_costa,
    type = "POLYGON"
  )
)

rejilla_costa <- suppressWarnings(
  st_cast(
    rejilla_costa,
    "MULTIPOLYGON"
  )
)

rejilla_costa <- rejilla_costa[
  !st_is_empty(rejilla_costa),
]

rejilla_costa <- rejilla_costa %>%
  mutate(
    celda_id = row_number()
  )

# ------------------------------------------------------------------------------
# Unión espacial
# ------------------------------------------------------------------------------

incendios_por_celda <- st_join(
  incendios_costa_sf,
  rejilla_costa["celda_id"],
  join = st_intersects,
  left = FALSE
) %>%
  arrange(incendio_id) %>%
  distinct(
    incendio_id,
    .keep_all = TRUE
  )

if (nrow(incendios_por_celda) == 0) {
  stop(
    "No fue posible asignar los incendios a la cuadrícula."
  )
}

# ------------------------------------------------------------------------------
# Número de años analizados
# ------------------------------------------------------------------------------

numero_anios <- n_distinct(
  incendios_costa_sf$year
)

# ------------------------------------------------------------------------------
# Resumen por celda
# ------------------------------------------------------------------------------

resumen_celda_costa <- incendios_por_celda %>%
  st_drop_geometry() %>%
  group_by(celda_id) %>%
  summarise(
    frecuencia = n(),
    
    superficie_total = sum(
      Superficie,
      na.rm = TRUE
    ),
    
    superficie_promedio = mean(
      Superficie,
      na.rm = TRUE
    ),
    
    superficie_maxima = max(
      Superficie,
      na.rm = TRUE
    ),
    
    anios_con_incendio = n_distinct(
      year
    ),
    
    persistencia = anios_con_incendio /
      numero_anios,
    
    .groups = "drop"
  )

rejilla_riesgo_costa <- rejilla_costa %>%
  left_join(
    resumen_celda_costa,
    by = "celda_id"
  ) %>%
  mutate(
    frecuencia = replace_na(
      frecuencia,
      0
    ),
    
    superficie_total = replace_na(
      superficie_total,
      0
    ),
    
    superficie_promedio = replace_na(
      superficie_promedio,
      0
    ),
    
    superficie_maxima = replace_na(
      superficie_maxima,
      0
    ),
    
    anios_con_incendio = replace_na(
      anios_con_incendio,
      0
    ),
    
    persistencia = replace_na(
      persistencia,
      0
    )
  )

# ------------------------------------------------------------------------------
# Función de normalización
# ------------------------------------------------------------------------------

normalizar <- function(x) {
  
  limites <- range(
    x,
    na.rm = TRUE
  )
  
  if (
    !all(is.finite(limites)) ||
    diff(limites) == 0
  ) {
    return(
      rep(
        0,
        length(x)
      )
    )
  }
  
  (x - limites[1]) /
    (limites[2] - limites[1])
}

# ------------------------------------------------------------------------------
# Índice histórico de riesgo
# ------------------------------------------------------------------------------

rejilla_riesgo_costa <- rejilla_riesgo_costa %>%
  mutate(
    frecuencia_transformada = log1p(
      frecuencia
    ),
    
    superficie_transformada = log1p(
      superficie_total
    ),
    
    frecuencia_norm = normalizar(
      frecuencia_transformada
    ),
    
    superficie_norm = normalizar(
      superficie_transformada
    ),
    
    persistencia_norm = normalizar(
      persistencia
    ),
    
    riesgo = (
      peso_frecuencia *
        frecuencia_norm
    ) + (
      peso_superficie *
        superficie_norm
    ) + (
      peso_persistencia *
        persistencia_norm
    )
  )

demanda_costa <- rejilla_riesgo_costa %>%
  filter(
    riesgo > 0
  ) %>%
  arrange(celda_id) %>%
  mutate(
    demanda_id = row_number()
  )

if (nrow(demanda_costa) == 0) {
  stop(
    "No existen celdas costeras con riesgo mayor que cero."
  )
}

demanda_puntos_costa <- demanda_costa %>%
  st_point_on_surface()

# ------------------------------------------------------------------------------
# Candidatos para instalar brigadas
# ------------------------------------------------------------------------------

candidatos_costa <- comunidades_m %>%
  mutate(
    candidato_id = row_number()
  )

numero_candidatos <- nrow(
  candidatos_costa
)

numero_demandas <- nrow(
  demanda_costa
)

numero_comunidades <- nrow(
  comunidades_m
)

if (numero_brigadas > numero_candidatos) {
  stop(
    "El número de brigadas supera el número de comunidades candidatas."
  )
}

# ------------------------------------------------------------------------------
# Distancias candidato-celda
# ------------------------------------------------------------------------------

distancia_riesgo_m <- st_distance(
  candidatos_costa,
  demanda_puntos_costa
)

distancia_riesgo_km <- units::drop_units(
  distancia_riesgo_m
) / 1000

tiempo_riesgo_min <- (
  distancia_riesgo_km *
    factor_sinuosidad /
    velocidad_promedio_kmh
) * 60

# ------------------------------------------------------------------------------
# Distancias entre candidatos y comunidades
# ------------------------------------------------------------------------------

distancia_comunidades_m <- st_distance(
  candidatos_costa,
  comunidades_m
)

distancia_comunidades_km <- units::drop_units(
  distancia_comunidades_m
) / 1000

tiempo_comunidades_min <- (
  distancia_comunidades_km *
    factor_sinuosidad /
    velocidad_promedio_kmh
) * 60

# ------------------------------------------------------------------------------
# Cobertura de las zonas con riesgo
# ------------------------------------------------------------------------------

cobertura_riesgo <- ifelse(
  tiempo_riesgo_min <=
    tiempo_maximo_riesgo_min,
  1L,
  0L
)

# ------------------------------------------------------------------------------
# Cobertura de las comunidades prioritarias
# ------------------------------------------------------------------------------

cobertura_comunidades <- ifelse(
  tiempo_comunidades_min <=
    tiempo_maximo_comunidad_min,
  1L,
  0L
)

# ------------------------------------------------------------------------------
# Coeficientes normalizados
# ------------------------------------------------------------------------------

riesgo_demanda <- demanda_costa$riesgo

if (sum(riesgo_demanda) <= 0) {
  stop(
    "La suma del riesgo es igual a cero."
  )
}

peso_objetivo_riesgo <- 1 -
  peso_objetivo_comunidades

coeficientes_riesgo <- (
  peso_objetivo_riesgo *
    riesgo_demanda /
    sum(riesgo_demanda)
)

coeficientes_comunidades <- rep(
  peso_objetivo_comunidades /
    numero_comunidades,
  numero_comunidades
)

# ------------------------------------------------------------------------------
# Modelo de máxima cobertura costera
# ------------------------------------------------------------------------------

modelo_costa <- MIPModel() %>%
  
  # x[j] indica si se coloca una brigada en la comunidad j
  add_variable(
    x[j],
    j = 1:numero_candidatos,
    type = "binary"
  ) %>%
  
  # y[i] indica si se cubre la celda de riesgo i
  add_variable(
    y[i],
    i = 1:numero_demandas,
    type = "binary"
  ) %>%
  
  # u[c] indica si se cubre la comunidad prioritaria c
  add_variable(
    u[c],
    c = 1:numero_comunidades,
    type = "binary"
  ) %>%
  
  # Maximizar conjuntamente riesgo y comunidades
  set_objective(
    
    sum_expr(
      coeficientes_riesgo[i] * y[i],
      i = 1:numero_demandas
    ) +
      
      sum_expr(
        coeficientes_comunidades[c] * u[c],
        c = 1:numero_comunidades
      ),
    
    sense = "max"
  ) %>%
  
  # Cobertura de celdas con incendios
  add_constraint(
    y[i] <= sum_expr(
      cobertura_riesgo[j, i] * x[j],
      j = 1:numero_candidatos
    ),
    i = 1:numero_demandas
  ) %>%
  
  # Cobertura de comunidades prioritarias
  add_constraint(
    u[c] <= sum_expr(
      cobertura_comunidades[j, c] * x[j],
      j = 1:numero_candidatos
    ),
    c = 1:numero_comunidades
  ) %>%
  
  # Número exacto de brigadas
  add_constraint(
    sum_expr(
      x[j],
      j = 1:numero_candidatos
    ) == numero_brigadas
  )

# ------------------------------------------------------------------------------
# Ejecutar optimización
# ------------------------------------------------------------------------------

resultado_costa <- solve_model(
  modelo_costa,
  with_ROI(
    solver = "glpk",
    verbose = TRUE
  )
)

# ------------------------------------------------------------------------------
# Ubicaciones seleccionadas
# ------------------------------------------------------------------------------

seleccion_costa <- get_solution(
  resultado_costa,
  x[j]
) %>%
  filter(
    value > 0.5
  ) %>%
  arrange(j)

brigadas_costa <- candidatos_costa[
  seleccion_costa$j,
]

brigadas_costa <- brigadas_costa %>%
  mutate(
    numero_brigada = row_number()
  )

brigadas_costa %>%
  st_drop_geometry() %>%
  select(
    numero_brigada,
    Municipio,
    Comunidad,
    Latitud,
    Longitud
  )

# ------------------------------------------------------------------------------
# Tiempos desde las brigadas elegidas hasta las comunidades
# ------------------------------------------------------------------------------

tiempos_comunidades_seleccionadas <- tiempo_comunidades_min[
  seleccion_costa$j,
  ,
  drop = FALSE
]

brigada_cercana_local <- apply(
  tiempos_comunidades_seleccionadas,
  2,
  which.min
)

brigada_cercana_global <- seleccion_costa$j[
  brigada_cercana_local
]

tiempo_minimo_comunidad <- apply(
  tiempos_comunidades_seleccionadas,
  2,
  min
)

resultado_comunidades <- comunidades_m %>%
  mutate(
    brigada_asignada = candidatos_costa$Comunidad[
      brigada_cercana_global
    ],
    
    tiempo_estimado_min = tiempo_minimo_comunidad,
    
    comunidad_cubierta =
      tiempo_estimado_min <=
      tiempo_maximo_comunidad_min
  )

tabla_cobertura_comunidades <- resultado_comunidades %>%
  st_drop_geometry() %>%
  select(
    Municipio,
    Comunidad,
    brigada_asignada,
    tiempo_estimado_min,
    comunidad_cubierta
  ) %>%
  mutate(
    tiempo_estimado_min = round(
      tiempo_estimado_min,
      1
    )
  )

tabla_cobertura_comunidades

# ------------------------------------------------------------------------------
# Asignar celdas a las brigadas elegidas
# ------------------------------------------------------------------------------

tiempos_riesgo_seleccionados <- tiempo_riesgo_min[
  seleccion_costa$j,
  ,
  drop = FALSE
]

brigada_celda_local <- apply(
  tiempos_riesgo_seleccionados,
  2,
  which.min
)

brigada_celda_global <- seleccion_costa$j[
  brigada_celda_local
]

tiempo_minimo_celda <- apply(
  tiempos_riesgo_seleccionados,
  2,
  min
)

demanda_costa_asignada <- demanda_costa %>%
  mutate(
    brigada_asignada = candidatos_costa$Comunidad[
      brigada_celda_global
    ],
    
    tiempo_respuesta_min = tiempo_minimo_celda,
    
    celda_cubierta =
      tiempo_respuesta_min <=
      tiempo_maximo_riesgo_min
  )

# ------------------------------------------------------------------------------
# Cobertura del riesgo
# ------------------------------------------------------------------------------

riesgo_total <- sum(
  demanda_costa_asignada$riesgo,
  na.rm = TRUE
)

riesgo_cubierto <- sum(
  demanda_costa_asignada$riesgo[
    demanda_costa_asignada$celda_cubierta
  ],
  na.rm = TRUE
)

porcentaje_riesgo_cubierto <- (
  riesgo_cubierto /
    riesgo_total
) * 100

# ------------------------------------------------------------------------------
# Cobertura de comunidades
# ------------------------------------------------------------------------------

porcentaje_comunidades_cubiertas <- mean(
  resultado_comunidades$comunidad_cubierta
) * 100

# ------------------------------------------------------------------------------
# Tiempos
# ------------------------------------------------------------------------------

tiempo_promedio_riesgo <- weighted.mean(
  demanda_costa_asignada$tiempo_respuesta_min,
  demanda_costa_asignada$riesgo
)

tiempo_promedio_comunidades <- mean(
  resultado_comunidades$tiempo_estimado_min
)

metricas_costa <- tibble(
  `Número de brigadas` = numero_brigadas,
  
  `Comunidades candidatas` = numero_comunidades,
  
  `Comunidades cubiertas (%)` = round(
    porcentaje_comunidades_cubiertas,
    2
  ),
  
  `Riesgo cubierto (%)` = round(
    porcentaje_riesgo_cubierto,
    2
  ),
  
  `Tiempo promedio a comunidades (min)` = round(
    tiempo_promedio_comunidades,
    2
  ),
  
  `Tiempo ponderado a zonas de riesgo (min)` = round(
    tiempo_promedio_riesgo,
    2
  )
)

metricas_costa

# ------------------------------------------------------------------------------
# Información para tooltips
# ------------------------------------------------------------------------------

demanda_costa_asignada <- demanda_costa_asignada %>%
  mutate(
    tooltip_celda = paste0(
      "<b>Índice de riesgo:</b> ",
      round(riesgo, 3),
      "<br>",
      "<b>Incendios:</b> ",
      frecuencia,
      "<br>",
      "<b>Superficie acumulada:</b> ",
      round(superficie_total, 1),
      " ha<br>",
      "<b>Persistencia:</b> ",
      round(persistencia * 100, 1),
      "%<br>",
      "<b>Brigada asignada:</b> ",
      brigada_asignada,
      "<br>",
      "<b>Tiempo estimado:</b> ",
      round(tiempo_respuesta_min, 1),
      " min"
    )
  )

resultado_comunidades <- resultado_comunidades %>%
  mutate(
    seleccionada = comunidad_id %in%
      brigadas_costa$comunidad_id,
    
    tooltip_comunidad = paste0(
      "<b>Comunidad:</b> ",
      Comunidad,
      "<br>",
      "<b>Municipio:</b> ",
      Municipio,
      "<br>",
      "<b>Brigada asignada:</b> ",
      brigada_asignada,
      "<br>",
      "<b>Tiempo estimado:</b> ",
      round(tiempo_estimado_min, 1),
      " min<br>",
      "<b>Cubierta:</b> ",
      ifelse(
        comunidad_cubierta,
        "Sí",
        "No"
      )
    )
  )

rejilla_costa_plot <- demanda_costa_asignada %>%
  st_make_valid() %>%
  st_collection_extract(
    "POLYGON"
  ) %>%
  st_cast(
    "MULTIPOLYGON",
    warn = FALSE
  )

zona_costa_plot <- zona_costa_m %>%
  st_make_valid() %>%
  st_collection_extract(
    "POLYGON"
  ) %>%
  st_cast(
    "MULTIPOLYGON",
    warn = FALSE
  )

coordenadas_comunidades <- st_coordinates(
  resultado_comunidades
)

comunidades_xy <- resultado_comunidades %>%
  st_drop_geometry() %>%
  mutate(
    X = coordenadas_comunidades[, 1],
    Y = coordenadas_comunidades[, 2]
  )

# ------------------------------------------------------------------------------
# Crear área de contexto alrededor de la zona costera
# ------------------------------------------------------------------------------

# Distancia adicional alrededor del área de estudio
# 50000 metros = 50 km
distancia_contexto_m <- 80000

# Buffer alrededor de los municipios prioritarios
area_contexto <- st_buffer(
  zona_costa_union,
  dist = distancia_contexto_m
)

# Recortar el buffer al territorio de Chiapas
area_contexto <- suppressWarnings(
  st_intersection(
    area_contexto,
    st_union(chiapas_m)
  )
)

# Municipios que intersectan el área de contexto
municipios_contexto <- st_filter(
  municipios_m,
  area_contexto,
  .predicate = st_intersects
)

# Limpiar geometrías para evitar problemas con ggplotly
municipios_contexto_plot <- municipios_contexto %>%
  st_make_valid() %>%
  st_collection_extract(
    "POLYGON"
  ) %>%
  st_cast(
    "MULTIPOLYGON",
    warn = FALSE
  )

chiapas_contexto_plot <- chiapas_m %>%
  st_make_valid() %>%
  st_collection_extract(
    "POLYGON"
  ) %>%
  st_cast(
    "MULTIPOLYGON",
    warn = FALSE
  )

# Límites que tendrá el mapa
bbox_contexto <- st_bbox(
  area_contexto
)

# ------------------------------------------------------------------------------
# Mapa de localización de brigadas
# ------------------------------------------------------------------------------

mapa_brigadas_costa <- ggplot() +
  
  geom_sf(
    data = rejilla_costa_plot,
    aes(
      fill = riesgo,
      text = tooltip_celda
    ),
    color = NA
  ) +
  
  scale_fill_gradient(
    low = "white",
    high = "red",
    limits = c(0, 1),
    name = "Índice de\nriesgo"
  ) +
  
  geom_sf(
    data = zona_costa_plot,
    fill = NA,
    color = "gray45",
    linewidth = 0.35
  ) +
  
  # Comunidades que no fueron seleccionadas
  geom_point(
    data = comunidades_xy %>%
      filter(!seleccionada),
    aes(
      x = X,
      y = Y,
      text = tooltip_comunidad
    ),
    shape = 21,
    size = 3,
    fill = "white",
    color = "black",
    stroke = 0.8
  ) +
  
  # Comunidades seleccionadas para brigadas
  geom_point(
    data = comunidades_xy %>%
      filter(seleccionada),
    aes(
      x = X,
      y = Y,
      text = tooltip_comunidad
    ),
    shape = 24,
    size = 5,
    fill = "deepskyblue",
    color = "black",
    stroke = 1
  ) +
  
  geom_text(
    data = comunidades_xy %>%
      filter(seleccionada),
    aes(
      x = X,
      y = Y,
      label = Comunidad
    ),
    nudge_y = 5000,
    size = 3,
    fontface = "bold"
  ) +
  
  coord_sf(
    crs = st_crs(zona_costa_plot),
    expand = FALSE
  ) +
  
  labs(
    title = "Ubicación estratégica de brigadas en la costa de Chiapas",
    
    subtitle = paste0(
      numero_brigadas,
      " brigadas | Comunidades cubiertas: ",
      round(
        porcentaje_comunidades_cubiertas,
        1
      ),
      "% | Riesgo cubierto: ",
      round(
        porcentaje_riesgo_cubierto,
        1
      ),
      "%"
    ),
    
    caption = paste(
      "Triángulos azules: comunidades seleccionadas.",
      "Círculos blancos: comunidades prioritarias no seleccionadas."
    )
  ) +
  
  theme_minimal() +
  
  theme(
    axis.title = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    legend.position = "right",
    
    panel.grid.minor = element_blank()
  )

mapa_brigadas_costa

mapa_brigadas_costa_interactivo <- ggplotly(
  mapa_brigadas_costa,
  tooltip = "text"
) %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(
        color = "black",
        size = 11
      )
    ),
    
    margin = list(
      l = 40,
      r = 160,
      t = 105,
      b = 55
    ),
    
    dragmode = "zoom"
  )

mapa_brigadas_costa_interactivo

