# ------------------------------------------------------------------------------
# Paquetes para localización óptima de brigadas
# ------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(units)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)
library(plotly)
library(scales)

# ------------------------------------------------------------------------------
# Parámetros generales
# ------------------------------------------------------------------------------

# Sistema de coordenadas métrico para México
crs_metrico <- 6372

# Tamaño aproximado de las celdas hexagonales
tamano_celda_m <- 10000   # 10 km

# Número de brigadas que se desea ubicar
numero_brigadas <- 6

# Tiempo máximo para considerar una zona cubierta
tiempo_maximo_min <- 60

# Aproximación inicial del desplazamiento
velocidad_promedio_kmh <- 45

# Corrige la diferencia entre distancia recta y recorrido real
factor_sinuosidad <- 1.35

# Pesos del índice de riesgo
peso_frecuencia  <- 0.50
peso_superficie  <- 0.35
peso_persistencia <- 0.15


# ------------------------------------------------------------------------------
# Preparar datos
# ------------------------------------------------------------------------------

fires_modelo <- fires_all %>%
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

if (nrow(fires_modelo) == 0) {
  stop("No hay incendios válidos para construir el modelo.")
}

numero_anios <- n_distinct(fires_modelo$year)

cat("Incendios utilizados:", nrow(fires_modelo), "\n")
cat("Número de años:", numero_anios, "\n")
cat(
  "Superficie total:",
  round(sum(fires_modelo$Superficie), 1),
  "ha\n"
)

# ------------------------------------------------------------------------------
# Transformar polígonos y puntos a coordenadas métricas
# ------------------------------------------------------------------------------

chiapas_m <- chiapas %>%
  st_make_valid() %>%
  st_transform(crs_metrico)

municipios_m <- municipios_chiapas %>%
  st_make_valid() %>%
  st_transform(crs_metrico)

incendios_sf <- fires_modelo %>%
  st_as_sf(
    coords = c("Lon", "Lat"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(crs_metrico)

# Conservar únicamente incendios dentro del estado
chiapas_union <- st_union(chiapas_m)

incendios_sf <- incendios_sf[
  lengths(
    st_intersects(
      incendios_sf,
      chiapas_union
    )
  ) > 0,
]


# ------------------------------------------------------------------------------
# Crear cuadrícula hexagonal
# ------------------------------------------------------------------------------

rejilla <- st_make_grid(
  chiapas_union,
  cellsize = tamano_celda_m,
  square = FALSE
) %>%
  st_as_sf() %>%
  mutate(
    celda_id = row_number()
  )

# Recortar la cuadrícula al límite de Chiapas
rejilla <- suppressWarnings(
  st_intersection(
    rejilla,
    chiapas_union
  )
)

# Eliminar geometrías vacías
rejilla <- rejilla[
  !st_is_empty(rejilla),
]

# Restaurar identificadores en caso de que la intersección los modifique
rejilla <- rejilla %>%
  mutate(
    celda_id = row_number()
  )

plot(
  st_geometry(rejilla),
  border = "gray70",
  main = "Cuadrícula de análisis"
)

plot(
  st_geometry(chiapas_m),
  add = TRUE,
  border = "black",
  lwd = 2
)


# ------------------------------------------------------------------------------
# Unión espacial entre incendios y cuadrícula
# ------------------------------------------------------------------------------

incendios_por_celda <- st_join(
  incendios_sf,
  rejilla["celda_id"],
  join = st_intersects,
  left = FALSE
) %>%
  arrange(incendio_id) %>%
  distinct(
    incendio_id,
    .keep_all = TRUE
  )

if (nrow(incendios_por_celda) == 0) {
  stop("No fue posible asignar los incendios a la cuadrícula.")
}

# ------------------------------------------------------------------------------
# Resumen de riesgo por celda
# ------------------------------------------------------------------------------

resumen_celda <- incendios_por_celda %>%
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
    
    anios_con_incendio = n_distinct(year),
    
    persistencia = anios_con_incendio / numero_anios,
    
    .groups = "drop"
  )

rejilla_riesgo <- rejilla %>%
  left_join(
    resumen_celda,
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
# Función de normalización entre 0 y 1
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
# Índice de riesgo
# ------------------------------------------------------------------------------

rejilla_riesgo <- rejilla_riesgo %>%
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


# ------------------------------------------------------------------------------
# Celdas que representan demanda de atención
# ------------------------------------------------------------------------------

demanda <- rejilla_riesgo %>%
  filter(
    riesgo > 0
  ) %>%
  arrange(
    celda_id
  ) %>%
  mutate(
    demanda_id = row_number()
  )

if (nrow(demanda) == 0) {
  stop("No existen celdas con riesgo mayor que cero.")
}

# Punto representativo dentro de cada celda
demanda_puntos <- demanda %>%
  st_point_on_surface()

cat(
  "Celdas con riesgo:",
  nrow(demanda),
  "\n"
)

# ------------------------------------------------------------------------------
# Sitios candidatos: municipios de Chiapas
# ------------------------------------------------------------------------------

candidatos <- municipios_m %>%
  mutate(
    candidato_id = row_number(),
    
    municipio_candidato = case_when(
      "NAME_2" %in% names(.) ~ as.character(NAME_2),
      TRUE ~ paste(
        "Municipio",
        row_number()
      )
    )
  ) %>%
  st_point_on_surface()

if (numero_brigadas > nrow(candidatos)) {
  stop(
    "El número de brigadas es mayor que el número de sitios candidatos."
  )
}

cat(
  "Sitios candidatos:",
  nrow(candidatos),
  "\n"
)

# ------------------------------------------------------------------------------
# Distancia entre candidatos y celdas de demanda
# ------------------------------------------------------------------------------

distancia_m <- st_distance(
  candidatos,
  demanda_puntos
)

distancia_km <- units::drop_units(
  distancia_m
) / 1000

# ------------------------------------------------------------------------------
# Tiempo aproximado de viaje
# ------------------------------------------------------------------------------

tiempo_min <- (
  distancia_km *
    factor_sinuosidad /
    velocidad_promedio_kmh
) * 60

dim(tiempo_min)


# ------------------------------------------------------------------------------
# Cobertura dentro del tiempo máximo
# ------------------------------------------------------------------------------

matriz_cobertura <- ifelse(
  tiempo_min <= tiempo_maximo_min,
  1L,
  0L
)

numero_candidatos <- nrow(candidatos)
numero_demandas <- nrow(demanda)

riesgo_demanda <- demanda$riesgo


# ------------------------------------------------------------------------------
# Modelo de máxima cobertura ponderada
# ------------------------------------------------------------------------------

modelo_cobertura <- MIPModel() %>%
  
  # x[j] = 1 cuando se instala una brigada en el sitio j
  add_variable(
    x[j],
    j = 1:numero_candidatos,
    type = "binary"
  ) %>%
  
  # y[i] = 1 cuando la celda i queda cubierta
  add_variable(
    y[i],
    i = 1:numero_demandas,
    type = "binary"
  ) %>%
  
  # Maximizar el riesgo cubierto
  set_objective(
    sum_expr(
      riesgo_demanda[i] * y[i],
      i = 1:numero_demandas
    ),
    sense = "max"
  ) %>%
  
  # Una celda solo puede considerarse cubierta
  # cuando al menos una brigada puede llegar a tiempo
  add_constraint(
    y[i] <= sum_expr(
      matriz_cobertura[j, i] * x[j],
      j = 1:numero_candidatos
    ),
    i = 1:numero_demandas
  ) %>%
  
  # Número exacto de brigadas disponibles
  add_constraint(
    sum_expr(
      x[j],
      j = 1:numero_candidatos
    ) == numero_brigadas
  )

# ------------------------------------------------------------------------------
# Ejecutar optimización
# ------------------------------------------------------------------------------

resultado_modelo <- solve_model(
  modelo_cobertura,
  with_ROI(
    solver = "glpk",
    verbose = TRUE
  )
)


# ------------------------------------------------------------------------------
# Obtener sitios elegidos
# ------------------------------------------------------------------------------

solucion_brigadas <- get_solution(
  resultado_modelo,
  x[j]
) %>%
  filter(
    value > 0.5
  ) %>%
  arrange(j)

brigadas_optimas <- candidatos[
  solucion_brigadas$j,
]

brigadas_optimas <- brigadas_optimas %>%
  mutate(
    numero_brigada = row_number()
  )

brigadas_optimas %>%
  st_drop_geometry() %>%
  select(
    numero_brigada,
    municipio_candidato
  )


# ------------------------------------------------------------------------------
# Tiempos desde las brigadas seleccionadas
# ------------------------------------------------------------------------------

tiempos_seleccionados <- tiempo_min[
  solucion_brigadas$j,
  ,
  drop = FALSE
]

# Posición de la brigada más rápida para cada celda
brigada_local_mas_cercana <- apply(
  tiempos_seleccionados,
  2,
  which.min
)

# Índice original del candidato
brigada_global_mas_cercana <- solucion_brigadas$j[
  brigada_local_mas_cercana
]

# Tiempo mínimo de respuesta
tiempo_respuesta_min <- apply(
  tiempos_seleccionados,
  2,
  min
)

demanda_asignada <- demanda %>%
  mutate(
    candidato_id = candidatos$candidato_id[
      brigada_global_mas_cercana
    ],
    
    brigada_asignada = candidatos$municipio_candidato[
      brigada_global_mas_cercana
    ],
    
    tiempo_respuesta_min = tiempo_respuesta_min,
    
    cubierta_60_min = tiempo_respuesta_min <=
      tiempo_maximo_min
  )


# ------------------------------------------------------------------------------
# Indicadores de cobertura
# ------------------------------------------------------------------------------

riesgo_total <- sum(
  demanda_asignada$riesgo,
  na.rm = TRUE
)

riesgo_cubierto <- sum(
  demanda_asignada$riesgo[
    demanda_asignada$cubierta_60_min
  ],
  na.rm = TRUE
)

porcentaje_riesgo_cubierto <- (
  riesgo_cubierto /
    riesgo_total
) * 100

porcentaje_celdas_cubiertas <- mean(
  demanda_asignada$cubierta_60_min
) * 100

tiempo_promedio_simple <- mean(
  demanda_asignada$tiempo_respuesta_min
)

tiempo_promedio_ponderado <- weighted.mean(
  demanda_asignada$tiempo_respuesta_min,
  demanda_asignada$riesgo
)

tiempo_maximo_observado <- max(
  demanda_asignada$tiempo_respuesta_min
)

metricas_modelo <- tibble(
  `Número de brigadas` = numero_brigadas,
  
  `Tiempo objetivo (min)` = tiempo_maximo_min,
  
  `Riesgo cubierto (%)` = round(
    porcentaje_riesgo_cubierto,
    2
  ),
  
  `Celdas cubiertas (%)` = round(
    porcentaje_celdas_cubiertas,
    2
  ),
  
  `Tiempo promedio (min)` = round(
    tiempo_promedio_simple,
    2
  ),
  
  `Tiempo promedio ponderado (min)` = round(
    tiempo_promedio_ponderado,
    2
  ),
  
  `Tiempo máximo estimado (min)` = round(
    tiempo_maximo_observado,
    2
  )
)

metricas_modelo

# ------------------------------------------------------------------------------
# Coordenadas geográficas de los sitios seleccionados
# ------------------------------------------------------------------------------

brigadas_wgs84 <- brigadas_optimas %>%
  st_transform(4326)

coordenadas_brigadas <- st_coordinates(
  brigadas_wgs84
)

tabla_brigadas <- brigadas_wgs84 %>%
  st_drop_geometry() %>%
  mutate(
    Longitud = coordenadas_brigadas[, 1],
    Latitud = coordenadas_brigadas[, 2]
  ) %>%
  select(
    numero_brigada,
    municipio_candidato,
    Latitud,
    Longitud
  )

tabla_brigadas

# ------------------------------------------------------------------------------
# Preparar textos para mapa interactivo
# ------------------------------------------------------------------------------

demanda_mapa <- demanda_asignada %>%
  mutate(
    tooltip_riesgo = paste0(
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

brigadas_mapa <- brigadas_optimas %>%
  mutate(
    tooltip_brigada = paste0(
      "<b>Brigada ",
      numero_brigada,
      "</b><br>",
      "<b>Ubicación:</b> ",
      municipio_candidato
    )
  )

# ------------------------------------------------------------------------------
# Revisar tipos de geometría
# ------------------------------------------------------------------------------

cat(
  "rejilla_riesgo:",
  paste(
    unique(st_geometry_type(rejilla_riesgo)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "municipios_m:",
  paste(
    unique(st_geometry_type(municipios_m)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "chiapas_m:",
  paste(
    unique(st_geometry_type(chiapas_m)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "brigadas_mapa:",
  paste(
    unique(st_geometry_type(brigadas_mapa)),
    collapse = ", "
  ),
  "\n"
)

# ------------------------------------------------------------------------------
# Función para limpiar y uniformar polígonos
# ------------------------------------------------------------------------------

limpiar_poligonos <- function(x) {
  
  x <- st_make_valid(x)
  
  # Extraer únicamente componentes poligonales
  x <- suppressWarnings(
    st_collection_extract(
      x,
      type = "POLYGON"
    )
  )
  
  # Uniformar todos los objetos como MULTIPOLYGON
  x <- suppressWarnings(
    st_cast(
      x,
      "MULTIPOLYGON"
    )
  )
  
  # Eliminar geometrías vacías
  x <- x[
    !st_is_empty(x),
  ]
  
  return(x)
}


# ------------------------------------------------------------------------------
# Crear capas limpias exclusivamente para la gráfica
# ------------------------------------------------------------------------------

rejilla_plot <- limpiar_poligonos(
  rejilla_riesgo
)

municipios_plot <- limpiar_poligonos(
  municipios_m
)

chiapas_plot <- limpiar_poligonos(
  chiapas_m
)


unique(st_geometry_type(rejilla_plot))
unique(st_geometry_type(municipios_plot))
unique(st_geometry_type(chiapas_plot))


# ------------------------------------------------------------------------------
# Preparar puntos de brigadas para Plotly
# ------------------------------------------------------------------------------

brigadas_plot <- brigadas_mapa %>%
  st_cast(
    "POINT",
    warn = FALSE
  )

coordenadas_brigadas_plot <- st_coordinates(
  brigadas_plot
)

brigadas_xy <- brigadas_plot %>%
  st_drop_geometry() %>%
  mutate(
    X = coordenadas_brigadas_plot[, 1],
    Y = coordenadas_brigadas_plot[, 2]
  )


# ------------------------------------------------------------------------------
# Crear mapa compatible con ggplotly
# ------------------------------------------------------------------------------

mapa_brigadas <- ggplot() +
  
  # Cuadrícula de riesgo
  geom_sf(
    data = rejilla_plot,
    aes(
      fill = riesgo,
      text = paste0(
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
        "%"
      )
    ),
    color = NA
  ) +
  
  scale_fill_gradient(
    low = "white",
    high = "red",
    limits = c(0, 1),
    name = "Índice de\nriesgo"
  ) +
  
  # Límites municipales
  geom_sf(
    data = municipios_plot,
    fill = NA,
    color = "gray55",
    linewidth = 0.15
  ) +
  
  # Límite estatal
  geom_sf(
    data = chiapas_plot,
    fill = NA,
    color = "black",
    linewidth = 0.80
  ) +
  
  # Ubicación de brigadas
  geom_point(
    data = brigadas_xy,
    aes(
      x = X,
      y = Y,
      text = tooltip_brigada
    ),
    shape = 23,
    size = 5,
    fill = "deepskyblue",
    color = "black",
    stroke = 1
  ) +
  
  # Número de brigada
  geom_text(
    data = brigadas_xy,
    aes(
      x = X,
      y = Y,
      label = numero_brigada
    ),
    size = 3,
    fontface = "bold"
  ) +
  
  coord_sf(
    crs = st_crs(rejilla_plot),
    expand = FALSE
  ) +
  
  labs(
    title = "Ubicación estratégica de brigadas contra incendios",
    
    subtitle = paste0(
      numero_brigadas,
      " brigadas | Tiempo objetivo: ",
      tiempo_maximo_min,
      " minutos | Riesgo cubierto: ",
      round(
        porcentaje_riesgo_cubierto,
        1
      ),
      "%"
    ),
    
    caption = paste(
      "Los sitios se obtuvieron mediante un modelo",
      "de máxima cobertura ponderada por riesgo."
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

mapa_brigadas


# ------------------------------------------------------------------------------
# Convertir a Plotly
# ------------------------------------------------------------------------------

mapa_brigadas_interactivo <- ggplotly(
  mapa_brigadas,
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
      r = 150,
      t = 100,
      b = 50
    ),
    
    dragmode = "zoom"
  )

mapa_brigadas_interactivo


# ------------------------------------------------------------------------------
# Guardar tabla de ubicaciones
# ------------------------------------------------------------------------------

write.csv(
  tabla_brigadas,
  "ubicaciones_brigadas_optimas.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# Guardar capa espacial
# ------------------------------------------------------------------------------

st_write(
  brigadas_wgs84,
  "brigadas_optimas_chiapas.gpkg",
  delete_dsn = TRUE
)

# ------------------------------------------------------------------------------
# Guardar asignación de celdas
# ------------------------------------------------------------------------------

st_write(
  demanda_asignada %>%
    st_transform(4326),
  "celdas_asignadas_brigadas.gpkg",
  delete_dsn = TRUE
)

