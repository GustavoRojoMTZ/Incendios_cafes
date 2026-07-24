# ==============================================================================
# MAPA DE BRIGADAS COSTERAS CON TODOS LOS MUNICIPIOS VISIBLES
# ==============================================================================

library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)
library(scales)

# ------------------------------------------------------------------------------
# 1. Configuración del área visible
# ------------------------------------------------------------------------------

# Distancia adicional alrededor de la zona de estudio
# 50 000 metros = 50 kilómetros
distancia_contexto_m <- 40000

# Asegurar que todos los municipios estén en el mismo CRS métrico
municipios_chiapas_m <- municipios_chiapas %>%
  st_make_valid() %>%
  st_transform(crs_metrico)

# Obtener la extensión rectangular de la zona costera prioritaria
bbox_contexto <- st_bbox(zona_costa_union)

# Ampliar la extensión en todas las direcciones
bbox_contexto["xmin"] <- bbox_contexto["xmin"] - distancia_contexto_m
bbox_contexto["xmax"] <- bbox_contexto["xmax"] + distancia_contexto_m
bbox_contexto["ymin"] <- bbox_contexto["ymin"] - distancia_contexto_m
bbox_contexto["ymax"] <- bbox_contexto["ymax"] + distancia_contexto_m

# Convertir la extensión rectangular en geometría
poligono_contexto <- st_as_sfc(bbox_contexto)

st_crs(poligono_contexto) <- st_crs(municipios_chiapas_m)


# ------------------------------------------------------------------------------
# 2. Seleccionar todos los municipios que aparecen dentro del mapa
# ------------------------------------------------------------------------------

municipios_visibles <- st_filter(
  municipios_chiapas_m,
  poligono_contexto,
  .predicate = st_intersects
)

# Recortar las geometrías al área visible
municipios_visibles <- suppressWarnings(
  st_crop(
    municipios_visibles,
    bbox_contexto
  )
)


# ------------------------------------------------------------------------------
# 3. Función para limpiar geometrías poligonales
# ------------------------------------------------------------------------------

limpiar_poligonos <- function(x) {
  
  x <- st_make_valid(x)
  
  x <- suppressWarnings(
    st_collection_extract(
      x,
      type = "POLYGON"
    )
  )
  
  x <- suppressWarnings(
    st_cast(
      x,
      "MULTIPOLYGON",
      warn = FALSE
    )
  )
  
  x <- x[
    !st_is_empty(x),
  ]
  
  x
}


# ------------------------------------------------------------------------------
# 4. Limpiar los municipios visibles
# ------------------------------------------------------------------------------

municipios_visibles_plot <- limpiar_poligonos(
  municipios_visibles
)

municipios_visibles_plot <- municipios_visibles_plot %>%
  mutate(
    tooltip_municipio = paste0(
      "<b>Municipio:</b> ",
      NAME_2
    )
  )

cat(
  "Número de municipios visibles:",
  nrow(municipios_visibles_plot),
  "\n"
)

print(
  municipios_visibles_plot %>%
    st_drop_geometry() %>%
    select(
      NAME_1,
      NAME_2
    )
)


# ------------------------------------------------------------------------------
# 5. Preparar etiquetas de los municipios visibles
# ------------------------------------------------------------------------------

etiquetas_municipios_sf <- municipios_visibles_plot %>%
  st_point_on_surface()

coordenadas_etiquetas <- st_coordinates(
  etiquetas_municipios_sf
)

etiquetas_municipios_xy <- etiquetas_municipios_sf %>%
  st_drop_geometry() %>%
  mutate(
    X = coordenadas_etiquetas[, 1],
    Y = coordenadas_etiquetas[, 2]
  )


# ------------------------------------------------------------------------------
# 6. Preparar las celdas de riesgo
# ------------------------------------------------------------------------------

demanda_costa_asignada <- demanda_costa_asignada %>%
  mutate(
    tooltip_celda = paste0(
      "<b>Índice de riesgo:</b> ",
      round(riesgo, 3),
      "<br>",
      
      "<b>Incendios registrados:</b> ",
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
      " minutos<br>",
      
      "<b>Celda cubierta:</b> ",
      ifelse(
        celda_cubierta,
        "Sí",
        "No"
      )
    )
  )

rejilla_costa_plot <- limpiar_poligonos(
  demanda_costa_asignada
)


# ------------------------------------------------------------------------------
# 7. Preparar los municipios costeros prioritarios
# ------------------------------------------------------------------------------

zona_costa_plot <- limpiar_poligonos(
  zona_costa_m
)


# ------------------------------------------------------------------------------
# 8. Preparar las comunidades prioritarias
# ------------------------------------------------------------------------------

resultado_comunidades <- resultado_comunidades %>%
  mutate(
    # Identificar cuáles comunidades fueron seleccionadas
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
      " minutos<br>",
      
      "<b>Cubierta:</b> ",
      ifelse(
        comunidad_cubierta,
        "Sí",
        "No"
      ),
      "<br>",
      
      "<b>Seleccionada como base:</b> ",
      ifelse(
        seleccionada,
        "Sí",
        "No"
      )
    )
  )

# Obtener coordenadas X y Y
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
# 9. Crear mapa estático
# ------------------------------------------------------------------------------

mapa_brigadas_costa <- ggplot() +
  
  # --------------------------------------------------------------------------
# Todos los municipios ubicados dentro de la extensión visible
# --------------------------------------------------------------------------
geom_sf(
  data = municipios_visibles_plot,
  aes(
    text = tooltip_municipio
  ),
  fill = "gray97",
  color = "gray65",
  linewidth = 0.28
) +
  
  # --------------------------------------------------------------------------
# Celdas hexagonales con riesgo histórico
# --------------------------------------------------------------------------
geom_sf(
  data = rejilla_costa_plot,
  aes(
    fill = riesgo,
    text = tooltip_celda
  ),
  color = NA,
  alpha = 0.88
) +
  
  # Escala blanco-rojo del índice de riesgo
  scale_fill_gradient(
    low = "white",
    high = "red",
    limits = c(0, 1),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0.00",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Índice de\nriesgo"
  ) +
  
  # --------------------------------------------------------------------------
# Delimitación de los municipios prioritarios
# --------------------------------------------------------------------------
geom_sf(
  data = zona_costa_plot,
  fill = NA,
  color = "black",
  linewidth = 0.65
) +
  
  # --------------------------------------------------------------------------
# Nombres de todos los municipios visibles
# --------------------------------------------------------------------------
geom_text(
  data = etiquetas_municipios_xy,
  aes(
    x = X,
    y = Y,
    label = NAME_2
  ),
  size = 2.1,
  color = "gray35",
  check_overlap = TRUE
) +
  
  # --------------------------------------------------------------------------
# Comunidades prioritarias que no fueron seleccionadas
# --------------------------------------------------------------------------
geom_point(
  data = comunidades_xy %>%
    filter(!seleccionada),
  aes(
    x = X,
    y = Y,
    text = tooltip_comunidad
  ),
  shape = 21,
  size = 3.5,
  fill = "white",
  color = "black",
  stroke = 0.9
) +
  
  # --------------------------------------------------------------------------
# Comunidades seleccionadas para instalar brigadas
# --------------------------------------------------------------------------
geom_point(
  data = comunidades_xy %>%
    filter(seleccionada),
  aes(
    x = X,
    y = Y,
    text = tooltip_comunidad
  ),
  shape = 24,
  size = 5.5,
  fill = "deepskyblue",
  color = "black",
  stroke = 1.1
) +
  
  # --------------------------------------------------------------------------
# Etiquetas de las comunidades seleccionadas
# --------------------------------------------------------------------------
geom_text(
  data = comunidades_xy %>%
    filter(seleccionada),
  aes(
    x = X,
    y = Y,
    label = Comunidad
  ),
  nudge_y = 4500,
  size = 3,
  color = "black",
  fontface = "bold",
  check_overlap = TRUE
) +
  
  # --------------------------------------------------------------------------
# Extensión geográfica del mapa
# --------------------------------------------------------------------------
coord_sf(
  crs = st_crs(municipios_visibles_plot),
  
  xlim = c(
    bbox_contexto["xmin"],
    bbox_contexto["xmax"]
  ),
  
  ylim = c(
    bbox_contexto["ymin"],
    bbox_contexto["ymax"]
  ),
  
  expand = FALSE
) +
  
  # --------------------------------------------------------------------------
# Títulos
# --------------------------------------------------------------------------
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
    "Triángulos azules: comunidades seleccionadas para instalar brigadas.",
    "Círculos blancos: comunidades prioritarias no seleccionadas.",
    "Las líneas grises muestran todos los municipios visibles."
  )
) +
  
  # --------------------------------------------------------------------------
# Tema
# --------------------------------------------------------------------------
theme_minimal() +
  
  theme(
    axis.title = element_blank(),
    
    axis.text = element_text(
      size = 8
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 10
    ),
    
    plot.caption = element_text(
      size = 8,
      hjust = 0
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    panel.grid.major = element_line(
      color = "gray88",
      linewidth = 0.20
    ),
    
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(
      t = 15,
      r = 25,
      b = 15,
      l = 15
    )
  )


# Mostrar versión estática
mapa_brigadas_costa


# ------------------------------------------------------------------------------
# 10. Convertir el mapa a Plotly
# ------------------------------------------------------------------------------

mapa_brigadas_costa_interactivo <- ggplotly(
  mapa_brigadas_costa,
  tooltip = "text",
  dynamicTicks = FALSE
) %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "gray50",
      font = list(
        color = "black",
        size = 11
      )
    ),
    
    margin = list(
      l = 55,
      r = 180,
      t = 110,
      b = 70
    ),
    
    dragmode = "zoom"
  )


# Mostrar mapa interactivo
mapa_brigadas_costa_interactivo


# ------------------------------------------------------------------------------
# 11. Guardar como archivo HTML
# ------------------------------------------------------------------------------

# Descomenta este bloque para guardar el mapa:

# htmlwidgets::saveWidget(
#   widget = mapa_brigadas_costa_interactivo,
#   file = "mapa_brigadas_costa_municipios_visibles.html",
#   selfcontained = TRUE
#)
