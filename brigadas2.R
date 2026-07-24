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
