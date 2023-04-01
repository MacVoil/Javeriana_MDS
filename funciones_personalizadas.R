# Función para crear un "rain cloud plot"
gg_rain_cloud <- function(df, x){
    ggplot(df , aes( y = {{x}})) + # Se establece el eje y
        stat_halfeye(adjust = 0.5, # Se establece el ajuste y la justificación
                     justification = -0.2,
                     .width = 0,
                     point_colour = NA,
                     fill = "#1F78B4") + # Se establece el color de relleno
        geom_boxplot(
            width = 0.12, # Se establece la anchura del boxplot
            outlier.color = NA, # Se eliminan los outliers
            alpha = 0.5,
            fill = "#1F78B4" # Se establece el color de relleno
        ) +
        stat_dots(
            side = "left", # Se establece el lado de los puntos
            justification = 1.1, # Se establece la justificación
            fill = "#1F78B4" # Se establece el color de relleno
        ) +
        coord_flip() + # Se gira el gráfico
        theme_tq() + # Se establece el tema del gráfico
        scale_fill_tq(theme = "light") + # Se establece la escala de colores
        theme(axis.text.y=element_blank() # Se eliminan las etiquetas del eje y
        ) +
        xlab("") # Se establece la etiqueta del eje x
}

# Función para crear un gráfico de barras
gg_bar <-  function(df, x){
    ggplot(df |>  
               mutate(
                   variable = fct_infreq({{x}})), 
           aes(x = variable, fill = variable)) + # Se establecen los datos del gráfico
        geom_bar() + # Se crea el gráfico de barras
        theme_tq() + # Se establece el tema del gráfico
        scale_fill_tq(theme = "light") + # Se establece la escala de colores
        theme(legend.position = "none") + # Se elimina la leyenda
        coord_flip() + # Se gira el gráfico
        xlab("") # Se establece la etiqueta del eje x
}

# Función para crear tabla de datos de resumen
summary_table <- function(df, x) {
    df %>%                         
        summarize(min = min({{x}}),
                  q1 = quantile({{x}}, 0.25),
                  median = median({{x}}),
                  mean = mean({{x}}),
                  q3 = quantile({{x}}, 0.75),
                  max = max({{x}}),
                  skewness = skewness({{x}})) %>% 
        mutate(across(where(is.numeric),~ round(.,2))) |> 
        kable()
}