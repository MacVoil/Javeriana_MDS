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
    df |>  # Se utiliza el operador pipe para pasar el dataframe a las funciones siguientes                        
        summarize(min = min({{x}}),  # Se utiliza la función summarize para calcular estadísticos de la variable {{x}}
                  q1 = quantile({{x}}, 0.25),
                  median = median({{x}}),
                  mean = mean({{x}}),
                  q3 = quantile({{x}}, 0.75),
                  max = max({{x}}),
                  skewness = skewness({{x}})) %>% 
        mutate(across(where(is.numeric),~ round(.,2))) %>% # Se utiliza la función mutate para redondear los valores numéricos a dos decimales
        kable()  # Se utiliza la función kable del paquete "knitr" para imprimir la tabla en formato "markdown"
}


# Función para filtrar un dataframe quitando los outliers de una columa espeficicado untilziando la regla del 1.5 del rango intercuartil
remove_outliers <- function(dataframe, column) {
    column_name <- as_name(enquo(column)) # Convierte la columna de entrada en un nombre simbólico y luego en una cadena de caracteres
    
    # Calcula los cuartiles y el rango intercuartílico de la columna especificada
    q <- quantile(dataframe[[column_name]], probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- diff(q)
    
    # Define los límites superior e inferior utilizando la regla del 1.5 y el rango intercuartílico
    upper <- q[2] + 1.5 * iqr
    lower <- q[1] - 1.5 * iqr
    
    # Filtra los datos para excluir los valores atípicos en la columna especificada
    filtered_data <- dataframe %>% filter({{column}} >= lower, {{column}} <= upper)
    
    # Devuelve el objeto de datos filtrado
    return(filtered_data)
}

reg_analysis <- function(df){
    p1 <- ggplot(df, aes(.fitted, .resid)) +
        geom_point(alpha = 0.2) +
        geom_smooth(se = FALSE) +
        theme_tq() +
        ggtitle("Residuals vs Fitted")
    
    p2 <-  ggplot(df,  aes(sample = .std.resid)) +
        stat_qq_band(alpha = 0.2) +
        stat_qq_line(linewidth = 0.1) +
        stat_qq_point(alpha = 0.2, size = 0) +
        theme_tq() +
        ggtitle("Normal Q-Q") 
    
    p3 <-  ggplot(df, aes(.fitted, sqrt(abs(.std.resid)))) +
        geom_point(alpha = 0.2) +
        geom_smooth(se = FALSE) +
        theme_tq() +
        ggtitle("Scale-Location")
    
    p4 <-   ggplot(df, aes(.hat, .std.resid)) +
        geom_point(alpha = 0.2) +
        geom_smooth(se = FALSE) +
        theme_tq() +
        ggtitle("Residuals vs Leverage")
    
    grid.arrange(p1, p2, p3, p4, 
                 nrow = 2,
                 top = textGrob("Regression Analysis"))
    
}