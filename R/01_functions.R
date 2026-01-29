
# Asgeurar que todas tengan lo de paquete::funcion() i tanto @param como @return

#' Get the first n unique valid values from each column
#'
#' Iterates through a dataframe to extract unique values, ignoring
#' NAs and empty strings.
#'
#' @param df The dataframe to inspect.
#' @param n The maximum number of unique values to return per column (default 10).
#' @return A list where each element contains unique values for a specific column.
get_unique_valid_values <- function(df, n = 10) {
  purrr::map(df, function(col) {
    # 1. Convert to character to handle mixed types and empty strings
    col_char <- as.character(col)
    
    # 2. Filter: Keep values that are NOT NA and NOT just whitespace/empty
    valid_values <- col[!is.na(col_char) & trimws(col_char) != ""]
    
    # 3. Return as a vector the first n unique values found
    result <- head(unique(valid_values), n)
    return(as.vector(result))
    })
}




#' Clean and Refine Hawks Dataset
#'
#' Selecciona variables biológicas clave para el clustering. Mantiene 'Species'
#' como referencia externa y filtra el resto para conservar solo mediciones
#' numéricas, eliminando factores que podrían distorsionar el cálculo de distancias.
#'
#' @param df El dataframe original de Hawks.
#' @return Un dataframe refinado con Species y variables biométricas numéricas.
#' @export
refine_hawks_data <- function(df) {
  df %>%
    dplyr::select(
      # 1. Target Variable (Ground Truth para evaluación posterior)
      Species, 
      # 2. Biometric Measurements (Continuous Variables)
      Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus
    ) %>%
    dplyr::mutate(
      # Aseguramos que Species sea un factor para la validación externa
      Species = as.factor(Species),
      # Forzamos que el resto sean numéricas para evitar errores en el escalado
      across(where(is.numeric), as.numeric)
    )
}


#' Drop columns with high missingness
#'
#' Automatically removes features that exceed a specific percentage 
#' of missing values (NAs or empty strings).
#'
#' @param df The dataframe to process.
#' @param threshold Percentage limit of missing data (default is 30).
#' @return A dataframe with only the columns that meet the criteria.
drop_high_na_columns <- function(df, threshold = 30) {
  # 1. Identificar qué columnas cumplen la condición
  cols_to_keep <- purrr::map_lgl(df, function(col) {
    # Calculamos el % de NAs o vacíos
    na_pct <- sum(is.na(col) | trimws(as.character(col)) == "") / length(col) * 100
    # Devolvemos TRUE si está por debajo del umbral
    return(na_pct <= threshold)
  })
  
  # 2. Filtrar el dataframe usando el vector lógico
  return(df[, cols_to_keep])
}





#' Auditoría de valores ausentes (NAs)
#' 
#' Esta función identifica qué columnas del dataset contienen valores NA
#' y devuelve un resumen con el conteo total por variable.
#' 
#' @param df El dataframe a inspeccionar.
#' @return Un tibble con las variables y su total de NAs (solo si tienen > 0).
#' @export
audit_nas <- function(df) {
  df %>%
    # Contamos los NAs en cada columna del dataframe
    dplyr::summarise(dplyr::across(everything(), ~ sum(is.na(.)))) %>%
    # Transformamos de formato ancho a largo para facilitar la lectura
    tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Total_NA") %>%
    # Filtramos para mostrar solo las columnas que realmente tienen problemas
    dplyr::filter(Total_NA > 0) %>%
    # Ordenamos de mayor a menor número de ausentes
    dplyr::arrange(desc(Total_NA))
}




#' Imputación por mediana para variables numéricas
#' 
#' Detecta los valores NA en columnas de tipo numérico y los sustituye
#' por la mediana de dicha columna. Se elige la mediana por ser robusta
#' ante la presencia de outliers (como en el caso de la variable Hallux).
#' 
#' @param df El dataframe que contiene los valores ausentes.
#' @return El dataframe con las columnas numéricas completas.
#' @export
impute_median <- function(df) {
  df %>%
    # Aplicamos el cambio solo a las columnas que son numéricas o enteras
    dplyr::mutate(dplyr::across(where(is.numeric), 
                                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
}






#' Plot Biometric Distributions
#'
#' Generates a faceted grid of boxplots for all numeric variables in the dataset.
#' This visualization is optimized for outlier detection using the IQR method.
#'
#' @param df A dataframe containing numeric biometric variables.
#' @param fill_color Hex code or color name for the boxes (default: #69b3a2).
#' @return A ggplot object with faceted boxplots.
#' @export
plot_biometric_outliers <- function(df, fill_color = "#69b3a2") {
  df %>%
    # 1. Seleccionamos solo las variables numéricas para el gráfico
    dplyr::select(where(is.numeric)) %>%
    # 2. Transformamos de formato 'ancho' a 'largo'
    tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    # 3. Construimos el gráfico
    ggplot2::ggplot(ggplot2::aes(x = "", y = Value)) +
    ggplot2::geom_boxplot(
      fill = fill_color, 
      outlier.color = "red", 
      outlier.shape = 16, 
      outlier.size = 1.5,
      width = 0.5
    ) +
    # 4. Creamos la rejilla compacta
    ggplot2::facet_wrap(~Variable, scales = "free", ncol = 4) +
    # 5. Estética profesional y compacta
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = NULL, 
      y = "Measurement Value",
      title = "Biometric Outlier Analysis (IQR Method)",
      subtitle = "Red dots represent points > 1.5x IQR"
    ) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_blank(), # Eliminamos texto innecesario en X
      panel.grid.minor = ggplot2::element_blank()
    )
}



#' Impute Outliers with Median
#'
#' Identifies outliers using the IQR method (1.5x) and replaces them 
#' with the column's median value.
#'
#' @param x A numeric vector.
#' @return A numeric vector with outliers replaced by the median.
impute_outliers_median <- function(x) {
  # 1. Calculamos los cuartiles y el IQR
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  
  # 2. Definimos los límites
  lower_bound <- q1 - 1.5 * iqr_val
  upper_bound <- q3 + 1.5 * iqr_val
  
  # 3. Calculamos la mediana de los valores "sanos"
  med_val <- median(x, na.rm = TRUE)
  
  # 4. Sustituimos los que se salgan de los límites
  x_imputed <- ifelse(x < lower_bound | x > upper_bound, med_val, x)
  
  return(x_imputed)
}


#' Estandarización de variables numéricas (Z-score)
#' 
#' Transforma las variables numéricas para que tengan media 0 y 
#' desviación estándar 1. Es un paso crítico para algoritmos 
#' basados en distancias como K-means o DBSCAN.
#' 
#' @param df El dataframe con los datos ya imputados y sin outliers.
#' @return El dataframe con las columnas numéricas escaladas.
#' @export
standardize_data <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ as.numeric(scale(.))))
}




#' Visualización de Frecuencia de la Variable Objetivo
#' 
#' Genera un gráfico de barras para la columna Species con etiquetas de conteo.
#' Utiliza la sintaxis after_stat para compatibilidad con ggplot2 3.4.0+
#' 
#' @param df Dataframe procesado.
#' @export
plot_species_frequency <- function(df) {

  ggplot(df, aes(x = Species, fill = Species)) +
    # Dibujamos las barras con una transparencia elegante
    geom_bar(alpha = 0.8) +
    
    # Añadimos el conteo exacto usando la sintaxis moderna para evitar warnings
    geom_text(stat = 'count', 
              aes(label = after_stat(count)), 
              vjust = -1, 
              size = 4,
              fontface = "bold") +
    
    # Paleta de colores profesional
    scale_fill_brewer(palette = "Set1") +
    
    # IMPORTANTE: Damos un 15% de margen extra arriba para que no choque con el título
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
    
    # Etiquetas y títulos
    labs(
      title = "Distribución de Especies (Ground Truth)",
      subtitle = "Conteo de individuos por clase para validación externa",
      x = "Especie",
      y = "Número de Halcones"
    ) +
    
    # Estética minimalista
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
      axis.title = element_text(face = "italic"),
      panel.grid.major.x = element_blank() # Limpiamos líneas verticales innecesarias
    )
}





#' Visualización de densidades por especie
#' @export
plot_numeric_distributions <- function(df, save_plot = FALSE) {
  
  df_long <- df %>%
    tidyr::pivot_longer(cols = where(is.numeric), 
                        names_to = "Variable", 
                        values_to = "Valor")
  
  p <- ggplot(df_long, aes(x = Valor, fill = Species)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~Variable, scales = "free", ncol = 3) +
    # Asignación manual de colores para coherencia total
    scale_fill_manual(values = c("CH" = "#377EB8", "RT" = "#E41A1C", "SS" = "#4DAF4A")) +
    labs(
      title = "Distribución de Variables Biométricas Estandarizadas",
      subtitle = "Comparativa de densidad por especie (Z-score)",
      x = "Valor Normalizado", y = "Densidad"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  
  if (save_plot) {
    if (!dir.exists("../output")) dir.create("../output")
    ggsave("../output/01_eda_densities.png", plot = p, width = 10, height = 7, dpi = 300)
  }
  return(p)
}







#' Matriz de Correlación de Biometría
#' 
#' @param df Dataframe con las variables procesadas.
#' @param save_plot Lógico. Si es TRUE, guarda en ../output/01_eda_correlation.png
#' @export
plot_correlation_matrix <- function(df, save_plot = FALSE) {

  # 1. Calculamos la matriz
  cor_data <- df %>% 
    dplyr::select(where(is.numeric)) %>% 
    cor(use = "complete.obs")
  
  # 2. Definimos la configuración del gráfico una sola vez
  # Usamos una función interna para evitar repetir código
  draw_plot <- function() {
    corrplot(cor_data, method = "color", type = "upper", 
             addCoef.col = "black", tl.col = "black", tl.srt = 45,
             diag = FALSE, title = "\n\nBiometric Correlation Matrix", 
             mar = c(0,0,1,0))
  }
  
  # 3. Lógica de guardado
  if (save_plot) {
    if (!dir.exists("../output")) dir.create("../output")
    png("../output/01_eda_correlation.png", width = 800, height = 800, res = 120)
    draw_plot() # Invocamos el dibujo aquí
    dev.off()
  }
  
  # 4. Lo mostramos en el Notebook
  draw_plot() # Invocamos el mismo dibujo aquí
}





#' Pairplot de Biometría por Especie
#' @export
plot_pairplot <- function(df, save_plot = FALSE) {
  
  # Definimos los colores manualmente para que coincidan con el resto de la práctica
  # RT = Rojo, CH = Azul, SS = Verde
  spec_colors <- c("CH" = "#377EB8", "RT" = "#E41A1C", "SS" = "#4DAF4A")
  
  p <- ggpairs(df, 
               columns = 2:ncol(df), 
               aes(color = Species, fill = Species, alpha = 0.4),
               upper = list(continuous = wrap("cor", size = 3.5)),
               lower = list(continuous = wrap("points", alpha = 0.3, size = 0.6))) +
    theme_minimal() +
    # Aplicamos los colores manuales a los bordes y rellenos
    scale_color_manual(values = spec_colors) +
    scale_fill_manual(values = spec_colors) +
    labs(title = "Relaciones Biométricas Cruzadas",
         subtitle = "Visualización multivariante y separación de clústeres naturales") +
    theme(axis.text = element_text(size = 7),
          strip.text = element_text(size = 8, face = "bold"))
  
  if (save_plot) {
    if (!dir.exists("../output")) dir.create("../output")
    ggsave("../output/01_eda_pairplot.png", plot = p, width = 12, height = 10, dpi = 300)
  }
  return(p)
}








#' PCA Manual desde Matriz de Covarianza
#' 
#' @param df Dataframe con variables ya normalizadas.
#' @return Una lista con autovalores, autovectores y los scores (datos proyectados).
#' @export
calculate_pca <- function(df) {
  # 1. Convertimos a matriz (solo numéricas)
  X <- as.matrix(df)
  
  # 2. Calculamos la Matriz de Covarianza
  # Como ya está normalizada, esto es equivalente a la matriz de correlación
  R <- cov(X)
  
  # 3. Cálculo de Autovalores (lambda) y Autovectores (v)
  # Resolvemos la ecuación: (R - lambda*I)v = 0
  eigen_decomp <- eigen(R)
  
  # 4. Proyectamos los datos originales sobre los autovectores
  # Scores = X %*% V
  pca_scores <- X %*% eigen_decomp$vectors
  
  # Renombramos para que sea legible
  colnames(pca_scores) <- paste0("PC", 1:ncol(pca_scores))
  rownames(eigen_decomp$vectors) <- colnames(df)
  colnames(eigen_decomp$vectors) <- paste0("PC", 1:ncol(pca_scores))
  
  # Devolvemos todo en una lista organizada
  return(list(
    values = eigen_decomp$values,
    vectors = eigen_decomp$vectors,
    scores = as.data.frame(pca_scores)
  ))
}





#' Visualizar Varianza Explicada (Scree Plot) - Versión Manual
#' 
#' @param pca_list Lista devuelta por calculate_manual_pca (contiene $values)
#' @export
plot_pca_variance <- function(pca_list) {
  require(ggplot2)
  
  # Calculamos varianza individual y acumulada a partir de los autovalores
  var_exp <- pca_list$values / sum(pca_list$values)
  var_acum <- cumsum(var_exp)
  
  # Preparamos el dataframe para ggplot
  df_plot <- data.frame(
    PC = factor(paste0("PC", 1:length(var_exp)), 
                levels = paste0("PC", 1:length(var_exp))),
    Varianza = var_exp,
    Acumulada = var_acum
  )
  
  # Creamos el gráfico
  ggplot(df_plot, aes(x = PC)) +
    # Barras para la varianza de cada componente
    geom_col(aes(y = Varianza), fill = "steelblue", alpha = 0.8) +
    # Línea y puntos para la varianza acumulada
    geom_line(aes(y = Acumulada, group = 1), color = "firebrick", size = 1) +
    geom_point(aes(y = Acumulada), color = "firebrick", size = 2) +
    # Línea de corte al 90% (umbral típico de calidad)
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") +
    annotate("text", x = 4.5, y = 0.93, label = "Umbral 90%", size = 3) +
    # Formateo de ejes y etiquetas
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
    labs(title = "Scree Plot: Varianza Explicada Acumulada",
         subtitle = "Justificación de la reducción de dimensiones",
         x = "Componentes Principales (Ejes rotados)",
         y = "% de Varianza Retenida") +
    theme_minimal()
}



#' Crear Dataframe de Componentes Principales
#' 
#' Combina los resultados del PCA manual con las especies originales.
#' 
#' @param pca_list Lista resultante de calculate_manual_pca()
#' @param species_vector Vector de especies (procedente de hawks_final)
#' @param n_comp Cantidad de componentes a incluir (por defecto 2)
#' @return Un dataframe listo para el análisis de clustering
#' @export
create_pca_df <- function(pca_list, species_vector, n_comp = 2) {
  # Extraemos solo las columnas de scores que nos interesan
  df_pca <- as.data.frame(pca_list$scores[, 1:n_comp])
  
  # Añadimos la columna Species
  df_pca$Species <- species_vector
  
  # Reorganizamos: Species al principio y luego los componentes
  df_pca <- df_pca[, c("Species", paste0("PC", 1:n_comp))]
  
  return(df_pca)
}