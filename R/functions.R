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
#' Selects biologically relevant variables and ensures correct data types
#' for clustering analysis.
#'
#' @param df The raw Hawks dataframe from Stat2Data.
#' @return A refined dataframe with selected factors and numeric biometrics.
#' @export
refine_hawks_data <- function(df) {
  df %>%
    dplyr::select(
      # 1. Identifiers and Factors
      Species, Age, Sex, 
      # 2. Biometric Measurements
      Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus,
      # 3. Physiological Indicators
      WingPitFat, KeelFat, Crop
    ) %>%
    dplyr::mutate(
      across(c(Species, Age, Sex), as.factor),
      across(c(Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus, 
               WingPitFat, KeelFat, Crop), as.numeric)
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




#' KNN Missing Value Imputation
#'
#' Fills numeric NaNs using the k-Nearest Neighbors algorithm.
#' @param df The dataframe to process.
#' @param k Number of neighbors (default: 5).
#' @return A dataframe with no missing values in numeric columns.
impute_knn <- function(df, k = 5) {
  # VIM handles the scaling and calculation automatically
  # We select only numeric columns for the mathematical distance
  df_numeric <- df %>% dplyr::select(where(is.numeric))
  df_meta <- df %>% dplyr::select(!where(is.numeric))
  
  # Perform imputation
  imputed_data <- VIM::kNN(df_numeric, k = k, imp_var = FALSE)
  
  # Recombine with non-numeric data (Species, etc.)
  return(dplyr::bind_cols(df_meta, imputed_data))
}