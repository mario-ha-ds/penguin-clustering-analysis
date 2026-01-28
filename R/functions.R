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