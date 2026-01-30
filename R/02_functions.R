#' Calculate WCSS for multiple K values
#' 
#' @param data Numeric matrix or dataframe (PCA scores)
#' @param max_k Maximum number of clusters to test
#' @return A tibble with K and its corresponding WSS
#' @export
calculate_wss_values <- function(data, max_k = 10) {
  set.seed(123) # For reproducibility
  wss <- purrr::map_dbl(1:max_k, function(k) {
    kmeans(data, centers = k, nstart = 25)$tot.withinss
  })
  
  tibble::tibble(K = 1:max_k, WSS = wss)
}




#' Plot the Elbow Method
#' 
#' @param wss_df Dataframe from calculate_wss_values
#' @export
plot_elbow <- function(wss_df) {
  require(ggplot2)
  ggplot(wss_df, aes(x = K, y = WSS)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "firebrick", size = 2) +
    scale_x_continuous(breaks = 1:nrow(wss_df)) +
    labs(
      title = "The Elbow Method",
      subtitle = "WSS reduction as a function of K",
      x = "Number of Clusters (K)",
      y = "Total Within-Cluster Sum of Squares"
    ) +
    theme_minimal()
}