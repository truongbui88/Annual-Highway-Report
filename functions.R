library(ggplot2)
library(viridisLite)
# Function to generate maps for each specified ranking column
create_maps <- function(data, columns) {
  # Check if the input is an sf object
  if (!inherits(data, "sf")) {
    stop("Need (sf) object")
  }
  
  # format column names 
  format_title <- function(name) {
    name %>%
      gsub("_", " ", .) %>% 
      tools::toTitleCase() 
  }
  
  # Loop through each column 
  for (col in columns) {
    if (!col %in% names(data)) {
      warning(paste("Column", col, "not found"))
      next
    }
    
    # Format the title
    formatted_title <- format_title(col)
    
  
    # Generate the map
    p_map <- ggplot(data = data) +
      geom_sf(aes(geometry = geom, fill = !!rlang::sym(col)), color = "white") +
      geom_sf_text(aes(geometry = geom, label = !!rlang::sym(col)), check_overlap = TRUE, size = 2, color = "white") +
      scale_fill_viridis_c(option = "D", direction = -1) +  
      labs(title = paste("Ranking by", formatted_title)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),   
        axis.title.y = element_blank(),  
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),
        legend.position = "none"
      )
    
    # Print the map
    print(p_map)
  }
}

#create_maps(d_map, c("overall_score_rank"))
