add_labels <- function(data,
                       guide,
                       dataset_name,
                       variables) {
  
  for (variable_name in variables) {
    
    lookup <- guide %>%
      dplyr::filter(table == dataset_name,
                    field_name == variable_name) %>%
      dplyr::distinct(code, label) |> 
      dplyr::mutate(code = as.numeric(code)) %>%
      dplyr::group_by(code) %>%
      dplyr::summarise(label = dplyr::last(label), .groups = "drop")
    
    names(lookup) <- c(variable_name, paste0(variable_name, "_label"))
    
    data <- data %>%
      dplyr::left_join(lookup, by = variable_name)
  }
  
  data
}