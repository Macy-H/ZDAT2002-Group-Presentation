clean_missing_data_codes <- function(data,
                                     data_guide,
                                     dataset_name,
                                     variables) {
  
  for(variable_name in variables) {
    
    missing_codes <- data_guide %>%
      filter(table == dataset_name,
             field_name == variable_name) %>%
      filter(grepl("missing|unknown|out of range|unallocated",
                   label,
                   ignore.case = TRUE)) %>%
      pull(code) %>%
      as.numeric()
    
    data[[variable_name]][
      data[[variable_name]] %in% missing_codes
    ] <- NA
  }
  
  data
}