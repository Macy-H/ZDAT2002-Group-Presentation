# Converts the DfT data guide (.xlsx file) into a safe .csv lookup file (the original has commas in it)
# Also makes the names snake_case aso its nicer to use in R
library("readxl")

lookup_file <- "lookup/dft-road-casualty-statistics-road-safety-open-dataset-data-guide-2025.xlsx"
output_file <- "lookup/data_map.csv"

if (!file.exists(output_file)) {
    lookup <- read_excel(lookup_file, sheet = "2024_code_list")
    names(lookup) <- c("table", "field_name", "code", "label", "note")

    # Remove rows that dont have a mapping
    lookup <- lookup[!is.na(lookup$code) & !is.na(lookup$label), ]

    write.csv(lookup, "lookup/data_map.csv", row.names = FALSE)
}
