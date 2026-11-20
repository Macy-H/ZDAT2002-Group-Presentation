# ----- Read in the data guide -----
data_guide <- read_csv("data/data_guide.csv")

# ----- Vehicle data ----
# Extract the vehicle types 
vehicle_lookup <- data_guide %>%
  filter(dataset == "vehicle", 
         variable == "vehicle_type") %>%
  transmute(
    vehicle_type = as.numeric(code),
    vehicle_type_label = label,
    vehicle_group = case_when(
      
      grepl("Pedal cycle", label, ignore.case = TRUE) ~
        "Pedal Cycle",
      
      grepl("Motorcycle|Scooter", label, ignore.case = TRUE) ~
        "Motorcycle",
      
      grepl("Car|Taxi", label, ignore.case = TRUE) ~
        "Car/Taxi",
      
      grepl("Bus|Coach|Minibus", label, ignore.case = TRUE) ~
        "Public Transport",
      
      grepl("Goods vehicle|HGV|LGV|Van", label, ignore.case = TRUE) ~
        "Goods Vehicle",
      
      grepl("Agricultural", label, ignore.case = TRUE) ~
        "Agricultural",
      
      TRUE ~ "Other"
    )
  )