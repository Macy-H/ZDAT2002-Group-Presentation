#' 02_data_prep.R
#' Road safety open data: decode relevant variables in each dataset
#'
#'
#' The guide codes missing values as -1. 
#' Some fields also use 9 or 99 for "unknown (self reported)".

# ----- Source relevant components -----
source("functions/fct_add_labels.R")
source("functions/fct_clean_missing_data_codes.R")

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)

# ----- Read the data -----
collision_data <- readr::read_csv("data/dft-road-casualty-statistics-collision-2025.csv")
vehicle_data  <- readr::read_csv("data/dft-road-casualty-statistics-vehicle-2025.csv")
casualty_data <- readr::read_csv("data/dft-road-casualty-statistics-casualty-2025.csv")

data_guide <- readr::read_csv("lookup/data_map.csv")

# ----- Decode the Collisions Data -----
# Select the relevant collision vars 
# NOTE: We may want to add adjusted severity, I have not looked at this? 

collision_vars_to_select <- c("collision_index", "collision_ref_no",
                              "date", "collision_year", "day_of_week", "time",
                              "collision_severity_label", "number_of_vehicles", 
                              "number_of_casualties", "speed_limit", "road_type_label",
                              "light_conditions_label", "weather_conditions_label", 
                              "road_surface_conditions_label", "urban_or_rural_area_label")

collision_vars_to_decode <- c("collision_severity", "light_conditions", 
                              "road_surface_conditions", "urban_or_rural_area", 
                              "road_type", "weather_conditions")

collision_data <- clean_missing_data_codes(collision_data, data_guide, "collision", collision_vars_to_decode)

collision_data_decoded <- add_labels(collision_data, data_guide, "collision", collision_vars_to_decode) |> 
  dplyr::select(dplyr::all_of(collision_vars_to_select)) |> 
  dplyr::mutate(
    # Dates are DD/MM/YYYY; time is HH:MM separate out into date, month and hour
    date  = if (inherits(date, "Date")) date else lubridate::dmy(date),
    month = lubridate::floor_date(date, "month"),
    
    # Only 20, 30, 40, 50, 60, 70 are valid limits
    speed_limit = dplyr::case_when(speed_limit %in% c(20, 30, 40, 50, 60, 70) ~ speed_limit,
                                   TRUE ~ NA_real_),
    
    # Light: 1 = Daylight, 4-7 = Darkness (7 = lighting unknown)
    night = grepl("Darkness", light_conditions_label),
  )

# ----- Decode the Vehicle Data -----
# Select the relevant vehicle variables
# NOTE:: age of vehicle is -1 for everything so I have not selected this

vehicle_vars_to_select <- c("collision_index", "collision_ref_no", 
                            "collision_year", "vehicle_group",
                            "age_of_driver", "vehicle_left_hand_drive_label")

vehicle_vars_to_decode <- c("vehicle_type", "vehicle_left_hand_drive")

vehicle_data <- clean_missing_data_codes(vehicle_data, data_guide, "vehicle", vehicle_vars_to_decode)

vehicle_data_decoded <- add_labels(vehicle_data, data_guide, "vehicle", vehicle_vars_to_decode) |> 
  dplyr::mutate(
    vehicle_group = case_when(
      stringr::str_detect(vehicle_type_label, "Motorcycle") ~ "Motorcycle",
      stringr::str_detect(vehicle_type_label, "Car|Taxi") ~ "Car/Taxi",
      stringr::str_detect(vehicle_type_label, "Goods") ~ "Goods Vehicle",
      stringr::str_detect(vehicle_type_label, "Bus|Coach|Minibus") ~ "Public Transport",
      vehicle_type_label == "Pedal cycle" ~ "Pedal Cycle",
      TRUE ~ "Other"
      )
    ) |> 
  dplyr::select(dplyr::all_of(vehicle_vars_to_select))

# ----- Decode the Casualty Data -----
casualty_vars_to_select <- c("collision_index", "collision_ref_no", 
                             "collision_year", "casualty_severity_label",
                             "road_user_group")

casualty_vars_to_decode <- c("casualty_type", "casualty_severity")

casualty_data_decoded <- add_labels(casualty_data, data_guide, "casualty", casualty_vars_to_decode) |> 
  dplyr::mutate(
    road_user_group = case_when(
      stringr::str_detect(casualty_type_label,
                          stringr::regex("pedestrian", ignore_case = TRUE)) ~ "Pedestrian",
      stringr::str_detect(casualty_type_label,
                          stringr::regex("cycl", ignore_case = TRUE)) ~ "Cyclist",
      stringr::str_detect(casualty_type_label,
                          stringr::regex("motorcycle|scooter|moped",
                                ignore_case = TRUE)) ~ "Motorcyclist",
      stringr::str_detect(casualty_type_label,
                          stringr::regex("car|taxi|bus|coach|minibus|goods|van|occupant",
                                ignore_case = TRUE)) ~ "Motor Vehicle Occupant",
      TRUE ~ "Other"
    )
  ) |> 
  dplyr::select(dplyr::all_of(casualty_vars_to_select))
