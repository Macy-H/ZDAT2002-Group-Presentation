#' data_prep.R
#' Road safety open data: clean, group and combine the three DfT tables
#'
#'   collisions : one row per collision
#'   vehicles   : one row per vehicle involved in a collision
#'   casualties : one row per person injured in a collision
#'
#' Only join MANY-to-ONE (vehicles/casualties -> collisions).
#' Anything needed at collision level is summarised first, then joined.
#' Codes are from the DfT data guide.

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

# ----- Read the data -----
collision_data <- read_csv("data/dft-road-casualty-statistics-collision-2025.csv")
vehicle_data  <- read_csv("data/dft-road-casualty-statistics-vehicle-2025.csv")
casualty_data <- read_csv("data/dft-road-casualty-statistics-casualty-2025.csv")

# ----- Collisions -----
# The guide codes missing values as -1. 
# Some fields also use 9 or 99 for "unknown (self reported)".
# Recode all of these to NA so they are not treated as categories.
collision_data <- collision_data %>%
  dplyr::mutate(across(any_of(c("speed_limit", "light_conditions", "weather_conditions",
                                "road_surface_conditions", "road_type", "urban_or_rural_area")), ~ na_if(.x, -1)),
                road_surface_conditions = dplyr::na_if(road_surface_conditions, 9),
                weather_conditions      = dplyr::na_if(weather_conditions, 9),
                
                # Dates are DD/MM/YYYY; time is HH:MM separate out into date, month and hour
                date  = if (inherits(date, "Date")) date else lubridate::dmy(date),
                month = floor_date(date, "month"),
                hour  = as.integer(substr(time, 1, 2)),
                
                # Only 20, 30, 40, 50, 60, 70 are valid limits
                speed_limit = dplyr::case_when(speed_limit %in% c(20, 30, 40, 50, 60, 70) ~ speed_limit,
                                               TRUE ~ NA_real_),
    
                # Severity: 1 = Fatal, 2 = Serious, 3 = Slight
                severity = factor(collision_severity, levels = 1:3,
                                  labels = c("Fatal", "Serious", "Slight")),
                fatal = collision_severity == 1,
                fatal_or_serious = collision_severity %in% c(1, 2),
    
                # Light: 1 = Daylight, 4-7 = Darkness (7 = lighting unknown)
                night = dplyr::case_when(light_conditions == 1 ~ FALSE,
                                         light_conditions %in% 4:7 ~ TRUE),
                light_group = dplyr::case_when(light_conditions == 1 ~ "Daylight",
                                               light_conditions == 4 ~ "Dark - lights lit",
                                               light_conditions %in% c(5, 6) ~ "Dark - unlit or no lighting"),
                road_surface = factor(road_surface_conditions, levels = 1:7,
                                      labels = c("Dry", "Wet or damp", "Snow", "Frost or ice",
                                                 "Flood over 3cm", "Oil or diesel", "Mud")),
                area_type = factor(urban_or_rural_area, levels = 1:2,
                                   labels = c("Urban", "Rural"))   # 3 = Unallocated becomes NA
  )

# ----- Vehicles -----
vehicle_data <- vehicle_data %>%
  dplyr::mutate(vehicle_type  = dplyr::na_if(vehicle_type, -1),
                age_of_driver = dplyr::na_if(age_of_driver, -1),
                vehicle_group = dplyr::case_when(
                  vehicle_type == 1 ~ "Pedal cycle",
                  vehicle_type %in% c(2, 3, 4, 5, 23, 97, 103, 104, 105, 106) ~ "Motorcycle",
                  vehicle_type %in% c(8, 9, 108, 109) ~ "Car/Taxi",
                  vehicle_type %in% c(10, 11) ~ "Public transport",
                  vehicle_type %in% c(19, 20, 21, 98, 113) ~ "Goods vehicle",
                  vehicle_type == 17 ~ "Agricultural",
                  vehicle_type %in% c(16, 22, 33, 18, 90) ~ "Other",
                  TRUE ~ NA_character_  # includes 99 (unknown) and -1 (missing)
    )
  )

# ----- Casualties -----
casualty_data <- casualty_data %>%
  dplyr::mutate(casualty_type = dplyr::na_if(casualty_type, -1),
                casualty_class = dplyr::na_if(casualty_class, -1),
                age_of_casualty = dplyr::na_if(age_of_casualty, -1),
                casualty_severity_label = factor(casualty_severity, levels = 1:3,
                                                 labels = c("Fatal", "Serious", "Slight")),
    road_user_group = dplyr::case_when(casualty_type == 0 ~ "Pedestrian",
                                       casualty_type == 1 ~ "Cyclist",
                                       casualty_type %in% c(2, 3, 4, 5, 23, 97, 103, 104, 105, 106) ~ "Motorcyclist",
                                       casualty_type %in% c(8, 9, 10, 11, 17, 18, 19, 20, 21, 90, 98, 108, 109, 110, 113) ~ "Motor Vehicle Occupant",
                                       casualty_type %in% c(16, 22) ~ "Other",
                                       TRUE ~ NA_character_),
    active_travel = road_user_group %in% c("Pedestrian", "Cyclist"),
    killed_or_seriously_injured = casualty_severity %in% c(1, 2)
  )

# ----- Checks before joining ----
# One row per collision
stopifnot(n_distinct(collision_data$collision_index) == nrow(collision_data))

# Every vehicle and casualty should belong to a known collision
no_match_veh <- dplyr::anti_join(vehicle_data,  collision_data, by = "collision_index") %>% nrow()
no_match_cas <- dplyr::anti_join(casualty_data, collision_data, by = "collision_index") %>% nrow()
message("Vehicles with no matching collision:  ", no_match_veh)
message("Casualties with no matching collision: ", no_match_cas)

# ----- Combine Datasets -----
# Collision columns that the other tables need
collision_cols <- collision_data %>%
  dplyr::select(collision_index, collision_severity, severity, speed_limit,
         light_conditions, light_group, night, road_surface, area_type,
         local_authority_ons_district, date, month, hour)

# Casualty level: one row per casualty (many-to-one) 
casualty_level_joined <- casualty_data %>%
  dplyr::left_join(collision_cols, by = "collision_index", relationship = "many-to-one")

# Vehicle level: one row per vehicle
vehicle_level_joined <- vehicle_data %>%
  dplyr::left_join(collision_cols, by = "collision_index", relationship = "many-to-one")

# Collision level flags: summarise first, then join (stays one row per collision)
# Summarise the vehicle data
# For each collision how many of each vehicle group was involved
vehicle_summary <- vehicle_data %>%
  dplyr::count(collision_index, vehicle_group) %>%
  tidyr::pivot_wider(names_from = vehicle_group,
                     values_from = n,
                     values_fill = 0)

# Summarise the casualty data
# For each collision was there any active travel (predestrian or cyclist) and how many casualties were there
casualty_summary <- casualty_data %>%
  dplyr::group_by(collision_index) %>%
  dplyr::summarise(any_active_travel = any(active_travel, na.rm = TRUE),
            n_casualty_rows   = n(),
            .groups = "drop")

collision_data <- collision_data %>%
  dplyr::left_join(veh_summary, by = "collision_index", relationship = "one-to-one") %>%
  dplyr::left_join(cas_summary, by = "collision_index", relationship = "one-to-one")

# ----- Post-join checks ----
stopifnot(nrow(casualty_joined) == nrow(casualty_data),
          nrow(vehicle_joined)  == nrow(vehicle_data),
          n_distinct(collision_data$collision_index) == nrow(collision_data))

# Does the number of casualty rows match the collision table's own count?
mismatch <- collision_data %>%
  filter(!is.na(n_casualty_rows), n_casualty_rows != number_of_casualties) %>%
  nrow()
message("Collisions where casualty rows != number_of_casualties: ", mismatch)