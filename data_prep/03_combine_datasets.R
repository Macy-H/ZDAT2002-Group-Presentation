#' 03_combine_datasets.R
#' Road safety open data: clean, group and combine the three DfT tables
#'
#'   collisions : one row per collision
#'   vehicles   : one row per vehicle involved in a collision
#'   casualties : one row per person injured in a collision
#'
#' Only join MANY-to-ONE (vehicles/casualties -> collisions).
#' Anything needed at collision level is summarised first, then joined.

# ----- Get the decoded data to join -----
source("02_data_prep.R")

# ----- Checks before joining ----
# One row per collision
stopifnot(n_distinct(collision_data_decoded$collision_index) == nrow(collision_data_decoded))

# Every vehicle and casualty should belong to a known collision
no_match_veh <- dplyr::anti_join(vehicle_data_decoded,  collision_data_decoded, by = "collision_index") %>% nrow()
no_match_cas <- dplyr::anti_join(casualty_data_decoded, collision_data_decoded, by = "collision_index") %>% nrow()
message("Vehicles with no matching collision:  ", no_match_veh)
message("Casualties with no matching collision: ", no_match_cas)

# ----- Combine Datasets -----
# Collision columns that the other tables need
collision_cols <- collision_data_decoded %>%
  dplyr::select(-collision_ref_no, -collision_year)

# Casualty level: one row per casualty (many-to-one) 
casualty_level_joined <- casualty_data_decoded %>%
  dplyr::left_join(collision_cols, by = c("collision_index"), relationship = "many-to-one")

# Vehicle level: one row per vehicle
vehicle_level_joined <- vehicle_data_decoded %>%
  dplyr::left_join(collision_cols, by = "collision_index", relationship = "many-to-one")

# Collision level flags: summarise first, then join (stays one row per collision)
# Summarise the vehicle data
# For each collision how many of each vehicle group was involved
vehicle_summary <- vehicle_data_decoded %>%
  dplyr::count(collision_index, vehicle_group) %>%
  tidyr::pivot_wider(names_from = vehicle_group,
                     values_from = n,
                     values_fill = 0)

# Summarise the casualty data
# For each collision was there any active travel (pedestrian or cyclist) and how many casualties were there
casualty_summary <- casualty_data_decoded %>%
  group_by(collision_index) %>%
  summarise(any_active_travel = any(road_user_group %in% c("Pedestrian", "Cyclist"),
                                    na.rm = TRUE),
    n_casualty_rows = n(),
    .groups = "drop"
  )


collision_data_joined <- collision_data_decoded %>%
  dplyr::left_join(vehicle_summary, by = "collision_index", relationship = "one-to-one") %>%
  dplyr::left_join(casualty_summary, by = "collision_index", relationship = "one-to-one")

# ----- Post-join checks ----
stopifnot(nrow(casualty_level_joined) == nrow(casualty_data_decoded),
          nrow(vehicle_level_joined)  == nrow(vehicle_data_decoded),
          n_distinct(collision_data_joined$collision_index) == nrow(collision_data_decoded))

# Does the number of casualty rows match the collision table's own count?
mismatch <- collision_data_joined %>%
  filter(!is.na(n_casualty_rows), n_casualty_rows != number_of_casualties) %>%
  nrow()
message("Collisions where casualty rows != number_of_casualties: ", mismatch)
