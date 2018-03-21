# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Create dataframe for maps: 
##   input:  `report_df_procsd`
##   output: `report_df_maps`
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Source `report_df_procsd` and helper functions ----
###
if (operational) {  ### OPERATIONAL ###
  source("./report_processor.R", local = TRUE)
  load("mi_counties_list.Rdata")
  # source("./plots_helper_fxns.R", local = TRUE)
} else {            ### DEBUGGING ###
  source("./UDSEnrollmentDashboard/report_processor.R", local = TRUE)
  load("UDSEnrollmentDashboard/mi_counties_list.Rdata")
  # source("./UDSEnrollmentDashboard/plots_helper_fxns.R", local = TRUE)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Load libraries ----
###
library(dplyr)
library(tidyr)
library(ggplot2)
# devtools::install_github("dkahle/ggmap")
library(ggmap)
library(maps)
library(mapdata)

## Helper functions
nix_lat_long_grid <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

## Copy report_df_procsd
report_df_maps <- report_df_procsd

## US states
us_states <- map_data("state")
## Michigan state
michigan <- subset(us_states, region %in% c("michigan"))
## US counties
us_counties <- map_data("county")
## Michigan counties
mi_counties <- subset(us_counties, region == "michigan")

# Michigan base map
mi_base_map <- ggplot(data = michigan, aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3) +
  geom_polygon(color = "black", fill = "gray") +
  theme_nothing() +
  geom_polygon(data = mi_counties, fill = NA, color = "white") +
  geom_polygon(fill = NA, color = "black")
# mi_base

## Clean up county names in report_df_procsd
report_df_maps$county <- gsub(pattern = "^Genessee$", replacement = "Genesee",
                              x = report_df_maps$county)
report_df_maps$county <- gsub(pattern = "^Oakand$", replacement = "Oakland",
                              x = report_df_maps$county)
report_df_maps$county <- gsub(pattern = "^Eaton $", replacement = "Eaton",
                              x = report_df_maps$county)
report_df_maps$county <- gsub(pattern = "St.", replacement = "St",
                              x = report_df_maps$county)
report_df_maps$county <- tolower(report_df_maps$county)

## Eliminate space in redcap_event_name values
report_df_maps$redcap_event_name <- gsub(pattern = " ", replacement = "",
                                         x = report_df_maps$redcap_event_name)

## Select appropriate columns from report_df_maps
report_df_maps <- report_df_maps %>% 
  select(subject_id, exam_date, county)
  
## Participant counts by county table
report_df_maps_county_count <- report_df_maps %>%
  group_by(county) %>% 
  rename(subregion = county) %>% 
  summarize(Count = n())

## left join `mi_counties` with `report_df_maps_county_count`
mi_counties_partic_count <- left_join(mi_counties, 
                                      report_df_maps_county_count, 
                                      by = "subregion")
# mi_base_map +
#   geom_polygon(data = mi_counties_partic_count, aes(fill = Count),
#                color = "white", size = 0.1) +
#   geom_polygon(color = "black", fill = NA) +
#   theme_bw() +
#   nix_lat_long_grid +
#   scale_fill_gradient(low = "#FFFFFF", high = "darkblue",
#                       breaks = c(1, 20, 40, 60, 80, 100)) +
#   ggtitle(label = "Participant Counts by County", 
#           subtitle = "March 2017 to Present")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # #     EXTRA  SPACE    # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 