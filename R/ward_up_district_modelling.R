library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(readxl)


# Build ward-up using HWB ward assignments --------------------------------

# import lookup for wards to districts based on HWB table
ward_district_locality_lookup <- read_excel("data/ward_district_locality_lookup.xlsx") |> 
  janitor::clean_names() |> 
  mutate(district = str_replace(district, " Birmingham", ""))

# merge wards that belong to the same district
proposed_bham_districts <- bham_ward25_boundaries |> 
  left_join(ward_district_locality_lookup,
            by = join_by("wd25nm" == "ward")) |> 
  group_by(district) |> 
  summarise() 

# show districts on map
# first convert Selly Oak from multipolygon to polygon to get labels on both chunks
proposed_bham_districts <- lapply(1:nrow(proposed_bham_districts), function(i) {
  st_cast(proposed_bham_districts[i, ], "POLYGON")
}) %>%
  do.call(rbind, .) 

proposed_bham_districts|> 
  leaflet() |> 
  addTiles(options = tileOptions(opacity = 0.5)) |> 
  addPolygons(data = bham_ward25_boundaries,
              color = "lightgrey",
              weight = 1,
              opacity = 1,
              fillOpacity = 1,
              fillColor = "white") |>
  addPolygons(color = border_colour,
              weight = 1,
              opacity = 1,
              fillOpacity = 0,
              fillColor = "white",
              label = ~district,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE))

# same again for localities
proposed_bham_localities <- bham_ward25_boundaries |> 
  left_join(ward_district_locality_lookup,
            by = join_by("wd25nm" == "ward")) |> 
  group_by(locality) |> 
  summarise()

leaflet() |> 
  addTiles(options = tileOptions(opacity = 1)) |> 
  addPolygons(data = proposed_bham_localities,
              color = border_colour,
              weight = 1,
              opacity = 1,
              fillOpacity = 0.7,
              fillColor = "white",
              label = ~locality,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE))


# Build ward-up using best fit to current districts -----------------------

# use existing df to see which district a ward "belongs" to in terms of greatest percent inside that district
district_wards_best_fit <- district_wards |> 
  as.data.frame() |> 
  select(wd25nm, district_name, pct) |> 
  arrange(desc(pct)) |> 
  distinct(wd25nm,
           .keep_all = T)

# plot
proposed_bham_districts_best_fit <- bham_ward25_boundaries |> 
  left_join(district_wards_best_fit,
            by = "wd25nm") |> 
  group_by(district_name) |> 
  summarise()

leaflet() |> 
  addTiles(options = tileOptions(opacity = 0.5)) |> 
  addPolygons(data = bham_ward25_boundaries,
              color = "lightgrey",
              weight = 1,
              opacity = 1,
              fillOpacity = 1,
              fillColor = "white") |> 
  addPolygons(data = proposed_bham_districts_best_fit,
              color = border_colour,
              weight = 1,
              opacity = 1,
              fillOpacity = 0,
              fillColor = "white",
              label = ~district_name,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE))

# see which wards are different between the two
district_wards_best_fit |> 
  left_join(ward_district_locality_lookup,
            by = join_by("wd25nm" == "ward")) |> 
  rename(district_HWB = district,
         district_best_fit = district_name) |> 
  mutate(agree = case_when(district_HWB == district_best_fit ~ "yes",
                           .default = "no")) |> 
  View()


# Map to compare district models ------------------------------------------

leaflet() |> 
  addTiles(options = tileOptions(opacity = 0.5)) |> 
  addPolygons(data = bham_ward25_boundaries,
              color = "lightgrey",
              weight = 1,
              opacity = 1,
              fillOpacity = 1,
              fillColor = "white") |>
  addPolygons(data = proposed_bham_districts,
              color = border_colour,
              weight = 1,
              opacity = 1,
              fillOpacity = 0,
              fillColor = "white",
              label = ~district,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              group = "Model C: Ward-up, HWB-assigned") |> 
  addPolygons(data = proposed_bham_districts_best_fit,
              color = border_colour,
              weight = 1,
              opacity = 1,
              fillOpacity = 0,
              fillColor = "white",
              label = ~district_name,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              group = "Model D: Ward-up, best fit") |> 
  addPolygons(data = bham_district22_boundaries,
              color = border_colour,
              weight = 1,
              opacity = 1,
              fillOpacity = 0,
              fillColor = "white",
              label = ~pcon22nm,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              group = "Model A: Pre-2024 constituencies") |> 
  addPolygons(data = bham_pcon24_boundaries,
              color = border_colour,
              weight = 1,
              opacity = 1,
              fillOpacity = 0,
              fillColor = "white",
              label = ~pcon24nm,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              group = "Model B: Post-2024 constituencies") |> 
  addLayersControl(
    baseGroups = c("Model A: Pre-2024 constituencies", "Model B: Post-2024 constituencies", "Model C: Ward-up, HWB-assigned", "Model D: Ward-up, best fit"))
