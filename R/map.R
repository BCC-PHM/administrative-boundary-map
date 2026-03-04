library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(readxl)

# import data
# local authority boundary
bham_la24_boundaries <- read_sf("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/Shape files/boundaries-la-districts-2024-wmca.geojson") |> 
  filter(lad24nm == "Birmingham")

# 2024 constituencies
bham_pcon24_boundaries <- read_sf("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/Shape files/birmingham_constituencies_2024_bgc.geojson") |> 
  mutate(pcon24nm = str_replace(pcon24nm, "Birmingham ", ""))

#districts
bham_district22_boundaries <- read_sf("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/Shape files/boundaries-constituencies-2022-wmca.geojson") |> 
  filter(local_authority_name == "Birmingham") |> 
  mutate(pcon22nm = str_replace(pcon22nm, "Birmingham, ", ""))

# create localities from districts
bham_locality_boundaries <- bham_district22_boundaries |> 
  mutate(locality = case_when(pcon22nm == "Sutton Coldfield" ~ "North",
                              pcon22nm == "Erdington" ~ "North",
                              pcon22nm == "Perry Barr" ~ "West",
                              pcon22nm == "Ladywood" ~ "West",
                              pcon22nm == "Hodge Hill" ~ "East",
                              pcon22nm == "Yardley" ~ "East",
                              pcon22nm == "Edgbaston" ~ "South",
                              pcon22nm == "Northfield" ~ "South",
                              pcon22nm == "Hall Green" ~ "Central",
                              pcon22nm == "Selly Oak" ~ "Central")) |> 
  group_by(locality) |> 
  summarise(do_union = TRUE)

# wards
bham_ward25_boundaries <- read_sf("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/Shape files/boundaries-wards-2025-wmca.geojson") |> 
  filter(lad25nm == "Birmingham")

# GPs
bham_gp_points <- read_csv("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/fingertips-gp-practices_bsol.csv") |> 
  janitor::clean_names() |> 
  filter(grepl("Birmingham", constituency_name) | constituency_name == "Sutton Coldfield")

# make gp dataset a shape file
bham_gp_points <- st_as_sf(bham_gp_points,
                           coords = c("longitude", "latitude"),
                           crs = 4326)

# LSOAs
bham_lsoa21_boundaries <- read_sf("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/Shape files/boundaries-lsoa21-bsol.geojson") |>
  filter(local_authority_name == "Birmingham")


# IMD 2025 by LSOA
bham_imd25 <- read_csv("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/IMD_2025_England.csv") |> 
  janitor::clean_names() |> 
  filter(local_authority_district_name_2024 == "Birmingham") |> 
  select(lsoa_code_2021, index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as) |> 
  rename(imd25_decile = index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_percent_of_lso_as)

# join LSOA and IMD25 data
bham_imd25 <- bham_lsoa21_boundaries |> 
  left_join(bham_imd25,
            by = join_by("lsoa21cd" == "lsoa_code_2021"))

# import pcn/gp/locality lookup
bham_pcns <- read_excel("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/bsol_gp_pcn_lookup.xlsx",
                         skip = 5) |> 
  janitor::clean_names() |> 
  filter(!is.na(code)) |> 
  rename(locality_name = constituency_locality) |> 
  mutate(post_code = str_replace(post_code, "  ", " "))

# import postcode lookup to get geopoints for PCNs
postcode_lookup <- read_sf("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/postcodes_wmca_dec25.geojson") |> 
  janitor::clean_names() |> 
  select(postcode, local_authority_name, geometry)


# add geopoints to PCNs
bham_pcns <- postcode_lookup |> 
  right_join(bham_pcns,
            by = join_by(postcode == post_code))

# set up labels

label_districts <- paste0(bham_district22_boundaries$pcon22nm) |> 
  lapply(htmltools::HTML)

label_wards25 <- paste0(bham_ward25_boundaries$wd25nm) |> 
  lapply(htmltools::HTML)

# wrap ward names when longer than 10 characters
label_wards25_wrap <- gsub("\n", "<br>", paste0(str_wrap(bham_ward25_boundaries$wd25nm, 10))) |> 
  lapply(htmltools::HTML)

label_localities <- paste0(bham_locality_boundaries$locality_name) |> 
  lapply(htmltools::HTML)

label_GPs <- paste0(bham_gp_points$surgery_name)|> 
  lapply(htmltools::HTML)


# Set constants -----------------------------------------------------------

# set colours as variables
highlight_colour <- "red"
border_colour <- "black"
border_sub_colour <- "grey"
ward_text_colour <- "black"

# create a custom viridis colour palette
viridis_pal <- viridisLite::viridis(10, begin = 0.3, end = 1)

# set up fill palette for imd25
imd25_pal <- colorNumeric(viridis_pal,
                          domain = bham_imd25$imd25_decile,
                          reverse = T)

# ward_stroke_pal <- colorNumeric(colorRamp(c("grey", "white"),
#                                           interpolate = "spline"),
#                           domain = bham_imd25$imd25_decile,
#                           reverse = T)

# Maps --------------------------------------------------------------------
# 



leaflet(width = "100%", height = "600px") |> 
  # Define custom panes for strict ordering
  addMapPane("LAPane", zIndex = 200) |>       # LA boundary layer
  addMapPane("imdPane", zIndex = 300) |>       # IMD layer (bottom)
  addMapPane("wardsTextPane", zIndex = 350) |> # Wards
  addMapPane("wardsPane", zIndex = 400) |>   
  addMapPane("pconsTextPane", zIndex = 450) |>  # constituencies
  addMapPane("pconsPane", zIndex = 500) |>     
  addMapPane("districtsTextPane", zIndex = 550) |>
  addMapPane("districtsPane", zIndex = 600) |> # Districts
  addMapPane("localitiesTextPane", zIndex = 650) |>
  addMapPane("localitiesPane", zIndex = 700) |># Localities
  addMapPane("markersPane", zIndex = 800) |>   # GP markers
  addMapPane("clustersPane", zIndex = 2000) |>   # GP clusters (top)
  
  # Base tiles
  addTiles(options = tileOptions(opacity = 0.5)) |>
  
  # Birmingham LA boundaries (optional pane if needed)
  addPolygons(data = bham_la24_boundaries,
              color = border_colour,
              weight = 0,
              opacity = 0,
              fillOpacity = 0.7,
              fillColor = "white",
              options = pathOptions(pane = "LAPane")) |>
  
  # IMD 2025 layer (always bottom)
  addPolygons(data = bham_imd25,
              fillColor = ~imd25_pal(imd25_decile),
              fillOpacity = 0.7,
              color = ~imd25_pal(imd25_decile),
              weight = 1,
              opacity = 0.35,
              stroke = TRUE,
              options = pathOptions(pane = "imdPane"),
              group = "IMD 2025") |>
  
  # Wards - labels only
  addPolygons(data = bham_ward25_boundaries,
              color = border_sub_colour,
              weight = 0,
              opacity = 0,
              fillOpacity = 0,
              label = label_wards25_wrap,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              options = pathOptions(pane = "wardsTextPane"),
              group = "2025 Wards") |>
  
  # Wards - boundaries
  addPolygons(data = bham_ward25_boundaries,
              color = border_colour,
              weight = 2,
              opacity = 1,
              fillOpacity = 0,
              label = label_wards25,
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              options = pathOptions(pane = "wardsPane"),
              group = "2025 Wards") |>
  
  # Constituencies - labels only
  addPolygons(data = bham_pcon24_boundaries,
              color = border_sub_colour,
              weight = 0,
              opacity = 0,
              fillOpacity = 0,
              label = label_pcons24,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              options = pathOptions(pane = "pconsTextPane"),
              group = "2024 Constituencies") |>
  
  # Constituencies - boundaries
  addPolygons(data = bham_pcon24_boundaries,
              color = border_colour,
              weight = 2,
              opacity = 1,
              fillOpacity = 0,
              label = label_pcons24,
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              options = pathOptions(pane = "pconsPane"),
              group = "2024 Constituencies") |>
  
  # Districts - labels only
  addPolygons(data = bham_district22_boundaries,
              color = border_sub_colour,
              weight = 0,
              opacity = 0,
              fillOpacity = 0,
              label = label_districts,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              options = pathOptions(pane = "districtsTextPane"),
              group = "Districts") |>
  
  # Districts - boundaries
  addPolygons(data = bham_district22_boundaries,
              color = border_colour,
              weight = 2,
              opacity = 1,
              fillOpacity = 0,
              label = label_districts,
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              options = pathOptions(pane = "districtsPane"),
              group = "Districts") |>
  
  # Localities - labels only
  addPolygons(data = bham_locality_boundaries,
              color = border_sub_colour,
              weight = 0,
              opacity = 0,
              fillOpacity = 0,
              label = label_localities,
              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 1,
                                          style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                          textsize = "8px",
                                          direction = "center"),
              options = pathOptions(pane = "localitiesTextPane"),
              group = "Localities") |>
  
  # Localities - boundaries
  addPolygons(data = bham_locality_boundaries,
              color = border_colour,
              weight = 2,
              opacity = 1,
              fillOpacity = 0,
              label = label_localities,
              highlightOptions = highlightOptions(
                weight = 3,
                color = highlight_colour,
                bringToFront = TRUE),
              options = pathOptions(pane = "localitiesPane"),
              group = "Localities") |>
  
  # # GP markers (top)
  # addMarkers(data = bham_gp_points,
  #            label = label_GPs,
  #            group = "GP Surgeries",
  #            clusterOptions = markerClusterOptions(
  #              pane = "clustersPane",  # Keep clusters above labels
  #              iconCreateFunction = JS("
  #   function (cluster) {
  #     var count = cluster.getChildCount();
  #     
  #     // Dynamic size based on count
  #     var size = count < 10 ? 40 : (count < 20 ? 50 : 60);
  #     var innerSize = size * 0.7; // Inner circle smaller
  #     
  #     var color = '#D00070'; // Fixed color for all clusters
  #     
  #     return new L.DivIcon({
  #       html: '<div style=\"position:relative; width:' + size + 'px; height:' + size + 'px;\">' +
  #               '<div style=\"background-color:' + color + '; opacity:0.4; border-radius:50%; width:' + size + 'px; height:' + size + 'px; position:absolute; top:0; left:0;\"></div>' +
  #               '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:' + innerSize + 'px; height:' + innerSize + 'px; position:absolute; top:' + ((size - innerSize)/2) + 'px; left:' + ((size - innerSize)/2) + 'px; display:flex; align-items:center; justify-content:center; font-weight:bold;\">' + count + '</div>' +
  #             '</div>',
  #       className: 'marker-cluster',
  #       iconSize: [size, size],
  #       pane: 'clustersPane'
  #     });
  #   }
  # ")
  #            ),
  #            options = markerOptions(pane = "markersPane")) |>
  
  # Legend
  addLegend(data = bham_imd25,
            "bottomright", 
            pal = imd25_pal, 
            values = ~imd25_decile,
            title = "IMD 2025 Decile",
            labFormat = labelFormat(prefix = "Decile "),
            opacity = 0.7) |>
  
  # Layer control
  addLayersControl(
    baseGroups = c("2025 Wards", "2024 Constituencies", "Districts", "Localities"),
    overlayGroups = c("IMD 2025"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup("GP Surgeries")

# District/ward maps ------------------------------------------------------

# visualise interactions between districts and wards

# set up empty dataframes
district_wards_full <- data.frame()
district_wards <- data.frame()

# create list of districts
districts <- bham_district22_boundaries$pcon22nm

# loop through the list of districts
for (district in districts) {
  # clip wards inside district boundary
  temp_district_wards <- bham_district22_boundaries |> 
    filter(pcon22nm == district)|> 
    st_intersection(bham_ward25_boundaries) |> 
    mutate(district_name = district)
  
  # find out the area of each clipped ward
  temp_district_wards$area <- st_area(temp_district_wards)
  
  # pull out unclipped wards for this district
  temp_district_wards_full <- bham_ward25_boundaries |> 
    filter(wd25cd %in% temp_district_wards$wd25cd) |> 
    mutate(district_name = district)
  
  # find area of unclipped ward and add to clipped ward sf
  temp_district_wards$full_area <- st_area(temp_district_wards_full)
  
  # calulate what % of each ward is inside the boundary
  # then filter out wards with less than 1% area inside boundary
  temp_district_wards <- temp_district_wards |> 
    mutate(pct = area/full_area*100) |> 
    filter(as.numeric(pct) > 1)
  
  # recalculate unclipped wards now wards with <1% are excluded
 temp_district_wards_full <- bham_ward25_boundaries |> 
    filter(wd25cd %in% temp_district_wards$wd25cd) |> 
   mutate(district_name = district)
 
 # append temp dfs to main dfs
 district_wards_full <- rbind(district_wards_full,
                             temp_district_wards_full)

 district_wards <- rbind(district_wards,
                         temp_district_wards)
}

# district lsoa maps
district_lsoas <- data.frame()

# loop through each district
for (district in districts) {
  # clip wards inside district boundary
  temp_district_lsoas <- bham_district22_boundaries |> 
    filter(pcon22nm == district)|> 
    st_intersection(bham_imd25) |> 
    mutate(district_name = district)
  
  # bind to main df
  district_lsoas <- rbind(district_lsoas,
                          temp_district_lsoas)
}

# filter to get gps which fall within each district boundary
district_gp_points <- data.frame()

# loop through each district
for (district in districts) {
  # find out which GPs are contained within that district
  temp_district_gp_list <- bham_district22_boundaries |> 
    filter(pcon22nm == district) |> 
    st_contains(bham_gp_points)
  
  # extract from list
  temp_district_gp_list <- temp_district_gp_list[[1]]
  
  temp_district_gp_points <- bham_gp_points |> 
    filter(row_number() %in% temp_district_gp_list) |> 
    mutate(district_name = district)
  
  district_gp_points <- rbind(district_gp_points,
                              temp_district_gp_points)
}

# maps are plotted using make_district_ward_map() function

# output table showing wards, districts and %
district_wards |> 
  as_tibble() |> 
  select(wd25nm, pcon22nm, pct) |> 
  arrange(wd25nm) |> 
  mutate(pct = round(pct, 0),
         #pct = as.character(pct),
         pct = paste0(pct, "%")) |> 
  rename(`Ward name` = wd25nm,
         `District name` = pcon22nm,
         `% of ward in district` = pct) |> 
  datatable(rownames = F,
            filter = "top",
            options = list(pageLength = 10,
                           autoWidth = TRUE))

# Pcon/ward maps ----------------------------------------------------------

pcon_wards_full <- data.frame()
pcon_wards <- data.frame()

pcons <- bham_pcon24_boundaries$pcon24nm

for (pcon in pcons) {
  # clip wards inside pcon boundary
  temp_pcon_wards <- bham_pcon24_boundaries |> 
    filter(pcon24nm == pcon)|> 
    st_intersection(bham_ward25_boundaries) |> 
    mutate(pcon_name = pcon)
  
  # find out the area of each clipped ward
  temp_pcon_wards$area <- st_area(temp_pcon_wards)
  
  # pull out unclipped wards for this pcon
  temp_pcon_wards_full <- bham_ward25_boundaries |> 
    filter(wd25cd %in% temp_pcon_wards$wd25cd) |> 
    mutate(pcon_name = pcon)
  
  # find area of unclipped ward and add to clipped ward sf
  temp_pcon_wards$full_area <- st_area(temp_pcon_wards_full)
  
  # calulate what % of each ward is inside the boundary
  # then filter out wards with less than 1% area inside boundary
  temp_pcon_wards <- temp_pcon_wards |> 
    mutate(pct = area/full_area*100) |> 
    filter(as.numeric(pct) > 1)
  
  # recalculate unclipped wards now wards with <1% are excluded
  temp_pcon_wards_full <- bham_ward25_boundaries |> 
    filter(wd25cd %in% temp_pcon_wards$wd25cd) |> 
    mutate(pcon_name = pcon)
  
  # append temp dfs to main df
  pcon_wards_full <- rbind(pcon_wards_full,
                               temp_pcon_wards_full)
  
  pcon_wards <- rbind(pcon_wards,
                          temp_pcon_wards)
}

# pcon lsoa maps
pcon_lsoas <- data.frame()

for (pcon in pcons) {
  # clip wards inside pcon boundary
  temp_pcon_lsoas <- bham_pcon24_boundaries |> 
    filter(pcon24nm == pcon)|> 
    st_intersection(bham_imd25) |> 
    mutate(pcon_name = pcon)
  
  pcon_lsoas <- rbind(pcon_lsoas,
                          temp_pcon_lsoas)
}

# filter to get gps which fall within each pcon boundary
pcon_gp_points <- data.frame()

for (pcon in pcons) {
  temp_pcon_gp_list <- bham_pcon24_boundaries |> 
    filter(pcon24nm == pcon) |> 
    st_contains(bham_gp_points)
  
  temp_pcon_gp_list <- temp_pcon_gp_list[[1]]
  
  temp_pcon_gp_points <- bham_gp_points |> 
    filter(row_number() %in% temp_pcon_gp_list) |> 
    mutate(pcon_name = pcon)
  
  pcon_gp_points <- rbind(pcon_gp_points,
                              temp_pcon_gp_points)
}


# Locality/constituency maps ----------------------------------------------

locality_pcons_full <- data.frame()
locality_pcons <- data.frame()

sf_use_s2(FALSE)

localities <- bham_locality_boundaries$locality_name

bham_locality_boundaries <- bham_locality_boundaries |> 
  rename(locality_name = locality)

for (locality in localities) {
  # clip pcons inside district boundary
  temp_locality_pcons <- bham_locality_boundaries |> 
    filter(locality_name == locality)|> 
    st_intersection(bham_pcon24_boundaries) |> 
    mutate(locality_name = locality)
  
  # find out the area of each clipped pcon
  temp_locality_pcons$area <- st_area(temp_locality_pcons)
  
  # pull out unclipped pcons for this locality
  temp_locality_pcons_full <- bham_pcon24_boundaries |> 
    filter(pcon24nm %in% temp_locality_pcons$pcon24nm) |> 
    mutate(locality_name = locality)
  
  # find area of unclipped pcon and add to clipped pcon sf
  temp_locality_pcons$full_area <- st_area(temp_locality_pcons_full)
  
  # calulate what % of each pcon is inside the boundary
  # then filter out pcons with less than 1% area inside boundary
  temp_locality_pcons <- temp_locality_pcons |> 
    mutate(pct = area/full_area*100) |> 
    filter(as.numeric(pct) > 1)
  
  # recalculate unclipped pcons now pcons with <1% are excluded
  temp_locality_pcons_full <- bham_pcon24_boundaries |> 
    filter(pcon24nm %in% temp_locality_pcons$pcon24nm) |> 
    mutate(locality_name = locality)
  
  # append temp dfs to main df
  locality_pcons_full <- rbind(locality_pcons_full,
                               temp_locality_pcons_full)
  
  locality_pcons <- rbind(locality_pcons,
                          temp_locality_pcons)
}

pcon_text_colour <- "black"


# Locality/PCN maps --------------------------------------------------------

# get pcns in north locality only
north_data <- bham_pcns |>
  filter(locality_name == "North")

# set up labels
label_pcn_GPs <- paste0(north_data$prac_name, "<br>",
                        north_data$pcn, "<br>",
                        north_data$type)|> 
  lapply(htmltools::HTML)

# set up colour palette
pal <- colorFactor(
  palette = "Set2",   # or another palette name
  domain = north_data$pcn
)

# plot map of just PCNs in North locality, and North locality boundaries
leaflet(north_data) |> 
  addTiles(options = tileOptions(opacity = 0.5)) |>
  addPolygons(data = bham_locality_boundaries |> filter(locality_name == "North"),
              color = border_colour,
              weight = 4,
              opacity = 1,
              fillOpacity = 0.6,
              fillColor = "white") |>
  addCircleMarkers(color = ~pal(pcn),
                   fillColor = ~pal(pcn),
                   fillOpacity = 0.8,
                   radius = 6,
                   stroke = FALSE,
                   label = label_pcn_GPs) |> 
  addLegend(
    "bottomright",
    pal = pal,
    values = ~pcn,
    title = "PCN"
  )

# this is generalised to the function make_locality_pcn_map()

# setup to create table of localities and pcns

# set up empty df
locality_pcn_table <- data.frame()

# loop throgh list of localities
for (locality in localities){
  # find pcns belonging to a locality
  temp_locality_pcns <- bham_pcns |>
    filter(locality_name == locality)
  
  # create a matrix of whether each practice actually falls inside the locality boundaries (TRUE/FALSE)
  temp_locality_pcn_matrix <- bham_locality_boundaries |>
    filter(locality_name == locality) |> 
    st_contains(temp_locality_pcns,
                sparse = F)
  
  # use locality as row name
  row.names(temp_locality_pcn_matrix) <- locality
  
  # use practices as column names
  colnames(temp_locality_pcn_matrix) <- temp_locality_pcns$prac_name
  
  # convert to matrix and pivot longer so there are three columns:
  # locality, practice name and whether in locality (T/F)
  temp_locality_pcn_matrix <- temp_locality_pcn_matrix |> 
    as_tibble(rownames = "locality_name") |> 
    pivot_longer(cols = !locality_name,
                 names_to = "prac_name",
                 values_to = "in_locality")
  
  # bind this to the main df
  locality_pcn_table <- rbind(locality_pcn_table,
                              temp_locality_pcn_matrix)
}

# output full df as interactive table
locality_pcn_table |> 
  left_join(bham_pcns,
            by = c("locality_name", "prac_name")) |> 
  select(locality_name, pcn, prac_name, in_locality) |> 
  rename(`Locality` = locality_name,
         PCN = pcn,
         `Practice Name` = prac_name,
         `Falls inside assigned locality` = in_locality) |> 
  datatable(rownames = F,
            filter = "top",
            options = list(pageLength = 15,
                           autoWidth = TRUE))

# output df to show PCNs and % within locality
locality_pcn_table |> 
  left_join(bham_pcns,
            by = c("locality_name", "prac_name")) |> 
  select(locality_name, pcn, prac_name, in_locality) |> 
  group_by(locality_name, pcn, in_locality) |> 
  count() |> 
  #ungroup() |> 
  group_by(locality_name, pcn) |> 
  mutate(d = sum(n)) |> 
  filter(in_locality == F) |> 
  mutate(pct = n/d*100,
         label = paste0(round(pct, 1), "% (", n, " out of ", d, " practices)")) |> 
  select(locality_name, pcn, label) |> 
  rename(`Locality` = locality_name,
         PCN = pcn,
         `Practices falling outside assigned locality` = label) |> 
  datatable(rownames = F,
            filter = "top",
            options = list(pageLength = 15,
                           autoWidth = TRUE))

# plot localities against % of assigned practices which fall within boundary
locality_pcn_table |> 
  left_join(bham_pcns,
            by = c("locality_name", "prac_name")) |> 
  select(locality_name, pcn, prac_name, in_locality) |> 
  group_by(locality_name, in_locality) |> 
  count() |> 
  group_by(locality_name) |> 
  mutate(d = sum(n)) |> 
  filter(in_locality == TRUE) |> 
  mutate(pct = n/d*100) |> 
  select(locality_name, n, d, pct) |> 
  ggplot(aes(x = locality_name,
             y = pct)) +
  geom_col()
