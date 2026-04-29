make_district_ward_map <- function(district, zoom) {
  # wrapped ward names for district map
  label_wards25_wrap_district <- district_wards |> 
    filter(district_name == district) |> 
    pull(wd25nm) |> 
    str_wrap(15)
  label_wards25_wrap_district<- gsub("\n", "<br>", label_wards25_wrap_district) |> 
    lapply(htmltools::HTML)
  
  # labels with % inside district for district map
  label_wards25_district <- district_wards |> 
    filter(district_name == district) |> 
    mutate(label = paste0(wd25nm, "<br>",
                          round(pct, 0), "% falls within ", district_name)) |> 
    pull(label) |> 
    lapply(htmltools::HTML)
  
  lat <- bham_district22_boundaries |> 
    filter(pcon22nm == district) |> 
    pull(lat)
  
  long <- bham_district22_boundaries |> 
    filter(pcon22nm == district) |> 
    pull(long)
  
  leaflet(width = "100%", height = "650px") |> 
    addMapPane("districtPane", zIndex = 200) |>       # LA boundary layer
    addMapPane("imdPane", zIndex = 300) |>       # IMD layer (bottom)
    addMapPane("wardsTextPane", zIndex = 350) |>
    addMapPane("wardsPane", zIndex = 400) |>     # Wards
    addMapPane("markersPane", zIndex = 700) |>   # GP markers
    addMapPane("clustersPane", zIndex = 2000) |>   # GP clusters (top)
    addTiles(options = tileOptions(opacity = 0.5)) |> 
    # display district of interest only
    addPolygons(data = bham_district22_boundaries |> filter(pcon22nm == district),
                color = border_colour,
                weight = 4,
                opacity = 1,
                fillOpacity = 0.7,
                fillColor = "white",
                options = pathOptions(pane = "districtPane")) |>
    # display IMD by LSOA for district of interest only
    addPolygons(data = district_lsoas |> filter(district_name == district),
                fillColor = ~imd25_pal(imd25_decile),
                fillOpacity = 0.7,
                color = ~imd25_pal(imd25_decile),
                weight = 1,
                opacity = 0.35,
                stroke = T,
                group = "IMD 2025",
                options = pathOptions(pane = "imdPane")) |>
    # full wards text only
    addPolygons(data = district_wards_full |> filter(district_name == district),
                color = border_colour,
                weight = 0,
                opacity = 0,
                fillOpacity = 0,
                fillColor = "white",
                label = label_wards25_wrap_district,
                labelOptions = labelOptions(noHide = T, textOnly = T, opacity = 1,
                                            style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                            textsize = "8px",
                                            direction = "center"),
                options = pathOptions(pane = "wardsTextPane"),
                group = "Full Wards") |>
    # full wards
    addPolygons(data = district_wards_full |> filter(district_name == district),
                color = border_colour,
                weight = 1,
                opacity = 1,
                fillOpacity = 0,
                fillColor = "white",
                #label = ~wd25nm,
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = highlight_colour,
                  bringToFront = TRUE,
                  sendToBack = F),
                label = label_wards25_district,
                options = pathOptions(pane = "wardsPane"),
                group = "Full Wards") |>
    addMarkers(data = district_gp_points |> filter(district_name == district),
               label = ~surgery_name,
               group = "GP Surgeries",
               options = pathOptions(pane = "markersPane"),
               clusterOptions = markerClusterOptions(
                 pane = "clustersPane",  # Keep clusters above labels
                 iconCreateFunction = JS("
    function (cluster) {
      var count = cluster.getChildCount();
      
      // Dynamic size based on count
      var size = count < 10 ? 40 : (count < 20 ? 50 : 60);
      var innerSize = size * 0.7; // Inner circle smaller
      
      var color = '#D00070'; // Fixed color for all clusters
      
      return new L.DivIcon({
        html: '<div style=\"position:relative; width:' + size + 'px; height:' + size + 'px;\">' +
                '<div style=\"background-color:' + color + '; opacity:0.4; border-radius:50%; width:' + size + 'px; height:' + size + 'px; position:absolute; top:0; left:0;\"></div>' +
                '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:' + innerSize + 'px; height:' + innerSize + 'px; position:absolute; top:' + ((size - innerSize)/2) + 'px; left:' + ((size - innerSize)/2) + 'px; display:flex; align-items:center; justify-content:center; font-weight:bold;\">' + count + '</div>' +
              '</div>',
        className: 'marker-cluster',
        iconSize: [size, size],
        pane: 'clustersPane'
      });
    }
  ")
               )) |>
    addLegend(data = district_lsoas,
              "bottomright", 
              pal = imd25_pal, 
              values = ~imd25_decile,
              title = "IMD 2025 Decile",
              labFormat = labelFormat(prefix = "Decile "),
              opacity = 0.5) |> 
    setView(lat = lat, lng = long, zoom = zoom) |> 
    addLayersControl(
      overlayGroups = c("GP Surgeries",
                        "IMD 2025")) |> 
    hideGroup("GP Surgeries")
  
}

make_district_ward_map("Hodge Hill", 13)

make_pcon_ward_map <- function(pcon, zoom) {
  # wrapped ward names for district map
  label_wards25_wrap_pcon <- pcon_wards |> 
    filter(pcon_name == pcon) |> 
    pull(wd25nm) |> 
    str_wrap(15)
  label_wards25_wrap_pcon<- gsub("\n", "<br>", label_wards25_wrap_pcon) |> 
    lapply(htmltools::HTML)
  
  # labels with % inside pcon for pcon map
  label_wards25_pcon <- pcon_wards |> 
    filter(pcon_name == pcon) |> 
    mutate(label = paste0(wd25nm, "<br>",
                          round(pct, 0), "% falls within ", pcon_name)) |> 
    pull(label) |> 
    lapply(htmltools::HTML)
  
  lat <- bham_pcon24_boundaries |> 
    filter(pcon24nm == pcon) |> 
    pull(lat)
  
  long <- bham_pcon24_boundaries |> 
    filter(pcon24nm == pcon) |> 
    pull(long)
  
  leaflet(width = "100%", height = "650px") |> 
    addMapPane("pconPane", zIndex = 200) |>       # LA boundary layer
    addMapPane("imdPane", zIndex = 300) |>       # IMD layer (bottom)
    addMapPane("wardsTextPane", zIndex = 350) |>
    addMapPane("wardsPane", zIndex = 400) |>     # Wards
    addMapPane("markersPane", zIndex = 700) |>   # GP markers
    addMapPane("clustersPane", zIndex = 2000) |>   # GP clusters (top)
    addTiles(options = tileOptions(opacity = 0.5)) |> 
    # display pcon of interest only
    addPolygons(data = bham_pcon24_boundaries |> filter(pcon24nm == pcon),
                color = border_colour,
                weight = 4,
                opacity = 1,
                fillOpacity = 0.7,
                fillColor = "white",
                options = pathOptions(pane = "pconPane")) |>
    # display IMD by LSOA for pcon of interest only
    addPolygons(data = pcon_lsoas |> filter(pcon_name == pcon),
                fillColor = ~imd25_pal(imd25_decile),
                fillOpacity = 0.7,
                color = ~imd25_pal(imd25_decile),
                weight = 1,
                opacity = 0.35,
                stroke = T,
                group = "IMD 2025",
                options = pathOptions(pane = "imdPane")) |>
    # full wards text only
    addPolygons(data = pcon_wards_full |> filter(pcon_name == pcon),
                color = border_colour,
                weight = 0,
                opacity = 0,
                fillOpacity = 0,
                fillColor = "white",
                label = label_wards25_wrap_pcon,
                labelOptions = labelOptions(noHide = T, textOnly = T, opacity = 1,
                                            style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                            textsize = "8px",
                                            direction = "center"),
                options = pathOptions(pane = "wardsTextPane"),
                group = "Full Wards") |>
    # full wards
    addPolygons(data = pcon_wards_full |> filter(pcon_name == pcon),
                color = border_colour,
                weight = 1,
                opacity = 1,
                fillOpacity = 0,
                fillColor = "white",
                #label = ~wd25nm,
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = highlight_colour,
                  bringToFront = TRUE,
                  sendToBack = F),
                label = label_wards25_pcon,
                options = pathOptions(pane = "wardsPane"),
                group = "Full Wards") |>
    addMarkers(data = pcon_gp_points |> filter(pcon_name == pcon),
               label = ~surgery_name,
               group = "GP Surgeries",
               options = pathOptions(pane = "markersPane"),
               clusterOptions = markerClusterOptions(
                 pane = "clustersPane",  # Keep clusters above labels
                 iconCreateFunction = JS("
    function (cluster) {
      var count = cluster.getChildCount();
      
      // Dynamic size based on count
      var size = count < 10 ? 40 : (count < 20 ? 50 : 60);
      var innerSize = size * 0.7; // Inner circle smaller
      
      var color = '#D00070'; // Fixed color for all clusters
      
      return new L.DivIcon({
        html: '<div style=\"position:relative; width:' + size + 'px; height:' + size + 'px;\">' +
                '<div style=\"background-color:' + color + '; opacity:0.4; border-radius:50%; width:' + size + 'px; height:' + size + 'px; position:absolute; top:0; left:0;\"></div>' +
                '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:' + innerSize + 'px; height:' + innerSize + 'px; position:absolute; top:' + ((size - innerSize)/2) + 'px; left:' + ((size - innerSize)/2) + 'px; display:flex; align-items:center; justify-content:center; font-weight:bold;\">' + count + '</div>' +
              '</div>',
        className: 'marker-cluster',
        iconSize: [size, size],
        pane: 'clustersPane'
      });
    }
  ")
               )) |>
    addLegend(data = pcon_lsoas,
              "bottomright", 
              pal = imd25_pal, 
              values = ~imd25_decile,
              title = "IMD 2025 Decile",
              labFormat = labelFormat(prefix = "Decile "),
              opacity = 0.5) |> 
    setView(lat = lat, lng = long, zoom = zoom) |> 
    addLayersControl(
      overlayGroups = c("GP Surgeries",
                        "IMD 2025")) |> 
    hideGroup("GP Surgeries")
  
}

make_pcon_ward_map("Edgbaston", 13)

make_locality_pcon_map <- function(loc) {
  # wrapped pcon names for district map
  label_pcons24_wrap_locality <- locality_pcons |> 
    filter(locality == loc) |> 
    pull(pcon24nm) |> 
    str_wrap(15)
  label_pcons24_wrap_locality<- gsub("\n", "<br>", label_pcons24_wrap_locality) |> 
    lapply(htmltools::HTML)
  
  # labels with % inside locality for locality map
  label_pcons24_locality <- locality_pcons |> 
    filter(locality == loc) |> 
    mutate(label = paste0(pcon24nm, " constituency", "<br>",
                          round(pct, 0), "% falls within ", locality, " locality")) |> 
    pull(label) |> 
    lapply(htmltools::HTML)
  
  # gp labels for locality map
  
  leaflet(width = "100%", height = "650px") |> 
    addMapPane("localityPane", zIndex = 200) |>       # LA boundary layer
    addMapPane("imdPane", zIndex = 300) |>       # IMD layer (bottom)
    addMapPane("pconsTextPane", zIndex = 350) |>
    addMapPane("pconsPane", zIndex = 400) |>     # pcons
    addMapPane("markersPane", zIndex = 700) |>   # GP markers
    addMapPane("clustersPane", zIndex = 2000) |>   # GP clusters (top)
    addTiles(options = tileOptions(opacity = 0.5)) |> 
    # display locality of interest only
    addPolygons(data = bham_locality_boundaries |> filter(locality == loc),
                color = border_colour,
                weight = 4,
                opacity = 1,
                fillOpacity = 0.6,
                fillColor = "white",
                options = pathOptions(pane = "localityPane")) |>
    # display IMD by LSOA for district of interest only
    addPolygons(data = locality_lsoas |> filter(locality == loc),
                fillColor = ~imd25_pal(imd25_decile),
                fillOpacity = 0.7,
                color = ~imd25_pal(imd25_decile),
                weight = 1,
                opacity = 0.2,
                stroke = F,
                options = pathOptions(pane = "imdPane"),
                group = "IMD 2025") |>
    # full pcons text only
    addPolygons(data = locality_pcons_full |> filter(locality == loc),
                color = border_colour,
                weight = 0,
                opacity = 0,
                fillOpacity = 0,
                fillColor = "white",
                label = label_pcons24_wrap_locality,
                labelOptions = labelOptions(noHide = T, textOnly = T, opacity = 1,
                                            style = list("font-family" ="Arial", "font-weight" = "bold", "color" = pcon_text_colour),
                                            textsize = "8px",
                                            direction = "center"),
                options = pathOptions(pane = "pconsTextPane"),
                group = "Full pcons") |>
    # full pcons
    addPolygons(data = locality_pcons_full |> filter(locality == loc),
                color = border_colour,
                weight = 1,
                opacity = 1,
                fillOpacity = 0,
                fillColor = "white",
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = highlight_colour,
                  bringToFront = TRUE,
                  sendToBack = F),
                label = label_pcons24_locality,
                options = pathOptions(pane = "pconsPane"),
                group = "Full pcons") |>
    addMarkers(data = locality_gp_points |> filter(locality == loc),
               label = ~surgery_name,
               group = "GP Surgeries",
               options = pathOptions(pane = "markersPane"),
               clusterOptions = markerClusterOptions(
                 pane = "clustersPane",  # Keep clusters above labels
                 iconCreateFunction = JS("
    function (cluster) {
      var count = cluster.getChildCount();
      
      // Dynamic size based on count
      var size = count < 10 ? 40 : (count < 20 ? 50 : 60);
      var innerSize = size * 0.7; // Inner circle smaller
      
      var color = '#D00070'; // Fixed color for all clusters
      
      return new L.DivIcon({
        html: '<div style=\"position:relative; width:' + size + 'px; height:' + size + 'px;\">' +
                '<div style=\"background-color:' + color + '; opacity:0.4; border-radius:50%; width:' + size + 'px; height:' + size + 'px; position:absolute; top:0; left:0;\"></div>' +
                '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:' + innerSize + 'px; height:' + innerSize + 'px; position:absolute; top:' + ((size - innerSize)/2) + 'px; left:' + ((size - innerSize)/2) + 'px; display:flex; align-items:center; justify-content:center; font-weight:bold;\">' + count + '</div>' +
              '</div>',
        className: 'marker-cluster',
        iconSize: [size, size],
        pane: 'clustersPane'
      });
    }
  ")
               )) |>
    addLegend(data = locality_lsoas,
              "bottomright", 
              pal = imd25_pal, 
              values = ~imd25_decile,
              title = "IMD 2025 Decile",
              labFormat = labelFormat(prefix = "Decile "),
              opacity = 0.5) |> 
    #setView(lat = 52.4491, lng = -1.96463, zoom = 13) |> 
    addLayersControl(
      overlayGroups = c("GP Surgeries",
                        "IMD 2025")) |> 
    hideGroup("GP Surgeries")
}
make_locality_pcon_map("Central")

make_locality_ward_map <- function(loc) {
  # wrapped ward names for district map
  label_wards25_wrap_locality <- locality_wards |> 
    filter(locality == loc) |> 
    pull(wd25nm) |> 
    str_wrap(15)
  label_wards25_wrap_locality<- gsub("\n", "<br>", label_wards25_wrap_locality) |> 
    lapply(htmltools::HTML)
  
  # labels with % inside locality for locality map
  label_wards25_locality <- locality_wards |> 
    filter(locality == loc) |> 
    mutate(label = paste0(wd25nm, "<br>",
                          round(pct, 0), "% falls within ", locality, " locality")) |> 
    pull(label) |> 
    lapply(htmltools::HTML)
  
  # gp labels for locality map
  
  leaflet(width = "100%", height = "650px") |> 
    addMapPane("localityPane", zIndex = 200) |>       # LA boundary layer
    addMapPane("imdPane", zIndex = 300) |>       # IMD layer (bottom)
    addMapPane("wardsTextPane", zIndex = 350) |>
    addMapPane("wardsPane", zIndex = 400) |>     # Wards
    addMapPane("markersPane", zIndex = 700) |>   # GP markers
    addMapPane("clustersPane", zIndex = 2000) |>   # GP clusters (top)
    addTiles(options = tileOptions(opacity = 0.5)) |> 
    # display locality of interest only
    addPolygons(data = bham_locality_boundaries |> filter(locality == loc),
                color = border_colour,
                weight = 4,
                opacity = 1,
                fillOpacity = 0.6,
                fillColor = "white",
                options = pathOptions(pane = "localityPane")) |>
    # display IMD by LSOA for district of interest only
    addPolygons(data = locality_lsoas |> filter(locality == loc),
                fillColor = ~imd25_pal(imd25_decile),
                fillOpacity = 0.7,
                color = ~imd25_pal(imd25_decile),
                weight = 1,
                opacity = 0.2,
                stroke = F,
                options = pathOptions(pane = "imdPane"),
                group = "IMD 2025") |>
    # full wards text only
    addPolygons(data = locality_wards_full |> filter(locality == loc),
                color = border_colour,
                weight = 0,
                opacity = 0,
                fillOpacity = 0,
                fillColor = "white",
                label = label_wards25_wrap_locality,
                labelOptions = labelOptions(noHide = T, textOnly = T, opacity = 1,
                                            style = list("font-family" ="Arial", "font-weight" = "bold", "color" = ward_text_colour),
                                            textsize = "8px",
                                            direction = "center"),
                options = pathOptions(pane = "wardsTextPane"),
                group = "Full Wards") |>
    # full wards
    addPolygons(data = locality_wards_full |> filter(locality == loc),
                color = border_colour,
                weight = 1,
                opacity = 1,
                fillOpacity = 0,
                fillColor = "white",
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = highlight_colour,
                  bringToFront = TRUE,
                  sendToBack = F),
                label = label_wards25_locality,
                options = pathOptions(pane = "wardsPane"),
                group = "Full Wards") |>
    addMarkers(data = locality_gp_points |> filter(locality == loc),
               label = ~surgery_name,
               group = "GP Surgeries",
               options = pathOptions(pane = "markersPane"),
               clusterOptions = markerClusterOptions(
                 pane = "clustersPane",  # Keep clusters above labels
                 iconCreateFunction = JS("
    function (cluster) {
      var count = cluster.getChildCount();
      
      // Dynamic size based on count
      var size = count < 10 ? 40 : (count < 20 ? 50 : 60);
      var innerSize = size * 0.7; // Inner circle smaller
      
      var color = '#D00070'; // Fixed color for all clusters
      
      return new L.DivIcon({
        html: '<div style=\"position:relative; width:' + size + 'px; height:' + size + 'px;\">' +
                '<div style=\"background-color:' + color + '; opacity:0.4; border-radius:50%; width:' + size + 'px; height:' + size + 'px; position:absolute; top:0; left:0;\"></div>' +
                '<div style=\"background-color:' + color + '; color:white; border-radius:50%; width:' + innerSize + 'px; height:' + innerSize + 'px; position:absolute; top:' + ((size - innerSize)/2) + 'px; left:' + ((size - innerSize)/2) + 'px; display:flex; align-items:center; justify-content:center; font-weight:bold;\">' + count + '</div>' +
              '</div>',
        className: 'marker-cluster',
        iconSize: [size, size],
        pane: 'clustersPane'
      });
    }
  ")
               )) |>
    addLegend(data = locality_lsoas,
              "bottomright", 
              pal = imd25_pal, 
              values = ~imd25_decile,
              title = "IMD 2025 Decile",
              labFormat = labelFormat(prefix = "Decile "),
              opacity = 0.5) |> 
    #setView(lat = 52.4491, lng = -1.96463, zoom = 13) |> 
    addLayersControl(
      overlayGroups = c("GP Surgeries",
                        "IMD 2025")) |> 
    hideGroup("GP Surgeries")
}

make_locality_pcn_map <- function(loc){
  locality_data <- bham_pcns |>
    filter(locality == loc)
  
  label_pcn_GPs <- paste0(locality_data$prac_name, "<br>",
                          locality_data$pcn, "<br>",
                          locality_data$type)|> 
    lapply(htmltools::HTML)
  
  pal <- colorFactor(
    palette = "Dark2",   # or another palette name
    domain = locality_data$pcn
  )
  
  leaflet(locality_data,
          width = "100%", height = "650px") |> 
    addTiles(options = tileOptions(opacity = 0.5)) |>
    addPolygons(data = bham_locality_boundaries |> filter(locality == loc),
                color = border_colour,
                weight = 4,
                opacity = 1,
                fillOpacity = 0.6,
                fillColor = "white") |>
    addCircleMarkers(color = ~pal(pcn),
                     fillColor = ~pal(pcn),
                     fillOpacity = 1,
                     radius = 8,
                     stroke = FALSE,
                     label = label_pcn_GPs) |> 
    addLegend(
      "bottomright",
      pal = pal,
      values = ~pcn,
      title = "PCN"
    )
}
make_locality_pcn_map("North")
