library(tidyverse)
library(writexl)

model_a_district <- district_wards |> 
  as_tibble() |> 
  select(wd25nm, pcon22nm, pct) |> 
  arrange(pcon22nm) |> 
  mutate(pct = round(pct, 0)) |> 
  rename(`Ward name` = wd25nm,
         `District name` = pcon22nm,
         `% of ward in district` = pct)

model_b_district <- pcon_wards |> 
  as_tibble() |> 
  select(wd25nm, pcon_name, pct) |> 
  arrange(pcon_name) |> 
  mutate(pct = round(pct, 0)) |> 
  rename(`Ward name` = wd25nm,
         `District name` = pcon_name,
         `% of ward in district` = pct)

model_c_district <- ward_district_locality_lookup |> 
  select(ward, district) |> 
  arrange(district) |> 
  mutate(pct = 100) |> 
  rename(`Ward name` = ward,
         `District name` = district,
         `% of ward in district` = pct)

model_d_district <- district_wards_best_fit |> 
  select(wd25nm, district_name) |> 
  arrange(district_name) |> 
  mutate(pct = 100) |> 
  rename(`Ward name` = wd25nm,
         `District name` = district_name,
         `% of ward in district` = pct)

model_a_locality <- locality_wards |> 
  as_tibble() |> 
  select(wd25nm, locality, pct) |> 
  arrange(locality) |> 
  mutate(pct = round(pct, 0)) |> 
  rename(`Ward name` = wd25nm,
         `Locality name` = locality,
         `% of ward in locality` = pct)

model_b_locality <- locality_pcon24_wards |> 
  as_tibble() |> 
  select(wd25nm, locality, pct) |> 
  arrange(locality) |> 
  mutate(pct = round(pct, 0)) |> 
  rename(`Ward name` = wd25nm,
         `Locality name` = locality,
         `% of ward in locality` = pct)

model_c_locality <- ward_district_locality_lookup |> 
  select(ward, locality) |> 
  arrange(locality) |> 
  mutate(pct = 100) |> 
  rename(`Ward name` = ward,
         `Locality name` = locality,
         `% of ward in locality` = pct)

model_d_locality <- district_wards_best_fit |> 
  left_join(ward_district_locality_lookup |> select(district, locality) |> distinct(district, .keep_all = T),
            by = join_by("district_name" == "district")) |> 
  select(wd25nm, locality) |> 
  arrange(locality) |> 
  mutate(pct = 100) |> 
  rename(`Ward name` = wd25nm,
         `Locality name` = locality,
         `% of ward in district` = pct)

model_table_list <- list(`Model A - Districts` = model_a_district,
                         `Model A - Localities` = model_a_locality,
                         `Model B - Districts` = model_b_district,
                         `Model B - Localities` = model_b_locality,
                         `Model C - Districts` = model_c_district,
                         `Model C - Localities` = model_c_locality,
                         `Model D - Districts` = model_d_district,
                         `Model D - Localities` = model_d_locality)

write_xlsx(model_table_list,
           path = "output/model lookup tables.xlsx")

# write new ward-up district models to geojson files
library(sf)
st_write(proposed_bham_districts, "model_c.geojson")
st_write(proposed_bham_districts_best_fit, "model_d.geojson")

