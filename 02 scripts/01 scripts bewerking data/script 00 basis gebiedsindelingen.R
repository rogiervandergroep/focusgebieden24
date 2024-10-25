
#source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

library(sf)
library(tidyverse)

my_spatial_type    <- function(type, jaar, datum) {
  
  pad <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/amsterdam"

    st_read(glue::glue("{pad}/{jaar}/{type}-{jaar}-geo.json")) |>
    st_drop_geometry()|> 
    add_column(spatial_date = datum,
               spatial_type = type)  |>
    rename(spatial_code = code, 
           spatial_name = naam) |>
    select(spatial_code, spatial_name, spatial_date, spatial_type)
  
}

my_spatial_wg_type <- function(type, datum) {
  
  pad <- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/winkelgebieden/2024/winkelgebieden-2024-geo.json"
  
  st_read(pad) |>
    st_drop_geometry()|> 
    add_column(spatial_date = datum,
               spatial_type = type)  |>
    rename(spatial_code = code, 
           spatial_name = naam) |>
    select(spatial_code, spatial_name, spatial_date, spatial_type)
  
}



geo_df<- bind_rows(
  
  # nieuwe gebiedsindeling
  tibble(spatial_code = c("0363"), spatial_name = "Amsterdam", spatial_type = 'gemeente', spatial_date ='20220324'), 
  
  my_spatial_type("buurten"   , 2022,  '20220324'),
  my_spatial_type("wijken"    , 2022,  '20220324'),
  my_spatial_type("stadsdelen", 2022,  '20220324'),
  my_spatial_type("gebieden"  , 2022,  '20220324'),
  my_spatial_wg_type("winkelgebieden", "20220324"),
  
  # oude gebiedsindeling
  tibble(spatial_code = c("AMS"), spatial_name = "Amsterdam", spatial_type = 'gemeente', spatial_date = '20150101'),
  
  my_spatial_type("buurten"   , "2015-2020", '20150101'),
  my_spatial_type("wijken"    , "2015-2020", '20150101'),
  # my_spatial_type("stadsdelen", "2015-2020", '20150101'),
  my_spatial_type("gebieden"  , "2015-2020", '20150101') |>
    mutate(spatial_code=str_replace(spatial_code, "PX" , "DX"))
)



geo_list<- list(
  
  # nieuwe gebiedsindeling
  ams_nieuw       = tibble(spatial_code = c("0363"), spatial_name = "Amsterdam", spatial_type = 'gemeente', spatial_date ='20220324'), 
  
  buurten_nieuw   = my_spatial_type("buurten"   , 2022, '20220324'),
  wijken_nieuw    = my_spatial_type("wijken"    , 2022, '20220324'),
  sd_nieuw        = my_spatial_type("stadsdelen", 2022, '20220324'),
  gebieden_nieuw  = my_spatial_type("gebieden"  , 2022, '20220324'),
  winkelgeb_nieuw = my_spatial_wg_type("winkelgebieden", "20220324"),
  
  # oude gebiedsindeling
  ams_oud        = tibble(spatial_code = c("AMS"), spatial_name = "Amsterdam", spatial_type = 'gemeente', spatial_date = '20150101'),
  
  buurten_oud    = my_spatial_type("buurten"   , "2015-2020", '20150101'),
  wijken_oud     = my_spatial_type("wijken"    , "2015-2020", '20150101'),
  sd_oud         = my_spatial_type("stadsdelen", "2015-2020", '20150101'),
  gebieden_oud   = my_spatial_type("gebieden"  , "2015-2020", '20150101')
)

### koppelen stadsdelen aan winkelgebieden

