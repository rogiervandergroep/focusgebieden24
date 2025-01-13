
library(tidyverse)
library(sf)
library(openxlsx)
library(patchwork)
library(svglite)


# inlezen bestand zuidoost
data_zo_ruw <- read_rds("03 tussentijds/BBGA_data_zo.rds")

# lijstje met indicatoren die we rekeken tot de kernindicatoren
kernindicatoren <- data_zo_ruw |>
  ungroup()|>
  filter(kernindicator_zo == TRUE)|>
  select(variabele)|>
  distinct()|>
  pull()

kernind_df <- data_zo_ruw |>
  ungroup()|>
  filter(kernindicator_zo == TRUE)|>
  select(thema_zuidoost_label, indicator_sd, variabele)|>
  distinct()


### data voor factsheet
data_zo_def <- data_zo_ruw |>
  filter(
    !is.na(value)
    )
  
### data_zo_def wordt gebruikt voor de figuren

### BASISTABELLEN ---

# met de functie my_temporal_filter worden per indicator twee waarden geselecteerd
# namelijk de waarde van het laatste jaar en van het jaar 2020 (of rond 2020)

my_temporal_filter <- function(x) {
  
  x |>
  
    mutate(
      temporal_date = as.numeric(temporal_date))|>
    
    group_by(variabele)|>
    
    filter(
      temporal_date == max(temporal_date) | 
        
        if      (2020 %in% temporal_date) {
          temporal_date == 2020 
          
        } else if (2021 %in% temporal_date) {
          temporal_date == 2021 
          
        } else if (2019 %in% temporal_date ) {
          temporal_date == 2019 
          
        } else temporal_date == 2022 )|>
   
    mutate(
      meting = case_when(
        temporal_date == max(temporal_date) ~ '1-meting',
        temporal_date != max(temporal_date) ~ '0-meting'),
      meting = factor(
        meting, levels = c("0-meting", "1-meting")),
      besch_jaren = paste(unique(temporal_date),  collapse = " | "))|>
    
    select(-c(spatial_type, spatial_code, temporal_date, tweedeling_def))|>
    
    arrange(meting)|>
    
    pivot_wider(
      names_from  = c(meting,spatial_name), 
      values_from = value, 
      names_sep   = " ")
}


# tabel op sd en gem niveau
tabel_eenmeting <- data_zo_def |>
  
  filter(
    tweedeling_def == 'totaal',
    !is.na(thema_zuidoost_label),
    spatial_type %in% c("stadsdelen", "gemeente"))|>
  
  my_temporal_filter()|>

  mutate(
    thema_zuidoost_label = str_replace(
      thema_zuidoost_label, 'SD3 Openbare ruimte en mobiliteit', 'SD3 ruimte en mob.'))|>
  
  ungroup()|>
  
  select(
    thema_zuidoost_label, indicator_sd,	
    kernindicator_zo,  besch_jaren,	
    `0-meting Zuidoost`, `1-meting Zuidoost`,	
    `0-meting Amsterdam`,`1-meting Amsterdam`)|>
  
  set_names(c('thema_zuidoost_label', 'naam indicator', "kernindicator", 'peilmomenten', 	
              'ZO 0-meting',	'ZO 1-meting', 'AMS 0-meting', 'AMS 1-meting')) |>
  
  mutate(`naam indicator`= case_when(
    kernindicator == TRUE ~ glue::glue("{`naam indicator`} (kernindicator)"),
    TRUE                  ~ `naam indicator`))|>
  
  arrange(desc(kernindicator))|>
  select(-kernindicator)

# maak een lijst van de tabel 
list_eenmeting <- split(
  tabel_eenmeting, 
  f = tabel_eenmeting$thema_zuidoost_label)|>
  map(\(x) select(x, -thema_zuidoost_label))



  
# met dit script wordt een automatische tabel in excel gegenereerd 
source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

naam_focusgebied         <- 'Masterplan Zuidoost'
naam_stadsdeel           <- 'Zuidoost'
naam_monitor             <- 'Outcomemonitor Masterplan Zuidoost'
naam_website_focusgebied <- 'www.zoiszuidoost.nl'


# Create a new workbook voor sd tabel
wb_zuidoost<- my_style_sheet(
  
  x = list_eenmeting, 
  col_dark_bl  = c(5,6), #kolommen donkerblauw
  col_light_bl = c(3,4),  #kolommen lichtblauw
  colofon_type = colofon_eenmeting
  
)

# Save the workbook to a file
saveWorkbook(wb_zuidoost, glue::glue("04 tabellen/01 tabellen zuidoost/tabel eenmeting { naam_focusgebied } nov 2024.xlsx"), overwrite = TRUE)
saveWorkbook(wb_zuidoost, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel eenmeting { naam_focusgebied } nov 2024.xlsx"), overwrite = TRUE)

### tabel op gebied-niveau ---

sel_beter <- c(
  
  "thema_zuidoost_label","kernindicator_zo","indicator_sd",            
  "variabele","thema_zuidoost_code","besch_jaren", 
  
  "0-meting Bijlmer-West","1-meting Bijlmer-West",  
  "0-meting Bijlmer-Centrum","1-meting Bijlmer-Centrum", 
  "0-meting Bijlmer-Oost","1-meting Bijlmer-Oost",  
  "0-meting Gaasperdam","1-meting Gaasperdam")


tabel_geb_eenmeting <- data_zo_def |>
  
  filter(
    tweedeling_def == 'totaal',
    !is.na(thema_zuidoost_label),
    spatial_type %in% c("gebieden"))|>
  
  mutate(spatial_name = case_when(
    spatial_name == 'Bijlmer Centrum'       ~ 'Bijlmer-Centrum',
    spatial_name == 'Bijlmer Oost'          ~ 'Bijlmer-Oost',
    spatial_name == 'Gaasperdam / Driemond' ~ 'Gaasperdam',
    TRUE ~  spatial_name)
  )|>
  
  my_temporal_filter()|>
  
  mutate(
    thema_zuidoost_label = str_replace(
      thema_zuidoost_label, 'SD3 Openbare ruimte en mobiliteit', 'SD3 ruimte en mob.'))|>
  
  ungroup()|>
  select(all_of(sel_beter))|>
  arrange(desc(kernindicator_zo))|>
  select(-kernindicator_zo)

# maak een lijst van de tabel 
list_geb_eenmeting <- split(
  tabel_geb_eenmeting, 
  f = tabel_geb_eenmeting$thema_zuidoost_label)|>
  map(\(x) select(x, -c("thema_zuidoost_label", "thema_zuidoost_code")))




# met dit script wordt een automatische tabel in excel gegenereerd 
source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

naam_focusgebied         <- 'Masterplan Zuidoost'
naam_stadsdeel           <- 'Zuidoost'
naam_monitor             <- 'Outcomemonitor Masterplan Zuidoost'
naam_website_focusgebied <- 'www.zoiszuidoost.nl'


# Create a new workbook voor sd tabel
wb_geb_zuidoost<- my_style_sheet(
  
  x = list_geb_eenmeting, 
  col_dark_bl  = c(6,7,10,11), #kolommen donkerblauw
  col_light_bl = c(4,5,8,9),  #kolommen lichtblauw
  colofon_type = colofon_eenmeting
  
)

# Save the workbook to a file
saveWorkbook(wb_geb_zuidoost, glue::glue("04 tabellen/01 tabellen zuidoost/tabel eenmeting { naam_focusgebied } gebieden nov 2024.xlsx"), overwrite = TRUE)
saveWorkbook(wb_geb_zuidoost, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel eenmeting { naam_focusgebied } gebieden nov 2024.xlsx"), overwrite = TRUE)


