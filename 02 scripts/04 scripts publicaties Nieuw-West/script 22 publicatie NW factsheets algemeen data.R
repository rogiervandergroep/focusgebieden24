
library(tidyverse)
library(sf)
library(openxlsx)
library(patchwork)
library(svglite)

# inlezen bestand noord
data_nw_ruw <- read_rds("03 tussentijds/BBGA_data_nw.rds")

# lijstje met kernindicatoren
kernindicatoren <- data_nw_ruw |>
  ungroup()|>
  filter(kernindicator_nw %in% c("basis", "primair")) |>
  select(variabele)|>
  distinct()|>
  pull()

kernind_df <- data_nw_ruw |>
  ungroup()|>
  filter(kernindicator_nw %in% c("basis", "primair")) |>
  select(indicator_sd, variabele, kernindicator_nw, thema_nw_kleur, thema_nw_label)|>
  distinct()


### stylesheet for exceltabellen ---
source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

my_temporal_filter <- function(x) {
  
  x |>
    
    mutate(
      value = round(value, 1),
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

### data voor factsheet
tabel_eenmeting <- data_nw_ruw |>
  select(- c("inw_aantal_24", "focusbuurt" ))|>
  filter(
    !is.na(value),
    tweedeling_def == 'totaal',
    !is.na(thema_nw_label),
    spatial_code %in% c("F", "0363"))|>
  my_temporal_filter()|>
  
  mutate(indicator_sd = case_when(
    kernindicator_nw == 'primair' ~ glue::glue("{indicator_sd} (kernindicator)"),
    TRUE ~ indicator_sd)) |>
  
  arrange(kernindicator_nw, thema_nw_kleur, thema_nw_label )|>
  select(- kernindicator_nw)|>
  ungroup()|>
 
  select(all_of(c( 
    "indicator_sd", "thema_nw_kleur", "thema_nw_label", "besch_jaren", 
    "0-meting Nieuw-West", "1-meting Nieuw-West",
    "0-meting Amsterdam", "1-meting Amsterdam")))|>
  rename(
    'naam indicator' = 'indicator_sd')



list_eenmeting <- split(tabel_eenmeting, f= tabel_eenmeting$thema_nw_kleur)|>
  map(\(x) select(x, -thema_nw_kleur) )

source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

naam_focusgebied         <- 'Samen Nieuw-West'
naam_stadsdeel           <- 'Nieuw-West'
naam_monitor             <- 'Outcomemonitor Samen Nieuw-West'
naam_website_focusgebied <- 'samennieuw-west.nl'

# Create a new workbook voor sd tabel
wb_nw <- my_style_sheet(
  
  x = list_eenmeting, 
  col_dark_bl  = c(6,7), #kolommen donkerblauw
  col_light_bl = c(4,5),  #kolommen lichtblauw
  colofon_type = colofon_eenmeting
  
)

# Save the workbook to a file
saveWorkbook(wb_nw, glue::glue("04 tabellen/03 tabellen nieuwwest/tabel eenmeting { naam_focusgebied } nov 2024.xlsx"), overwrite = TRUE)
saveWorkbook(wb_nw, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel eenmeting { naam_focusgebied } nov 2024.xlsx"), overwrite = TRUE)




################################
# DATA alleen voor focusgebieden
################################

### data voor factsheet
tabel_eenmeting_focusbuurten <- data_nw_ruw |>
  select(- c("inw_aantal_24"))|>
  filter(
    !is.na(value),
    tweedeling_def == 'totaal',
    !is.na(thema_nw_label),
    (focusbuurt == TRUE | spatial_type == 'stadsdelen')
    )|>
  
  mutate(
    value = round(value, 1),
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
  
  select(-c(spatial_type, focusbuurt, temporal_date, tweedeling_def))|>
  
  arrange(meting) |>
  
  mutate(indicator_sd = case_when(
    kernindicator_nw == 'primair' ~ glue::glue("{indicator_sd} (kernindicator)"),
    TRUE ~ indicator_sd)) |>
  
  arrange(kernindicator_nw, thema_nw_kleur, thema_nw_label, variabele )|>
  ungroup()|>
  select(- c("kernindicator_nw", "variabele"))|>
  pivot_wider(names_from = meting, values_from = value)|>
  rename('naam indicator' = 'indicator_sd')


list_eenmeting_fb <- split(tabel_eenmeting_focusbuurten, f = tabel_eenmeting_focusbuurten$thema_nw_kleur)|>
  map(\(x) select(x, - thema_nw_kleur) )

source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

naam_focusgebied         <- 'Samen Nieuw-West'
naam_stadsdeel           <- 'Nieuw-West'
naam_monitor             <- 'Outcomemonitor Samen Nieuw-West'
naam_website_focusgebied <- 'samennieuw-west.nl'

# Create a new workbook voor sd tabel
wb_nw <- my_style_sheet(
  
  x = list_eenmeting_fb, 
  col_dark_bl  = c(6,7), #kolommen donkerblauw
  col_light_bl = c(4,5),  #kolommen lichtblauw
  colofon_type = colofon_eenmeting
  
)

# Save the workbook to a file
saveWorkbook(wb_nw, glue::glue("04 tabellen/03 tabellen nieuwwest/tabel eenmeting { naam_focusgebied } fb jan 25.xlsx"), overwrite = TRUE)
saveWorkbook(wb_nw, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel eenmeting { naam_focusgebied } fb jan 25.xlsx"), overwrite = TRUE)




# FA Sloterdijk Nieuw-West
# FB FC FD FE Geuzenveld Slotermeer
# FF FH  FJ FK Osdorp
# FL FM FN FP Slotervaart
# FG FQ De Aker SLoten Nieuw-Sloten
