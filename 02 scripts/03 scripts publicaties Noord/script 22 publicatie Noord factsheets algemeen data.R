
library(tidyverse)
library(sf)
library(openxlsx)

library(patchwork)
library(svglite)

# inlezen bestand noord
data_noord_ruw <- read_rds("03 tussentijds/BBGA_data_noord.rds")

# lijstje met kernindicatoren
kernindicatoren<- data_noord_ruw |>
  ungroup()|>
  filter(kernindicator_noord ==TRUE) |>
  select(variabele)|>
  distinct()|>
  pull()

kernind_df<- data_noord_ruw |>
  ungroup()|>
  filter(kernindicator_noord == TRUE) |>
  select(variabele, thema_noord_eenmeting)|>
  distinct()

data_noord_def <- data_noord_ruw |>
  filter(
    !is.na(value),
    tweedeling_def == 'totaal',
    (spatial_name %in% c("Amsterdam", "Noord") | focusbuurt == TRUE | spatial_code == 'NOVERIG')
  )

### duidelijke tabel voor op de website ---

volgorde <- c(
  
  "thema_noord_eenmeting" , "indicator_sd"  , "besch_jaren", 
  "0-meting gemiddelde focusbuurten","1-meting gemiddelde focusbuurten",                            
  "0-meting gemiddelde overige buurten", "1-meting gemiddelde overige buurten",
  "0-meting Noord" , "1-meting Noord",
  "0-meting Amsterdam", "1-meting Amsterdam"
  )  

volgorde_beter <- c(
  
  "thema_noord_eenmeting" , "naam indicator"  , "peilmomenten",
  "0-meting focusbuurten","1-meting focusbuurten",                            
  "0-meting ov. buurten", "1-meting ov. buurten",
  "0-meting Noord", "1-meting Noord",
  "0-meting AMS", "1-meting AMS"
  )   

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

tabel_eenmeting <- data_noord_def |>
  select(-c(aanpak_noord, focusbuurt))|>
  filter(
    tweedeling_def == 'totaal',
    !is.na(thema_noord_eenmeting),
    spatial_code %in% c("N", "0363", "NFOCUS", "NOVERIG"))|>
  my_temporal_filter()|>
  mutate(indicator_sd= case_when(
    kernindicator_noord == TRUE ~ glue::glue("{indicator_sd} (kernindicator)"),
    TRUE ~ indicator_sd)) |>
  select(-kernindicator_noord)|>
  ungroup()|>
  select(all_of(volgorde))|>
  set_names(volgorde_beter)|>
  filter(thema_noord_eenmeting != 'basis')

list_eenmeting <- split(tabel_eenmeting, f= tabel_eenmeting$thema_noord_eenmeting)|>
  map(\(x) select(x, - thema_noord_eenmeting)) 

source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

naam_focusgebied         <- 'Aanpak Noord'
naam_stadsdeel           <- 'Noord'
naam_monitor             <- 'Outcomemonitor Aanpak Noord'
naam_website_focusgebied <- 'www.aanpaknoord.nl'


# Create a new workbook voor sd tabel
wb_noord <- my_style_sheet(
  
  x = list_eenmeting, 
  col_dark_bl  = c(9,10), #kolommen donkerblauw
  col_light_bl = c(3,4,7,8),  #kolommen lichtblauw
  colofon_type = colofon_eenmeting
  
)

# Save the workbook to a file
saveWorkbook(wb_noord, glue::glue("04 tabellen/02 tabellen noord/tabel eenmeting { naam_focusgebied } nov 2024.xlsx"), overwrite = TRUE)
saveWorkbook(wb_noord, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel eenmeting { naam_focusgebied } nov 2024.xlsx"), overwrite = TRUE)





