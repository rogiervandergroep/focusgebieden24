
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
  
  # # ranking voor volgorde lijnen in figuur
  # mutate(spatial_type_code = case_when(
  #   spatial_type == 'buurten'    ~ 5,
  #   spatial_type == 'wijken'     ~ 4,
  #   spatial_type == 'gebieden'   ~ 3,
  #   spatial_type == 'stadsdelen' ~ 2,
  #   spatial_type == 'gemeente'   ~ 1)
  #   )|>
  # 
  # mutate(facet_buurt = case_when(
  #   spatial_type == 'buurten'  ~ 'buurten',
  #   spatial_type == 'wijken'   ~ 'wijken',
  #   TRUE ~  'gebieden, gemeente, stadsdeel')
  #   )

### BASISTABELLEN ---

basis_tabel_zo <- list(

  # nulmeting en eenmeting stadsdeel en stad
  tabel_sd_ams <- data_zo_def |>
    filter(
      tweedeling_def == 'totaal',
      !is.na(thema_zuidoost_label),
      spatial_type %in% c("stadsdelen", "gemeente"))|>
    mutate(temporal_date = as.numeric(temporal_date))|>
    group_by(variabele)|>
    filter(
      temporal_date == max(temporal_date) | 
          if      (2020 %in% temporal_date) {
          temporal_date == 2020 
          
        } else if (2021 %in% temporal_date) {
          temporal_date == 2021 
          
        } else if (2019 %in% temporal_date ) {
          temporal_date == 2019 
          
        } else temporal_date == 2022 
        
        )|>
  mutate(
    meting = case_when(
      temporal_date == max(temporal_date) ~ 'eenmeting',
      temporal_date != max(temporal_date) ~ 'nulmeting'),
    meting = factor(meting, levels = c("nulmeting", "eenmeting")),
    besch_jaren        = paste(unique(temporal_date),  collapse = " | "))|>
  select(-c(spatial_type, spatial_code, temporal_date, tweedeling_def))|>
  arrange(meting)|>
  pivot_wider(names_from =  c(meting,spatial_name), values_from = value, names_sep = " "),
  
  # tabel zuidoost en ams oud en nieuwe bewoner
  tabel_zittend_nieuw_oud <- data_zo_def |>
    filter(
      !is.na(thema_zuidoost_label),
      spatial_type %in% c("stadsdelen", "gemeente"))|>
    mutate(temporal_date = as.numeric(temporal_date))|>
    group_by(variabele)|>
    filter(
      temporal_date == max(temporal_date))|>
    select(-c(spatial_type, spatial_code))|>
    pivot_wider(names_from =  c(spatial_name, tweedeling_def), values_from = value, names_sep = " ")|>
    filter(!is.na(`Zuidoost nieuwe bewoner`)),
  
  # tabel naar gebied: NB: oude gebiedsindeling
  data_zo_def |>
    filter(
      tweedeling_def == 'totaal',
      !is.na(thema_zuidoost_label),
      spatial_type %in% c("gebieden", "stadsdelen", "gemeente"))|>
    mutate(temporal_date = as.numeric(temporal_date))|>
    group_by(variabele)|>
    filter(
      temporal_date == max(temporal_date))|>
    select(-c(spatial_type, spatial_name, tweedeling_def))|>
    pivot_wider(names_from =  c(spatial_code), values_from = value)
)

write.xlsx(basis_tabel_zo, "04 tabellen/01 tabellen zuidoost/overzicht indicatoren Zuidoost.xlsx")

###


tabel_eenmeting <- data_zo_def |>
  filter(
    tweedeling_def == 'totaal',
    !is.na(thema_zuidoost_label),
    spatial_type %in% c("stadsdelen", "gemeente"))|>
  mutate(temporal_date = as.numeric(temporal_date))|>
  group_by(variabele)|>
  filter(
    temporal_date == max(temporal_date) | 
      if      (2020 %in% temporal_date) {
        temporal_date == 2020 
        
      } else if (2021 %in% temporal_date) {
        temporal_date == 2021 
        
      } else if (2019 %in% temporal_date ) {
        temporal_date == 2019 
        
      } else temporal_date == 2022 
    
  )|>
  mutate(
    meting = case_when(
      temporal_date == max(temporal_date) ~ 'eenmeting',
      temporal_date != max(temporal_date) ~ 'nulmeting'),
    meting = factor(meting, levels = c("nulmeting", "eenmeting")),
    besch_jaren        = paste(unique(temporal_date),  collapse = " | "))|>
  select(-c(spatial_type, spatial_code, temporal_date, tweedeling_def))|>
  arrange(meting)|>
  pivot_wider(names_from =  c(meting,spatial_name), values_from = value, names_sep = " ")|>
  mutate(thema_zuidoost_label = str_replace(thema_zuidoost_label, 'SD3 Openbare ruimte en mobiliteit', 'SD3 ruimte en mob.'))|>
  ungroup()|>
  select(thema_zuidoost_label, indicator_sd,	besch_jaren,	`nulmeting Zuidoost`, `eenmeting Zuidoost`,	`nulmeting Amsterdam`,	`eenmeting Amsterdam`) |>
  set_names(c('thema_zuidoost_label', 'naam indicator', 'peilmomenten', 	'ZO nulmeting',	'ZO eenmeting', 'AMS nulmeting', 'AMS eenmeting'))


list_eenmeting <- split(tabel_eenmeting, f= tabel_eenmeting$thema_zuidoost_label)|>
  map(\(x) select(x, -thema_zuidoost_label) ) 
  

grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
font <- "Amsterdam Sans"


# Define a style with blue background for the header row
header_style <- createStyle(
  fontName = font,
  fontColour = "#FFFFFF",      
  fgFill = "#004699",          
  halign = "LEFT",             
  textDecoration = "bold",
  fontSize = 8
  )

data_style <- createStyle(
  fontName = font,
  fontSize = 8
)

light_blue_style <- createStyle(
  fontName = font,
  fgFill = "#b8bcdd",
  fontSize = 8
  )

dark_blue_style <- createStyle(
  fontName = font,
  fgFill = "#707ebb",
  fontColour = "#FFFFFF" ,
  fontSize = 8
  )


# Create a new workbook
wb <- createWorkbook()

# Loop through the list and add each element to a new sheet
for (i in seq_along(list_eenmeting)) {
  
  sheet_name <- names(list_eenmeting)[i]  
  addWorksheet(wb, sheet_name)     
  writeData(wb, sheet_name, list_eenmeting[[i]], headerStyle = header_style) 
  addStyle(wb, sheet_name, data_style, rows = 1:(nrow(list_eenmeting[[i]])+1), cols = 1:ncol(list_eenmeting[[i]]),gridExpand = TRUE)
  addStyle(wb, sheet_name, dark_blue_style,  rows = 2:(nrow(list_eenmeting[[i]])+ 1), cols = 5:6, gridExpand = TRUE)
  addStyle(wb, sheet_name, light_blue_style, rows = 2:(nrow(list_eenmeting[[i]])+ 1), cols = 3:4, gridExpand = TRUE)
  addStyle(wb, sheet_name, header_style,     rows = 1,                          cols = 1:ncol(list_eenmeting[[i]]), gridExpand = TRUE)

}

# Save the workbook to a file
saveWorkbook(wb, "04 tabellen/01 tabellen zuidoost/overzicht indicatoren Zuidoost eenmeting 2024.xlsx", overwrite = TRUE)





# aanroepen functies -
source("02 scripts/02 scripts publicaties Zuidoost/script 20 publicatie ZO functies.R")
