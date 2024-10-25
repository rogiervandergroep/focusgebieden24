
library(tidyverse)

### SELECTIE DATA NOORD ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

# toevoegen lijst met aanpak noord buurten

aanpak_noord_buurten <- read.xlsx("01 indicatoren/tabel_aanpak_noord_buurten.xlsx")|>
  select(spatial_code, aanpak_noord_buurt)

BBGA_data_noord_wide <- data_def2 |>
  
  filter(  
    str_detect(spatial_code, "^N") | spatial_code %in% c("STAD", "0363"),
    aanpak_noord == TRUE | basis == TRUE) |>
  
  select(
    "thema_noord", "indicator_sd", "variabele", "temporal_date", "value", "kernindicator_noord", 
    "spatial_code", "spatial_name", "aanpak_noord", "spatial_type", "tweedeling_def"
  )|>
  
  left_join(aanpak_noord_buurten, by='spatial_code')|>

  mutate(
    aanpak_noord_buurt = case_when(
      is.na(aanpak_noord_buurt) ~ FALSE,
      TRUE ~ aanpak_noord_buurt),
    
    thema_noord=case_when(
      is.na(thema_noord) ~ 'basis',
      TRUE ~ thema_noord)
    ) |>
    
  arrange(
    temporal_date, variabele)|>
  
  pivot_wider(
    names_from = temporal_date, 
    values_from = value) |>
  
  arrange(variabele)



# to do: gebieden en winkelgebieden in Noord
BBGA_data_noord_long <- data_def2 |>
  
  filter(  
    str_detect(spatial_code, "^N") | spatial_code %in% c("STAD", "0363"),
    aanpak_noord == TRUE | basis == TRUE)|>
  
  select(
    "thema_noord", "indicator_sd", "variabele", "temporal_date", "value", "kernindicator_noord", 
    "spatial_code", "spatial_name", "aanpak_noord", "spatial_type", "tweedeling_def"
  )|>
  
  left_join(
    aanpak_noord_buurten, by='spatial_code')|>
  
  mutate(
    aanpak_noord_buurt = case_when(
      is.na(aanpak_noord_buurt) ~ FALSE,
      TRUE ~ aanpak_noord_buurt),

    thema_noord=case_when(
      is.na(thema_noord) ~ 'basis',
      TRUE ~ thema_noord)) |>
  
  arrange(
    temporal_date, variabele) |>
  
  arrange(variabele)


list_noord <- split(BBGA_data_noord_wide, f = BBGA_data_noord_wide$thema_noord)

write.xlsx(list_noord, "04 tabellen/tabellen noord/tabel_focusgebieden_noord_buurten.xlsx", overwrite = T)
write_rds(BBGA_data_noord_long, "03 tussentijds/BBGA_data_noord.rds")


