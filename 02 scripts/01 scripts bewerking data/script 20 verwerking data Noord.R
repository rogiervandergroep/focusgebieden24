
library(tidyverse)

### SELECTIE DATA NOORD ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

# toevoegen lijst met aanpak noord buurten

aanpak_noord_buurten <- read.xlsx("../data/01 indicatoren/tabel_aanpak_noord_buurten.xlsx")|>
  select(spatial_code, aanpak_noord_buurt)

BBGA_data_noord_wide <- data_def2 |>
  
  filter(  
    str_detect(spatial_code, "^N") | spatial_code %in% c("STAD", "0363"),
    aanpak_noord == TRUE | basis == TRUE) |>
  
  select(
    -c("ambitie_zuidoost", "thema_bbga", "label_bbga", "mpzo", "nplv", "spatial_date", "samen_nw", 
       'bbga',  "bronhouder", "opmerking", "thema_nw" , "aanpak_noord", "bron",  "basis", "besch_jaren", "besch_tweedeling", "besch_aggr_niveaus")
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


BBGA_data_noord_long <- data_def2 |>
  
  filter(  
    str_detect(spatial_code, "^N") | spatial_code %in% c("STAD", "0363"),
    aanpak_noord == TRUE | basis == TRUE)|>
  
  select(
    -c("ambitie_zuidoost", "thema_bbga", "label_bbga", "mpzo", "nplv", "spatial_date",
       'bbga',  "bronhouder", "opmerking", "thema_nw", "samen_nw",
       "aanpak_noord")
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


