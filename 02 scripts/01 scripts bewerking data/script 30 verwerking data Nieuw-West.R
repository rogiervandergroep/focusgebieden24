
library(tidyverse)

### SELECTIE DATA NOORD ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

# wellicht komt hier nog een lijst met aandachtbuurten

# selectie Nieuw-West
BBGA_data_nw <- data_def2 |>
  
  filter(  
    str_detect(spatial_code, "^F") | 
    str_detect(spatial_code, "^GF") |
    spatial_code %in% c("STAD", "0363"),
    samen_nw == TRUE | basis == TRUE
    )|>
  
  select(
    indicator_sd, variabele, temporal_date, value, 
    kernindicator_nw, thema_nw_kleur, thema_nw_label,
    spatial_code, spatial_name, spatial_type, tweedeling_def
    )|>
  
  mutate(
    kernindicator_nw = case_when(is.na(kernindicator_nw) ~ 'basis', TRUE ~ kernindicator_nw),
    thema_nw_kleur   = case_when(is.na(thema_nw_kleur)   ~ 'basis', TRUE ~ thema_nw_kleur),
    thema_nw_label   = case_when(is.na(thema_nw_label)   ~ 'basis', TRUE ~ thema_nw_label) 
    )|>
  
  arrange(temporal_date, variabele)

#omzetten naar wide format
BBGA_data_nw_wide <- BBGA_data_nw |>
  
  pivot_wider(
    names_from = temporal_date, 
    values_from = value) |>
  
  arrange(variabele) 



list_nw <- split(BBGA_data_nw_wide, f = BBGA_data_nw_wide$thema_nw_kleur)

write.xlsx(list_nw,      "04 tabellen/03 tabellen nieuwwest/tabel_focusgebieden_nw_buurten.xlsx", overwrite = T)
write_rds (BBGA_data_nw, "03 tussentijds/BBGA_data_nw.rds")


