
library(tidyverse)

### SELECTIE DATA NOORD ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

# toevoegen lijst met aanpak noord buurten

aanpak_noord_buurten <- read.xlsx("01 indicatoren/overzicht focusbuurten noord en nieuwwest.xlsx")|>
  filter(stadsdeel == 'Noord')|>
  select(spatial_code, focusbuurt)


# koppel inwoneraantal aan aanpak Noord buurten

df_inwoner_brt <- data_def2 |>
  filter(
    spatial_type == 'buurten',
    variabele == 'bevtotaal', 
    temporal_date == max(temporal_date)
    )|>
  ungroup()|>
  select(spatial_code , value) |>
  rename ("inw_aantal_24" = "value")

aanpak_noord_buurten_wf <- aanpak_noord_buurten |>
  left_join(df_inwoner_brt, by = 'spatial_code')


# to do: gebieden en winkelgebieden in Noord
BBGA_data_noord_long <- data_def2 |>
  
  filter(  
    str_detect(spatial_code, "^N") | spatial_code %in% c("STAD", "0363"),
    aanpak_noord == TRUE | basis == TRUE)|>
  
  select(
    "thema_noord_eenmeting", "indicator_sd", "variabele", "temporal_date", "value", "kernindicator_noord", 
    "spatial_code", "spatial_name", "aanpak_noord", "spatial_type", "tweedeling_def", 
  )|>
  
  left_join(
    aanpak_noord_buurten_wf, by='spatial_code')|>
  
  mutate(
    focusbuurt = case_when(
      is.na(focusbuurt) ~ FALSE,
      TRUE ~ focusbuurt),

    thema_noord_eenmeting=case_when(
      is.na(thema_noord_eenmeting) ~ 'basis',
      TRUE ~ thema_noord_eenmeting)) |>
  
  arrange(
    temporal_date, variabele) |>
  
  arrange(variabele)

### toevoegen gemiddeldes focus-buurten 
data_noord_focus_brt <- BBGA_data_noord_long |>
  ungroup()|>
  filter(
    inw_aantal_24 > 200,
    spatial_type == 'buurten',
    focusbuurt == TRUE
    )|>
  group_by(    
    thema_noord_eenmeting,  indicator_sd, variabele, temporal_date, kernindicator_noord, 
    aanpak_noord, tweedeling_def, focusbuurt, spatial_type
    )|>
  summarise(
    value    = round(mean(value,      na.rm = T),1),
    )|>  
  add_column(
    spatial_name = 'gemiddelde focusbuurten',
    spatial_code = 'NFOCUS')
  
  
### toevoegen gemiddeldes overige buurten 
data_noord_overig_brt <- BBGA_data_noord_long |>
  filter(
    inw_aantal_24 > 200,
    spatial_type == 'buurten',
    focusbuurt == FALSE)|>
  group_by(    
    thema_noord_eenmeting,  indicator_sd, variabele, temporal_date, kernindicator_noord, 
    aanpak_noord, tweedeling_def, focusbuurt, spatial_type
    )|>
  summarise(
    value    = round(mean(value,      na.rm = T),1),
    )|>
  add_column(
    spatial_name =  'gemiddelde overige buurten',
    spatial_code =  'NOVERIG')

BBGA_data_noord_def <- bind_rows(
  
  BBGA_data_noord_long, 
  data_noord_overig_brt, 
  data_noord_focus_brt
  )

BBGA_data_noord_def_wide <- BBGA_data_noord_def |>
  filter(thema_noord_eenmeting != 'basis')|>
  pivot_wider(values_from = value, names_from = temporal_date) |>
  rename(`naam indicator` = indicator_sd)|>
  mutate(`naam indicator`= case_when(
    kernindicator_noord == TRUE ~ glue::glue("{`naam indicator`} (kernindicator)"),
    TRUE                  ~ `naam indicator`))|>
  arrange(desc(kernindicator_noord))|>
  select(-kernindicator_noord)

list_noord <- split(BBGA_data_noord_def_wide, f = BBGA_data_noord_def_wide$thema_noord_eenmeting)|>
  map(\(x) select(x, -thema_noord_eenmeting))


source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

### colofon voor de tabellenrapportage totaal ---
naam_focusgebied         <- 'Aanpak Noord'
naam_stadsdeel           <- 'Noord'
naam_monitor             <- 'Outcomemonitor Aanpak Noord'
naam_website_focusgebied <- 'www.aanpaknoord.nl'


# Create a new workbook voor sd tabel
wb_noord <- my_style_sheet(
  
  x = list_noord, 
  col_dark_bl  = NULL, #kolommen donkerblauw
  col_light_bl = NULL,  #kolommen lichtblauw
  colofon_type = colofon_totaal
  
)

saveWorkbook(wb_noord, glue::glue("04 tabellen/02 tabellen noord/tabel alle data { naam_focusgebied } nov 2024.xlsx"), overwrite = T)
saveWorkbook(wb_noord, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel alle data { naam_focusgebied } nov 2024.xlsx"), overwrite = T)





write_rds(BBGA_data_noord_def, "03 tussentijds/BBGA_data_noord.rds")


