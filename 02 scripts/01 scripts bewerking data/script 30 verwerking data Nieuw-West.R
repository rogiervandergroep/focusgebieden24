
library(tidyverse)

### SELECTIE DATA NIEUW-WEST ---

data_def2 <- read_rds("03 tussentijds/data_def2.rds")

# lijst met aandachtbuurten
aanpak_nw_buurten <- read.xlsx("01 indicatoren/overzicht focusbuurten noord en nieuwwest.xlsx")|>
  filter(stadsdeel == 'Nieuw-West')|>
  select(spatial_code, focusbuurt)

winkelgebieden <- read.csv("01 indicatoren/winkelgebied_stadsdeel.csv")

wg_nieuwwest <- winkelgebieden |>
  filter(code.1 == "F") |>
  select(code)|>
  pull()

# toevoegen inwoneraantal
df_inwoner_brt <- data_def2 |>
  filter(
    spatial_type == 'buurten',
    variabele == 'bevtotaal', 
    temporal_date == max(temporal_date)
  )|>
  ungroup()|>
  select(spatial_code , value) |>
  rename ("inw_aantal_24" = "value")

aanpak_nw_buurten_wf <- aanpak_nw_buurten |>
  left_join(df_inwoner_brt, by = 'spatial_code')


# selectie Nieuw-West
BBGA_data_nw <- data_def2 |>
  filter(  
    str_detect(spatial_code, "^F") | 
    str_detect(spatial_code, "^GF") |
    spatial_code %in% c("STAD", "0363", wg_nieuwwest),
    samen_nw == TRUE | basis == TRUE
    )|>
  select(
    indicator_sd, variabele, temporal_date, value, 
    kernindicator_nw, thema_nw_kleur, thema_nw_label,
    spatial_code, spatial_name, spatial_type, tweedeling_def
  )|>
  left_join(
    aanpak_nw_buurten_wf, by = 'spatial_code')|>
  mutate(
    focusbuurt       = case_when(is.na(focusbuurt)       ~ FALSE,   TRUE ~ focusbuurt),
    kernindicator_nw = case_when(is.na(kernindicator_nw) ~ 'basis', TRUE ~ kernindicator_nw),
    thema_nw_kleur   = case_when(is.na(thema_nw_kleur)   ~ 'basis', TRUE ~ thema_nw_kleur),
    thema_nw_label   = case_when(is.na(thema_nw_label)   ~ 'basis', TRUE ~ thema_nw_label) 
    )|>
  arrange(temporal_date, variabele)

### toevoegen gemiddeldes focus-buurten 
BBGA_data_nw_focus_brt <- BBGA_data_nw |>
  ungroup()|>
  filter(
    inw_aantal_24 > 200,
    spatial_type == 'buurten',
    focusbuurt == TRUE
  )|>
  group_by(    
    indicator_sd, variabele, temporal_date, 
    kernindicator_nw, thema_nw_kleur, thema_nw_label,
    tweedeling_def, focusbuurt, spatial_type
  )|>
  summarise(
    value    = round(mean(value, na.rm = T),1),
  )|>  
  add_column(
    spatial_name = 'gemiddelde focusbuurten',
    spatial_code = 'F_FOCUS')


### toevoegen gemiddeldes overige buurten 
BBGA_data_nw_overig_brt <- BBGA_data_nw |>
  ungroup()|>
  filter(
    inw_aantal_24 > 200,
    spatial_type == 'buurten',
    focusbuurt == FALSE
  )|>
  group_by(    
    indicator_sd, variabele, temporal_date, 
    kernindicator_nw, thema_nw_kleur, thema_nw_label,
    tweedeling_def, focusbuurt, spatial_type
  )|>
  summarise(
    value    = round(mean(value, na.rm = T),1),
  )|>  
  add_column(
    spatial_name = 'gemiddelde overige buurten',
    spatial_code = 'F_OVERIG')

# samenvoegen data  
BBGA_data_nw_def <- bind_rows(
  
  BBGA_data_nw, 
  BBGA_data_nw_focus_brt, 
  BBGA_data_nw_overig_brt
  
)



#omzetten naar wide format
BBGA_data_nw_wide <- BBGA_data_nw_def |>
  
  select(-inw_aantal_24)|>
  
  pivot_wider(
    names_from = temporal_date, 
    values_from = value) |>
  
  arrange(kernindicator_nw, variabele) |>
  
  select(thema_nw_kleur, thema_nw_label, kernindicator_nw,  everything())

thema_nw <- c(
  "basis",
  "eigenaarschap en inclusie",
  "kansen voor de jeugd",                       
  "veiligheid",                                 
  "werk en bestaanszekerheid",                  
  "wonen" )  

list_nw <- split(BBGA_data_nw_wide, f = BBGA_data_nw_wide$thema_nw_label)|>
  set_names(thema_nw)




source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

### colofon voor de tabellenrapportage totaal ---
naam_focusgebied         <- 'Samen Nieuw-West'
naam_stadsdeel           <- 'Nieuw-West'
naam_monitor             <- 'Outcomemonitor Samen Nieuw-West'
naam_website_focusgebied <- 'www.samennieuw-west.nl'


# Create a new workbook voor sd tabel
wb_nw <- my_style_sheet(
  
  x = list_nw, 
  col_dark_bl  = NULL, #kolommen donkerblauw
  col_light_bl = NULL,  #kolommen lichtblauw
  colofon_type = colofon_totaal
  
)

saveWorkbook(wb_nw, glue::glue("04 tabellen/03 tabellen nieuwwest/tabel alle data { naam_focusgebied } { datum_vandaag }.xlsx"), overwrite = T)
saveWorkbook(wb_nw, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel alle data { naam_focusgebied } { datum_vandaag }.xlsx"), overwrite = T)



write_rds (BBGA_data_nw_def, "03 tussentijds/BBGA_data_nw.rds")


