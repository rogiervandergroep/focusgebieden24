
### SELECTIE DATA ZUIDOOST ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

winkelgebieden <- read.csv("01 indicatoren/winkelgebied_stadsdeel.csv")

wg_zuidoost <- winkelgebieden |>
  filter(code.1 == "T") |>
  select(code)|>
  pull()

sel_zo<- c(
  
  "thema_zuidoost_label", "kernindicator_zo",
  "indicator_sd", "variabele","spatial_code",
  "spatial_name","spatial_type", 
  "tweedeling_def","temporal_date","value")

data_def3 <- data_def2 |>
  mutate(value = case_when(
    str_ends(variabele, "_p")  ~ round(value),
    str_ends(variabele, "_r")  ~ round(value,1),
    variabele == 'bhstart_oph' ~ round(value,1),   
    TRUE                       ~ round(value)
  ))


### naar gebied (nb: geen data nodig op winkelgebiedniveau)
BBGA_data_zo_buurt <- data_def3 |>
  
  filter(
    str_detect(
      spatial_code, "^T") |
      spatial_code %in% c(
      "T","GT21","GT22","GT23","GT24", 
      "DX20","DX21","DX22","STAD","0363", wg_zuidoost),
    mpzo == TRUE | basis == TRUE )|>
  
  arrange(temporal_date, variabele)|>
  
  select(all_of(sel_zo))|>
  
  mutate(temporal_date = str_glue("jaar {temporal_date}"))|>
  
  pivot_wider(
    names_from = c(temporal_date), 
    values_from = value) |>
  
  mutate(thema_zuidoost_label = str_replace(
    thema_zuidoost_label, 'SD3 Openbare ruimte en mobiliteit', 'SD3 ruimte en mob.'))  |>
  mutate(`naam indicator`= case_when(
      kernindicator_zo == TRUE ~ glue::glue("{`indicator_sd`} (kernindicator)"),
      TRUE                  ~ `indicator_sd`))|>
  
  arrange(desc(kernindicator_zo))|>
  select(-c(kernindicator_zo, indicator_sd))|>
  select(`naam indicator`, everything())

list_dataset <- split(
  BBGA_data_zo_buurt, 
  f = BBGA_data_zo_buurt$thema_zuidoost_label)|>
  map(\(x) select(x, -thema_zuidoost_label))


source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

### colofon voor de tabellenrapportage totaal ---
naam_focusgebied         <- 'Masterplan Zuidoost'
naam_stadsdeel           <- 'Zuidoost'
naam_monitor             <- 'Outcomemonitor Masterplan Zuidoost'
naam_website_focusgebied <- 'www.zoiszuidoost.nl'


# Create a new workbook voor sd tabel
wb_zuidoost <- my_style_sheet(
  
  x = list_dataset, 
  col_dark_bl  = NULL, #kolommen donkerblauw
  col_light_bl = NULL,  #kolommen lichtblauw
  colofon_type = colofon_totaal
  
)

saveWorkbook(wb_zuidoost, glue::glue("04 tabellen/01 tabellen zuidoost/tabel alle data { naam_focusgebied } nov 2024.xlsx"), overwrite = T)
saveWorkbook(wb_zuidoost, glue::glue("04 tabellen/05 tabellen website focusgebieden/tabel alle data { naam_focusgebied } nov 2024.xlsx"), overwrite = T)



BBGA_data_zo <- data_def3 |>
  
  filter(
    str_detect(
      spatial_code, "^T") |
      spatial_code %in% c(
        "T","GT21","GT22","GT23","GT24", 
        "DX20","DX21","DX22","STAD","0363", wg_zuidoost),
    mpzo == TRUE | basis == TRUE)|>
  
  arrange(temporal_date, variabele)|>
  
  select(all_of(sel_zo))|>
  mutate(thema_zuidoost_code=str_sub(thema_zuidoost_label, 1,3))|>
  mutate(thema_zuidoost_code=replace_na(thema_zuidoost_code, "BASIS"))



write_rds(BBGA_data_zo, "03 tussentijds/BBGA_data_zo.rds")

