
### SELECTIE DATA ZUIDOOST ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")


sel_zo<- c("ambitie_zuidoost","indicator_sd", "variabele","thema_bbga","label_bbga","spatial_code",
           "spatial_name","spatial_type","temporal_date",
           "value","tweedeling_def","bron")

data_def3 <- data_def2 |>
  mutate(value = case_when(
    str_ends(variabele, "_p")  ~ round(value),
    str_ends(variabele, "_r")  ~ round(value, 1),
    TRUE                       ~ round(value)
  ))


### naar buurt
BBGA_data_zo_buurt <- data_def3 |>
  
  filter(
    str_detect(spatial_code, "^T") | spatial_code %in% c("STAD", "0363"),
    mpzo == TRUE | basis == TRUE)|>
  
  arrange(temporal_date, variabele)|>
  
  mutate(ambitie_zuidoost=case_when(
    is.na(ambitie_zuidoost) ~ 'basis',
    TRUE ~ ambitie_zuidoost)) |>
  
  select(
    all_of(sel_zo))|>
  
  pivot_wider(
    names_from = c(temporal_date), 
    values_from = value)

list_zo_buurt <- split(BBGA_data_zo_buurt, f = BBGA_data_zo_buurt$ambitie_zuidoost)

write.xlsx(list_zo_buurt, "04 tabellen/tabellen zuidoost/tabel_focusgebieden_zuidoost_buurt.xlsx", overwrite = T)


### naar stadsdeel en Amsterdam
BBGA_data_zo_sd <- data_def3 |>
  
  filter(
    spatial_code %in% c("T", "STAD", "0363"),
    mpzo == TRUE | basis == TRUE)|>
  
  arrange(
    temporal_date, variabele)|>
  
  mutate(ambitie_zuidoost=case_when(
    is.na(ambitie_zuidoost) ~ 'basis',
    TRUE ~ ambitie_zuidoost)) |>
  
  select(
    all_of(sel_zo)) |>
  
  pivot_wider(
    names_from = c(temporal_date), 
    values_from = value)

list_zo_sd <- split(BBGA_data_zo_sd, f = BBGA_data_zo_sd$ambitie_zuidoost)

write.xlsx(list_zo_sd, "04 tabellen/tabellen zuidoost/tabel_focusgebieden_zuidoost_stadsdeel.xlsx", overwrite = T)
