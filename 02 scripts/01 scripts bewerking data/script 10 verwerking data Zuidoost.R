
### SELECTIE DATA ZUIDOOST ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

winkelgebieden <- read.csv("01 indicatoren/winkelgebied_stadsdeel.csv")

wg_zuidoost <- winkelgebieden |>
  filter(code.1 == "T") |>
  select(code)|>
  pull()

sel_zo<- c(
  "thema_zuidoost_label", "kernindicator_zo","basis",
  "indicator_sd", "variabele","spatial_code",
  "spatial_name","spatial_type", 
  "tweedeling_def","temporal_date","value")

data_def3 <- data_def2 |>
  mutate(value = case_when(
    str_ends(variabele, "_p")  ~ round(value),
    str_ends(variabele, "_r")  ~ round(value, 1),
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
  mutate(thema_zuidoost_code=str_sub(thema_zuidoost_label, 1,3))|>
  mutate(thema_zuidoost_code=replace_na(thema_zuidoost_code, "BASIS"))|>
  
  pivot_wider(
    names_from = c(temporal_date), 
    values_from = value)

list_zo_buurt <- split(BBGA_data_zo_buurt, f = BBGA_data_zo_buurt$thema_zuidoost_code)

write.xlsx(list_zo_buurt, "04 tabellen/01 tabellen zuidoost/tabel_focusgebieden_zuidoost_geb_sd.xlsx", overwrite = T)


# ### overzicht indicatoren en kernindicatoren zuidoost 
# 
# tabel_zo <- tabel_ind_def |>
#   filter(mpzo == TRUE)|>
#   select(thema_zuidoost_label, indicator_sd, variabele, kernindicator_zo)|>
#   mutate(thema_zuidoost_code=str_sub(thema_zuidoost_label, 1,3))
# 
# 
# 
# list_zo_buurt <- split(tabel_zo, f = tabel_zo$thema_zuidoost_code)|>
#   map(\(x) select(x, -thema_zuidoost_code))
# 
# write.xlsx(list_zo_buurt,  "01 indicatoren/overzicht indicatoren Zuidoost.xlsx", withFilter=T, overwrite = T)

#####################################################################################################################

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

