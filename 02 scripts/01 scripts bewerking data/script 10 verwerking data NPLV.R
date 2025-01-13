
### SELECTIE DATA nplv ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

sel_nplv<- c("thema_nplv", "indicator_sd","variabele","spatial_name","spatial_code", "spatial_type", "temporal_date", "value")   

actielijnen <- c(
 'actielijn1' = "We verbeteren slechte woningen en zorgen voor meer gemengde wijken",
 'actielijn2' = "We zorgen dat meer bewoners mee kunnen doen in de samenleving" ,
 'actielijn3' = "We investeren in de preventie van jeugdcriminaliteit en vergroten de weerbaarheid van jongeren" 
)


data_def_nplv <- data_def2 |>
  filter(
    temporal_date %in% c(2020, 2021, 2022, 2023),
    tweedeling_def == 'totaal',
    nplv == TRUE,
    spatial_code %in% c('F', 'N', 'T', '0363'))|>
  select(all_of(c(sel_nplv)))|>
  mutate(
    value = round(value,2),
    thema_nplv_name =  actielijnen[thema_nplv])|>
  select(-c("spatial_code", "spatial_type", "variabele"))|>
  group_by(spatial_name)|>
  pivot_wider(
    names_from = c(spatial_name, temporal_date), 
    values_from = value,
    names_glue = ("{spatial_name} | {temporal_date}")
    )|>
  ungroup()|>
  select(
    thema_nplv, thema_nplv_name, indicator_sd, 
  
    starts_with("Nieuw-West"),
    starts_with("Noord"),
    starts_with("Zuidoost"),
    starts_with("Amsterdam")
    
    )|>
  arrange(thema_nplv, indicator_sd)|>
  rename (indicator=indicator_sd)




source("02 scripts/01 scripts bewerking data/script 00 layout excel.R")

# maak een lijst (functie snapt alleen lists en geen dataframes)
list_nplv <- list(data_nplv = data_def_nplv)

# Create a new workbook voor sd tabel
wb_nplv <- my_style_sheet(
  
  x = list_nplv, 
  col_dark_bl  = c(1:3), #kolommen donkerblauw
  col_light_bl = c(8:11,16:19),  #kolommen lichtblauw
  colofon_type = colofon_nplv
  
)

# Save the workbook to a file
saveWorkbook(wb_nplv, "04 tabellen/04 tabellen NPLV/tabel alle data NPLV nov 2024.xlsx", overwrite = TRUE)
saveWorkbook(wb_nplv, "04 tabellen/05 tabellen website focusgebieden/tabel alle data NPLV nov 2024.xlsx", overwrite = TRUE)
