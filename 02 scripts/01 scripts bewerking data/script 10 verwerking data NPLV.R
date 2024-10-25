
### SELECTIE DATA ZUIDOOST ---

source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")



sel_nplv<- c("thema_nplv", "indicator_sd","variabele","spatial_name","spatial_code", "spatial_type", "temporal_date", "value")   

data_def_nplv <- data_def2 |>
  filter(
    temporal_date > 2018,
    tweedeling_def== 'totaal',
    nplv==TRUE,
    spatial_code %in% c('F', 'N', 'T', '0363'))|>
  select(all_of(c(sel_nplv)))|>
  mutate(value = case_when(
    variabele == 'ses_woa'     ~ round(value,3),
    str_ends(variabele, "_p")  ~ round(value),
    str_ends(variabele, "_r")  ~ round(value, 1),
    TRUE                       ~ round(value)
  ))|>
  pivot_wider(names_from = c(spatial_name,temporal_date), values_from = value)




# Actielijn 1 We verbeteren slechte woningen en zorgen voor meer gemengde wijken
# Actielijn 2 We zorgen dat meer bewoners mee kunnen doen in de samenleving
# Actielijn 3 We investeren in de preventie van jeugdcriminaliteit en vergroten de weerbaarheid van jongeren	

### naar gebied (nb: geen data nodig op winkelgebiedniveau)

# pwerkend_p staat in BBGA wordt in oktober 24 geupdate met 2023

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
saveWorkbook(wb, "04 tabellen/04 tabellen NPLV/tabel_nplv_indicatoren_sd_2.xlsx", overwrite = TRUE)

