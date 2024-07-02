
library(tidyverse)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

source("02 0 scripts/00 BBGA gebiedsindelingen.R")
source("02 0 scripts/01 BBGA foutencheck functies.R")
source("02 0 scripts/03 BBGA inlezen ruwe data.R")
source("02 0 scripts/04 BBGA inlezen meta data.R")

my_attr<- function (x){
  
  x |>
    mutate(
      spatial_code  = as.character(spatial_code),
      spatial_date  = as.character(spatial_date),
      temporal_date = as.character(temporal_date),
      temporal_type = as.character(temporal_type),
      measure       = as.character(measure),
      value         = as.double(value)
    )
  
}

# fouten <- data |>
#   map(\(x) my_attr(x)) |>
#   map(\(x) functie_check(x, META_DEF))
# 


### wegschrijven data naar definitieve csv's 
onderwerpen <- names(data)


my_attr_def<- function (x){
  
  x |>
    mutate(
      spatial_code  = as.character(spatial_code),
      spatial_date  = as.integer(spatial_date),
      temporal_date = as.integer(temporal_date),
      temporal_type = as.character(temporal_type),
      measure       = as.character(measure),
      value         = as.double(value)
    )
  
}



# data |>
#   map(\(x) my_attr_def(x))|>
#   walk2(onderwerpen, \(x,y) write.csv2(
#     x, 
#     glue::glue("03 0 data voor export/20240226_{y}_cijfers.csv"),
#     row.names = F))






### overzicht oude en nieuwe vars ---




data_long <- data |>
  map_df(\(x) my_attr_def(x))

# vector met de vars die alleen geupdate worden
meta_basis_vector <- meta_basis  |>
  select("Variabele") |>
  pull()|>
  str_to_upper()

# vector met vars die nieuw zijn
meta_nieuw_vector <- meta_nieuw  |>
  select("Variabele") |>
  pull()|>
  str_to_upper()

# dit zijn alle vars die geupdate worden en de nieuwe vars
meta_all<- data |>
  map_df (\(x) my_attr_def(x))|>
  
  group_by(
    Variabele = measure,  
    temporal_type, 
    laatste_jaar = temporal_date) |>
  
  summarise(aantal=n()) |>
  
  group_by(Variabele) |>
  filter(laatste_jaar == max(laatste_jaar)) |> 
  mutate(status=case_when(
    Variabele %in% meta_basis_vector ~ 'update',
    Variabele %in% meta_nieuw_vector ~ 'nieuw')
  ) |>
  left_join(META_DEF, by = "Variabele")|>
  select(Variabele, temporal_type, status, THEMA, Label, laatste_jaar)



openxlsx::  write.xlsx(
  list(update_en_nieuwe_vars= meta_all, vervallen_vars= META_WEG), 
  "overzicht_vars_update20240226.xlsx", overwrite = T)

