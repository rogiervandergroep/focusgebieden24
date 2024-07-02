
##########################
### extra berekeningen ---
##########################

extra  <- bind_rows(
  
  # berekening boomkronen oppervlakte
  data_def|>
    
    filter(
      variabele %in% c('orland', 'opp_boomkroon_publiek_ha'),
      temporal_date == '2023')|>
    select(spatial_code:value)|>
    pivot_wider(
      values_from = value, 
      names_from = variabele)|>
    mutate(
      value = (opp_boomkroon_publiek_ha/orland)*100)|>
    add_column(variabele = 'opp_boomkroon_publiek_p') |>
    left_join(tabel_ind, by="variabele") |>
    select(-(c("opp_boomkroon_publiek_ha", "orland"))),
  
 
  # berekening groei vestigingen
  data_def|>
    filter(
      variabele %in% c('bhvest'))|>
    select(spatial_code:value)|>
    group_by(
      spatial_code, spatial_name)|>
    arrange(temporal_date )|>
    mutate(
      value = value/first(value)*100)|>
    select(-variabele)|>
    add_column(variabele = 'bhvest_groei')|>
    left_join(tabel_ind, by="variabele"),
  

  # berekening publiekelijk groen per 1000 inwoners
  data_def|>
    
    filter(
      variabele %in% c('orpubgroen', 'bevtotaal'),
      temporal_date == '2023')|>
    select(spatial_code:value)|>
    pivot_wider(
      values_from = value, 
      names_from = variabele)|>
    mutate(
      value = (orpubgroen/bevtotaal)*1000)|>
    add_column(variabele = 'orpubgroen_inw')|>
    left_join(tabel_ind, by="variabele")  |>
    select(-(c("orpubgroen", "bevtotaal"))),
  
  # bevolkingsgroei
 data_def|>
    filter(
      variabele %in% c('bevtotaal'))|>
    select(spatial_code:value)|>
    group_by(
      spatial_code, spatial_name)|>
   arrange(temporal_date )|>
    mutate(
      value = value/first(value)*100)|>
   select(-variabele)|>
    add_column(variabele = 'bevtotaal_groei')|>
    left_join(tabel_ind, by="variabele"),
 
 
 # woningvoorraadontwikkeling
 data_def|>
   filter(
     variabele %in% c('wvoorrbag'))|>
   select(spatial_code:value)|>
   group_by(
     spatial_code, spatial_name)|>
   arrange(temporal_date )|>
   mutate(
     value = value/first(value)*100)|>
   select(-variabele)|>
   add_column(variabele = 'wvoorrbag_groei')|>
   left_join(tabel_ind, by="variabele")
  
  
  
  
)  |>
  add_column(tweedeling_def= 'totaal')

