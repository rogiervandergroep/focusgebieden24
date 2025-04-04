
##########################
### extra berekeningen ---
##########################

# met dit script worden extra indicatoren toegevoegd. Vaak gaat het om aandelen op basis van bestaande indicatoren. 
# In de functie geef je aan welke twee variabelen er toegevoegd worden en hoe de nieuwe variabele heet
# er zijn groeicijfers; percentages; dichtheden


# my_berekening<- function (x, berekening, teller, noemer, newvar) {
#   
#      
#   y <- x |>
#     filter(
#       variabele %in% c('bhvest'))|>
#     select(all_of(var_sel))|>
#     group_by(
#       spatial_code, spatial_name)|>
#     arrange(temporal_date )
#   
#   if (berekening = 'groeicijfer') {
#     
#     mutate(
#       value = value/first(value)*100)|>
#     
#   }
#     
#     select(-variabele)|>
#     add_column(variabele = 'bhvest_groei')|>
#     left_join(tabel_ind, by="variabele")
#   
#   
# }

var_sel<- c("spatial_code", "spatial_name", "spatial_type", "temporal_date", "variabele", "value")

extra  <- bind_rows(
  

  
 
  # berekening groei vestigingen
  data_def|>
    filter(
      variabele %in% c('bhvest'))|>
    select(all_of(var_sel))|>
    group_by(
      spatial_code, spatial_name)|>
    arrange(temporal_date )|>
    mutate(
      value = value/first(value)*100)|>
    select(-variabele)|>
    add_column(variabele = 'bhvest_groei')|>
    left_join(tabel_ind, by="variabele"),
  
  
  data_def|>
    filter(
      variabele %in% c('orntrgroen', 'opp_publieke_ruimte_land_ha')
      )|>
    select(all_of(var_sel))|>
    pivot_wider(
      values_from = value, 
      names_from = variabele)|>
    mutate(
      value = (orntrgroen/opp_publieke_ruimte_land_ha)*100)|>
    add_column(variabele = 'orntrgroen_p')|>
    left_join(tabel_ind, by="variabele")  |>
    select(-(c("orntrgroen", "opp_publieke_ruimte_land_ha"))),
  

  # berekening publiekelijk groen per 1000 inwoners
  data_def|>
    filter(
      variabele %in% c('orpubgroen', 'bevtotaal'))|>
    select(all_of(var_sel))|>
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
    select(all_of(var_sel))|>
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
   select(all_of(var_sel))|>
   group_by(
     spatial_code, spatial_name)|>
   arrange(temporal_date )|>
   mutate(
     value = value/first(value)*100)|>
   select(-variabele)|>
   add_column(variabele = 'wvoorrbag_groei')|>
   left_join(tabel_ind, by="variabele"),
  
 # aandeel starters gedeeld door aandeel opheffingen   NB: nieuwe variabele: werkt pas na BBGA update
 data_def|>
   filter(
     variabele %in% c('bhstart_tot', 'bhophef_tot', 'bhvest'))|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = ((bhstart_tot/bhvest)/(bhophef_tot/bhvest))
     )|>
   add_column(variabele = 'bhstart_oph')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhstart_tot", "bhophef_tot"))),
 
 # berekening aandeel STARTERS
 data_def|>
   filter(
     variabele %in% c('bhstart_tot', 'bhvest'))|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (bhstart_tot/bhvest)*100)|>
   add_column(variabele = 'bhstart_tot_p')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhstart_tot", "bhvest"))),
 
  # berekening aandeel zonder startkwalificatie
 data_def|>
   filter(
     variabele %in% c('startkwalificatie_p'),
     tweedeling_def=='totaal')|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (100-startkwalificatie_p))|>
   add_column(variabele = 'startkwal_zonder_p')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("startkwalificatie_p"))),
 
 # berekening aandeel werkzame personen in CI 
 data_def|>
   filter(
     variabele %in% c('bhwp_ci', 'bhwp'))|>
   select(all_of(var_sel))|>
   pivot_wider(
     values_from = value,
     names_from = variabele)|>
   mutate(
     value = (bhwp_ci/bhwp)*100)|>
   add_column(variabele = 'bhwp_ci_p')|>
   left_join(tabel_ind, by="variabele")  |>
   select(-(c("bhwp_ci", "bhwp")))
  
  )  |>
  add_column(tweedeling_def= 'totaal')

