
# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/04 scripts publicaties Nieuw-West/script 22 publicatie NW factsheets algemeen data.R")
source("02 scripts/01 scripts bewerking data/script 50 functies voor kaarten.R")

kernind_vector <- kernind_df |>
  filter(kernindicator_nw == 'primair')|>
  select(variabele)|>
  pull()
  
data_nw_ruw|>
  ungroup()|>
  select(spatial_code, spatial_name, focusbuurt)|>
  filter(focusbuurt == TRUE)|>
  distinct()|>
  select(spatial_code)|>
  pull()
 
#"F", "0363" 

focusbuurten <- c(
  "FB03", "FC02", "FC03", "FJ01",
  "FK01", "FL03", "FM06", "FB08",
  "FE01", "FE02"
  )


# Het heeft niet veel nut om met 10 buurten kwartielen te berekenen

### functie om kwartielen te berekenen
my_temporal_filter <- function (x, kern_var, met) {
  
  y <- x |>
    
    group_by(variabele)|>
    
    filter(
      
      spatial_code %in% focusbuurten,
      tweedeling_def == 'totaal',
      
      temporal_date == max(temporal_date) |
        
        if (2020 %in% temporal_date) {
          temporal_date == 2020
          
        } else if (2021 %in% temporal_date) {
          temporal_date == 2021
          
        } else if (2019 %in% temporal_date ) {
          temporal_date == 2019
          
        } else temporal_date == 2022 
      )|>
    
    mutate(meting = case_when(
      temporal_date == max(temporal_date) ~ '1-meting',
      TRUE                                ~ '0-meting')
      )|>
    
    group_by(variabele, meting) |>
   
    filter(
      !is.na(value),
      variabele == kern_var,
      meting    == met)
  
 
}


# toevoeing ranking aan indicatoren
# "ostrflez_p", "ostrfrek_p", "ostrftaal_p"
  
var_pos <- c(
  "bhwinkelaanbod_r", "lbetrokken_r","lbuurt_r","wwoning_r","lomganggroepenb_r",  "bhstart_tot_p",       
  "vveiligvoelen_p", "ldiscri_p")

var_neg <- c(
  "w_krap_kind_p",  "skkwets34_p", "neet_1826_p", "iminhh130_p", "or_grof_p")           


tabel_basis <- bind_rows(
  
  c(var_pos, var_neg) |>
    map_df (\(x) my_temporal_filter(data_nw_ruw, x, '1-meting')),

  c(var_pos, var_neg) |>
    map_df (\(x) my_temporal_filter(data_nw_ruw, x, '0-meting')))|>
  group_by(variabele)|>
  arrange(temporal_date)|>
  mutate(besch_jaren = paste(unique(temporal_date), collapse = "|"))|>
  group_by(indicator_sd, meting)|>
  mutate(relatieve_waarde =  case_when(
    (value >  mean(value) &  variabele %in% var_pos)  ~ 3,
    (value == mean(value) &  variabele %in% var_pos)  ~ 2,
    (value <  mean(value) &  variabele %in% var_pos)  ~ 1,
    
    (value >  mean(value) &  variabele %in% var_neg)  ~ 1,
    (value == mean(value) &  variabele %in% var_neg)  ~ 2,
    (value <  mean(value) &  variabele %in% var_neg)  ~ 3)
  )|>
  select(indicator_sd,  thema_nw_label, spatial_code, spatial_name, besch_jaren, meting, relatieve_waarde)|>
  pivot_wider(values_from = relatieve_waarde, names_from = c('spatial_code', 'spatial_name'), names_sep = " | ")

  write.xlsx(tabel_basis, "10 rapporten/04 rapporten Nieuw-West/tabel_scores_kernindicatoren_NW.xlsx")


