
# met dit script wordt de data ingelezen en worden functies aangemaakt voor de plots
source("02 scripts/03 scripts publicaties Noord/script 22 publicatie Noord factsheets algemeen data.R")
source("02 scripts/01 scripts bewerking data/script 50 functies voor kaarten.R")


# "opp_boomkroon_publiek_p", "orgafspark_m" van deze indicatoren is maar een jaar beschikbaar
#  ostrftaal_p van deze indicator ontbreekt waardes voor sommige buurten
var_pos <- c(
  "bhhorecaaanbod_r", "bhvest", "bhwinkelaanbod_r", "bhzzp_p", "vveiligavond_r",
  "lbetrokken_r", "lbuurt_r", "lverbondenbuurt_p", "orntrgroen_p", "lomganggroepenb_r",
  "oronderhoudstraten_r", "wonderhoudwoning_r", "orpubgroen_p", 
  "wwoning_r", "wzgezond_p")  

#"wzdepr_p"
var_neg <- c(
  "iminhh130_p", "iminjong130_p",  "startkwal_zonder_p",
  "skkwets34_p",  "wlabelefg_p",   "v_verloed_i", "neet_1826_p", 
  "dnietiso_p", "ovveindi_p")

# focusbuurten in Noord
focusbuurten <- c(
  "NA07",  "NC01",  "NC02",  "NC03",  "NE01",  "NE02",  "NE03",  "NE04",
  "NH02",  "NH05",  "NJ02",  "NJ03",  "NJ05",  "NJ06",  "NK01",  "NK02",
  "NK03",  "NL01",  "NL03",  "NL04",  "NM01",  "NN01",  "NN02"
)

labels_score <- c(
  "veel positiever", "positiever", 
  "negatiever", "veel negatiever dan gemiddelde focusbuurten")

### functie om kwartielen te berekenen
my_quantcut <- function (x, kern_var, met) {
  
  y <- x |>
    
    group_by(variabele)|>
    
    filter(
      
      focusbuurt == TRUE,
      spatial_code != 'NFOCUS',
      !is.na(value),
      
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
  
  print(nrow(y))
  
    if(nrow(y) != 0){
    
      y <- y |> 
        
      mutate(
        value_kl = gtools::quantcut(value, na.rm = T),
        
        value_kl_naam = case_when(
          
          variabele %in% var_neg ~ gtools::quantcut(
            value, na.rm = T, labels = seq(levels(value_kl))),
          
          variabele %in% var_pos ~ gtools::quantcut(
            value, na.rm = T, labels = rev(seq(levels(value_kl))))
        )
      )
    } 
  
  return(y)
}


# toevoeing ranking aan indicatoren

tabel_basis <- bind_rows(
  
  c(var_pos, var_neg) |>
    map_df (\(x) my_quantcut(data_noord_def, x, '1-meting')),

  c(var_pos, var_neg) |>
    map_df (\(x) my_quantcut(data_noord_def, x, '0-meting'))
)  |>
  ungroup()|>
  select(
    thema_noord_eenmeting, variabele, indicator_sd, spatial_code, 
    spatial_name, meting, value, value_kl, value_kl_naam)|>
  mutate(
    value_kl_naam = as.numeric(value_kl_naam))


tabel_basis_sum <- tabel_basis|>
  group_by(spatial_code, spatial_name, meting)|>
  summarise(
    value_kl_naam = sum(value_kl_naam, na.rm = T)
    )|>
  add_column(
    indicator_sd = 'totaal',
    thema_noord_eenmeting = '',
    variabele = '') |>
  group_by(meting)|>
  mutate(
    value_label      = gtools::quantcut(value_kl_naam, na.rm = T),
    value_label_naam = gtools::quantcut(
      value_kl_naam, 
      na.rm = T, 
      labels = labels_score)
  )
      
write.xlsx(
  
  tabel_basis_sum|>
    select(spatial_code, spatial_name, meting, value_kl_naam, value_label_naam)|>
    pivot_wider(values_from = c(value_kl_naam, value_label_naam) , names_from = meting),
  
  "10 rapporten/03 rapporten Noord/tabel_scores_kernindicatoren_tabel1.xlsx")






tabel_basis_groei <- tabel_basis_sum |>
  ungroup()|>
  group_by(spatial_code, spatial_name, indicator_sd)|>
  summarise(value_kl_naam =(
    value_kl_naam - lag(value_kl_naam))/lag(value_kl_naam)*100
    )|>
  add_column(meting = 'verandering (%)')|>
  filter(!is.na(value_kl_naam))



tabel_basis_def <- bind_rows(tabel_basis, tabel_basis_sum, tabel_basis_groei)  
    



tabel_basis_wide <- bind_rows(tabel_basis, tabel_basis_sum)   |>
  
  select(thema_noord_eenmeting, indicator_sd, spatial_code, spatial_name, value_kl_naam, meting)|>
  pivot_wider(values_from = value_kl_naam, names_from =  c(spatial_code, spatial_name))|>
  arrange(thema_noord_eenmeting, indicator_sd, meting)

write.xlsx(tabel_basis_wide, "10 rapporten/03 rapporten Noord/tabel_scores_kernindicatoren.xlsx")


y <- bind_rows(tabel_basis_sum, tabel_basis_groei) |>
  add_column(
    spatial_type = 'buurten')|>
  
  rename("value" = "value_kl_naam")|>
  group_by(meting)|>
  mutate(
    value_kl      = gtools::quantcut(value, na.rm = T),
    value_kl_naam = gtools::quantcut(value, na.rm = T, labels = labels_score)
  )



# alle buurten
kaart_buurten <- kaart_buurt |>
  filter(stadsdeelCode == 'N')|>
  
  filter(code %in% focusbuurten) |>
  left_join(
    y,  
    by = c("code" = "spatial_code")
  )|>
  mutate(value_kl_naam = case_when(
    is.na(value_kl_naam) ~ 'onbekend',
    TRUE ~ value_kl_naam)
  )|>
  mutate(value_kl_naam = factor(
    value_kl_naam, 
    levels = labels_score)
  )

# alle wijken
kaart_wijken <- kaart_wijk |>
  filter(stadsdeelCode == 'N')

# plot
ggplot()+
  
  geom_sf(data = kaart_wijken, fill = "grey" , color = "white", linewidth = 0.7)+
  
  geom_sf(data = kaart_buurten, aes(fill = value_kl_naam) , color = "white", linewidth = 0.7)+
  
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_manual(name = NULL, values  = c("#53b361", "#cdde87", "#f6bd57", "#ec0000")) +
  guides(
    colour = "none",
    fill = guide_legend(title = NULL, reverse = F, nrow = 1))+
  theme_os3(legenda_pos = "bottom")+
  facet_wrap(~ meting, ncol= 2)
plot

ggsave(filename = glue::glue(
  "10 rapporten/03 rapporten Noord/figuren/kaart_totaal_sscore.svg"),
  device = "svg", width = 8, height = 5)

### HEATMAP ---


tabel <- tabel_basis_def |>
  arrange(spatial_code, thema_noord_eenmeting)|>
  filter(
    indicator_sd != 'totaal')|>
  mutate(value_kl_naam = factor(
    value_kl_naam, 
    levels = c(1,2,3,4),
    labels = labels_score)
  )




  

z <- bind_rows(tabel,y)|>
  
  mutate(value_kl_nummer = case_when(
      
      value_kl_naam == "veel positiever" ~ 1,
      value_kl_naam == "positiever" ~ 2,
      value_kl_naam == "negatiever" ~ 3,
      value_kl_naam == "veel negatiever dan gemiddelde focusbuurten" ~ 4)
    )|>
  mutate(indicator_sd = case_when(
    
    variabele == "iminjong130_p"       ~ 'jongeren (0 - 17) in minimahuishoudens (%)',
    variabele == 'iminhh130_p'         ~ 'minimahuishoudens (%)',
    variabele == "ovveindi_p"          ~ 'kinderen (30 - 47 mnd.) met VVE-indicatie (%)',
    variabele == "startkwal_zonder_p"  ~ 'jongeren (18 - 26) zonder startkwalificatie (%)',
    TRUE ~ indicator_sd))|>
  mutate(indicator_sd = str_remove_all(indicator_sd, "oordeel "))|>
  mutate(indicator_df= glue::glue("{indicator_sd} {str_sub(thema_noord_eenmeting, 0,2)}"))|>
  mutate(thema_nr = as.numeric(str_sub(thema_noord_eenmeting,2,2)))|>
  mutate(thema_nr = replace_na(thema_nr, 8))


my_tyle_plot <- function (x = z){
  
  x |>
    filter(meting != 'verandering (%)')|>
    arrange(thema_noord_eenmeting)|>
    ggplot(aes(
      y = fct_rev(fct_reorder( indicator_df, thema_nr)),
      x = spatial_code,
      fill = value_kl_naam)) +
    theme_os2()+
    labs(title = NULL, x = NULL, y = NULL) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1) +  
    scale_fill_manual(name = NULL, values  = c("#53b361", "#cdde87", "#f6bd57", "#ec0000"))
}

z|>
  filter(meting=='1-meting')|>
my_tyle_plot()

ggsave(
  filename = "10 rapporten/03 rapporten Noord/figuren/kaart_heatmap_1.svg",
  device = "svg", width = 15, height = 7)

z|>
  filter(meting=='0-meting')|>
  my_tyle_plot()

ggsave(
  filename = "10 rapporten/03 rapporten Noord/figuren/kaart_heatmap_0.svg",
  device = "svg", width = 15, height = 7)

