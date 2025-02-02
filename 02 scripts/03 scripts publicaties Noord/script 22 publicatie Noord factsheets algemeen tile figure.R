





### functie om kwartielen te berekenen
my_quantcut <- function (x , var) {
  
  y <- x |>
    
    filter(
      # spatial_name != 'Amsterdam',
      # spatial_name != 'Noord',
      temporal_date == max(temporal_date),
      variabele %in% var
    )|> 
    
    mutate(
      value_kl= gtools::quantcut(value, na.rm = T)
    )
  
  if (var %in% var_neg){
    
    y <- y |>
      mutate(
        value_kl_naam = gtools::quantcut(
          value, na.rm = T, labels = c(4,3,2,1)
        )
      )
    
    
  } else {
    
    y <- y |>
      mutate(
        value_kl_naam = gtools::quantcut(
          value, na.rm = T, labels = c(1,2,3,4)
        )
      )
    
    
  } 
  
  return (y)
  
}


# basis
var_basis <- c(
  "bevtotaal_groei","bevhhmkind_p","wvoorrbag_groei")

# negatief
var_neg <- c(
  "beveenouderhh_p","wlabelefg_p", "wzdepr_p", "neet_jongeren_p",
  "wzzwaar_p","skkwets34_p")

# positief
var_pos <- c(
  "wonderhoudwoning_r", "wwoning_r","lbetrokken_r","lbuurt_r", "vveiligavond_r",
  "bhwinkelaanbod_r", "startkwalificatie_p","orpubgroen_p", "werkopl_1866_p",
  "ihhink_gem","ostrftaal_p","ovvedeeln_p","wzgezond_p")     



# toevoeing ranking aan indicatoren
tabel_basis <- bind_rows(
  
  var_basis |>
    map_df(\(x) my_quantcut(data_noord_def, x)),
  
  var_pos |>
    map_df(\(x) my_quantcut(data_noord_def, x)),
  
  var_neg |>
    map_df(\(x) my_quantcut(data_noord_def, x))
  
)

tabel_basis_sel <- tabel_basis |>
  
  ungroup()|>
  # mutate(value_kl_naam= as.numeric(value_kl_naam))|>
  select(
    thema_noord_eenmeting, variabele, indicator_sd, spatial_name,value, value_kl, value_kl_naam)

write.xlsx(tabel_basis_sel, "04 tabellen/tabellen noord/tabel_scores_kernindicatoren.xlsx")

hcl <- farver::decode_colour(blauw_pal, "rgb", "hcl")

label_col <- ifelse(hcl[, "l"] > 50, "black", "white")





my_tile_figure <- function(x, indicatoren){
  
  x|>
    
    filter(
      variabele %in% indicatoren) |>
    
    ggplot(aes(
      x = fct_relevel(variabele, indicatoren),
      y = fct_relevel(fct_reorder(spatial_name, value, .fun = sum), c( "Amsterdam","Noord")),
      fill = value_kl_naam)
    ) +
    geom_tile(color = "white",lwd = 0.9,linetype = 1)+
    geom_text(
      aes(
        label = value,
        color = value_kl_naam), 
      family = font)+
    labs(title = NULL, x=NULL, y = NULL)+
    scale_x_discrete(position = "top", guide = guide_axis(angle = 45))+
    scale_fill_manual(name=NULL, values  = blauw_pal[c(9,7,5,3,1)])+
    scale_color_manual(name=NULL, values  = label_col[c(9,7,5,3,1)])+
    theme_os2(
      legend_position = 'none')+
    coord_fixed(0.3)
}

### inkomen 
tabel_basis_sel|>
  my_tile_figure(indicatoren = c(
    "ihhink_gem",  "iminhh130_p", "skkwets34_p","wwoning_r",'wonderhoudwoning_r', 
    "lbetrokken_r","lbuurt_r", "vveiligavond_r", 'bhwinkelaanbod_r',
    "wzgezond_p",   "startkwalificatie_p","neet_jongeren_p")
  )
ggsave("20 figuren rapport noord/tile_noord_inkomen.svg", width = 16, height = 10)


tabel_basis_sel |>
  filter(variabele %in% c(
    "ihhink_gem", "iminhh130_p", "skkwets34_p", "wwoning_r", "lbetrokken_r", 
    'wonderhoudwoning_r',"vveiligavond_r", 'bhwinkelaanbod_r',
    "wzgezond_p", "startkwalificatie_p","neet_jongeren_p")
  )|>
  select(variabele, indicator_sd )|>
  distinct()|>
  write.xlsx("04 tabellen/tabellen noord/kernindicatoren_geom_tile.xlsx")

