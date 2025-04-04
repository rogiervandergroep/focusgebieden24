library(tidyverse)
library(sf)

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
font <- "Amsterdam Sans"

wild_pal_nw <-c(
  "#ec0000", "#004699", "#00a03c", "#ffe600", 
  "#ff9100", "#6cbd74", "#fdb0cb", "#bed200", "#d48fb9", "#a00078")

wild_pal_no <-c(
  "#ec0000","#004699","#00a03c",
  "#ffe600","#ff9100","#6cbd74","#fdb0cb","#bed200", "#d48fb9", "#a00078")


hcl <- farver::decode_colour(blauw_pal, "rgb", "hcl")

label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

# theme voor lijnddiagram
theme_os2 <- function(
    orientation="horizontal", 
    legend_position = "bottom"){
  
  theme <- theme_bw() +
    theme(
      text = element_text(family = font, size = 12),
      axis.text = element_text(family = font, size = 12),
      plot.caption = element_text(family = font, size = 12),
      axis.title = element_text(family = font, hjust = 1, size = 12),
      plot.subtitle = element_text(family = font, size = 12),
      legend.text = element_text(family = font, size = 12),
      plot.title = element_text(family = font, lineheight = 1.2, size = 12),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      legend.title=element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position=legend_position,
      panel.border = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(color = "black", family = font, face = "bold", size = 12)
    ) 
  
  if (orientation %in% c("vertical", "v")){
    theme <- theme + theme(panel.grid.major.x = element_blank())
  } else if (orientation %in% c("horizontal", "h")){
    theme <- theme + theme(panel.grid.major.y = element_blank())
  }
  
}

# theme voor geokaartjes
theme_os3 <- function (legenda_pos) {
  
  theme_bw() +
    theme(
      plot.subtitle = element_text(family = font, size = 12),
      legend.text = element_text(family = font, size = 12),
      plot.title = element_text(family = font, lineheight = 1.2, size = 14),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(family = font, lineheight = 1.2, size = 12),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      panel.border = element_rect(fill = "transparent", color = NA),
      plot.background = element_blank(),
      legend.position = legenda_pos,
    )
  
  
  
  
}

#####################################
### illustratieve kaart met namen ---
#####################################

# basiskaarten wijk
kaart_wijk  <- os_get_geom("wijken")|>
  mutate(naam_code = glue::glue("{code} {naam}"))

kaart_buurt <- os_get_geom("buurten")|>
  mutate(naam_code = glue::glue("{code} {naam}"))


discreet <- c(
  "#004699", "#009dec", "#6cbd74", "#bed200", "#ffe600", "#fdb0cb", 
  "#ff9100", "#ec0000", "#004699", "#009dec", "#6cbd74", "#bed200",
  "#ffe600", "#d48fb9", "#ec0000", "#ff9100")

# functie om illustratieve kaarten te maken met de buurtcodes en wijknamen
my_kaart <- function(stadsdeel){
  
  kaart_wijk  <- kaart_wijk |>
    filter(stadsdeelNaam == stadsdeel)
  
  kaart_buurt <- kaart_buurt |>
    filter(stadsdeelNaam == stadsdeel)
  
  if (stadsdeel == 'Zuidoost' ){
    map  <- "02 rapporten Zuidoost"
  } else if (stadsdeel == 'Noord'){
    map  <- "03 rapporten Noord"
  } else if (stadsdeel == 'Nieuw-West'){
    map  <- "04 rapporten Nieuw-West"
  }
  
  ggplot()+
    
    geom_sf(data = kaart_wijk, color = "white", aes(fill = fct_reorder(naam_code, code)) , linewidth = 1.1) +
    
    geom_sf(data = kaart_buurt, color = "white", fill = NA, linewidth = 0.9) + 
    
    geom_sf_text(data = kaart_buurt, aes(label = code), size = 3, family = font, check_overlap = F) +
    
    #theme_os()+
    labs(title = NULL,  x = NULL, y = NULL) +
    scale_fill_manual(name = NULL, values  = discreet) +
    theme_os3(legenda_pos = legenda_pos)+
    guides(
      fill = guide_legend(ncol = aantal_kolommen)
    )
  
  ggsave(glue::glue("10 rapporten/{map}/figuren/kaart_wijknamen_{stadsdeel}.svg"), device = "svg",  width = breed, height = hoog)
  
}

# functie om illustratieve kaarten te maken met de buurtcodes van de focusbuurten
my_focus_buurt_kaart <- function(stadsdeel){
  
  kaart_wijk  <- kaart_wijk |>
    filter(stadsdeelNaam == stadsdeel)
  
  kaart_buurt <- kaart_buurt |>
    filter(code %in% focusbuurten)
  
  kleur <-rep(c("#004699"), 30)
  
  if (stadsdeel == 'Zuidoost' ){
    map  <- "02 rapporten Zuidoost"
  } else if (stadsdeel == 'Noord'){
    map  <- "03 rapporten Noord"
  } else if (stadsdeel == 'Nieuw-West'){
    map  <- "04 rapporten Nieuw-West"
  }
  
  ggplot()+
    
    geom_sf(data = kaart_wijk, fill = "grey" , color = "white", linewidth = 0.7)+
    
    geom_sf(data = kaart_buurt, aes(fill = fct_reorder(naam_code, code)), color = "white", linewidth = 0.7) +
    
    geom_sf(data = kaart_buurt, fill = "#004699",  color = "white", linewidth = 0.7)+
    
    geom_sf_text(
      aes(label = code),
      data = kaart_buurt,
      color = "white",
      family = font, check_overlap = F)+
    
    labs(title = NULL, x = NULL, y = NULL)+
    scale_fill_manual(name = NULL, values  = kleur) +
    guides(

      fill = guide_legend(ncol = aantal_kolommen))+
    theme_os3(legenda_pos = legenda_pos)
    
  ggsave(glue::glue("10 rapporten/{map}/figuren/kaart_focusbuurten_{stadsdeel}.svg"), device = "svg",  width = breed, height = hoog)
  
}

##############################
# plots en thematische kaarten
##############################


# functie lijnfiguur basis
my_line_plot <- function(x, var, facet_var = NULL){
  
  
  if (stadsdeel == 'Zuidoost' ){
    map  <- "02 rapporten Zuidoost"
  } else if (stadsdeel == 'Noord'){
    map  <- "03 rapporten Noord"
  } else if (stadsdeel == 'Nieuw-West'){
    map  <- "04 rapporten Nieuw-West"
  }
  
  
  plot <- x|>
    
    filter(
      !is.na(value),
      tweedeling_def == 'totaal',
      variabele %in% var)|>
    
    ggplot(aes(
      y = value, 
      x = as.character(temporal_date), 
      label = round(value),
      group = spatial_name)) +
    
    geom_line(aes(
      color = spatial_name)) +
    labs(title = NULL , x = NULL, y = NULL) +
    theme_os2(orientation="vertical")+ 
    scale_color_manual(name= NULL, values  = wild_pal) +
    guides(color = guide_legend(reverse = F, nrow = 1))+
    expand_limits(y = 0)+
    facet_wrap(vars({{facet_var}}))
  
  ggsave(glue::glue("10 rapporten/{map}/figuren/line_{var[1]}.svg"), width = breed, height = hoog)
  
}

# functie lijnfiguur alleen voor noord voor gemiddelde focusbuurten vs overige buurten
my_line_plot_noord <- function(x, var, schaal = 'free', ymin = 0, ymax = NA){
  
  sp_4 <- c("Amsterdam", "Noord", "gemiddelde focusbuurten", "gemiddelde overige buurten" )
  
  plot <- x|>
    filter(
     # !is.na(value),
      tweedeling_def == 'totaal',
      variabele %in% var,
      spatial_name %in% sp_4
    )|>
    
    ggplot(aes(
      y = value, 
      x = as.character(temporal_date), 
      label = round(value),
      group = spatial_name,
      color = fct_relevel(spatial_name, sp_4))) +
    
    geom_line() +
    
    geom_point() +
    
    labs(title = NULL , x = NULL, y = NULL) +
    ylim(ymin,ymax)+
    theme_os2(orientation = "vertical") + 
    scale_color_manual(name = NULL, values = wild_pal) +
    guides(color = guide_legend( reverse = F, nrow = 2))+
    facet_wrap(~ indicator_sd, scales = schaal)
  
  ggsave(plot = plot, filename = glue::glue(
    "10 rapporten/03 rapporten Noord/figuren/line_{var[1]}.svg"),
    device = "svg", width = breed, height = hoog)
}

# functie staafdiagram DEF horizontaal -
my_bar_plot <- function(x, var,  jaar = NULL, fill_var = NULL, facet_var, schaal = 'fixed' ){
  
  driekleur <- c("#ec0000", "#004699", "#00a03c")
  
  if (stadsdeel == 'Zuidoost' ){
    map  <- "02 rapporten Zuidoost"
  } else if (stadsdeel == 'Noord'){
    map  <- "03 rapporten Noord"
  } else if (stadsdeel == 'Nieuw-West'){
    map  <- "04 rapporten Nieuw-West"
  }
  
  volgorde <- c(
    "Amsterdam", 
    glue::glue("{stadsdeel}"), 
    glue::glue("{stadsdeel} (totaal)"), 
    glue::glue("{stadsdeel} (zittende bewoner)"),
    glue::glue("{stadsdeel} (nieuwe bewoner)")
  )
  
  plot <- x|>
    filter(
      temporal_date %in% c(max(temporal_date), jaar),
      variabele %in% var)|>
    
    ggplot(aes(
      x = value,
      y = fct_relevel(
        fct_reorder (spatial_name, value), volgorde),
      label = round(value, 1),
      fill = {{fill_var}})) +
    
    geom_col(
      aes(fill = {{fill_var}}),
      position = position_dodge2(width = 0.9, preserve = "single")) +
    
    geom_text(
      family = font, 
      position = position_dodge2(width = 0.9, preserve = "single"),  
      hjust  = 1.7, 
      size =  3.5, 
      color = 'white')+
    labs(y = NULL, x = NULL) +
    theme_os2(orientation = "horizontal")+
    scale_fill_manual(name= NULL, values  = blauw_pal[c(2,4,6,8)])+
    facet_wrap(vars({{facet_var}}), scales = schaal, nrow = 1)
  
  ggsave(plot = plot, filename = glue::glue("10 rapporten/{map}/figuren/bar_{var[1]}.svg"), device = "svg",  width = breed, height = hoog)
}

# functie thematische kaart DEF -
my_kaart_plot <- function(x, var, afr, geo, jaar, facet_var){
  
  
  if (stadsdeel == 'Zuidoost' ){
    map  <- "02 rapporten Zuidoost"
  } else if (stadsdeel == 'Noord'){
    map  <- "03 rapporten Noord"
  } else if (stadsdeel == 'Nieuw-West'){
    map  <- "04 rapporten Nieuw-West"
  }
  
  y <- x |>
    
    filter(
      !is.na(value),
      spatial_type == geo,
      variabele %in% var,
      temporal_date %in% c(max(temporal_date), jaar))|>
    
    mutate(
      value_kl = gtools::quantcut(value, na.rm = T),
      value_kl_labels  = gtools::quantcut(value, na.rm = T,
        labels = c(
          glue::glue("veel lager dan gemiddelde {geo}"), 
          glue::glue("lager dan gemiddelde {geo}"), 
          glue::glue("hoger dan gemiddelde {geo}"), 
          glue::glue("veel hoger dan gemiddelde {geo}")))
    )
  
  # buurtkaart
  kaart <- os_get_geom(geo)|>
    filter(stadsdeelNaam == stadsdeel)|>
    left_join(y, by = c("code"="spatial_code"))|>
    filter(!is.na(value))
  
  kaart_basis <- os_get_geom(geo)|>
    filter(stadsdeelNaam == stadsdeel)  
  
  kaart_wijk <- os_get_geom("wijken")|>
    filter(stadsdeelNaam == stadsdeel) 
  
  # plot
  plot <- ggplot()+
    
    geom_sf(data = kaart_basis,  color = "white", fill = "#cfcfcf", linewidth = 0.9)+
    
    geom_sf(data = kaart, aes(fill = value_kl_labels) , color = "white", linewidth = 0.9)+
    
    geom_sf_text(data = kaart, aes(
      label = round(value, afr),
      color = value_kl_labels),      
      size = 4,
      lineheight = .8,
      family = font, check_overlap = F)+
    
    labs(title = NULL,  x = NULL, y = NULL) +
    
    scale_fill_manual(name = NULL, values = blauw_pal[c(9,7,5,3,1)]) +
    scale_color_manual(name = NULL, values = label_col[c(9,7,5,3,1)])+
    
    guides(
      colour = "none",
      fill = guide_legend( reverse = F, nrow = 2))+
    
    theme_os3(legenda_pos = legenda_pos)+
    facet_wrap(vars({{facet_var}}))
  
  ggsave(glue::glue("10 rapporten/{map}/figuren/kaart_{var[1]}.svg"), width = breed, height = hoog)
  
}

# functie thematische kaart Noord en gemiddelde focusbuurten en overige buurten -
my_kaart_plot_noord <- function(x, var, afr = 0){
  
  legenda_labels = c(
    "veel lager ","lager", "hoger", 
    "veel hoger dan gemiddelde focusbuurten"
    )

  y <- x |>
    ungroup()|>
    filter(
      spatial_type == 'buurten',
      spatial_code != 'NOVERIG',
      spatial_code != 'NFOCUS',
      variabele    %in% var
    )|>
    filter(temporal_date == max(temporal_date))|>
    mutate(
      value_kl         = gtools::quantcut(value, na.rm = T),
      value_kl_labels  = gtools::quantcut(value, na.rm = T, labels = legenda_labels)
    )
  
  # focusbuurten in Noord
  focusbuurten <- c(
    "NA07",  "NC01",  "NC02",  "NC03",  "NE01",  "NE02",  "NE03",  "NE04",
    "NH02",  "NH05",  "NJ02",  "NJ03",  "NJ05",  "NJ06",  "NK01",  "NK02",
    "NK03",  "NL01",  "NL03",  "NL04",  "NM01",  "NN01",  "NN02"
  )
  
  # alle buurten
  kaart_buurten <- kaart_buurt |>
    filter(stadsdeelCode == 'N')|>

    filter(code %in% focusbuurten) |>
    left_join(
      y,  
      by = c("code" = "spatial_code")
    )|>
    mutate(value_kl_labels = case_when(
      is.na(value_kl_labels) ~ 'onbekend',
      TRUE ~ value_kl_labels)
    )|>
    mutate(value_kl_labels = factor(
      value_kl_labels, 
      levels = legenda_labels)
    )
  
  # alle wijken
  kaart_wijken <- kaart_wijk |>
    filter(stadsdeelCode == 'N')
  
  # plot
  plot<-ggplot()+
    
    geom_sf(data = kaart_wijken, fill = "grey" , color = "white", linewidth = 0.7)+
    
    geom_sf(data = kaart_buurten, aes(fill = value_kl_labels) , color = "white", linewidth = 0.7)+
    
    geom_sf_text(
      data = kaart_buurten, 
      aes(label = round(value, afr), color = value_kl_labels), 
      family = font, check_overlap = F)+
    
    labs(title = NULL, x = NULL, y = NULL) +
    scale_fill_manual(name = NULL, values  = c("#cacde6", "#959dcc", "#5d6fb3", "#004699", "#e6e6e6"))+
    scale_color_manual(name = NULL, values = c("#000000", "#000000", "#ffffff", "#ffffff", "#000000"))+
    guides(
      colour = "none",
      fill = guide_legend(title = NULL, reverse = F, nrow = 1))+
    theme_os3(legenda_pos = legenda_pos)
  
  ggsave(plot = plot, filename = glue::glue(
    "10 rapporten/03 rapporten Noord/figuren/kaart_{var[1]}.svg"),
    device = "svg", width = 8, height = 4)
  
}

  




