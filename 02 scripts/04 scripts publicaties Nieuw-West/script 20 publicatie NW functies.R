

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

library(tidyverse)
library(sf)

grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
#grDevices::windowsFonts("Corbel" = grDevices::windowsFont("Corbel"))

font <- "Amsterdam Sans"

wild_pal <-c(
 "#ec0000","#004699","#00a03c",
  
 "#ffe600","#ff9100","#6cbd74","#fdb0cb","#bed200", "#d48fb9", "#a00078")


theme_os2 <- function(orientation="horizontal", legend_position = "bottom"){
  
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family = font, size = 12),
      axis.text = ggplot2::element_text(family = font, size = 12),
      plot.caption = ggplot2::element_text(family = font, size = 12),
      axis.title = ggplot2::element_text(family = font, hjust = 1, size = 12),
      plot.subtitle = ggplot2::element_text(family = font, size = 12),
      legend.text = ggplot2::element_text(family = font, size = 12),
      plot.title = ggplot2::element_text(family = font, lineheight = 1.2, size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.title=element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position=legend_position,
      panel.border = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.text = ggplot2::element_text(color = "black", family = font, face = "bold", size = 12)
    ) 
  
  if (orientation %in% c("vertical", "v")){
    theme <- theme + ggplot2::theme(panel.grid.major.x = element_blank())
  } else if (orientation %in% c("horizontal", "h")){
    theme <- theme + ggplot2::theme(panel.grid.major.y = element_blank())
  }
  
}

##############################
### scripts wijk factsheet ---
##############################


hcl <- farver::decode_colour(blauw_pal, "rgb", "hcl")

label_col <- ifelse(hcl[, "l"] > 50, "black", "white")


breed = 12
hoog = 7

# script lijnfiguur NW (geschikt voor data met meerdere jaren )
my_line_plot <- function(x, var, facet_var = NULL){
  
  plot<-x|>
    
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
      color = fct_reorder(spatial_name, spatial_type_code))) +
      labs(title = NULL , x = NULL, y = NULL) +
      theme_os2(orientation="vertical")+ 
      scale_color_manual(name= NULL, values  = wild_pal) +
      guides(color      = guide_legend(reverse = F, nrow = 2))+
    facet_wrap(vars({{facet_var}}))
  
  ggsave(glue::glue("10 rapporten/04 rapporten Nieuw-West/line_{var}.svg"), width = breed, height = hoog)

    }

# script stack NW (geschikt voor data met meerdere jaren )
my_stack_plot <- function(x, var){
  
  plot<-x|>
    filter(
      !is.na(value),
      tweedeling_def == 'totaal',
      variabele %in% var)|>
    
    ggplot(aes(
      y = value, 
      x = as.character(temporal_date), 
      label = round(value),
      fill  = fct_reorder(spatial_name, spatial_code))
      ) +
    
    geom_col() +
    labs(title = NULL , x = NULL, y = NULL) +
    theme_os2(orientation="vertical")+ 
    scale_fill_manual(name= NULL, values  = discreet) +
    guides(fill= guide_legend(reverse = T))
  
  ggsave(plot = plot, filename = glue::glue("10 rapporten/04 rapporten Nieuw-West/stack_{var}.svg"), width = breed, height = hoog)
  
}

# staafdiagram voor geselecteerde (default op null) en laatste jaar, op wijkniveau
my_bar_plot <- function(x, var, jaar = NULL, facet_var){

  driekleur <- c("#ec0000", "#004699", "#00a03c")

  #position <- position_nudge(x = - 0.5)

  jaar_plot <-   x|>
    ungroup()|>
    filter(temporal_date == max(temporal_date) )|>
    filter(variabele %in% var)|>
    select(temporal_date)|>
    distinct()|>
    pull()

  plot<-x|>
    filter(
      temporal_date %in% c(max(temporal_date), jaar),
      variabele %in% var)|>

    ggplot(aes(
      x = value,
      y = fct_relevel(fct_reorder (spatial_name, value), "Amsterdam",  "Nieuw-West"),
      label = round(value,1))) +

    geom_col(fill = driekleur[2]) +

    geom_text(family = font, position = position_stack(vjust = 0.8), color = 'white')+
    labs(y = NULL, y = NULL, x = glue::glue("jaartal {jaar_plot}")) +
    theme_os2(orientation = "horizontal")+
    facet_wrap(vars({{facet_var}}))
  
  var_name <- var[1]
  
  ggsave(plot = plot, filename = glue::glue("10 rapporten/04 rapporten Nieuw-West/bar_{var_name}.svg"), device = "svg",  width = breed, height = hoog)
}

# kaart 
my_kaart_plot <- function(x, var, afr, geo, jaar, facet_var){
  
  y <- x |>
    
    filter(
      !is.na(value),
      spatial_type == geo,
      variabele %in% var,
      temporal_date %in% c(max(temporal_date), jaar))|>
    
    mutate(
      value_kl         = gtools::quantcut(
        value, na.rm = T),
      value_kl_labels  = gtools::quantcut(
        value, na.rm = T, 
        labels = c("veel lager", "lager", "hoger", "veel hoger"))
    )
  
  # buurtkaart
  kaart <- os_get_geom(geo)|>
    filter(stadsdeelCode == 'F')|>
    left_join(y, by = c("code"="spatial_code"))|>
    filter(!is.na(value))
  
  kaart_basis <- os_get_geom(geo)|>
    filter(stadsdeelCode == 'F')  
  
  kaart_wijk <- os_get_geom("wijken")|>
    filter(stadsdeelCode == 'F') 
  
  grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
  
  font <- "Amsterdam Sans"
  
  # plot
  plot <- ggplot()+
    
    geom_sf(data = kaart_basis,  color = "white", fill = "#cfcfcf", linewidth = 0.9)+
    
    geom_sf(data = kaart, aes(fill = value_kl_labels) , color = "white", linewidth = 0.9)+
    
    geom_sf_text(data = kaart, aes(
      label = round(value, afr),
      color = value_kl_labels),
      lineheight = .8,
      family = font, check_overlap = F)+
    
    theme_os()+
    labs(title = NULL,  x = NULL, y = NULL) +
    scale_fill_manual(values  = blauw_pal[c(9,7,5,3,1)]) +
    scale_color_manual(name = NULL, values = label_col[c(9,7,5,3,1)])+
    guides(
      colour = "none",
      fill = guide_legend( reverse = F, nrow = 1))+
    theme(
      plot.subtitle = element_text(family = font, size = 12),
      legend.text = element_text(family = font, size = 12),
      plot.title = element_text(family = font, lineheight = 1.2, size = 14),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      plot.background = element_blank())+
    facet_wrap(vars({{facet_var}}))
  
  ggsave(glue::glue("10 rapporten/04 rapporten Nieuw-West/kaart_{var[1]}.svg"), width = breed, height = hoog)

}





### illustratieve kaart met namen ---
kaart_wijk <- os_get_geom("wijken")|>
    filter(stadsdeelCode == 'F')|>
  mutate(naam_code = glue::glue("{code} {naam}"))

kaart_geb <- os_get_geom("gebieden")|>
  filter(stadsdeelCode == 'F')  

kaart_buurt <- os_get_geom("buurten")|>
  filter(stadsdeelCode == 'F')  
  

discreet <- c("#004699", "#009dec", "#6cbd74", "#bed200", "#ffe600", "#fdb0cb",  "#ff9100", "#ec0000", "#004699", "#009dec", "#6cbd74", "#bed200", "#ffe600", "#d48fb9","#ec0000", "#ff9100" )


wit<- c(
  "FA01",
  "FA02",
  "FA03",
  "FA04",
  "FA05",
  "FA06",
  "FA07",
  "FA08",
  "FJ01",
  "FJ02",
  "FJ03",
  "FJ04")




ggplot()+
  
  geom_sf(data = kaart_wijk, color = "white", aes(fill = fct_reorder(naam_code, code)) , linewidth = 1.1) +
 
  geom_sf(data = kaart_buurt, color = "white", fill = NA, linewidth = 0.9) + 
  
  geom_sf_text(data = kaart_buurt, aes(label = code), size = 3, family = font, check_overlap = F) +
  
  theme_os()+
  labs(title = NULL,  x = NULL, y = NULL) +
  scale_fill_manual(values  = discreet) +
  theme(
    legend.position = 'right',
    plot.subtitle = element_text(family = font, size = 12),
    legend.text = element_text(family = font, size = 12),
    plot.title = element_text(family = font, lineheight = 1.2, size = 14),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    plot.background = element_blank()
  )


ggsave("10 rapporten/04 rapporten Nieuw-West/kaart_wijknamen.svg", device = "svg",  width = 9, height = 7)





 
# x |>
#   ggplot(aes(
#     
#     y = fct_rev(monitor),
#     group = fct_rev({{afzet_var}}),
#     x = value,
#     label = value))+
#   
#   geom_col(
#     aes(fill = {{afzet_var}}))+
#   
#   geom_text(
#     aes(
#       label = if_else(value > 10,as.character(round(value)),""),
#       color = {{afzet_var}}),
#     
#     position = position_stack(vjust =0.5),
#     family = font, 
#     lineheight = .8)+
#   
#   labs(
#     title=NULL, x=NULL, y = NULL) +
#   
#   theme_os2()+
#   
#   scale_fill_manual(
#     name= NULL, values = blauw_pal[c(2,3,5,6,7,8)])+
#   
#   scale_color_manual(
#     name= NULL, values = label_col[c(2,3,5,6,7,8)])+
#   
#   guides(
#     fill = guide_legend(
#       nrow =1, reverse = F), 
#     colour = "none") 
# 
#   