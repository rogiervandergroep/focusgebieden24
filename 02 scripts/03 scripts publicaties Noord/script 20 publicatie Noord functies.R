

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

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


# script lijnfiguur Noord (geschikt voor data met meerdere jaren )
my_line_plot<- function(x, var, wijkcode){
  
  
  titel_plot <- x|>
    ungroup()|>
    filter(
      !is.na(value),
      tweedeling_def == 'totaal',
      variabele == var,
      (spatial_name %in% c("Amsterdam", "Noord") | str_detect(spatial_code, wijkcode))
    )|>
    select(indicator_sd)|>
    distinct()|>
    pull()
  
  x|>
    
    filter(
      !is.na(value),
      tweedeling_def == 'totaal',
      variabele %in% var,
      (spatial_name %in% c("Amsterdam", "Noord") | str_detect(spatial_code, wijkcode))
    )|>
    
    ggplot(aes(
      y = value, 
      x = as.character(temporal_date), 
      label = round(value),
      group = spatial_name)) +
    
    geom_line(aes(
      color = fct_reorder(spatial_name, spatial_type_code))) +
      labs(title = titel_plot , x = NULL, y = NULL) +
      theme_os2(orientation="vertical")+ 
      scale_color_manual(name= NULL, values  = wild_pal) +
      guides(color      = guide_legend(reverse = F, nrow = 3))+
      facet_wrap( ~ fct_rev(facet_buurt), scales = 'free_x')

    }

# script met staaf voor enkele jaren 
my_bar_plot<- function(x, var, wijkcode){
  
  
  titel_plot <-   x|>
    ungroup()|>
    filter(
      !is.na(value),
      tweedeling_def == 'totaal',
      variabele == var,
      (spatial_name %in% c("Amsterdam", "Noord") | str_detect(spatial_code, wijkcode))
    )|>
    select(indicator_sd)|>
    distinct()|>
    pull()
  
  x|>
    filter(
      !is.na(value),
      tweedeling_def == 'totaal',
      variabele %in% var,
      (spatial_name %in% c("Amsterdam", "Noord") | str_detect(spatial_code, wijkcode))
    )|>
    
    ggplot(aes(
      y = value, 
      x = fct_relevel(spatial_name, "Amsterdam",  "Noord"), 
      label = round(value),
      fill = as.character(temporal_date))) +
    
    geom_col(
      position = 'dodge', width = 0.5) +
    
    #geom_text(family = font, vjust = -.5, check_overlap = T)+
    labs(title = titel_plot , x = NULL, y = NULL) +
    theme_os2(orientation="vertical")+ 
    scale_fill_manual(name= NULL, values  = wild_pal) +
    guides(fill = guide_legend(reverse = F))+
    facet_wrap(~ fct_rev(facet_buurt), scales = 'free_x', ncol = 2) 
}

# NM01 (Blauwe Zand) wordt toegevoegd aan NH (Tuindorp Buiksloot)

### illustratieve kaartjes ---
my_plot<- function(wc){
  
  aanpak_noord_buurten <- read.xlsx(
    "../data/01 indicatoren/tabel_aanpak_noord_buurten.xlsx")|>
    select(spatial_code, aanpak_noord_buurt)  |>
    mutate(
      spatial_code = str_replace_all(spatial_code, 'NM01', 'NH99'),
    ) 
  
  # alle buurten
  kaart_buurten <- os_get_geom("buurten")|>
    filter(stadsdeelCode == 'N')|>
    mutate(
      code = str_replace_all(code, 'NM01', 'NH99'),
      wijkCode = str_replace_all(wijkCode, 'NM', 'NH')
      )|>
    left_join(aanpak_noord_buurten, by = c("code"="spatial_code"))|>
    filter(aanpak_noord_buurt == TRUE)
  
  # buurtselectie
  kaart_buurt<- kaart_buurten |>
    filter(wijkCode==wc)
  
  # buurt naam bij titel
  kaart_buurt_namen <- kaart_buurt |>
    pull(naam)
  
  # alle wijken 
  kaart_wijken <- os_get_geom("wijken")|>
    filter(stadsdeelCode == 'N')
  
  # wijkselectie
  kaart_wijk<- kaart_wijken |>
    filter(code == wc) 
  
  # plot
  plot<-ggplot()+
    geom_sf(data = kaart_wijken,  fill = "#cfcfcf", color = "white", linewidth = 0.7)+
    geom_sf(data = kaart_buurten, fill = "#b8bcdd", color = "white", linewidth = 0.7)+
    geom_sf(data = kaart_buurt,   fill = "#004699", color = "white", linewidth = 0.7)+ 
    geom_sf(data = kaart_wijk,    fill = NA,        color = "red",   linewidth = 0.7)+
    
    theme_os(legend_position = "none")+
    labs(title = NULL , x = NULL, y = NULL) +
    guides(fill = guide_legend( title = NULL))+
    theme(
      axis.line = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank(), 
      panel.background = element_blank(), 
      panel.grid = element_blank(), 
      panel.spacing = unit(0, "lines"), 
      plot.background = element_blank()
    )
  
  return(plot)
}

### kaartjes met data ---
my_plot_ind <- function(x, var_sel, wc){
  
  
  y <- x |>
    filter(variabele == var_sel)|>
    filter(temporal_date == max(temporal_date))|>
    mutate(
      value_kl         = gtools::quantcut(value, na.rm = T),
      value_kl_labels  = gtools::quantcut(value, na.rm = T, labels = c("veel lager", "lager", "hoger", "veel hoger"))
      )
  
  
  titel_plot <-   y|>
    select(indicator_sd)|>
    distinct()|>
    pull()
  
  jaar_plot <-   y|>
    select(temporal_date)|>
    distinct()|>
    pull()
  
  aanpak_noord_buurten <- read.xlsx(
    "../data/01 indicatoren/tabel_aanpak_noord_buurten.xlsx")|>
    select(spatial_code, aanpak_noord_buurt)|>
    mutate(spatial_code = str_replace_all(spatial_code, 'NM01', 'NH99')
    ) 
  
  # alle buurten
  kaart_buurten <- os_get_geom("buurten")|>
    filter(stadsdeelCode == 'N')|>
    mutate(
      code = str_replace_all(code, 'NM01', 'NH99'),
      wijkCode = str_replace_all(wijkCode, 'NM', 'NH')
      )|>
    left_join(aanpak_noord_buurten, by = c("code"="spatial_code"))|>
    filter(aanpak_noord_buurt == TRUE) |>
    left_join(y,  by = c("code"="spatial_code"))
  
  # alle wijken 
  kaart_wijken <- os_get_geom("wijken")|>
    filter(stadsdeelCode == 'N')
  
  # wijkselectie
  kaart_wijk<- kaart_wijken |>
    filter(code==wc) 
  
  
  grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))

  font <- "Amsterdam Sans"
  
  ondertitel = 'ten opzichte van de andere Aanpak Noord-buurten'

  # plot
  plot<-ggplot()+
    
    geom_sf(data = kaart_wijken,  fill = "#cfcfcf", color = "white", linewidth = 0.7)+

    geom_sf(data = kaart_buurten, aes(fill = value_kl_labels) , color = "white", linewidth = 0.7)+
    
    geom_sf(data = kaart_wijk,  fill = NA, color = "red", linewidth = 0.7)+
    
    geom_sf_text(data = kaart_buurten, aes(label = round(value,1)) , family = font, check_overlap = F)+
    
    theme_os()+
    labs(title = glue::glue("{titel_plot} in {jaar_plot}") , subtitle = ondertitel, x = NULL, y = NULL) +
    scale_fill_manual(values  = blauw_pal[c(9,7,5,3,1)]) +
    guides(fill = guide_legend( reverse = F, nrow = 1))+
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
      plot.background = element_blank()
    )
  
  return(plot)
}


##################################
### scripts algemene factsheet noord ---
##################################


# staafdiagram voor laatste jaar, voor de aanpak noord buurten
my_bar_plot_alg<- function(x, var, jaar_var){
  
  driekleur <- c("#ec0000", "#004699", "#00a03c")
  
  #position = position_nudge(x = - 0.5), 
  
  jaar_plot <-   x|>
    ungroup()|>
    filter(temporal_date == max(temporal_date) )|>
    filter(variabele %in% var)|>
    select(temporal_date)|>
    distinct()|>
    pull()
  
  x|>
    filter(
      temporal_date %in% c(max(temporal_date), jaar_var),
      variabele %in% var)|>
    

    ggplot(aes(
      x = value, 
      y = fct_relevel(fct_reorder (spatial_name, value), "Amsterdam",  "Noord"), 
      label = round(value,1))) +
    
    geom_col(fill = driekleur[2]) +
    
    geom_text(family = font, position = position_stack(vjust = 0.8), color = 'white')+
    labs(y=NULL, y = NULL, x = glue::glue("jaartal {jaar_plot}")) +
    theme_os2(orientation="horizontal")
}

### kaartjes met data ---
my_plot_ind_alg<- function(x, var_sel, jaar_var ){
  
  y <- x |>
    filter(
      variabele %in% var_sel,
      temporal_date %in% c(max(temporal_date), jaar_var)
      )|>
    mutate(
      value_kl         = gtools::quantcut(value, na.rm = T),
      value_kl_labels  = gtools::quantcut(value, na.rm = T, labels = c("veel lager", "lager", "hoger", "veel hoger"))
    )
  
  aanpak_noord_buurten <- read.xlsx(
    "01 indicatoren/tabel_aanpak_noord_buurten.xlsx")|>
    select(spatial_code, aanpak_noord_buurt)
  
  # alle buurten
  kaart_buurten <- os_get_geom("buurten")|>
    filter(stadsdeelCode == 'N')|>
    left_join(aanpak_noord_buurten, by = c("code"="spatial_code"))|>
    filter(aanpak_noord_buurt == TRUE) |>
    left_join(y,  by = c("code"="spatial_code")
    )
  
  # alle wijken 
  kaart_wijken <- os_get_geom("wijken")|>
    filter(stadsdeelCode == 'N')
  
  grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
  
  font <- "Amsterdam Sans"
  
  # plot
  plot<-ggplot()+
    
    geom_sf(data = kaart_wijken, fill = "grey" , color = "white", linewidth = 0.7)+
    
    geom_sf(data = kaart_buurten, aes(fill = value_kl_labels) , color = "white", linewidth = 0.7)+
    
    geom_sf_text(data = kaart_buurten, aes(label = round(value,1)) , family = font, check_overlap = F)+
    
    theme_os()+
    labs(title = NULL,  x = NULL, y = NULL) +
    scale_fill_manual(values  = blauw_pal[c(9,7,5,3,1)]) +
    guides(fill = guide_legend( reverse = F, nrow = 1))+
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
      plot.background = element_blank()
    )
  
  return(plot)
}


### kaartjes met tekstlabel ---

# aanpak noord buurten
aanpak_noord_buurten <- read.xlsx(
    "01 indicatoren/tabel_aanpak_noord_buurten.xlsx")|>
    select(spatial_code, aanpak_noord_buurt)
  
# alle buurten
kaart_buurten <- os_get_geom("buurten")|>
  filter(stadsdeelCode == 'N')|>
  left_join(aanpak_noord_buurten, by = c("code"="spatial_code"))|>
  filter(aanpak_noord_buurt == TRUE)
  
# alle wijken 
kaart_wijken <- os_get_geom("wijken")|>
  filter(stadsdeelCode == 'N')
  
grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
  
font <- "Amsterdam Sans"
  
blauw <- c("#004699", "#959dcc", "#b8b8b8")


  
  # plot
ggplot()+
    
    geom_sf(data = kaart_wijken, fill = blauw[3] , color = "white", linewidth = 0.7)+
    
    geom_sf(data = kaart_buurten, fill = blauw[1], color = "white", linewidth = 0.7)+
    
    geom_sf_text(data = kaart_buurten, aes(label = code) , color = "white", family = font, check_overlap = F)+
    
    theme_os()+
    labs(title = NULL,  x = NULL, y = NULL) +
    guides(fill = guide_legend( reverse = F, nrow = 1))+
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
      plot.background = element_blank()
    )
ggsave("20 figuren rapport noord/fig_kaart_buurtnamen.svg", width = 10, height = 5)    






