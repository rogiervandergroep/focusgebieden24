

source("http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R")

grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
font <- "Amsterdam Sans"

wild_pal <-c(
 "#ec0000", "#004699", "#00a03c", "#ffe600", 
 "#ff9100", "#6cbd74", "#fdb0cb", "#bed200", "#d48fb9", "#a00078")

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
theme_os3 <- function () {
  
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
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      panel.border = element_rect(fill = "transparent", color = NA),
      plot.background = element_blank(),
      legend.position="bottom",
  )
  
  
  
  
}

# lijst met aanpaknoord-buurten
aanpak_noord_buurten <- read.xlsx(
  "01 indicatoren/overzicht focusbuurten noord en nieuwwest.xlsx")|>
  select(spatial_code, focusbuurt)

# basiskaartjes
kaart_buurten <- os_get_geom("buurten")
kaart_wijken <- os_get_geom("wijken")

##############################
### scripts rapport definitief  ---
##############################

# script lijnfiguur Noord (geschikt voor data met meerdere jaren )
my_line_plot <- function(x, var){
  
  sp_4 <- c("Amsterdam", "Noord", "gemiddelde focusbuurten", "gemiddelde overige buurten" )
  
  plot <- x|>
    filter(
      !is.na(value),
      tweedeling_def == 'totaal',
      variabele %in% var,
      spatial_name %in% sp_4
    )|>
    
    ggplot(aes(
      y = value, 
      x = as.character(temporal_date), 
      label = round(value),
      group = spatial_name)) +
    
    geom_line(aes(
      color = fct_relevel(spatial_name, sp_4))) +
    labs(title = NULL , x = NULL, y = NULL) +
    theme_os2(orientation = "vertical") + 
    scale_color_manual(name = NULL, values = wild_pal) +
    guides(color = guide_legend( reverse = F, nrow = 2))+
    facet_wrap(~ indicator_sd, scales = 'free')
  
  ggsave(plot = plot, filename = glue::glue(
    "10 rapporten/03 rapporten Noord/20 figuren rapport noord/line_{var[1]}.svg"),
    device = "svg", width = breed, height = hoog)
    }

### kaartjes op buurtniveau ---
# default afronding = 0
my_plot_ind_alg <- function(x, var, afr = 0){

  y <- data_noord_def |>
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
      value_kl_labels  = gtools::quantcut(value, na.rm = T, labels = c("veel lager", "lager", "hoger", "veel hoger"))
    )
      
  # alle buurten
  kaart_buurten <- kaart_buurten |>
    filter(stadsdeelCode == 'N')|>
    left_join(
      aanpak_noord_buurten, 
      by = c("code" = "spatial_code"))|>
    filter(focusbuurt == TRUE) |>
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
      levels = c("veel lager", "lager", "hoger", "veel hoger", "onbekend"))
      )

  # alle wijken
  kaart_wijken <- kaart_wijken |>
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
    scale_fill_manual(name = NULL, values  = c("#cacde6", "#959dcc", "#5d6fb3", "#004699", "#e6e6e6")) +
    scale_color_manual(name = NULL, values = c("#000000", "#000000", "#ffffff", "#ffffff", "#000000"))+
    guides(
      colour = "none",
      fill = guide_legend(title = NULL, reverse = F, nrow = 1))+
    theme_os3()
  
  ggsave(plot = plot, filename = glue::glue(
    "10 rapporten/03 rapporten Noord/20 figuren rapport noord/kaart_{var[1]}.svg"),
    device = "svg", width = breed, height = hoog)
  
}



#### illustratief kaartje ---




### illustratieve kaart met namen ---
kaart_wijk <- os_get_geom("wijken")|>
  filter(stadsdeelCode == 'N')|>
  mutate(naam_code = glue::glue("{code} {naam}"))

kaart_geb <- os_get_geom("gebieden")|>
  filter(stadsdeelCode == 'N')  

kaart_buurt <- os_get_geom("buurten")|>
  filter(stadsdeelCode == 'N')  


discreet <- c("#004699", "#009dec", "#6cbd74", "#bed200", "#ffe600", "#fdb0cb",  "#ff9100", "#ec0000", "#004699", "#009dec", "#6cbd74", "#bed200", "#ffe600", "#d48fb9","#bed200" )

cols <- c("a" = "white", "b" = "black")

ggplot()+
  
  geom_sf(data = kaart_wijk, color = "white", aes(fill = fct_reorder(naam_code, code)) , linewidth = 1.0) +
  
  geom_sf(data = kaart_buurt, color = "white", fill = NA, linewidth = 0.8) + 
  
  geom_sf_text(
    data = kaart_buurt,
    aes(
      label = code , 
      colour = if_else(
        (str_detect(code , "NA") | str_detect(code , "NH") | str_detect(code , "NJ") ), "a", "b")
    ),
    size = 3, family = font, check_overlap = F) +
  
  theme_os()+
  labs(title = NULL,  x = NULL, y = NULL) +
  scale_fill_manual(values  = discreet) +
  scale_colour_manual(values  = cols) +
  theme(
    legend.position = 'bottom',
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
  )+
  guides(
    colour = "none",
    fill = guide_legend(ncol = 4)     )
  


ggsave("10 rapporten/03 rapporten Noord/20 figuren rapport noord/kaart_wijknamen_noord.svg", device = "svg",  width = 12, height = 7)



kaart_buurt |>
  st_drop_geometry()|>
  select(code, naam)|>
  write.xlsx("10 rapporten/03 rapporten Noord/20 figuren rapport noord/buurtnaam.xlsx")












