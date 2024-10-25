library(tidyverse)

  
data_buurt_score <- readxl::read_excel(
  "10 rapporten en documenten/leefbarometer ams.xlsx", 
  sheet = "leefbaarometer_scores_ams" )|>
  mutate(Column1 = str_replace_all(Column1, "-", " "))
  

tabel_buurt_oud <- read.csv("01 indicatoren/buurtindeling_oud.csv")|>
  mutate(naam = str_replace_all(naam, "-", " "))




oordeel<- c(
  "9 Uitstekend", "8 Zeer goed","7 Goed", "6 Ruim voldoende", "5 Voldoende",    
  "4 Zwak", "3 Onvoldoende", "2 Ruim onvoldoende", "1 Zeer onvoldoende","Onbekend")  


oordeel_goed      <- c("9 Uitstekend", "8 Zeer goed","7 Goed", "6 Ruim voldoende") 
  
oordeel_voldoende <- c("5 Voldoende")

oordeel_zwak      <- c("4 Zwak")

oordeel_slecht    <- c("3 Onvoldoende", "2 Ruim onvoldoende", "1 Zeer onvoldoende")





data_buurt_score2 <- data_buurt_score |>
  left_join(
    tabel_buurt_oud, by = c("Column1"="naam"))|>
  select(Column1,code, everything()) |>
  
  mutate(
    oordeel_2020 = case_when(
      `2020` %in% oordeel_goed         ~ '4 goed',
      `2020` %in% oordeel_voldoende    ~ '3 voldoende',
      `2020` %in% oordeel_zwak         ~ '2 zwak',
      `2020` %in% oordeel_slecht       ~ '1 slecht'),
    
    oordeel_2022 = case_when(
      `2022` %in% oordeel_goed         ~ '4 goed',
      `2022` %in% oordeel_voldoende    ~ '3 voldoende',
      `2022` %in% oordeel_zwak         ~ '2 zwak',
      `2022` %in% oordeel_slecht       ~ '1 slecht'),
    
    verschil_20_22 = case_when(
      oordeel_2020 %in% c('3 voldoende', '4 goed') & oordeel_2022 %in% c('3 voldoende', '4 goed')   ~ '6 voldoende of goed',
      oordeel_2020 %in% c('2 zwak', '1 slecht')    & oordeel_2022 %in% c('3 voldoende', '4 goed')   ~ '5 van zwak naar voldoende',
      oordeel_2020 %in% c('3 voldoende', '4 goed') & oordeel_2022 %in% c('2 zwak')                  ~ '4 van voldoende naar zwak',
      oordeel_2020 == '2 zwak'                     & oordeel_2022 %in% c('2 zwak')                  ~ '3 zwak',
      oordeel_2020 == '2 zwak'                     & oordeel_2022 %in% c('1 slecht')                ~ '2 van zwak naar slecht',
      oordeel_2020 == '1 slecht'                   & oordeel_2022 %in% c('1 slecht')                ~ '1 slecht')
      )



test_slecht<- data_buurt_score2 |>
  select(code, Column1, verschil_20_22) |>
  filter(verschil_20_22 == '1 slecht')


test_zw_slecht<- data_buurt_score2 |>
  select(code, Column1, verschil_20_22) |>
  filter(verschil_20_22 == '1 slecht')


test_slecht<- data_buurt_score2 |>
  select(code, Column1, verschil_20_22) |>
  filter(verschil_20_22 == '1 slecht')



test_slecht<- data_buurt_score2 |>
  select(code, Column1, verschil_20_22) |>
  filter(verschil_20_22 == '1 slecht')

write.xlsx(
  data_buurt_score2, "10 rapporten en documenten/buurten_leefbaar_20_22.xlsx")
      

        

url<- "https://gitlab.com/os-amsterdam/datavisualisatie-onderzoek-en-statistiek/-/raw/develop/public/geo/amsterdam/2015-2020/buurten-2015-2020-zw-geo.json"

grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))

font <- "Amsterdam Sans"

### kaartjes 



stoplicht <- c("#ec0000", "#f27d14", "#f6bd57", "#fff3a7", "#cdde87", "#96c86f",  "#e6e6e6")

# alle wijken 
kaart_wijken <- os_get_geom("wijken")

buurtkaart_oud <- st_read(url)

# figuur
kaart_buurten <- buurtkaart_oud |>

  left_join(data_buurt_score2, by = "code")


kaart_buurten |>
  
  ggplot()+
    
    geom_sf(aes(fill = verschil_20_22) , color = "grey", linewidth = 0.7)+
    geom_sf(data = kaart_wijken,  fill = NA, color = "white", linewidth = 0.9)+
    
    #geom_sf_text(data = kaart_buurten, aes(label = round(value,1)) , family = font, check_overlap = F)+
    
    theme_os()+
    labs(title = NULL,  x = NULL, y = NULL) +
    scale_fill_manual(values  = stoplicht)+
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
  
ggsave("10 rapporten en documenten/kaart_leefbaarheid.png")    
    


