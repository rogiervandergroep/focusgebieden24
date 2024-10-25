
library(tidyverse)
library(openxlsx)



### toevoeging 2021 Armoedemonitor data bminreg_p en bminregjong_p

# inlezen gebieden met gebiedscodes
# source("02 scripts/01 scripts bewerking data/script 00 basis gebiedsindelingen.R")
# 
# my_mutate <-function (x) {
#   
#   x |>
#   
#   mutate(
#     spatial_name = str_replace_all(spatial_name, "-", " "),
#     spatial_name = str_replace_all(spatial_name, " / ", " "),
#     spatial_name = str_replace_all(spatial_name, "/", " "),
#     spatial_name = str_replace_all(spatial_name, ",", " ")
#     )
# 
# }
# 
# geo_list <- geo_list|>
#   map(\(x) my_mutate(x))|>
#   map(\(x) select(x, spatial_code, spatial_name, spatial_date, spatial_type))
#   
# 
#                                   
#                             
# 
# df_reg_22 <- bind_rows(
#   
#   # op wijkniveau 
#   read.xlsx(
#     "00 ruwe data/230035_Armoedemonitor_2022_Stadsdeelrapportage.xlsx", 
#     sheet = "29", startRow = 2)|>
#   select(spatial_name, BMINREG_P, BMINREGJONG_P)|>
#   filter(spatial_name != 'Amsterdam',
#         (!is.na(BMINREG_P) & !is.na(BMINREGJONG_P)))|>
#   my_mutate()|>
#   left_join(geo_list$wijken_oud, by= "spatial_name")|>
#   pivot_longer(cols  = c(BMINREG_P,BMINREGJONG_P), names_to = 'measure' ),
# 
# # op gebiedniveau
#   read.xlsx(
#     "00 ruwe data/230035_Armoedemonitor_2022_Stadsdeelrapportage.xlsx", 
#     sheet = "20", startRow = 2 )|>
#   select(spatial_name, BMINREG_P, BMINREGJONG_P)|>
#   filter(spatial_name != 'Amsterdam', 
#          spatial_name != 'Weesp',
#          (!is.na(BMINREG_P) & !is.na(BMINREGJONG_P)))|>
#   my_mutate()|>
#   left_join(geo_list$gebieden_oud, by= "spatial_name")|>
#   pivot_longer(cols  = c(BMINREG_P,BMINREGJONG_P), names_to = 'measure' ),
# 
# # op stadsdeel niveau en Amsterdam
#   read.xlsx(
#     "00 ruwe data/230035_Armoedemonitor_2022_Stadsdeelrapportage.xlsx", 
#     sheet = "11", startRow = 2)|>
#   select(spatial_name, BMINREG_P, BMINREGJONG_P)|>
#   filter(!is.na(BMINREG_P) & !is.na(BMINREGJONG_P))|>
#   my_mutate()|>
#   left_join(geo_list$sd_oud, by= "spatial_name")|>
#   pivot_longer(cols  = c(BMINREG_P,BMINREGJONG_P), names_to = 'measure' )
# ) |>
#   filter(
#     spatial_name != 'onbekend',
#     spatial_name != 'Weesp')|>
#   
#   mutate(
#     
#     spatial_code = case_when(
#       spatial_name == 'Zeeburgereiland Nieuwe diep'~ 'DX16',
#       spatial_name == 'Oud West De Baarsjes' ~ 'DX05',
#       spatial_name == 'Geuzenveld Slotermeer Sloterdijken' ~ 'DX06',
#       spatial_name == 'De Aker  Sloten en Nieuw Sloten' ~ 'DX08',
#       spatial_name == 'Oud Zuid' ~ 'DX10',
#       spatial_name == 'Indische Buurt Oostelijk Havengebied'~ 'DX14',
#       spatial_name == 'IJburg Zeeburgereiland'~ 'M34',
#       spatial_name == 'Amsterdam' ~ '0363',
#       TRUE ~ spatial_code),
#     
#     spatial_type = case_when(
#       spatial_code == 'M34'  ~ 'wijken',
#       spatial_code == '0363' ~ 'gemeente',
#       is.na(spatial_type) ~ 'gebieden',
#       TRUE ~ spatial_type),
#     
#     spatial_date = case_when(
#       is.na(spatial_date) ~ '20150101',
#       TRUE ~ spatial_date)
#     )|>
#   add_column(temporal_date =  '2022')
#   
# df_reg_21 <- read.xlsx(
#   "00 ruwe data/data_armoedecijfers_21.xlsx")|>
#   filter(measure %in% c("BMINREG_P","BMINREGJONG_P"))|>
#   mutate(
#     spatial_date = as.character(spatial_date),
#     temporal_date = as.character(temporal_date),
#     spatial_date = str_replace_all(spatial_date, "2020" , "20150101"),
#     spatial_code = case_when(
#       spatial_type == 'gemeente' ~ '0363',
#       TRUE ~ spatial_code)
#     )
#   
# 
# reg_def<- bind_rows(df_reg_21, df_reg_22)|>
#   select(-spatial_name) |>
#   mutate(
#     spatial_type = str_replace_all(spatial_type, "wijken", "wijk"),
#     spatial_type = str_replace_all(spatial_type, "gebieden", "GGW-gebied"),
#     spatial_type = str_replace_all(spatial_type, "stadsdelen", "stadsdeel")
#   )
# 
# write.xlsx(reg_def, "00 ruwe data/niet in bbga/data_regelingen.xlsx")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ---
### inlezen woon en werk gebied per stadsdeel en per gebied
### ### ### ### ### ### ### ### ### ### ### ### ### ### ---

# # GGW-gebied
# df_woonwerk_gb <- read.xlsx(
#   "00 ruwe data/Tabel banen detailhandel en horeca.xlsx", sheet = 1)|>
#   filter(sector == 'totaal', branche == 'totaal')|>
#   mutate(jaar = str_remove_all(jaar, "banen_bedrijven_dec"))|>
#   select(jaar, werk_ggw_code:aantal_banen, woon_werk )|>
#   group_by(jaar, werk_ggw_code, werk_ggw_naam, woon_werk)|>
#   summarise(aantal = sum(aantal_banen))|>
#   group_by(jaar, werk_ggw_code, werk_ggw_naam )|>
#   mutate(aandeel = aantal/sum(aantal)*100)|>
#   ungroup()|>
#   select(jaar, werk_ggw_code, woon_werk, aandeel)|>
#   set_names(c("temporal_date", "spatial_code", "measure", "value"))|>
#   filter(spatial_code != 'B', 
#          spatial_code != '0363')|>
#   add_column(spatial_type = 'GGW-gebied',
#              spatial_date = '20230401')
#   
# 
# 
# # stadsdeel
# df_woonwerk_sd <- read.xlsx(
#   "00 ruwe data/Tabel banen detailhandel en horeca.xlsx", sheet = 1)|>
#   filter(sector == 'totaal', branche == 'totaal')|>
#   select(jaar, werk_ggw_code, woon_ggw_code,aantal_banen)|>
#   mutate(
#     jaar = str_remove_all(jaar, "banen_bedrijven_dec"),
#     werk_sd = str_sub(werk_ggw_code, 1,2),
#     woon_sd = str_sub(woon_ggw_code, 1,2),
#     werk_sd = str_replace_all(werk_sd, "03", "0363"),
#     woon_sd = str_replace_all(woon_sd, "03", "0363"),
#     woon_sd = str_remove_all(woon_sd, "G"),
#     werk_sd = str_remove_all(werk_sd, "G"))|>
#   mutate(
#     woon_sd = replace_na(woon_sd, "woont buiten Amsterdam")
#   )|>
#   group_by(jaar, werk_sd, woon_sd)|>
#   summarise(aantal_banen = sum(aantal_banen))|>
#   mutate(woon_werk=case_when(
#     werk_sd == woon_sd ~ 'woont in werkgebied',
#     woon_sd == 'woont buiten Amsterdam' ~ 'woont buiten Amsterdam',
#     TRUE ~ 'woont in Amsterdam')
#   )|>
#   group_by(jaar, werk_sd, woon_werk )|>
#   summarise(aantal_banen = sum(aantal_banen))|>
#   group_by(jaar, werk_sd)|>
#   mutate(aandeel = aantal_banen/sum(aantal_banen)*100)|>
#   select(jaar, werk_sd, woon_werk, aandeel)|>
#   set_names(c("temporal_date", "spatial_code", "measure", "value"))  |>
#   add_column(spatial_date = '20230401') |>
#   mutate(spatial_type = case_when(
#     spatial_code == '0363'  ~ 'Gemeente',
#     TRUE  ~ 'Stadsdeel')
#   )
#   
# 
# df_woonwerk <- bind_rows(df_woonwerk_gb, df_woonwerk_sd) |>
#   filter(measure == 'woont in werkgebied') |>
#   mutate(measure = 'woonwerk_geb')
# 
# write.xlsx(df_woonwerk, "00 ruwe data/niet in bbga/data_woonwerk.xlsx")


# afgerond op 5 9 2024

