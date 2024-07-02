
##### januari 2024 Indicatorenmonitor MPZO

#library(tidyverse) > handmatig inlezen!
library(haven) 
library(janitor) 
library(readxl) 
library(dplyr) 
library(writexl)
library(tidyverse)
library(openxlsx)

koppel_wijkbuurt <- read_excel("02 formats//koppeltabel cbs buurt en wijk.xlsx")

# aanpassen naamgeving koppelbestand in lijn met bbga -  aangepast op 25 januari 2024
koppel_wijkbuurt <- koppel_wijkbuurt %>% 
  rename(spatial_code=gebied_code_nieuw,
         spatial_name=gebied_naam_nieuw)

# databestand inlezen
  
left_join(koppel_wijkbuurt, by=c("gebied_code_oud"="gwb_code_8")) %>%
  
# bij een left_join worden de velden zonder koppelsleutel niet gejoined. In dit geval de stadsdelen en gebieden. 
# deze krijgen als waarde NA; met onderstaand script worden de 'oude namen' verplaatst aan de toegevoegde kolommen
  
  mutate (gebied_naam_nieuw=
            case_when(is.na(gebied_naam_nieuw) ~ gebied_naam_oud,
                            TRUE               ~ gebied_naam_nieuw))%>%
  
  mutate (gebied_code_nieuw=
            case_when(is.na(gebied_code_nieuw) ~ gebied_code_oud,
                            TRUE               ~ gebied_code_nieuw)) %>%
  
  select (variabele, gebied_niveau, gebied_code_nieuw, gebied_naam_nieuw, jaar, peiljaar_gebiedsindeling, waarde)%>%
  
  rename (gebied_naam = gebied_naam_nieuw,
          gebied_code = gebied_code_nieuw)


write.xlsx(data_startkwal_def, "01 data/nog niet gereed/data_startkwal_def.xlsx")
          
          



