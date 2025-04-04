

# script datasets te bouwen 

# met dit script wordt het basisbestand "data_def2.rds" gemaakt
source("02 scripts/01 scripts bewerking data/script 02 basis opbouw basisset.R")

# "data_def2.rds" dient als basis voor de volgende scripts:
# met ondertaande scripts worden BBGA_data_noord.rds BBGA_data_zo.rds  BBGA_data_nw.rds gemaakt

source("02 scripts/01 scripts bewerking data/script 09 verwerking data tot definitieve indicatorenlijsten.R")
source("02 scripts/01 scripts bewerking data/script 10 verwerking data NPLV.R")
source("02 scripts/01 scripts bewerking data/script 20 verwerking data Zuidoost.R")
source("02 scripts/01 scripts bewerking data/script 30 verwerking data Nieuw-West.R")
source("02 scripts/01 scripts bewerking data/script 40 verwerking data Noord.R") 

# scripts om tabellen te maken voor publicatie doeleinden
# BBGA_data_noord.rds BBGA_data_zo.rds  BBGA_data_nw.rds dienen als basis voor onderstaande scripts
source("02 scripts/02 scripts publicaties Zuidoost/script 22 publicatie ZO eenmeting basis.R")
source("02 scripts/03 scripts publicaties Noord/script 22 publicatie Noord factsheets algemeen data.R")
source("02 scripts/04 scripts publicaties Nieuw-West/script 22 publicatie NW factsheets algemeen data.R")
