
library(tidyverse)

### tests BBGA ---

test<- read.csv2("bbga_excel_test2024-02-29.csv") 

test_weesp <- test |>
  filter(gebiedcode22 %in% c("S", "SA", "SB" ),
         jaar %in% c(2021,2022,2023))

openxlsx::write.xlsx(test_weesp, "test_weesp.xlsx")
