# inlezen Statistiekhub: gedownload van
#
# 1. Download de zip
download.file(
  url = "https://api.data.amsterdam.nl/bulk-data/csv/statistieken_v2_all_openbaar.csv.zip",
  destfile = "statistieken_v2_all_openbaar.csv.zip",
  mode = "wb" # belangrijk voor binaire bestanden op Windows
)

# 2. Bekijk wat erin zit
unzip("statistieken_v2_all_openbaar.csv.zip", list = TRUE)

# 3. Alles uitpakken naar een map
unzip(
  "statistieken_v2_all_openbaar.csv.zip",
  exdir = "00 ruwe data/statistiekhub ruw"
)


################################################################
#### Deze methode (= het rechtstreek streamen van csv) werkt vooralsnog niet,
#### na aantal pagina's wordt download geblokkeerd
################################################################

# https://api.data.amsterdam.nl/v1/docs/datasets/statistieken@v2.html#cijfers
Statistiekhub_raw <- readr::read_csv(
  "https://api.data.amsterdam.nl/v1/statistieken/v2/cijfers?id=2067&_format=csv"
)

# https://api.data.amsterdam.nl/v1/docs/datasets/statistieken@v2.html#indicatoren
Statistiekhub_meta <- readr::read_csv(
  "00 ruwe data/statistiekhub ruw/statistieken_v2_indicatoren_openbaar.csv"
)
