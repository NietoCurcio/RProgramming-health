library(tidyverse)
library(geobr)

getFakeTotalPop <- function(n_municipalities) {
  # Get a fake number of total population in municipalities
  numbers <- runif(n_municipalities, min = 0.001)
  pop1 <- numbers[numbers < 0.25]
  pop2 <- numbers[numbers >= 0.25 & numbers < 0.5]
  pop3 <- numbers[numbers >= 0.5 & numbers < 0.75]
  pop4 <- numbers[numbers >= 0.75 & numbers <= 1.0]

  total_population <- c(
    round(pop1 * 10000),
    round(pop2 * 60000),
    round(pop3 * 110000),
    round(pop4 * 160000)
  )
}

getFakeVaccinatedPop <- function(total_population) {
  vaccinated <- round(
    runif(length(total_population), min = 0.35) * total_population
  )
}

getBreastfeedingComplete <- function(total_population) {
  breastfeeding <- round(
    runif(length(total_population), min = 0.2) * total_population
  )
}

getDataByYear <- function(year) {
  municipalities <- read_municipality(
    year = year,
    showProgress = FALSE
  )

  # object.size(municipalities) # 24.9 MB
  municipalities <- rmapshaper::ms_simplify(municipalities)
  # object.size(simplified) # 8.4 MB

  # without the next line, the following warning happens when calling leaflet:
  # "sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs). Need '+proj=longlat +datum=WGS84'"
  # src: https://stackoverflow.com/questions/57223853/how-to-apply-spatial-polygons-to-leaflet-map-using-shp
  municipalities <- municipalities %>% sf::st_transform("+proj=longlat +datum=WGS84")

  total_population <- getFakeTotalPop(nrow(municipalities))

  custom_data_df <- data.frame(
    code_muni = municipalities$code_muni,
    total_population = total_population,
    total_vaccinated = getFakeVaccinatedPop(total_population),
    breastfeeding_complete = getBreastfeedingComplete(total_population),
    year = year
  )

  data <- municipalities %>% left_join(custom_data_df, by = c("code_muni" = "code_muni"))
  return(data)
}