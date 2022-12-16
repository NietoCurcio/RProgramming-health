# source("./helpers.R")
source("./app/helpers.R")

data2019 <- getDataByYear(2019)
data2020 <- getDataByYear(2020)

states2019 <- getStatesByYear(2019)
states2020 <- getStatesByYear(2020)

# data_list <- list("2019" = data2019, "2020" = data2020)

data <- rbind(data2019, data2020)
dataStates <- rbind(states2019, states2020)
# glimpse(data)
# glimpse(dataStates)

glimpse(states2020)

rj <- dataStates %>% filter(year == 2020, abbrev_state == "RJ")

attributes(rj$geom)

View(rj$geom)
class(rj$geom)
summary(rj$geom)
str(rj$geom)

slotNames(rj$geom)


typeof(rj$geom)
length(rj$geom)
rj$geom

kke <-  sf::st_geometry(rj)
kke2 <-  sf::st_bbox(rj)

kke[["xmin"]]
kke$bbox
