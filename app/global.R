# source("./helpers.R")
source("./app/helpers.R")

data2019 <- getDataByYear(2019)
data2020 <- getDataByYear(2020)

# data_list <- list("2019" = data2019, "2020" = data2020)

data <- rbind(data2019, data2020)
# glimpse(data)
