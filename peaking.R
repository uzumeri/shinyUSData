library(tidyverse)

peaking <- read.csv("peaking.csv", stringsAsFactors = FALSE) %>% arrange(resources)
peaking$resources <- as.Date(peaking$resources, format = "%d-%b")
