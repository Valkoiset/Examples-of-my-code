

library(dplyr)

rm(list = ls())
setwd("C:/Users/oleksandr-romanchenk/Desktop/Alfredo")
mydata <- read.csv("Country-City-List.csv")

mydata <- mydata %>% mutate_at(.vars = vars(s_country_name, s_array_city), .funs = as.character) %>% 
  mutate(s_array_city = gsub("\\[|\\]", "", s_array_city)) 

cities <- strsplit(mydata$s_array_city, split = ";")

for (row in seq_along(cities)) {
  if (length(cities[[row]]) > 1) {
    for (val in 2:length(cities[[row]])) {
      # it's important to overwrite dataset here, otherwise if it is written as
      # newdata <- mydata %>% rbind(mydata[row, ]) 
      # it creates new dataset with 1 additional row and in the end gives you dataset which each is
      # only 1 row more than initial one (254 --> 255 instead of 254 --> 462)
      mydata <- mydata %>% rbind(mydata[row, ]) 
      mydata[nrow(mydata), ] <- mydata[nrow(mydata), ] %>% mutate(s_array_city = cities[[row]][[val]])
    }
  }
}

mydata <- mydata %>% arrange(s_country_name) %>% 
  mutate(s_array_city = gsub(";.*", "", s_array_city))

mydata %>% select(s_country_name, s_array_city) %>% View()

write.csv(mydata, "mydata.csv")


