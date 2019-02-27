# Analysis of movies dataset

library(ggplot2)
library(ggplot2movies)
library(dplyr)
library(tidyverse)
?movies
View(movies)

# histogram of movies' length
gmovies.hist <- ggplot(data = movies, aes(length))
gmovies.hist + geom_histogram(color = "blue", fill = "lightgreen") +
  scale_x_continuous(limits = c(0, 200))

# top 10 movies
top10 <- movies %>%
  arrange(desc(rating)) %>%
  head(10) %>%
  mutate(title = fct_reorder(title, rating))

# dividing by genres
movies <- within (movies, { 
  Genre = ifelse (Action == 1, "Action" ,
                  ifelse(Animation == 1,'Animation',
                         ifelse(Comedy == 1, "Comedy",
                                ifelse(Drama == 1, "Drama", 
                                       ifelse(Documentary == 1, 'Documentary',
                                              ifelse(Romance == 1, "Romance", 
                                                     ifelse(Short==1, 'Short', '')))))))})


# boxplot
movies.box <- ggplot(movies, aes(factor(Genre), rating))
movies.box + geom_boxplot(aes(fill = Genre)) +
  scale_y_continuous(limits = c(5, 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# facet wrap of bar chart for movies after 1920
genres.bar <- movies %>%
  filter(year >= 1920)

movies.bar <- ggplot(genres.bar, aes(year))
movies.bar + geom_bar(aes(fill = Genre)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap( ~ Genre)

# bars by genres
movies.bar1 <- ggplot(genres.bar, aes(Genre))
movies.bar1 + geom_bar(aes(fill = Genre)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

