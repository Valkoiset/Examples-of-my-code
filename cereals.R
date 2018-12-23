# Analysis of cereals dataset using shiny library

# link to the app: https://valkoiset.shinyapps.io/cereals/

library(devtools)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggcorrplot)
library(lattice)
cereal <- read.csv("cereal.csv")
theme_set(theme_light())

cereal$shelf <- NULL

attach(cereal)

# DATA PREPARATION
# checking  that these data are numeric and not something that doesn't work
sapply(cereal, mode)
sapply(cereal, class)

# GRAPHS
# fat - rating with mnf as factor
scat <- ggplot(cereal, aes(x = calories, y = rating))
scat + geom_point(
  aes(color = mfr),
  shape = 21,
  fill = "White",
  size = 3,
  stroke = 2
) + theme_light() +
  labs(x = "fat", y = "rating")

# rating with sugar and calories
gg = ggplot(data = cereal, aes(x = sugars, y = calories, col = rating)) +
  geom_jitter(data = cereal, aes(sugars, calories, col = rating)) +
  labs(x = "Sugar", y = "Calories") +
  geom_smooth(method = "lm", se = FALSE, col = 'black') +
  theme_bw()
gg
ggplotly(gg)

# bar chart: top 10 companies by rating
bar.cereal <- cereal %>%
  arrange(desc(rating)) %>%
  head(10) %>%
  mutate(name = fct_reorder(name, rating)) %>%
  select(name, rating)

ggplot(bar.cereal, aes(name, rating, fill = name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")

# one more bar chart
ggplot(bar.cereal, aes(x = name, y = rating)) +
  geom_bar(stat = "identity", width = .5, fill = "tomato3") +
  labs(title = "Ordered Bar Chart",
       subtitle = "Make Vs Avg. Mileage",
       caption = "source: mpg") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

# vars of top 10 companies
top10 <- cereal %>%
  arrange(desc(rating)) %>%
  head(10) %>%
  mutate(name = fct_reorder(name, calories))

ggplot(top10, aes(x = name, y = calories)) +
  geom_bar(stat = "identity", width = .5, fill = "tomato3") +
  labs(title = "Ordered Bar Chart") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

# Manufacturers to weights
ggplot() +
  geom_point(cereal, mapping = aes(
    x = mfr,
    y = weight,
    color = mfr,
    size = 3
  )) +
  labs(x = "Manufacturer", y = "Ounce") +
  ggtitle("Manufacturers use Different Weights for One Serving")

# Manufacturers to cups
ggplot() +
  geom_point(cereal, mapping = aes(
    x = mfr,
    y = cups,
    color = mfr,
    size = 3
  )) +
  labs(x = "Manufacturer", y = "Cup") +
  ggtitle("Manufacturers use Different Cups for One Serving")

# creting var with only numeric columns from cereal data
cereal.numeric <- cereal[, sapply(cereal, is.numeric)]
cereal.numeric.scat <- cereal.numeric
cereal.numeric.scat$weight <- NULL
cereal.numeric.scat$cups <- NULL

# ???
mean(cereal$calories)
summary(cereal)

cereal.box <- cereal.numeric
cereal.box <-
  gather(cereal.box, calories, protein, fat, sodium, fiber,
         carbo, sugars, potass, vitamins,
         key = "components", value = "value")

box <- ggplot(cereal.box, aes(factor(components), value))
box + geom_boxplot(aes(fill = components)) +
  scale_y_continuous(limits = c(0, 250)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bar <- ggplot(cereal.numeric, aes(sugars))
bar + geom_bar(aes(fill = sugars)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# example of density plot
ggplot(cereal.numeric, aes(x=calories))+
  geom_density(color="darkblue", fill="lightblue")

# Shiny ------------------------------------------------------------------------
ui <-
  navbarPage(
    theme = shinytheme("yeti"),
    title = h4("Cereals analysis"),
    
    tabPanel(
      "Rating with comparison of variables",
      selectInput("x", "choose x", colnames(cereal.numeric.scat)),
      selectInput("y", "choose y", colnames(cereal.numeric.scat)),
      plotlyOutput("scat")
    ),
    
    tabPanel(
      "Distribution",
      selectInput("distribution", "choose var", colnames(cereal.numeric)),
      plotlyOutput("hist")
    ),
    
    tabPanel("Top 10 companies",
             plotOutput("bar")),
    
    tabPanel(
      "Cups and weights",
      br(),
      h1("Same manufacturers use different cups and
         weights for one serving "),
      br(),
      plotOutput("point1"),
      br(),
      plotOutput("point2")
      ),
    
    tabPanel(
      "Data Table with the underlying Data",
      DT::dataTableOutput("tableDT")
    ),
    
    tabPanel(
      "Documentation",
      h1("77 Cereals"),
      br(),
      strong("Context"),
      br(),
      p(
        "If you like to eat cereal, do yourself a favor and avoid this
        dataset at all costs. After seeing these data it will never
        be the same for me to eat Fruity Pebbles again."
      ),
      br(),
      strong("Sources"),
      br(),
      "The original source of dataset can be found",
      a("here", href = "https://perso.telecom-paristech.fr/eagan/class/igr204/datasets"),
      br(),
      "In this project was used cleaned dataset from",
      a("kaggle", href = "https://www.kaggle.com/crawford/80-cereals/home")
      )
    )

server <- function(input, output) {
  x <- reactive({
    cereal.numeric.scat[, input$x]
  })
  y <- reactive({
    cereal.numeric.scat[, input$y]
  })
  dist <- reactive({
    cereal.numeric[, input$distribution]
  })
  output$scat = renderPlotly({
    scat <- ggplot(data = cereal.numeric, aes(x(), y(),
                                              col = rating)) +
      geom_jitter(data = cereal.numeric, aes(x(), y(),
                                             col = rating), size = 3) +
      labs(x = input$x, y = input$y) +
      geom_smooth(method = "lm",
                  se = FALSE,
                  # se shows small field around regression line
                  col = 'tomato3', size = 1.5) +
      theme_gray()  +
      scale_colour_gradient2()
    ggplotly(scat)
  })
  
  output$hist = renderPlotly({
    hist <- ggplot(data = cereal.numeric, aes(dist())) +
      geom_histogram(color = "blue", fill = "lightgreen") +
      labs(x = input$distribution)
    ggplotly(hist)
  })
  
  output$bar = renderPlot({
    ggplot(bar.cereal, aes(x = name, y = rating)) +
      geom_bar(stat = "identity",
               width = .5,
               fill = "tomato3") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.6))
  })
  
  output$point1 = renderPlot({
    ggplot() +
      geom_point(cereal, mapping = aes(
        x = mfr,
        y = weight,
        color = mfr,
        size = 3
      )) +
      labs(x = "Manufacturer", y = "Ounce") +
      ggtitle("Manufacturers use Different Weights for One Serving")
  })
  
  output$point2 = renderPlot({
    ggplot() +
      geom_point(cereal, mapping = aes(
        x = mfr,
        y = cups,
        color = mfr,
        size = 3
      )) +
      labs(x = "Manufacturer", y = "Cup") +
      ggtitle("Manufacturers use Different Cups for One Serving")
  })
  
  output$tableDT <- DT::renderDataTable(DT::datatable(cereal))
  
}

shinyApp(ui, server)
