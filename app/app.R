# Initiate----
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wesanderson)

theme_set(theme_light())

# Data ----
one_one <- readRDS(file = "one_one.RDS")

# UI ----
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Den ojämlika brottsligheten",
                                    titleWidth = 300),
                    dashboardSidebar(
                      width = 300,
                      menuItem("1.1 Polisanmälda brott 1975-2017", tabName = "one_one"),
                      menuItem("1.2 Antal lagföringar, samtliga brott, Brås officiella statistik respektive studiens lagföringsdata.", tabName = "one_two")
                    ),
                    dashboardBody(
                      fluidRow(
                        box(title = "Polisanmälda brott 1975-2017", status = "primary",
                            plotOutput("plot_one_one", height = 250)),
                        
                        box(status = "primary",
                            sliderInput("range", "År", 1975, 2017, c(1975,2017), sep= "", step = 1),
                            column(3, 
                                   checkboxGroupInput("checkcrime", 
                                                      h5("Brott"), 
                                                      choices = unique(one_one$Crime),
                                                      selected = one_one$Crime[1:4]))
                        )
                      )
                    )
)

# Server function ----
server <- function(input, output, session) {
  
  # 1.1 ----
  output$plot_one_one <- renderPlot({
    one_one <- one_one %>% subset(Year >= input$range[1] & Year <= input$range[2]) 
    one_one <- subset(one_one, Crime %in% input$checkcrime)
    
    ggplot(one_one, aes(Year, Values, color = Crime)) +
      geom_line(size = 1.3) +
      labs(color = "Brott",
           y = "Antal per 100 000",
           x = "År") +
      scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
      scale_color_manual(values=wes_palette(n=4, name="GrandBudapest1"))
    
  })
  
}

# App ----
shinyApp(ui, server)