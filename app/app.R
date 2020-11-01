# Initiate----
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(wesanderson)
library(DT)

theme_set(theme_light())

# Data ----
load(file = "datasets.rda")

# UI ----
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "The Uneven Crime Drop",
                                    titleWidth = 300),
                    
                    dashboardSidebar(
                      width = 300,
                      menuItem("1.1 Reported crime 1975-2017", tabName = "one_one")
                    ),
                    
                    dashboardBody(
                      
                      fluidRow(
                        box(
                          width = 3,
                          sliderInput("range", "Year", 
                                      1975, 2017, c(1975,2017), sep= "", step = 1),
                          checkboxGroupInput("checkcrime", "Crime", 
                                             choices = unique(one_one_l$Crime),
                                             selected = one_one_l$Crime[1:4])
                        )
                      ),
                      
                      fluidRow(
                        box(title = "Reported crime",
                            plotlyOutput("plot_one_one", height = 500)),
                        
                        box(dataTableOutput("mytable"))

                        )
                    )
)

# Server function ----
server <- function(input, output, session) {
  
  # 1.1 ----
  output$plot_one_one <- renderPlotly({
    validate(
      need(input$checkcrime, "Please select a crime :)")
    )
    
    one_one_l <- one_one_l %>% 
    subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime)
    
    ggplotly(ggplot(one_one_l, aes(Year, Value, color = Crime)) +
               geom_line(size = 1.3) +
               geom_point() +
               labs(color = "Crime",
                    y = "Crimes per 100 000",
                    x = "Year") +
               scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
               theme(axis.text.x = element_text(angle = -45)) +
               # geom_smooth(size = 0.3, method = "lm", se = F) +
               scale_color_manual(values=wes_palette(n=4, name="GrandBudapest1"))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$mytable = renderDataTable({
    one_one_w <- one_one_w %>% select(Year, input$checkcrime) %>% 
      subset(Year >= input$range[1] & Year <= input$range[2])
    
    datatable(one_one_w, 
              rownames= FALSE, 
              options = list(
                dom = 't', 
                pageLength = -1, 
                ordering=F, 
                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })  

  
}

# App ----
shinyApp(ui, server)