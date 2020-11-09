# Initiate----
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(wesanderson)

theme_set(theme_bw())

# Data ----
load(file = "datasets.rda")

# UI ----
ui <- fluidPage(
  theme = shinytheme("lumen"),
  navbarPage("The Uneven Crime Drop"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Create graph and table from crime data"),
      selectInput("var", 
                  label = strong("Choose data to display"),
                  choices = list(" " = c(
                    "Polisanmälda brott per 100 000 invånare",
                    "Antal lagföringar samtliga brott",
                    "Antal lagföringar respektive antal lagförda brott per"),
                    "Lagföringar:" = c(
                      "Våld-, stöld- respektive narkotikabrott",
                      "Samtliga brott, efter kön",
                      "Våld-, stöld-, respektive narkotikabrott efter kön",
                      "Samtliga brott efter ålder och kön",
                      "Stöldbrott efter ålder och kön",
                      "Våldsbrott efter ålder och kön",
                      "Narkotikabrott efter ålder och kön",
                      "Samtliga brott efter härkomst och kön",
                      "Stöld-, våld- respektive narkotikabrott. Män efter härkomst",
                      "Stöld-, våld- respektive narkotikabrott. Kvinnor efter härkomst",
                      "Samtliga brott. Män respektive kvinnor, 15-24 år, efter föräldrars inkomst",
                      "Våld-, stöld- respektive narkotikabrott efter föräldrars inkomst. Män respektive kvinnor, 15-24 år"),
                    "Risken (%) att lagföras:" = c(
                      "Samtliga brott med stigande ålder bland födelsekohorter av män och kvinnor",
                      "Våldsbrott med stigande ålder bland födelsekohorter av män och kvinnor",
                      "Stöldbrott med stigande ålder bland födelsekohorter av män och kvinnor",
                      "Narkotikabrott med stigande ålder bland födelsekohorter av män och kvinnor",
                      "Brottsdeltagande och brottsfrekvens bland kohorter av män och kvinnor födda mellan 1958 och 1992",
                      "Snedfördelningen i lagförd brottslighet bland födelsekohorter av män och kvinnor"),
                    "Andel (%):" = c(
                      "Lagförda män och kvinnor vid 24 års ålder efter härkomst",
                      "Högaktiva män och kvinnorvid 24 års ålder efter härkomst",
                      "Lagförda män och kvinnor vid 24 års ålder efter socio-ekonomiskt ursprung",
                      "Högaktiva män och kvinnor vid 24 års ålder efter socio-ekonomiskt",
                      "24-åringar från låg- respektive höginkomstkategorin efter härkomst",
                      "24-åriga män med minst 1 lagföring och minst 4 lagföringar från låg- respektive höginkomstkategorin efter härkomst",
                      "24-åriga kvinnor med minst 1 lagföring och minst 2 lagföringar från låg- respektive höginkomstkategorin efter härkomst")),
                  selected = "Våld-, stöld- respektive narkotikabrott",
                  selectize = FALSE,
                  size = 15),
      
      sliderInput("range", strong("Year"), 
                  1975, 2017, c(1975,2017), sep= "", step = 1),
      
      checkboxGroupInput("checkcrime", strong("Crime"), 
                         choices = unique(one_one_l$Crime),
                         selected = one_one_l$Crime[1:4]),
      tags$br(),
      downloadButton("downloadPlot", "Download Plot"),
      downloadButton('downloadData', 'Download Table')
    ),
    
    mainPanel(
      fluidRow(
        column(7, 
               title = "Reported crime",
               plotOutput("plot_one_one", height = 500)),
        column(5, dataTableOutput("mytable"))
      )
    )
  )
)

# Server function ----
server <- function(input, output) {
  
  # 1.1 ----
  plotInput <- reactive({
    validate(
      need(input$checkcrime, "Please select a crime :)")
    )
    
    one_one_l <- one_one_l %>% 
      subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime)
    
    ggplot(one_one_l, aes(Year, Value, color = Crime)) +
               geom_line(size = 1.2) +
               geom_point(size = 3) +
               labs(color = "Crime",
                    y = "Crimes per 100 000",
                    x = "Year") +
               scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
               theme(text = element_text(size=15), 
                     axis.text.x = element_text(angle = -45)) +
               scale_color_manual(values=wes_palette(n=4, name="GrandBudapest1"))
    
  })
  
  output$mytable = renderDataTable({
    one_one_w <- one_one_w %>% select(Year, input$checkcrime) %>% 
      subset(Year >= input$range[1] & Year <= input$range[2])
    
    datatable(one_one_w, 
              rownames= FALSE, 
              extensions = "Buttons",
              options = list(
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = FALSE,
                dom = "t",
                pageLength = -1,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })  
  
  output$plot_one_one <- renderPlot({
    print(plotInput())
  })

  output$downloadPlot <- downloadHandler(
    filename = function(){paste("plot-", Sys.Date(), '.png', sep = '')},
    
    content = function(file){
      png(file, width = 1000, height = 700)
      print(plotInput())
      dev.off()
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      one_one_w <- one_one_w %>% select(Year, input$checkcrime) %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
      write.csv(one_one_w, con)
    }
  )
  
}

# App ----
shinyApp(ui, server)