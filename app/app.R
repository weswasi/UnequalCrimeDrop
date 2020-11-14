# Initiate----
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)

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
      
      # Drop-down list ----
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
                  selected = "Polisanmälda brott per 100 000 invånare",
                  selectize = FALSE),
      
      # Slider ----
      sliderInput("range", strong("Year"), 
                  1975, 2017, c(1975,2017), sep= "", step = 1),
      
      # Checkbox crime type ----
      checkboxGroupInput("checkcrime", strong("Crime"), 
                         choices = unique(one_one_l$Crime),
                         selected = one_one_l$Crime[1:4]),
      
      # Checkbox age-group ----
      checkboxGroupInput("checkage", strong("Age"), 
                         choices = unique(three_five_l$Age),
                         selected = three_five_l$Age[1:13]),
      tags$br(),
      
      # Download buttons ----
      downloadButton("downloadPlot", "Download Plot"),
      downloadButton('downloadData', 'Download Table')
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  
                  #Tab 1 Plots ----
                  tabPanel(title="Plot",
                           tags$br(),
                           column(10, 
                                  title = "Reported crime",
                                  plotOutput("plot", height = 500))),
                  
                  #Tab 2 Datatable ----
                  tabPanel(title="Table",
                           tags$br(),
                           column(7, dataTableOutput("mytable"))
                  )
      )
    )
  )
)

# Server function ----
server <- function(input, output) {
  
  # Create plot ----
  plotInput <- reactive({
    
    # Force user input atleast one value ----
    validate(
      need(input$checkcrime, "Please select a crime :)"),
      need(input$checkage, "Please select age :)")
    )
    
    if (input$var == "Polisanmälda brott per 100 000 invånare") {
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
              axis.text.x = element_text(angle = -45))
    }
    
    else{
      
      if (input$var == "Samtliga brott, efter kön") {
        three_five_l <- three_five_l %>% 
          subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage)
        
        ggplot(three_five_l, aes(Year, Value, color = Age)) +
          facet_grid(Gender ~ ., scales="free") +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          labs(color = "Age",
               y = "Crimes ",
               x = "Year") +
          scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
          theme(text = element_text(size=15), 
                axis.text.x = element_text(angle = -45))
      }
    }
  })
  
  # Create data for datatable ----
  tableInput <- reactive({
    
    if (input$var == "Polisanmälda brott per 100 000 invånare") {
      one_one_w <- one_one_w %>% select(Year, input$checkcrime) %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else{
      if (input$var == "Samtliga brott, efter kön") {
        three_five_w <- three_five_w %>% 
          subset(Year >= input$range[1] & Year <= input$range[2])
      }}
  })
  
  # Plot output ----
  output$plot <- renderPlot({
    plotInput()
  })
  
  # Datatable output ----
  output$mytable <- renderDataTable({
    tableInput() %>% 
      datatable( 
        rownames= FALSE,
        options = list(
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = FALSE,
          dom = "t",
          pageLength = -1,
          columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  
  # Download button content ----
  # Plot ----
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste("plot-", Sys.Date(), '.png', sep = '')},
    
    content = function(file){
      png(file, width = 1000, height = 700)
      print(plotInput())
      dev.off()
    })
  
  # Datatable ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file){
      write.csv(tableInput(), file, row.names = FALSE)
    }
  )
}

# App ----
shinyApp(ui, server)