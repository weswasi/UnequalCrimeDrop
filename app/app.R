# Initiate----
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)

# Ggplot theme and palette ----
theme_set(theme_bw())

custom.col <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Data ----
load(file = "datasets.rda")

# UI ----
ui <- fluidPage(
  theme = shinytheme("lumen"),
  navbarPage("The Uneven Crime drop"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Create graph and table from crime data gathered trough The Uneven Crime Drop project (Department of Criminology, University of Stockholm)"),
      
      # Drop-down list ----
      selectInput("var", 
                  label = strong("Choose data to display"),
                  choices = list(" " = c(
                    "Polisanmälda brott per 100 000 invånare",
                    "Antal lagföringar samtliga brott",
                    "Antal lagföringar respektive antal lagförda brott per 100 000 invånare"),
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
                  1973, 2017, c(1973,2017), sep= "", step = 1),
      
      # Checkbox crime type ----
      conditionalPanel(
        condition = 
          "input.var == 'Polisanmälda brott per 100 000 invånare' || 
        input.var == 'Antal lagföringar samtliga brott'",
        checkboxGroupInput("checkcrime", strong("Crime"), 
                           choices = unique(one_one_l$Crime),
                           selected = one_one_l$Crime[1:4], inline = TRUE)
      ),
      
      # Checkbox age-group ----
      checkboxGroupInput("checkage", strong("Age"), 
                         choices = unique(three_three_four_l$Age),
                         selected = three_three_four_l$Age[1:13]),
      # Checkbox region ----
      checkboxGroupInput("checkregion", strong("Härkomst"), 
                         choices = c("Sverige", "Sverige, utlandsfödda föräldrar", "Västländer", "Övriga länder"),
                         selected = c("Sverige", "Sverige, utlandsfödda föräldrar", "Västländer", "Övriga länder")),
      tags$br(),
      
      # Download buttons ----
      downloadButton("downloadPlot", "Download graph"),
      downloadButton('downloadData', 'Download table'),
      
      tags$br(),
      tags$br(),
      tags$a(
        href="https://github.com/weswasi/UnevenCrimeDrop", 
        tags$img(src="git.png",
                 width="40",
                 height="35")
      ),
      tags$a(href="https://www.su.se/profiles/enal3400-1.424017", "Enes Al Weswasi")
      
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  
                  # Tab 1 Plots ----
                  tabPanel(title = "Graph",
                           tags$br(),
                           column(10, plotOutput("plot", width  = 1000, height = 700))),
                  
                  # Tab 2 Datatable ----
                  tabPanel(title = "Table",
                           tags$br(),
                           column(7, dataTableOutput("mytable"))
                  ),
                  
                  #Tab 3 PDF ---
                  tabPanel(title = "Description of graph and table from the report (in Swedish)",
                           tags$br(),
                           uiOutput("pdfview"))
      )
    )
  )
)

# Server function ----
server <- function(input, output) {
  
  # Create plot ----
  plotInput <- reactive({
    
    # Force user input at least one Value ----
    validate(
      need(input$checkcrime, "Please select a crime :)"),
      need(input$checkage, "Please select age :)")
    )
    
    # Plot ---
    if (input$var == "Polisanmälda brott per 100 000 invånare") {
      one_one_l %>% 
        subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Polisanmälda brott per 100 000 invånare",
             y = "Crime per 100 000",
             x = "Year",
             color = "Crime") +
        scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
        theme(plot.title = element_text(color="gray50"),
              text = element_text(size=15), 
              axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
    }
    
    else if (input$var == "Antal lagföringar samtliga brott") {
      one_two_l %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Source)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Antal lagföringar samtliga brott",
             y = "Crime ",
             x = "Year",
             color = "Data source") +
        scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
    }
    
    else if (input$var == "Antal lagföringar respektive antal lagförda brott per 100 000 invånare"){
      one_three_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Prosecutions)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Antal lagföringar respektive antal lagförda brott per 100 000 invånare",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Prosecutions") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
    }
    
    else if (input$var == "Våld-, stöld- respektive narkotikabrott"){
      one_four_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Våld-, stöld- respektive narkotikabrott",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Crime") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
    }
    
    else if (input$var == "Samtliga brott, efter kön") {
      three_one_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Gender)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Samtliga brott, efter kön",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Gender") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
      
    }
    
    else if (input$var == "Våld-, stöld-, respektive narkotikabrott efter kön") {
      three_two_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Våld-, stöld-, respektive narkotikabrott efter kön",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Crime") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
      
    }
    
    else if (input$var == "Samtliga brott efter ålder och kön") {
      three_three_four_l %>% 
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Samtliga brott, efter kön",
             y = "Crime ",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15), 
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
    }
    
    else if (input$var == "Stöldbrott efter ålder och kön") {
      three_five_ad_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Stöldbrott efter ålder och kön",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
      
    }
    
    else if (input$var == "Våldsbrott efter ålder och kön") {
      three_five_be_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Våldsbrott efter ålder och kön",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
      
    }
    
    else if (input$var == "Narkotikabrott efter ålder och kön") {
      three_five_cf_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Narkotikabrott efter ålder och kön",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_manual(values=custom.col)
      
    }
    
  })
  
  # Create data for datatable ----
  tableInput <- reactive({
    
    if (input$var == "Polisanmälda brott per 100 000 invånare") {
      one_one_w <- one_one_w %>% 
        select(Year, input$checkcrime) %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Antal lagföringar samtliga brott") {
      one_two_w <- one_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Antal lagföringar respektive antal lagförda brott per 100 000 invånare") {
      one_three_w <- one_three_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Våld-, stöld- respektive narkotikabrott") {
      one_four_w <- one_four_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Samtliga brott, efter kön") {
      three_one_w <- three_one_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Våld-, stöld-, respektive narkotikabrott efter kön") {
      three_two_w <- three_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Samtliga brott efter ålder och kön") {
      three_three_four_w <- three_three_four_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
    else if (input$var == "Stöldbrott efter ålder och kön") {
      three_five_ad_w <- three_five_ad_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
    else if (input$var == "Våldsbrott efter ålder och kön") {
      three_five_be_w <- three_five_be_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
    else if (input$var == "Narkotikabrott efter ålder och kön") {
      three_five_cf_w <- three_five_cf_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
  })
  
  # Reactive page number
  output$pdfview <- renderUI({
    
    if (input$var == "Polisanmälda brott per 100 000 invånare") {
      tags$iframe(style="height:780px; width:100%", src="rapport.pdf#page=4")
    }
    
    else if (input$var == "Antal lagföringar samtliga brott") {
      tags$iframe(style="height:980px; width:100%", src="rapport.pdf#page=30")
    }
    
    else if (input$var == "Antal lagföringar respektive antal lagförda brott per 100 000 invånare") {
      tags$iframe(style="height:980px; width:100%", src="rapport.pdf#page=12")
    }
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
          columnDefs = list(list(className = 'dt-center', targets = "_all"))),
        caption = htmltools::tags$caption(style = 'caption-side: top; color:gray; font-size:130% ;',input$var) )
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