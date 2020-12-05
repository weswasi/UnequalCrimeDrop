# Initiate----
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(scales)
library(RColorBrewer)


# Ggplot theme and palette ----
theme_set(theme_bw())

# Data ----
load(file = "datasets.rda")

# UI ----
ui <- fluidPage(
  theme = shinytheme("lumen"),
  navbarPage("The Uneven Crime drop"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Create and customize graphs and tables from Swedish crime data (1973-2017) gathered trough The Uneven Crime Drop project (Department of Criminology, University of Stockholm). Note that not all graps and tables are fully customizable."),
      
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
                      "24-åriga män från låginkomstfamiljer med minst 1 lagföring och minst 4 lagföringar efter härkomst",
                      "24-åriga kvinnor från låginkomstfamiljer med minst 1 lagföring och minst 2 lagföringar efter härkomst")),
                  selected = "Polisanmälda brott per 100 000 invånare",
                  selectize = FALSE),
      
      # Slider:Year ----
      sliderInput("range", strong("Year"), 
                  1973, 2017, c(1973,2017), sep= "", step = 1),
      
      # Slider: Age ----
      conditionalPanel(
        condition = 
          "input.var == 'Brottsdeltagande och brottsfrekvens bland kohorter av män och kvinnor födda mellan 1958 och 1992'",
        sliderInput("birthyear", strong("Birthyear"), 
                    1958, 1992, c(1958, 1992), sep= "", step = 1)),
      
      # Slider: Birthyear ----
      conditionalPanel(
        condition = 
          "input.var == 'Samtliga brott med stigande ålder bland födelsekohorter av män och kvinnor'",
        sliderInput("age", strong("Age"), 
                    15, 58, c(15,58), sep= "", step = 1)),
      
      # Checkbox crime type ----
      checkboxGroupInput("checkcrime", strong("Crime"), 
                         choices = unique(one_one_l$Crime),
                         selected = one_one_l$Crime[1:4], inline = TRUE),
      
      # Checkbox age-group ----
      checkboxGroupInput("checkage", strong("Age"), 
                         choices = unique(three_three_four_l$Age),
                         selected = three_three_four_l$Age[1:13]),
      # Checkbox region ----
      checkboxGroupInput("checkregion", strong("Country of origin"), 
                         choices = unique(four_three_ac_l$Country),
                         selected = four_three_ac_l$Country[1:10]),
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
        scale_colour_brewer(palette = "Paired")
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
        scale_colour_brewer(palette = "Paired")
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
        scale_colour_brewer(palette = "Paired")
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
        scale_colour_brewer(palette = "Paired")
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
        scale_colour_brewer(palette = "Paired")
      
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
        scale_colour_brewer(palette = "Paired")
      
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
        scale_colour_brewer(palette = "Paired")
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
        scale_colour_brewer(palette = "Paired")
      
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
        scale_colour_brewer(palette = "Paired")
      
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
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Samtliga brott efter härkomst och kön") {
      four_one_four_two_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Country %in% input$checkregion) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Samtliga brott efter härkomst och kön",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Stöld-, våld- respektive narkotikabrott. Män efter härkomst") {
      four_three_ac_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Country %in% input$checkregion) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Crime ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Stöld-, våld- respektive narkotikabrott. Män efter härkomst",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Stöld-, våld- respektive narkotikabrott. Kvinnor efter härkomst") {
      four_three_df_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Country %in% input$checkregion) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Crime ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Stöld-, våld- respektive narkotikabrott. Kvinnor efter härkomst",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Samtliga brott. Män respektive kvinnor, 15-24 år, efter föräldrars inkomst") {
      five_one_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Samtliga brott. Män respektive kvinnor, 15-24 år, efter föräldrars inkomst",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Income") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Våld-, stöld- respektive narkotikabrott efter föräldrars inkomst. Män respektive kvinnor, 15-24 år") {
      five_two_af_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Gender ~ Crime, scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Våld-, stöld- respektive narkotikabrott efter föräldrars inkomst. Män respektive kvinnor, 15-24 år",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Income") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Samtliga brott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_one_ab_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Samtliga brott med stigande ålder bland födelsekohorter av män och kvinnor",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Våldsbrott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_two_ad_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Våldsbrott med stigande ålder bland födelsekohorter av män och kvinnor",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Stöldbrott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_two_be_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Stöldbrott med stigande ålder bland födelsekohorter av män och kvinnor",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Narkotikabrott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_two_cf_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Narkotikabrott med stigande ålder bland födelsekohorter av män och kvinnor",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    # ADD GEOM POINT
    else if (input$var == "Brottsdeltagande och brottsfrekvens bland kohorter av män och kvinnor födda mellan 1958 och 1992") {
      six_three_ab_l %>%
        subset(Birthyear >= input$birthyear[1] & Birthyear <= input$birthyear[2]) %>% 
        ggplot(aes(x = Birthyear)) +
        facet_grid(Gender ~., scales="free") +
        geom_line(aes(y = `Proportion of prosecuted individuals`), size = 1.2, color = "#33A02C") +
        geom_line(aes(y = `Average number of criminal offenses` * 3.5), size = 1.2, color = "#1F78B4") +
        labs(title = "Brottsdeltagande och brottsfrekvens bland kohorter av män och kvinnor födda mellan 1958 och 1992",
             y = "Proportion of prosecuted (%)",
             x = "Birthyear") +
        scale_y_continuous("Proportion of prosecuted individuals (%)", 
                           sec.axis = sec_axis(~ . / 3.5, name = "Average number of criminal offenses")) +
        scale_x_continuous(breaks = seq(1958, 1992, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.title.y = element_text(color = "#33A02C"),
          axis.title.y.right = element_text(color = "#1F78B4"))
      
    }
    
    else if (input$var == "Snedfördelningen i lagförd brottslighet bland födelsekohorter av män och kvinnor") {
      six_four_ab_l %>%
        ggplot(aes(Birthyear, Value, fill = Percentile)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_bar(position="fill", stat="identity") +
        labs(title = "Snedfördelningen i lagförd brottslighet bland födelsekohorter av män och kvinnor",
             y = "Andel av alla brott",
             x = "Birthyear") +
        geom_text(aes(label = Value),
                  position="fill", vjust=+2.1, size=3) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_fill_grey() +
        scale_y_continuous(label = scales::percent) +
        scale_x_continuous(breaks = seq(1960, 1990, by = 5))
      
    }
    
    else if (input$var == "Lagförda män och kvinnor vid 24 års ålder efter härkomst") {
      seven_one_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Lagförda män och kvinnor vid 24 års ålder efter härkomst",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Högaktiva män och kvinnorvid 24 års ålder efter härkomst") {
      seven_one_cd_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Högaktiva män och kvinnorvid 24 års ålder efter härkomst",
             x = "Year",
             y = "",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Lagförda män och kvinnor vid 24 års ålder efter socio-ekonomiskt ursprung") {
      seven_two_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Lagförda män och kvinnor vid 24 års ålder efter socio-ekonomiskt ursprung",
             x = "Year",
             y = "",
             color = "Income") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "Högaktiva män och kvinnor vid 24 års ålder efter socio-ekonomiskt") {
      seven_two_cd_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Gender ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Högaktiva män (≥ 4 lagföringar) och kvinnor (≥ 2 lagföringar)  vid 24 års ålder efter socio-ekonomiskt",
             y = "",
             x = "Year",
             color = "Income") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "24-åringar från låg- respektive höginkomstkategorin efter härkomst") {
      seven_three_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Income ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-åringar från låg- respektive höginkomstkategorin efter härkomst",
             y = "",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "24-åriga män från låginkomstfamiljer med minst 1 lagföring och minst 4 lagföringar efter härkomst") {
      seven_four_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Prosecution ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-åriga män från låginkomstfamiljer med minst 1 lagföring och minst 4 lagföringar efter härkomst",
             y = "",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    else if (input$var == "24-åriga kvinnor från låginkomstfamiljer med minst 1 lagföring och minst 2 lagföringar efter härkomst") {
      seven_five_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Prosecution ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-åriga kvinnor från låginkomstfamiljer med minst 1 lagföring och minst 2 lagföringar efter härkomst",
             y = "",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
      
    }
    
    
  })
  
  # Create data for datatable ----
  tableInput <- reactive({
    
    if (input$var == "Polisanmälda brott per 100 000 invånare") {
      one_one_w %>% 
        select(Year, input$checkcrime) %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Antal lagföringar samtliga brott") {
      one_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Antal lagföringar respektive antal lagförda brott per 100 000 invånare") {
      one_three_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Våld-, stöld- respektive narkotikabrott") {
      one_four_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Samtliga brott, efter kön") {
      three_one_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Våld-, stöld-, respektive narkotikabrott efter kön") {
      three_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Samtliga brott efter ålder och kön") {
      three_three_four_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
    else if (input$var == "Stöldbrott efter ålder och kön") {
      three_five_ad_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
    else if (input$var == "Våldsbrott efter ålder och kön") {
      three_five_be_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
    else if (input$var == "Narkotikabrott efter ålder och kön") {
      three_five_cf_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))
    }
    
    else if (input$var == "Samtliga brott efter härkomst och kön") {
      four_one_four_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkregion))
    }
    
    else if (input$var == "Stöld-, våld- respektive narkotikabrott. Män efter härkomst") {
      four_three_ac_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, starts_with(input$checkregion) )
    }
    
    else if (input$var == "Stöld-, våld- respektive narkotikabrott. Kvinnor efter härkomst") {
      four_three_ac_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, starts_with(input$checkregion) )
    }
    
    else if (input$var == "Samtliga brott. Män respektive kvinnor, 15-24 år, efter föräldrars inkomst") {
      five_one_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Våld-, stöld- respektive narkotikabrott efter föräldrars inkomst. Män respektive kvinnor, 15-24 år") {
      five_two_af_w <- five_two_af_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])
    }
    
    else if (input$var == "Samtliga brott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_one_ab_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])
    }
    
    
    else if (input$var == "Våldsbrott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_two_ad_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])
    }
    
    else if (input$var == "Stöldbrott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_two_be_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])
    }
    
    else if (input$var == "Narkotikabrott med stigande ålder bland födelsekohorter av män och kvinnor") {
      six_two_cf_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])
    }
    
    else if (input$var == "Brottsdeltagande och brottsfrekvens bland kohorter av män och kvinnor födda mellan 1958 och 1992") {
      six_three_ab_w %>% 
        subset(Birthyear >= input$birthyear[1] & Birthyear <= input$birthyear[2])
    }
    
    else if (input$var == "Snedfördelningen i lagförd brottslighet bland födelsekohorter av män och kvinnor") {
      six_four_ab_w
    }
    
    else if (input$var == "Lagförda män och kvinnor vid 24 års ålder efter härkomst") {
      seven_one_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Högaktiva män och kvinnorvid 24 års ålder efter härkomst") {
      seven_one_cd_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Lagförda män och kvinnor vid 24 års ålder efter socio-ekonomiskt ursprung") {
      seven_two_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Högaktiva män och kvinnor vid 24 års ålder efter socio-ekonomiskt") {
      seven_two_cd_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "24-åringar från låg- respektive höginkomstkategorin efter härkomst") {
      seven_three_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "24-åriga män från låginkomstfamiljer med minst 1 lagföring och minst 4 lagföringar efter härkomst") {
      seven_four_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    
    else if (input$var == "24-åriga kvinnor från låginkomstfamiljer med minst 1 lagföring och minst 2 lagföringar efter härkomst") {
      seven_five_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
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