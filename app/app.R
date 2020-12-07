# Initiate----
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(scales)
library(RColorBrewer)

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
      helpText("Create and customize graphs and tables from Swedish crime data (1973-2017) gathered from the research project",
               code(tags$a(href="https://www.criminology.su.se/om-oss/nyheter/den-oj%C3%A4mlika-brottsligheten-ny-rapport-fr%C3%A5n-kriminologiska-institutionen-1.495808", 
                           "The Uneven Crime Drop")),"(Department of Criminology, University of Stockholm)."),
      
      # Drop-down list ----
      selectInput("var", 
                  label = strong("Choose data to display"),
                  choices = list(
                    "The general development of crime and prosecutions:" = c(
                      "Reported crimes",
                      "Number of prosecutions",
                      "Prosecutions and prosecuted crimes"),
                    "Prosecutions:" = c(
                      "Violence, theft, and drug offenses",
                      "All crimes, by sex",
                      "Violence, theft, and drug offenses, by sex",
                      "All crimes, by age and sex",
                      "Theft, by age and sex",
                      "Violence, by age and sex",
                      "Drug offenses, by age and sex",
                      "All crimes, by place of birth and sex",
                      "Violence, theft, and drug offenses. Males, by place of birth",
                      "Violence, theft, and drug offenses. Females, by place of birth",
                      "All crimes. 15-24-year-old males and females, by socio-economic background",
                      "Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background"),
                    "The risk (%) of being prosecuted:" = c(
                      "All crimes with increasing age among birth cohorts, by sex",
                      "Violent crime with increasing age among birth cohorts, by sex",
                      "Thefts with increasing age among birth cohorts, by sex",
                      "Drug offenses with increasing age among birth cohorts, by sex",
                      "Crime participation and crime rate among cohorts, by sex",
                      "The disproportion of prosecuted crime among birth cohorts, by sex"),
                    "Share (%) of:" = c(
                      "Prosecuted males and females at the age of 24, by place of birth",
                      "Highly active males and females at the age of 24, by place of birth",
                      "Prosecuted males and females at the age of 24, by socio-economic background",
                      "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background",
                      "24-year-olds from the low- and high-income category, by place of birth",
                      "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth",
                      "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth")),
                  selected = "Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background",
                  selectize = FALSE),
      
      # Slider:Year (1973-2017)----
      conditionalPanel(
        condition = 
          "input.var != 'Reported crimes' &
          input.var != 'Number of prosecutions' &
          input.var != 'All crimes with increasing age among birth cohorts, by sex' &
          input.var != 'Violent crime with increasing age among birth cohorts, by sex' &
          input.var != 'Thefts with increasing age among birth cohorts, by sex' &
          input.var != 'Drug offenses with increasing age among birth cohorts, by sex' &
          input.var != 'Crime participation and crime rate among cohorts, by sex' &
          input.var != 'The disproportion of prosecuted crime among birth cohorts, by sex' &
          input.var != 'All crimes. 15-24-year-old males and females, by socio-economic background' &
          input.var != 'Prosecuted males and females at the age of 24, by place of birth' &
          input.var != 'Highly active males and females at the age of 24, by place of birth' &
          input.var != 'Prosecuted males and females at the age of 24, by socio-economic background' &
          input.var != 'Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background' &
          input.var != '24-year-olds from the low- and high-income category, by place of birth' &
          input.var != '24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth' &
          input.var != '24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth' &
          input.var != 'Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background'",
        sliderInput("yearrange1", strong("Year"), 
                    1973, 2017, c(1973,2017), sep= "", step = 1)),
      
      # Slider:Year (1973-2017)----
      conditionalPanel(
        condition = 
          "input.var == 'Reported crimes' |
        input.var == 'Number of prosecutions'",
        sliderInput("yearrange2", strong("Year"), 
                    1975, 2017, c(1975,2017), sep= "", step = 1)),
      
      # Slider:Year (1990-2017)----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes. 15-24-year-old males and females, by socio-economic background' |
          input.var == 'Prosecuted males and females at the age of 24, by place of birth' |
          input.var == 'Highly active males and females at the age of 24, by place of birth' |
          input.var == 'Prosecuted males and females at the age of 24, by socio-economic background' |
          input.var == 'Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background' |
          input.var == '24-year-olds from the low- and high-income category, by place of birth' |
          input.var == '24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth' |
          input.var == '24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth' |
          input.var == 'Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background'",
        sliderInput("yearrange3", strong("Year"), 
                    1990, 2017, c(1975,2017), sep= "", step = 1)),
      
      # Slider: Age ----
      conditionalPanel(
        condition = 
          "input.var == 'Crime participation and crime rate among cohorts, by sex'",
        sliderInput("birthyear", strong("Birth year"), 
                    1958, 1992, c(1958, 1992), sep= "", step = 1)),
      
      # Slider: Birthyear ----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes with increasing age among birth cohorts, by sex' |
          input.var == 'Violent crime with increasing age among birth cohorts, by sex' |
          input.var == 'Thefts with increasing age among birth cohorts, by sex' |
          input.var == 'Drug offenses with increasing age among birth cohorts, by sex'",
        sliderInput("age", strong("Age"), 
                    15, 58, c(15,58), sep= "", step = 1)),
      
      # Checkbox Sex ----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes, by sex'",
        checkboxGroupInput("checksex", strong("Sex"), 
                           choices = unique(three_one_l$Sex),
                           selected = three_one_l$Sex[1:2], inline = TRUE)),
      
      # Checkbox crime type (incl All crimes) ----
      conditionalPanel(
        condition = 
          "input.var == 'Reported crimes'",
        checkboxGroupInput("checkcrime1", strong("Crime"), 
                           choices = unique(one_one_l$Crime),
                           selected = one_one_l$Crime[1:4], inline = TRUE)),
      
      # Checkbox crime type (excl All crimes) ----
      conditionalPanel(
        condition = 
          "input.var == 'Violence, theft, and drug offenses' | 
          input.var == 'Violence, theft, and drug offenses, by sex'",
        checkboxGroupInput("checkcrime2", strong("Crime"), 
                           choices = unique(one_four_l$Crime),
                           selected = one_four_l$Crime[1:3], inline = TRUE)),
      
      # Checkbox age-group ----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes, by age and sex' | 
          input.var == 'Theft, by age and sex' | 
          input.var == 'Violence, by age and sex' | 
          input.var == 'Drug offenses, by age and sex'",
        checkboxGroupInput("checkage", strong("Age"), 
                           choices = unique(three_three_four_l$Age),
                           selected = three_three_four_l$Age[1:13])),
      
      # Checkbox country (4 levels) ----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes, by place of birth and sex' | 
          input.var == 'Violence, theft, and drug offenses. Males, by place of birth' | 
          input.var == 'Violence, theft, and drug offenses. Females, by place of birth'",
        checkboxGroupInput("checkcountry1", strong("Place of birth"), 
                           choices = unique(four_three_ac_l$Country),
                           selected = four_three_ac_l$Country[1:10])),
      
      # Checkbox country (3 levels) ----
      conditionalPanel(
        condition = 
          "input.var == 'Prosecuted males and females at the age of 24, by place of birth' | 
          input.var == 'Highly active males and females at the age of 24, by place of birth' | 
          input.var == '24-year-olds from the low- and high-income category, by place of birth' |
          input.var == '24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth' |
          input.var == '24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth'",
        checkboxGroupInput("checkcountry2", strong("Place of birth"), 
                           choices = unique(seven_one_ab_l$Country),
                           selected = seven_one_ab_l$Country[1:10])),
      
      # Checkbox income ----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes. 15-24-year-old males and females, by socio-economic background' |
          input.var == 'Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background' |
          input.var == 'Prosecuted males and females at the age of 24, by socio-economic background' |
          input.var == 'Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background'",
        checkboxGroupInput("checkincome", strong("Parents income group"),
                           choices = unique(five_one_ab_l$Income),
                           selected = five_one_ab_l$Income[1:5])),
      
      # Checkbox birth year ----
      conditionalPanel(
        condition = 
          "input.var == 'The disproportion of prosecuted crime among birth cohorts, by sex'",
        checkboxGroupInput("checkbirthyear", strong("Birth year"),
                           choices = unique(six_four_ab_l$Birthyear),
                           selected = six_four_ab_l$Birthyear[1:40])),
      
      # Checkbox birth year ----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes with increasing age among birth cohorts, by sex' |
          input.var == 'Violent crime with increasing age among birth cohorts, by sex' |
          input.var == 'Thefts with increasing age among birth cohorts, by sex' |
          input.var == 'Drug offenses with increasing age among birth cohorts, by sex'",
        checkboxGroupInput("checkbirthyear2", strong("Birth year"),
                           choices = unique(six_one_ab_l$Birthyear),
                           selected = six_one_ab_l$Birthyear[1:20])),
      
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
      need(input$checkcrime1, "Please select a crime :)"),
      need(input$checkcrime2, "Please select a crime :)"),
      need(input$checkage, "Please select age :)"),
      need(input$checksex, "Please select sex :)"),
      need(input$checkage, "Please select age group :)"),
      need(input$checkcountry1, "Please select country :)"),
      need(input$checkcountry2, "Please select country :)"),
      need(input$checkincome, "Please select income :)"),
      need(input$checkbirthyear, "Please select birth year:)"),
      need(input$checkbirthyear2, "Please select birth year:)")
    )
    
    # Plot ---
    if (input$var == "Reported crimes") {
      one_one_l %>% 
        subset(Year >= input$yearrange2[1] & Year <= input$yearrange2[2] & Crime %in% input$checkcrime1) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Reported crimes",
             y = "Crime (per 100 000 population)",
             x = "Year",
             color = "Crime") +
        scale_x_continuous(breaks = seq(1975, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(plot.title = element_text(color="gray50"),
              text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Number of prosecutions") {
      one_two_l %>% 
        subset(Year >= input$yearrange2[1] & Year <= input$yearrange2[2]) %>% 
        ggplot(aes(Year, Value, color = Source)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Number of prosecutions (all crimes) depending on data source",
             y = "Prosecutions",
             x = "Year",
             color = "Data source") +
        scale_x_continuous(breaks = seq(1975, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Prosecutions and prosecuted crimes"){
      one_three_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        ggplot(aes(Year, Value, color = Prosecutions)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Prosecutions and prosecuted crimes (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Prosecutions") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violence, theft, and drug offenses"){
      one_four_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Crime %in% input$checkcrime2) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Crime") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "All crimes, by sex") {
      three_one_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Sex %in% input$checksex) %>% 
        ggplot(aes(Year, Value, color = Sex)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes, by sex (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Sex") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violence, theft, and drug offenses, by sex") {
      three_two_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Crime %in% input$checkcrime2) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses, by sex (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Crime") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "All crimes, by age and sex") {
      three_three_four_l %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes, by age and sex (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Theft, by age and sex") {
      three_five_ad_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Theft, by age and sex (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violence, by age and sex") {
      three_five_be_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, by age and sex (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Drug offenses, by age and sex") {
      three_five_cf_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Drug offenses, by age and sex (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Age") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "All crimes, by place of birth and sex") {
      four_one_four_two_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Country %in% input$checkcountry1) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes, by place of birth and sex (15-75 years; age-standardized)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violence, theft, and drug offenses. Males, by place of birth") {
      four_three_ac_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Country %in% input$checkcountry1) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Crime ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses. Males, by place of birth (15-75 years; age-standardized)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violence, theft, and drug offenses. Females, by place of birth") {
      four_three_df_l %>%
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2] & Country %in% input$checkcountry1) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Crime ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses. Females, by place of birth (15-75 years; age-standardized)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "All crimes. 15-24-year-old males and females, by socio-economic background") {
      five_one_ab_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Income %in% input$checkincome) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes. 15-24-year-old males and females, by socio-economic background (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Parents income") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background") {
      five_two_af_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Income %in% input$checkincome) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ Crime, scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background (15-75 years)",
             y = "Prosecutions (per 100 000 population)",
             x = "Year",
             color = "Parents income") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 4)) +
        scale_y_continuous(limits = c(0, NA)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "All crimes with increasing age among birth cohorts, by sex") {
      six_one_ab_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2] & Birthyear %in% input$checkbirthyear2) %>%
        ggplot(aes(Age, Value, color = Birthyear)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes with increasing age among birth cohorts, by sex",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Birth year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violent crime with increasing age among birth cohorts, by sex") {
      six_two_ad_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2] & Birthyear %in% input$checkbirthyear2) %>%
        ggplot(aes(Age, Value, color = Birthyear)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violent crime with increasing age among birth cohorts, by sex",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Birth year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Thefts with increasing age among birth cohorts, by sex") {
      six_two_be_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2] & Birthyear %in% input$checkbirthyear2) %>%
        ggplot(aes(Age, Value, color = Birthyear)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Thefts with increasing age among birth cohorts, by sex",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Birth year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Drug offenses with increasing age among birth cohorts, by sex") {
      six_two_cf_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2] & Birthyear %in% input$checkbirthyear2) %>%
        ggplot(aes(Age, Value, color = Birthyear)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Drug offenses with increasing age among birth cohorts, by sex",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Birth year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Crime participation and crime rate among cohorts, by sex") {
      six_three_ab_l %>%
        subset(Birthyear >= input$birthyear[1] & Birthyear <= input$birthyear[2]) %>% 
        ggplot(aes(x = Birthyear)) +
        facet_grid(Sex ~., scales="free") +
        geom_line(aes(y = `Proportion of prosecuted individuals`), size = 1.2, color = "#33A02C") +
        geom_point(aes(y = `Proportion of prosecuted individuals`), color = "#33A02C") +
        geom_line(aes(y = `Average number of criminal offenses` * 3.5), size = 1.2, color = "#1F78B4") +
        geom_point(aes(y = `Average number of criminal offenses` * 3.5), color = "#1F78B4") +
        labs(title = "Crime participation and crime rate among cohorts, by sex",
             y = "Proportion of prosecuted (%)",
             x = "Birth year") +
        scale_y_continuous("Proportion of prosecuted individuals (%) in population", 
                           labels = percent_format(scale = 1, accuracy = 1),
                           sec.axis = sec_axis(~ . / 3.5, name = "Average number of criminal offenses")) +
        scale_x_continuous(breaks = seq(1958, 1992, by = 2)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.title.y = element_text(color = "#33A02C"),
          axis.title.y.right = element_text(color = "#1F78B4"))
    }
    
    else if (input$var == "The disproportion of prosecuted crime among birth cohorts, by sex") {
      six_four_ab_l %>%
        subset(Birthyear %in% input$checkbirthyear) %>% 
        ggplot(aes(Birthyear, Value, fill = Percentile)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_bar(position="fill", stat="identity") +
        labs(title = "The disproportion of prosecuted crime among birth cohorts, by sex",
             y = "Proportions (%) of all prosecuted crimes",
             x = "Birth year") +
        geom_text(aes(label = Value),
                  position="fill", vjust=+2.1, size=3, color="white") +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_fill_grey() +
        scale_y_continuous(label = scales::percent) +
        scale_x_continuous(breaks = seq(1960, 1990, by = 5))
    }
    
    else if (input$var == "Prosecuted males and females at the age of 24, by place of birth") {
      seven_one_ab_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Country %in% input$checkcountry2) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Prosecuted males and females at the age of 24, by place of birth",
             x = "Year",
             y = "",
             color = "Country") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Highly active males and females at the age of 24, by place of birth") {
      seven_one_cd_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Country %in% input$checkcountry2) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Highly active males and females at the age of 24, by place of birth",
             x = "Year",
             y = "",
             color = "Country") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Prosecuted males and females at the age of 24, by socio-economic background") {
      seven_two_ab_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Income %in% input$checkincome) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Prosecuted males and females at the age of 24, by socio-economic background",
             x = "Year",
             y = "",
             color = "Parents income") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background") {
      seven_two_cd_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Income %in% input$checkincome) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background",
             y = "",
             x = "Year",
             color = "Parents income") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "24-year-olds from the low- and high-income category, by place of birth") {
      seven_three_ab_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Country %in% input$checkcountry2) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Income ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-year-olds from the low- and high-income category, by place of birth",
             y = "",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth") {
      seven_four_ab_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Country %in% input$checkcountry2) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Prosecution ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth",
             y = "",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth") {
      seven_five_ab_l %>%
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2] & Country %in% input$checkcountry2) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Prosecution ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth",
             y = "",
             x = "Year",
             color = "Country") +
        scale_x_continuous(breaks = seq(1990, 2017, by = 2)) +
        scale_y_continuous(limits = c(0, NA),
                           labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
  })
  
  # Create data for datatable ----
  tableInput <- reactive({
    
    if (input$var == "Reported crimes") {
      one_one_w %>% 
        select(Year, input$checkcrime1) %>% 
        subset(Year >= input$yearrange2[1] & Year <= input$yearrange2[2])}
    
    else if (input$var == "Number of prosecutions") {
      one_two_w %>% 
        subset(Year >= input$yearrange2[1] & Year <= input$yearrange2[2])}
    
    else if (input$var == "Prosecutions and prosecuted crimes") {
      one_three_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2])}
    
    else if (input$var == "Violence, theft, and drug offenses") {
      one_four_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2])}
    
    else if (input$var == "All crimes, by sex") {
      three_one_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, input$checksex)}
    
    else if (input$var == "Violence, theft, and drug offenses, by sex") {
      three_two_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2])}
    
    else if (input$var == "All crimes, by age and sex") {
      three_three_four_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "Theft, by age and sex") {
      three_five_ad_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "Violence, by age and sex") {
      three_five_be_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "Drug offenses, by age and sex") {
      three_five_cf_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "All crimes, by place of birth and sex") {
      four_one_four_two_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, ends_with(input$checkcountry1))}
    
    else if (input$var == "Violence, theft, and drug offenses. Males, by place of birth") {
      four_three_ac_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, starts_with(input$checkcountry1))}
    
    else if (input$var == "Violence, theft, and drug offenses. Females, by place of birth") {
      four_three_ac_w %>% 
        subset(Year >= input$yearrange1[1] & Year <= input$yearrange1[2]) %>% 
        select(Year, starts_with(input$checkcountry1))}
    
    else if (input$var == "All crimes. 15-24-year-old males and females, by socio-economic background") {
      five_one_ab_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, ends_with(input$checkincome))}
    
    else if (input$var == "Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background") {
      five_two_af_w <- five_two_af_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, contains(input$checkincome))}
    
    else if (input$var == "All crimes with increasing age among birth cohorts, by sex") {
      six_one_ab_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2]) %>% 
        select(Age, ends_with(input$checkbirthyear2))}
    
    else if (input$var == "Violent crime with increasing age among birth cohorts, by sex") {
      six_two_ad_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2]) %>% 
        select(Age, ends_with(input$checkbirthyear2))}
    
    else if (input$var == "Thefts with increasing age among birth cohorts, by sex") {
      six_two_be_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2]) %>% 
        select(Age, ends_with(input$checkbirthyear2))}
    
    else if (input$var == "Drug offenses with increasing age among birth cohorts, by sex") {
      six_two_cf_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2]) %>% 
        select(Age, ends_with(input$checkbirthyear2))}
    
    else if (input$var == "Crime participation and crime rate among cohorts, by sex") {
      six_three_ab_w %>% 
        subset(Birthyear >= input$birthyear[1] & Birthyear <= input$birthyear[2])}
    
    else if (input$var == "The disproportion of prosecuted crime among birth cohorts, by sex") {
      six_four_ab_w %>% 
        subset(Birthyear %in% input$checkbirthyear)}
    
    else if (input$var == "Prosecuted males and females at the age of 24, by place of birth") {
      seven_one_ab_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2])  %>% 
        select(Year, ends_with(input$checkcountry2))}
    
    else if (input$var == "Highly active males and females at the age of 24, by place of birth") {
      seven_one_cd_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, ends_with(input$checkcountry2))}
    
    else if (input$var == "Prosecuted males and females at the age of 24, by socio-economic background") {
      seven_two_ab_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, ends_with(input$checkincome))}
    
    else if (input$var == "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background") {
      seven_two_cd_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, ends_with(input$checkincome))}
    
    else if (input$var == "24-year-olds from the low- and high-income category, by place of birth") {
      seven_three_ab_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, starts_with(input$checkcountry2))}
    
    else if (input$var == "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth") {
      seven_four_ab_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, starts_with(input$checkcountry2))}
    
    else if (input$var == "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth") {
      seven_five_ab_w %>% 
        subset(Year >= input$yearrange3[1] & Year <= input$yearrange3[2]) %>% 
        select(Year, starts_with(input$checkcountry2))}
    
  })
  
  # Reactive page number
  output$pdfview <- renderUI({
    
    if (input$var == "Reported crimes") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=16")}
    
    else if (input$var == "Number of prosecutions") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=20")}
    
    else if (input$var == "Prosecutions and prosecuted crimes") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=22")}
    
    else if (input$var == "Violence, theft, and drug offenses") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=23")}
    
    else if (input$var == "All crimes, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=49")}
    
    else if (input$var == "Violence, theft, and drug offenses, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=50")}
    
    else if (input$var == "All crimes, by age and sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=52")}
    
    else if (input$var == "Theft, by age and sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=54")}
    
    else if (input$var == "Violence, by age and sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=54")}
    
    else if (input$var == "Drug offenses, by age and sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=54")}
    
    else if (input$var == "All crimes, by place of birth and sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=59")}
    
    else if (input$var == "Violence, theft, and drug offenses. Males, by place of birth") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=62")}
    
    else if (input$var == "Violence, theft, and drug offenses. Females, by place of birth") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=64")}
    
    else if (input$var == "All crimes. 15-24-year-old males and females, by socio-economic background") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=69")}
    
    else if (input$var == "Violence, theft, and drug offenses. 15-24-year-old males and females, by socio-economic background") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=70")}
    
    else if (input$var == "All crimes with increasing age among birth cohorts, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Violent crime with increasing age among birth cohorts, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Thefts with increasing age among birth cohorts, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Drug offenses with increasing age among birth cohorts, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Crime participation and crime rate among cohorts, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=80")}
    
    else if (input$var == "The disproportion of prosecuted crime among birth cohorts, by sex") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=82")}
    
    else if (input$var == "Prosecuted males and females at the age of 24, by place of birth") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=86")}
    
    else if (input$var == "Highly active males and females at the age of 24, by place of birth") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=86")}
    
    else if (input$var == "Prosecuted males and females at the age of 24, by socio-economic background") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=89")}
    
    else if (input$var == "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=89")}
    
    else if (input$var == "24-year-olds from the low- and high-income category, by place of birth") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=91")}
    
    else if (input$var == "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=91")}
    
    else if (input$var == "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=91")}
    
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
        caption = htmltools::tags$caption(style = 'caption-side: top; color:gray; font-size:130% ;',input$var))
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
    })
}

# App ----
shinyApp(ui, server)