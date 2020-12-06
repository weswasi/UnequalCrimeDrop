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
               code(tags$a(href="https://www.criminology.su.se/forskning/forskningsprojekt/den-oj%C3%A4mlika-brottsligheten-1.255012", 
                           "The Uneven Crime Drop")),"(Department of Criminology, University of Stockholm)."),
      
      # Drop-down list ----
      selectInput("var", 
                  label = strong("Choose data to display"),
                  choices = list(
                    "The general development of crime and prosecutions:" = c(
                      "Reported crimes (per 100 000 population)",
                      "Number of prosecutions (all crimes)",
                      "Number of prosecutions and number of prosecuted crimes (per 100 000 population)"),
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
                      "All crimes. Males and females, 15-24 years, by parents income",
                      "Violence, theft, and drug offenses, by parents income. Male and females, 15-24 years"),
                    "The risk (%) of being prosecuted:" = c(
                      "All crimes with increasing age among birth cohorts of men and women",
                      "Violent crime with increasing age among birth cohorts of men and women",
                      "Thefts with increasing age among birth cohorts of men and women",
                      "Drug offenses with increasing age among birth cohorts of men and women",
                      "Crime participation and crime rate among cohorts of men and women born between 1958 and 1992",
                      "The disproportion of prosecuted crime among birth cohorts of men and women"),
                    "Share (%) of:" = c(
                      "Prosecuted males and females at the age of 24, by place of birth",
                      "Highly active males and females at the age of 24, by place of birth",
                      "Prosecuted males and females at the age of 24, by socio-economic background",
                      "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background",
                      "24-year-olds from the low- and high-income category, by place of birth",
                      "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth",
                      "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth")),
                  selected = "Violence, theft, and drug offenses, by parents income. Male and females, 15-24 years",
                  selectize = FALSE),
      
      # Slider:Year ----
      sliderInput("range", strong("Year"), 
                  1973, 2017, c(1973,2017), sep= "", step = 1),
      
      # Slider: Age ----
      conditionalPanel(
        condition = 
          "input.var == 'Crime participation and crime rate among cohorts of men and women born between 1958 and 1992'",
        sliderInput("birthyear", strong("Birthyear"), 
                    1958, 1992, c(1958, 1992), sep= "", step = 1)),
      
      # Slider: Birthyear ----
      conditionalPanel(
        condition = 
          "input.var == 'All crimes with increasing age among birth cohorts of men and women'",
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
      need(input$checkage, "Please select age :)"))
    
    # Plot ---
    if (input$var == "Reported crimes (per 100 000 population)") {
      one_one_l %>% 
        subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Reported crimes (per 100 000 population)",
             y = "Crime per 100 000",
             x = "Year",
             color = "Crime") +
        scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
        theme(plot.title = element_text(color="gray50"),
              text = element_text(size=15), 
              axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Number of prosecutions (all crimes)") {
      one_two_l %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Source)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Number of prosecutions (all crimes)",
             y = "Prosecutions",
             x = "Year",
             color = "Data source") +
        scale_x_continuous(breaks = seq(1975, 2017, by = 3)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Number of prosecutions and number of prosecuted crimes (per 100 000 population)"){
      one_three_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Prosecutions)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Number of prosecutions and number of prosecuted crimes (per 100 000 population)",
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
    
    else if (input$var == "Violence, theft, and drug offenses"){
      one_four_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses",
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
    
    else if (input$var == "All crimes, by sex") {
      three_one_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Sex)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes, by sex",
             y = "Prosecutions per 100 000",
             x = "Year",
             color = "Sex") +
        scale_x_continuous(breaks = seq(1973, 2017, by = 4)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15),
          axis.text.x = element_text(angle = -45)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violence, theft, and drug offenses, by sex") {
      three_two_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Crime %in% input$checkcrime) %>% 
        ggplot(aes(Year, Value, color = Crime)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses, by sex",
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
    
    else if (input$var == "All crimes, by age and sex") {
      three_three_four_l %>% 
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes, by sex",
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
    
    else if (input$var == "Theft, by age and sex") {
      three_five_ad_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Theft, by age and sex",
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
    
    else if (input$var == "Violence, by age and sex") {
      three_five_be_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, by age and sex",
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
    
    else if (input$var == "Drug offenses, by age and sex") {
      three_five_cf_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Age %in% input$checkage) %>% 
        ggplot(aes(Year, Value, color = Age)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Drug offenses, by age and sex",
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
    
    else if (input$var == "All crimes, by place of birth and sex") {
      four_one_four_two_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Country %in% input$checkregion) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes, by place of birth and sex",
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
    
    else if (input$var == "Violence, theft, and drug offenses. Males, by place of birth") {
      four_three_ac_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Country %in% input$checkregion) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Crime ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses. Males, by place of birth",
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
    
    else if (input$var == "Violence, theft, and drug offenses. Females, by place of birth") {
      four_three_df_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2] & Country %in% input$checkregion) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Crime ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses. Females, by place of birth",
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
    
    else if (input$var == "All crimes. Males and females, 15-24 years, by parents income") {
      five_one_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes. Males and females, 15-24 years, by parents income",
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
    
    else if (input$var == "Violence, theft, and drug offenses, by parents income. Male and females, 15-24 years") {
      five_two_af_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ Crime, scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violence, theft, and drug offenses, by parents income. Male and females, 15-24 years",
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
    
    else if (input$var == "All crimes with increasing age among birth cohorts of men and women") {
      six_one_ab_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "All crimes with increasing age among birth cohorts of men and women",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Violent crime with increasing age among birth cohorts of men and women") {
      six_two_ad_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Violent crime with increasing age among birth cohorts of men and women",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Thefts with increasing age among birth cohorts of men and women") {
      six_two_be_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Thefts with increasing age among birth cohorts of men and women",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Drug offenses with increasing age among birth cohorts of men and women") {
      six_two_cf_l %>%
        subset(Age >= input$age[1] & Age <= input$age[2]) %>%
        ggplot(aes(Age, Value, color = Year)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Drug offenses with increasing age among birth cohorts of men and women",
             y = "Proportion of prosecuted (%)",
             x = "Age",
             color = "Year") +
        scale_x_continuous(breaks = seq(15, 58, by = 2)) +
        scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
        theme(
          plot.title = element_text(color="gray50"),
          text = element_text(size=15)) +
        scale_colour_brewer(palette = "Paired")
    }
    
    else if (input$var == "Crime participation and crime rate among cohorts of men and women born between 1958 and 1992") {
      six_three_ab_l %>%
        subset(Birthyear >= input$birthyear[1] & Birthyear <= input$birthyear[2]) %>% 
        ggplot(aes(x = Birthyear)) +
        facet_grid(Sex ~., scales="free") +
        geom_line(aes(y = `Proportion of prosecuted individuals`), size = 1.2, color = "#33A02C") +
        geom_point(aes(y = `Proportion of prosecuted individuals`), color = "#33A02C") +
        geom_line(aes(y = `Average number of criminal offenses` * 3.5), size = 1.2, color = "#1F78B4") +
        geom_point(aes(y = `Average number of criminal offenses` * 3.5), color = "#1F78B4") +
        labs(title = "Crime participation and crime rate among cohorts of men and women born between 1958 and 1992",
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
    
    else if (input$var == "The disproportion of prosecuted crime among birth cohorts of men and women") {
      six_four_ab_l %>%
        ggplot(aes(Birthyear, Value, fill = Percentile)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_bar(position="fill", stat="identity") +
        labs(title = "The disproportion of prosecuted crime among birth cohorts of men and women",
             y = "Proportions of all crimes",
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
    
    else if (input$var == "Prosecuted males and females at the age of 24, by place of birth") {
      seven_one_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Prosecuted males and females at the age of 24, by place of birth",
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
    
    else if (input$var == "Highly active males and females at the age of 24, by place of birth") {
      seven_one_cd_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Highly active males and females at the age of 24, by place of birth",
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
    
    else if (input$var == "Prosecuted males and females at the age of 24, by socio-economic background") {
      seven_two_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Prosecuted males and females at the age of 24, by socio-economic background",
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
    
    else if (input$var == "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background") {
      seven_two_cd_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Income)) +
        facet_grid(Sex ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "Highly active males (≥ 4 prosecutions) and females (≥ 2 prosecutions) at the age of 24, by socio-economic background",
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
    
    else if (input$var == "24-year-olds from the low- and high-income category, by place of birth") {
      seven_three_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Income ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-year-olds from the low- and high-income category, by place of birth",
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
    
    else if (input$var == "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth") {
      seven_four_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Prosecution ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth",
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
    
    else if (input$var == "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth") {
      seven_five_ab_l %>%
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        ggplot(aes(Year, Value, color = Country)) +
        facet_grid(Prosecution ~ ., scales="free") +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(title = "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth",
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
    
    if (input$var == "Reported crimes (per 100 000 population)") {
      one_one_w %>% 
        select(Year, input$checkcrime) %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Number of prosecutions (all crimes)") {
      one_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Number of prosecutions and number of prosecuted crimes (per 100 000 population)") {
      one_three_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Violence, theft, and drug offenses") {
      one_four_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "All crimes, by sex") {
      three_one_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Violence, theft, and drug offenses, by sex") {
      three_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "All crimes, by age and sex") {
      three_three_four_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "Theft, by age and sex") {
      three_five_ad_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "Violence, by age and sex") {
      three_five_be_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "Narkotikabrott efter ålder och kön") {
      three_five_cf_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkage))}
    
    else if (input$var == "All crimes, by place of birth and sex") {
      four_one_four_two_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, ends_with(input$checkregion))}
    
    else if (input$var == "Violence, theft, and drug offenses. Males, by place of birth") {
      four_three_ac_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, starts_with(input$checkregion))}
    
    else if (input$var == "Violence, theft, and drug offenses. Females, by place of birth") {
      four_three_ac_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2]) %>% 
        select(Year, starts_with(input$checkregion))}
    
    else if (input$var == "All crimes. Males and females, 15-24 years, by parents income") {
      five_one_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Violence, theft, and drug offenses, by parents income. Male and females, 15-24 years") {
      five_two_af_w <- five_two_af_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "All crimes with increasing age among birth cohorts of men and women") {
      six_one_ab_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])}
    
    else if (input$var == "Violent crime with increasing age among birth cohorts of men and women") {
      six_two_ad_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])}
    
    else if (input$var == "Thefts with increasing age among birth cohorts of men and women") {
      six_two_be_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])}
    
    else if (input$var == "Drug offenses with increasing age among birth cohorts of men and women") {
      six_two_cf_w %>% 
        subset(Age >= input$age[1] & Age <= input$age[2])}
    
    else if (input$var == "Crime participation and crime rate among cohorts of men and women born between 1958 and 1992") {
      six_three_ab_w %>% 
        subset(Birthyear >= input$birthyear[1] & Birthyear <= input$birthyear[2])}
    
    else if (input$var == "The disproportion of prosecuted crime among birth cohorts of men and women") {
      six_four_ab_w}
    
    else if (input$var == "Prosecuted males and females at the age of 24, by place of birth") {
      seven_one_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Highly active males and females at the age of 24, by place of birth") {
      seven_one_cd_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Prosecuted males and females at the age of 24, by socio-economic background") {
      seven_two_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "Highly active criminal male (≥ 4 prosecutions) and females (≥ 4 prosecutions) at the age of 24, by socio-economic background") {
      seven_two_cd_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "24-year-olds from the low- and high-income category, by place of birth") {
      seven_three_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "24-year-old males from low-income families with ≥ 1 prosecution and ≥ 4 prosecutions, by place of birth") {
      seven_four_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
    else if (input$var == "24-year-old females from low-income families with ≥ 1 prosecution and ≥ 2 prosecutions, by place of birth") {
      seven_five_ab_w %>% 
        subset(Year >= input$range[1] & Year <= input$range[2])}
    
  })
  
  # Reactive page number
  output$pdfview <- renderUI({
    
    if (input$var == "Reported crimes (per 100 000 population)") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=16")}
    
    else if (input$var == "Number of prosecutions (all crimes)") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=20")}
    
    else if (input$var == "Number of prosecutions and number of prosecuted crimes (per 100 000 population)") {
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
    
    else if (input$var == "All crimes. Males and females, 15-24 years, by parents income") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=69")}
    
    else if (input$var == "Violence, theft, and drug offenses, by parents income. Male and females, 15-24 years") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=70")}
    
    else if (input$var == "All crimes with increasing age among birth cohorts of men and women") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Violent crime with increasing age among birth cohorts of men and women") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Thefts with increasing age among birth cohorts of men and women") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Drug offenses with increasing age among birth cohorts of men and women") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=76")}
    
    else if (input$var == "Crime participation and crime rate among cohorts of men and women born between 1958 and 1992") {
      tags$iframe(style="height:980px; width:780px", src="rapport.pdf#page=80")}
    
    else if (input$var == "The disproportion of prosecuted crime among birth cohorts of men and women") {
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