# Load packages ----
library(shiny)
library(tidyverse)
library(DT)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel(h1("CTCAE")),  
    sidebarLayout(
      sidebarPanel(checkboxGroupInput("display", label = h3("View Hidden Column"), 
                                      choices = list("MedDRA" = "MedDRA", 
                                                     "Definition" = "Definition", 
                                                     "Errata(v4.0)" = "Errata", 
                                                     "Note(v5.0)" = "Note", 
                                                     "Change(v5.0)" = "Change"), 
                                      select = c("SOC", "Term", "Grade")), 
                   selectInput("SOC", label = h3("System Organ Class (SOC)"), 
                                      choices = list("All" = "All", 
                                                     "Blood and lymphatic" = "Blood and lymphatic system disorders", 
                                                     "Cardiac" = "Cardiac disorders", 
                                                     "Congenital" = "Congenital, familial and genetic disorders", 
                                                     "Ear and labyrinth" = "Ear and labyrinth disorders", 
                                                     "Endocrine" = "Endocrine disorders", 
                                                     "Eye" = "Eye disorders", 
                                                     "Gastric" = "Gastrointestinal disorders", 
                                                     "General" = "General disorders and administration site conditions", 
                                                     "Hepatobiliary" = "Hepatobiliary disorders", 
                                                     "Immune system" = "Immune system disorders", 
                                                     "Infections, infestations" = "Infections and infestations", 
                                                     "Injury, poisoning, procedural" = "Injury, poisoning and procedural complications", 
                                                     "Investigations" = "Investigations", 
                                                     "Metabolic, nutrition" = "Metabolism and nutrition disorders", 
                                                     "Musculoskeletal, CTD" = "Musculoskeletal and connective tissue disorders", 
                                                     "Neoplasms" = "Neoplasms benign, malignant and unspecified (incl cysts and polyps)", 
                                                     "Nervous system" = "Nervous system disorders", 
                                                     "Pregnancy" = "Pregnancy, puerperium and perinatal conditions", 
                                                     "Psychiatric" = "Psychiatric disorders", 
                                                     "Renal, urinary" = "Renal and urinary disorders", 
                                                     "Reproductive system and breast" = "Reproductive system and breast disorders", 
                                                     "Respiratory, thoracic, mediastinal" = "Respiratory, thoracic and mediastinal disorders", 
                                                     "Skin, subcutaneous" = "Skin and subcutaneous tissue disorders", 
                                                     "Social circumstances" = "Social circumstances", 
                                                     "Surgical, medical procedures" = "Surgical and medical procedures", 
                                                     "Vascular" = "Vascular disorders"),
                                      selected = "All"
                                        ), 
                   sliderInput("Grade", label = h3("CTCAE Grade"), 
                               min = 1, max = 5, value = c(1, 5))
                   ), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("ver.4.0.3", DT::dataTableOutput("dtl1")),
                              tabPanel("ver 5.0", DT::dataTableOutput("dtl2"))
                              ))
    ) 
  )

# Server logic----

server <- function(input, output) {
  CTCAEv4_subset1 <- reactive({
    display_column(CTCAEv4, input$display, "MedDRA") %>% 
    display_column(input$display, "Definition") %>% 
    display_column(input$display, "Errata")
  })
  
  CTCAEv5_subset1 <- reactive({
    display_column(CTCAEv5, input$display, "MedDRA") %>% 
      display_column(input$display, "Definition") %>% 
      display_column(input$display, "Note") %>% 
      display_column(input$display, "Change")
  })
  
  CTCAEv4_subset2 <- reactive({
    if(input$SOC == "All") {
      return(CTCAEv4_subset1())
    }else{
      filter(CTCAEv4_subset1(), CTCAE_v4.0_SOC == input$SOC)
    }
  })
  
  CTCAEv5_subset2 <- reactive({
    if(input$SOC == "All") {
      return(CTCAEv5_subset1())
    }else{
      filter(CTCAEv5_subset1(), CTCAE_v4.0_SOC == input$SOC)
    }
  })
  
  CTCAEv4_subset3 <- reactive({
    select_grade(CTCAEv4_subset2(), input$Grade[1], input$Grade[2])
  })
  
  CTCAEv5_subset3 <- reactive({
    select_grade(CTCAEv5_subset2(), input$Grade[1], input$Grade[2])
  })
  
  output$dtl1 <- DT::renderDataTable(CTCAEv4_subset3())
  
  output$dtl2 <- DT::renderDataTable(CTCAEv5_subset3())
}

# Run the app
shinyApp(ui, server)
