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
      sidebarPanel(checkboxGroupInput("SOC", label = h3("System Organ Class (SOC)"), 
                                      choices = list("Blood and lymphatic" = "Blood and lymphatic system disorders", 
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
                                      selected = c("Blood and lymphatic system disorders", 
                                                   "Cardiac disorders", 
                                                   "Congenital, familial and genetic disorders", 
                                                   "Ear and labyrinth disorders",                                                      "Endocrine disorders", 
                                                   "Eye disorders", 
                                                   "Gastrointestinal disorders", 
                                                   "General disorders and administration site conditions", 
                                                   "Hepatobiliary disorders", 
                                                   "Immune system disorders", 
                                                   "Infections and infestations", 
                                                   "Injury, poisoning and procedural complications", 
                                                   "Investigations", 
                                                   "Metabolism and nutrition disorders", 
                                                   "Musculoskeletal and connective tissue disorders", 
                                                   "Neoplasms benign, malignant and unspecified (incl cysts and polyps)", 
                                                   "Nervous system disorders", 
                                                   "Pregnancy, puerperium and perinatal conditions", 
                                                   "Psychiatric disorders", 
                                                   "Renal and urinary disorders", 
                                                   "Reproductive system and breast disorders", 
                                                   "Respiratory, thoracic and mediastinal disorders", 
                                                   "Skin and subcutaneous tissue disorders", 
                                                   "Social circumstances", 
                                                   "Surgical and medical procedures", 
                                                   "Vascular disorders")
                                        )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("ver.4.0.3", DT::dataTableOutput("dtl1")),
                              tabPanel("ver 5.0", DT::dataTableOutput("dtl2"))
                              ))
    ) 
  )

# Server logic----

server <- function(input, output) {
  CTCAEv4_subset <- reactive({
    filter(CTCAEv4, CTCAE_v4.0_SOC %in% input$SOC)
  })
  
  CTCAEv5_subset <- reactive({
    filter(CTCAEv5, MedDRA_SOC %in% input$SOC)
  })
  
  output$dtl1 <- DT::renderDataTable(CTCAEv4_subset())
  
  output$dtl2 <- DT::renderDataTable(CTCAEv5_subset())
}

# Run the app
shinyApp(ui, server)
