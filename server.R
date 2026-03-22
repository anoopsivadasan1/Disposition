library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(plotly)
library(DT)

#---------------------------
# Load Excel data


# file_path <- "C:/Users/cex/OneDrive/Documents/Shiny_Practice/New folder/sample_disposition_dashboard_data.xlsx"

file_path <- "sample_disposition_dashboard_data.xlsx"

# Updated study name"

parent <- read_excel(file_path, sheet = "parent")
ole <- read_excel(file_path, sheet = "ole")


#---------------------------
# SERVER
#---------------------------

server <- function(input, output, session) {
  
  #---------------------------
  # Phase selector
  #---------------------------
  
  output$phase_ui <- renderUI({
    
    req(input$study)
    
    if(input$study == "parent"){
      
      selectInput(
        "phase",
        "Study Phase",
        choices = c("Induction","Maintenance"),
        selected = "Induction"
      )
      
    }
    
  })
  
  #---------------------------
  # Cohort selector
  #---------------------------
  
  output$cohort_ui <- renderUI({
    
    req(input$study)
    
    if(input$study == "parent"){
      
      req(input$phase)
      
      if(input$phase == "Induction"){
        
        selectInput(
          "cohort",
          "Cohort",
          choices = c("All","1","2","3"),
          selected = "All"
        )
        
      }
      
    }
    
  })
  
  #---------------------------
  # Reactive dataset
  #---------------------------
  
  data <- reactive({
    
    req(input$study)
    
    if(input$study == "parent"){
      
      df <- parent
      
      req(input$phase)
      
      if(input$phase == "Induction"){
        
        df <- df %>% filter(SAFFIFL == "Y")
        
      }
      
      if(input$phase == "Maintenance"){
        
        df <- df %>% filter(SAFFMFL == "Y")
        
      }
      
      if(!is.null(input$cohort) && input$cohort != "All"){
        
        df <- df %>% filter(COHORT == as.numeric(input$cohort))
        
      }
      
    } else {
      
      df <- ole %>% filter(OLEFL == "Y")
      
    }
    
    df
    
  })
  
  #---------------------------
  # Study Disposition Pie
  #---------------------------
  
  output$pie1 <- renderPlotly({
    
    df <- data()
    
    if(input$study == "parent"){
      
      summary <- df %>%
        filter(!is.na(COMPSTDI)) %>%
        count(COMPSTDI)
      
      plot_ly(summary,
              labels = ~COMPSTDI,
              values = ~n,
              type = "pie")
      
    } else {
      
      summary <- df %>%
        filter(!is.na(COMPSTDO)) %>%
        count(COMPSTDO)
      
      plot_ly(summary,
              labels = ~COMPSTDO,
              values = ~n,
              type = "pie")
      
    }
    
  })
  
  #---------------------------
  # Study Discontinuation Reasons
  #---------------------------
  
  output$bar1 <- renderPlotly({
    
    df <- data()
    
    if(input$study == "parent"){
      
      summary <- df %>%
        filter(DSSTDRSI != "") %>%
        count(DSSTDRSI)
      
      plot_ly(summary,
              x = ~DSSTDRSI,
              y = ~n,
              type = "bar")
      
    } else {
      
      summary <- df %>%
        filter(STDDRSO != "") %>%
        count(STDDRSO)
      
      plot_ly(summary,
              x = ~STDDRSO,
              y = ~n,
              type = "bar")
      
    }
    
  })
  
  #---------------------------
  # Drug Disposition Pie
  #---------------------------
  
  output$pie2 <- renderPlotly({
    
    df <- data()
    
    if(input$study == "parent"){
      
      summary <- df %>%
        filter(!is.na(DISCTRT)) %>%
        count(DISCTRT)
      
      plot_ly(summary,
              labels = ~DISCTRT,
              values = ~n,
              type = "pie")
      
    } else {
      
      summary <- df %>%
        filter(!is.na(DISTRTO)) %>%
        count(DISTRTO)
      
      plot_ly(summary,
              labels = ~DISTRTO,
              values = ~n,
              type = "pie")
      
    }
    
  })
  
  #---------------------------
  # Drug Discontinuation Reasons
  #---------------------------
  
  output$bar2 <- renderPlotly({
    
    df <- data()
    
    if(input$study == "parent"){
      
      summary <- df %>%
        filter(DSTRTRS != "") %>%
        count(DSTRTRS)
      
      plot_ly(summary,
              x = ~DSTRTRS,
              y = ~n,
              type = "bar")
      
    } else {
      
      summary <- df %>%
        filter(TRTDRSO != "") %>%
        count(TRTDRSO)
      
      plot_ly(summary,
              x = ~TRTDRSO,
              y = ~n,
              type = "bar")
      
    }
    
  })
  
  #---------------------------
  # Summary table
  #---------------------------
  
  output$summary <- renderDataTable({
    
    df <- data()
    
    if(input$study == "parent"){
      
      df %>% count(DSSTDRSI)
      
    } else {
      
      df %>% count(STDDRSO)
      
    }
    
  })
  
  #---------------------------
  # Subject listing
  #---------------------------
  
  output$listing <- renderDataTable({
    
    datatable(
      data(),
      options = list(pageLength = 10)
    )
    
  })
  
}

