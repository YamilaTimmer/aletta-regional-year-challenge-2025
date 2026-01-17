library(shiny)
library(tidyverse)
library(pROC)

diabetes_model <- readRDS("../models/diabetes_model.rds")
hypertension_model <- readRDS("../models/hypertension_model.rds")

server <- function(input, output, session) {
  
  # ============================================================
  # TAB 1 — RISK PREDICTION
  # ============================================================
  observeEvent(input$predict, {
    
    
    lat <- zipcode_data$latitude[zipcode_data$postcode == input$zipcode]
    lon <- zipcode_data$longitude[zipcode_data$postcode == input$zipcode]

    length_m = input$body_height/100
    bmi_t1 = input$body_weight / (length_m^2)
    

    # Build new-person data frame
    if (input$prediction == "Diabetes") {
      new_person <- data.frame(
        age = input$age,
        gender = as.numeric(input$gender),
        circumference_waist_1 = input$waist,
        longitude = lon,
        nova_foodintake_1 = input$nova1,
        bmi_t1 = bmi_t1

      )
      
      prob <- predict(diabetes_model, newdata = new_person, type = "response")
      
      output$message <- renderText("Predicted diabetes risk (5–10 years):")
      output$risk <- renderText(paste0(round(prob * 100, 2), " %"))
      
    } else if (input$prediction == "Hypertension") {
      new_person <- data.frame(
        age = input$age,
        gender = as.numeric(input$gender),
        circumference_waist_1 = input$waist,
        latitude = lat,
        alcohol_intake = input$alcohol_intake,
        nova_foodintake_1 = input$nova1,
        bmi_t1 = bmi_t1
      )
      
      prob <- predict(hypertension_model, newdata = new_person, type = "response")
      
      output$message <- renderText("Predicted hypertension risk (5–10 years):")
      output$risk <- renderText(paste0(round(prob * 100, 2), " %"))
    }
  })
  
  # Limitations
  output$limitations <- renderUI({
    tagList(
      p("• Dietary patterns change over time; models rely mainly on T1–T2 data."),
      p("• Cholesterol was significant but excluded because it is not easily measured at home."),
      p("• Predictions are based on Lifelines cohort data and may not generalize to all populations."),
      p("• This tool is not a medical device and does not replace professional medical advice.")
    )
  })
  
  # ============================================================
  # TAB 2 — BIOMARKER PREDICTION
  # ============================================================
  observeEvent(input$predict_bio, {
    # Placeholder logic — replace with your biomarker model later
    predicted_value <- input$bio_bmi * 0.3 + input$bio_waist * 0.1 + input$bio_age * 0.05
    
    output$bio_result <- renderText({
      paste("Example biomarker score:", round(predicted_value, 2))
    })
  })
}
