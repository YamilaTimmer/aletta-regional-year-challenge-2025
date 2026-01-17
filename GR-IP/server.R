library(shiny)
library(tidyverse)
library(pROC)

diabetes_model <- readRDS("../models/diabetes_model.rds")
hypertension_model <- readRDS("../models/hypertension_model.rds")

body_wt_model <- readRDS("../models/bodyweight/bodywt_nnet_model.rds") 
body_wt_preproc <- readRDS("../models/bodyweight/bodywt_preprocess.rds") 
body_wt_dummies <- readRDS("../models/bodyweight/bodywt_dummyVars.rds")

cholesterol_model <- readRDS("../models/cholesterol/chol1_nnet_model.rds") 
cholesterol_preproc <- readRDS("../models/cholesterol/chol1_preprocess.rds") 
cholesterol_dummies <- readRDS("../models/cholesterol/chol1_dummyVars.rds")

glucose_model <- readRDS("../models/glucose/glucose1_nnet_model.rds") 
glucose_preproc <- readRDS("../models/glucose/glucose1_preprocess.rds") 
glucose_dummies <- readRDS("../models/glucose/glucose1_dummyVars.rds")


server <- function(input, output, session) {
  
  # ============================================================
  # TAB 1 — RISK PREDICTION
  # ============================================================
  observeEvent(input$predict, {
    
    
    lat <- zipcode_data$latitude[zipcode_data$postcode == input$zipcode]
    lon <- zipcode_data$longitude[zipcode_data$postcode == input$zipcode]
    
    length_m = input$body_height/100
    bmi_t1 = input$body_weight / (length_m^2)
    
    
    # ============================================================
    # Prediction 1 — DIABETES PREDICTION
    # ============================================================
    
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
      
    } 
    
    
    # ============================================================
    # Prediction 2 — HYPERTENSION PREDICTION
    # ============================================================
    else if (input$prediction == "Hypertension") {
      new_person <- data.frame(
        age = input$age,
        gender = as.numeric(input$gender),
        circumference_waist_1 = input$waist,
        latitude = lat,
        alcohol_intake = input$alcohol_intake * 10,
        nova_foodintake_1 = input$nova1,
        bmi_t1 = bmi_t1
      )
      
      prob <- predict(hypertension_model, newdata = new_person, type = "response")
      
      output$message <- renderText("Predicted hypertension risk (5–10 years):")
      output$risk <- renderText(paste0(round(prob * 100, 2), " %"))
      
    } 
    
    
    # ============================================================
    # Prediction 3 — WEIGHT GAIN
    # ============================================================
    else if (input$prediction == "Weight Gain") {
      
      new_person <- data.frame(
        
        age = input$age,
        gender = as.numeric(input$gender),
        circumference_waist_1 = input$waist,
        circumference_hip_1 = input$hip,
        body_weight_1 = input$body_weight,
        body_length_1 = input$body_height,
        longitude = lon,
        alcohol_intake = input$alcohol_intake * 10,
        nova_foodintake_1 = input$nova1,
        nova_foodintake_4 = input$nova4,
        kcal_intake = input$kcal,
        added_sugar = input$sugar
      )
      
      new_processed <- predict(body_wt_preproc, new_person)
      new_dummied <- predict(body_wt_dummies, new_processed)
      
      prob <- predict(body_wt_model, new_dummied)
      
      output$message <- renderText("Predicted weight gain (5–10 years):")
      output$risk <- renderText(paste0(round(prob - input$body_weight, 2), " kg"))
      
      output$bmi_plot <- renderPlot({
        
        bmi_value <- bmi_t1
        
        bmi_df <- tibble(
          category = factor(c("Underweight", "Healthy", "Overweight", "Obese"),
                            levels = c("Underweight", "Healthy", "Overweight", "Obese")),
          xmin = c(0, 18.5, 25, 30),
          xmax = c(18.5, 25, 30, 50)
        )
        
        ggplot() +
          geom_rect(data = bmi_df,
                    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = category),
                    alpha = 0.6) +
          geom_vline(xintercept = bmi_value, color = "black", size = 2) +
          annotate("text", x = bmi_value, y = 1.1,
                   label = paste0("Your BMI: ", round(bmi_value, 1)),
                   size = 5, fontface = "bold") +
          scale_fill_manual(values = c(
            "Underweight" = "#74add1",
            "Healthy" = "#a1d99b",
            "Overweight" = "#fdae61",
            "Obese" = "#d73027"
          )) +
          theme_minimal() +
          theme(axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(face = "bold")
          ) +
          labs(x = "BMI", title = "Your BMI Classification")
        
      })
      
      output$waist_plot <- renderPlot({
        
        waist <- input$waist
        gender <- as.numeric(input$gender)   # 0 = female, 1 = male
        
        if (gender == 1) {
          # Male
          wc_df <- tibble(
            category = factor(c("Healthy", "Elevated", "High risk"),
                              levels = c("Healthy", "Elevated", "High risk")),
            xmin = c(0, 94, 102),
            xmax = c(94, 102, 150)
          )
        } else {
          # Female
          wc_df <- tibble(
            category = factor(c("Healthy", "Elevated", "High risk"),
                              levels = c("Healthy", "Elevated", "High risk")),
            xmin = c(0, 80, 88),
            xmax = c(80, 88, 150)
          )
        }
        
        
        ggplot() +
          geom_rect(data = wc_df,
                    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = category),
                    alpha = 0.7) +
          geom_vline(xintercept = waist, color = "black", size = 2) +
          annotate("text", x = waist, y = 1.15,
                   label = paste0("Your waist circumference: ", round(waist, 1), " cm"),
                   size = 5, fontface = "bold") +
          scale_fill_manual(values = c(
            "Healthy"   = "#a1d99b",
            "Elevated" = "#fdae61",
            "High risk"  = "#d73027"
          )) +
          labs( x = "Waist circumference (cm)", y = NULL, 
                title = "Classification waist circumference", 
                caption = "Categories based on Voedingscentrum"
          ) +
          
          theme_minimal() +
          theme(
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(face = "bold")
          )
      })
      
      
    } 
    
    # ============================================================
    # Prediction 4 — CHOLESTEROL
    # ============================================================
    else if (input$prediction == "Cholesterol"){
      
      new_person <- data.frame(
        
        age = input$age,
        gender = as.numeric(input$gender),
        circumference_waist_1 = input$waist,
        circumference_hip_1 = input$hip,
        body_weight_1 = input$body_weight,
        body_length_1 = input$body_height,
        longitude = lon,
        alcohol_intake = input$alcohol_intake * 10,
        nova_foodintake_1 = input$nova1,
        nova_foodintake_4 = input$nova4,
        kcal_intake = input$kcal,
        added_sugar = input$sugar
      )
      
      new_processed <- predict(cholesterol_preproc, new_person)
      new_dummied <- predict(cholesterol_dummies, new_processed)
      
      cholesterol <- predict(cholesterol_model, new_dummied)
      
      output$message <- renderText("Predicted cholesterol (5–10 years):")
      output$risk <- renderText(paste0(round(cholesterol, 2), " mmol/l"))
      
      
      output$cholesterol_plot <- renderPlot({
        
        
        cholesterol_df <- tibble(
          category = factor(c("Optimal", "Borderline High", "High"),
                            levels = c("Optimal", "Borderline High", "High")),
          xmin = c(0, 5.2, 6.2),
          xmax = c(5.2, 6.2, 10)
        )
        
        
        ggplot() +
          geom_rect(data = cholesterol_df,
                    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = category),
                    alpha = 0.6) +
          geom_vline(xintercept = cholesterol, color = "black", size = 2) +
          annotate("text", x = cholesterol, y = 1.1,
                   label = paste0("Your cholesterol levels: ", round(cholesterol, 2)),
                   size = 5, fontface = "bold") +
          scale_fill_manual(values = c(
            "Optimal" = "#a1d99b",
            "Borderline High" = "#fdae61",
            "High" = "#d73027"
          )) +
          theme_minimal() +
          theme(axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(face = "bold")
          ) +
          labs(x = "Cholesterol (mmol/L)", 
               title = "Your Cholesterol Classification",
               caption = "Categories based on informedhealth.org"
          )
        
      })
      
    }
    
    # ============================================================
    # Prediction 4 — GLUCOSE
    # ============================================================
    
    else {
      new_person <- data.frame(
        
        age = input$age,
        gender = as.numeric(input$gender),
        circumference_waist_1 = input$waist,
        circumference_hip_1 = input$hip,
        body_weight_1 = input$body_weight,
        body_length_1 = input$body_height,
        longitude = lon,
        alcohol_intake = input$alcohol_intake * 10,
        nova_foodintake_1 = input$nova1,
        nova_foodintake_4 = input$nova4,
        kcal_intake = input$kcal,
        added_sugar = input$sugar
      )
      
      new_processed <- predict(glucose_preproc, new_person)
      new_dummied <- predict(glucose_dummies, new_processed)
      
      glucose <- predict(glucose_model, new_dummied)
      
      output$message <- renderText("Predicted glucose (5–10 years):")
      output$risk <- renderText(paste0(round(glucose, 2), " mg/dL"))
      
      
      output$glucose_plot <- renderPlot({
        
        
        cholesterol_df <- tibble(
          category = factor(c("Optimal", "Borderline High", "High"),
                            levels = c("Optimal", "Borderline High", "High")),
          xmin = c(0, 5.2, 6.2),
          xmax = c(5.2, 6.2, 10)
        )
        
        
        ggplot() +
          geom_rect(data = cholesterol_df,
                    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = category),
                    alpha = 0.6) +
          geom_vline(xintercept = cholesterol, color = "black", size = 2) +
          annotate("text", x = cholesterol, y = 1.1,
                   label = paste0("Your cholesterol levels: ", round(cholesterol, 2)),
                   size = 5, fontface = "bold") +
          scale_fill_manual(values = c(
            "Optimal" = "#a1d99b",
            "Borderline High" = "#fdae61",
            "High" = "#d73027"
          )) +
          theme_minimal() +
          theme(axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(face = "bold")
          ) +
          labs(x = "Cholesterol (mmol/L)", title = "Your Cholesterol Classification")
        
      })
    }
    
    
  })
  
  # Notes/limitations
  output$notes <- renderUI({
    tagList(
      p("• Predictions are based on Lifelines cohort data and may not generalize to all populations."),
      p("• This tool is not a medical device and does not replace professional medical advice."),
      p("• User data is not stored.")
      
    )
  })
  
}
