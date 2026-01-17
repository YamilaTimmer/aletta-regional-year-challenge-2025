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

make_risk_plot <- function(
    risk_value,
    xlab = "Predicted risk",
    title = "Risk Classification",
    caption = NULL
) {
  
  risk_df <- tibble(
    category = factor(c("Low risk", "Elevated risk", "High risk"),
                      levels = c("Low risk", "Elevated risk", "High risk")),
    xmin = c(0, 0.20, 0.40),
    xmax = c(0.20, 0.40, 1.00)
  )
  
  ggplot() +
    geom_rect(
      data = risk_df,
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = category),
      alpha = 0.6
    ) +
    geom_vline(xintercept = risk_value, color = "black", size = 2) +
    scale_fill_manual(values = c(
      "Low risk" = "#a1d99b",
      "Elevated risk" = "#fdae61",
      "High risk" = "#d73027"
    )) +
    labs(
      x = xlab,
      y = NULL,
      title = title,
      caption = caption
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold")
    )
}


make_classification_plot <- function(
    value,
    categories_df,
    xlab,
    title,
    caption = NULL,
    label_prefix = "Your value:",
    colors
) {
  ggplot() +
    geom_rect(
      data = categories_df,
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = category),
      alpha = 0.6
    ) +
    geom_vline(xintercept = value, color = "black", size = 2) +
    scale_fill_manual(values = colors) +
    labs(
      x = xlab,
      y = NULL,
      title = title,
      caption = caption
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold")
    )
}


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
      
      output$diabetes_plot <- renderPlot({
        
        p <- prob
        
        base <- make_risk_plot(
          risk_value = p,
          xlab = "Predicted diabetes risk",
          title = "Your Diabetes Risk Classification",
          caption = "Risk thresholds: <20% low, 20–40% elevated, >40% high"
        )
        
        # Dynamic label placement
        if (p < 0.20) {
          base + annotate("text", x = p + 0.03, y = 0.5,
                          label = paste0(round(p*100,1), "%"),
                          size = 5, fontface = "bold", hjust = 0)
        } else {
          base + annotate("text", x = p - 0.03, y = 0.5,
                          label = paste0(round(p*100,1), "%"),
                          size = 5, fontface = "bold", hjust = 1)
        }
      })
      
      
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
      
      output$hypertension_plot <- renderPlot({
        
        p <- prob
        
        base <- make_risk_plot(
          risk_value = p,
          xlab = "Predicted hypertension risk",
          title = "Your Hypertension Risk Classification",
          caption = "Risk thresholds: <20% low, 20–40% elevated, >40% high"
        )
        
        if (p < 0.20) {
          base + annotate("text", x = p + 0.03, y = 0.5,
                          label = paste0(round(p*100,1), "%"),
                          size = 5, fontface = "bold", hjust = 0)
        } else {
          base + annotate("text", x = p - 0.03, y = 0.5,
                          label = paste0(round(p*100,1), "%"),
                          size = 5, fontface = "bold", hjust = 1)
        }
      })
      
      
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
      
      predicted_weight <- predict(body_wt_model, new_dummied)
      predicted_bmi <- predicted_weight / (length_m^2)
      
      
      output$message <- renderText("Predicted weight gain (5–10 years):")
      output$risk <- renderText(paste0(round(predicted_weight - input$body_weight, 2), " kg"))
      
      # ============================
      # BMI WARNING SYSTEM
      # ============================

      
      if ((bmi_t1 >= 18.5 && bmi_t1 < 25) && (predicted_bmi >= 18.5 && predicted_bmi < 25)) {
        
        # Normal weight
        output$warning_box <- renderUI({
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
               padding:15px; margin-top:15px;",
            strong("✔️ Healthy BMI: "),
            paste0("Your current (", round(bmi_t1,1), 
                   ") and predicted BMI (", round(predicted_bmi,1), 
                   ") are both in the healthy range.")
          )
        })
        
      } else if (bmi_t1 >= 30 || predicted_bmi >= 30) {
        
        # Obesity
        output$warning_box <- renderUI({
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
               padding:15px; margin-top:15px;",
            strong("⚠️ High-risk BMI: "),
            paste0("Your (predicted) BMI is in the obese range (current: ", round(bmi_t1,1),
                   ", predicted: ", round(predicted_bmi,1), 
                   "). This is associated with increased health risks, 
                   consider consulting a healthcare specialist")
          )
        })
        
        } else if (bmi_t1 < 18.5 || predicted_bmi < 18.5) {
          
          # Underweight
          output$warning_box <- renderUI({
            div(
              style = "background-color:#fff3cd; border-left:6px solid #e6a800;
               padding:15px; margin-top:15px;",
              strong("⚠️ BMI too low: "),
              paste0("Your (predicted) BMI is in the underweight range (current: ", round(bmi_t1,1),
                   ", predicted: ", round(predicted_bmi,1), 
                   "). This is associated with increased health risks, 
                   consider consulting a healthcare specialist.")
            )
          })
        
      } else {
        
        # Overweight
        output$warning_box <- renderUI({
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
               padding:15px; margin-top:15px;",
            strong("⚠️ Elevated BMI: "),
            paste0("Your (predicted) BMI is in the overweight range (current: ", round(bmi_t1,1),
                   ", predicted: ", round(predicted_bmi,1), 
                   "). Consider monitoring lifestyle habits or consulting 
                   a healthcare professional.")
          )
        })
      }
      
      
      output$bmi_plot <- renderPlot({
        
        bmi_value <- bmi_t1
        predicted_bmi <- predicted_weight / (length_m^2)
        
        bmi_df <- tibble(
          category = factor(c("Underweight", "Healthy", "Overweight", "Obese"),
                            levels = c("Underweight", "Healthy", "Overweight", "Obese")),
          xmin = c(0, 18.5, 25, 30),
          xmax = c(18.5, 25, 30, 50)
        )
        
        base_plot <- make_classification_plot(
          value = bmi_value,
          categories_df = bmi_df,
          xlab = "BMI",
          title = "Your BMI Classification",
          caption = "Categories based on Voedingscentrum",
          colors = c(
            "Underweight" = "#74add1",
            "Healthy" = "#a1d99b",
            "Overweight" = "#fdae61",
            "Obese" = "#d73027"
          )
        )
        
        # Dynamic label positioning
        if (predicted_bmi < bmi_value) {
          # Predicted is lower → label left of predicted, right of current
          base_plot +
            geom_vline(xintercept = predicted_bmi, color = "blue", size = 2, linetype = "dashed") +
            annotate(
              "text",
              x = predicted_bmi - 0.5,
              y = 0.5,
              label = paste0("Predicted BMI: ", round(predicted_bmi, 1)),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 1
            ) +
            annotate(
              "text",
              x = bmi_value + 0.5,
              y = 0.5,
              label = paste0("Current BMI: ", round(bmi_value, 1)),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 0
            )
        } else {
          # Predicted is higher → label right of predicted, left of current
          base_plot +
            geom_vline(xintercept = predicted_bmi, color = "blue", size = 2, linetype = "dashed") +
            annotate(
              "text",
              x = predicted_bmi + 0.5,
              y = 0.5,
              label = paste0("Predicted BMI: ", round(predicted_bmi, 1)),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 0
            ) +
            annotate(
              "text",
              x = bmi_value - 0.5,
              y = 0.5,
              label = paste0("Current BMI: ", round(bmi_value, 1)),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 1
            )
        }
        
        
        
      })
      
      
      output$waist_plot <- renderPlot({
        waist <- input$waist
        if (is.na(waist) || waist == "") {
          waist <- 80
        }
        gender <- as.numeric(input$gender)
        
        wc_df <- if (gender == 1) {
          tibble(
            category = factor(c("Healthy", "Elevated", "High risk"),
                              levels = c("Healthy", "Elevated", "High risk")),
            xmin = c(0, 94, 102),
            xmax = c(94, 102, 150)
          )
        } else {
          tibble(
            category = factor(c("Healthy", "Elevated", "High risk"),
                              levels = c("Healthy", "Elevated", "High risk")),
            xmin = c(0, 80, 88),
            xmax = c(80, 88, 150)
          )
        }
        
        waistplot <- make_classification_plot(
          value = waist,
          categories_df = wc_df,
          xlab = "Waist circumference (cm)",
          title = "Classification waist circumference",
          caption = "Categories based on Voedingscentrum",
          colors = c(
            "Healthy" = "#a1d99b",
            "Elevated" = "#fdae61",
            "High risk" = "#d73027"
          )
        )  
        if (waist < 55) { 
          # Label on the RIGHT 
          waistplot + annotate( "text", x = waist + 2, y = 0.5, 
                                label = paste0("Your waist circumference: ", 
                                               round(waist, 1), " cm"), 
                                size = 5, fontface = "bold", 
                                hjust = 0 ) } 
        else { 
          # Label on the LEFT 
          waistplot + annotate( "text", 
                                x = waist - 2, 
                                y = 0.5, 
                                label = paste0("Your waist circumference: ", 
                                               round(waist, 1), " cm"), 
                                size = 5, fontface = "bold", hjust = 1 ) }
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
      
      
      if (cholesterol < 5.2) {
        # Healthy zone
        output$warning_box <- renderUI({
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
               padding:15px; margin-top:15px;",
            strong("✔️ Healthy result: "),
            "Your predicted cholesterol level is in the optimal range."
          )
        })
        
      } else if (cholesterol >= 6.2) {
        # High-risk zone
        output$warning_box <- renderUI({
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
               padding:15px; margin-top:15px;",
            strong("⚠️ High-risk cholesterol predicted: "),
            "Your predicted cholesterol level is high and can lead to health 
            risks. Please consider consulting a healthcare specialist."
          )
        })
        
      } 
      # Cholesterol between 5.2 & 6.2
      else {
        # Elevated-risk zone
        output$warning_box <- renderUI({
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
               padding:15px; margin-top:15px;",
            strong("⚠️ Elevated cholesterol predicted: "),
            "Your predicted cholesterol level is elevated. Consider consulting 
            a healthcare specialist"
          )
        })
        
      } 
      
      
      
      output$message <- renderText("Predicted cholesterol (5–10 years):")
      output$risk <- renderText(paste0(round(cholesterol, 2), " mmol/l"))
      
      
      output$cholesterol_plot <- renderPlot({
        cholesterol <- cholesterol
        
        chol_df <- tibble(
          category = factor(c("Optimal", "Borderline High", "High"),
                            levels = c("Optimal", "Borderline High", "High")),
          xmin = c(0, 5.2, 6.2),
          xmax = c(5.2, 6.2, 10)
        )
        
        make_classification_plot(
          value = cholesterol,
          categories_df = chol_df,
          xlab = "Cholesterol (mmol/L)",
          title = "Your Cholesterol Classification",
          caption = "Categories based on InformedHealth.org",
          label_prefix = "Predicted cholesterol:",
          colors = c(
            "Optimal" = "#a1d99b",
            "Borderline High" = "#fdae61",
            "High" = "#d73027"
          )
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
      output$risk <- renderText(paste0(round(glucose, 2), " mmol/L"))
      
      
      output$glucose_plot <- renderPlot({
        glucose <- glucose
        
        glucose_df <- tibble(
          category = factor(c("Low", "Normal", "Prediabetes", "Diabetes"),
                            levels = c("Low", "Normal", "Prediabetes", "Diabetes")),
          xmin = c(0, 3.78, 6.97, 7.61),
          xmax = c(3.78, 6.97, 7.61, 21)
        )
        
        make_classification_plot(
          value = glucose,
          categories_df = glucose_df,
          xlab = "Glucose (mmol/L)",
          title = "Your Glucose Classification",
          caption = "Categories based on World Health Organization (WHO)",
          label_prefix = "Predicted glucose:",
          colors = c(
            "Low" = "#74add1",
            "Normal" = "#a1d99b",
            "Prediabetes" = "#fdae61",
            "Diabetes" = "#d73027"
          )
        )
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
