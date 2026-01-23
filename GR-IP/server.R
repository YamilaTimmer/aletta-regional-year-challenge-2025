library(shiny)
library(tidyverse)
library(pROC)

diabetes_model <- readRDS("../models/diabetes_model.rds")
hypertension_model <- readRDS("../models/hypertension_model.rds")

body_wt_model <- readRDS("../models/bodyweight/bodywt_nnet_model.rds")
body_wt_preproc <- readRDS("../models/bodyweight/bodywt_preprocess.rds")
body_wt_dummies <- readRDS("../models/bodyweight/bodywt_dummyVars.rds")

cholesterol_model_1 <- readRDS("../models/cholesterol/chol1_nnet_model.rds")
cholesterol_preproc_1 <- readRDS("../models/cholesterol/chol1_preprocess.rds")
cholesterol_dummies_1 <- readRDS("../models/cholesterol/chol1_dummyVars.rds")

cholesterol_model_2 <- readRDS("../models/cholesterol/chol2_nnet_model.rds")
cholesterol_preproc_2 <- readRDS("../models/cholesterol/chol2_preprocess.rds")
cholesterol_dummies_2 <- readRDS("../models/cholesterol/chol2_dummyVars.rds")

glucose_model_1 <- readRDS("../models/glucose/glucose1_nnet_model.rds")
glucose_preproc_1 <- readRDS("../models/glucose/glucose1_preprocess.rds")
glucose_dummies_1 <- readRDS("../models/glucose/glucose1_dummyVars.rds")

glucose_model_2 <- readRDS("../models/glucose/glucose2_nnet_model.rds")
glucose_preproc_2 <- readRDS("../models/glucose/glucose2_preprocess.rds")
glucose_dummies_2 <- readRDS("../models/glucose/glucose2_dummyVars.rds")

make_risk_plot <- function(risk_value,
                           xlab = "Predicted risk",
                           title = "Risk Classification",
                           caption = NULL,
                           low_label,
                           elevated_label,
                           high_label) {
  risk_df <- tibble(
    category = factor(
      c(low_label, elevated_label, high_label),
      levels = c(low_label, elevated_label, high_label)
    ),
    xmin = c(0, 0.20, 0.40),
    xmax = c(0.20, 0.40, 1.00)
  )
  
  ggplot() +
    geom_rect(
      data = risk_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = 1,
        fill = category
      ),
      alpha = 0.6
    ) +
    geom_vline(xintercept = risk_value,
               color = "black",
               size = 2) +
    scale_fill_manual(values = setNames(
      c("#a1d99b", "#fdae61", "#d73027"),
      c(low_label, elevated_label, high_label)
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



make_classification_plot <- function(value,
                                     categories_df,
                                     xlab,
                                     title,
                                     caption = NULL,
                                     label_prefix = "Your value:",
                                     colors) {
  ggplot() +
    geom_rect(
      data = categories_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = 1,
        fill = category
      ),
      alpha = 0.6
    ) +
    geom_vline(xintercept = value,
               color = "black",
               size = 2) +
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
  dict <- list(
    # ============================
    # ENGLISH
    # ============================
    en = list(
      # --- Generic phrases ---
      your_predicted_diabetes_risk_is        = "Your predicted diabetes risk for the next 5-10 years is",
      your_predicted_hypertension_risk_is    = "Your predicted hypertension risk for the next 5-10 years is",
      your_current_predicted_cholesterol_is  = "Your current predicted cholesterol level is",
      your_predicted_cholesterol_is          = "Your predicted cholesterol level for the next 5-10 years is",
      your_current_predicted_glucose_is      = "Your current predicted glucose level is",
      your_predicted_glucose_is              = "Your predicted glucose level for the next 5-10 years is",
      your_predicted_weight_gain_is          = "Your predicted weight gain for the next 5-10 years is",
      current_bmi                            = "Your current BMI is",
      predicted_bmi                          = "Your predicted BMI for the next 5-10 years is",
      
      low_risk = "Low risk",
      elevated_risk = "Elevated risk",
      high_risk = "High risk",
      
      black_line = "(black line)",
      blue_line = "(blue, dashed line)",
      
      considered_low       = "This is considered low.",
      considered_elevated  = "This is considered elevated.",
      considered_high      = "This is considered high.",
      consult_specialist   = "Consider consulting a healthcare specialist.",
      
      # --- Diabetes warnings ---
      warn_low_diabetes  = "Low diabetes risk",
      warn_elev_diabetes = "Elevated diabetes risk",
      warn_high_diabetes = "High diabetes risk",
      
      # --- Hypertension warnings ---
      warn_low_hyper  = "Low hypertension risk",
      warn_elev_hyper = "Elevated hypertension risk",
      warn_high_hyper = "High hypertension risk",
      
      # --- Weight gain warnings ---
      warn_bmi_healthy   = "Healthy BMI",
      warn_bmi_under     = "BMI too low",
      warn_bmi_over      = "Elevated BMI",
      warn_bmi_obese     = "High-risk BMI",
      
      # --- Cholesterol warnings ---
      warn_chol_optimal   = "Healthy result",
      warn_chol_elevated  = "Elevated cholesterol predicted",
      warn_chol_high      = "High-risk cholesterol predicted",
      
      chol_optimal    = "Optimal",
      chol_borderline = "Borderline High",
      chol_high       = "High",
      
      
      # --- Glucose categories ---
      glucose_low        = "Low",
      glucose_normal     = "Normal",
      glucose_prediabetes = "Prediabetes",
      glucose_diabetes    = "Diabetes",
      
      # --- Glucose warn ---
      glucose_warn_low        = "Low glucose",
      glucose_warn_normal     = "Healthy result",
      glucose_warn_prediabetes = "Elevated glucose",
      glucose_warn_diabetes    = "High glucose",
      
      # --- BMI categories ---
      bmi_underweight = "Underweight",
      bmi_healthy     = "Healthy",
      bmi_overweight  = "Overweight",
      bmi_obese       = "Obese",
      
      # --- Waist categories ---
      waist_healthy  = "Healthy",
      waist_elevated = "Elevated",
      waist_high     = "High risk",
      
      
      # --- Plot titles ---
      title_diabetes_plot     = "Your Diabetes Risk Classification",
      title_hypertension_plot = "Your Hypertension Risk Classification",
      title_bmi_plot          = "Your BMI Classification",
      title_waist_plot        = "Classification waist circumference",
      title_chol_plot         = "Your Cholesterol Classification",
      title_glucose_plot      = "Your Glucose Classification",
      x_axis_waist_plot        = "Waist circumference in cm",
      
      
      # --- Captions ---
      caption_bmi        = "Categories based on Voedingscentrum",
      caption_cholesterol = "Categories based on InformedHealth.org",
      caption_glucose     = "Categories based on WHO",
      
      risk_thresholds = "Risk thresholds: <20% low risk, 20–40% elevated risk, >40% high risk",
      
      #---- Notes ----
      notes_generalize = "• Predictions are based on Lifelines cohort data and may not generalize to all populations.",
      notes_not_medical = "• This tool is not a medical device and does not replace professional medical advice.",
      notes_not_stored = "• User data is not stored."
      
    ),
    
    
    # ============================
    # DUTCH
    # ============================
    nl = list(
      # --- Generic phrases ---
      your_predicted_diabetes_risk_is        = "Uw voorspelde diabetesrisico voor de komende 5-10 jaar is",
      your_predicted_hypertension_risk_is    = "Uw voorspelde hypertensierisico voor de komende 5-10 jaar is",
      your_current_predicted_cholesterol_is  = "Uw huidige voorspelde cholesterolwaarde is",
      your_predicted_cholesterol_is          = "Uw voorspelde cholesterolwaarde voor de komende 5-10 jaar is",
      your_current_predicted_glucose_is      = "Uw huidige voorspelde glucosewaarde is",
      your_predicted_glucose_is              = "Uw voorspelde glucosewaarde voor de komende 5-10 jaar is",
      your_predicted_weight_gain_is          = "Uw voorspelde gewichtstoename voor de komende 5-10 jaar is",
      current_bmi                            = "Uw huidige BMI is",
      predicted_bmi                          = "Uw voorspelde BMI voor de komende 5-10 jaar is",
      
      low_risk = "Laag risico",
      elevated_risk = "Verhoogd risico",
      high_risk = "Hoog risico",
      
      black_line = "(zwarte lijn)",
      blue_line = "(blauwe stippellijn)",
      
      considered_low       = "Dit wordt beschouwd als laag.",
      considered_elevated  = "Dit wordt beschouwd als verhoogd.",
      considered_high      = "Dit wordt beschouwd als hoog.",
      consult_specialist   = "Overweeg een zorgprofessional te raadplegen.",
      
      # --- Diabetes warnings ---
      warn_low_diabetes  = "Laag diabetesrisico",
      warn_elev_diabetes = "Verhoogd diabetesrisico",
      warn_high_diabetes = "Hoog diabetesrisico",
      
      # --- Hypertension warnings ---
      warn_low_hyper  = "Laag hypertensierisico",
      warn_elev_hyper = "Verhoogd hypertensierisico",
      warn_high_hyper = "Hoog hypertensierisico",
      
      # --- Weight gain warnings ---
      warn_bmi_healthy   = "Gezonde BMI",
      warn_bmi_under     = "BMI te laag",
      warn_bmi_over      = "Verhoogde BMI",
      warn_bmi_obese     = "Hoog-risico BMI",
      
      # --- Cholesterol warnings ---
      warn_chol_optimal   = "Gezond resultaat",
      warn_chol_elevated  = "Verhoogd cholesterol voorspeld",
      warn_chol_high      = "Hoog cholesterol voorspeld",
      
      chol_optimal    = "Optimaal",
      chol_borderline = "Verhoogd",
      chol_high       = "Hoog",
      
      # --- Glucose categories ---
      glucose_low        = "Laag",
      glucose_normal     = "Normaal",
      glucose_prediabetes = "Prediabetes",
      glucose_diabetes    = "Diabetes",
      
      # --- Glucose warn ---
      glucose_warn_low        = "Laag glucosegehalte",
      glucose_warn_normal     = "Gezond resultaat",
      glucose_warn_prediabetes = "Verhoogd glucosegehalte",
      glucose_warn_diabetes    = "Hoog glucosegehalte",
      
      # --- BMI categories ---
      bmi_underweight = "Ondergewicht",
      bmi_healthy     = "Gezond",
      bmi_overweight  = "Overgewicht",
      bmi_obese       = "Obesitas",
      
      # --- Waist categories ---
      waist_healthy  = "Gezond",
      waist_elevated = "Verhoogd",
      waist_high     = "Hoog risico",
      
      
      # --- Plot titles ---
      title_diabetes_plot     = "Uw diabetesrisicoclassificatie",
      title_hypertension_plot = "Uw hypertensierisicoclassificatie",
      title_bmi_plot          = "Uw BMI‑classificatie",
      title_waist_plot        = "Classificatie middelomtrek",
      x_axis_waist_plot        = "Omtrek taille in cm",
      title_chol_plot         = "Uw cholesterolclassificatie",
      title_glucose_plot      = "Uw glucoseclassificatie",
      
      # --- Captions ---
      caption_bmi        = "Categorieën gebaseerd op Voedingscentrum",
      caption_cholesterol = "Categorieën gebaseerd op InformedHealth.org",
      caption_glucose     = "Categorieën gebaseerd op WHO",
      
      risk_thresholds = "Risico grenswaarden: <20% laag risico, 20–40% verhoogd risico, >40% hoog risico",
      
      # --- Notes ----
      notes_generalize = "• Voorspellingen zijn gebaseerd op Lifelines-cohortdata en generaliseren mogelijk niet naar alle populaties.",
      notes_not_medical = "• Deze tool is geen medisch hulpmiddel en vervangt geen professioneel medisch advies.",
      notes_not_stored = "• Gebruikersgegevens worden niet opgeslagen."
      
    )
  )
  
  
  tr <- function(key) {
    dict[[input$language]][[key]]
  }
  
  # ============================================================
  # TAB 1 — RISK PREDICTION
  # ============================================================
  observeEvent(input$predict, {
    lat <- zipcode_data$latitude[zipcode_data$postcode == input$zipcode]
    lon <- zipcode_data$longitude[zipcode_data$postcode == input$zipcode]
    
    length_m = input$body_height / 100
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
      
      diabetes_risk <- predict(diabetes_model, newdata = new_person, type = "response")
      
      # ============================
      # DIABETES WARNING SYSTEM
      # ============================
      
      output$warning_box <- renderUI({
        if (diabetes_risk < 0.20) {
          # LOW RISK
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "✅ ", tr("warn_low_diabetes"), ": "
            )),
            
            tr("your_predicted_diabetes_risk_is"),
            " ",
            strong(paste0(
              round(diabetes_risk * 100, 1), "%"
            )),
            ". ",
            tr("considered_low")
          )
          
        } else if (diabetes_risk >= 0.40) {
          # HIGH RISK
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_high_diabetes"), ": "
            )),
            
            tr("your_predicted_diabetes_risk_is"),
            " ",
            strong(paste0(
              round(diabetes_risk * 100, 1), "%"
            )),
            ". ",
            
            tr("considered_high"),
            " ",
            tr("consult_specialist")
          )
          
          
        } else {
          # ELEVATED RISK
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_elev_diabetes"), ": "
            )),
            
            tr("your_predicted_diabetes_risk_is"),
            " ",
            strong(paste0(
              round(diabetes_risk * 100, 1), "%"
            )),
            ". ",
            
            tr("considered_elevated")
          )
          
        }
      })
      
      
      # ============================
      # DIABETES PLOT
      # ============================
      
      output$diabetes_plot <- renderPlot({
        base <- make_risk_plot(
          risk_value = diabetes_risk,
          xlab = tr("msg_diabetes"),
          title = tr("title_diabetes_plot"),
          caption = tr("risk_thresholds"),
          low_label = tr("low_risk"),
          elevated_label = tr("elevated_risk"),
          high_label = tr("high_risk")
        )
        
        # Dynamic label placement
        if (diabetes_risk < 0.20) {
          base +
            annotate(
              "text",
              x = diabetes_risk + 0.03,
              y = 0.5,
              label = paste0(round(diabetes_risk * 100, 1), "%"),
              size = 5,
              fontface = "bold",
              hjust = 0
            )
        } else {
          base +
            annotate(
              "text",
              x = diabetes_risk - 0.03,
              y = 0.5,
              label = paste0(round(diabetes_risk * 100, 1), "%"),
              size = 5,
              fontface = "bold",
              hjust = 1
            )
        }
      })
    }
    
    # ============================================================
    # Prediction 2 — HYPERTENSION PREDICTION
    # ============================================================
    else if (input$prediction == "Hypertension" ||
             input$prediction == "Hoge bloeddruk") {
      new_person <- data.frame(
        age = input$age,
        gender = as.numeric(input$gender),
        circumference_waist_1 = input$waist,
        latitude = lat,
        alcohol_intake = input$alcohol_intake * 10,
        nova_foodintake_1 = input$nova1,
        bmi_t1 = bmi_t1
      )
      
      hypertension_risk <- predict(hypertension_model,
                                   newdata = new_person,
                                   type = "response")
      
      
      # ============================
      # HYPERTENSION WARNING SYSTEM
      # ============================
      
      output$warning_box <- renderUI({
        if (hypertension_risk < 0.20) {
          # LOW RISK
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
           padding:15px; margin-top:15px;",
            
            strong(paste0("✅ ", tr(
              "warn_low_hyper"
            ), ": ")),
            
            tr("your_predicted_hypertension_risk_is"),
            " ",
            strong(paste0(
              round(hypertension_risk * 100, 1), "%"
            )),
            " . ",
            
            tr("considered_low")
          )
          
          
        } else if (hypertension_risk >= 0.40) {
          # HIGH RISK
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_high_hyper"), ": "
            )),
            
            tr("your_predicted_hypertension_risk_is"),
            " ",
            strong(paste0(
              round(hypertension_risk * 100, 1), "%"
            )),
            " . ",
            
            tr("considered_high"),
            " ",
            tr("consult_specialist")
          )
          
          
        } else {
          # ELEVATED RISK
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_elev_hyper"), ": "
            )),
            
            tr("your_predicted_hypertension_risk_is"),
            " ",
            strong(paste0(
              round(hypertension_risk * 100, 1), "%"
            )),
            " . ",
            
            tr("considered_elevated"),
            " ",
            tr("consult_specialist")
          )
          
        }
      })
      
      
      # ============================
      # HYPERTENSION PLOT
      # ============================
      
      output$hypertension_plot <- renderPlot({
        base <- make_risk_plot(
          risk_value = hypertension_risk,
          xlab = tr("msg_hypertension"),
          title = tr("title_hypertension_plot"),
          caption = tr("risk_thresholds"),
          low_label = tr("low_risk"),
          elevated_label = tr("elevated_risk"),
          high_label = tr("high_risk")
        )
        
        # Dynamic label placement
        if (hypertension_risk < 0.20) {
          base +
            annotate(
              "text",
              x = hypertension_risk + 0.03,
              y = 0.5,
              label = paste0(round(hypertension_risk * 100, 1), "%"),
              size = 5,
              fontface = "bold",
              hjust = 0
            )
        } else {
          base +
            annotate(
              "text",
              x = hypertension_risk - 0.03,
              y = 0.5,
              label = paste0(round(hypertension_risk * 100, 1), "%"),
              size = 5,
              fontface = "bold",
              hjust = 1
            )
        }
      })
    }
    
    
    
    # ============================================================
    # Prediction 3 — WEIGHT GAIN
    # ============================================================
    else if (input$prediction == "Weight Gain" ||
             input$prediction == "Gewichtstoename") {
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
      
      
      # ============================
      # BMI WARNING SYSTEM
      # ============================
      
      output$warning_box <- renderUI({
        if ((bmi_t1 >= 18.5 &&
             bmi_t1 < 25) && (predicted_bmi >= 18.5 && predicted_bmi < 25)) {
          # HEALTHY BMI
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "✅ ", tr("warn_bmi_healthy"), ": "
            )),
            
            tr("current_bmi"),
            " ",
            strong(round(bmi_t1, 1)),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("predicted_bmi"),
            " ",
            strong(round(predicted_bmi, 1)),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("your_predicted_weight_gain_is"),
            " ",
            strong(paste0(
              round(predicted_weight - input$body_weight, 1),
              " kg."
            ))
          )
          
          
        } else if (bmi_t1 >= 30 || predicted_bmi >= 30) {
          # OBESE
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_bmi_obese"), ": "
            )),
            
            tr("current_bmi"),
            " ",
            strong(round(bmi_t1, 1)),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("predicted_bmi"),
            " ",
            strong(round(predicted_bmi, 1)),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("your_predicted_weight_gain_is"),
            " ",
            strong(paste0(
              round(predicted_weight - input$body_weight, 1),
              " kg."
            )),
            " ",
            
            tr("consult_specialist")
          )
          
          
        } else if (bmi_t1 < 18.5 || predicted_bmi < 18.5) {
          # UNDERWEIGHT
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_bmi_under"), ": "
            )),
            
            tr("current_bmi"),
            " ",
            strong(round(bmi_t1, 1)),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("predicted_bmi"),
            " ",
            strong(round(predicted_bmi, 1)),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("your_predicted_weight_gain_is"),
            " ",
            strong(paste0(
              round(predicted_weight - input$body_weight, 1),
              " kg."
            )),
            " ",
            
            tr("consult_specialist")
          )
          
          
        } else {
          # OVERWEIGHT
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
           padding:15px; margin-top:15px;",
            
            strong(paste0("⚠️ ", tr(
              "warn_bmi_over"
            ), ": ")),
            
            tr("current_bmi"),
            " ",
            strong(round(bmi_t1, 1)),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("predicted_bmi"),
            " ",
            strong(round(predicted_bmi, 1)),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("your_predicted_weight_gain_is"),
            " ",
            strong(paste0(
              round(predicted_weight - input$body_weight, 1),
              " kg."
            ))
          )
          
        }
      })
      
      
      # ============================
      # BMI PLOT
      # ============================
      
      output$bmi_plot <- renderPlot({
        bmi_value <- bmi_t1
        predicted_bmi <- predicted_weight / (length_m^2)
        
        bmi_df <- tibble(
          category = factor(c(
            tr("bmi_underweight"),
            tr("bmi_healthy"),
            tr("bmi_overweight"),
            tr("bmi_obese")
          ), levels = c(
            tr("bmi_underweight"),
            tr("bmi_healthy"),
            tr("bmi_overweight"),
            tr("bmi_obese")
          )),
          xmin = c(0, 18.5, 25, 30),
          xmax = c(18.5, 25, 30, 50)
        )
        
        colors <- c("#74add1", "#a1d99b", "#fdae61", "#d73027")
        
        names(colors) <- c(
          tr("bmi_underweight"),
          tr("bmi_healthy"),
          tr("bmi_overweight"),
          tr("bmi_obese")
        )
        
        base_plot <- make_classification_plot(
          value = bmi_value,
          categories_df = bmi_df,
          xlab = "BMI",
          title = tr("title_bmi_plot"),
          caption = tr("caption_bmi"),
          colors = colors
        )
        
        # Dynamic label placement
        if (predicted_bmi < bmi_value) {
          base_plot +
            geom_vline(
              xintercept = predicted_bmi,
              color = "blue",
              size = 2,
              linetype = "dashed"
            ) +
            annotate(
              "text",
              x = predicted_bmi - 0.5,
              y = 0.5,
              label = round(predicted_bmi, 1),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 1
            ) +
            annotate(
              "text",
              x = bmi_value + 0.5,
              y = 0.5,
              label = round(bmi_value, 1),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 0
            )
        } else {
          base_plot +
            geom_vline(
              xintercept = predicted_bmi,
              color = "blue",
              size = 2,
              linetype = "dashed"
            ) +
            annotate(
              "text",
              x = predicted_bmi + 0.5,
              y = 0.5,
              label = round(predicted_bmi, 1),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 0
            ) +
            annotate(
              "text",
              x = bmi_value - 0.5,
              y = 0.5,
              label = round(bmi_value, 1),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 1
            )
        }
      })
      
      
      # ============================
      # WAIST PLOT
      # ============================
      
      output$waist_plot <- renderPlot({
        waist <- input$waist
        if (is.na(waist) || waist == "")
          waist <- 80
        gender <- as.numeric(input$gender)
        
        wc_df <- if (gender == 1) {
          tibble(
            category = factor(c(
              tr("waist_healthy"),
              tr("waist_elevated"),
              tr("waist_high")
            ), levels = c(
              tr("waist_healthy"),
              tr("waist_elevated"),
              tr("waist_high")
            )),
            xmin = c(0, 94, 102),
            xmax = c(94, 102, 150)
          )
        } else {
          tibble(
            category = factor(c(
              tr("waist_healthy"),
              tr("waist_elevated"),
              tr("waist_high")
            ), levels = c(
              tr("waist_healthy"),
              tr("waist_elevated"),
              tr("waist_high")
            )),
            xmin = c(0, 80, 88),
            xmax = c(80, 88, 150)
          )
        }
        
        colors <- c("#a1d99b", "#fdae61", "#d73027")
        
        names(colors) <- c(tr("waist_healthy"),
                           tr("waist_elevated"),
                           tr("waist_high"))
        
        
        waistplot <- make_classification_plot(
          value = waist,
          categories_df = wc_df,
          title = tr("title_waist_plot"),
          xlab = tr("x_axis_waist_plot"),
          caption = tr("caption_bmi"),
          colors = colors
        )
        
        waistplot +
          annotate(
            "text",
            x = waist + 2,
            y = 0.5,
            label = round(waist, 1),
            size = 5,
            fontface = "bold",
            hjust = 0
          )
        
      })
    }
    
    
    # ============================================================
    # Prediction 4 — CHOLESTEROL
    # ============================================================
    else if (input$prediction == "Cholesterol") {
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
      
      current_chol_processed <- predict(cholesterol_preproc_1, new_person)
      current_chol_dummied <- predict(cholesterol_dummies_1, current_chol_processed)
      current_cholesterol <- predict(cholesterol_model_1, current_chol_dummied)
      
      
      future_chol_processed <- predict(cholesterol_preproc_2, new_person)
      future_chol_dummied <- predict(cholesterol_dummies_2, future_chol_processed)
      future_cholesterol <- predict(cholesterol_model_2, future_chol_dummied)
      
      # ============================
      # CHOLESTEROL WARNING SYSTEM
      # ============================
      
      output$warning_box <- renderUI({
        if (current_cholesterol < 5.2 && future_cholesterol < 5.2) {
          # OPTIMAL CHOLESTEROL
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "✅ ", tr("warn_chol_optimal"), ": "
            )),
            
            tr("your_current_predicted_cholesterol_is"),
            " ",
            strong(paste0(
              round(current_cholesterol, 2), " mmol/L"
            )),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("your_predicted_cholesterol_is"),
            " ",
            strong(paste0(
              round(future_cholesterol, 2), " mmol/L"
            )),
            " ",
            paste0(tr("blue_line"), ".")
          )
          
          
        } else if (current_cholesterol >= 6.2 ||
                   future_cholesterol >= 6.2) {
          # HIGH CHOLESTEROL RISK
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_chol_high"), ": "
            )),
            
            tr("your_current_predicted_cholesterol_is"),
            " ",
            strong(paste0(
              round(current_cholesterol, 2), " mmol/L"
            )),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("your_predicted_cholesterol_is"),
            " ",
            strong(paste0(
              round(future_cholesterol, 2), " mmol/L"
            )),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("consult_specialist")
          )
          
          
        } else {
          # ELEVATED CHOLESTEROL RISK
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("warn_chol_elevated"), ": "
            )),
            
            tr("your_current_predicted_cholesterol_is"),
            " ",
            strong(paste0(
              round(current_cholesterol, 2), " mmol/L"
            )),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("your_predicted_cholesterol_is"),
            " ",
            strong(paste0(
              round(future_cholesterol, 2), " mmol/L"
            )),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("consult_specialist")
          )
          
        }
      })
      
      
      # ============================
      # CHOLESTEROL PLOT
      # ============================
      
      output$cholesterol_plot <- renderPlot({
        chol_df <- tibble(
          category = factor(c(
            tr("chol_optimal"),
            tr("chol_borderline"),
            tr("chol_high")
          ), levels = c(
            tr("chol_optimal"),
            tr("chol_borderline"),
            tr("chol_high")
          )),
          xmin = c(0, 5.2, 6.2),
          xmax = c(5.2, 6.2, 10)
        )
        
        colors <- c("#a1d99b", "#fdae61", "#d73027")
        
        names(colors) <- c(tr("chol_optimal"),
                           tr("chol_borderline"),
                           tr("chol_high"))
        
        chol_plot <- make_classification_plot(
          value = current_cholesterol,
          categories_df = chol_df,
          xlab = tr("msg_cholesterol"),
          title = tr("title_chol_plot"),
          caption = tr("caption_cholesterol"),
          label_prefix = tr("msg_cholesterol"),
          colors = colors
        )
        
        
        if (future_cholesterol < current_cholesterol) {
          chol_plot +
            geom_vline(
              xintercept = future_cholesterol,
              color = "blue",
              size = 2,
              linetype = "dashed"
            ) +
            annotate(
              "text",
              x = future_cholesterol - 0.5,
              y = 0.5,
              label = round(future_cholesterol, 2),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 1
            ) +
            annotate(
              "text",
              x = current_cholesterol + 0.5,
              y = 0.5,
              label = round(current_cholesterol, 2),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 0
            )
        } else {
          chol_plot +
            geom_vline(
              xintercept = future_cholesterol,
              color = "blue",
              size = 2,
              linetype = "dashed"
            ) +
            annotate(
              "text",
              x = future_cholesterol + 0.5,
              y = 0.5,
              label = round(future_cholesterol, 2),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 0
            ) +
            annotate(
              "text",
              x = current_cholesterol - 0.5,
              y = 0.5,
              label = round(current_cholesterol, 2),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 1
            )
        }
      })
    }
    
    
    # ============================================================
    # Prediction 5 — GLUCOSE
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
      
      current_gluc_processed <- predict(glucose_preproc_1, new_person)
      current_gluc_dummied <- predict(cholesterol_dummies_1, current_gluc_processed)
      current_gluc <- predict(glucose_model_1, current_gluc_dummied)
      
      future_gluc_processed <- predict(glucose_preproc_2, new_person)
      future_gluc_dummied <- predict(cholesterol_dummies_2, future_gluc_processed)
      future_gluc <- predict(glucose_model_2, future_gluc_dummied)
      
      
      output$warning_box <- renderUI({
        if (current_gluc < 3.78 || future_gluc < 3.78) {
          # LOW GLUCOSE
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("glucose_warn_low"), ": "
            )),
            
            tr("your_current_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(current_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("your_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(future_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("consult_specialist")
          )
          
          
        } else if (current_gluc < 6.97 && future_gluc < 6.97) {
          # NORMAL GLUCOSE
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "✅ ", tr("glucose_warn_normal"), ": "
            )),
            
            tr("your_current_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(current_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("your_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(future_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("blue_line"), ".")
          )
          
          
        } else if (current_gluc < 7.61 || future_gluc < 7.61) {
          # PREDIABETES GLUCOSE
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("glucose_warn_prediabetes"), ": "
            )),
            
            tr("your_current_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(current_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("your_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(future_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("consult_specialist")
          )
          
          
        } else {
          # DIABETES GLUCOSE
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
           padding:15px; margin-top:15px;",
            
            strong(paste0(
              "⚠️ ", tr("glucose_warn_diabetes"), ": "
            )),
            
            tr("your_current_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(current_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("black_line"), ". "),
            
            tr("your_predicted_glucose_is"),
            " ",
            strong(paste0(
              round(future_gluc, 2), " mmol/L"
            )),
            " ",
            paste0(tr("blue_line"), ". "),
            
            tr("consult_specialist")
          )
          
        }
      })
      
      
      output$glucose_plot <- renderPlot({
        glucose_df <- tibble(
          category = factor(c(
            tr("glucose_low"),
            tr("glucose_normal"),
            tr("glucose_prediabetes"),
            tr("glucose_diabetes")
          ), levels = c(
            tr("glucose_low"),
            tr("glucose_normal"),
            tr("glucose_prediabetes"),
            tr("glucose_diabetes")
          )),
          xmin = c(0, 3.78, 6.97, 7.61),
          xmax = c(3.78, 6.97, 7.61, 21)
        )
        
        
        colors <- c("#74add1", "#a1d99b", "#fdae61", "#d73027")
        
        names(colors) <- c(
          tr("glucose_low"),
          tr("glucose_normal"),
          tr("glucose_prediabetes"),
          tr("glucose_diabetes")
        )
        
        gluc_plot <- make_classification_plot(
          value = current_gluc,
          categories_df = glucose_df,
          xlab = "Glucose (mmol/L)",
          title = "Your Glucose Classification",
          caption = tr("caption_glucose"),
          label_prefix = "Predicted glucose:",
          colors = colors
        )
        
        if (future_gluc < current_gluc) {
          gluc_plot +
            geom_vline(
              xintercept = future_gluc,
              color = "blue",
              size = 2,
              linetype = "dashed"
            ) +
            annotate(
              "text",
              x = future_gluc - 0.5,
              y = 0.5,
              label = round(future_gluc, 2),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 1
            ) +
            annotate(
              "text",
              x = current_gluc + 0.5,
              y = 0.5,
              label = round(current_gluc, 2),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 0
            )
        } else {
          gluc_plot +
            geom_vline(
              xintercept = future_gluc,
              color = "blue",
              size = 2,
              linetype = "dashed"
            ) +
            annotate(
              "text",
              x = future_gluc + 0.5,
              y = 0.5,
              label = round(future_gluc, 2),
              size = 5,
              fontface = "bold",
              color = "blue",
              hjust = 0
            ) +
            annotate(
              "text",
              x = current_gluc - 0.5,
              y = 0.5,
              label = round(current_gluc, 2),
              size = 5,
              fontface = "bold",
              color = "black",
              hjust = 1
            )
        }
      })
    }
    
    
    
    output$message <- renderText({
      req(input$prediction)
      
      if (input$prediction == "Diabetes") {
        tr("msg_diabetes")
        
      } else if (input$prediction == "Hypertension" ||
                 input$prediction == "Hoge bloeddruk") {
        tr("msg_hypertension")
        
      } else if (input$prediction == "Weight Gain" ||
                 input$prediction == "Gewichtstoename") {
        tr("msg_weight")
      } else if (input$prediction == "Cholesterol") {
        tr("msg_cholesterol")
      } else {
        tr("msg_glucose")
        
      }
      
    })
    
    
    output$risk <- renderText({
      req(input$prediction)
      
      if (input$prediction == "Diabetes") {
        paste0(round(diabetes_risk * 100, 2), " %")
      } else if (input$prediction == "Hypertension" ||
                 input$prediction == "Hoge bloeddruk") {
        paste0(round(hypertension_risk * 100, 2), " %")
      } else if (input$prediction == "Weight Gain" ||
                 input$prediction == "Gewichtstoename") {
        paste0(round(predicted_weight - input$body_weight, 2), " kg")
      } else if (input$prediction == "Cholesterol") {
        paste0(round(current_cholesterol, 2), " mmol/L")
      } else if (input$prediction == "Glucose") {
        paste0(round(current_glucose, 2), " mmol/L")
        
      }
      
    })
    
    
  })
  
  # Sidebar panel
  
  output$sidebar_inputs <- renderUI({
    if (input$language == "en") {
      tagList(
        selectInput(
          "prediction",
          "Select prediction:",
          choices = c(
            "Diabetes",
            "Hypertension",
            "Weight Gain",
            "Cholesterol",
            "Glucose"
          )
        ),
        
        
        # Shared inputs
        numericInput(
          "age",
          "Age (years)",
          value = 40,
          min = 18,
          max = 100
        ),
        selectInput("gender", "Gender", choices = c(
          "Female" = 0, "Male" = 1
        )),
        numericInput(
          "body_height",
          "Body length (cm)",
          value = 180,
          min = 70,
          max = 250
        ),
        numericInput(
          "body_weight",
          "Body weight (kg)",
          value = 80,
          min = 30,
          max = 250
        ),
        numericInput(
          "waist",
          "Waist circumference (cm)",
          value = 80,
          min = 40,
          max = 200
        ),
        numericInput(
          "nova1",
          "Daily intake of unprocessed foods (in gram)",
          value = 600,
          min = 0,
          max = 5000
        ),
        helpText("E.g. vegetables and fruit."),
        selectInput("zipcode", "Zipcode (first 4 digits)", choices = zipcode_data$postcode),
        
        
        conditionalPanel(
          condition = "input.prediction == 'Hypertension' ||
                      input.prediction == 'Weight Gain' ||
                      input.prediction == 'Cholesterol' ||
                      input.prediction == 'Glucose' ",
          
          
          
          numericInput(
            "alcohol_intake",
            "Daily alcohol intake (per standard drink)",
            value = 0,
            min = 0,
            max = 30
          ),
          helpText(
            "1 standard drink = 1 glass of beer/wine or 1 shot of hard liquor."
          ),
          
        ),
        
        conditionalPanel(
          condition = " input.prediction == 'Weight Gain' ||
          input.prediction == 'Cholesterol' ||
          input.prediction == 'Glucose' ",
          
          numericInput(
            "hip",
            "Hip circumference (cm)",
            value = 90,
            min = 40,
            max = 200
          ),
          numericInput(
            "nova4",
            "Daily intake of highly processed foods (in gram)",
            value = 500,
            min = 0,
            max = 5000
          ),
          helpText("E.g. ready‑to‑eat meals or crisps."),
          numericInput(
            "kcal",
            "Daily caloric intake (in kcal)",
            value = 2000,
            min = 0,
            max = 10000
          ),
          numericInput(
            "sugar",
            "Daily intake of added sugar (in gram)",
            value = 20,
            min = 0,
            max = 1000
          ),
          helpText("E.g. sugar added to tea/coffee or snacks with added sugar."),
          
        ),
        
        actionButton("predict", "Predict risk")
      )
    } else {
      tagList(
        selectInput(
          "prediction",
          "Selecteer voorspelling:",
          choices = c(
            "Diabetes",
            "Hoge bloeddruk",
            "Gewichtstoename",
            "Cholesterol",
            "Glucose"
          )
          
        ),
        
        # Shared inputs
        numericInput(
          "age",
          "Leeftijd (in jaren)",
          value = 40,
          min = 18,
          max = 100
        ),
        selectInput("gender", "Geslacht", choices = c(
          "Vrouw" = 0, "Man" = 1
        )),
        numericInput(
          "body_height",
          "Lengte (in cm)",
          value = 180,
          min = 70,
          max = 250
        ),
        numericInput(
          "body_weight",
          "Lichaamsgewicht (in kg)",
          value = 80,
          min = 30,
          max = 250
        ),
        numericInput(
          "waist",
          "Omtrek taille (in cm)",
          value = 80,
          min = 40,
          max = 200
        ),
        numericInput(
          "nova1",
          "Dagelijkse inname van onbewerkt voedsel (in gram)",
          value = 600,
          min = 0,
          max = 5000
        ),
        helpText("Bijvoorbeeld groente en fruit."),
        selectInput(
          "zipcode",
          "Postcode (eerste 4 cijfers)",
          choices = zipcode_data$postcode
        ),
        
        conditionalPanel(
          condition = "input.prediction == 'Hoge bloeddruk' ||
                     input.prediction == 'Gewichtstoename' ||
                     input.prediction == 'Cholesterol' ||
                     input.prediction == 'Glucose'",
          
          numericInput(
            "alcohol_intake",
            "Dagelijkse alcoholinname (per standaardglas)",
            value = 0,
            min = 0,
            max = 30
          ),
          helpText("1 standaardglas = 1 glas bier/wijn of 1 shot sterke drank")
        ),
        
        conditionalPanel(
          condition = " input.prediction == 'Gewichtstoename' ||
          input.prediction == 'Cholesterol' ||
          input.prediction == 'Glucose' ",
          
          numericInput(
            "hip",
            "Heupomvang (in cm)",
            value = 90,
            min = 40,
            max = 200
          ),
          numericInput(
            "nova4",
            "Dagelijkse inname van sterk bewerkt voedsel (in gram)",
            value = 500,
            min = 0,
            max = 5000
          ),
          helpText("Bijvoorbeeld kant-en-klare maaltijden of chips"),
          numericInput(
            "kcal",
            "Dagelijkse inname van calorieën (in kcal)",
            value = 2000,
            min = 0,
            max = 10000
          ),
          numericInput(
            "sugar",
            "Dagelijkse toegevoegde suiker (in gram)",
            value = 50,
            min = 0,
            max = 1000
          ),
          helpText(
            "Bijvoorbeeld suiker in de thee/koffie of tussendoortjes met toegevoegde suikers"
          ),
          
        ),
        actionButton("predict", "Voorspel waarde")
        
      )
    }
  })
  
  # Notes/limitations
  output$notes <- renderUI({
    tagList(p(tr("notes_generalize")), p(tr("notes_not_medical")), p(tr("notes_not_stored")))
  })
  
  output$tab_background_title <- renderUI({
    if (input$language == "en") {
      "Background Information"
    } else {
      "Achtergrondinformatie"
    }
    
  })
  
  output$tab_usage_title <- renderUI({
    if (input$language == "en") {
      "How to use"
    } else {
      "Gebruiksaanwijzing"
    }
  })
  
  output$result <- renderUI({
    if (input$language == "en") {
      "Result"
    } else {
      "Resultaat"
    }
  })
  
  output$notes_title <- renderUI({
    if (input$language == "en") {
      "Notes"
    } else {
      "Opmerkingen"
    }
  })
  
  output$tab_risk_title <- renderUI({
    if (input$language == "en") {
      "Risk prediction"
    } else {
      "Risicovoorspelling"
    }
  })
  
  output$app_title <- renderUI({
    if (input$language == "en") {
      "Groningen Risk‑Insight for Prevention (GR-IP)"
    } else {
      "Groningen Risico‑Inzicht voor Preventie (GR-IP)"
    }
  })
  
  
}
