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
  
  dict <- list(
    
    # ============================
    # ENGLISH
    # ============================
    en = list(

      # --- Generic phrases ---
      your_predicted_diabetes_risk_is     = "Your predicted diabetes risk for the next 5-10 years is",
      your_predicted_hypertension_risk_is = "Your predicted hypertension risk for the next 5-10 years is",
      your_predicted_cholesterol_is       = "Your predicted cholesterol level for the next 5-10 years is",
      your_predicted_glucose_is           = "Your predicted glucose level for the next 5-10 years is",
      your_predicted_weight_gain_is       = "Your predicted weight gain for the next 5-10 years is",
      
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
      
      # --- BMI categories ---
      bmi_underweight = "Underweight",
      bmi_healthy     = "Healthy",
      bmi_overweight  = "Overweight",
      bmi_obese       = "Obese",
      
      # --- Waist categories ---
      waist_healthy  = "Healthy",
      waist_elevated = "Elevated",
      waist_high     = "High risk",
      
      # --- Units ---
      unit_percent = "%",
      unit_kg      = "kg",
      unit_mmol_l  = "mmol/L",
      
      # --- Plot titles ---
      title_diabetes_plot     = "Your Diabetes Risk Classification",
      title_hypertension_plot = "Your Hypertension Risk Classification",
      title_bmi_plot          = "Your BMI Classification",
      title_waist_plot        = "Classification waist circumference",
      title_chol_plot         = "Your Cholesterol Classification",
      title_glucose_plot      = "Your Glucose Classification",
      
      # --- Captions ---
      caption_bmi        = "Categories based on Voedingscentrum",
      caption_waist      = "Categories based on Voedingscentrum",
      caption_cholesterol = "Categories based on InformedHealth.org",
      caption_glucose     = "Categories based on WHO",
      
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
      your_predicted_diabetes_risk_is     = "Uw voorspelde diabetesrisico voor de komende 5-10 jaar is",
      your_predicted_hypertension_risk_is = "Uw voorspelde hypertensierisico voor de komende 5-10 jaar is",
      your_predicted_cholesterol_is       = "Uw voorspelde cholesterolwaarde voor de komende 5-10 jaar is",
      your_predicted_glucose_is           = "Uw voorspelde glucosewaarde voor de komende 5-10 jaar is",
      your_predicted_weight_gain_is       = "Uw voorspelde gewichtstoename voor de komende 5-10 jaar is",
      
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
      
      # --- BMI categories ---
      bmi_underweight = "Ondergewicht",
      bmi_healthy     = "Gezond",
      bmi_overweight  = "Overgewicht",
      bmi_obese       = "Obesitas",
      
      # --- Waist categories ---
      waist_healthy  = "Gezond",
      waist_elevated = "Verhoogd",
      waist_high     = "Hoog risico",
      
      # --- Units ---
      unit_percent = "%",
      unit_kg      = "kg",
      unit_mmol_l  = "mmol/L",
      
      # --- Plot titles ---
      title_diabetes_plot     = "Uw diabetesrisicoclassificatie",
      title_hypertension_plot = "Uw hypertensierisicoclassificatie",
      title_bmi_plot          = "Uw BMI‑classificatie",
      title_waist_plot        = "Classificatie middelomtrek",
      title_chol_plot         = "Uw cholesterolclassificatie",
      title_glucose_plot      = "Uw glucoseclassificatie",
      
      # --- Captions ---
      caption_bmi        = "Categorieën gebaseerd op Voedingscentrum",
      caption_waist      = "Categorieën gebaseerd op Voedingscentrum",
      caption_cholesterol = "Categorieën gebaseerd op InformedHealth.org",
      caption_glucose     = "Categorieën gebaseerd op WHO",
      
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
            strong(paste0("✅ ", tr("warn_low_diabetes"), ": ")),
            paste0(
              tr("your_predicted_diabetes_risk_is"), " ",
              round(diabetes_risk * 100, 1), "%. ",
              tr("considered_low")
            )
          )
          
        } else if (diabetes_risk >= 0.40) {
          
          # HIGH RISK
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_high_diabetes"), ": ")),
            paste0(
              tr("your_predicted_diabetes_risk_is"), " ",
              round(diabetes_risk * 100, 1), "%. ",
              tr("considered_high"), " ",
              tr("consult_specialist")
            )
          )
          
        } else {
          
          # ELEVATED RISK
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_elev_diabetes"), ": ")),
            paste0(
              tr("your_predicted_diabetes_risk_is"), " ",
              round(diabetes_risk * 100, 1), "%. ",
              tr("considered_elevated")
            )
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
          caption = "Risk thresholds: <20% low, 20–40% elevated, >40% high"
        )
        
        # Dynamic label placement
        if (diabetes_risk < 0.20) {
          base +
            annotate(
              "text",
              x = diabetes_risk + 0.03,
              y = 0.5,
              label = paste0(round(diabetes_risk * 100, 1), tr("unit_percent")),
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
              label = paste0(round(diabetes_risk * 100, 1), tr("unit_percent")),
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
      
      hypertension_risk <- predict(hypertension_model, newdata = new_person, type = "response")
      
      
      # ============================
      # HYPERTENSION WARNING SYSTEM 
      # ============================
      
      output$warning_box <- renderUI({
        
        if (hypertension_risk < 0.20) {
          
          # LOW RISK
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
                 padding:15px; margin-top:15px;",
            strong(paste0("✅ ", tr("warn_low_hyper"), ": ")),
            paste0(
              tr("your_predicted_hypertension_risk_is"), " ",
              round(hypertension_risk * 100, 1), "%. ",
              tr("considered_low")
            )
          )
          
        } else if (hypertension_risk >= 0.40) {
          
          # HIGH RISK
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_high_hyper"), ": ")),
            paste0(
              tr("your_predicted_hypertension_risk_is"), " ",
              round(hypertension_risk * 100, 1), "%. ",
              tr("considered_high"), " ",
              tr("consult_specialist")
            )
          )
          
        } else {
          
          # ELEVATED RISK
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_elev_hyper"), ": ")),
            paste0(
              tr("your_predicted_hypertension_risk_is"), " ",
              round(hypertension_risk * 100, 1), "%. ",
              tr("considered_elevated")
            )
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
          caption = "Risk thresholds: <20% low, 20–40% elevated, >40% high"
        )
        
        # Dynamic label placement
        if (hypertension_risk < 0.20) {
          base +
            annotate(
              "text",
              x = hypertension_risk + 0.03,
              y = 0.5,
              label = paste0(round(hypertension_risk * 100, 1), tr("unit_percent")),
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
              label = paste0(round(hypertension_risk * 100, 1), tr("unit_percent")),
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
      
      
      # ============================
      # BMI WARNING SYSTEM
      # ============================
      
      output$warning_box <- renderUI({
        
        if ((bmi_t1 >= 18.5 && bmi_t1 < 25) && (predicted_bmi >= 18.5 && predicted_bmi < 25)) {
          
          # HEALTHY BMI
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
                 padding:15px; margin-top:15px;",
            strong(paste0("✅ ", tr("warn_bmi_healthy"), ": ")),
            paste0(
              tr("your_predicted_weight_gain_is"), " ",
              round(predicted_weight - input$body_weight, 1), " ", tr("unit_kg"), ". ",
              tr("warn_bmi_healthy")
            )
          )
          
        } else if (bmi_t1 >= 30 || predicted_bmi >= 30) {
          
          # OBESE
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_bmi_obese"), ": ")),
            paste0(
              tr("your_predicted_weight_gain_is"), " ",
              round(predicted_weight - input$body_weight, 1), " ", tr("unit_kg"), ". ",
              tr("warn_bmi_obese"), ". ",
              tr("consult_specialist")
            )
          )
          
        } else if (bmi_t1 < 18.5 || predicted_bmi < 18.5) {
          
          # UNDERWEIGHT
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_bmi_under"), ": ")),
            paste0(
              tr("your_predicted_weight_gain_is"), " ",
              round(predicted_weight - input$body_weight, 1), " ", tr("unit_kg"), ". ",
              tr("warn_bmi_under"), ". ",
              tr("consult_specialist")
            )
          )
          
        } else {
          
          # OVERWEIGHT
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_bmi_over"), ": ")),
            paste0(
              tr("your_predicted_weight_gain_is"), " ",
              round(predicted_weight - input$body_weight, 1), " ", tr("unit_kg"), ". ",
              tr("warn_bmi_over")
            )
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
          category = factor(
            c(tr("bmi_underweight"), tr("bmi_healthy"), tr("bmi_overweight"), tr("bmi_obese")),
            levels = c(tr("bmi_underweight"), tr("bmi_healthy"), tr("bmi_overweight"), tr("bmi_obese"))
          ),
          xmin = c(0, 18.5, 25, 30),
          xmax = c(18.5, 25, 30, 50)
        )
        
        colors <- c(
          "#74add1",
          "#a1d99b",
          "#fdae61",
          "#d73027"
        )
        
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
            geom_vline(xintercept = predicted_bmi, color = "blue", size = 2, linetype = "dashed") +
            annotate("text", x = predicted_bmi - 0.5, y = 0.5,
                     label = paste0(tr("msg_weight"), " ", round(predicted_bmi, 1)),
                     size = 5, fontface = "bold", color = "blue", hjust = 1) +
            annotate("text", x = bmi_value + 0.5, y = 0.5,
                     label = paste0("BMI: ", round(bmi_value, 1)),
                     size = 5, fontface = "bold", color = "black", hjust = 0)
        } else {
          base_plot +
            geom_vline(xintercept = predicted_bmi, color = "blue", size = 2, linetype = "dashed") +
            annotate("text", x = predicted_bmi + 0.5, y = 0.5,
                     label = paste0(tr("msg_weight"), " ", round(predicted_bmi, 1)),
                     size = 5, fontface = "bold", color = "blue", hjust = 0) +
            annotate("text", x = bmi_value - 0.5, y = 0.5,
                     label = paste0("BMI: ", round(bmi_value, 1)),
                     size = 5, fontface = "bold", color = "black", hjust = 1)
        }
      })
      
      
      # ============================
      # WAIST PLOT
      # ============================
      
      output$waist_plot <- renderPlot({
        
        waist <- input$waist
        if (is.na(waist) || waist == "") waist <- 80
        gender <- as.numeric(input$gender)
        
        wc_df <- if (gender == 1) {
          tibble(
            category = factor(
              c(tr("waist_healthy"), tr("waist_elevated"), tr("waist_high")),
              levels = c(tr("waist_healthy"), tr("waist_elevated"), tr("waist_high"))
            ),
            xmin = c(0, 94, 102),
            xmax = c(94, 102, 150)
          )
        } else {
          tibble(
            category = factor(
              c(tr("waist_healthy"), tr("waist_elevated"), tr("waist_high")),
              levels = c(tr("waist_healthy"), tr("waist_elevated"), tr("waist_high"))
            ),
            xmin = c(0, 80, 88),
            xmax = c(80, 88, 150)
          )
        }
        
        colors <- c(
          "#a1d99b",
          "#fdae61",
          "#d73027"
        )
        
        names(colors) <- c(
          tr("waist_healthy"),
          tr("waist_elevated"),
          tr("waist_high")
          )
        
        
        waistplot <- make_classification_plot(
          value = waist,
          categories_df = wc_df,
          xlab = tr("title_waist_plot"),
          title = tr("title_waist_plot"),
          caption = tr("caption_waist"),
          colors = colors
        )
        
        if (waist < 55) {
          waistplot +
            annotate("text", x = waist + 2, y = 0.5,
                     label = paste0(tr("Current waist circumference"), ": ", round(waist, 1), " cm"),
                     size = 5, fontface = "bold", hjust = 0)
        } else {
          waistplot +
            annotate("text", x = waist - 2, y = 0.5,
                     label = paste0(tr("Current waist circumference"), ": ", round(waist, 1), " cm"),
                     size = 5, fontface = "bold", hjust = 1)
        }
      })
    }
    
    
    # ============================================================
    # Prediction 4 — CHOLESTEROL (TWEETALIG)
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
      
      new_processed <- predict(cholesterol_preproc, new_person)
      new_dummied <- predict(cholesterol_dummies, new_processed)
      
      cholesterol <- predict(cholesterol_model, new_dummied)
      
      
      # ============================
      # CHOLESTEROL WARNING SYSTEM 
      # ============================
      
      output$warning_box <- renderUI({
        
        if (cholesterol < 5.2) {
          
          # OPTIMAL
          div(
            style = "background-color:#e6ffe6; border-left:6px solid #009900;
                 padding:15px; margin-top:15px;",
            strong(paste0("✅ ", tr("warn_chol_optimal"), ": ")),
            paste0(
              tr("your_predicted_cholesterol_is"), " ",
              round(cholesterol, 2), " ", tr("unit_mmol_l"), "."
            )
          )
          
        } else if (cholesterol >= 6.2) {
          
          # HIGH RISK
          div(
            style = "background-color:#ffe6e6; border-left:6px solid #cc0000;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_chol_high"), ": ")),
            paste0(
              tr("your_predicted_cholesterol_is"), " ",
              round(cholesterol, 2), " ", tr("unit_mmol_l"), ". ",
              tr("consult_specialist")
            )
          )
          
        } else {
          
          # ELEVATED
          div(
            style = "background-color:#fff3cd; border-left:6px solid #e6a800;
                 padding:15px; margin-top:15px;",
            strong(paste0("⚠️ ", tr("warn_chol_elevated"), ": ")),
            paste0(
              tr("your_predicted_cholesterol_is"), " ",
              round(cholesterol, 2), " ", tr("unit_mmol_l"), "."
            )
          )
        }
      })
      
      
      # ============================
      # CHOLESTEROL PLOT
      # ============================
      
      output$cholesterol_plot <- renderPlot({
        
        chol_df <- tibble(
          category = factor(
            c(tr("chol_optimal"), tr("chol_borderline"), tr("chol_high")),
            levels = c(tr("chol_optimal"), tr("chol_borderline"), tr("chol_high"))
          ),
          xmin = c(0, 5.2, 6.2),
          xmax = c(5.2, 6.2, 10)
        )
        
        colors <- c(
          "#a1d99b",
          "#fdae61",
          "#d73027"
        )
        
        names(colors) <- c(
          tr("chol_optimal"),
          tr("chol_borderline"),
          tr("chol_high")
          )
        
        make_classification_plot(
          value = cholesterol,
          categories_df = chol_df,
          xlab = tr("msg_cholesterol"),
          title = tr("title_chol_plot"),
          caption = tr("caption_cholesterol"),
          label_prefix = tr("msg_cholesterol"),
          colors = colors
        )
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
      
      new_processed <- predict(glucose_preproc, new_person)
      new_dummied <- predict(glucose_dummies, new_processed)
      
      glucose <- predict(glucose_model, new_dummied)
      
      output$glucose_plot <- renderPlot({
        glucose <- glucose
        
        glucose_df <- tibble(
          category = factor(
            c(tr("glucose_low"), tr("glucose_normal"), tr("glucose_prediabetes"), tr("glucose_diabetes")),
            levels = c(tr("glucose_low"), tr("glucose_normal"), tr("glucose_prediabetes"), tr("glucose_diabetes"))
          ),
          xmin = c(0, 3.78, 6.97, 7.61),
          xmax = c(3.78, 6.97, 7.61, 21)
        )
 
        
        colors <- c(
          "#74add1",
          "#a1d99b",
          "#fdae61",
          "#d73027"
        )
        
        names(colors) <- c(
          tr("glucose_low"),
          tr("glucose_normal"),
          tr("glucose_prediabetes"),
          tr("glucose_diabetes")
        )
        
        make_classification_plot(
          value = glucose,
          categories_df = glucose_df,
          xlab = "Glucose (mmol/L)",
          title = "Your Glucose Classification",
          caption = "Categories based on World Health Organization (WHO)",
          label_prefix = "Predicted glucose:",
          colors = colors
        )
      })
    }
    
    
    output$message <- renderText({
      req(input$prediction)
      
      switch(input$prediction,
             "Diabetes"     = tr("msg_diabetes"),
             "Hypertension" = tr("msg_hypertension"),
             "Weight Gain"  = tr("msg_weight"),
             "Cholesterol"  = tr("msg_cholesterol"),
             "Glucose"      = tr("msg_glucose")
      )
    })
    
    
    output$risk <- renderText({
      req(input$prediction)
      
      if (input$prediction == "Diabetes") {
        paste0(round(diabetes_risk * 100, 2), " %")
      } else if (input$prediction == "Hypertension") {
        paste0(round(hypertension_risk * 100, 2), " %")
      } else if (input$prediction == "Weight Gain") {
        paste0(round(predicted_weight - input$body_weight, 2), " kg")
      } else if (input$prediction == "Cholesterol") {
        paste0(round(cholesterol, 2), " mmol/l")
      } else {
        paste0(round(glucose, 2), " mmol/L")
      }
      
    })
    

  })
  
  # Notes/limitations
  output$notes <- renderUI({
    tagList(
      p(tr("notes_generalize")),
      p(tr("notes_not_medical")),
      p(tr("notes_not_stored"))
    )
  })
  
  output$tab_background_title <- renderUI({
    if (input$language == "en") {
      "Background Information"
    } else {
      "Achtergrondinformatie"
    }
  })
}
