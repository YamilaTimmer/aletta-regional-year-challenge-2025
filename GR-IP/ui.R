library(shiny)

zipcode_data <- read.csv("../data/north_zipcodes.csv")


ui <- fluidPage(

  titlePanel(
    div(
      style = "display: flex; align-items: center;",
      tags$img(src = "logo.png", height = "80px", style = "margin-right: 15px;"),
      h2("Groningen Risk‑Insight for Prevention (GR-IP)")
    )
  ),
  
  
  
  tabsetPanel(
    
    # ============================================================
    # TAB 1 — RISK PREDICTION
    # ============================================================
    tabPanel(
      "Risk Prediction",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "prediction",
            "Select prediction:",
            choices = c("Diabetes", "Hypertension", "Weight Gain", "Cholesterol", "Glucose")
          ),
          
          conditionalPanel(
            condition = "input.prediction == 'Diabetes'",
            
            
            numericInput("age", "Age (years)", value = 40, min = 18, max = 100),
            selectInput("gender", "Gender", choices = c("Female" = 0, "Male" = 1)),
            numericInput("body_height", "Body length (cm)", value = 180, min = 70, max = 250),
            numericInput("body_weight", "Body weight (kg)", value = 80, min = 30, max = 250),
            numericInput("waist", "Waist circumference (cm)", value = 90, min = 40, max = 200),
            numericInput("nova1", "NOVA group 1 intake (servings/day)", value = 5, min = 0, max = 30),
            selectInput("zipcode", "Zipcode (first 4 digits)", choices = zipcode_data$postcode),
            
          ),

          
          conditionalPanel(
            condition = "input.prediction == 'Hypertension'",
            
            numericInput("age", "Age (years)", value = 40, min = 18, max = 100),
            selectInput("gender", "Gender", choices = c("Female" = 0, "Male" = 1)),
            numericInput("body_height", "Body length (cm)", value = 180, min = 70, max = 250),
            numericInput("body_weight", "Body weight (kg)", value = 80, min = 30, max = 250),
            numericInput("waist", "Waist circumference (cm)", value = 90, min = 40, max = 200),
            numericInput("nova1", "NOVA group 1 intake (servings/day)", value = 5, min = 0, max = 30),
            numericInput("alcohol_intake", "Alcohol intake (avg servings/day)", value = 0, min = 0, max = 30),
            selectInput("zipcode", "Zipcode (first 4 digits)", choices = zipcode_data$postcode),
          
          ),
          
          conditionalPanel(
            condition = "input.prediction == 'Weight Gain'",
         
            
          ),
          
          conditionalPanel(
            condition = "input.prediction == 'Glucose'",
            
      
          ),
          
          conditionalPanel(
            condition = "input.prediction == 'Cholesterol'",
            
         
          ),
          
          
          actionButton("predict", "Predict risk")
        ),
        
        mainPanel(
          h3("Result"),
          verbatimTextOutput("message"),
          verbatimTextOutput("risk"),
          tags$hr(),
          h4("Notes & limitations"),
          uiOutput("limitations")
        )
      )
    ),
    
    
    # ============================================================
    # TAB 2 — BACKGROUND INFO/HOW TO USE
    # ============================================================
    tabPanel(
      "Background Information",
      
    ),
    
    # ============================================================
    # TAB 2 — BIOMARKER PREDICTION
    # ============================================================
    tabPanel(
      "Biomarker Prediction",
      
      sidebarLayout(
        sidebarPanel(
          h4("Biomarker model (placeholder)"),
          helpText("Add your biomarker inputs here."),
          
          numericInput("bio_age", "Age", value = 40, min = 18, max = 100),
          numericInput("bio_bmi", "BMI", value = 25, min = 10, max = 60),
          numericInput("bio_waist", "Waist circumference", value = 90, min = 40, max = 200),
          
          actionButton("predict_bio", "Predict biomarker")
        ),
        
        mainPanel(
          h3("Biomarker prediction result"),
          verbatimTextOutput("bio_result")
        )
      )
    )
  )
)
