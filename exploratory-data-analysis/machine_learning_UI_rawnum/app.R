library(shiny)
library(tidyverse)
library(isotree)

# 모델 로드
model_obj <- readRDS("diabetes_iso_model_unscaled.rds")

iso_model      <- model_obj$iso_model
dummies        <- model_obj$dummies
X_train        <- model_obj$X_train
best_threshold <- model_obj$best_threshold
train_scores   <- model_obj$train_scores

ui <- fluidPage(
  titlePanel("predict diabetese"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("lat", "Latitude", value = round(mean(X_train$latitude),4), min = min(X_train$latitude), max = max(X_train$latitude)),
      numericInput("lon", "Longitude",  value = round(mean(X_train$longitude),4), min = min(X_train$longitude), max = max(X_train$longitude)),
      numericInput("age", "age (scaled)", value = round(mean(X_train$age),0), min = min(X_train$age), max = max(X_train$age)),
      numericInput("weight", "weight (scaled)", value =round(mean(X_train$body_weight_1),2), min = min(X_train$body_weight_1), max = max(X_train$body_weight_1)),
      numericInput("waist", "circumference waist (scaled)", value = round(mean(X_train$circumference_waist_1),2), min = min(X_train$circumference_waist_1), max = max(X_train$circumference_waist_1)),
      numericInput("hip", "circumferemce hip (scaled)", value = round(mean(X_train$circumference_hip_1),2), min = min(X_train$circumference_hip_1), max = max(X_train$circumference_hip_1)),
      
      actionButton("predict", "predict", class = "btn-danger")
    ),
    
    mainPanel(
      br(),
      h1(textOutput("risk_text"), align = "center"),
      br(),
      div(
        style = "border:2px dashed #ccc; padding:20px; min-height:120px;",
        textOutput("health_info")
      )
    )
  )
)


server <- function(input, output) {
  
  risk_result <- eventReactive(input$predict, {
    
    new_person <- data.frame(
      latitude = input$lat,
      longitude = input$lon,
      age = input$age,
      body_weight_1 = input$weight,
      circumference_waist_1 = input$waist,
      circumference_hip_1 = input$hip,
      kcal_intake = 0,
      nova_foodintake_1 = 0,
      nova_foodintake_4 = 0,
      alcohol_intake = 0,
      added_sugar = 0
    )
    
    # Dummy encoding
    new_person_num <- predict(dummies, newdata = new_person) %>% as.data.frame()
    
    # 컬럼 맞추기
    missing_cols <- setdiff(names(X_train), names(new_person_num))
    for (col in missing_cols) new_person_num[[col]] <- 0
    new_person_num <- new_person_num[, names(X_train)]
    
    # Anomaly score
    score <- predict(iso_model, as.matrix(new_person_num), type = "score")
    
    # Risk scaling (0–1)
    risk <- (score - min(train_scores)) /
      (max(train_scores) - min(train_scores))
    risk <- min(max(risk, 0), 1)
    
    list(score = score, risk = risk)
  })
  
  output$risk_text <- renderText({
    req(risk_result())
    paste0("risk score: ", round(risk_result()$risk * 100, 1), "%")
  })
  
  output$health_info <- renderText({
    req(risk_result())
    if (risk_result()$risk > best_threshold) {
      "⚠️ high risk (health care guidline will be placed)"
    } else {
      "✅ low risk"
    }
  })
}

shinyApp(ui, server)
