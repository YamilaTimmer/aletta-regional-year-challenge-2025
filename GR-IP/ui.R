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
            "language",
            "Taal / Language",
            choices = c("English" = "en", "Nederlands" = "nl"),
            selected = "nl"
          ),
          
          uiOutput("sidebar_inputs")
          
          
        ),
        
        mainPanel(
          h3("Result"),
          uiOutput("warning_box"),
          
          conditionalPanel(
            condition = "input.prediction == 'Diabetes'",
            plotOutput("diabetes_plot", height = "200px")
          ),
          
          conditionalPanel(
            condition = "input.prediction == 'Hypertension' || input.prediction == 'Hoge bloeddruk'",
            plotOutput("hypertension_plot", height = "200px")
          ),
          
          
          conditionalPanel( condition = "input.prediction == 'Weight Gain' || input.prediction == 'Gewichtstoename'", 
                            plotOutput("bmi_plot", height = "200px"), 
                            plotOutput("waist_plot", height = "200px") ),
          
          conditionalPanel( condition = "input.prediction == 'Cholesterol'", 
                            plotOutput("cholesterol_plot", height = "200px")
          ), 
          
          conditionalPanel( condition = "input.prediction == 'Glucose'", 
                            plotOutput("glucose_plot", height = "200px")
          ), 
          
          
          tags$hr(),
          h4("Notes"),
          uiOutput("notes")
        )
      )
    ),
    
    # ============================================================
    # TAB 2 — How to use
    # ============================================================
    tabPanel(
      title = "How to use",
      
      # Background box
      div(
        style = "
      background-color: #f7f9fc;
      padding: 30px;
      border-radius: 12px;
      max-width: 900px;
      margin: auto;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
    ",
        
        # Title
        h2("How to use the Prediction Tool", style = "font-weight: 700; margin-bottom: 20px;"),
        
        # Section 1
        h3("Predictor variables", style = "margin-top: 25px;"),
        p("Below a description can be found on all of the variables used to 
          predict risk/health outcomes. The different models each use different 
          variables."),
        
        tags$ul(
          tags$li(tags$b("Age:"), " Age in years."),
          tags$li(tags$b("Gender:"), " Biological sex (male/female)."),
          tags$li(tags$b("Body length and weight:"), "Current length in cm and weight in kg."),
          tags$li(tags$b("ZIP code:"), "The first four digits of a Dutch zipcode 
          in either Drenthe, Groningen or Friesland. E.g. for zipcode 9613 AL, '9613' has to be used.")
        ),
        
        h4("Nutritional Variables", style = "margin-top: 20px; font-weight: bold;"),
        
        tags$ul(
          tags$li(tags$b("Unprocessed food intake:"), " Average daily grams consumed of unprocessed foods. 
                  Such as fruits, vegetables, nuts, eggs and whole wheat products."),
          tags$li(tags$b("Highly-processed food intake:"), " Average daily grams consumed of highly-processed 
                  foods. Such as ready meals, crisps, fastfood and chocolate."),
          tags$li(tags$b("Alcohol intake:"), " Average number of standard drinks per day. One standard drink 
                  is equal to one glass of beer (250 mL), one glass of wine (100 mL) or 1 shot (35 mL
                  ) of strong liquor."),
          tags$li(tags$b("Added sugar:"), " Average added sugar intake daily in grams. Added 
                  sugars include sugar added to coffee/tea or non-naturally occuring sugars that were added 
                  during processing of the food item and are present in most snacks/sweets, soda and ice cream. 
                  Nutritional info on added sugar can be found on the packaging."),
          tags$li(tags$b("Kcal intake:"), " Average number of calories consumed daily."),
          
          # Waist circumference item + image 
          tags$li( tags$b("Waist/hip circumference:"), 
                   " Measure using a measuring tape as illustrated below.", 
                   tags$br(), tags$img(src = "measure.png", 
                                       height = "250px", 
                                       style = "margin-top:10px; border-radius:8px;") 
          ),
          
        )
      )
    ),
    
    # ============================================================
    # TAB 3 — BACKGROUND INFO
    # ============================================================
    tabPanel(
      title = uiOutput("tab_background_title"),
      
      # ============================
      # ENGLISH VERSION
      # ============================
      conditionalPanel(
        condition = "input.language == 'en'",
        
        div(
          style = "
        background-color: #f7f9fc;
        padding: 30px;
        border-radius: 12px;
        max-width: 900px;
        margin: auto;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      ",
          
          h2("About This Prediction Tool", style = "font-weight: 700; margin-bottom: 20px;"),
          
          tags$blockquote(
            style = "font-style: italic; color: #555; margin-top: 10px;",
            "GR-IP: A risk predicting interactive app that helps people get a GRIP on their food environment."
          ),
          
          h3("What this tool does", style = "margin-top: 25px;"),
          p("This app provides lifestyle based risk assessments for disease (hypertension & diabetes) 
         and other factors such as weight gain, and cholesterol/glucose levels. The prediction is based on a timespan of 5-10 years.
         Factors that are taken into account when doing predictions are easily measured at home and include items such as 
         body length/weight and caloric intake."),
          
          h3("Important notes", style = "margin-top: 25px;"),
          div(
            style = "
          background-color: #fff3cd;
          border-left: 6px solid #ffca2c;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 20px;
        ",
            p("This tool is not a medical device. Predictions are based on statistical patterns and 
           should not replace professional medical advice. User data is fully private and is not saved.")
          ),
          
          h3("Data sources", style = "margin-top: 25px;"),
          p("The prediction models used by this app are trained on Lifelines data, 
         one of the largest population studies in the Netherlands, aimed specifically at the northern provinces of 
         the country, including Drenthe, Groningen and Friesland. For more information, visit the ",
            tags$a(href = "https://www.lifelines.nl", "Lifelines website", target = "_blank"), "."),
          
          h3("Motivation", style = "margin-top: 25px;"),
          p("GR-IP was developed by three life sciences students from Hanze 
         University of Applied Sciences, in assignment for ",
            tags$a(href = "https://www.rug.nl/aletta/education/aletta-s-regional-year-challenge",
                   "'Aletta’s Regional Year Challenge 2025'", target = "_blank"), ". 
         The theme for this years challenge was to work on a project that can 
         help improve the food environment of northern Netherlands, we chose to 
         do so by making an app that gives users insight into health risks based 
         on lifestyle choices. All used models and data processing steps can be 
         found on our ",
            tags$a(href = "https://github.com/YamilaTimmer/aletta-regional-year-challenge-2025", 
                   "GitHub repository", target = "_blank"), "."),
          
          h3("Acknowledgements", style = "margin-top: 25px;"),
          p("This project was developed in cooperation with the following partner organizations:"),
          
          tags$ul(
            tags$li(tags$a(href = "https://www.lifelines.nl", target = "_blank",
                           tags$img(src = "lifelines-logo.png", height = "50px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.rug.nl", target = "_blank",
                           tags$img(src = "rug-logo.png", height = "60px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.hanze.nl", target = "_blank",
                           tags$img(src = "hanze-logo.png", height = "80px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.rug.nl/aletta", target = "_blank",
                           tags$img(src = "aletta-logo.png", height = "100px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.umcg.nl", target = "_blank",
                           tags$img(src = "umcg-logo.png", height = "50px", style = "margin-right:10px;")))
          ),
          
          h3("Sources", style = "margin-top: 25px;"),
          p("Categories for BMI, cholesterol and glucose are based on the sources below:"),
          tags$ul(
            tags$li(
              "InformedHealth.org. (2025, September 24). Overview: High cholesterol. Institute for Quality and Efficiency in Health Care (IQWiG). ",
              tags$a(
                href = "https://www.ncbi.nlm.nih.gov/books/NBK279318/",
                "https://www.ncbi.nlm.nih.gov/books/NBK279318/",
                target = "_blank"
              )
            )
          )
        )
      ),
      
      # ============================
      # DUTCH VERSION
      # ============================
      conditionalPanel(
        condition = "input.language == 'nl'",
        
        div(
          style = "
        background-color: #f7f9fc;
        padding: 30px;
        border-radius: 12px;
        max-width: 900px;
        margin: auto;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      ",
          
          h2("Achtergrondinformatie", style = "font-weight: 700; margin-bottom: 20px;"),
          
          tags$blockquote(
            style = "font-style: italic; color: #555; margin-top: 10px;",
            "GR-IP: Een interactieve risicovoorspellende app die mensen helpt grip te krijgen op hun voedselomgeving."
          ),
          
          h3("Wat deze tool doet", style = "margin-top: 25px;"),
          p("Deze app geeft leefstijl-gebaseerde risicobeoordelingen voor ziekten (hypertensie & diabetes) 
         en andere factoren zoals gewichtstoename en cholesterol-/glucosewaarden. De voorspellingen zijn gebaseerd op een periode van 5–10 jaar.
         De gebruikte factoren zijn eenvoudig thuis te meten, zoals lengte/gewicht en calorie-inname."),
          
          h3("Belangrijke opmerkingen", style = "margin-top: 25px;"),
          div(
            style = "
          background-color: #fff3cd;
          border-left: 6px solid #ffca2c;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 20px;
        ",
            p("Deze tool is geen medisch hulpmiddel. Voorspellingen zijn gebaseerd op statistische patronen 
           en vervangen geen professioneel medisch advies. Gebruikersdata is volledig privé en wordt niet opgeslagen.")
          ),
          
          h3("Databronnen", style = "margin-top: 25px;"),
          p("De voorspellingsmodellen in deze app zijn getraind op Lifelines-data, 
         een van de grootste bevolkingsstudies in Nederland, gericht op de noordelijke provincies 
         waaronder Drenthe, Groningen en Friesland. Voor meer informatie, bezoek de ",
            tags$a(href = "https://www.lifelines.nl", "Lifelines-website", target = "_blank"), "."),
          
          h3("Motivatie", style = "margin-top: 25px;"),
          p("GR-IP is ontwikkeld door drie Life Sciences-studenten van de 
         Hanzehogeschool Groningen, in opdracht voor ",
            tags$a(href = "https://www.rug.nl/aletta/education/aletta-s-regional-year-challenge",
                   "'Aletta’s Regional Year Challenge 2025'", target = "_blank"), ". 
         Het thema van deze challenge was om te werken aan een project dat 
         de voedselomgeving van Noord-Nederland kan verbeteren. Wij kozen ervoor 
         om een app te maken die gebruikers inzicht geeft in gezondheidsrisico’s 
         op basis van leefstijlkeuzes. Alle gebruikte modellen en 
         dataverwerkingsstappen zijn te vinden op onze ",
            tags$a(href = "https://github.com/YamilaTimmer/aletta-regional-year-challenge-2025",
                   "GitHub-repository", target = "_blank"), "."),
          
          h3("Dankwoord", style = "margin-top: 25px;"),
          p("Dit project is ontwikkeld in samenwerking met de volgende partnerorganisaties:"),
          
          tags$ul(
            tags$li(tags$a(href = "https://www.lifelines.nl", target = "_blank",
                           tags$img(src = "lifelines-logo.png", height = "50px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.rug.nl", target = "_blank",
                           tags$img(src = "rug-logo.png", height = "60px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.hanze.nl", target = "_blank",
                           tags$img(src = "hanze-logo.png", height = "80px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.rug.nl/aletta", target = "_blank",
                           tags$img(src = "aletta-logo.png", height = "100px", style = "margin-right:10px;"))),
            tags$li(tags$a(href = "https://www.umcg.nl", target = "_blank",
                           tags$img(src = "umcg-logo.png", height = "50px", style = "margin-right:10px;")))
          ),
          
          h3("Bronnen", style = "margin-top: 25px;"),
          p("Categorieën voor BMI, cholesterol en glucose zijn gebaseerd op onderstaande bronnen:"),
          tags$ul(
            tags$li(
              "InformedHealth.org. (2025, September 24). Overview: High cholesterol. Institute for Quality and Efficiency in Health Care (IQWiG). ",
              tags$a(
                href = "https://www.ncbi.nlm.nih.gov/books/NBK279318/",
                "https://www.ncbi.nlm.nih.gov/books/NBK279318/",
                target = "_blank"
              )
            )
          )
        )
      )
    )
  )
)