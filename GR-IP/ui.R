library(shiny)

zipcode_data <- read.csv("../data/north_zipcodes.csv")

ui <- fluidPage(
  
  titlePanel(
    div(
      style = "display: flex; align-items: center;",
      tags$img(src = "logo.png", height = "80px", style = "margin-right: 15px;"),
      h2(uiOutput("app_title"))
    )
  ),
  
  tabsetPanel(
    
    # ============================================================
    # TAB 1 — RISK PREDICTION
    # ============================================================
    tabPanel(
      title = uiOutput("tab_risk_title"),
      
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
          h3(uiOutput("result")),
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
          h4(uiOutput("notes_title")),
          uiOutput("notes")
        )
      )
    ),
    
    # ============================================================
    # TAB 2 — How to use
    # ============================================================
    
    tabPanel(
      title = uiOutput("tab_usage_title"),
      
      # ============================
      # ENGLISH VERSION
      # ============================
      conditionalPanel(
        condition = "input.language == 'en'",
        
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
          
          p("Under the 'Risk Prediction' tab, select a variable to predict and fill in 
          all the inputs in the sidebar and click the predict button to get a personalised 
          prediction of a selected health outcome."),
          
          # Section 1
          h3("Predictor variables", style = "margin-top: 25px;"),
          p("Below a description can be found on all of the variables used to 
          predict risk/health outcomes. This app uses various machine learning 
          models that use the variables below to predict outcomes."),
          
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
                                         height = "300px", 
                                         style = "margin-top:10px; border-radius:8px;") 
            )
          )
        )
      ),
      
      # ============================
      # DUTCH VERSION
      # ============================
      conditionalPanel(
        condition = "input.language == 'nl'",
        
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
          h2("Hoe gebruik je de voorspellingstool", 
             style = "font-weight: 700; margin-bottom: 20px;"),
          
          p("Selecteer een variabele om te voorspellen in het 'Risicovoorspelling' 
          tabblad, vul alle invoervelden van de zijbalk in en klik de 'Voorspel waarde' knop
          om een gepersonaliseerde voorspelling te ontvangen van de geselecteerde variabele."),
          
          # Section 1
          h3("Voorspellende variabelen", style = "margin-top: 25px;"),
          p("Hieronder vind je een beschrijving van alle variabelen die worden gebruikt 
          om risico’s en gezondheidsuitkomsten te voorspellen. Er wordt hiervoor gebruik gemaakt van 
            verschillende machine learning modellen die verschillende variabelen gebruiken."),
          
          tags$ul(
            tags$li(tags$b("Leeftijd:"), " Leeftijd in jaren."),
            tags$li(tags$b("Geslacht:"), " Biologisch geslacht (man/vrouw)."),
            tags$li(tags$b("Lengte en gewicht:"), " Huidige lengte in cm en gewicht in kg."),
            tags$li(tags$b("Postcode:"), 
                    " De eerste vier cijfers van een Nederlandse postcode in Drenthe, 
            Groningen of Friesland. Bijvoorbeeld: bij postcode 9613 AL gebruik je '9613'.")
          ),
          
          h4("Voedingsgerelateerde variabelen", 
             style = "margin-top: 20px; font-weight: bold;"),
          
          tags$ul(
            tags$li(tags$b("Inname van onbewerkt voedsel:"), 
                    " Gemiddeld aantal grammen per dag van onbewerkte producten, zoals groente, 
            fruit, noten, eieren en volkorenproducten."),
            
            tags$li(tags$b("Inname van sterk bewerkt voedsel:"), 
                    " Gemiddeld aantal grammen per dag van sterk bewerkte producten, zoals 
            kant-en-klare maaltijden, chips, fastfood en chocolade."),
            
            tags$li(tags$b("Alcoholinname:"), 
                    " Gemiddeld aantal standaardglazen per dag. Eén standaardglas staat gelijk aan 
            één glas bier (250 mL), één glas wijn (100 mL) of één shot (35 mL) sterke drank."),
            
            tags$li(tags$b("Toegevoegde suikers:"), 
                    " Gemiddelde dagelijkse inname van toegevoegde suikers in grammen. Dit omvat o.a.
            suiker die wordt toegevoegd aan koffie/thee of tijdens de verwerking van 
            voedingsmiddelen, en komt voor in veel snacks, frisdrank en ijs. 
            Voedingsinformatie over toegevoegde suikers staat op de verpakking."),
            
            tags$li(tags$b("Calorie-inname (kcal):"), 
                    " Gemiddeld aantal calorieën dat dagelijks wordt geconsumeerd."),
            
            # Waist circumference item + image 
            tags$li(
              tags$b("Taille-/heupomtrek:"), 
              " Meet dit met een meetlint zoals hieronder weergegeven.",
              tags$br(),
              tags$img(src = "measure.png",
                       height = "300px",
                       style = "margin-top:10px; border-radius:8px;")
            )
          )
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
            ),
            tags$li(
              "Voedingscentrum. (z.d.). BMI berekenen.",  
              tags$a(
                href = "https://www.voedingscentrum.nl/bmi",
                "https://www.voedingscentrum.nl/bmi",
                target = "_blank"
              )
            ),
            tags$li(
              "World Health Organization. (2011). Use of glycated haemoglobin (HbA1c) in the diagnosis of diabetes mellitus. World Health Organization."
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
            "GR-IP: Een interactieve risicovoorspellende app die mensen helpt GRIP te krijgen op hun voedselomgeving."
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
            ),
            tags$li(
              "Voedingscentrum. (z.d.). BMI berekenen.", 
              tags$a(
                href = "https://www.voedingscentrum.nl/bmi",
                "https://www.voedingscentrum.nl/bmi",
                target = "_blank"
              )
            ),
            tags$li(
              "World Health Organization. (2011). Use of glycated haemoglobin (HbA1c) in the diagnosis of diabetes mellitus. World Health Organization."
            )
          )
          
        )
      )
    )
  )
)