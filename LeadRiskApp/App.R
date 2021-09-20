# Load required packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#### User Interface (UI) ####
ui <- shinyUI(
  dashboardPage(
    
    title = "IUPUI Lead Risk App", # text that shows in browser tab
    
    header = dashboardHeader(
      title = "IUPUI Lead Risk App" # text that shows for title on app itself
    ), # close Dashboard header
    
    dashboardSidebar(
      minified = FALSE,
      collapsed = TRUE,
      sidebarMenu( id = "tabs",
                   menuItem(
                     text="Start Here",
                     tabName="home",
                     icon=icon("sign-in-alt")
                   ),
                   menuItem(
                     text="What is Lead",
                     tabName="about",
                     icon=icon("info-circle")
                   ),
                   menuItem(
                     text="Q1) Home Age?",
                     tabName="age",
                     icon=icon("hourglass")
                   ),
                   menuItem(
                     text="Q2) Peeling Paint?",
                     tabName="intpnt",
                     icon=icon("hammer")
                   ),
                   menuItem(
                     text="Risk Results",
                     tabName="risk",
                     icon=icon("chart-bar")
                   ),
                   menuItem(
                     text="How this works",
                     tabName="model",
                     icon=icon("question")
                   )
      )
    ), # close Dashboard sidebar
    
    dashboardBody(
      
      shinyjs::useShinyjs(),
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "LeadRiskApp.css")
      ),
      
      tabItems(
        tabItem(tabName = "home",
                div(class = "background1",
                    div(class = "textContCenter",
                          p(class = "textHead1", "What is your household risk for lead exposure?"
                          ),
                          actionButton(class = "btn1", inputId = "btn1", label = "Click here to find out!")
                    )
                )
        ),
        
        tabItem(tabName = "about",
                div(class = "background1",
                    div(class = "textContTop",
                         p(class = "textHead2", "Exposure to lead in dust, soil, paint, or water is dangerous because it can cause:"),
                         div(
                            tags$ul(class = "myListsRadios",
                              tags$li("Intellectual impairment (young children are most vulnerable)"),
                              tags$li("Weakness"),
                              tags$li("Cardiovascular disease"),
                              tags$li("Internal organ damage")
                            )
                         ),
                         br(),
                         p(class = "textHead2", "What does this app do?"),
                         div(
                           tags$ul(class = "myListsRadios",
                                   tags$li("Asks you a few simple questions about your home. It is ok to not be sure about an answer for any question!"),
                                   tags$li("Predicts if you are at low or high risk for having elevated lead in your home's dust."),
                                   tags$li("Links you to FREE home lead screening resources offered globally"),
                                   tags$li("Explains how we make our prediction for you, and links to the data and code we use.")
                           )
                         )
                    ),
                    actionButton(class = "btn2", inputId = "btn2", label = "Estimate your risk", icon=icon("arrow-right"))
                )
        ),
        
        tabItem(tabName = "age",
                div(class = "background1",
                    div(class = "textContCenter",
                         p(class = "textHead1", "1 of 2: What is the approximate age of your home?"),
                         div(class = "myListsRadios",
                           radioButtons("radioAge", label = NULL,
                                        choices = list("Built pre-1940" = 3,
                                                       "Built 1940-1959" = 2,
                                                       "Built 1960-1979" = 1,
                                                       "Built 1980-Present" = 0,
                                                       "Not Sure" = 4
                                        ),
                                        selected = ""
                           )
                         )
                    ),
                    actionButton(class = "btn2", inputId = "btn3", label = "Next Question", icon=icon("arrow-right"))
                )
        ),
        
        tabItem(tabName = "intpnt",
                div(class = "background1",
                    div(class = "textContCenter",
                         p(class = "textHead1", "2 of 2: Does your home have any peeling interior paint?"),
                         div(class = "myListsRadios",
                             radioButtons("radioPaint", label = NULL,
                                          choices = list("Yes" = 1,
                                                         "No" = 0,
                                                         "Not Sure" = 2
                                          ),
                                          selected = ""
                             )
                         )
                    ),
                    actionButton(class = "btn2", inputId = "btn4", label = "See Risk Results", icon=icon("arrow-right"))
                )
        ),
        
        tabItem(tabName = "risk",
                div(class = "background2",
                    div(class = "textContTop",
                         p(class = "textHead2", "Your Home's Lead Risk"),
                         div(class = "resHigh", id = "resHigh",
                             p(class = "textHead3", "Your home has a HIGHER chance of having elevated* levels of lead in house dust."),
                             a(class = "textP1", "Click here to register for FREE lead screening to determine the real lead content in your home's dust,
                               and learn how to reduce your overall lead exposure risks.",
                               href = "https://www.mapmyenvironment.com/",
                               target = "_blank"),
                             hr(),
                             p("*Elevated is considered >80 ppm, based on California's safe screening level for lead.
                             This is merely a simplified predictive model, and is not 100% accurate, even though it has
                               been shown to perform well against real-world data from across the United States.")
                          ),
                         div(class = "resLow", id = "resLow",
                             p(class = "textHead3", "Your home has a LOW chance of having elevated levels of lead in house dust.*"),
                             a(class = "textP1", "Despite having low risk you can still click here to register for FREE lead screening to determine the real lead
                             content in your home's dust, and learn how to reduce your overall lead exposure risks.",
                               href = "https://www.mapmyenvironment.com/",
                               target = "_blank"),
                             hr(),
                             p("*Elevated is considered >80 ppm, based on California's safe screening level for lead.
                             This is merely a simplified predictive model, and is not 100% accurate, even though it has
                               been shown to perform well against real-world data from across the United States.")
                         ),
                         div(class = "resStart", id = "resStart",
                             p(class = "textHead3", "Please answer the previous questions to determine your home's lead risk")
                         )
                    ),
                    actionButton(class = "btn2", inputId = "btn5", label = "Back to Start", icon=icon("redo")),
                    actionButton(class = "btn3", inputId = "btn6", label = "Learn how we make this prediction", icon=icon("calculator"))
                )
        ),
        
        tabItem(tabName = "model",
                div(class = "background1",
                    div(class = "textContTop",
                         p(class = "textHead1", "How our predictive model works."),
                         div(class = "myListsRadios",
                             p("Our model is based on ~400 house dust samples collected from across the United States."),
                             p("If your household dust is deemed likely to be elevated in lead, that means there is a
                               46% chance based on our model that the lead dust concentration is >80 ppm."),
                             p("If your household dust is deemed NOT likely to be elevated in lead, that means there is a
                               95% chance based on our model that the lead dust concentration is <80 ppm."),
                             p("Overall, our model correctly classified dusts as > or < 80 ppm lead 76% of the time."),
                             p("To see our actual logisitic regression model and data outputs, ",
                               a("click here.", href="https://raw.githubusercontent.com/dietrimj/Community-Science-Pb-Prediction/main/DustSafe_Logistic_Regression_Code_and_Output.pdf", target = "_blank")
                               ),
                             p("To see all of the source data and files associated with the model, ",
                               a("click here.", href="https://github.com/dietrimj/Community-Science-Pb-Prediction", target="_blank")
                             )
                         )
                    ),
                    actionButton(class = "btn2", inputId = "btn8", label = "Back to Start", icon=icon("redo"))
                )
        )
      )# close tab items block
    )# close Dashboard body
  )# close Dashboard Page block
)
#### End UI ####

##### Server ####
server <- function(input, output, session){
  
  shinyjs::runjs('
      $("#resHigh").hide();
      $("#resLow").hide();
  ')
  
  riskAppData <- reactiveValues(
    userData = NULL,
    prediction = NULL
  )
  
  observeEvent(c(input$btn1), {
    updateTabItems(session, "tabs", "about")
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$btn2), {
    updateTabItems(session, "tabs", "age")
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$btn3), {
    updateTabItems(session, "tabs", "intpnt")
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$btn4), {
    updateTabItems(session, "tabs", "risk")
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$btn5, input$btn8), {
    updateTabItems(session, "tabs", "home")
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$btn6), {
    updateTabItems(session, "tabs", "model")
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$radioAge, input$radioPaint), {
    req(!is.null(input$radioAge), !is.null(input$radioPaint))
    riskAppData$userData <- data.frame(Age = as.numeric(input$radioAge), Paint = as.numeric(input$radioPaint))
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$tabs), {
    req(input$tabs == "risk", !is.null(input$radioAge), !is.null(input$radioPaint))
    
    shinyjs::runjs('
      $("#resStart").hide();
    ')
    
    # browser()
    
    #calculate probability of LOW Pb!
    #log[p/(1-p)] = 2.1413 - 0. 4506 (Housing) - 1.1535 (Interior Paint Peeling)
    
    foo <- 2.1413 - 0.4506*riskAppData$userData$Age - 1.1535*riskAppData$userData$Paint
    foo.prob <- logit2prob(foo)
    
    if (foo.prob <= 0.8) {
      shinyjs::runjs('
      $("#resHigh").show();
      $("#resLow").hide();
      ')
      
    } else {
      shinyjs::runjs('
      $("#resHigh").hide();
      $("#resLow").show();
      ')
    }
    
  })
  
} 
#### End Server ####

# Initiate app
shinyApp(ui=ui, server=server)
