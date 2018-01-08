library(shiny)
fluidPage(
  titlePanel("Occupancy detection using sensor dataset.\n From UCI ML repository."),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition="input.tabselected==1",
        selectInput("choice1", h5("Please select"),
                    choices = list("Training Data" = 1, "Testing Data" = 2,
                                   "Data Summary" = 3), selected = 1)
      ),
      conditionalPanel(
        condition="input.tabselected==2",
        selectInput("choice2", h5("Please select"),
                    choices = list("Correlation" = 1, "Importance" = 2), selected = 1)
      ),
      conditionalPanel(
        condition="input.tabselected==3",
        selectInput("choice3", h5("Please select"),
                    choices = list("Accuracy" = 1, "Confusion Matrix" = 2), selected = 1)
      ),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", value = 1, 
                 conditionalPanel(condition="input.choice1==1", tableOutput("train")),
                 conditionalPanel(condition="input.choice1==2", tableOutput("test")),
                 conditionalPanel(condition="input.choice1==3", verbatimTextOutput("summary"))
                 ),
        tabPanel("Attribute", value = 2, 
                 conditionalPanel(condition="input.choice2==1", plotOutput("corr")),
                 conditionalPanel(condition="input.choice2==2", plotOutput("var_imp"))
                 ),
        tabPanel("Result", value = 3, 
                 conditionalPanel(condition="input.choice3==1", tableOutput("acc")),
                 conditionalPanel(condition="input.choice3==2", verbatimTextOutput("conf_matrix"))
                 ),
        id = "tabselected"
        )
      )
    )
  )