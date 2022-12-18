library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Stroke Analysis"),
  dashboardSidebar(sidebarMenu(
    menuItem("Group 11", tabName = "group", icon = icon("user-group")),
    menuItem("Dataset",
             tabName = "data",
             icon = icon("database")),
    menuItem("Chart", tabName = "chart", icon = icon("chart-simple")),
    menuItem("Predict", tabName = "predict", icon = icon("medal"))
  )),
  dashboardBody(tabItems(
    #Group tab content
    tabItem(tabName = "group",
            fluidRow(
              infoBox(
                "19IT1",
                "Tran Quang Dat",
                "19IT006",
                icon = icon("snowflake")
              ),
              infoBox(
                "19IT1",
                "Nguyen Van An",
                "19IT001",
                icon = icon("pagelines"),
                color = "olive",
              ),
              infoBox(
                "19IT1",
                "Ngo Thi Huong Giang",
                "19IT008",
                icon = icon("wand-magic-sparkles"),
                color = "purple"
              ),
            )),
    # Dataset tab content
    tabItem(
      tabName = "data",
      tabBox(
        id = "t1",
        width = 12,
        tabPanel("About", icon = icon("address-card"),
                 fluidRow(
                   column(width = 8,
                          tags$img(
                            src = "stroke1.jpg",
                            width = '100%',
                            height = 'auto',
                            object = 'cover'
                          ), ),
                   column(
                     width = 4,
                     tags$br() ,
                     tags$h4(
                       "According to the World Health Organization (WHO) stroke is the 2nd leading cause of death globally, responsible for approximately 11% of total deaths."
                     ),
                     tags$h4(
                       "This dataset is used to predict whether a patient is likely to get stroke based on the input parameters like gender, age, various diseases, and smoking status. Each row in the data provides relavant information about the patient."
                     )
                   )
                 )),
        tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")),
        tabPanel(
          "Structure",
          verbatimTextOutput("structure"),
          icon = icon("uncharted")
        ),
        tabPanel(
          "Summary Stats",
          verbatimTextOutput("summary"),
          icon = icon("chart-pie")
        )
      )
      
    ),
    # Second tab content
    tabItem(tabName = "chart",
            h2("Chart")),
    tabItem(tabName = "predict",
            h2("Predict"))
  ))
)
