dashboardPage(
  skin = "black",
  dashboardHeader(title = "Stroke Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Group 11", tabName = "group", icon = icon("user-group")),
      menuItem("Dataset",
               tabName = "data",
               icon = icon("database")),
      menuItem(
        "Categorical Feature",
        tabName = "categorial",
        icon = icon("chart-simple")
      ),
      menuItem(
        "Exploratory Data Analysis",
        tabName = "eda",
        icon = icon("chart-simple")
      ),
      menuItem("Logistic Regression", tabName = "lr", icon = icon("medal"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$script(HTML("$('body').addClass('fixed');")),
    tabItems(
      #Group tab content
      tabItem(tabName = "group",
              fluidRow(
                infoBox("19IT1",
                        "Tran Quang Dat",
                        "19IT006",
                        icon = icon("snowflake")),
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
                     column(
                       width = 8,
                       tags$img(
                         src = "stroke1.jpg",
                         width = '100%',
                         height = 'auto',
                         object = 'cover'
                       ),
                       
                     ),
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
          tabPanel("Summary",
                   verbatimTextOutput("summary"),
                   icon = icon("chart-pie"))
        )
        
      ),
      # Second tab content
      tabItem(
        tabName = "categorial",
        fluidRow(
          box(
            width = 4,
            plotOutput("g1_gender"),
            tags$p(
              "There are more Female patients than Male. The one entry that was stated as Other was added to the Female section since majority are female patients."
            )
          ),
          box(
            width = 4,
            plotOutput("g1_married"),
            tags$p(
              "Roughly double the amount of patients have been married before than those who have not."
            )
          ),
          box(
            width = 4,
            plotOutput("g1_residence"),
            tags$p(
              "The patients are nearly evenly distributed between rural and urban residences"
            )
          )
        ),
        fluidRow(
          box(
            width = 4,
            plotOutput("g1_hyper"),
            tags$p(
              "The number of patients without hypertension is vastly greater than the number of patients with hypertension, but the gap is slightly less than the gap seen for stroke victims."
            )
          ),
          box(
            width = 4,
            plotOutput("g1_heart"),
            tags$p(
              "The gap between patients with and without heart disease more closely resembles the gap between those with and without strokes."
            )
          ),
          box(
            width = 4,
            plotOutput("g1_stroke"),
            tags$p(
              "The number of patients who have not had strokes is vastly greater than the number of patients who have."
            ),
          ),
          box(
            width = 12,
            plotOutput("g1_age"),
            tags$p(
              ""
            )
          ),
          box(
            width = 12,
            plotOutput("g1_bmi"),
            tags$p(
              ""
            )
          ),
          box(
            width = 12,
            plotOutput("g1_glu"),
            tags$p(
              ""
            )
          ),
          box(
            width = 12,
            plotOutput("g1_work"),
            tags$p(
              "There are approximately even amounts of patients that are working government jobs, are self-employed, and are children. The majority of patients work for private companies, and a small number have never worked."
            )
          ),
          box(
            width = 12,
            plotOutput("g1_smoking"),
            tags$p(
              "The unknown data was randomly added to the three categories above based of the probability. Most patients have either never smoked. The data for formerly and currently smokers are similar."
            )
          ),
        )
      ),
      tabItem(tabName = "eda",
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = 12,
                    plotOutput("g2_gender"),
                    tags$p(
                      "We can see from the plots that the gender is not a feature that descriminate a person having a stroke or not."
                    )
                  ),
                  box(width = 12, plotOutput("g2_hyper"),
                      tags$p("")),
                  box(
                    width = 12,
                    plotOutput("g2_smoking"),
                    tags$p(
                      "Surprisingly, it seems that the stroke is not highly corralated to smokers since the proportion of person having a stroke is fairly the same among the different smoking status."
                    )
                  ),
                  box(width = 12, plotOutput("g2_married"),
                      tags$p("")),
                ),
                column(
                  width = 6,
                  box(width = 12, plotOutput("g2_residence"),
                      tags$p("Residence Type has nothing to do with stroke")),
                  
                  box(width = 12, plotOutput("g2_heart"),
                      tags$p("")),
                  box(
                    width = 12,
                    plotOutput("g2_work"),
                    tags$p(
                      "In term of proportion private and self-employed have the similar amount of people having a stroke. However people from the gouvernment are more likely to not have a stroke compared to both first gategories moreover chlidren are not very likekly to get a stroke. Maybe that could be explain due to the degree of pressure felt by workers"
                    )
                  ),
                ),
                
              )),
      tabItem(tabName = "lr",
              h2("Logistic Regression"),
              fluidRow(
                column(
                  width = 6,
                  plotOutput("g_corrplot")
                ),
                column(
                  width = 6,
                  verbatimTextOutput("modal_summary")
                )
              ))
    )
  )
)
