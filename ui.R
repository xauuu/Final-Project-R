dashboardPage(
  skin = "black",
  dashboardHeader(title = "Stroke Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "group", icon = icon("house")),
      menuItem("Dataset",
               tabName = "data",
               icon = icon("database")),
      menuItem("Chart",
               tabName = "categorial",
               icon = icon("chart-simple")),
      menuItem("Chart 2",
               tabName = "eda",
               icon = icon("chart-pie")),
      menuItem(
        "Logistic Regression",
        tabName = "lr",
        icon = icon("medal")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tags$script(HTML("$('body').addClass('fixed');")),
    tabItems(
      #Group tab content
      tabItem(
        tabName = "group",
        tags$div(
          class = "about",
          id = "about",
          tags$canvas(id = "canvas"),
          tags$div(class = "logo",
                   tags$img(src = "vku.png")),
          tags$div(
            class = "title",
            tags$h2("Data Analysis with R"),
            tags$h3("Topic: Stroke Analysis"),
            tags$div("Group 5")
          ),
          fluidRow(
            infoBox(
              "19IT1",
              "Nguyen Van An",
              "19IT001",
              icon = icon("pagelines"),
              color = "olive",
            ),
            infoBox("19IT1",
                    "Tran Quang Dat",
                    "19IT006",
                    icon = icon("snowflake")),
            infoBox(
              "19IT1",
              "Ngo Thi Huong Giang",
              "19IT008",
              icon = icon("wand-magic-sparkles"),
              color = "purple"
            ),
          )
        ),
      ),
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
          tabPanel("Structure",
                   fluidRow(
                     column(
                       width = 12,
                       verbatimTextOutput("structure"),
                       tags$p("Attribute Information"),
                       tags$ul(
                         tags$li("gender: Male, Female"),
                         tags$li("age: age of the patient"),
                         tags$li(
                           "hypertension: 0 if the patient doesn't have hypertension, 1 if the patient has hypertension"
                         ),
                         tags$li(
                           "heart_disease: 0 if the patient doesn't have any heart diseases, 1 if the patient has a heart disease"
                         ),
                         tags$li("ever_married: No or Yes"),
                         tags$li(
                           "work_type: children, Govt_jov, Never_worked, Private, Self-employed"
                         ),
                         tags$li("Residence_type: Rural or Urban"),
                         tags$li("avg_glucose_level: average glucose level in blood"),
                         tags$li("bmi: body mass index"),
                         tags$li(
                           "smoking_status: formerly smoked, never smoked, smokes or Unknown"
                         ),
                         tags$li("stroke: 1 if the patient had a stroke or 0 if not")
                       )
                     )
                   ),
                   icon = icon("uncharted")),
          tabPanel(
            "Summary",
            verbatimTextOutput("summary"),
            icon = icon("pen-to-square")
          )
        )
        
      ),
      # Second tab content
      tabItem(
        tabName = "categorial",
        fluidRow(
          box(
            width = 3,
            plotOutput("g1_gender", height = 300),
            tags$p("There are more Female patients than Male.")
          ),
          box(
            width = 3,
            plotOutput("g1_married", height = 300),
            tags$p(
              "Roughly double the amount of patients have been married before than those who have not."
            )
          ),
          box(
            width = 3,
            plotOutput("g1_residence", height = 300),
            tags$p(
              "The patients are nearly evenly distributed between rural and urban residences"
            )
          ),
          box(
            width = 3,
            plotOutput("g1_heart", height = 300),
            tags$p(
              "The gap between patients with and without heart disease more closely resembles the gap between those with and without strokes."
            )
          ),
          box(
            width = 3,
            plotOutput("g1_hyper", height = 300),
            tags$p(
              "The number of patients without hypertension is vastly greater than the number of patients with hypertension, but the gap is slightly less than the gap seen for stroke victims."
            )
          ),
          box(
            width = 3,
            plotOutput("g1_stroke", height = 300),
            tags$p(
              "The number of patients who have not had strokes is vastly greater than the number of patients who have."
            ),
          ),
          box(width = 6,
              plotOutput("g1_age"),
              tags$p("")),
          box(width = 6,
              plotOutput("g1_bmi"),
              tags$p("")),
          box(width = 6,
              plotOutput("g1_glu"),
              tags$p("")),
          box(
            width = 6,
            plotOutput("g1_work"),
            tags$p(
              "There are approximately even amounts of patients that are working government jobs, are self-employed, and are children. The majority of patients work for private companies, and a small number have never worked."
            )
          ),
          box(
            width = 6,
            plotOutput("g1_smoking"),
            tags$p(
              "The unknown data was randomly added to the three categories above based of the probability. Most patients have either never smoked. The data for formerly and currently smokers are similar."
            )
          ),
        )
      ),
      tabItem(
        tabName = "eda",
        fluidRow(
          box(width = 6,
              plotOutput("g2_gender"),
              tags$p("")),
          box(
            width = 6,
            plotOutput("g2_residence"),
            tags$p(
              "There are not many differences in these two graphs for residence type. Perhaps, this variable can be insignificant."
            )
          ),
          box(
            width = 6,
            plotOutput("g2_married"),
            tags$p(
              "The graphs show that married people have a higher rate of stroke than those who are not..."
            )
          ),
          box(
            width = 6,
            plotOutput("g2_hyper"),
            tags$p(
              "Blood pressure that is higher than normal is called hypertension. The graph shows that people with high blood pressure have a higher rate of stroke than people without the disease."
            )
          ),
          box(
            width = 6,
            plotOutput("g2_heart"),
            tags$p(
              "The graphs show that people with heart disease have a higher rate of stroke than people without the disease."
            )
          ),
          box(
            width = 6,
            plotOutput("g2_smoking"),
            tags$p(
              "The 'formerly smoked' and 'smokes' percentages are slightly higher in the stroke = 1 data."
            )
          ),
          box(width = 6,
              plotOutput("g2_work"),
              tags$p("")),
          box(
            width = 6,
            plotOutput("g2_age"),
            tags$p("The older you get the higher the chance of getting stroke.")
          ),
        )
      ),
      tabItem(
        tabName = "lr",
        h2("Logistic Regression"),
        fluidRow(
          box(
            width = 12,
            title = "Data preprocessing",
            verbatimTextOutput("t3_str")
          ),
          box(width = 12,
              dataTableOutput("t3_table")),
          box(
            width = 12,
            title = "Model Summary",
            verbatimTextOutput("modal_summary"),
            uiOutput("equationLogistic"),
          ),
          box(
            width = 12,
            title = "Model Evaluation",
            column(width = 6,
                   verbatimTextOutput("t3_test")),
            column(width = 6,
                   plotOutput("t3_rocr")),
          )
        )
      )
    ),
    tags$script(src = "app.js")
  )
)