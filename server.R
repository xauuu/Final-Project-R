library(DT)

df <- read.csv('dataset.csv')
# remove outlier values
df_clean <- df %>% filter(df$gender != 'Other')
df_clean = df_clean %>% filter(df_clean$bmi != 'N/A')

df_clean = na.omit(df_clean)
server <- function(input, output) {
  
  # Rendering table dataset
  output$dataT <- DT::renderDataTable({
    datatable(head(df_clean), options = list(scrollX = TRUE))
  })
  
  # For Structure output
  output$structure  <- renderPrint({
    str(df_clean)
  })
  
  # For Summary Output
  output$summary <- renderPrint({
    summary(df_clean)
  })
}