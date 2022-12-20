function(input, output) {
  # Rendering table dataset
  output$dataT <- DT::renderDataTable({
    datatable(df_clean, options = list(scrollX = TRUE))
  })
  
  # For Structure output
  output$structure  <- renderPrint({
    str(df_clean)
  })
  
  # For Summary Output
  output$summary <- renderPrint({
    summary(df_clean)
  })
  
  # Chart Description
  ## Gender
  output$g1_gender <- renderPlot({
    g1_gender
  })
  output$g1_residence <- renderPlot({
    g1_residence
  })
  output$g1_married <- renderPlot({
    g1_married
  })
  output$g1_hyper <- renderPlot({
    g1_hyper
  })
  output$g1_heart <- renderPlot({
    g1_heart
  })
  output$g1_smoking <- renderPlot({
    g1_smoking
  })
  output$g1_work <- renderPlot({
    g1_work
  })
  output$g1_stroke <- renderPlot({
    g1_stroke
  })
  output$g1_age <- renderPlot({
    g1_age
  })
  output$g1_bmi <- renderPlot({
    g1_bmi
  })
  output$g1_glu <- renderPlot({
    g1_glu
  })
  
  output$g2_gender <- renderPlot({
    g2_gender
  })
  output$g2_hyper <- renderPlot({
    g2_hyper
  })
  output$g2_heart <- renderPlot({
    g2_heart
  })
  output$g2_married <- renderPlot({
    g2_married
  })
  output$g2_smoking <- renderPlot({
    g2_smoking
  })
  output$g2_work <- renderPlot({
    g2_work
  })
  output$g2_residence <- renderPlot({
    g2_residence
  })
  
  output$g_corrplot <- renderPlot({
    res <- cor(df_num)
    corrplot(
      res,
      type = "upper",
      order = "hclust",
      tl.col = "black",
      tl.srt = 45
    )
  })
  
  output$modal_summary <- renderPrint({
    summary(model)
  })
}
