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
  
  # Menu Item 3 (Categorical Feature)
  ## Gender Distribution
  output$g1_gender <- renderPlot({
    ggplot(data = df_copy, aes(x = gender, fill = gender)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(values = c(
        wes_palette("GrandBudapest2")[2],
        wes_palette("GrandBudapest2")[3]
      )) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Gender", title = "Gender Distribution")
  })
  
  ## Residence Type Distribution
  output$g1_residence <- renderPlot({
    ggplot(df_copy, aes(x = Residence_type, fill = Residence_type)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(values = c(
        wes_palette("GrandBudapest2")[2],
        wes_palette("GrandBudapest2")[3]
      )) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Residence Type", title = "Residence Type Distribution")
  })
  
  ## Ever Married Distribution
  output$g1_married <- renderPlot({
    ggplot(data = df_copy, aes(x = ever_married, fill = ever_married)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(values = c(
        wes_palette("GrandBudapest2")[2],
        wes_palette("GrandBudapest2")[3]
      )) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Ever Married", title = "Ever Married Distribution")
  })
  
  ## Hypertension Distribution
  output$g1_hyper <- renderPlot({
    ggplot(df_copy, aes(as.factor(hypertension), fill = as.factor(hypertension))) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(values = c(
        wes_palette("GrandBudapest2")[2],
        wes_palette("GrandBudapest2")[3]
      )) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Hypertension", title = "Hypertension Distribution")
  })
  
  ## Heart Disease Distribution
  output$g1_heart <- renderPlot({
    ggplot(df_copy, aes(as.factor(heart_disease), fill = as.factor(heart_disease))) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(values = c(
        wes_palette("GrandBudapest2")[2],
        wes_palette("GrandBudapest2")[3]
      )) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Heart Dsiease", title = "Heart Disease Distribution")
  })
  
  ## Smoking Status Distribution
  output$g1_smoking <- renderPlot({
    ggplot(df_copy, aes(x = smoking_status, fill = smoking_status)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(values = c(
        wes_palette("GrandBudapest2")[1],
        wes_palette("GrandBudapest2")[2],
        wes_palette("GrandBudapest2")[3],
        wes_palette("GrandBudapest2")[4]
      )) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Smoking Status", title = "Smoking Status Distribution")
  })
  
  ## Work Type Distribution
  output$g1_work <- renderPlot({
    ggplot(data = df_copy, aes(x = work_type, fill = work_type)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(
        values = c(
          wes_palette("GrandBudapest2")[1],
          wes_palette("GrandBudapest2")[2],
          wes_palette("GrandBudapest2")[3],
          wes_palette("GrandBudapest2")[4],
          wes_palette("GrandBudapest2")[5]
        )
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Work Type", title = "Work Type Distribution")
  })
  
  ## Stroke Distribution
  output$g1_stroke <- renderPlot({
    ggplot(df_copy, aes(as.factor(stroke), fill = as.factor(stroke))) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
      scale_fill_manual(values = c(
        wes_palette("GrandBudapest2")[2],
        wes_palette("GrandBudapest2")[3]
      )) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "None") +
      labs(y = "Count", x = "Stroke", title = "Stroke Distribution")
  })
  
  ## Age Distribution
  output$g1_age <- renderPlot({
    ggplot(df_copy) +
      geom_histogram(
        data = df_copy,
        aes(x = age),
        fill = wes_palette("IsleofDogs1")[1],
        color = "gray"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 15, face = "bold")) +
      labs(y = "Count", x = "Age (years)", title = "Age Distribution")
  })
  
  ## BMI Distribution
  output$g1_bmi <- renderPlot({
    ggplot(df_copy) +
      geom_histogram(
        data = df_copy,
        aes(x = bmi),
        fill = wes_palette("IsleofDogs1")[1],
        color = "gray"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 15, face = "bold")) +
      labs(y = "Count", x = "BMI", title = "BMI Distribution")
  })
  
  ## Average Glucose Level Distribution
  output$g1_glu <- renderPlot({
    ggplot(df_copy) +
      geom_histogram(
        data = df_copy,
        aes(x = avg_glucose_level),
        fill = wes_palette("IsleofDogs1")[1],
        color = "gray"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 15, face = "bold")) +
      labs(y = "Count", x = "Average Glucose Level", title = "Average Glucose Level Distribution")
  })
  
  # Menu Item 4(Exploratory Data Analysis)
  ##  Gender Distribution by Stroke Status
  output$g2_gender <- renderPlot({
    tbg2_gender <-
      df_copy %>% group_by(gender) %>% count(stroke) %>% mutate(pct = prop.table(n))
    ggplot(tbg2_gender,
           aes(
             x = gender,
             y = pct,
             fill = as.factor(stroke),
             label = scales::percent(pct)
           )) +
      geom_col(position = 'dodge') +
      geom_text(
        position = position_dodge(width = .9),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold")) +
      labs(
        y = "Proportion",
        x = "Gender",
        title = "Gender Distribution by Stroke Status",
        fill = "Stroke"
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  ## Hypertension Distribution by Stroke Status
  output$g2_hyper <- renderPlot({
    tbg2_hyper <-
      df_copy %>% group_by(hypertension) %>% count(stroke) %>% mutate(pct = prop.table(n))
    ggplot(tbg2_hyper,
           aes(
             x = as.factor(hypertension),
             y = pct,
             fill = as.factor(stroke),
             label = scales::percent(pct)
           )) +
      geom_col(position = 'dodge') +
      geom_text(
        position = position_dodge(width = .9),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold")) +
      labs(
        y = "Proportion",
        x = "Hypertension",
        title = "Hypertension Distribution by Stroke Status",
        fill = "Stroke"
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  ## Heart Disease Distribution by Stroke Status
  output$g2_heart <- renderPlot({
    tbg2_heart <-
      df_copy %>% group_by(heart_disease) %>% count(stroke) %>% mutate(pct = prop.table(n))
    ggplot(tbg2_heart,
           aes(
             x = as.factor(heart_disease),
             y = pct,
             fill = as.factor(stroke),
             label = scales::percent(pct)
           )) +
      geom_col(position = 'dodge') +
      geom_text(
        position = position_dodge(width = .9),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold")) +
      labs(
        y = "Proportion",
        x = "Heart Disease",
        title = "Heart Disease Distribution by Stroke Status",
        fill = "Stroke"
      ) +
      scale_y_continuous(labels = scales::percent)
    
  })
  
  ## Ever Married Distribution by Stroke Status
  output$g2_married <- renderPlot({
    tbg2_married <-
      df_copy %>% group_by(ever_married) %>% count(stroke) %>% mutate(pct = prop.table(n))
    ggplot(
      tbg2_married,
      aes(
        x = ever_married,
        y = pct,
        fill = as.factor(stroke),
        label = scales::percent(pct)
      )
    ) +
      geom_col(position = 'dodge') +
      geom_text(
        position = position_dodge(width = .9),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold")) +
      labs(
        y = "Proportion",
        x = "Ever Married",
        title = "Ever Married Distribution by Stroke Status",
        fill = "Stroke"
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  ## Smoking Status Distribution by Stroke Status
  output$g2_smoking <- renderPlot({
    tbg2_smoking <-
      df_copy %>% group_by(smoking_status) %>% count(stroke) %>% mutate(pct = prop.table(n))
    ggplot(
      tbg2_smoking,
      aes(
        x = smoking_status,
        y = pct,
        fill = as.factor(stroke),
        label = scales::percent(pct)
      )
    ) +
      geom_col(position = 'dodge') +
      geom_text(
        position = position_dodge(width = .9),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold")) +
      labs(
        y = "Proportion",
        x = "Smoking Status",
        title = "Smoking Status Distribution by Stroke Status",
        fill = "Stroke"
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  ## Work Type Distribution by Stroke Status
  output$g2_work <- renderPlot({
    tbg2_work <-
      df_copy %>% group_by(work_type) %>% count(stroke) %>% mutate(pct = prop.table(n))
    ggplot(tbg2_work,
           aes(
             x = work_type,
             y = pct,
             fill = as.factor(stroke),
             label = scales::percent(pct)
           )) +
      geom_col(position = 'dodge') +
      geom_text(
        position = position_dodge(width = .9),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold")) +
      labs(
        y = "Proportion",
        x = "Work Type",
        title = "Work Type Distribution by Stroke Status",
        fill = "Stroke"
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  ## Residence Type Distribution by Stroke Status
  output$g2_residence <- renderPlot({
    tbg2_residence <-
      df_copy %>% group_by(Residence_type) %>% count(stroke) %>% mutate(pct = prop.table(n))
    ggplot(
      tbg2_residence,
      aes(
        x = Residence_type,
        y = pct,
        fill = as.factor(stroke),
        label = scales::percent(pct)
      )
    ) +
      geom_col(position = 'dodge') +
      geom_text(
        position = position_dodge(width = .9),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold")) +
      labs(
        y = "Proportion",
        x = "Residence Type",
        title = "Residence Type Distribution by Stroke Status",
        fill = "Stroke"
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  ## Age Distribution by Stroke Status
  output$g2_age <- renderPlot({
    ggplot(df_copy) +
      geom_boxplot(aes(
        x = as.factor(stroke),
        y = age,
        fill = as.factor(stroke)
      )) +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      labs(x = "Stroke", y = "Age (years)", title = "")
  })
  
  ## BMI Distribution by Stroke Status
  output$g2_bmi <- renderPlot({
    ggplot(df_copy) +
      geom_boxplot(aes(
        x = as.factor(stroke),
        y = bmi,
        fill = as.factor(stroke)
      )) +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      labs(x = "Stroke", y = "BMI", title = "")
    
  })
  
  ## Glu Distribution by Stroke Status
  output$g2_glu <- renderPlot({
    ggplot(df_copy) +
      geom_boxplot(aes(
        x = as.factor(stroke),
        y = avg_glucose_level,
        fill = as.factor(stroke)
      )) +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
      labs(x = "Stroke", y = "Average Glucose Level", title = "")
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
  
  output$t3_table <- DT::renderDataTable({
    datatable(df_num, options = list(scrollX = TRUE))
  })
  
  output$t3_str <- renderPrint({
    str(df_num)
  })
  
  output$modal_summary <- renderPrint({
    summary(model)
  })
  
  output$equationLogistic <- renderUI({
    text = extract_eq(
      model,
      use_coefs = TRUE,
      # display coefficients
      wrap = TRUE,
      # multiple lines
      terms_per_line = 5
    )
    withMathJax(tags$p(text))
  })
  
  output$t3_test <- renderPrint({
    confusionMatrix(pred_glm, as.factor(test$stroke))
  })
  
  output$t3_rocr <- renderPlot({
    pred <- prediction(pred_test, test$stroke)
    perf <- performance(pred, "tpr", "fpr")
    plot(perf, colorize = TRUE)
  })
}