# Project: ICU Early-Warning System
# Author: AGU Ekeoma Emmanuel (PG/24/0869)
# COSC 848 Programming for Health Informatics

library(shiny)
library(ggplot2)

source("engine.R") # Load our numerical logic

ui <- fluidPage(
  titlePanel("ICU Early-Warning System (Sepsis Prediction)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Monitoring physiological deterioration using numerical analysis."),
      sliderInput("window", "Prediction Look-back Window (Hours):", 1, 48, 24)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vital Sign Trends", 
                 plotOutput("hrPlot"),
                 plotOutput("mapPlot")),
        tabPanel("Numerical Derivatives", 
                 plotOutput("slopePlot")),
        tabPanel("Wavelet Analysis", 
                 plotOutput("waveletPlot"),
                 helpText("High energy peaks indicate sudden physiological instability (Autonomic stress).")),
        tabPanel("Dimensionality Reduction (PCA)", 
                 plotOutput("pcaPlot"),
                 helpText("The arrows show which variables are driving the patient's deterioration. 
         If HR_Slope and Wavelet_Energy point in the same direction, they are highly correlated.")),
        tabPanel("Sepsis Prediction",
                 h3("Early Warning Probability"),
                 plotOutput("probPlot"),
                 hr(),
                 uiOutput("riskStatus")),
        tabPanel("Statistical Validation",
                 h3("Model Performance (Numerical Integration)"),
                 plotOutput("rocPlot"),
                 h4(uiOutput("aucText")),
                 helpText("The AUC is calculated using the Trapezoidal Rule, integrating the area under the ROC curve."))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive data processing
  data <- reactive({
    generate_patient_data(input$window) %>%
      calculate_derivatives() %>%
      apply_smoothing() %>% 
      calculate_wavelets() %>% 
      predict_sepsis()
  })
  
  # Plot Heart Rate (Raw vs Smoothed)
  output$hrPlot <- renderPlot({
    ggplot(data(), aes(x = time)) +
      geom_point(aes(y = hr), alpha = 0.5) +
      geom_line(aes(y = hr_smooth), color = "red", size = 1) +
      labs(title = "Heart Rate (BPM) - Raw vs Savitzky-Golay Smooth", y = "HR") +
      theme_minimal()
  })
  
  # Plot MAP
  output$mapPlot <- renderPlot({
    ggplot(data(), aes(x = time)) +
      geom_point(aes(y = map), alpha = 0.5) +
      geom_line(aes(y = map_smooth), color = "blue", size = 1) +
      labs(title = "Mean Arterial Pressure (mmHg)", y = "MAP") +
      theme_minimal()
  })
  
  # Plot Derivative (Rate of Change)
  output$slopePlot <- renderPlot({
    ggplot(data(), aes(x = time)) +
      geom_area(aes(y = hr_slope), fill = "orange", alpha = 0.3) +
      labs(title = "Numerical Derivative of HR (Velocity of Change)", y = "ΔHR/Δt") +
      theme_minimal()
  })
  
  # Add the Wavelet Plot
  output$waveletPlot <- renderPlot({
    ggplot(data(), aes(x = time, y = wavelet_energy)) +
      geom_line(color = "purple", size = 1) +
      geom_area(fill = "purple", alpha = 0.2) +
      labs(title = "Wavelet Energy (Level D1) - Heart Rate Instability", 
           y = "Energy Coefficient", x = "Hours") +
      theme_minimal()
  })
  
  # Add PCA plot
  output$pcaPlot <- renderPlot({
    # Get the PCA result from our data
    df_raw <- data()
    pca_obj <- calculate_pca(df_raw)
    
    if(is.null(pca_obj)) return(NULL)
    
    # 1. Extract the scores (where the patient is in PCA space)
    scores <- as.data.frame(pca_obj$x)
    
    # 2. Extract the loadings (the arrows/variables)
    loadings <- as.data.frame(pca_obj$rotation)
    loadings$var <- rownames(loadings)
    
    # Create the Biplot
    ggplot() +
      # Draw the patient's path through PCA space (PC1 vs PC2)
      geom_path(data = scores, aes(x = PC1, y = PC2), color = "gray", alpha = 0.5) +
      geom_point(data = scores, aes(x = PC1, y = PC2, color = 1:nrow(scores))) +
      # Draw the arrows representing our numerical features
      geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1*3, yend = PC2*3), 
                   arrow = arrow(length = unit(0.2, "cm")), color = "red") +
      geom_text(data = loadings, aes(x = PC1*3.5, y = PC2*3.5, label = var), color = "red") +
      scale_color_gradient(low = "blue", high = "red", name = "Hour") +
      labs(title = "PCA Biplot: Drivers of Deterioration",
           subtitle = "Red arrows indicate the 'weight' of each numerical feature",
           x = "PC1 (Severity Index)", y = "PC2 (Compensatory State)") +
      theme_minimal()
  })
  
  # Plot the Probability over time
  output$probPlot <- renderPlot({
    ggplot(data(), aes(x = time, y = probability)) +
      geom_line(color = "darkgreen", size = 1.5) +
      geom_hline(yintercept = 0.7, linetype = "dashed", color = "red") +
      ylim(0, 1) +
      labs(title = "Predicted Probability of Sepsis (Next 6 Hours)",
           y = "Probability (0 to 1)", x = "Hours") +
      theme_minimal()
  })
  
  # Status Text
  output$riskStatus <- renderUI({
    df <- data()
    # Look at the last 6 hours of probability
    recent_probs <- tail(df$probability, 6)
    max_recent_risk <- max(recent_probs, na.rm = TRUE)
    
    if(max_recent_risk > 0.7) {
      tags$div(style="background-color:#721c24; color:#f8d7da; padding:20px; text-align:center; border-radius:10px; border: 5px solid red;",
               h2("CRITICAL ALERT: SEPSIS RISK DETECTED"),
               p(paste("Highest Risk in last 6h:", round(max_recent_risk * 100, 1), "%")))
    } else {
      tags$div(style="background-color:#155724; color:#d4edda; padding:20px; text-align:center; border-radius:10px;",
               h2("PATIENT STABLE"),
               p("Continuous Monitoring Active"))
    }
  })
  
  output$rocPlot <- renderPlot({
    df <- data()
    # Create synthetic "Ground Truth": 
    # Let's say any patient with a true HR > 95 for more than 3 hours is a "1"
    truth <- ifelse(max(df$hr) > 95, 1, 0)
    
    # Since we only have one patient in the simulation, we'll simulate 
    # a small batch of 10 virtual patients for the ROC curve
    set.seed(42)
    sim_probs <- c(df$probability, runif(50, 0.1, 0.6)) # mix current and random
    sim_truth <- c(rep(truth, nrow(df)), rep(0, 50))
    
    roc_data <- calculate_auc(sim_probs, sim_truth)
    
    ggplot(data.frame(fpr = roc_data$fpr, tpr = roc_data$tpr), aes(x = fpr, y = tpr)) +
      geom_line(color = "blue", size = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Receiver Operating Characteristic (ROC) Curve",
           subtitle = "Integrated via Trapezoidal Rule") +
      theme_minimal()
  })
  
  output$aucText <- renderUI({
    # (Using the same simulation logic as above for the text)
    df <- data()
    truth <- ifelse(max(df$hr) > 95, 1, 0)
    sim_probs <- c(df$probability, runif(50, 0.1, 0.6))
    sim_truth <- c(rep(truth, nrow(df)), rep(0, 50))
    roc_data <- calculate_auc(sim_probs, sim_truth)
    
    paste("Calculated AUC: ", round(roc_data$auc, 4))
  })
}

shinyApp(ui = ui, server = server)
