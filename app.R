library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# Helper Functions
generate_correlated_rng <- function(n, correlation = 0.9) {
  x <- numeric(n)
  x[1] <- runif(1)
  for(i in 2:n) {
    x[i] <- correlation * x[i-1] + (1 - correlation) * runif(1)
  }
  x
}

generate_randu <- function(n, seed = 1) {
  x <- numeric(n)
  x[1] <- seed
  
  # RANDU algorithm: X_(n+1) = (65539 * X_n) mod 2^31
  for(i in 2:n) {
    x[i] <- (65539 * x[i-1]) %% 2^31
  }
  
  # Normalize to [0,1] interval
  x / 2^31
}

# UI Definition
ui <- page_sidebar(
  title = "RNG Quality Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar
  sidebar = sidebar(
    conditionalPanel(
      condition = "input.tabset === 'Distribution Analysis' || 
                   input.tabset === 'Statistical Tests' || 
                   input.tabset === 'Randomness Tests' || 
                   input.tabset === 'Successive Triplets' ",
      selectInput("rng_type", "RNG Type", 
                 choices = c("Good RNG (runif)" = "runif", 
                           "Good RNG (rnorm)" = "rnorm", 
                           "Good RNG (rexp)" = "rexp",
                           "Poor RNG (Correlated)" = "correlated",
                           "Poor RNG (RANDU)" = "randu")),
      numericInput("n_samples", "Number of Samples", 
                  value = 1000, min = 100, max = 10000),
      conditionalPanel(
        condition = "input.rng_type == 'correlated'",
        sliderInput("correlation", "Correlation Strength",
                   min = 0, max = 1, value = 0.9, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.rng_type == 'randu'",
        numericInput("randu_seed", "RANDU Seed", 
                    value = 1, min = 1)
      ),
      actionButton("generate", "Generate New Data", class = "btn-primary w-100")
    )
  ),
  
  # Main content area with tabs
  navset_tab(
    id = "tabset",
    
    # Distribution Analysis Tab
    nav_panel(
      title = "Distribution Analysis",
      card(
        card_header("About This Tab"),
        "This tab helps you analyze the distribution of random numbers to assess their quality. A good RNG should show:",
        tags$ul(
          tags$li(strong("Uniform or Expected Distribution: "), "The histogram should match the expected probability distribution (uniform, normal, or exponential)"),
          tags$li(strong("No Patterns in Sequence: "), "The sequence plot should show no obvious patterns or trends"),
          tags$li(strong("Consistent Statistics: "), "Mean and standard deviation should align with theoretical values")
        ),
        "Compare the visualizations between good and poor RNGs to see the differences."
      ),
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Sample Size",
          value = textOutput("sample_size"),
          theme = "primary"
        ),
        value_box(
          title = "Mean",
          value = textOutput("mean_value"),
          theme = "secondary"
        ),
        value_box(
          title = "Standard Deviation",
          value = textOutput("sd_value"),
          theme = "info"
        )
      ),
      card(
        card_header("Distribution Plot"),
        "The histogram and density curve show how the random numbers are distributed. For uniform RNG:",
        tags$ul(
          tags$li("Expect a roughly flat histogram"),
          tags$li("The density curve (red line) should be approximately horizontal"),
          tags$li("No significant peaks or valleys should be present")
        ),
        plotOutput("dist_plot")
      ),
      card(
        card_header("Sequence Plot"),
        "This plot shows the random numbers in the order they were generated:",
        tags$ul(
          tags$li("Good RNGs show no discernible patterns"),
          tags$li("Look for unwanted trends, cycles, or clustering"),
          tags$li("Points should appear randomly scattered")
        ),
        plotOutput("sequence_plot")
      )
    ),
    
    # Statistical Tests Tab
    nav_panel(
      title = "Statistical Tests",
      card(
        card_header("About This Tab"),
        "This tab provides formal statistical tests to evaluate RNG quality:",
        tags$ul(
          tags$li(strong("Kolmogorov-Smirnov Test: "), "Compares the distribution to theoretical expectations"),
          tags$li(strong("Chi-Square Test: "), "Checks if values are evenly distributed across intervals"),
          tags$li(strong("Q-Q Plot: "), "Visually compares the distribution to theoretical quantiles")
        )
      ),
      layout_columns(
        card(
          card_header("Kolmogorov-Smirnov Test"),
          "Tests if the sample follows the expected distribution:",
          tags$ul(
            tags$li("High p-value (>0.05): Sample matches expected distribution"),
            tags$li("Low p-value (<0.05): Sample differs from expected distribution")
          ),
          textOutput("ks_test")
        ),
        card(
          card_header("Chi-Square Test"),
          "Tests if values are uniformly distributed across intervals:",
          tags$ul(
            tags$li("High p-value: Even distribution"),
            tags$li("Low p-value: Uneven distribution")
          ),
          textOutput("chi_test")
        )
      ),
      card(
        card_header("QQ Plot"),
        "Compares sample quantiles to theoretical quantiles:",
        tags$ul(
          tags$li("Points should follow the diagonal line"),
          tags$li("Deviations indicate departure from expected distribution"),
          tags$li("S-shaped curves suggest skewness")
        ),
        plotOutput("qq_plot")
      )
    ),
    
    # Randomness Tests Tab
    nav_panel(
      title = "Randomness Tests",
      card(
        card_header("About This Tab"),
        "This tab focuses on detecting patterns and dependencies in the random numbers:",
        tags$ul(
          tags$li(strong("Runs Test: "), "Detects if values are truly independent"),
          tags$li(strong("Autocorrelation: "), "Measures correlation between sequential values"),
          tags$li(strong("Sequential Scatter: "), "Visualizes relationships between consecutive numbers")
        )
      ),
      layout_columns(
        card(
          card_header("Runs Test"),
          "Analyzes the sequence of values above and below the median:",
          tags$ul(
            tags$li("Too few runs: Values are positively correlated"),
            tags$li("Too many runs: Values are negatively correlated"),
            tags$li("Expected: Random mix of runs above/below median")
          ),
          textOutput("runs_test")
        ),
        card(
          card_header("Autocorrelation"),
          "Shows correlation between values at different lags:",
          tags$ul(
            tags$li("Bars should stay within blue dashed lines"),
            tags$li("Significant bars indicate serial correlation"),
            tags$li("Pattern in bars suggests cyclic behavior")
          ),
          plotOutput("acf_plot")
        )
      ),
      card(
        card_header("Scatter Plot of Sequential Values"),
        "Plots each number against the next in sequence:",
        tags$ul(
          tags$li("Good RNGs show random cloud of points"),
          tags$li("Diagonal pattern indicates correlation"),
          tags$li("Clusters suggest non-randomness")
        ),
        plotOutput("sequential_scatter")
      )
    ),
    
    # Successive Triplets Tab
    nav_panel(
      title = "Successive Triplets",
      card(
        card_header("About This Tab"),
        "This visualization shows triplets of successive random numbers in 3D space:",
        tags$ul(
          tags$li("Each point represents three consecutive numbers from the RNG"),
          tags$li("RANDU shows a distinctive pattern where points lie on 15 parallel planes"),
          tags$li("Good RNGs should show points distributed throughout the space")
        )
      ),
      layout_columns(
        card(
          card_header("3D Visualization"),
          plotlyOutput("plot_3d", height = "600px")
        ),
        card(
          card_header("Visualization Controls"),
          sliderInput("point_opacity", "Point Opacity",
                     min = 0.1, max = 1, value = 0.7, step = 0.1),
          sliderInput("point_size", "Point Size",
                     min = 1, max = 8, value = 4, step = 1)
        )
      ),
      card(
        card_header("Interpretation Guide"),
        "What to look for:",
        tags$ul(
          tags$li(strong("Good RNGs: "), "Points should fill the 3D space uniformly"),
          tags$li(strong("RANDU: "), "Points will align on parallel planes"),
          tags$li(strong("Correlated RNG: "), "Points may cluster along the diagonal or show other patterns"),
          tags$li("Rotate the plot to better see the structures in the data")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values for generated data
  rv <- reactiveVal(NULL)
  rv_3d <- reactiveVal(NULL)
  rv_triplets <- reactiveVal(NULL)
  
  # Generate new data when button is clicked
  observeEvent(input$generate, {
    n <- input$n_samples
    data <- switch(input$rng_type,
                  "runif" = runif(n),
                  "rnorm" = rnorm(n),
                  "rexp" = rexp(n),
                  "correlated" = generate_correlated_rng(n, input$correlation),
                  "randu" = generate_randu(n, input$randu_seed))
    rv(data)
  })
  
  # Generate new data for 3D comparison
  observeEvent(input$generate_3d, {
    n <- input$n_samples_3d
    
    rng1 <- switch(input$rng1_type,
                  "runif" = runif(n),
                  "rnorm" = rnorm(n),
                  "rexp" = rexp(n),
                  "correlated" = generate_correlated_rng(n, input$correlation_3d),
                  "randu" = generate_randu(n, input$randu_seed_3d))
    
    rng2 <- switch(input$rng2_type,
                  "runif" = runif(n),
                  "rnorm" = rnorm(n),
                  "rexp" = rexp(n),
                  "correlated" = generate_correlated_rng(n, input$correlation_3d),
                  "randu" = generate_randu(n, input$randu_seed_3d))
    
    data_3d <- data.frame(
      x = rng1,
      y = rng2,
      z = 1:n,
      time = 1:n
    )
    
    rv_3d(data_3d)
  })

  
  # Generate triplets of successive numbers
  observeEvent(input$generate, {
    n <- input$n_samples
    
    # Generate initial sequence
    data <- switch(input$rng_type,
                  "runif" = runif(n + 2),  # Generate 2 extra for triplets
                  "rnorm" = rnorm(n + 2),
                  "rexp" = rexp(n + 2),
                  "correlated" = generate_correlated_rng(n + 2, input$correlation),
                  "randu" = generate_randu(n + 2, input$randu_seed))
    
    # Create triplets
    triplets <- data.frame(
      x = head(data, -2),
      y = data[2:(length(data)-1)],
      z = tail(data, -2)
    )
    
    rv_triplets(triplets)
  })
  

  # Summary statistics
  output$sample_size <- renderText({
    req(rv())
    length(rv())
  })
  
  output$mean_value <- renderText({
    req(rv())
    round(mean(rv()), 4)
  })
  
  output$sd_value <- renderText({
    req(rv())
    round(sd(rv()), 4)
  })
  
  # Distribution plot
  output$dist_plot <- renderPlot({
    req(rv())
    ggplot(data.frame(x = rv()), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red") +
      theme_minimal() +
      labs(title = "Distribution of Generated Numbers",
           x = "Value",
           y = "Density")
  })
  
  # Sequence plot
  output$sequence_plot <- renderPlot({
    req(rv())
    df <- data.frame(
      index = 1:length(rv()),
      value = rv()
    )
    ggplot(df, aes(x = index, y = value)) +
      geom_line(color = "steelblue", alpha = 0.7) +
      geom_point(alpha = 0.5) +
      theme_minimal() +
      labs(title = "Sequence of Generated Numbers",
           x = "Index",
           y = "Value")
  })
  
  # QQ plot
  output$qq_plot <- renderPlot({
    req(rv())
    ggplot(data.frame(x = rv()), aes(sample = x)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = "Q-Q Plot",
           x = "Theoretical Quantiles",
           y = "Sample Quantiles")
  })
    
  # Statistical tests (continued)
  output$ks_test <- renderText({
    req(rv())
    test <- switch(input$rng_type,
                  "runif" = ks.test(rv(), "punif"),
                  "rnorm" = ks.test(rv(), "pnorm"),
                  "rexp" = ks.test(rv(), "pexp"),
                  "correlated" = ks.test(rv(), "punif"),
                  "randu" = ks.test(rv(), "punif"))
    paste("p-value:", round(test$p.value, 4))
  })
  
  output$chi_test <- renderText({
    req(rv())
    breaks <- seq(min(rv()), max(rv()), length.out = 11)
    observed <- table(cut(rv(), breaks))
    expected <- rep(length(rv())/10, 10)
    test <- chisq.test(observed, p = expected/sum(expected))
    paste("p-value:", round(test$p.value, 4))
  })
  
  # Runs test
  output$runs_test <- renderText({
    req(rv())
    median_runs <- median(rv())
    runs <- rle(rv() > median_runs)
    n_runs <- length(runs$lengths)
    paste("Number of runs:", n_runs)
  })
  
  # Autocorrelation plot
  output$acf_plot <- renderPlot({
    req(rv())
    acf(rv(), main = "Autocorrelation Function", lag.max = 20)
  })
  
  # Sequential scatter plot
  output$sequential_scatter <- renderPlot({
    req(rv())
    df <- data.frame(
      current = head(rv(), -1),
      next_value = tail(rv(), -1)
    )
    ggplot(df, aes(x = current, y = next_value)) +
      geom_point(alpha = 0.5, color = "steelblue") +
      theme_minimal() +
      labs(title = "Sequential Value Correlation",
           x = "Current Value",
           y = "Next Value")
  })

  # 3D plot for triplets
  output$plot_3d <- renderPlotly({
    req(rv_triplets())
    
    data <- rv_triplets()
    
    plot_ly(data, x = ~x, y = ~y, z = ~z,
            type = 'scatter3d', 
            mode = 'markers',
            marker = list(
              size = input$point_size,
              opacity = input$point_opacity
            )) %>%
      layout(scene = list(
        xaxis = list(title = "X(n)"),
        yaxis = list(title = "X(n+1)"),
        zaxis = list(title = "X(n+2)")
      ),
      title = paste("Successive Triplets -", input$rng_type)) %>%
      add_annotations(
        text = "Drag to rotate. Scroll to zoom.",
        xref = "paper", yref = "paper",
        x = 0, y = 1,
        showarrow = FALSE
      )
  })
  
}

# Run the app
shinyApp(ui, server)