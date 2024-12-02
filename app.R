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
                 choices = c("runif (Good RNG)" = "runif", 
                           "rnorm (Good RNG)" = "rnorm", 
                           "rexp (Good RNG)" = "rexp",
                           "Correlated (Poor RNG)" = "correlated",
                           "RANDU (Poor RNG)" = "randu")),
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
        "The histogram and density curve show how the random numbers are distributed. Expected patterns depend on the chosen RNG type:",
        tags$ul(
          tags$li(strong("Uniform (runif): "), "Expect a flat histogram with approximately horizontal density curve"),
          tags$li(strong("Normal (rnorm): "), "Expect a bell-shaped curve centered at 0"),
          tags$li(strong("Exponential (rexp): "), "Expect a curve that peaks at 0 and decays exponentially"),
          tags$li(strong("Poor RNGs: "), "May show unexpected patterns, clusters, or deviations from these expected shapes")
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
        "Compares sample quantiles against theoretical quantiles. The interpretation depends on your RNG type:",
        tags$ul(
          tags$li(strong("Uniform (runif): "), "Points should follow a straight line from (0,0) to (1,1)"),
          tags$li(strong("Normal (rnorm): "), "Points should follow a straight line when plotted against normal quantiles"),
          tags$li(strong("Exponential (rexp): "), "Points should follow a curved line matching exponential quantiles"),
          tags$li(strong("Poor RNGs: "), "May show systematic deviations from expected patterns, such as:"),
          tags$ul(
            tags$li("S-shaped curves indicating skewness"),
            tags$li("Curved tails indicating heavy or light tails"),
            tags$li("Step patterns indicating discretization or clustering")
          )
        ),
        "Note: The grey reference line shows the ideal theoretical relationship.", 
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
# Server logic
server <- function(input, output, session) {
  # Store ALL RNG parameters in a reactive value
  rng_params <- reactiveValues(
    type = NULL,
    n = NULL,
    correlation = NULL,
    randu_seed = NULL,
    data = NULL,
    triplets = NULL
  )
  
  
  # Generate new data when button is clicked
  observeEvent(input$generate, {
    # Update all parameters
    rng_params$type <- input$rng_type
    rng_params$n <- input$n_samples
    rng_params$correlation <- input$correlation
    rng_params$randu_seed <- input$randu_seed
    
    # Generate new data
    rng_params$data <- switch(rng_params$type,
      "runif" = runif(rng_params$n),
      "rnorm" = rnorm(rng_params$n),
      "rexp" = rexp(rng_params$n),
      "correlated" = generate_correlated_rng(rng_params$n, rng_params$correlation),
      "randu" = generate_randu(rng_params$n, rng_params$randu_seed)
    )
    
    # Generate triplets
    data_extended <- switch(rng_params$type,
      "runif" = runif(rng_params$n + 2),
      "rnorm" = rnorm(rng_params$n + 2),
      "rexp" = rexp(rng_params$n + 2),
      "correlated" = generate_correlated_rng(rng_params$n + 2, rng_params$correlation),
      "randu" = generate_randu(rng_params$n + 2, rng_params$randu_seed)
    )
    
    rng_params$triplets <- data.frame(
      x = head(data_extended, -2),
      y = data_extended[2:(length(data_extended)-1)],
      z = tail(data_extended, -2)
    )
  })
  
  # Summary statistics
  output$sample_size <- renderText({
    req(rng_params$data)
    length(rng_params$data)
  })
  
  output$mean_value <- renderText({
    req(rng_params$data)
    round(mean(rng_params$data), 4)
  })
  
  output$sd_value <- renderText({
    req(rng_params$data)
    round(sd(rng_params$data), 4)
  })
  
  # Distribution plot
  output$dist_plot <- renderPlot({
    req(rng_params$data)
    ggplot(data.frame(x = rng_params$data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red") +
      theme_minimal() +
      labs(title = "Distribution of Generated Numbers",
           x = "Value",
           y = "Density")
  })
  
  # Sequence plot
  output$sequence_plot <- renderPlot({
    req(rng_params$data)
    df <- data.frame(
      index = 1:length(rng_params$data),
      value = rng_params$data
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
    req(rng_params$data)
    data <- data.frame(x = rng_params$data)
    
    p <- ggplot(data, aes(sample = x)) +
      theme_minimal() +
      labs(title = "Q-Q Plot",
           x = "Theoretical Quantiles",
           y = "Sample Quantiles")
    
    p <- switch(rng_params$type,
      "runif" = p + stat_qq(distribution = qunif) + 
                stat_qq_line(distribution = qunif),
      "rnorm" = p + stat_qq(distribution = qnorm) + 
                stat_qq_line(distribution = qnorm),
      "rexp" = p + stat_qq(distribution = qexp) + 
               stat_qq_line(distribution = qexp),
      p + stat_qq(distribution = qunif) + 
          stat_qq_line(distribution = qunif)
    )
    
    p
  })
  
  # Statistical tests
  output$ks_test <- renderText({
    req(rng_params$data)
    test <- switch(rng_params$type,
      "runif" = ks.test(rng_params$data, "punif"),
      "rnorm" = ks.test(rng_params$data, "pnorm"),
      "rexp" = ks.test(rng_params$data, "pexp"),
      "correlated" = ks.test(rng_params$data, "punif"),
      "randu" = ks.test(rng_params$data, "punif"))
    paste("p-value:", round(test$p.value, 4))
  })
  
  output$chi_test <- renderText({
    req(rng_params$data)
    breaks <- seq(min(rng_params$data), max(rng_params$data), length.out = 11)
    observed <- table(cut(rng_params$data, breaks))
    expected <- rep(length(rng_params$data)/10, 10)
    test <- chisq.test(observed, p = expected/sum(expected))
    paste("p-value:", round(test$p.value, 4))
  })
  
  # Runs test
  output$runs_test <- renderText({
    req(rng_params$data)
    median_runs <- median(rng_params$data)
    runs <- rle(rng_params$data > median_runs)
    n_runs <- length(runs$lengths)
    paste("Number of runs:", n_runs)
  })
  
  # Autocorrelation plot
  output$acf_plot <- renderPlot({
    req(rng_params$data)
    acf(rng_params$data, main = "Autocorrelation Function", lag.max = 20)
  })
  
  # Sequential scatter plot
  output$sequential_scatter <- renderPlot({
    req(rng_params$data)
    df <- data.frame(
      current = head(rng_params$data, -1),
      next_value = tail(rng_params$data, -1)
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
    req(rng_params$triplets)
    
    plot_ly(rng_params$triplets, x = ~x, y = ~y, z = ~z,
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
      title = paste("Successive Triplets -", rng_params$type)) %>%
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