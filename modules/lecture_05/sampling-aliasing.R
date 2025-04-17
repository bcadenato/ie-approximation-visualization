
aliasingAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        titlePanel("Aliasing Visualization"),
        fluidRow(
            column(6,
                   selectInput(ns("signal_type"), "Select Input Signal (10 Hz):",
                               choices = c("Cosine (10 Hz)" = "Cosine", "Square Wave (10 Hz)" = "Square Wave"))
            ),
            column(6,
                   sliderInput(ns("sampling_freq"), "Sampling Frequency (Hz):", min = 1, max = 100, value = 20)
            )
        ),
        fluidRow(
            column(6, plotOutput(ns("input_plot"))),
            column(6, plotOutput(ns("sampled_plot")))
        ),
        fluidRow(
            column(12, plotOutput(ns("recovered_plot")))
        )
    )
}

aliasingAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        sinc <- function(x) {
            y <- rep(0, length(x))
            y[x == 0] <- 1
            y[x != 0] <- sin(pi * x[x != 0]) / (pi * x[x != 0])
            return(y)
        }
        
        time <- reactive({ seq(0, 1, length.out = 1000) })
        
        signal <- reactive({
            t <- time()
            freq <- 10
            switch(input$signal_type,
                   "Cosine" = cos(2 * pi * freq * t),
                   "Square Wave" = sign(sin(2 * pi * freq * t)))
        })
        
        sampled_data <- reactive({
            t <- time()
            y <- signal()
            samp_rate <- input$sampling_freq
            samp_indices <- seq(1, length(t), by = floor(1000 / samp_rate))
            list(times = t[samp_indices], values = y[samp_indices])
        })
        
        recovered_signal <- reactive({
            t <- time()
            sampled <- sampled_data()
            Ts <- 1 / input$sampling_freq
            
            sinc_interp <- function(t, tn, yn, Ts) {
                outer(t, tn, function(t, tn) sinc((t - tn) / Ts)) %*% yn
            }
            
            as.vector(sinc_interp(t, sampled$times, sampled$values, Ts))
        })
        
        output$input_plot <- renderPlot({
            tibble(t = time(), y = signal()) %>%
                ggplot(aes(x = t, y = y)) +
                geom_line(color = "blue") +
                labs(title = "Input Signal", x = "Time", y = "Amplitude") +
                theme_minimal()
        })
        
        output$sampled_plot <- renderPlot({
            full <- tibble(t = time(), y = signal())
            sampled <- sampled_data()
            ggplot(full, aes(x = t, y = y)) +
                geom_line(color = "gray") +
                geom_point(data = tibble(t = sampled$times, y = sampled$values),
                           aes(x = t, y = y), color = "orange") +
                geom_segment(data = tibble(t = sampled$times, y = sampled$values),
                             aes(x = t, xend = t, y = 0, yend = y), color = "orange") +
                labs(title = "Sampled Signal", x = "Time", y = "Amplitude") +
                theme_minimal()
        })
        
        output$recovered_plot <- renderPlot({
            tibble(t = time(), y = recovered_signal()) %>%
                ggplot(aes(x = t, y = y)) +
                geom_line(color = "green") +
                labs(title = "Recovered Signal (Ideal Filter)", x = "Time", y = "Amplitude") +
                theme_minimal()
        })
    })
}
