
eulerWaveAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        titlePanel("Wave Comparison Explorer"),
        sidebarLayout(
            sidebarPanel(
                helpText("Frequency w1 is fixed at 4 Hz"),
                sliderInput(ns("w2"), "Frequency w2 (Hz):", min = 1, max = 20, value = 4, step = 0.1),
                sliderInput(ns("phi"), "Phase Difference (φ):", min = floor(-2*pi), max = ceiling(2*pi), value = 0, step = 0.1)
            ),
            mainPanel(
                plotOutput(ns("waveRealPlot")),
                plotOutput(ns("waveImagPlot"))
            )
        )
    )
}

eulerWaveAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$waveRealPlot <- renderPlot({
            t_seq <- seq(0, 2, length.out = 500)
            w1 <- 4
            w2 <- input$w2
            df <- tibble(
                t = t_seq,
                real1 = cos(2*pi*w1 * t_seq),
                real2 = cos(2*pi*w2 * t_seq + input$phi)
            )
            
            ggplot(df) +
                geom_line(aes(t, real1), color = "blue") +
                geom_line(aes(t, real2), color = "orange") +
                labs(title = "Real Parts of Two Waves", x = "Time (s)", y = "Real") +
                theme_minimal()
        })
        
        output$waveImagPlot <- renderPlot({
            t_seq <- seq(0, 2, length.out = 500)
            w1 <- 4
            w2 <- input$w2
            df <- tibble(
                t = t_seq,
                imag1 = sin(2*pi*w1 * t_seq),
                imag2 = sin(2*pi*w2 * t_seq + input$phi)
            )
            
            ggplot(df) +
                geom_line(aes(t, imag1), color = "blue") +
                geom_line(aes(t, imag2), color = "orange") +
                labs(title = "Imaginary Parts of Two Waves", x = "Time (s)", y = "Imaginary") +
                theme_minimal()
        })
    })
}
