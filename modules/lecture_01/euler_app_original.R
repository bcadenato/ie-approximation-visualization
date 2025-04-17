library(shiny)
library(ggplot2)
library(tidyverse)

pi_breaks <- seq(-4*pi, 4*pi, by = pi/2)
pi_labels <- sapply(pi_breaks, function(x) {
    if (x == 0) return("0")
    n <- x / pi
    if (n == 1) return("\u03c0")
    if (n == -1) return("-\u03c0")
    return(paste0(n, "\u03c0"))
})

ui <- fluidPage(
    titlePanel("Exploring Euler's Equation and Exponentials"),
    tabsetPanel(
        tabPanel("Euler's Formula",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("theta", "\u03b8 (radians):",
                                     min = floor( -4 * pi), max = ceiling( 4 * pi), value = 0, step = 0.01)
                     ),
                     mainPanel(
                         plotOutput("unitCirclePlot"),
                         plotOutput("realPartPlot"),
                         plotOutput("imagPartPlot")
                     )
                 )
        ),
        tabPanel("Wave Comparison",
                 sidebarLayout(
                     sidebarPanel(
                         helpText("Frequency w1 is fixed at 4 Hz"),
                         sliderInput("w2", "Frequency w2 (Hz):",
                                     min = 1, max = 20, value = 4, step = 0.1),
                         sliderInput("phi", "Phase Difference (\u03c6):",
                                     min = floor(-2*pi), max = ceiling(2*pi), value = 0, step = 0.1)
                     ),
                     mainPanel(
                         plotOutput("waveRealPlot"),
                         plotOutput("waveImagPlot")
                     )
                 )
        )
    )
)

server <- function(input, output, session) {
    
    output$unitCirclePlot <- renderPlot({
        theta <- input$theta
        df <- tibble(x = cos(theta), y = sin(theta))
        circle <- tibble(t = seq(0, 2*pi, length.out = 500), x = cos(t), y = sin(t))
        
        ggplot() +
            geom_path(data = circle, aes(x, y), color = "grey") +
            geom_point(data = df, aes(x, y), color = "blue", size = 4) +
            geom_segment(data = df, aes(x = 0, y = 0, xend = x, yend = 0), color = "blue", linetype = "solid") +
            geom_segment(data = df, aes(x = 0, y = 0, xend = 0, yend = y), color = "orange", linetype = "solid") +
            geom_segment(data = df, aes(x = 0, y = y, xend = x, yend = y), color = "blue", linetype = "dotted") +
            geom_segment(data = df, aes(x = x, y = 0, xend = x, yend = y), color = "orange", linetype = "dotted") +
            coord_fixed() +
            labs(title = "Unit Circle", x = "Real", y = "Imaginary") +
            theme_minimal()
    })
    
    output$realPartPlot <- renderPlot({
        t_seq <- seq(-4*pi, 4*pi, length.out = 500)
        df <- tibble(t = t_seq, real = cos(t))
        
        ggplot(df, aes(t, real)) +
            geom_line(color = "blue") +
            geom_vline(xintercept = input$theta, linetype = "dashed", color = "red") +
            scale_x_continuous(breaks = pi_breaks, labels = pi_labels) +
            labs(title = expression("Real Part of "*e^{i*theta}), x = "Time", y = "Real") +
            theme_minimal()
    })
    
    output$imagPartPlot <- renderPlot({
        t_seq <- seq(-4*pi, 4*pi, length.out = 500)
        df <- tibble(t = t_seq, imag = sin(t))
        
        ggplot(df, aes(t, imag)) +
            geom_line(color = "orange") +
            geom_vline(xintercept = input$theta, linetype = "dashed", color = "red") +
            scale_x_continuous(breaks = pi_breaks, labels = pi_labels) +
            labs(title = expression("Imaginary Part of "*e^{i*theta}), x = "Time", y = "Real") +
            theme_minimal()
    })
    
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
            geom_line(aes(t, real1), color = "blue", linetype = "solid") +
            geom_line(aes(t, real2), color = "orange", linetype = "solid") +
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
            geom_line(aes(t, imag1), color = "blue", linetype = "solid") +
            geom_line(aes(t, imag2), color = "orange", linetype = "solid") +
            labs(title = "Imaginary Parts of Two Waves", x = "Time (s)", y = "Imaginary") +
            theme_minimal()
    })
}

shinyApp(ui, server)
