# modules/lecture_07/fourier-transform-blocks-app.R

library(shiny)
library(tidyverse)
library(signal)

generate_signal <- function(type, T, t) {
    period <- max(t) - min(t)
    switch(type,
           "Sin(T)" = sin(2 * pi * t / period),
           "Sin(T/2)" = sin(4 * pi * t / period),
           "Sin(T/4)" = sin(8 * pi * t / period),
           "Square(T)" = sign(sin(2 * pi * t / period)),
           "Square(T/2)" = sign(sin(4 * pi * t / period)),
           "Square(T/4)" = sign(sin(8 * pi * t / period)),
           "Square Pulse" = ifelse(abs(t - mean(t)) < period/10, 1, 0),
           rep(0, length(t)))
}

fourierTransformBlocksAppUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        titlePanel("Fourier Transform Visualization"),
        fluidRow(
            column(3, selectInput(ns("block1"), "Block 1", choices = signal_choices())),
            column(3, selectInput(ns("block2"), "Block 2", choices = signal_choices())),
            column(3, selectInput(ns("block3"), "Block 3", choices = signal_choices())),
            column(3, selectInput(ns("block4"), "Block 4", choices = signal_choices()))
        ),
        fluidRow(
            column(6, plotOutput(ns("timePlot"))),
            column(6, plotOutput(ns("freqPlot"), brush = brushOpts(id = ns("freqBrush")),
                                 hover = hoverOpts(id = ns("freqHover"))))
        ),
        fluidRow(
            column(12, plotOutput(ns("zoomedFreqPlot"), hover = hoverOpts(id = ns("zoomHover"))))
        ),
        verbatimTextOutput(ns("hoverInfo"))
    )
}

signal_choices <- function() {
    c("Sin(T)", "Sin(T/2)", "Sin(T/4)", "Square(T)", "Square(T/2)", "Square(T/4)", "No Signal", "Square Pulse")
}

fourierTransformBlocksAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        T <- 1
        N <- 1000
        t <- seq(0, 4*T, length.out = 4*N)
        
        compute_signal <- reactive({
            list(
                s1 = generate_signal(input$block1, T, t[1:N]),
                s2 = generate_signal(input$block2, T, t[(N+1):(2*N)]),
                s3 = generate_signal(input$block3, T, t[(2*N+1):(3*N)]),
                s4 = generate_signal(input$block4, T, t[(3*N+1):(4*N)])
            )
        })
        
        full_signal <- reactive({
            s <- compute_signal()
            c(s$s1, s$s2, s$s3, s$s4)
        })
        
        output$timePlot <- renderPlot({
            df <- tibble(Time = t, Signal = full_signal())
            ggplot(df, aes(x = Time, y = Signal)) +
                geom_line() +
                labs(title = "Time Signal") +
                theme_minimal()
        })
        
        output$freqPlot <- renderPlot({
            fft_signal <- fft(full_signal())
            freq <- seq(0, length(fft_signal)/2 - 1) / (4*T)
            amplitude <- Mod(fft_signal[1:(length(fft_signal)/2)])
            df <- tibble(Frequency = freq, Amplitude = amplitude)
            ggplot(df, aes(x = Frequency, y = Amplitude)) +
                geom_line() +
                labs(title = "Fourier Transform", x = "Frequency (Hz)") +
                theme_minimal() +
                scale_x_continuous(breaks = round(seq(0, max(freq), length.out = 10), 1))
        })
        
        output$zoomedFreqPlot <- renderPlot({
            req(input$freqBrush)
            fft_signal <- fft(full_signal())
            freq <- seq(0, length(fft_signal)/2 - 1) / (4*T)
            amplitude <- Mod(fft_signal[1:(length(fft_signal)/2)])
            df <- tibble(Frequency = freq, Amplitude = amplitude) %>%
                filter(Frequency >= input$freqBrush$xmin, Frequency <= input$freqBrush$xmax)
            ggplot(df, aes(x = Frequency, y = Amplitude)) +
                geom_line() +
                labs(title = "Zoomed Fourier Transform", x = "Frequency (Hz)") +
                theme_minimal() +
                scale_x_continuous(breaks = round(seq(min(df$Frequency), max(df$Frequency), length.out = 10), 1))
        })
        
        output$hoverInfo <- renderText({
            hover <- input$freqHover
            if (!is.null(hover)) {
                paste("Frequency: ", round(hover$x, 1), "Hz")
            } else {
                "Hover over the frequency plot to see values."
            }
        })
    })
}
