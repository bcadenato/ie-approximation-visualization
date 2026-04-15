# modules/lecture_07/windowed-fourier-transform-app.R

library(shiny)
library(tidyverse)
library(signal)
library(tuneR)
library(glue)

f_s <- 500000
T <- 1
max_f <- 1000

wft_generate_chirp <- function(n = f_s * T, f0 = 1, f1 = 50, duration = T) {
    t <- seq(0, duration, length.out = n)
    signal <- sin(2 * pi * (f0 + (f1 - f0) * t / (2 * duration)) * t)
    data.frame(time = t, signal = signal)
}

apply_window <- function(window_type, signal) {
    n <- length(signal)
    w <- switch(window_type,
                "Rectangular" = rep(1, n),
                "Gaussian" = dnorm(seq(-2, 2, length.out = n)),
                "Hanning" = hanning(n),
                "Hamming" = hamming(n),
                "Blackman" = blackman(n))
    signal * w
}

compute_wft2 <- function(windowed_signal) {
    n = f_s * T
    t <- seq(0, T, length.out = n)
    signal <- c(windowed_signal, rep(0, length(t) - length(windowed_signal)))
    wft <- fft(signal)
    freq <- seq(0, f_s, length.out = n)
    data.frame(freq = freq, power = Mod(wft)^2)
}

windowedFourierTransformAppUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        titlePanel("Windowed Fourier Transform Explorer"),
        fluidRow(
            column(4, sliderInput(ns("start_freq"), "Start Frequency:", min = 1, max = max_f, value = 1)),
            column(4, sliderInput(ns("end_freq"), "End Frequency:", min = 1, max = max_f, value = 500)),
            column(12, plotOutput(ns("input_signal"), brush = brushOpts(id = ns("signal_brush"))))
        ),
        fluidRow(
            selectInput(ns("window1"), "Select Window for Graph 1:",
                        choices = c("Rectangular", "Gaussian", "Hanning", "Hamming", "Blackman")),
            column(6, plotOutput(ns("window1_plot"))),
            column(6, plotOutput(ns("wft_plot1")))
        ),
        fluidRow(
            column(3, verbatimTextOutput(ns("window_samples"))),
            column(3, verbatimTextOutput(ns("wft_samples"))),
            column(3, verbatimTextOutput(ns("wft_plotted_samples"))),
            column(3, verbatimTextOutput(ns("wft_max_freq")))
        )
    )
}

windowedFourierTransformAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        signal_data <- reactive({
            wft_generate_chirp(f0 = input$start_freq, f1 = input$end_freq)
        })
        
        output$input_signal <- renderPlot({
            
            ggplot(signal_data(), aes(x = time, y = signal)) +
                geom_line() +
                labs(title = "Chirp Signal", x = "Time", y = "Amplitude")
        })
        
        selected_signal <- reactive({
            if (is.null(input$signal_brush)) return(NULL)
            signal_data() %>% dplyr::filter(time >= input$signal_brush$xmin,
                                     time <= input$signal_brush$xmax)
        })
        
        output$window1_plot <- renderPlot({
            if (is.null(selected_signal())) return()
            windowed_signal <- apply_window(input$window1, selected_signal()$signal)
            tibble(time = selected_signal()$time, signal = windowed_signal) %>%
                ggplot(aes(x = time, y = signal)) +
                geom_line()
        })
        
        wft_data <- reactive({
            if (is.null(selected_signal())) return()
            windowed_signal <- apply_window(input$window1, selected_signal()$signal)
            compute_wft2(windowed_signal)
        })
        
        output$wft_plot1 <- renderPlot({
            if (is.null(wft_data())) return()
            ggplot(wft_data(), aes(x = freq, y = power)) +
                geom_line() +
                labs(title = "Windowed Fourier Transform", x = "Frequency", y = "Power") +
                coord_cartesian(xlim = c(0, max_f * 1.25))
        })
        
        output$window_samples <- renderText({
            if (is.null(selected_signal())) return("No selection")
            glue("{nrow(selected_signal())} samples")
        })
        
        output$wft_samples <- renderText({
            if (is.null(wft_data())) return("No data")
            glue("{nrow(wft_data())} samples")
        })
        
        output$wft_plotted_samples <- renderText({
            if (is.null(wft_data())) return("No data")
            glue("{nrow(wft_data() %>% dplyr::filter(freq <= max_f * 1.25))} samples")
        })
        
        output$wft_max_freq <- renderText({
            if (is.null(wft_data())) return("No data")
            glue("{max(wft_data()$freq)} max freq")
        })
    })
}
