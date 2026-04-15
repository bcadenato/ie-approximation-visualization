# modules/lecture_07/wft-viewer-app.R

library(shiny)
library(tidyverse)
library(signal)
library(pracma)
library(viridis)

wftViewerAppUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        titlePanel("Windowed Fourier Transform Viewer"),
        sidebarLayout(
            sidebarPanel(
                sliderInput(ns("segments"), "Number of Segments:",
                            min = 2, max = 10, value = 5),
                selectInput(ns("window"), "Select Window Function:",
                            choices = c("Rectangular", "Gaussian", "Hanning", "Hamming", "Blackman")),
                tags$h4("WFT Controls"),
                numericInput(ns("window_ms"), "Window Size (ms):", value = 50, min = 1, max = 500)
            ),
            mainPanel(
                plotOutput(ns("timePlot")),
                plotOutput(ns("wftPlot"))
            )
        )
    )
}

wftViewerAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        signal_data <- reactive({
            fs <- 48000
            t <- seq(0, 1, by = 1/fs)
            N <- input$segments
            len_segment <- length(t) / N
            freqs <- seq(20, 1000, length.out = N)
            
            signal <- numeric(length(t))
            
            for (i in 1:N) {
                idx_start <- floor((i - 1) * len_segment) + 1
                idx_end <- floor(i * len_segment)
                segment_time <- t[idx_start:idx_end] - t[idx_start]
                signal[idx_start:idx_end] <- sin(2 * pi * freqs[i] * segment_time)
            }
            
            tibble(t = t, signal = signal)
        })
        
        output$timePlot <- renderPlot({
            ggplot(signal_data(), aes(x = t, y = signal)) +
                geom_line() +
                labs(title = "Time Domain Signal", x = "Time (s)", y = "Amplitude") +
                theme_minimal()
        })
        
        output$wftPlot <- renderPlot({
            data <- signal_data()
            fs <- 48000
            freq_limit <- 1200
            window_size <- as.integer((input$window_ms / 1000) * fs)
            window_size_half <- as.integer(window_size / 2)
            step <- floor(window_size / 4)
            t <- data$t
            x <- data$signal
            
            win_func <- switch(input$window,
                               "Rectangular" = rep(1, window_size),
                               "Gaussian" = gausswin(window_size),
                               "Hanning" = hanning(window_size),
                               "Hamming" = hamming(window_size),
                               "Blackman" = blackman(window_size))
            
            n_windows <- floor((length(x) - window_size) / step)
            freq_axis <- seq(0, fs/2, length.out = window_size_half)
            wft_data <- tibble()
            
            for (i in 1:n_windows) {
                start_idx <- (i - 1) * step + 1
                segment <- x[start_idx:(start_idx + window_size - 1)] * win_func
                fft_segment <- abs(fft(segment))[1:window_size_half]
                temp_df <- tibble(
                    Time = rep(t[start_idx], window_size_half),
                    Frequency = freq_axis,
                    Amplitude = fft_segment
                )
                wft_data <- bind_rows(wft_data, temp_df)
            }
            
            wft_data_plot <- 
                wft_data %>% 
                dplyr::filter(Frequency <= freq_limit)
            
            ggplot(wft_data_plot, aes(x = Time, y = Frequency, fill = Amplitude)) +
                geom_tile() +
                scale_fill_viridis(option = "plasma") +
                labs(title = "Windowed Fourier Transform (Heatmap)", x = "Time (s)", y = "Frequency (Hz)", fill = "Amplitude") +
                theme_minimal()
        })
    })
}
