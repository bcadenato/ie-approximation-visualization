# modules/lecture_07/manual-spectrogram-app.R

library(shiny)
library(tidyverse)
library(signal)
library(tuneR)

freq_to_note <- function(freq) {
    a4 <- 440
    notes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
    if (freq <= 0) return(list(note = NA, freq = NA))
    n <- round(12 * log2(freq / a4))
    note_index <- (n + 9) %% 12 + 1
    octave <- 4 + ((n + 9) %/% 12)
    note_name <- paste0(notes[note_index], octave)
    true_freq <- a4 * 2^(n / 12)
    list(note = note_name, freq = round(true_freq, 2))
}

manualSpectrogramAppUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        sidebarLayout(
            sidebarPanel(
                # selectInput(ns("file"), "Choose a file:", choices = list.files("www")),
                selectInput(ns("file"), "Choose a file:", choices = list.files("www", pattern = "\\.wav$")),
                sliderInput(ns("time_spread"), "Time Step (ms):", min = 0, max = 500, value = 10),
                sliderInput(ns("freq_spread"), "Frequency Step (Hz):", min = 1, max = 100, value = 10),
                sliderInput(ns("max_freq"), "Maximum Frequency (Hz):", min = 100, max = 2000, value = 2000),
                sliderInput(ns("window_width"), "Window Width (ms):", min = 1, max = 500, value = 50),
                checkboxInput(ns("show_grid"), "Show Lattice Grid", value = FALSE),
                actionButton(ns("calculate"), "Calculate")
            ),
            mainPanel(
                plotOutput(ns("spectrogram"), click = ns("spec_click")),
                uiOutput(ns("click_info_card")),
                plotOutput(ns("windowed_segment")),
                plotOutput(ns("integral_progress"))
            )
        )
    )
}

manualSpectrogramAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        signal_data <- reactiveVal(NULL)
        spec_points <- reactiveVal(NULL)
        
        observeEvent(input$calculate, {
            req(input$file)
            path <- file.path("www", input$file)
            snd <- tuneR::readWave(path)
            fs <- snd@samp.rate
            sig <- snd@left / 2^(snd@bit - 1)
            
            time_seq <- seq(0, length(sig) / fs, by = input$time_spread / 1000)
            freq_seq <- seq(0, input$max_freq, by = input$freq_spread)
            
            window_length <- round(input$window_width / 1000 * fs)
            hann_win <- signal::hanning(window_length)
            
            points_list <- list()
            for (t0 in time_seq) {
                for (nu in freq_seq) {
                    idx_center <- round(t0 * fs)
                    idx_start <- max(1, idx_center - floor(window_length / 2))
                    idx_end <- min(length(sig), idx_start + window_length - 1)
                    segment <- sig[idx_start:idx_end]
                    if (length(segment) < window_length) {
                        segment <- c(segment, rep(0, window_length - length(segment)))
                    }
                    windowed_segment <- segment * hann_win
                    exp_wave <- exp(-1i * 2 * pi * nu * (0:(window_length - 1)) / fs)
                    coef <- sum(windowed_segment * exp_wave) / sqrt(window_length)
                    points_list[[length(points_list) + 1]] <- tibble(
                        time = t0, freq = nu, coef = coef
                    )
                }
            }
            
            spec_df <- bind_rows(points_list)
            signal_data(list(sig = sig, fs = fs, window_length = window_length,
                             hann_win = hann_win, time_seq = time_seq, freq_seq = freq_seq))
            spec_points(spec_df)
        })
        
        output$spectrogram <- renderPlot({
            req(spec_points())
            info <- signal_data()
            p <- ggplot(spec_points(), aes(x = time, y = freq)) +
                geom_point(aes(size = Mod(coef), color = Mod(coef))) +
                scale_color_gradient(low = "white", high = "black") +
                theme_minimal() +
                labs(title = "Manual Spectrogram", x = "Time (s)", y = "Frequency (Hz)")
            if (input$show_grid && !is.null(info)) {
                p <- p +
                    geom_vline(xintercept = info$time_seq, color = "gray80", linetype = "dotted") +
                    geom_hline(yintercept = info$freq_seq, color = "gray80", linetype = "dotted")
            }
            p
        })
        
        observeEvent(input$spec_click, {
            click <- input$spec_click
            df <- spec_points()
            nearest <- df %>%
                mutate(dist = sqrt((time - click$x)^2 + (freq - click$y)^2)) %>%
                arrange(dist) %>%
                slice(1)
            
            info <- signal_data()
            idx_center <- round(nearest$time * info$fs)
            idx_start <- max(1, idx_center - floor(info$window_length / 2))
            idx_end <- min(length(info$sig), idx_start + info$window_length - 1)
            segment <- info$sig[idx_start:idx_end]
            if (length(segment) < info$window_length) {
                segment <- c(segment, rep(0, info$window_length - length(segment)))
            }
            
            windowed <- segment * info$hann_win
            exp_wave <- exp(-1i * 2 * pi * nearest$freq * (0:(info$window_length - 1)) / info$fs)
            product <- windowed * exp_wave
            integral_progress <- cumsum(product) / sqrt(info$window_length)
            note_info <- freq_to_note(nearest$freq)
            
            output$click_info_card <- renderUI({
                div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 10px; margin: 10px 0;",
                    h4("Clicked Point Info"),
                    p(strong("Time:"), sprintf("%.5f s", nearest$time)),
                    p(strong("Frequency:"), sprintf("%.2f Hz", nearest$freq)),
                    p(strong("Closest Note:"), paste0(note_info$note, " (", note_info$freq, " Hz)"))
                )
            })
            
            output$windowed_segment <- renderPlot({
                tibble(time = (0:(info$window_length - 1)) / info$fs, amplitude = windowed) %>%
                    ggplot(aes(x = time, y = amplitude)) +
                    geom_line() +
                    theme_minimal() +
                    labs(title = "Windowed Segment", x = "Time (s)", y = "Amplitude")
            })
            
            output$integral_progress <- renderPlot({
                tibble(time = (0:(info$window_length - 1)) / info$fs, integral_abs = Mod(integral_progress)) %>%
                    ggplot(aes(x = time, y = integral_abs)) +
                    geom_line() +
                    theme_minimal() +
                    labs(title = "Progressive Integral (abs)", x = "Time (s)", y = "|Integral|")
            })
        })
    })
}
