# modules/lecture_07/time-frequency-analysis-app.R

library(shiny)
library(tidyverse)
library(signal)

timeFrequencyAnalysisAppUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        fluidRow(
            column(2, numericInput(ns("time_window"), "Time Window Size (ms)", value = 100)),
            column(2, numericInput(ns("freq_window"), "Frequency Window Size (Hz)", value = 50)),
            column(2, sliderInput(ns("max_freq"), "Max Frequency (Hz)", min = 100, max = 5000, value = 2000, step = 100)),
            column(2, checkboxGroupInput(ns("components"), "Show Components", 
                                         choices = c("low_freq_sinusoidal", "burst", "impulse", "chirp"),
                                         selected = c("low_freq_sinusoidal", "burst", "impulse", "chirp"))),
            column(2, selectInput(ns("wavelet_type"), "Wavelet Type", choices = c("Morlet", "Mexican Hat")))
        ),
        fluidRow(column(12, plotOutput(ns("signal_plot")))),
        fluidRow(column(12, plotOutput(ns("stft_plot")))),
        fluidRow(column(12, plotOutput(ns("dwt_plot"))))
    )
}

timeFrequencyAnalysisAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        generate_signal <- reactive({
            fs <- 48000
            T <- 1
            t <- seq(0, T - 1/fs, 1/fs)
            
            low_freq_sinusoidal <- 0.6 * sin(2 * pi * 8 * t)
            burst <- 0.25 * sin(2 * pi * 2000 * t) * dnorm(t, 0.25, 0.01) / dnorm(0, 0, 0.01)
            impulse <- ifelse(t == 0.5, 1, 0)
            
            f0 <- 200; f1 <- 4000
            tau <- (t - 0.7) / 0.2
            chirp <- 0.3 * sin(2 * pi * f0 * ((f1/f0)^tau - 1) / log(f1/f0)) * (t >= 0.7 & t <= 0.9)
            
            components <- list(
                low_freq_sinusoidal = low_freq_sinusoidal,
                burst = burst,
                impulse = impulse,
                chirp = chirp
            )
            
            selected <- input$components
            x <- if (length(selected) > 0) reduce(components[selected], `+`) else rep(0, length(t))
            
            list(t = t, x = x, low_freq_sinusoidal = low_freq_sinusoidal,
                 burst = burst, impulse = impulse, chirp = chirp, fs = fs)
        })
        
        compute_stft <- function(sig_data, time_win_ms, freq_win) {
            t <- sig_data$t; x <- sig_data$x; fs <- sig_data$fs
            time_seq <- seq(0, max(t), by = time_win_ms / 1000)
            freq_seq <- seq(0, input$max_freq, by = freq_win)
            win_len <- round(time_win_ms / 1000 * fs)
            hann_win <- hanning(win_len)
            
            map2_dfr(rep(time_seq, each = length(freq_seq)), rep(freq_seq, times = length(time_seq)), function(t0, nu) {
                idx_center <- round(t0 * fs)
                idx_start <- max(1, idx_center - floor(win_len/2))
                idx_end <- min(length(x), idx_start + win_len - 1)
                seg <- x[idx_start:idx_end]
                if (length(seg) < win_len) seg <- c(seg, rep(0, win_len - length(seg)))
                win_seg <- seg * hann_win
                exp_wave <- exp(-1i * 2 * pi * nu * (0:(win_len - 1)) / fs)
                coef <- sum(win_seg * exp_wave) / sqrt(win_len)
                tibble(time = t0, frequency = nu, magnitude = Mod(coef))
            })
        }
        
        morlet_wavelet <- function(t, a, b, f0 = 6) {
            z <- (t - b) / a
            pi^(-0.25) * exp(1i * 2 * pi * f0 * z) * exp(-z^2 / 2) / sqrt(a)
        }
        
        mexican_wavelet <- function(t, a, b) {
            z <- (t - b) / a
            (2 / (sqrt(3 * a) * pi^0.25)) * (1 - z^2) * exp(-z^2 / 2)
        }
        
        compute_dwt <- function(sig_data) {
            t <- sig_data$t; x <- sig_data$x; fs <- sig_data$fs
            wavelet_fun <- switch(input$wavelet_type,
                                  "Morlet" = function(a, b) morlet_wavelet(t, a, b),
                                  "Mexican Hat" = function(a, b) mexican_wavelet(t, a, b))
            
            expand_grid(j = seq(0, 12, 0.2), b = seq(0, 1, 0.01)) %>%
                mutate(a = 1 / 2^j,
                       wc = map2_dbl(a, b, ~ Mod(sum(wavelet_fun(.x, .y) * x / fs))),
                       wc_power = 20 * log10(wc + 1e-12))
        }
        
        output$signal_plot <- renderPlot({
            sig <- generate_signal()
            base_df <- tibble(t = sig$t, signal = sig$x)
            comp_df <- tibble(t = sig$t,
                              low_freq_sinusoidal = sig$low_freq_sinusoidal,
                              burst = sig$burst,
                              impulse = sig$impulse,
                              chirp = sig$chirp) %>%
                pivot_longer(cols = -t, names_to = "component", values_to = "value") %>%
                dplyr::filter(component %in% input$components)
            
            ggplot() +
                geom_line(data = base_df, aes(x = t, y = signal), color = if (length(input$components) > 0) "blue" else NA, alpha = 0.8) +
                geom_line(data = comp_df, aes(x = t, y = value, color = component), alpha = 0.8) +
                labs(title = "Input Signal and Components", x = "Time (s)", y = "Amplitude") +
                scale_color_manual(values = rep("orange", 4))
        })
        
        output$stft_plot <- renderPlot({
            sig <- generate_signal()
            stft_df <- compute_stft(sig, input$time_window, input$freq_window)
            ggplot(stft_df, aes(x = time, y = frequency, fill = magnitude)) +
                geom_tile() +
                labs(title = "STFT Spectrum", x = "Time (s)", y = "Frequency (Hz)") +
                scale_fill_viridis_c()
        })
        
        output$dwt_plot <- renderPlot({
            sig <- generate_signal()
            dwt_df <- compute_dwt(sig)
            ggplot(dwt_df, aes(x = b, y = a, fill = wc_power)) +
                geom_tile() +
                scale_y_log10() +
                labs(title = "DWT Scalogram", x = "Time (s)", y = "Scale (log)") +
                scale_fill_viridis_c()
        })
    })
}
