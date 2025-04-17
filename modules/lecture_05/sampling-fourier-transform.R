
sampledFftAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        titlePanel("Fourier Transform of Sampled Signals"),
        fluidRow(
            column(6,
                   selectInput(ns("function_type"), "Select Function:",
                               choices = c("Cosine 10 Hz", "Cosines 10 Hz & 25 Hz", "Rectangular Pulse 10 Hz"))
            ),
            column(6,
                   sliderInput(ns("sampling_freq"), "Sampling Frequency (Hz):", min = 1, max = 100, value = 50)
            )
        ),
        fluidRow(
            column(6, plotOutput(ns("time_signal_plot"))),
            column(6, plotOutput(ns("sampled_time_plot")))
        ),
        fluidRow(
            column(6, plotOutput(ns("original_fft_plot"))),
            column(6, plotOutput(ns("sampled_fft_plot")))
        )
    )
}

sampledFftAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        generate_signal <- reactive({
            time <- seq(0, 1, length.out = 1000)
            signal <- switch(input$function_type,
                             "Cosine 10 Hz" = cos(2 * pi * 10 * time),
                             "Cosines 10 Hz & 25 Hz" = cos(2 * pi * 10 * time) + cos(2 * pi * 25 * time),
                             "Rectangular Pulse 10 Hz" = as.numeric((time %% 0.1) < 0.05))
            tibble(time = time, value = signal)
        })
        
        sample_signal <- reactive({
            original <- generate_signal()
            sample_times <- seq(0, 1, by = 1 / input$sampling_freq)
            sampled_values <- approx(original$time, original$value, xout = sample_times)$y
            full_signal <- rep(0, length(original$time))
            sample_indices <- sapply(sample_times, function(t) which.min(abs(original$time - t)))
            full_signal[sample_indices] <- sampled_values
            list(
                full = tibble(time = original$time, value = full_signal),
                samples = tibble(time = sample_times, value = sampled_values)
            )
        })
        
        compute_fft <- function(signal_df) {
            n <- length(signal_df$value)
            fft_vals <- fft(signal_df$value)
            freq <- seq(0, length.out = n, by = 1) * (1 / (signal_df$time[2] - signal_df$time[1])) / n
            tibble(freq = freq[1:(n/2)], magnitude = Mod(fft_vals)[1:(n/2)])
        }
        
        output$time_signal_plot <- renderPlot({
            original <- generate_signal()
            ggplot(original, aes(x = time, y = value)) +
                geom_line(color = "blue") +
                labs(title = "Original Signal (Time Domain)", x = "Time (s)", y = "Amplitude") +
                theme_minimal()
        })
        
        output$sampled_time_plot <- renderPlot({
            original <- generate_signal()
            sampled <- sample_signal()
            ggplot() +
                geom_line(data = original, aes(x = time, y = value), color = "grey") +
                geom_segment(data = sampled$samples, aes(x = time, xend = time, y = 0, yend = value), color = "orange") +
                geom_point(data = sampled$samples, aes(x = time, y = value), color = "orange") +
                labs(title = "Sampled Signal (Time Domain)", x = "Time (s)", y = "Amplitude") +
                theme_minimal()
        })
        
        output$original_fft_plot <- renderPlot({
            original <- generate_signal()
            fft_data <- compute_fft(original)
            ggplot(fft_data, aes(x = freq, y = magnitude)) +
                geom_line(color = "blue") +
                labs(title = "FFT of Original Signal", x = "Frequency (Hz)", y = "Magnitude") +
                theme_minimal()
        })
        
        output$sampled_fft_plot <- renderPlot({
            sampled <- sample_signal()
            fft_data <- compute_fft(sampled$full)
            ggplot(fft_data, aes(x = freq, y = magnitude)) +
                geom_line(color = "orange") +
                labs(title = "FFT of Sampled Signal", x = "Frequency (Hz)", y = "Magnitude") +
                theme_minimal()
        })
    })
}
