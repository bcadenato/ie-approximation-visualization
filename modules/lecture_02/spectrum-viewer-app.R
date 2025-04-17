
spectrumViewerAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        titlePanel("Energy Density Spectrum"),
        fluidRow(
            column(4, 
                   selectInput(ns("fileSelect2"), "Select WAV File:", choices = NULL),
                   sliderInput(ns("maxFreq"), "Max Frequency to Display (Hz):", min = 100, max = 20000, value = 1000, step = 100)
            ), 
            column(8, plotOutput(ns("wavePlot2"), brush = brushOpts(id = ns("waveBrush2"))))
        ),
        fluidRow(
            column(6, plotOutput(ns("zoomWave2"))),
            column(6, plotOutput(ns("spectrumPlot"), click = ns("spectrumClick")))
        ),
        fluidRow(
            column(6, p("Frequency"), verbatimTextOutput(ns("frequencyValue"))),
            column(6, p("Coefficient Value:"), verbatimTextOutput(ns("coefficientValue")))
        )
    )
}

spectrumViewerAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        observe({
            wav_files <- list.files("www", pattern = "\\.wav$", full.names = FALSE)
            updateSelectInput(session, "fileSelect2", choices = wav_files)
        })
        
        waveData2 <- reactive({
            req(input$fileSelect2)
            wave <- tuneR::readWave(file.path("www", input$fileSelect2))
            waveMono <- mono(wave, "left")
            list(
                time = seq(0, length(waveMono@left) - 1) / waveMono@samp.rate,
                amplitude = waveMono@left / 2^15,
                samp.rate = waveMono@samp.rate
            )
        })
        
        output$wavePlot2 <- renderPlot({
            data <- waveData2()
            df <- data.frame(time = data$time, amplitude = data$amplitude)
            ggplot(df, aes(x = time, y = amplitude)) +
                geom_line() +
                ggtitle("Selected Sound Wave")
        })
        
        output$zoomWave2 <- renderPlot({
            data <- waveData2()
            brush <- input$waveBrush2
            if (is.null(brush)) return(NULL)
            df <- data.frame(time = data$time, amplitude = data$amplitude)
            df_zoom <- dplyr::filter(df, time >= brush$xmin, time <= brush$xmax)
            ggplot(df_zoom, aes(x = time, y = amplitude)) +
                geom_line(color = "blue") +
                ggtitle("Zoomed Section")
        })
        
        output$spectrumPlot <- renderPlot({
            data <- waveData2()
            y <- data$amplitude
            fs <- data$samp.rate
            n <- length(y)
            Y <- fft(y)
            freq <- seq(0, fs / 2, length.out = floor(n / 2) + 1)
            mag <- Mod(Y[1:(floor(n / 2) + 1)])^2 / n
            spectrum_df <- data.frame(freq = freq, mag = mag)
            spectrum_df <- dplyr::filter(spectrum_df, freq <= input$maxFreq)
            ggplot(spectrum_df, aes(x = freq, y = mag)) +
                geom_line() +
                ggtitle("Energy Density Spectrum") +
                xlab("Frequency (Hz)") +
                ylab("Magnitude")
        })
        
        output$frequencyValue <- renderPrint({
            click <- input$spectrumClick
            if (is.null(click)) return("Click on the spectrum to see details.")
            freq_clicked <- click$x
            paste("Frequency: ", round(freq_clicked, 2), " Hz")
        })
        
        output$coefficientValue <- renderPrint({
            click <- input$spectrumClick
            if (is.null(click)) return("Click on the spectrum to see details.")
            mag_clicked <- click$y
            paste("Magnitude: ", round(mag_clicked, 5))
        })
    })
}
