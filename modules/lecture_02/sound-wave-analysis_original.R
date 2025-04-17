library(shiny)
library(tuneR)
library(ggplot2)
library(dplyr)
library(signal)
library(reshape2)

ui <- fluidPage(
    titlePanel("Sound Wave Analyzer"),
    tabsetPanel(
        tabPanel("Fourier Coefficient Explorer",
                 fluidRow(
                     column(4,
                            selectInput("fileSelect", "Select WAV File:", choices = NULL),
                            sliderInput("freq", "Select Frequency (Hz):", min = 50, max = 500, value = 100)
                     ),
                     column(8,
                            plotOutput("fullWave", brush = brushOpts(id = "waveBrush"))
                     )
                 ),
                 fluidRow(
                     column(6,
                            plotOutput("zoomWave"),
                            plotOutput("realBasis")
                     ),
                     column(6,
                            plotOutput("productPlot"),
                            plotOutput("accumulatedArea") # energyBar
                     )
                 )
        ),
        tabPanel("Energy Density Spectrum",
                 fluidRow(
                     column(4, 
                            selectInput("fileSelect2", "Select WAV File:", choices = NULL),
                            sliderInput("maxFreq", "Max Frequency to Display (Hz):", min = 100, max = 20000, value = 1000, step = 100)), 
                     column(8, plotOutput("wavePlot2", brush = brushOpts(id = "waveBrush2")))
                     ),
                fluidRow(
                    column(6, plotOutput("zoomWave2")),
                    column(6, plotOutput("spectrumPlot", click = "spectrumClick"))
                ),
                fluidRow(
                    column(6, 
                           p("Frequency"),
                           verbatimTextOutput("frequencyValue")),
                    column(6, 
                           p("Coefficient Value:"),
                           verbatimTextOutput("coefficientValue")
                           ) 
                    ) 
                ) 
        ) 
    )

server <- function(input, output, session) {
    observe({
        wav_files <- list.files("www", pattern = "\\.wav$", full.names = FALSE)
        updateSelectInput(session, "fileSelect", choices = wav_files)
        updateSelectInput(session, "fileSelect2", choices = wav_files)
    })
    
    waveData <- reactive({
        req(input$fileSelect)
        wave <- readWave(file.path("www", input$fileSelect))
        waveMono <- mono(wave, "left")
        data.frame(
            time = seq(0, length(waveMono@left) - 1) / waveMono@samp.rate,
            amplitude = waveMono@left / 2^15,
            samp.rate = waveMono@samp.rate
        )
    })
    
    waveData2 <- reactive({
        req(input$fileSelect2)
        wave <- readWave(file.path("www", input$fileSelect2))
        waveMono <- mono(wave, "left")
        list(
            time = seq(0, length(waveMono@left) - 1) / waveMono@samp.rate,
            amplitude = waveMono@left / 2^15,
            samp.rate = waveMono@samp.rate
        )
    })
    
    yLimits <- reactive({
        df <- waveData()
        range(df$amplitude)
    })
    
    output$fullWave <- renderPlot({
        df <- waveData()
        ggplot(df, aes(x = time, y = amplitude)) +
            geom_line() +
            ggtitle("Full Sound Wave")
    })
    
    output$zoomWave <- renderPlot({
        df <- waveData()
        brush <- input$waveBrush
        if (is.null(brush)) return(NULL)
        zoom_df <- dplyr::filter(df, time >= brush$xmin, time <= brush$xmax)
        ggplot(zoom_df, aes(x = time, y = amplitude)) +
            geom_line(color = "blue") +
            ggtitle("Zoomed Section")
    })
    
    output$realBasis <- renderPlot({
        df <- waveData()
        freq <- input$freq
        brush <- input$waveBrush
        if (is.null(brush)) return(NULL)
        df <- dplyr::filter(df, time >= brush$xmin, time <= brush$xmax)
        real_basis <- Re(exp(-1i * 2 * pi * freq * df$time))
        ggplot(data.frame(time = df$time, value = real_basis), aes(x = time, y = value)) +
            geom_line(color = "orange") +
            ylim(-1, 1) +
            ggtitle("Real Part of Fourier Basis (exp(-i2πf₀t))")
    })
    
    df_zoomed <- reactive({
        df <- waveData()
        # freq <- input$freq
        brush <- input$waveBrush
        if (is.null(brush)) return(NULL)
        dplyr::filter(df, time >= brush$xmin, time <= brush$xmax)
    })
    
    product_real <- reactive({
        freq <- input$freq
        df <- df_zoomed()
        Re(df$amplitude * exp(-1i * 2 * pi * freq * df$time))
    })
    
    output$productPlot <- renderPlot({
        brush <- input$waveBrush
        if (is.null(brush)) return(NULL)
        
        df <- df_zoomed()
        product_r <- product_real()
        ggplot(data.frame(time = df$time, value = product_r), aes(x = time, y = value)) +
            geom_line(color = "grey") +
            ylim(yLimits()) +
            ggtitle("Product of Signal and Basis (Zoomed)")
    })
    
    output$energyBar <- renderPlot({
        df <- waveData()
        freq <- input$freq
        exp_part <- exp(-1i * 2 * pi * freq * df$time)
        projection <- df$amplitude * exp_part
        dt <- 1 / unique(df$samp.rate)
        energy_at_freq <- abs(sum(projection) * dt)^2
        total_energy <- sum(df$amplitude^2) * dt
        percent <- 100 * energy_at_freq / total_energy
        ggplot(data.frame(freq = freq, percent = percent), aes(x = factor(freq), y = percent)) +
            geom_col(fill = "red") +
            ylim(0, 100) +
            ggtitle("Energy at Selected Frequency (%)") +
            xlab("Frequency (Hz)") +
            ylab("Percentage of Total Energy")
    })
    
    output$accumulatedArea <- renderPlot({
        # df <- waveData()
        # freq <- input$freq
        # exp_part <- exp(-1i * 2 * pi * freq * df$time)
        # projection <- df$amplitude * exp_part
        # dt <- 1 / unique(df$samp.rate)
        # energy_at_freq <- abs(sum(projection) * dt)^2
        # total_energy <- sum(df$amplitude^2) * dt
        # percent <- 100 * energy_at_freq / total_energy
        
        df <- df_zoomed()
        product_r <- product_real()
        
        product_plot <- tibble(
            time = df$time,
            value = product_r) %>% 
            mutate(
                accumValue = cumsum(value))
        
        ggplot(product_plot, aes(x = time, y = accumValue)) +
            geom_line(colour = "red") +
            ggtitle("Accumulated Area") +
            xlab("time") +
            ylab("Accumulated Area")
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
        freq <- seq(0, fs/2, length.out = floor(n/2) + 1)
        mag <- Mod(Y[1:(floor(n/2) + 1)])^2 / n
        spectrum_df <- data.frame(freq = freq, mag = mag)
        spectrum_df <- dplyr::filter(spectrum_df, freq <= input$maxFreq)
        output$spectrum_df <- DT::renderDT({spectrum_df})
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
}

shinyApp(ui, server)
