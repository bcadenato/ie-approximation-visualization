
fourierExplorerAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        titlePanel("Fourier Coefficient Explorer"),
        fluidRow(
            column(4,
                   selectInput(ns("fileSelect"), "Select WAV File:", choices = NULL),
                   sliderInput(ns("freq"), "Select Frequency (Hz):", min = 50, max = 500, value = 100)
            ),
            column(8,
                   plotOutput(ns("fullWave"), brush = brushOpts(id = ns("waveBrush")))
            )
        ),
        fluidRow(
            column(6,
                   plotOutput(ns("zoomWave")),
                   plotOutput(ns("realBasis"))
            ),
            column(6,
                   plotOutput(ns("productPlot")),
                   plotOutput(ns("accumulatedArea"))
            )
        )
    )
}

fourierExplorerAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        observe({
            wav_files <- list.files("www", pattern = "\\.wav$", full.names = FALSE)
            updateSelectInput(session, "fileSelect", choices = wav_files)
        })
        
        waveData <- reactive({
            req(input$fileSelect)
            wave <- tuneR::readWave(file.path("www", input$fileSelect))
            waveMono <- mono(wave, "left")
            data.frame(
                time = seq(0, length(waveMono@left) - 1) / waveMono@samp.rate,
                amplitude = waveMono@left / 2^15,
                samp.rate = waveMono@samp.rate
            )
        })
        
        yLimits <- reactive({
            df <- waveData()
            range(df$amplitude)
        })
        
        df_zoomed <- reactive({
            df <- waveData()
            brush <- input$waveBrush
            if (is.null(brush)) return(NULL)
            dplyr::filter(df, time >= brush$xmin, time <= brush$xmax)
        })
        
        product_real <- reactive({
            freq <- input$freq
            df <- df_zoomed()
            Re(df$amplitude * exp(-1i * 2 * pi * freq * df$time))
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
        
        output$accumulatedArea <- renderPlot({
            brush <- input$waveBrush
            if (is.null(brush)) return(NULL)
            
            df <- df_zoomed()
            product_r <- product_real()
            product_plot <- tibble(
                time = df$time,
                value = product_r
            ) %>% mutate(accumValue = cumsum(value))
            
            ggplot(product_plot, aes(x = time, y = accumValue)) +
                geom_line(colour = "red") +
                ggtitle("Accumulated Area") +
                xlab("time") +
                ylab("Accumulated Area")
        })
    })
}
