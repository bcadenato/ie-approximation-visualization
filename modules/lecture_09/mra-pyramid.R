# modules/lecture_07/wavelet-decomposition-app.R

library(shiny)
library(waveslim)
library(tidyverse)
library(ggplot2)
library(viridis)

generate_chirp <- function(fs = 2^12, duration = 1, f0 = 10, f1 = 100) {
    t <- seq(0, duration, by = 1 / fs)[1:2^12]
    chirp <- sin(2 * pi * (f0 + (f1 - f0) * t / (2 * duration)) * t)
    tibble(t = t, signal = chirp)
}

reconstruct_signal <- function(dwt_result, S) {
    dwt_mod <- dwt_result
    max_level <- length(dwt_result) - 1
    for (lvl in 1:max_level) {
        if (lvl < (13 - S)) {
            dwt_mod[[paste0("d", lvl)]] <- rep(0, length(dwt_mod[[paste0("d", lvl)]]))
        }
    }
    idwt(dwt_mod)
}

get_pyramid_rects <- function(dwt_result, active_levels) {
    map_dfr(1:12, function(lvl) {
        coeffs <- dwt_result[[paste0("d", lvl)]]
        n <- length(coeffs)
        width <- 1 / n
        starts <- seq(0, 1 - width, by = width)
        ends <- starts + width
        tibble(
            level = 13 - lvl,
            label = 13 - lvl,
            display = lvl,
            xmin = starts,
            xmax = ends,
            ymin = 13 - lvl - 0.4,
            ymax = 13 - lvl + 0.4,
            value = coeffs,
            active = lvl >= (13 - active_levels)
        )
    })
}

waveletDecompositionAppUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        titlePanel("Wavelet Multiresolution Decomposition of Chirp Signal"),
        sidebarLayout(
            sidebarPanel(
                sliderInput(ns("scales"), "Number of Scales (S):", min = 1, max = 12, value = 1),
                selectInput(ns("wavelet"), "Wavelet Type:", 
                            choices = c("db2" = "d4", "db4" = "d8", "haar" = "haar"))
            ),
            mainPanel(
                plotOutput(ns("originalPlot")),
                plotOutput(ns("reconstructedPlot")),
                plotOutput(ns("pyramidPlot"), height = "600px")
            )
        )
    )
}

waveletDecompositionAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        chirp_data <- reactive({
            generate_chirp()
        })
        
        dwt_data <- reactive({
            dwt(chirp_data()$signal, wf = input$wavelet, n.levels = 12)
        })
        
        reconstructed_data <- reactive({
            tibble(
                t = chirp_data()$t,
                reconstructed = reconstruct_signal(dwt_data(), input$scales)
            )
        })
        
        pyramid_rects <- reactive({
            get_pyramid_rects(dwt_data(), input$scales)
        })
        
        output$originalPlot <- renderPlot({
            ggplot(chirp_data(), aes(x = t, y = signal)) +
                geom_line() +
                labs(title = "Original Chirp Signal", x = "Time (s)", y = "Amplitude") +
                theme_minimal()
        })
        
        output$reconstructedPlot <- renderPlot({
            ggplot(reconstructed_data(), aes(x = t, y = reconstructed)) +
                geom_line(color = "blue") +
                labs(title = paste("Reconstructed Signal with", input$scales, "Scales"),
                     x = "Time (s)", y = "Amplitude") +
                theme_minimal()
        })
        
        output$pyramidPlot <- renderPlot({
            ggplot(pyramid_rects(), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
                geom_rect(aes(fill = ifelse(active, value, NA)), color = "white") +
                scale_fill_viridis_c(na.value = "grey80", name = "Coeff.") +
                labs(title = "Wavelet Coefficients Pyramid", x = "Time (normalized)", y = "J Level") +
                theme_minimal() +
                scale_y_reverse(breaks = 1:12, labels = 1:12)
        })
    })
}
