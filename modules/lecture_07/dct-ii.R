# Load necessary libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(signal)

# Constants
N <- 8

# Define alpha function
alpha <- function(k, N) {
    if (k == 0) sqrt(1 / N) else sqrt(2 / N)
}

# DCT-II
dct_ii <- function(x) {
    sapply(0:(N-1), function(k) {
        alpha(k, N) * sum(x * cos(pi * (2 * (0:(N-1)) + 1) * k / (2 * N)))
    })
}

# DCT-III (inverse)
dct_iii <- function(X, n_vals) {
    sapply(n_vals, function(n) {
        sum(X * cos(pi * (2 * n + 1) * (0:(N-1)) / (2 * N)) *
                sapply(0:(N-1), alpha, N = N))
    })
}

# DFT and IDFT
dft <- function(x) {
    sapply(0:(N-1), function(k) {
        sum(x * exp(-2i * pi * k * (0:(N-1)) / N))
    })
}

idft <- function(X, n_vals) {
    sapply(n_vals, function(n) {
        sum(X * exp(2i * pi * (0:(N-1)) * n / N)) / N
    })
}

# UI Module
dctIiAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        titlePanel("Transform and Inverse with Extension"),
        sidebarLayout(
            sidebarPanel(
                textInput(ns("signal"), "Enter 8 comma-separated signal values:", "1,2,3,4,5,6,7,8"),
                selectInput(ns("transform_type"), "Select Transform:", choices = c("DCT-II", "DFT"))
            ),
            mainPanel(
                plotOutput(ns("coeffPlot")),
                plotOutput(ns("reconstructedPlot"))
            )
        )
    )
}

# Server Module
dctIiAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        signal_values <- reactive({
            as.numeric(unlist(strsplit(input$signal, ",")))
        })
        
        transform_result <- reactive({
            x <- signal_values()
            if(length(x) != 8) return(rep(NA, 8))
            if (input$transform_type == "DCT-II") {
                dct_ii(x)
            } else {
                dft(x)
            }
        })
        
        output$coeffPlot <- renderPlot({
            coeffs <- transform_result()
            if(any(is.na(coeffs))) return(NULL)
            df <- data.frame(k = 0:7, AbsCoeff = Mod(coeffs))
            ggplot(df, aes(x = k, y = AbsCoeff)) +
                geom_segment(aes(xend = k, yend = 0)) +
                geom_point(size = 2) +
                labs(title = paste(input$transform_type, "Coefficients (Magnitude)"), x = "k", y = "|X[k]|") +
                theme_minimal()
        })
        
        output$reconstructedPlot <- renderPlot({
            coeffs <- transform_result()
            if(any(is.na(coeffs))) return(NULL)
            
            n_range <- -16:24
            reconstructed <- if (input$transform_type == "DCT-II") {
                dct_iii(coeffs, n_range)
            } else {
                Re(idft(coeffs, n_range))
            }
            
            df <- data.frame(n = n_range, x = reconstructed,
                             type = ifelse(n_range %in% 0:7, "original", "extension"))
            
            ggplot(df, aes(x = n, y = x, color = type)) +
                geom_segment(aes(xend = n, yend = 0)) +
                geom_point(size = 2) +
                scale_color_manual(values = c("original" = "blue", "extension" = "orange")) +
                labs(title = paste("Reconstructed Signal via Inverse", input$transform_type), x = "n", y = "x[n]") +
                theme_minimal()
        })
    })
}
