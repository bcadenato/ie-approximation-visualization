# modules/lecture_07/jpeg-compression-app.R

library(shiny)
library(tidyverse)
library(jpeg)
library(png)
library(tiff)

# Function to compute the 2D DCT of an 8x8 block
dct2d <- function(block) {
    N <- nrow(block)
    T <- matrix(0, nrow = N, ncol = N)
    for (u in 0:(N-1)) {
        for (x in 0:(N-1)) {
            T[u+1, x+1] <- cos((pi * (2*x + 1) * u) / (2 * N))
        }
    }
    T[1, ] <- T[1, ] / sqrt(2)
    0.5 * T %*% block %*% t(T)
}

# Function to generate a single DCT basis function
dct_basis <- function(u, v, coefficient = 1, N = 8) {
    x <- seq(0, N - 1)
    y <- seq(0, N - 1)
    alpha <- function(k) ifelse(k == 0, sqrt(1/N), sqrt(2/N))
    outer(y, x, function(yy, xx) {
        coefficient * alpha(u) * alpha(v) *
            cos((2 * xx + 1) * u * pi / (2 * N)) *
            cos((2 * yy + 1) * v * pi / (2 * N))
    })
}

jpegCompressionAppUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        titlePanel("DCT on 8x8 Block from Image + Basis Explorer"),
        sidebarLayout(
            sidebarPanel(
                selectInput(ns("imageSelect"), "Select an Image:",
                            choices = c("Boat" = "boat.tiff", "Clock" = "clock.tiff")),
                helpText("Click on the image to select the top-left corner of an 8x8 block"),
                helpText("Then click on the DCT coefficient to see its basis function.")
            ),
            mainPanel(
                fluidRow(
                    column(8, plotOutput(ns("imgPlot"), click = ns("img_click"), height = "600px"),
                           textOutput(ns("coords"))),
                    column(4, plotOutput(ns("blockPlot")))
                ),
                fluidRow(
                    column(6, plotOutput(ns("dctPlot"), click = ns("dct_click"))),
                    column(6, plotOutput(ns("basisPlot")))
                )
            )
        )
    )
}

jpegCompressionAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        img <- reactive({
            filePath <- file.path("www", input$imageSelect)
            ext <- tolower(tools::file_ext(filePath))
            switch(ext,
                   "jpg" = readJPEG(filePath),
                   "jpeg" = readJPEG(filePath),
                   "png" = readPNG(filePath),
                   "tif" = readTIFF(filePath),
                   "tiff" = readTIFF(filePath),
                   NULL)
        })
        
        block_data <- reactive({
            click <- input$img_click
            im <- img()
            if (is.null(click) || is.null(im)) return(NULL)
            x <- round(click$x)
            y <- round(click$y)
            height <- dim(im)[1]
            width <- dim(im)[2]
            if (x + 7 > width || y + 7 > height) return(NULL)
            block <- im[y:(y+7), x:(x+7), drop = FALSE]
            block
        })
        
        dct_result <- reactive({
            block <- block_data()
            if (is.null(block)) return(NULL)
            dct2d(block)
        })
        
        selected_dct <- reactiveVal(c(0, 0))
        
        output$imgPlot <- renderPlot({
            im <- img()
            if (is.null(im)) return()
            expand_grid(x = 1:dim(im)[2], y = 1:dim(im)[1]) %>%
                mutate(value = im[cbind(y, x)]) %>%
                ggplot(aes(x, y, fill = value)) +
                geom_raster() +
                scale_y_reverse() +
                scale_fill_gradient(low = 'black', high = 'white', limits = c(0,1)) +
                coord_fixed() +
                theme_void()
        })
        
        output$coords <- renderText({
            click <- input$img_click
            if (is.null(click)) return(NULL)
            x <- round(click$x)
            y <- round(click$y)
            paste("Coordinates: x =", x, ", y =", y)
        })
        
        output$blockPlot <- renderPlot({
            block <- block_data()
            if (is.null(block)) return()
            expand_grid(x = 1:8, y = 1:8) %>%
                mutate(value = block[cbind(y,x)]) %>%
                ggplot(aes(x, y, fill = value)) +
                geom_raster() +
                scale_y_reverse() +
                scale_fill_gradient(low = 'black', high = 'white', limits = c(0,1)) +
                geom_text(aes(label = round(value, 2))) +
                coord_fixed() +
                theme_void()
        })
        
        output$dctPlot <- renderPlot({
            # dct_block <- dct_result()
            # if (is.null(dct_block)) return()
            # expand_grid(x = 0:7, y = 0:7) %>%
            #     mutate(value = dct_block[cbind(y+1, x+1)]) %>%
            #     dplyr::filter(x > 0 | y > 0) %>%
            #     ggplot(aes(x, y, fill = value)) +
            #     geom_tile(color = "black") +
            #     scale_fill_gradient2(low = 'blue', mid = 'white', high = 'orange', midpoint = 0,
            #                          limits = c(-1, 1)) +
            #     scale_x_continuous(breaks = 0:7, expand = c(0,0)) +
            #     scale_y_reverse(breaks = 0:7, expand = c(0,0)) +
            #     geom_text(aes(label = round(value,1))) +
            #     coord_fixed() +
            #     theme_void()
            # 
            dct_block <- dct_result()
            
            if (is.null(dct_block)) return()
            
            heat_map <- 
                expand_grid(x = 0:7, y = 0:7) %>%
                mutate(value = dct_block[cbind(y+1, x+1)]) %>%
                dplyr::filter(x > 0 | y > 0)
            
            heat_map %>% 
                ggplot(aes(x, y, fill = value)) +
                geom_tile(color = "black") +
                scale_fill_gradient2(low = 'blue', mid = 'white', high = 'orange', midpoint = 0,
                                     limits = c(
                                         min(-1, heat_map$value),
                                         max(1, heat_map$value))) +
                scale_x_continuous(breaks = 0:7, expand = c(0,0)) +
                scale_y_reverse(breaks = 0:7, expand = c(0,0)) +
                geom_text(aes(label = round(value,1))) +
                coord_fixed() +
                theme_void()
        })
        
        observeEvent(input$dct_click, {
            click_x <- round(input$dct_click$x)
            click_y <- round(input$dct_click$y)
            if (click_x >= 0 && click_x <= 7 && click_y >= 0 && click_y <= 7) {
                selected_dct(c(click_x, click_y))
            }
        })
        
        output$basisPlot <- renderPlot({
            sel <- selected_dct()
            dct_block <- dct_result()
            if (is.null(dct_block)) return()
            coefficient <- dct_block[sel[2] + 1, sel[1] + 1]
            mat <- dct_basis(sel[1], sel[2], coefficient)
            as.data.frame(as.table(mat)) %>%
                mutate(x = as.integer(Var2) - 1,
                       y = as.integer(Var1) - 1,
                       value = Freq) %>%
                ggplot(aes(x = x, y = y, fill = value)) +
                geom_tile(color = "black") +
                scale_fill_gradient2(low = 'black', mid = 'grey', high = 'white', midpoint = 0,
                                     limits = c(-1, 1)) +
                scale_x_continuous(breaks = 0:7, expand = c(0,0)) +
                scale_y_reverse(breaks = 0:7, expand = c(0,0)) +
                geom_text(aes(label = round(value,1))) +
                coord_fixed() +
                theme_void()
        })
    })
}
