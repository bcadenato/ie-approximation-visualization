library(shiny)
library(tidyverse)
library(ggplot2)

# DCT Basis Function
dct_basis <- function(u, v, N = 8) {
    x <- seq(0, N - 1)
    y <- seq(0, N - 1)
    
    alpha <- function(k) ifelse(k == 0, sqrt(1/N), sqrt(2/N))
    
    outer(y, x, function(yy, xx) {
        alpha(u) * alpha(v) *
            cos((2 * xx + 1) * u * pi / (2 * N)) *
            cos((2 * yy + 1) * v * pi / (2 * N))
    })
}

# UI Module
dctBasis2dAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        titlePanel("DCT Basis Functions Explorer"),
        checkboxInput(ns("toggle_mode"), "Maintain selection (toggle mode)", value = FALSE),
        fluidRow(
            column(6,
                   plotOutput(ns("grid"), click = ns("grid_click"))
            ),
            column(6,
                   plotOutput(ns("basis"))
            )
        )
    )
}

# Server Module
dctBasis2dAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        selected_set <- reactiveVal(list())
        
        observeEvent(input$grid_click, {
            req(input$grid_click)
            click_x <- round(input$grid_click$x)
            click_y <- round(input$grid_click$y)
            
            if (click_x >= 0 && click_x <= 7 && click_y >= 0 && click_y <= 7) {
                sel <- selected_set()
                pos_str <- paste(click_x, click_y, sep = ",")
                
                if (input$toggle_mode) {
                    if (pos_str %in% names(sel)) {
                        sel[[pos_str]] <- NULL
                    } else {
                        sel[[pos_str]] <- c(click_x, click_y)
                    }
                } else {
                    sel <- list()
                    sel[[pos_str]] <- c(click_x, click_y)
                }
                selected_set(sel)
            }
        })
        
        output$grid <- renderPlot({
            df <- expand.grid(x = 0:7, y = 0:7)
            sel_list <- selected_set()
            sel_df <- if (length(sel_list) > 0) {
                bind_rows(lapply(sel_list, function(coord) tibble(x = coord[1], y = coord[2])))
            } else {
                tibble(x = numeric(0), y = numeric(0))
            }
            
            final_df <- 
                df %>% 
                as_tibble() %>% 
                mutate(base_col = "white") %>% 
                left_join(
                    sel_df %>%  
                        mutate(sel_col = "grey80"),  
                    by = join_by(x, y)) %>% 
                mutate(final_col = coalesce(sel_col, base_col))
            
            ggplot(final_df, aes(x = x, y = y)) +
                geom_tile(aes(fill = final_col), color = "black") +
                scale_fill_identity() +
                scale_x_continuous(breaks = 0:7, expand = c(0, 0), limits = c(-0.5, 7.5)) +
                scale_y_reverse(breaks = 0:7, expand = c(0, 0), limits = c(7.5, -0.5)) +
                coord_fixed() +
                theme_void()
        })
        
        output$basis <- renderPlot({
            sel_list <- selected_set()
            if (length(sel_list) == 0) return(NULL)
            
            superposed <- Reduce(`+`, lapply(sel_list, function(coord) dct_basis(coord[1], coord[2])))
            
            df <- as.data.frame(as.table(superposed)) %>%
                mutate(x = as.integer(Var2) - 1,
                       y = as.integer(Var1) - 1,
                       value = Freq)
            
            ggplot(df, aes(x = x, y = y, fill = value)) +
                geom_tile(color = "black") +
                scale_fill_gradient2(low = "white", mid = "grey", high = "black", midpoint = 0) +
                scale_x_continuous(breaks = 0:7, expand = c(0, 0)) +
                scale_y_reverse(breaks = 0:7, expand = c(0, 0)) +
                coord_fixed() +
                theme_void() +
                theme(legend.position = "none")
        })
    })
}
