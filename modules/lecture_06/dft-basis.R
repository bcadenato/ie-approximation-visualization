
dftBasisAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        tags$head(tags$style(HTML(".shiny-input-container { margin-bottom: 10px; }"))),
        fluidRow(
            column(12,
                   selectInput(ns("basis"), "Select Basis Vector (k):", choices = 0:7, selected = 0)
            )
        ),
        fluidRow(
            column(12,
                   imageOutput(ns("unitCirclePlot"))
            )
        )
    )
}

dftBasisAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        N <- 8
        n <- 0:(N - 1)
        k_values <- 0:(N - 1)
        fine_steps <- 25
        total_frames <- N * fine_steps
        
        unit_circle <- tibble(
            angle = seq(0, 2 * pi, length.out = 100),
            x = cos(angle),
            y = sin(angle)
        )
        
        precomputed_animations <- map(0:7, function(k) {
            filename <- file.path(str_c("www/dft_basis_k", k, ".gif"))
            if (!file.exists(filename)) {
                data <- tibble(
                    frame = 1:total_frames,
                    phase = (frame - 1) / fine_steps,
                    second = floor((frame - 1) / fine_steps),
                    second_frame = (frame - 1) %% fine_steps,
                    value = exp(2i * pi * k * phase / N),
                    x = 0, y = 0,
                    xend = Re(value),
                    yend = Im(value)
                )
                
                glow_points <- tibble(
                    second = 0:(N - 1),
                    value = exp(2i * pi * k * second / N),
                    xend = Re(value),
                    yend = Im(value),
                    frame = list(1:fine_steps)
                ) %>%
                    unnest(frame) %>%
                    mutate(
                        global_frame = (second * fine_steps) + frame,
                        size = 6 * (1 - (frame - 1) / fine_steps),
                        size = pmax(size, 0.5),
                        x = 0, y = 0
                    )
                
                p <- ggplot() +
                    geom_path(data = unit_circle, aes(x, y), colour = "grey") +
                    geom_segment(data = data, aes(x = x, y = y, xend = xend, yend = yend),
                                 arrow = arrow(type = "closed", length = unit(0.2, "cm")),
                                 size = 1, colour = "blue") +
                    geom_point(data = glow_points, aes(x = xend, y = yend, size = size), colour = "orange") +
                    scale_size_identity() +
                    coord_fixed() +
                    theme_minimal() +
                    transition_manual(frames = 1:total_frames)
                
                anim_save(filename, animate(p, nframes = total_frames, fps = fine_steps))
            }
            filename
        })
        
        output$unitCirclePlot <- renderImage({
            k <- as.numeric(input$basis)
            list(src = precomputed_animations[[k + 1]], contentType = 'image/gif')
        }, deleteFile = FALSE)
    })
}
