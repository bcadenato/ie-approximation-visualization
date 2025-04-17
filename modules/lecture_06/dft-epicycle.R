
dftEpicycleAppUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        tags$head(tags$style(HTML(".shiny-input-container { margin-bottom: 10px; }"))),
        sidebarLayout(
            sidebarPanel(
                textInput(ns("signal"), "Enter Signal (comma-separated):", value = "1,2,3,4,5,6,7,8"), 
                actionButton(ns("recalc"), "Recalculate")
            ),
            mainPanel(
                imageOutput(ns("sumVectorPlot"))
            )
        )
    )
}

dftEpicycleAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        N <- 8
        fine_steps <- 25
        total_frames <- N * fine_steps
        
        values <- reactiveVal(c(1, 2, 3, 4, 5, 6, 7, 8))
        
        observeEvent(input$recalc, {
            signal <- as.numeric(strsplit(input$signal, ",")[[1]])
            if (length(signal) == N) {
                values(signal)
            }
        })
        
        output$sumVectorPlot <- renderImage({
            req(length(values()) == N)
            x_n <- values()
            X_k <- fft(x_n) / N
            anim_file <- tempfile(fileext = ".gif")
            
            path_data <- map_dfr(1:total_frames, function(frame) {
                phase <- (frame - 1) / fine_steps
                vectors <- tibble(
                    k = 0:(N-1),
                    basis = exp(2i * pi * k * phase / N),
                    coeff = X_k,
                    vector = coeff * basis
                ) %>%
                    mutate(
                        x_offset = lag(cumsum(Re(vector)), default = 0),
                        y_offset = lag(cumsum(Im(vector)), default = 0),
                        xend = x_offset + Re(vector),
                        yend = y_offset + Im(vector),
                        frame = frame
                    )
                vectors
            })
            
            glow_points_size <- path_data %>%
                group_by(frame) %>%
                dplyr::filter(k == max(k)) %>%
                ungroup() %>%
                mutate(
                    second = floor((frame - 1) / fine_steps),
                    second_frame = (frame - 1) %% fine_steps,
                    size = 6 * (1 - second_frame / fine_steps),
                    size = pmax(size, 0.5)
                ) 
            
            glow_points_coords <- glow_points_size %>%
                dplyr::filter(second_frame == 0) %>%
                select(second, xend, yend)
            
            glow_points <- glow_points_coords %>%
                left_join(glow_points_size %>% select(frame, second, size), by = join_by(second))
            
            p <- ggplot() +
                geom_segment(data = path_data, aes(x = x_offset, y = y_offset, xend = xend, yend = yend, group = frame),
                             arrow = arrow(type = "closed", length = unit(0.1, "cm")),
                             size = 0.25, colour = "blue") +
                geom_point(data = glow_points, aes(x = xend, y = yend, size = size), colour = "orange") +
                scale_size_identity() +
                coord_fixed() +
                labs(x = "Value (Real)", y = "Value (Imaginary)") +
                theme_minimal() +
                transition_manual(frames = frame)
            
            animated_plot <- animate(p, width = 800, height = 600, res = 200, nframes = total_frames, fps = fine_steps)
            anim_save(anim_file, animated_plot)
            
            list(src = anim_file, contentType = 'image/gif')
        }, deleteFile = TRUE)
    })
}
