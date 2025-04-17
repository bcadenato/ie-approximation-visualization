
eulerCircleAppUI <- function(id) {
    ns <- NS(id)
    
    pi_breaks <- seq(-4*pi, 4*pi, by = pi/2)
    pi_labels <- sapply(pi_breaks, function(x) {
        if (x == 0) return("0")
        n <- x / pi
        if (n == 1) return("π")
        if (n == -1) return("-π")
        return(paste0(n, "π"))
    })
    
    fluidPage(
        titlePanel("Euler's Formula - Unit Circle"),
        sidebarLayout(
            sidebarPanel(
                sliderInput(ns("theta"), "θ (radians):",
                            min = floor(-4 * pi), max = ceiling(4 * pi), value = 0, step = 0.01)
            ),
            mainPanel(
                plotOutput(ns("unitCirclePlot")),
                plotOutput(ns("realPartPlot")),
                plotOutput(ns("imagPartPlot"))
            )
        )
    )
}

eulerCircleAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        pi_breaks <- seq(-4*pi, 4*pi, by = pi/2)
        pi_labels <- sapply(pi_breaks, function(x) {
            if (x == 0) return("0")
            n <- x / pi
            if (n == 1) return("π")
            if (n == -1) return("-π")
            return(paste0(n, "π"))
        })
        
        output$unitCirclePlot <- renderPlot({
            theta <- input$theta
            df <- tibble(x = cos(theta), y = sin(theta))
            circle <- tibble(t = seq(0, 2*pi, length.out = 500), x = cos(t), y = sin(t))
            
            ggplot() +
                geom_path(data = circle, aes(x, y), color = "grey") +
                geom_point(data = df, aes(x, y), color = "blue", size = 4) +
                geom_segment(data = df, aes(x = 0, y = 0, xend = x, yend = 0), color = "blue") +
                geom_segment(data = df, aes(x = 0, y = 0, xend = 0, yend = y), color = "orange") +
                geom_segment(data = df, aes(x = 0, y = y, xend = x, yend = y), color = "blue", linetype = "dotted") +
                geom_segment(data = df, aes(x = x, y = 0, xend = x, yend = y), color = "orange", linetype = "dotted") +
                coord_fixed() +
                labs(title = "Unit Circle", x = "Real", y = "Imaginary") +
                theme_minimal()
        })
        
        output$realPartPlot <- renderPlot({
            t_seq <- seq(-4*pi, 4*pi, length.out = 500)
            df <- tibble(t = t_seq, real = cos(t))
            
            ggplot(df, aes(t, real)) +
                geom_line(color = "blue") +
                geom_vline(xintercept = input$theta, linetype = "dashed", color = "red") +
                scale_x_continuous(breaks = pi_breaks, labels = pi_labels) +
                labs(title = expression("Real Part of "*e^{i*theta}), x = "Time", y = "Real") +
                theme_minimal()
        })
        
        output$imagPartPlot <- renderPlot({
            t_seq <- seq(-4*pi, 4*pi, length.out = 500)
            df <- tibble(t = t_seq, imag = sin(t))
            
            ggplot(df, aes(t, imag)) +
                geom_line(color = "orange") +
                geom_vline(xintercept = input$theta, linetype = "dashed", color = "red") +
                scale_x_continuous(breaks = pi_breaks, labels = pi_labels) +
                labs(title = expression("Imaginary Part of "*e^{i*theta}), x = "Time", y = "Real") +
                theme_minimal()
        })
    })
}
