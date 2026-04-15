frequencyShiftingAppUI <- function(id) {
    ns <- NS(id)

    fluidPage(
        titlePanel("Exponential Sinusoids and Their Product"),
        fluidRow(
            column(
                4,
                sliderInput(
                    ns("f1_hz"),
                    "Frequency f_1 (Hz):",
                    min = 1,
                    max = 60,
                    value = 8,
                    step = 1
                ),
                sliderInput(
                    ns("f2_hz"),
                    "Frequency f_2 (Hz):",
                    min = 1,
                    max = 60,
                    value = 15,
                    step = 1
                )
            ),
            column(
                8,
                p(
                    "Complex exponentials e^{i*2*pi*f*t}. Their product is e^{i*2*pi*(f_1+f_2)*t}, ",
                    "so the real part oscillates at the sum frequency f_1 + f_2.",
                    style = "color: #555;"
                )
            )
        ),
        fluidRow(
            column(4, plotOutput(ns("plot_exp_f1"))),
            column(4, plotOutput(ns("plot_exp_f2"))),
            column(4, plotOutput(ns("plot_product_real")))
        )
    )
}

frequencyShiftingAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        exponentials <- reactive({
            t <- seq(0, 1, length.out = 1000)
            f1 <- input$f1_hz
            f2 <- input$f2_hz
            z1 <- exp(1i * 2 * pi * f1 * t)
            z2 <- exp(1i * 2 * pi * f2 * t)
            list(
                t = t,
                re1 = Re(z1),
                re2 = Re(z2),
                re_prod = Re(z1 * z2)
            )
        })

        output$plot_exp_f1 <- renderPlot({
            d <- exponentials()
            ggplot(data.frame(t = d$t, y = d$re1), aes(x = t, y = y)) +
                geom_line(color = "blue") +
                labs(
                    title = expression("Re" * "[" * e^{i * 2 * pi * f[1] * t} * "]"),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                theme_minimal()
        })

        output$plot_exp_f2 <- renderPlot({
            d <- exponentials()
            ggplot(data.frame(t = d$t, y = d$re2), aes(x = t, y = y)) +
                geom_line(color = "darkorange") +
                labs(
                    title = expression("Re" * "[" * e^{i * 2 * pi * f[2] * t} * "]"),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                theme_minimal()
        })

        output$plot_product_real <- renderPlot({
            d <- exponentials()
            ggplot(data.frame(t = d$t, y = d$re_prod), aes(x = t, y = y)) +
                geom_line(color = "grey30") +
                labs(
                    title = expression("Re" * "[" * e^{i * 2 * pi * f[1] * t} * e^{i * 2 * pi * f[2] * t} * "]"),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                theme_minimal()
        })
    })
}
