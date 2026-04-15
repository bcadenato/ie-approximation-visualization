
# Ideal passband filter (difference of two sinc lowpass impulse responses) and
# convolution with a two-tone sinusoidal input.

passbandLtiAppUI <- function(id) {
    ns <- NS(id)

    fluidPage(
        withMathJax(),
        titlePanel("Passband filter and convolution (ideal LTI)"),
        fluidRow(
            column(
                4,
                sliderInput(
                    ns("f1_hz"),
                    HTML("Sinusoid frequency f<sub>1</sub> (Hz):"),
                    min = 1,
                    max = 20,
                    value = 5,
                    step = 1
                ),
                sliderInput(
                    ns("f2_hz"),
                    HTML("Sinusoid frequency f<sub>2</sub> (Hz):"),
                    min = 1,
                    max = 20,
                    value = 12,
                    step = 1
                ),
                sliderInput(
                    ns("fl_hz"),
                    HTML("Passband lower cutoff f<sub>l</sub> (Hz):"),
                    min = 1,
                    max = 20,
                    value = 4,
                    step = 1
                ),
                sliderInput(
                    ns("fh_hz"),
                    HTML("Passband upper cutoff f<sub>h</sub> (Hz):"),
                    min = 1,
                    max = 20,
                    value = 15,
                    step = 1
                )
            ),
            column(
                8,
                p(
                    "Input x(t) = sin(2\u03c0 f_1 t) + sin(2\u03c0 f_2 t). ",
                    "Ideal bandpass impulse response h(t) is the difference of two ideal lowpass sinc responses ",
                    "with cutoffs f_l and f_h. Output y(t) = (x * h)(t) is computed by discrete convolution ",
                    "on a uniform grid from \u22123 s to 3 s.",
                    style = "color: #555;"
                )
            )
        ),
        fluidRow(
            column(12, plotOutput(ns("plot_input")))
        ),
        fluidRow(
            column(
                12,
                tags$div(
                    style = "text-align: center; margin: 16px 0 8px 0;",
                    tags$p(
                        style = "color: #333;",
                        HTML(
                            paste0(
                                "Ideal bandpass impulse response (difference of two ideal lowpass responses). ",
                                "$$h(t) = 2 f_h \\, \\mathrm{sinc}(2 f_h t) - 2 f_l \\, \\mathrm{sinc}(2 f_l t), \\qquad ",
                                "\\mathrm{sinc}(x) = \\frac{\\sin(\\pi x)}{\\pi x}$$"
                            )
                        )
                    )
                ),
                plotOutput(ns("plot_ir"))
            )
        ),
        fluidRow(
            column(12, plotOutput(ns("plot_output")))
        )
    )
}

# sinc(x) = sin(pi*x)/(pi*x); at 0 limit is 1
sinc_pi <- function(x) {
    ifelse(abs(x) < 1e-14, 1, sin(pi * x) / (pi * x))
}

ideal_lowpass_ir <- function(t, f_cut_hz) {
    2 * f_cut_hz * sinc_pi(2 * f_cut_hz * t)
}

ideal_bandpass_ir <- function(t, f_l_hz, f_h_hz) {
    ideal_lowpass_ir(t, f_h_hz) - ideal_lowpass_ir(t, f_l_hz)
}

passbandLtiAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        L <- 3

        ordered_cutoffs <- reactive({
            fl <- min(input$fl_hz, input$fh_hz)
            fh <- max(input$fl_hz, input$fh_hz)
            list(f_l = fl, f_h = fh)
        })

        time_grid <- reactive({
            dt <- 0.001
            t <- seq(-L, L, by = dt)
            list(t = t, dt = dt)
        })

        input_signal <- reactive({
            g <- time_grid()
            f1 <- input$f1_hz
            f2 <- input$f2_hz
            x <- sin(2 * pi * f1 * g$t) + sin(2 * pi * f2 * g$t)
            list(t = g$t, dt = g$dt, x = x)
        })

        impulse_response <- reactive({
            g <- time_grid()
            co <- ordered_cutoffs()
            h <- ideal_bandpass_ir(g$t, co$f_l, co$f_h)
            list(t = g$t, dt = g$dt, h = h, f_l = co$f_l, f_h = co$f_h)
        })

        convolution_result <- reactive({
            d_in <- input_signal()
            d_ir <- impulse_response()
            z <- convolve(d_in$x, d_ir$h, type = "open") * d_in$dt
            t0 <- min(d_in$t) + min(d_ir$t)
            t_z <- t0 + (seq_along(z) - 1) * d_in$dt
            keep <- which(t_z >= -L & t_z <= L)
            list(t = t_z[keep], y = z[keep])
        })

        output$plot_input <- renderPlot({
            d <- input_signal()
            ggplot(data.frame(t = d$t, x = d$x), aes(x = t, y = x)) +
                geom_line(color = "steelblue") +
                labs(
                    title = expression(x(t) == sin(2 * pi * f[1] * t) + sin(2 * pi * f[2] * t)),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                coord_cartesian(xlim = c(-L, L)) +
                theme_minimal()
        })

        output$plot_ir <- renderPlot({
            d <- impulse_response()
            ggplot(data.frame(t = d$t, h = d$h), aes(x = t, y = h)) +
                geom_line(color = "darkred") +
                labs(
                    title = expression("Impulse response" ~ h(t)),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                coord_cartesian(xlim = c(-L, L)) +
                theme_minimal()
        })

        output$plot_output <- renderPlot({
            d <- convolution_result()
            ggplot(data.frame(t = d$t, y = d$y), aes(x = t, y = y)) +
                geom_line(color = "grey25") +
                labs(
                    title = "Output y(t): convolution of x(t) with h(t)",
                    subtitle = "Discrete approximation to (x * h)(t) on [-3 s, 3 s]",
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                coord_cartesian(xlim = c(-L, L)) +
                theme_minimal()
        })
    })
}
