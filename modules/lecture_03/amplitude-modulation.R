amplitudeModulationAppUI <- function(id) {
    ns <- NS(id)

    fluidPage(
        titlePanel("Amplitude Modulation"),
        fluidRow(
            column(
                4,
                selectInput(
                    ns("signal_type"),
                    "Message signal x(t):",
                    choices = c(
                        "Cosine (10 Hz)" = "cosine",
                        "Chirp" = "chirp",
                        "Pulse train (10 Hz)" = "pulse"
                    )
                ),
                sliderInput(
                    ns("carrier_amp"),
                    "Carrier amplitude A_c:",
                    min = 0.2,
                    max = 3,
                    value = 1.5,
                    step = 0.1
                ),
                sliderInput(
                    ns("carrier_freq_hz"),
                    "Carrier frequency f_c (Hz):",
                    min = 20,
                    max = 150,
                    value = 80,
                    step = 1
                )
            ),
            column(
                8,
                p(
                    "Modulated signal: (A_c + x(t)) * cos(2*pi*f_c*t). ",
                    "x(t) is the message; the carrier is a cosine at f_c with implicit amplitude 1, scaled by the envelope (A_c + x(t)).",
                    style = "color: #555;"
                )
            )
        ),
        fluidRow(
            column(4, plotOutput(ns("plot_input"))),
            column(4, plotOutput(ns("plot_carrier"))),
            column(4, plotOutput(ns("plot_modulated")))
        ),
        fluidRow(
            column(6, plotOutput(ns("plot_esd_input"))),
            column(6, plotOutput(ns("plot_esd_modulated")))
        )
    )
}

amplitudeModulationAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        input_signal <- reactive({
            t <- seq(0, 1, length.out = 1000)
            x <- switch(
                input$signal_type,
                cosine = cos(2 * pi * 10 * t),
                chirp = signal::chirp(t, f0 = 2, f1 = 50, t1 = max(t)),
                pulse = as.numeric((t %% 0.1) < 0.05)
            )
            list(t = t, x = x)
        })

        carrier <- reactive({
            d <- input_signal()
            fc <- input$carrier_freq_hz
            cos(2 * pi * fc * d$t)
        })

        modulated <- reactive({
            d <- input_signal()
            Ac <- input$carrier_amp
            (Ac + d$x) * carrier()
        })

        # Two-sided energy spectrum: DFT bins folded to (-fs/2, fs/2], sorted by frequency
        esd_two_sided <- function(x, t) {
            n <- length(x)
            fs <- 1 / (t[2] - t[1])
            Y <- stats::fft(x)
            P <- Mod(Y)^2 / n
            j <- seq_len(n)
            freq <- (j - 1) * fs / n
            freq[freq > fs / 2] <- freq[freq > fs / 2] - fs
            o <- order(freq)
            tibble::tibble(freq = freq[o], esd = P[o])
        }

        output$plot_input <- renderPlot({
            d <- input_signal()
            ggplot(data.frame(t = d$t, x = d$x), aes(x = t, y = x)) +
                geom_line(color = "blue") +
                labs(
                    title = expression("Message" ~ x(t)),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                theme_minimal()
        })

        output$plot_carrier <- renderPlot({
            d <- input_signal()
            car <- carrier()
            ggplot(data.frame(t = d$t, carrier = car), aes(x = t, y = carrier)) +
                geom_line(color = "orange") +
                labs(
                    title = expression("Carrier" ~ cos(2 * pi * f[c] * t)),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                theme_minimal()
        })

        output$plot_modulated <- renderPlot({
            d <- input_signal()
            y <- modulated()
            ggplot(data.frame(t = d$t, y = y), aes(x = t, y = y)) +
                geom_line(color = "grey30") +
                labs(
                    title = expression((A[c] + x(t)) * cos(2 * pi * f[c] * t)),
                    x = expression(t ~ "(s)"),
                    y = "Amplitude"
                ) +
                theme_minimal()
        })

        output$plot_esd_input <- renderPlot({
            d <- input_signal()
            spec <- esd_two_sided(d$x, d$t)
            ggplot(spec, aes(x = freq, y = esd)) +
                geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.45) +
                geom_line(color = "blue") +
                labs(
                    title = expression("Energy spectral density" ~ ":" ~ x(t) ~ "(two-sided)"),
                    x = expression(f ~ "(Hz)"),
                    y = expression(frac(group("|", X * (f), "|")^2, N))
                ) +
                theme_minimal()
        })

        output$plot_esd_modulated <- renderPlot({
            d <- input_signal()
            y <- modulated()
            spec <- esd_two_sided(y, d$t)
            ggplot(spec, aes(x = freq, y = esd)) +
                geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.45) +
                geom_line(color = "orange") +
                labs(
                    title = expression("Energy spectral density" ~ ":" ~ "modulated" ~ "(two-sided)"),
                    x = expression(f ~ "(Hz)"),
                    y = expression(frac(group("|", Y * (f), "|")^2, N))
                ) +
                theme_minimal()
        })
    })
}
