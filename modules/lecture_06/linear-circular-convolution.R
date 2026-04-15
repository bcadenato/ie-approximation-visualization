linearCircularConvAppUI <- function(id) {
    ns <- NS(id)

    fluidPage(
        tags$head(tags$style(HTML(
            ".shiny-input-container { margin-bottom: 10px; }"
        ))),
        sidebarLayout(
            sidebarPanel(
                textInput(
                    ns("signal_x"),
                    "Input signal x (8 comma-separated integers):",
                    value = "1,2,3,4,5,6,7,8"
                ),
                textInput(
                    ns("signal_h"),
                    "Impulse response h (4 comma-separated integers):",
                    value = "1,0,0,1"
                ),
                actionButton(ns("recalc"), "Recalculate")
            ),
            mainPanel(
                imageOutput(ns("convAnim"), height = "auto")
            )
        )
    )
}

linear_conv_open <- function(x, h) {
    nx <- length(x)
    nh <- length(h)
    y <- numeric(nx + nh - 1)
    for (n in seq_along(y)) {
        acc <- 0
        for (k in seq_len(nx)) {
            j <- n - k + 1
            if (j >= 1 && j <= nh) {
                acc <- acc + x[k] * h[j]
            }
        }
        y[n] <- acc
    }
    y
}

circular_conv_fft <- function(x, h_pad) {
    N <- length(x)
    Re(fft(fft(x) * fft(h_pad), inverse = TRUE) / N)
}

linearCircularConvAppServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        N <- 8L
        M <- 4L
        fine_steps <- 25L
        n_lin <- N + M - 1L

        values_x <- reactiveVal(as.numeric(strsplit("1,2,3,4,5,6,7,8", ",")[[
            1
        ]]))
        values_h <- reactiveVal(as.numeric(strsplit("1,0,0,1", ",")[[1]]))

        observeEvent(input$recalc, {
            x <- suppressWarnings(as.numeric(strsplit(input$signal_x, ",")[[
                1
            ]]))
            h <- suppressWarnings(as.numeric(strsplit(input$signal_h, ",")[[
                1
            ]]))
            if (
                length(x) == N &&
                    length(h) == M &&
                    all(!is.na(x)) &&
                    all(!is.na(h))
            ) {
                values_x(x)
                values_h(h)
            }
        })

        output$convAnim <- renderImage(
            {
                req(length(values_x()) == N, length(values_h()) == M)

                x <- values_x()
                h <- values_h()
                h_rev <- rev(h)

                y_lin <- linear_conv_open(x, h)
                h_pad <- rep(0, N)
                h_pad[seq_len(M)] <- h
                y_circ <- circular_conv_fft(x, h_pad)

                # One amplitude scale for every pin (linear, circular, comparison): same mm per unit.
                s_xh <- max(1e-9, max(abs(c(x, h))))
                s_y <- max(1e-9, max(abs(c(y_lin, y_circ))))
                # Single amplitude for all pins: one unit of signal -> same stem length everywhere.
                scale_pin <- max(s_xh, s_y)

                total_frames <- n_lin * fine_steps
                anim_file <- tempfile(fileext = ".gif")

                # --- Layout: vertical stack — linear (top) → circular (middle) → comparison (bottom) ---
                pin_h <- 2.4
                col_ir <- "#805ad5"

                y_in_fix <- 20.5
                y_lin_out <- 15.5
                y_rule_below_linear <- 13.6

                cx <- 5.2
                cy <- 10.15
                R_x <- 2.05
                R_h <- 1.28
                R_y <- 0.74

                y_circ_title <- cy + R_x + 1.15

                y_cmp_title <- 4.85
                y_cmp <- 0.95

                y_lim_lo <- -0.35
                y_lim_hi <- 23.2

                ang_ring <- seq(0, 2 * pi, length.out = 160)
                circle_guides <- bind_rows(
                    tibble(
                        x = cx + R_x * cos(ang_ring),
                        y = cy + R_x * sin(ang_ring),
                        ring = "outer"
                    ),
                    tibble(
                        x = cx + R_h * cos(ang_ring),
                        y = cy + R_h * sin(ang_ring),
                        ring = "mid"
                    ),
                    tibble(
                        x = cx + R_y * cos(ang_ring),
                        y = cy + R_y * sin(ang_ring),
                        ring = "inner"
                    )
                )

                theta_k <- (seq_len(N) - 1) * 2 * pi / N - pi / 2

                path_data <- map_dfr(seq_len(total_frames), function(frame) {
                    second <- min(floor((frame - 1) / fine_steps), n_lin - 1L)
                    second_frame <- (frame - 1) %% fine_steps
                    frac <- second_frame / fine_steps

                    shift_lin <- second + frac - (M - 1L)
                    rot <- (second + frac) %% N

                    # Linear: input pins at 0..N-1
                    df_x_lin <- tibble(
                        section = "linear",
                        layer = "x_fixed",
                        xpos = 0:(N - 1),
                        y0 = y_in_fix,
                        y1 = y_in_fix + (x / scale_pin) * pin_h,
                        val = x,
                        frame = frame
                    )

                    # Linear: sliding reversed IR (M pins), same baseline as x
                    df_h_lin <- tibble(
                        section = "linear",
                        layer = "h_slide",
                        xpos = shift_lin + (0:(M - 1)),
                        y0 = y_in_fix,
                        y1 = y_in_fix + (h_rev / scale_pin) * pin_h,
                        val = h_rev,
                        frame = frame
                    )

                    # Linear: partial output 0..second
                    if (second >= 0) {
                        idx_out <- 0:second
                        df_y_lin <- tibble(
                            section = "linear",
                            layer = "y_partial",
                            xpos = idx_out,
                            y0 = y_lin_out,
                            y1 = y_lin_out +
                                (y_lin[idx_out + 1] / scale_pin) * pin_h,
                            val = y_lin[idx_out + 1],
                            frame = frame
                        )
                        glow_lin <- tibble(
                            section = "linear",
                            layer = "y_glow",
                            xpos = second,
                            y0 = y_lin_out,
                            y1 = y_lin_out +
                                (y_lin[second + 1] / scale_pin) * pin_h,
                            size = 6 * (1 - second_frame / fine_steps),
                            frame = frame
                        ) %>%
                            mutate(size = pmax(size, 0.5))
                    } else {
                        df_y_lin <- tibble(
                            section = "linear",
                            layer = "y_partial",
                            xpos = numeric(0),
                            y0 = numeric(0),
                            y1 = numeric(0),
                            val = numeric(0),
                            frame = frame
                        )
                        glow_lin <- tibble(
                            section = "linear",
                            layer = "y_glow",
                            xpos = numeric(0),
                            y0 = numeric(0),
                            y1 = numeric(0),
                            size = numeric(0),
                            frame = frame
                        )
                    }

                    # Fixed x on outer circle (Cartesian, offset to the right of linear rails)
                    df_x_circ <- tibble(
                        section = "circular",
                        layer = "x_fixed",
                        k = 0:(N - 1),
                        th = theta_k,
                        val = x,
                        x0 = cx + R_x * cos(th),
                        y0 = cy + R_x * sin(th),
                        x1 = cx + (R_x + (x / scale_pin) * pin_h) * cos(th),
                        y1 = cy + (R_x + (x / scale_pin) * pin_h) * sin(th),
                        frame = frame
                    )

                    # Inner: reversed h at continuous angles (smooth rotation on N-fold symmetry)
                    th_h <- (rot + (0:(M - 1))) * 2 * pi / N - pi / 2
                    df_h_circ <- tibble(
                        section = "circular",
                        layer = "h_slide",
                        j = seq_len(M),
                        th = th_h,
                        val = h_rev,
                        x0 = cx + R_h * cos(th_h),
                        y0 = cy + R_h * sin(th_h),
                        x1 = cx +
                            (R_h + (h_rev / scale_pin) * pin_h) * cos(th_h),
                        y1 = cy +
                            (R_h + (h_rev / scale_pin) * pin_h) * sin(th_h),
                        frame = frame
                    )

                    sc <- min(second, N - 1L)
                    idx_yc <- 0:sc
                    th_y <- theta_k[idx_yc + 1]
                    df_y_circ <- tibble(
                        section = "circular",
                        layer = "y_partial",
                        k = idx_yc,
                        val = y_circ[idx_yc + 1],
                        x0 = cx + R_y * cos(th_y),
                        y0 = cy + R_y * sin(th_y),
                        x1 = cx +
                            (R_y + (y_circ[idx_yc + 1] / scale_pin) * pin_h) *
                                cos(th_y),
                        y1 = cy +
                            (R_y + (y_circ[idx_yc + 1] / scale_pin) * pin_h) *
                                sin(th_y),
                        frame = frame
                    )

                    glow_circ <- tibble(
                        section = "circular",
                        layer = "y_glow",
                        th = theta_k[sc + 1],
                        size = 6 * (1 - second_frame / fine_steps),
                        frame = frame
                    ) %>%
                        mutate(
                            size = pmax(size, 0.5),
                            x1 = cx +
                                (R_y + (y_circ[sc + 1] / scale_pin) * pin_h) *
                                    cos(th),
                            y1 = cy +
                                (R_y + (y_circ[sc + 1] / scale_pin) * pin_h) *
                                    sin(th)
                        )

                    # Comparison: linear (blue) and circular (orange) on same baseline
                    idx_cmp <- 0:min(second, n_lin - 1L)
                    df_cmp_lin <- tibble(
                        section = "compare",
                        layer = "y_lin",
                        xpos = idx_cmp,
                        y0 = y_cmp,
                        y1 = y_cmp + (y_lin[idx_cmp + 1] / scale_pin) * pin_h,
                        val = y_lin[idx_cmp + 1],
                        frame = frame
                    )
                    sc2 <- min(second, N - 1L)
                    idx_c2 <- 0:sc2
                    off <- 0.22
                    df_cmp_circ <- tibble(
                        section = "compare",
                        layer = "y_circ",
                        xpos = idx_c2 + off,
                        y0 = y_cmp,
                        y1 = y_cmp + (y_circ[idx_c2 + 1] / scale_pin) * pin_h,
                        val = y_circ[idx_c2 + 1],
                        frame = frame
                    )

                    bind_rows(
                        mutate(df_x_lin, plot = "seg_lin_x"),
                        mutate(df_h_lin, plot = "seg_lin_h"),
                        mutate(df_y_lin, plot = "seg_lin_y"),
                        mutate(glow_lin, plot = "glow_seg"),
                        mutate(df_x_circ, plot = "seg_circ_x"),
                        mutate(df_h_circ, plot = "seg_circ_h"),
                        mutate(df_y_circ, plot = "seg_circ_y"),
                        mutate(glow_circ, plot = "glow_circ"),
                        mutate(df_cmp_lin, plot = "cmp_lin"),
                        mutate(df_cmp_circ, plot = "cmp_circ")
                    )
                })

                p <- ggplot() +
                    geom_path(
                        data = circle_guides,
                        aes(x = x, y = y, group = ring),
                        colour = "grey85",
                        linewidth = 0.28
                    ) +
                    geom_segment(
                        data = path_data %>% dplyr::filter(plot == "seg_lin_x"),
                        aes(x = xpos, xend = xpos, y = y0, yend = y1),
                        linewidth = 0.6,
                        colour = "#2c5282"
                    ) +
                    geom_point(
                        data = path_data %>% dplyr::filter(plot == "seg_lin_x"),
                        aes(x = xpos, y = y1),
                        size = 2.2,
                        colour = "#2c5282"
                    ) +
                    geom_segment(
                        data = path_data %>% dplyr::filter(plot == "seg_lin_h"),
                        aes(x = xpos, xend = xpos, y = y0, yend = y1),
                        linewidth = 0.6,
                        colour = col_ir
                    ) +
                    geom_point(
                        data = path_data %>% dplyr::filter(plot == "seg_lin_h"),
                        aes(x = xpos, y = y1),
                        size = 2.2,
                        colour = col_ir
                    ) +
                    geom_segment(
                        data = path_data %>% dplyr::filter(plot == "seg_lin_y"),
                        aes(x = xpos, xend = xpos, y = y0, yend = y1),
                        linewidth = 0.6,
                        colour = "#2c5282"
                    ) +
                    geom_point(
                        data = path_data %>% dplyr::filter(plot == "seg_lin_y"),
                        aes(x = xpos, y = y1),
                        size = 2.2,
                        colour = "#2c5282"
                    ) +
                    geom_segment(
                        data = path_data %>% dplyr::filter(plot == "glow_seg"),
                        aes(
                            x = xpos,
                            xend = xpos,
                            y = y0,
                            yend = y1,
                            linewidth = size
                        ),
                        colour = "#dd6b20",
                        lineend = "round"
                    ) +
                    scale_linewidth_identity() +
                    geom_segment(
                        data = path_data %>%
                            dplyr::filter(plot == "seg_circ_x"),
                        aes(x = x0, y = y0, xend = x1, yend = y1),
                        linewidth = 0.6,
                        colour = "#2c5282"
                    ) +
                    geom_point(
                        data = path_data %>%
                            dplyr::filter(plot == "seg_circ_x"),
                        aes(x = x1, y = y1),
                        size = 2.2,
                        colour = "#2c5282"
                    ) +
                    geom_segment(
                        data = path_data %>%
                            dplyr::filter(plot == "seg_circ_h"),
                        aes(x = x0, y = y0, xend = x1, yend = y1),
                        linewidth = 0.6,
                        colour = col_ir
                    ) +
                    geom_point(
                        data = path_data %>%
                            dplyr::filter(plot == "seg_circ_h"),
                        aes(x = x1, y = y1),
                        size = 2.2,
                        colour = col_ir
                    ) +
                    geom_segment(
                        data = path_data %>%
                            dplyr::filter(plot == "seg_circ_y"),
                        aes(x = x0, y = y0, xend = x1, yend = y1),
                        linewidth = 0.6,
                        colour = "#2f855a"
                    ) +
                    geom_point(
                        data = path_data %>%
                            dplyr::filter(plot == "seg_circ_y"),
                        aes(x = x1, y = y1),
                        size = 2.2,
                        colour = "#2f855a"
                    ) +
                    geom_point(
                        data = path_data %>% dplyr::filter(plot == "glow_circ"),
                        aes(x = x1, y = y1, size = size),
                        colour = "#dd6b20"
                    ) +
                    scale_size_identity() +
                    geom_segment(
                        data = path_data %>% dplyr::filter(plot == "cmp_lin"),
                        aes(x = xpos, xend = xpos, y = y0, yend = y1),
                        linewidth = 0.65,
                        colour = "#2b6cb0"
                    ) +
                    geom_point(
                        data = path_data %>% dplyr::filter(plot == "cmp_lin"),
                        aes(x = xpos, y = y1),
                        size = 2.4,
                        colour = "#2b6cb0"
                    ) +
                    geom_segment(
                        data = path_data %>% dplyr::filter(plot == "cmp_circ"),
                        aes(x = xpos, xend = xpos, y = y0, yend = y1),
                        linewidth = 0.65,
                        colour = "#c05621"
                    ) +
                    geom_point(
                        data = path_data %>% dplyr::filter(plot == "cmp_circ"),
                        aes(x = xpos, y = y1),
                        size = 2.4,
                        shape = 21,
                        fill = "#c05621",
                        colour = "#c05621"
                    ) +
                    annotate(
                        "segment",
                        x = -0.8,
                        xend = n_lin + 0.5,
                        y = y_rule_below_linear,
                        yend = y_rule_below_linear,
                        colour = "gray70",
                        linewidth = 0.35
                    ) +
                    annotate(
                        "text",
                        x = (n_lin - 1) / 2,
                        y = 22.1,
                        label = "Linear convolution: x (blue) and reversed h (purple) on one row; output accumulates below",
                        size = 3.2,
                        colour = "gray30"
                    ) +
                    annotate(
                        "text",
                        x = cx,
                        y = y_circ_title,
                        label = "Circular convolution: x on outer ring, reversed h on middle ring, output on inner ring",
                        size = 3.2,
                        colour = "gray30"
                    ) +
                    annotate(
                        "text",
                        x = (n_lin - 1) / 2,
                        y = y_cmp_title,
                        label = "Comparison: linear (blue) vs circular (orange), offset for clarity",
                        size = 3.2,
                        colour = "gray30"
                    ) +
                    coord_fixed(
                        xlim = c(-1.5, 12.5),
                        ylim = c(y_lim_lo, y_lim_hi),
                        ratio = 1,
                        clip = "off",
                        expand = FALSE
                    ) +
                    theme_void() +
                    theme(
                        plot.margin = margin(12, 12, 12, 12)
                    ) +
                    transition_manual(frames = frame)

                animated_plot <- animate(
                    p,
                    width = 900,
                    height = 1080,
                    res = 120,
                    nframes = total_frames,
                    fps = fine_steps
                )
                anim_save(anim_file, animated_plot)

                list(src = anim_file, contentType = "image/gif")
            },
            deleteFile = TRUE
        )
    })
}
