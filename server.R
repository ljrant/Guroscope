library(shiny)
library(plotly)
library(vegan)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)

# ---------- Load precomputed files ----------
precomputed     <- readRDS("distances.rds")
metadata        <- readRDS("metadata.rds")
decoder_scores  <- tryCatch(readRDS("decoder_scores.rds"), error = function(e) NULL)

# Explicit variable lists (stable)
graded_vars <- c(
  "Galaxy Brainness","Cultishness","Anti-Establishment","Grievance Mongering",
  "Self-Aggrandisement and Narcicissm","Cassandra Complex","Revolutionary Theories",
  "Pseudo Profound Bullshit","Conspiracy Mongering","Profiteering","Moral Grandstanding"
)
binary_vars <- c(
  "Monomania","Shilling Supplements","Broicity","Charisma","Neologism",
  "Strategic Disclaimers","Rebranding Theories","Loquaciousness","Never admitting error"
)



server <- function(input, output, session) {
  
  # ---------------------- Selection state ----------------------
  all_subjects <- metadata$Subject
  set.seed(42)
  selected_subjects <- reactiveVal(sample(all_subjects, 2))
  
  # ---------------------- Distance & PCoA ----------------------
  current_dist <- reactive({
    method <- tolower(if (is.null(input$dist_method)) "manhattan" else input$dist_method)
    precomputed[[method]]
  })
  current_ord <- reactive({
    vegan::capscale(as.dist(current_dist()) ~ 1, add = FALSE)
  })
  
  # ---------------------- Display settings (below ordination, hidden by default) ----------------------
  output$settingsPanel <- renderUI({
    s <- scores(current_ord(), display = "sites")
    axes <- paste0("PCoA", seq_len(ncol(s)))
    
    cur_dist <- isolate(input$dist_method)
    cur_x    <- isolate(input$axis1)
    cur_y    <- isolate(input$axis2)
    
    default_x <- if ("PCoA2" %in% axes) "PCoA2" else tail(axes, 1)
    default_y <- "PCoA1"
    
    tags$div(class = "settings",
             tags$style(HTML("
      .settings .form-select, .settings .form-control, .settings .form-check {
        margin-bottom: 4px !important;
      }
      .settings .form-select, .settings .form-control {
        padding-top: 2px; padding-bottom: 2px; height: auto;
      }
      .settings .row { margin-left: -6px; margin-right: -6px; }
      .settings [class*='col-'] { padding-left: 6px; padding-right: 6px; }
      .settings label { margin-bottom: 2px; font-size: 12px; color: #ddd; }
      .settings .form-check-label { font-size: 12px; }
      .settings .form-select, .settings .form-control { font-size: 12px; }
    ")),
             
             # Visible, high-contrast toggle
             tags$button(
               class = "btn btn-sm btn-warning fw-semibold",
               type = "button",
               `data-bs-toggle` = "collapse",
               `data-bs-target` = "#settingsBox",
               "⚙ Display settings"
             ),
             
             # Hidden by default (remove 'show')
             div(
               id = "settingsBox", class = "collapse",
               fluidRow(
                 column(4,
                        selectInput(
                          "dist_method", "Similarity metric",
                          choices = c("Euclidean","Manhattan","Cosine"),
                          selected = if (!is.null(cur_dist)) cur_dist else "Manhattan"
                        )
                 ),
                 column(4,
                        radioButtons("method", "Ordination", choices = c("PCA"), inline = TRUE)
                 ),
                 column(4,
                        selectInput(
                          "axis1", "X axis", choices = axes,
                          selected = if (!is.null(cur_x) && cur_x %in% axes) cur_x else default_x
                        ),
                        selectInput(
                          "axis2", "Y axis", choices = axes,
                          selected = if (!is.null(cur_y) && cur_y %in% axes) cur_y else default_y
                        )
                 )
               ),
               div(class="d-flex align-items-center justify-content-between",
                   selectInput(
                     "selectedTrait", "Highlight bonus trait",
                     choices = c("None", binary_vars),
                     selected = isolate(input$selectedTrait) %||% "None",
                     width = "50%"
                   ),
                   div(style="white-space:nowrap;",
                       checkboxInput("show_traits", "Show traits under chart",
                                     value = isTRUE(isolate(input$show_traits)))
                   )
               )
             )
    )
  })
  
  
  
  # ---------------------- Ordination data + plot ----------------------
  site_scores <- reactive({
    s <- scores(current_ord(), display = "sites")
    colnames(s) <- paste0("PCoA", seq_len(ncol(s)))
    df <- as.data.frame(s)
    df$Subject <- rownames(s)
    df <- left_join(df, metadata[, c("Subject","Percentage","Average")], by = "Subject")
    df$TraitColor <- if (!is.null(input$selectedTrait) && input$selectedTrait != "None") {
      present <- metadata[[input$selectedTrait]][match(df$Subject, metadata$Subject)] == 1
      ifelse(present, input$selectedTrait, "None")
    } else "None"
    df$IsSelected <- df$Subject %in% selected_subjects()
    df
  })
  
  output$skyPlot <- renderPlotly({
    df <- site_scores()
    if (nrow(df) == 0) return(NULL)
    
    # pick safe axes
    axes_all <- grep("^PCoA", names(df), value = TRUE)
    xvar <- if (!is.null(input$axis1) && input$axis1 %in% axes_all) input$axis1 else (if ("PCoA2" %in% axes_all) "PCoA2" else axes_all[1])
    yvar <- if (!is.null(input$axis2) && input$axis2 %in% axes_all) input$axis2 else (if ("PCoA1" %in% axes_all) "PCoA1" else axes_all[min(2, length(axes_all))])
    
    # small padding around data range (so labels don't clip)
    xr <- range(df[[xvar]], na.rm = TRUE); xpad <- diff(xr) * 0.04
    yr <- range(df[[yvar]], na.rm = TRUE); ypad <- diff(yr) * 0.06
    xrange <- c(xr[1] - xpad, xr[2] + xpad)
    yrange <- c(yr[1] - ypad, yr[2] + ypad)
    
    # palette
    palette <- c("None" = "gray60")
    if (!is.null(input$selectedTrait) && input$selectedTrait != "None")
      palette[input$selectedTrait] <- "orange"
    
    # ggplot with star markers; hover text includes Guru %
    p <- ggplot(df, aes_string(x = xvar, y = yvar)) +
      geom_point(
        aes(
          color = TraitColor,
          key   = Subject,                               # used for clicks
          text  = paste0(Subject,
                         "<br>Gurometer: ",
                         sprintf("%.0f%%", Percentage))   # used for hover
        ),
        shape = 8, size = 3.8, alpha = 0.95
      ) +
      scale_color_manual(values = palette) +
      # NOTE: no coord_equal() -> free scaling
      scale_x_continuous(expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = 0)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "white", fill = NA, linewidth = .5),
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        axis.title = element_blank(),
        axis.text  = element_text(color = "white")
      )
    
    # cyan ring on selected
    sel <- subset(df, IsSelected)
    if (nrow(sel) > 0) {
      p <- p + geom_point(
        data = sel,
        aes_string(x = xvar, y = yvar),
        shape = 21, size = 8, stroke = 2, color = "cyan", fill = NA
      )
    }
    
    # build widget: minimal ticks, tiny fonts, tight margins, autorange off (use our computed ranges)
    plt <- suppressWarnings(ggplotly(p, tooltip = "text", source = "ord")) %>%
      layout(
        dragmode = "zoom",
        hovermode = "closest",
        margin = list(l = 6, r = 6, b = 6, t = 6),
        paper_bgcolor = "black",
        plot_bgcolor  = "black",
        xaxis = list(
          title = "",
          range = xrange,
          showgrid = FALSE,
          zeroline = FALSE,
          ticks = "outside",
          ticklen = 3,
          tickfont = list(size = 10, color = "white"),
          automargin = FALSE
        ),
        yaxis = list(
          title = "",
          range = yrange,
          showgrid = FALSE,
          zeroline = FALSE,
          ticks = "outside",
          ticklen = 3,
          tickfont = list(size = 10, color = "white"),
          automargin = FALSE
        )
      ) %>%
      # names above each point
      add_text(
        data = df,
        x = ~get(xvar), y = ~get(yvar),
        text = ~Subject,
        textposition = "top center",
        textfont = list(color = "white", size = 12),
        inherit = FALSE, showlegend = FALSE, hoverinfo = "skip"
      )
    
    # register click event to avoid warnings
    plt <- event_register(plt, "plotly_click")
    plt
  })

# Debounced click handler (prevents rapid re-render thrash)
sky_click <- reactive(event_data("plotly_click", source = "ord"))
sky_click_d <- debounce(sky_click, 200)

observeEvent(sky_click_d(), {
  click <- sky_click_d(); if (is.null(click)) return()
  clicked <- click$key;   if (is.null(clicked) || length(clicked) == 0) return()

  cur <- selected_subjects()
  updated <- if (clicked %in% cur) {
    cur[cur != clicked]
  } else if (length(cur) < 2) {
    c(cur, clicked)
  } else {
    c(cur[2], clicked)
  }
  selected_subjects(updated)
}, ignoreInit = TRUE)

  

  
  # ---------------------- Comparison panel ----------------------
  observeEvent(list(input$sel_left, input$sel_right), ignoreInit = TRUE, {
    if (!is.null(input$sel_left) && !is.null(input$sel_right)) {
      selected_subjects(c(input$sel_left, input$sel_right))
    }
  })
  
  # Badge-style bonus traits
  trait_badges <- function(subj) {
    dat <- metadata %>% filter(Subject == subj)
    bins <- dat[binary_vars]
    lapply(names(bins), function(name) {
      val <- bins[[name]]
      if (!is.na(val)) {
        prefix <- if (val == -1) "anti-" else if (val == 0.5) "minor " else if (val == 2) "major " else ""
        bg <- if (val == -1) "steelblue" else if (val == 0.5) "darkorange" else if (val == 2) "red" else "orange"
        tags$div(
          style = paste0("margin:4px 0; padding:4px 6px; background-color:", bg,
                         "; border-radius:6px; display:block; color:white; width:100%;"),
          title = paste(name, "=", val),
          paste0("\U0001F947 ", prefix, name)
        )
      }
    })
  }
  
  output$comparePanel <- renderUI({
    sel <- selected_subjects()
    tagList(
      div(class = "panel-box panel-tight",
          fluidRow(
            column(12, tags$h4("Compare the Gurus", style = "margin-bottom:8px;"))
          ),
          fluidRow(
            column(6, selectInput("sel_left",  NULL, choices = metadata$Subject,
                                  selected = if (length(sel) >= 1) sel[1] else metadata$Subject[1])),
            column(6, selectInput("sel_right", NULL, choices = metadata$Subject,
                                  selected = if (length(sel) >= 2) sel[2] else metadata$Subject[2]))
          ),
          tags$hr(style = "border-color:#333; margin:8px 0;"),
          div(class = "d-flex justify-content-end align-items-center",
              radioButtons("compare_mode", NULL,
                           choices = c("Differences", "Average scores"),
                           selected = "Differences", inline = TRUE)
          ),
          uiOutput("comparisonPlotUI"),
          uiOutput("bonusTraitsBelow"),
          tags$hr(style = "border-color:#333; margin:8px 0;"),
          tags$button(
            class = "btn btn-sm btn-outline-light", type = "button",
            `data-bs-toggle` = "collapse", `data-bs-target` = "#decoderBox",
            "Display raw decoder scores"
          ),
          div(
            id = "decoderBox", class = "collapse",
            div(style="margin-top:8px;", uiOutput("decoderScoresTable"))
          )
      )
    )
  })
  
  # ---------------------- Comparison chart (responsive integers & aligned columns) ----------------------
  output$comparisonPlotUI <- renderUI({
    # reuse the same height logic you compute inside output$comparisonPlot
    w <- session$clientData$output_comparisonPlot_width %||% 860
    clamp <- function(x, lo, hi) max(lo, min(hi, x))
    scale_size <- function(w, w_lo, w_hi, s_lo, s_hi) {
      if (w <= w_lo) return(s_lo)
      if (w >= w_hi) return(s_hi)
      s_lo + (s_hi - s_lo) * ((w - w_lo) / (w_hi - w_lo))
    }
    
    # same wrap/row height math you use in the plot
    chars_per_line <- clamp(floor(w / 45), 10, 16)
    wrapped_levels <- stringr::str_wrap(graded_vars, width = chars_per_line)
    n_cat <- length(wrapped_levels)
    row_h <- round(scale_size(w, 360, 1200, 24, 34))
    plot_h <- row_h * n_cat + 300
    
    plotlyOutput("comparisonPlot", height = paste0(plot_h, "px"))
  })
  output$comparisonPlot <- renderPlotly({
    sel <- selected_subjects(); if (length(sel) < 2) return(NULL)
    s1 <- sel[1]; s2 <- sel[2]
    
    a <- metadata %>% dplyr::filter(Subject == s1)
    b <- metadata %>% dplyr::filter(Subject == s2)
    if (nrow(a) == 0 || nrow(b) == 0) return(NULL)
    
    valsA <- as.numeric(a[1, graded_vars])
    valsB <- as.numeric(b[1, graded_vars])
    
    df <- tibble::tibble(
      Characteristic = graded_vars,
      A = pmin(pmax(valsA, 0), 5),
      B = pmin(pmax(valsB, 0), 5)
    )
    
    # ---- responsive sizing helpers ----
    w <- session$clientData$output_comparisonPlot_width %||% 860
    
    clamp <- function(x, lo, hi) max(lo, min(hi, x))  # <- removed extra ')'
    
    scale_size <- function(w, w_lo, w_hi, s_lo, s_hi) {
      if (w <= w_lo) return(s_lo)
      if (w >= w_hi) return(s_hi)
      s_lo + (s_hi - s_lo) * ((w - w_lo) / (w_hi - w_lo))
    }
    
    row_h   <- round(scale_size(w, 360, 1200, 30, 42))   # was 24..34 → bumped to 30..42
    ylab_sz <- scale_size(w, 360, 1200, 9, 12)           # a touch larger labels
    num_sz  <- scale_size(w, 360, 1200, 3.6, 4.6)        # bigger numbers so they "pop"
    title_sz <- scale_size(w, 360, 1200, 15, 19)
    info_sz  <- scale_size(w, 360, 1200, 11, 13)
    
    # ==== ALWAYS-WRAP Y LABELS AGGRESSIVELY ====
    # small chars_per_line => more wrapping
    chars_per_line <- clamp(floor(w / 45), 10, 16)
    wrap_w <- chars_per_line
    
    wrapped_levels <- stringr::str_wrap(graded_vars, width = wrap_w)
    df$CharacteristicWrapped <- factor(
      stringr::str_wrap(df$Characteristic, width = wrap_w),
      levels = rev(wrapped_levels)
    )
  
  n_cat  <- length(wrapped_levels)
  plot_h <- row_h * n_cat + 300
  
  # similarity footer
  method  <- tolower(if (is.null(input$dist_method)) "manhattan" else input$dist_method)
  distmat <- as.matrix(precomputed[[method]])
  sim_txt <- if (s1 %in% rownames(distmat) && s2 %in% colnames(distmat)) {
    d <- distmat[s1, s2]; sprintf("Similarity: <b>%.1f%%</b>", 100 * (1 - d / max(distmat, na.rm = TRUE)))
  } else "Similarity: —"
  
  # gridlines BETWEEN categories
  between_lines <- data.frame(y = seq(0.5, n_cat + 0.5, by = 1))
  
  # geometry & integer labels in fixed columns
  XLIM    <- 5.20
  XBREAK  <- c(-5, 0, 5)
  XLABS   <- c("5","0","5")
  VAL_PAD <- 0.6
  label_fn <- function(z) sprintf('%.0f', round(z))  # integers only
  
  gp_base <- function() {
    ggplot2::ggplot() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position      = "none",
        plot.background      = ggplot2::element_rect(fill = "black", color = NA),
        panel.background     = ggplot2::element_rect(fill = "black", color = NA),
        panel.grid.major.x   = ggplot2::element_line(color = "#333333"),
        panel.grid.minor.x   = ggplot2::element_blank(),
        panel.grid.major.y   = ggplot2::element_blank(),
        panel.grid.minor.y   = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          color = "white", size = ylab_sz,
          lineheight = 80,   # tighter wrapping
          margin = ggplot2::margin(r = 2)
        ),
        axis.text.x          = ggplot2::element_text(color = "white"),
        axis.title           = ggplot2::element_blank(),
        plot.margin          = ggplot2::margin(5, 5, 5, 5)
      )
  }
  
  colA <- "#F8766D"; colB <- "#00BFC4"
  is_raw <- !is.null(input$compare_mode) && grepl("^raw", tolower(input$compare_mode))
  
  if (is_raw) {
    gp <- gp_base() +
      ggplot2::geom_hline(data = between_lines, ggplot2::aes(yintercept = y),
                          color = "#333333", linewidth = 0.3) +
      ggplot2::geom_col(data = df, ggplot2::aes(x = -A, y = CharacteristicWrapped),
                        fill = colA, width = 0.52) +
      ggplot2::geom_col(data = df, ggplot2::aes(x =  B, y = CharacteristicWrapped),
                        fill = colB, width = 0.52) +
      ggplot2::geom_vline(xintercept = 0, color = "white", linewidth = 0.4) +
      # integer columns near labels
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(x = -XLIM + VAL_PAD, y = CharacteristicWrapped, label = label_fn(A)),
        hjust = 1, vjust = 0.5, color = colA, size = num_sz, fontface = "bold", family = "mono"
      ) +
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(x =  XLIM - VAL_PAD, y = CharacteristicWrapped, label = label_fn(B)),
        hjust = 0, vjust = 0.5, color = colB, size = num_sz, fontface = "bold", family = "mono"
      ) +
      ggplot2::scale_x_continuous(limits = c(-XLIM, XLIM),
                                  breaks = XBREAK, labels = XLABS,
                                  expand = ggplot2::expansion(mult = c(0, 0)))
  } else {
    df2 <- df %>%
      dplyr::mutate(Diff = pmax(pmin(B - A, 5), -5),
                    Side = dplyr::case_when(Diff > 0 ~ "B", Diff < 0 ~ "A", TRUE ~ "Equal"))
    gp <- gp_base() +
      ggplot2::geom_hline(data = between_lines, ggplot2::aes(yintercept = y),
                          color = "#333333", linewidth = 0.3) +
      ggplot2::geom_col(data = df2, ggplot2::aes(x = Diff, y = CharacteristicWrapped, fill = Side),
                        width = 0.52, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = c(A = colA, B = colB, Equal = "grey60")) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "#808080") +
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(x = -XLIM + VAL_PAD, y = CharacteristicWrapped, label = label_fn(A)),
        hjust = 1, vjust = 0.5, color = colA, size = num_sz, fontface = "bold", family = "mono"
      ) +
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(x =  XLIM - VAL_PAD, y = CharacteristicWrapped, label = label_fn(B)),
        hjust = 0, vjust = 0.5, color = colB, size = num_sz, fontface = "bold", family = "mono"
      ) +
      ggplot2::scale_x_continuous(limits = c(-XLIM, XLIM),
                                  breaks = XBREAK, labels = XLABS,
                                  expand = ggplot2::expansion(mult = c(0, 0)))
  }
  
  # margins scale with wrapped lines
  max_lines   <- max(sapply(strsplit(as.character(wrapped_levels), "\n", fixed = TRUE), length))
  left_margin <- max(54, 140 + 10 * (max_lines - 1))  # a touch more generous
  right_margin <- 18
  
  plt <- plotly::ggplotly(gp, tooltip = NULL)
  
  
  
  
  # headers & similarity footer (with a bit of top air)
  header_y1 <- 1.12; header_y2 <- 1.08
  HEADER_X  <- XLIM - 0.06
  ann <- list(
    list(x = -HEADER_X, y = header_y1, xref = "x", yref = "paper",
         text = sprintf("<b>%s</b>", s1), showarrow = FALSE, xanchor = "left",
         font = list(color = "#F8766D", size = title_sz)),
    list(x =  HEADER_X, y = header_y1, xref = "x", yref = "paper",
         text = sprintf("<b>%s</b>", s2), showarrow = FALSE, xanchor = "right",
         font = list(color = "#00BFC4", size = title_sz)),
    list(x = -HEADER_X, y = header_y2, xref = "x", yref = "paper",
         text = sprintf("Gurometer: <b>%.0f%%</b> • Avg: <b>%.2f</b>", a$Percentage, a$Average),
         showarrow = FALSE, xanchor = "left",
         font = list(color = "#F4C542", size = info_sz)),
    list(x =  HEADER_X, y = header_y2, xref = "x", yref = "paper",
         text = sprintf("Gurometer: <b>%.0f%%</b> • Avg: <b>%.2f</b>", b$Percentage, b$Average),
         showarrow = FALSE, xanchor = "right",
         font = list(color = "#F4C542", size = info_sz)),
    list(x = 0, y = -0.07, xref = "x", yref = "paper",
         text = sim_txt, showarrow = FALSE, xanchor = "center",
         font = list(color = "#F4C542", size = info_sz))
  )
  
  plt %>%
    plotly::layout(
      margin = list(l = left_margin, r = right_margin, b = 46, t = 80),
      xaxis = list(automargin = FALSE),
      yaxis = list(automargin = FALSE, ticklen = 3, tickpad = 0),
      annotations = ann
    )
  })



  
  # Bonus traits BELOW the chart (toggle in settings). Guru name above each list.
  output$bonusTraitsBelow <- renderUI({
    if (is.null(input$show_traits) || !isTRUE(input$show_traits)) return(NULL)
    sel <- selected_subjects(); if (length(sel) < 2) return(NULL)
    s1 <- sel[1]; s2 <- sel[2]
    
    tags$div(
      style = "margin-top:8px;",
      fluidRow(
        column(6, tags$strong(s1), tags$div(style="margin-top:4px;", trait_badges(s1))),
        column(6, tags$strong(s2), tags$div(style="margin-top:4px;", trait_badges(s2)))
      )
    )
  })
  
  # Raw decoder scores (hidden by default via collapse)
  output$decoderScoresTable <- renderUI({
    sel <- selected_subjects(); if (length(sel) < 2 || is.null(decoder_scores)) return(NULL)
    tableOutput(outputId = "decoderTableInternal")
  })
  
  output$decoderTableInternal <- renderTable({
    sel <- selected_subjects(); if (length(sel) < 2 || is.null(decoder_scores)) return(NULL)
    s1 <- sel[1]; s2 <- sel[2]
    ds <- decoder_scores %>% filter(Subject %in% c(s1, s2))
    left  <- ds %>% filter(Subject == s1) %>% select(decoder, all_of(graded_vars))
    right <- ds %>% filter(Subject == s2) %>% select(decoder, all_of(graded_vars))
    L <- left  %>% pivot_longer(all_of(graded_vars), names_to = "Characteristic", values_to = "Left")
    R <- right %>% pivot_longer(all_of(graded_vars), names_to = "Characteristic", values_to = "Right")
    comp <- full_join(L, R, by = c("decoder","Characteristic")) %>%
      mutate(Diff = Right - Left) %>%
      arrange(factor(Characteristic, levels = graded_vars), decoder)
    names(comp) <- c("Decoder","Characteristic", s1, s2, "Diff")
    comp
  }, digits = 2)
}
