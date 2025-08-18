
#This is the old version of the guroscope app for simple copy-paste run not usin precomputed values for hosting. It pulls the data directly from the googlesheets.

# ---- 1. Load Required Packages ----
install_and_load <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
install_and_load(c("googlesheets4", "dplyr", "readr", "stringr", "shiny",
                   "vegan", "ggplot2", "plotly", "RColorBrewer", "tidyr",
                   "ggrepel", "bslib"))

# ---- 2. Read Google Sheet with all values as characters ----
gs4_deauth()
url <- "https://docs.google.com/spreadsheets/d/1Oe-af4_OmzLJavktcSKGfP0wmxCX0ppP8n_Tvi9l_yc"
raw <- read_sheet(url, col_types = "c", col_names = FALSE, skip = 1)

raw <- dplyr::mutate(raw, dplyr::across(dplyr::everything(),
  ~ stringr::str_replace_all(., c("\u00bd" = "0.5", "1/2" = "0.5"))
))

# ---- 3. Extract decoder blocks ----
decoder_rows <- which(raw[[1]] %in% c("CK", "MB", "RK"))
decoder_data <- list()
ck_header_row <- decoder_rows[1]
ck_header <- as.character(unlist(raw[ck_header_row, ]))
ck_header[is.na(ck_header) | ck_header == ""] <- paste0("X", which(is.na(ck_header) | ck_header == ""))
ck_header[1] <- "Subject"

for (i in seq_along(decoder_rows)) {
  name <- raw[[1]][decoder_rows[i]]
  start_row <- decoder_rows[i] + 1
  end_row <- if (i < length(decoder_rows)) decoder_rows[i + 1] - 1 else nrow(raw)
  block <- raw[start_row:end_row, ]
  colnames(block) <- ck_header
  decoder_data[[name]] <- block
}

# ---- 4. Manual subject name fixes ----
manual_fixes <- c(
  "Zizek" = "Slavoj Zizek",
  "MIkhaila Peterson" = "Mikhaila Peterson",
  "Michael O' Fallon (Aaron)" = "Michael O' Fallon",
  "Anna" = "Anna Khachiyan",
  "Dasha" = "Dasha Nekrasova",
  "Sabine" = "Sabine Hossenfelder"
)

for (decoder in names(decoder_data)) {
  decoder_data[[decoder]] <- decoder_data[[decoder]] %>%
    dplyr::mutate(
      Subject = dplyr::recode(Subject, !!!manual_fixes),
      decoder = decoder
    )
}

# ---- 5. Combine and clean data ----
graded_vars <- c(
  "Galaxy Brainness", "Cultishness", "Anti-Establishment", "Grievance Mongering",
  "Self-Aggrandisement and Narcicissm", "Cassandra Complex", "Revolutionary Theories",
  "Pseudo Profound Bullshit", "Conspiracy Mongering", "Profiteering", "Moral Grandstanding"
)

# Bonus traits (binary/severity-like features)
binary_vars <- c(
  "Monomania", "Shilling Supplements", "Broicity", "Charisma", "Neologism",
  "Strategic Disclaimers", "Rebranding Theories", "Loquaciousness", "Never admitting error"
)

all_data_clean <- dplyr::bind_rows(decoder_data) %>%
  dplyr::filter(!is.na(Subject) & stringr::str_trim(Subject) != "") %>%
  dplyr::relocate(decoder, .before = Subject) %>%
  dplyr::mutate(dplyr::across(all_of(graded_vars), ~ as.numeric(as.character(.)))) %>%
  dplyr::mutate(dplyr::across(all_of(binary_vars), ~ dplyr::case_when(
    . %in% c("\u00bd", "1/2") ~ 0.5,
    . == "-1" ~ -1,
    TRUE ~ as.numeric(as.character(.))
  )))

combined_data <- all_data_clean %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(
    Secular_Guru = dplyr::first(na.omit(`Secular Guru`)),
    dplyr::across(all_of(graded_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    Average = mean(c_across(all_of(graded_vars)), na.rm = TRUE),
    Sum = sum(c_across(all_of(graded_vars)), na.rm = TRUE),
    SD = sd(c_across(all_of(graded_vars)), na.rm = TRUE),
    Percentage = Average / 5 * 100
  ) %>%
  dplyr::ungroup()

binary_summary <- all_data_clean %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(dplyr::across(all_of(binary_vars), ~ suppressWarnings(max(.x, na.rm = TRUE))), .groups = "drop") %>%
  dplyr::mutate(dplyr::across(all_of(binary_vars), ~ ifelse(is.infinite(.), NA, .)))

combined_data <- dplyr::left_join(combined_data, binary_summary, by = "Subject")

# ---- 6. Prepare data for ordination (Characteristics only; Bonus traits added later if toggled) ----
aspect_vars <- graded_vars
binary_matrix <- combined_data[, binary_vars]
binary_matrix$Subject <- combined_data$Subject

aspect_data <- combined_data[, aspect_vars]
aspect_data[] <- lapply(aspect_data, function(x) as.numeric(as.character(x)))
aspect_data <- aspect_data[rowSums(is.na(aspect_data)) < length(aspect_vars), ]
aspect_data <- aspect_data[, apply(aspect_data, 2, function(x) var(x, na.rm = TRUE) > 0)]
rownames(aspect_data) <- combined_data$Subject

# ---- 6a. Scaled blocks: Characteristics (0..1) and Bonus traits (signed severity) ----
graded_scaled <- as.matrix(aspect_data) / 5
rownames(graded_scaled) <- rownames(aspect_data)

trait_raw <- combined_data[rownames(graded_scaled), binary_vars, drop = FALSE]
# Scale: -1, 0.5, 1, 2 -> -0.5, 0.25, 0.5, 1.0
trait_scaled <- as.matrix(data.frame(lapply(trait_raw, function(col) as.numeric(col) / 2)))
rownames(trait_scaled) <- rownames(graded_scaled)
trait_scaled[is.na(trait_scaled)] <- 0  # NA means “absent” so traits can contribute

safe_range <- function(v) { r <- range(v, na.rm = TRUE); if (!all(is.finite(r))) 0 else (r[2] - r[1]) }
safe_range_sq <- function(v) { x <- safe_range(v); x * x }

trait_ranges_manhattan <- sapply(as.data.frame(trait_scaled), safe_range)
trait_ranges_euclid_sq  <- sapply(as.data.frame(trait_scaled), safe_range_sq)

# ---- 6b. Build combined matrix with weights; clean NAs to avoid vegdist errors ----
build_comm <- function(dist_label, use_traits, w_trait) {
  wg <- 1 - w_trait
  wb <- if (use_traits) w_trait else 0

  if (tolower(dist_label) %in% c("euclidean", "cosine")) {
    Gw <- sweep(graded_scaled, 2, sqrt(wg), `*`)
    if (wb > 0) {
      Tw <- sweep(trait_scaled, 2, sqrt(wb), `*`)
      mat <- cbind(Gw, Tw)
    } else { mat <- Gw }
  } else {
    Gw <- sweep(graded_scaled, 2, wg, `*`)
    if (wb > 0) {
      Tw <- sweep(trait_scaled, 2, wb, `*`)
      mat <- cbind(Gw, Tw)
    } else { mat <- Gw }
  }

  if (ncol(mat) > 0) mat <- mat[, colSums(!is.na(mat)) > 0, drop = FALSE]
  if (nrow(mat) > 0) mat <- mat[rowSums(!is.na(mat)) > 0, , drop = FALSE]
  mat[is.na(mat)] <- 0
  mat
}

# ---- 6c. Pairwise similarity % aligned with ordination (no column dropping) ----
distance_pair_pct <- function(subj_a, subj_b, dist_label, use_traits, w_trait) {
  dm <- tolower(dist_label)
  wg <- 1 - w_trait
  wb <- if (use_traits) w_trait else 0

  if (!(subj_a %in% rownames(graded_scaled)) || !(subj_b %in% rownames(graded_scaled))) return(NA_real_)

  if (dm %in% c("euclidean", "cosine")) {
    ax <- sweep(graded_scaled[subj_a, , drop = FALSE], 2, sqrt(wg), `*`)
    bx <- sweep(graded_scaled[subj_b, , drop = FALSE], 2, sqrt(wg), `*`)
    if (wb > 0) {
      ax <- cbind(ax, sweep(trait_scaled[subj_a, , drop = FALSE], 2, sqrt(wb), `*`))
      bx <- cbind(bx, sweep(trait_scaled[subj_b, , drop = FALSE], 2, sqrt(wb), `*`))
    }
  } else {
    ax <- sweep(graded_scaled[subj_a, , drop = FALSE], 2, wg, `*`)
    bx <- sweep(graded_scaled[subj_b, , drop = FALSE], 2, wg, `*`)
    if (wb > 0) {
      ax <- cbind(ax, sweep(trait_scaled[subj_a, , drop = FALSE], 2, wb, `*`))
      bx <- cbind(bx, sweep(trait_scaled[subj_b, , drop = FALSE], 2, wb, `*`))
    }
  }

  XY <- rbind(ax, bx)
  if (!ncol(XY)) return(NA_real_)
  XY[is.na(XY)] <- 0  # keep all columns; missing -> 0 (absent)

  if (dm == "cosine") {
    stand <- vegan::decostand(XY, method = "normalize", MARGIN = 1)
    d <- as.numeric(vegan::vegdist(stand, method = "euclidean"))
    cos_sim <- 1 - (d^2) / 2
    return(100 * max(min(cos_sim, 1), 0))
  }

  d <- as.numeric(vegan::vegdist(XY, method = dm))

  cols <- colnames(XY)
  g_mask <- cols %in% colnames(graded_scaled)
  b_mask <- cols %in% colnames(trait_scaled)

  if (dm == "euclidean") {
    max_g <- if (any(g_mask)) sqrt(wg) * sqrt(sum(g_mask)) else 0
    max_b <- if (wb > 0 && any(b_mask)) sqrt(sum(wb * trait_ranges_euclid_sq[cols[b_mask]])) else 0
    d_max <- sqrt(max_g^2 + max_b^2)
    return(100 * (1 - d / ifelse(d_max > 0, d_max, 1)))
  } else {
    max_g <- if (any(g_mask)) wg * sum(g_mask) else 0
    max_b <- if (wb > 0 && any(b_mask)) wb * sum(trait_ranges_manhattan[cols[b_mask]]) else 0
    d_max <- max_g + max_b
    return(100 * (1 - d / ifelse(d_max > 0, d_max, 1)))
  }
}

# ---- 6d. Wikipedia labels/links for the chosen index ----
index_meta <- function(method) {
  switch(tolower(method),
    "euclidean" = list(label = "Euclidean similarity index", url = "https://en.wikipedia.org/wiki/Euclidean_distance"),
    "manhattan" = list(label = "Manhattan similarity index", url = "https://en.wikipedia.org/wiki/Manhattan_distance"),
    "cosine"    = list(label = "Cosine similarity index",    url = "https://en.wikipedia.org/wiki/Cosine_similarity"),
    list(label = "Similarity index", url = "#")
  )
}

# ---- 7. Shiny App ----
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5, bg = "#1A1A1A", fg = "#FFFFFF",
    primary = "#F24C0C", secondary = "#F4C542",
    base_font = bslib::font_google("Montserrat")
  ),
  titlePanel("Guroscope"),

  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Explore",
      tags$style(HTML("
        #skyplot-wrapper:hover {
          cursor: url('https://img.icons8.com/fluency-systems-regular/48/FFFFFF/illuminati-symbol.png') 16 16, auto;
        }
      ")),
      fluidRow(
        column(2,
          # Always-visible minimal control
          radioButtons("selectedTrait", "Highlight Bonus Trait", choices = c("None", binary_vars), selected = "None"),
          # Hidden-by-default Settings
          checkboxInput("show_settings", "Show Settings \u2699\ufe0f", value = FALSE),
          conditionalPanel(
  condition = "input.show_settings == true",
  tags$div(
    style = "margin-top:10px;padding:10px;border:1px solid #444;border-radius:8px;",
    tags$h4("Settings"),
    selectInput("dist_method", "Similarity index (metric)",
                choices = c("Euclidean", "Manhattan", "Cosine"),
                selected = "Euclidean"),
    radioButtons("method", "Ordination", choices = c("PCA", "NMDS"), inline = TRUE),

    # Collapsible Bonus traits box
    tags$div(
      style = "margin-top:10px;",
      tags$button(
        class = "btn btn-sm btn-outline-secondary",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#bonusTraitsBox",
        "Bonus traits options"
      ),
      div(
        id = "bonusTraitsBox",
        class = "collapse",
        tags$p(style = "font-size: 0.9em; color: #aaa;", 
               "Bonus traits are not currently supported in similarity % calculations."),
        checkboxInput("use_traits", "Include Bonus traits in similarity % calculations", value = FALSE),
        sliderInput("trait_weight", "Bonus trait weight (0–1)", min = 0, max = 1, value = 0.30, step = 0.05)
      )
    ),

    numericInput("nmds_seed", "NMDS Seed", value = 42),
    uiOutput("axis_select_ui"),
    actionButton("reset_settings", "Reset to defaults")
  )
)
        ),
        column(6,
          div(id = "skyplot-wrapper", plotlyOutput("skyPlot", height = "1000px"))
        ),
        column(4,
          uiOutput("similarityPanel"),
          fluidRow(
            column(6, uiOutput("subjectProfile1")),
            column(6, uiOutput("subjectProfile2"))
          )
        )
      ),
      fluidRow(
        column(12,
          tags$h4("Raw Gurometry scores by decoder (Note: ordination & similarity use per-Characteristic averages across available decoders.)"),
          uiOutput("decoderScores1"),
          tags$hr(style = "border-color: white;"),
          uiOutput("decoderScores2")
        )
      ),
      tags$div(
        style = "margin-top: 20px; font-size: 12px; text-align: center; color: gray;",
        HTML('• Gurometry by Chris Kavanagh and Matt Browne, from the <a href="https://decodingthegurus.com" style="color: #F4C542;" target="_blank">Decoding the Gurus</a> podcast<br>'),
        HTML('Cursor icon by <a href="https://icons8.com/icon/123267/illuminati" style="color: #F4C542;" target="_blank">Icons8</a>')
      )
    ),
    tabPanel(
      "About",
      tags$div(
        style = "padding: 16px; max-width: 900px;",
        tags$h3("About this app"),
        tags$p("This app visualizes Gurometer profiles — multiple 0–5 scored Characteristics — in a 2D ordination. Gurus with similar patterns of Characteristics appear closer together."),

        tags$h4("How the data is prepared"),
        tags$ol(
          tags$li("Ratings are loaded from a Google Sheet containing numeric ratings for each guru across graded Characteristics and Bonus traits."),
          tags$li("Special cases like “½” are converted to numeric form, and subject name inconsistencies are fixed."),
          tags$li("If multiple decoders scored the same guru, the mean rating per Characteristic is calculated across decoders; Bonus traits are summarised by the maximum recorded value."),
          tags$li("Characteristics with zero variance are removed; gurus with missing values for all graded Characteristics are excluded."),
          tags$li("Characteristics are scaled to 0–1; Bonus traits are scaled to preserve sign and magnitude (anti = −0.5, minor = 0.25, present = 0.5, major = 1.0).")
        ),

        tags$h4("Similarity index and Bonus trait weighting"),
        tags$p("Choose a similarity index (Euclidean, Manhattan, or Cosine) and optionally include Bonus traits with a weight (0–1). The combined feature matrix is built as follows:"),
        tags$ul(
          tags$li(tags$b("Characteristics:"), " scaled to 0–1 and assigned weight (1 − w)."),
          tags$li(tags$b("Bonus traits:"), " scaled to [−0.5, 1.0] and assigned weight w (if enabled)."),
          tags$li(tags$b("Weighting rule:"), " for Euclidean/Cosine, column weights use √weight; for Manhattan, linear weight is used.")
        ),

        tags$h4("How the ordination is computed"),
        tags$ul(
          tags$li(tags$b("PCA tab = PCoA:"), " Principal Coordinates Analysis on the chosen distance matrix via ", tags$code("vegan::capscale(comm ~ 1, distance = ...)"), "."),
          tags$li(tags$b("NMDS tab:"), " Non-metric multidimensional scaling via ", tags$code("vegan::metaMDS(comm, distance = ...)"), ", preserving the rank order of distances."),
          tags$li(tags$b("Cosine option:"), " rows are normalized to unit length with ", tags$code("vegan::decostand(method = 'normalize', MARGIN = 1)"), " and Euclidean distances between unit vectors are used (a monotonic transform of cosine similarity).")
        ),

        tags$h4("Similarity % in the side panel"),
        tags$ul(
          tags$li("For Cosine: cosine similarity × 100%."),
          tags$li("For Euclidean/Manhattan: distance is rescaled to a 0–100% similarity using the theoretical maximum distance under the same scaling and weights.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Two random subjects by default
  all_subjects <- rownames(graded_scaled)
  set.seed(42)
  selected_subjects <- reactiveVal(sample(all_subjects, 2))

  # Axis selectors (shown inside Settings)
  output$axis_select_ui <- renderUI({
    axes <- if (identical(input$method, "PCA")) c("PCoA1", "PCoA2") else c("NMDS1", "NMDS2")
    tagList(
      selectInput("axis1", "X Axis", choices = axes, selected = axes[1]),
      selectInput("axis2", "Y Axis", choices = axes, selected = axes[2])
    )
  })

  # Reset to defaults
  observeEvent(input$reset_settings, {
    updateSelectInput(session, "dist_method", selected = "Euclidean")
    updateRadioButtons(session, "method", selected = "PCA")
    updateCheckboxInput(session, "use_traits", value = FALSE)
    updateSliderInput(session, "trait_weight", value = 0.30)
    updateNumericInput(session, "nmds_seed", value = 42)
    updateRadioButtons(session, "selectedTrait", selected = "None")
    updateCheckboxInput(session, "show_settings", value = FALSE)
    axes <- c("PCoA1", "PCoA2")
    updateSelectInput(session, "axis1", choices = axes, selected = axes[1])
    updateSelectInput(session, "axis2", choices = axes, selected = axes[2])
    set.seed(42)
    selected_subjects(sample(all_subjects, 2))
  })

  # Community matrix for distances/ordination
  get_comm_for_distance <- reactive({
    dm <- tolower(if (is.null(input$dist_method)) "euclidean" else input$dist_method)
    comm <- build_comm(dm, isTRUE(input$use_traits), if (is.null(input$trait_weight)) 0.3 else input$trait_weight)
    if (dm == "cosine") {
      vegan::decostand(comm, method = "normalize", MARGIN = 1)
    } else {
      as.matrix(comm)
    }
  })

  # Ordination
  current_ord <- reactive({
    dm <- tolower(if (is.null(input$dist_method)) "euclidean" else input$dist_method)
    comm <- get_comm_for_distance()
    dist_method <- if (dm == "cosine") "euclidean" else dm

    if (identical(input$method, "PCA")) {
      vegan::capscale(comm ~ 1, distance = dist_method, add = FALSE)
    } else {
      set.seed(if (is.null(input$nmds_seed)) 42 else input$nmds_seed)
      vegan::metaMDS(comm, distance = dist_method, trymax = 500, k = 2, autotransform = FALSE)
    }
  })

  site_scores <- reactive({
    ord <- current_ord()
    s <- scores(ord, display = "sites")
    colnames(s) <- if (identical(input$method, "PCA")) paste0("PCoA", seq_len(ncol(s))) else paste0("NMDS", seq_len(ncol(s)))
    df <- as.data.frame(s)
    df$Subject <- rownames(s)

    idx <- match(df$Subject, combined_data$Subject)
    df$Percentage <- combined_data$Percentage[idx]
    df$Average <- combined_data$Average[idx]

    df$TraitColor <- if (!is.null(input$selectedTrait) && input$selectedTrait != "None") {
      present <- binary_matrix[[input$selectedTrait]][match(df$Subject, binary_matrix$Subject)] == 1
      ifelse(present, input$selectedTrait, "None")
    } else "None"

    df$IsSelected <- df$Subject %in% selected_subjects()
    df
  })

  output$skyPlot <- renderPlotly({
    df <- site_scores()
    palette <- c("None" = "gray60")
    if (!is.null(input$selectedTrait) && input$selectedTrait != "None") palette[input$selectedTrait] <- "orange"

    base <- ggplot(df, aes_string(x = input$axis1, y = input$axis2)) +
      geom_point(aes(color = TraitColor, alpha = Percentage, size = Average, key = Subject, text = Subject), shape = 8) +
      scale_color_manual(values = palette) +
      scale_alpha(range = c(0.1, 1)) +
      scale_size(range = c(2, 8)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "white", fill = NA),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white")
      )

    overlay <- base +
      geom_point(
        data = subset(df, IsSelected),
        aes_string(x = input$axis1, y = input$axis2, key = "Subject", text = "Subject"),
        shape = 21, size = 8, stroke = 2, color = "cyan", fill = NA
      )

    gg <- ggplotly(overlay, tooltip = "text") %>% layout(dragmode = "zoom")

    gg %>% add_text(
      data = df,
      x = ~get(input$axis1),
      y = ~get(input$axis2),
      text = ~Subject,
      textposition = "top center",
      textfont = list(color = "white", size = 12),
      inherit = FALSE,
      showlegend = FALSE
    )
  })

  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click)) {
      clicked <- click$key
      current <- selected_subjects()
      updated <- if (clicked %in% current) {
        current[current != clicked]
      } else if (length(current) < 2) {
        c(current, clicked)
      } else {
        c(current[2], clicked)
      }
      selected_subjects(updated)
    }
  })

  # Similarity panel
  output$similarityPanel <- renderUI({
    sel <- selected_subjects()
    if (length(sel) < 2) return(NULL)

    dm <- if (is.null(input$dist_method)) "Euclidean" else input$dist_method
    sim <- distance_pair_pct(sel[1], sel[2], dm,
                             isTRUE(input$use_traits),
                             if (is.null(input$trait_weight)) 0.3 else input$trait_weight)
    sim_txt <- if (is.na(sim)) "—" else sprintf("%.1f%%", sim)
    meta <- index_meta(dm)

    tags$div(
      style = "margin-bottom: 12px; padding: 10px; border: 1px solid #444; border-radius: 8px; background:#111;",
      tags$h4(
        tags$a(href = meta$url, target = "_blank", style = "color:#F4C542; text-decoration: none;", meta$label),
        style = "margin-top:0;"
      ),
      tags$p(HTML(sprintf("<b>%s</b> \u2194 <b>%s</b>: <span style='color:#F4C542;font-size:1.2em;'>%s</span>",
                          sel[1], sel[2], sim_txt))),
      tags$p(sprintf("Bonus traits %s, weight = %.2f",
                     if (isTRUE(input$use_traits)) "included" else "excluded",
                     if (isTRUE(input$use_traits)) input$trait_weight else 0))
    )
  })

  # Profiles
  make_profile_ui <- function(subj) {
    if (is.null(subj)) return(NULL)
    data <- combined_data %>% dplyr::filter(Subject == subj)
    aspects <- data[graded_vars]
    binaries <- data[binary_vars]
    is_active <- subj %in% selected_subjects()

    trait_display <- lapply(names(binaries), function(name) {
      val <- binaries[[name]]
      if (!is.na(val)) {
        prefix <- if (val == -1) "anti-" else if (val == 0.5) "minor " else if (val == 2) "major " else ""
        trait_name <- paste0(prefix, name)
        bg_color <- if (val == -1) "steelblue" else if (val == 0.5) "darkorange" else if (val == 2) "red" else "orange"
        tags$span(
          style = paste0("margin: 4px; padding: 4px; background-color: ", bg_color,
                         "; border-radius: 4px; display: inline-block; color: white;"),
          title = paste(name, "=", val),
          paste0("\U0001F947 ", trait_name)
        )
      }
    })

    tagList(
      tags$h4(subj, style = if (is_active) "color:#F4C542;" else NULL),
      tags$p(sprintf("Gurometer %%: %.1f%%", data$Percentage)),
      tags$p(sprintf("Average Score: %.2f", data$Average)),

      tags$h5("Characteristic scores:"),
      lapply(names(aspects), function(name) {
        val <- aspects[[name]]
        tags$div(style = "margin-bottom: 4px;",
                 tags$span(style = "width: 130px; display: inline-block;", name),
                 tags$progress(value = val, max = 5, style = "width: 150px;"))
      }),

      tags$h5("Bonus traits:"),
      tags$div(trait_display)
    )
  }

  output$subjectProfile1 <- renderUI({
    selected <- selected_subjects()
    if (length(selected) >= 1) make_profile_ui(selected[1])
  })

  output$subjectProfile2 <- renderUI({
    selected <- selected_subjects()
    if (length(selected) >= 2) make_profile_ui(selected[2])
  })

  # Raw per-decoder tables (for reference)
  output$decoderScores1 <- renderUI({
    selected <- selected_subjects()
    if (length(selected) >= 1) {
      tbl <- all_data_clean %>%
        dplyr::filter(Subject == selected[1]) %>%
        dplyr::select(decoder, all_of(graded_vars)) %>%
        dplyr::arrange(decoder)
      tagList(tags$h5(selected[1]), renderTable(tbl, digits = 2))
    }
  })
  output$decoderScores2 <- renderUI({
    selected <- selected_subjects()
    if (length(selected) >= 2) {
      tbl <- all_data_clean %>%
        dplyr::filter(Subject == selected[2]) %>%
        dplyr::select(decoder, all_of(graded_vars)) %>%
        dplyr::arrange(decoder)
      tagList(tags$h5(selected[2]), renderTable(tbl, digits = 2))
    }
  })
}

# run the app
guroscope <- shinyApp(ui, server)
