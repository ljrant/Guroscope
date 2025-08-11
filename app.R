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

raw <- raw %>%
  mutate(across(everything(), ~ str_replace_all(., c("\u00bd" = "0.5", "1/2" = "0.5"))))

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
    mutate(
      Subject = recode(Subject, !!!manual_fixes),
      decoder = decoder
    )
}

# ---- 5. Combine and clean data ----
graded_vars <- c(
  "Galaxy Brainness", "Cultishness", "Anti-Establishment", "Grievance Mongering",
  "Self-Aggrandisement and Narcicissm", "Cassandra Complex", "Revolutionary Theories",
  "Pseudo Profound Bullshit", "Conspiracy Mongering", "Profiteering", "Moral Grandstanding"
)

binary_vars <- c(
  "Monomania", "Shilling Supplements", "Broicity", "Charisma", "Neologism",
  "Strategic Disclaimers", "Rebranding Theories", "Loquaciousness", "Never admitting error"
)

all_data_clean <- bind_rows(decoder_data) %>%
  filter(!is.na(Subject) & str_trim(Subject) != "") %>%
  relocate(decoder, .before = Subject) %>%
  mutate(across(all_of(graded_vars), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(binary_vars), ~ case_when(
    . %in% c("\u00bd", "1/2") ~ 0.5,
    . == "-1" ~ -1,
    TRUE ~ as.numeric(as.character(.))
  )))

combined_data <- all_data_clean %>%
  group_by(Subject) %>%
  summarise(
    Secular_Guru = first(na.omit(`Secular Guru`)),
    across(all_of(graded_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Average = mean(c_across(all_of(graded_vars)), na.rm = TRUE),
    Sum = sum(c_across(all_of(graded_vars)), na.rm = TRUE),
    SD = sd(c_across(all_of(graded_vars)), na.rm = TRUE),
    Percentage = Average / 5 * 100
  ) %>%
  ungroup()

binary_summary <- all_data_clean %>%
  group_by(Subject) %>%
  summarise(across(all_of(binary_vars), ~ suppressWarnings(max(.x, na.rm = TRUE))), .groups = "drop") %>%
  mutate(across(all_of(binary_vars), ~ ifelse(is.infinite(.), NA, .)))

combined_data <- left_join(combined_data, binary_summary, by = "Subject")

# ---- 6. Prepare data for ordination ----
aspect_vars <- graded_vars
binary_matrix <- combined_data[, binary_vars]
binary_matrix$Subject <- combined_data$Subject

aspect_data <- combined_data[, aspect_vars]
aspect_data[] <- lapply(aspect_data, function(x) as.numeric(as.character(x)))
aspect_data <- aspect_data[rowSums(is.na(aspect_data)) < length(aspect_vars), ]
aspect_data <- aspect_data[, apply(aspect_data, 2, function(x) var(x, na.rm = TRUE) > 0)]
rownames(aspect_data) <- combined_data$Subject

# ---- 6b. Similarity helpers (vegan where possible) ----
cosine_pct_vegan <- function(x, y) {
  xy <- rbind(x, y)
  ok <- stats::complete.cases(t(xy))
  if (!any(ok)) return(NA_real_)
  xy <- xy[, ok, drop = FALSE]
  if (ncol(xy) == 0) return(NA_real_)
  if (all(xy[1, ] == 0, na.rm = TRUE) && all(xy[2, ] == 0, na.rm = TRUE)) return(100)
  stand <- vegan::decostand(xy, method = "normalize", MARGIN = 1)
  if (any(!is.finite(stand))) return(0)
  d <- as.numeric(vegan::vegdist(stand, method = "euclidean"))
  cos_sim <- 1 - (d^2) / 2
  100 * max(min(cos_sim, 1), 0)
}

distance_pct <- function(x, y, method) {
  k <- sum(!is.na(x) & !is.na(y))
  if (k == 0) return(NA_real_)
  if (method == "cosine") {
    return(cosine_pct_vegan(x, y))
  } else {
    xy <- rbind(x, y)
    ok <- stats::complete.cases(t(xy))
    xy <- xy[, ok, drop = FALSE]
    if (ncol(xy) == 0) return(NA_real_)
    d <- as.numeric(vegan::vegdist(xy, method = method))
    if (method == "euclidean") {
      return(100 * (1 - d / (5 * sqrt(k))))
    } else if (method == "manhattan") {
      return(100 * (1 - d / (5 * k)))
    } else {
      return(NA_real_)
    }
  }
}

# Map UI label & Wikipedia link for the chosen index
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
    version = 5,
    bg = "#1A1A1A",
    fg = "#FFFFFF",
    primary = "#F24C0C",
    secondary = "#F4C542",
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
        column(1,
          radioButtons("method", "Ordination", choices = c("PCA", "NMDS"), inline = TRUE),
          selectInput("dist_method", "Similarity index",
                      choices = c("Euclidean", "Manhattan", "Cosine"),
                      selected = "Euclidean"),
          numericInput("nmds_seed", "NMDS Seed", value = 42),
          uiOutput("axis_select_ui"),
          radioButtons("selectedTrait", "Highlight Trait", choices = c("None", binary_vars), selected = "None")
        ),
        column(7,
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
          tags$h4("Tables below show the raw Gurometry Scores per Decoder extracted from the google sheets (Note: The ordination plot above and similarity use per-attribute averages across available decoders.)"),
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
        tags$p("This app visualizes Gurometer profiles — ratings on multiple attributes scored from 0 to 5 — in a 2D plot using ordination. Gurus with similar rating patterns appear closer together, while those with different patterns appear farther apart."),
        
        tags$h4("How the data is prepared"),
        tags$ol(
          tags$li("Ratings are loaded from a Google Sheet containing numeric ratings for each guru across graded attributes and binary traits."),
          tags$li("Special cases like “½” are converted to numeric form, and subject name inconsistencies are fixed."),
          tags$li("If multiple raters scored the same guru, the mean rating per attribute is calculated across raters; binary traits are summarised by the maximum recorded value."),
          tags$li("Attributes with zero variance (same score for all gurus) are removed; gurus with missing values for all graded attributes are excluded."),
          tags$li("For Cosine similarity only, each guru’s ratings are normalised to unit length so that distances depend on the pattern of ratings, not their absolute magnitude.")
        ),
        
        tags$h4("How the ordination is computed"),
        tags$p("You can choose a similarity index (Euclidean, Manhattan, or Cosine). The app computes a distance matrix using that index and applies one of two ordination methods:"),
        tags$ul(
          tags$li(tags$b("PCA tab = PCoA:"), " Principal Coordinates Analysis is performed on the chosen distance matrix using ", tags$code("vegan::capscale(comm ~ 1, distance = ...)"), "."),
          tags$li(tags$b("NMDS tab:"), " Non-metric multidimensional scaling is run using ", tags$code("vegan::metaMDS(comm, distance = ...)"), ", which preserves the rank order of distances.")
        ),
        
        tags$h4("Available similarity index options"),
        tags$ul(
          tags$li(tags$b("Cosine:"), " Ratings are first scaled to unit length with ", tags$code("vegan::decostand(method = 'normalize', MARGIN = 1)"),
                  ", then Euclidean distances on these unit vectors are computed. On unit vectors, Euclidean distance is a monotonic transform of Cosine similarity."),
          tags$li(tags$b("Euclidean:"), " Raw rating profiles are compared directly using Euclidean distance."),
          tags$li(tags$b("Manhattan:"), " Raw rating profiles are compared using the sum of absolute differences.")
        ),
        
        tags$h4("Similarity % in the side panel"),
        tags$p("When you select  two gurus, the side panel shows their similarity as a percentage. Similarities are computed from per-attribute averages across available decoders."),
        tags$ul(
          tags$li("For Euclidean and Manhattan: a linear rescale of the distance relative to the maximum possible distance given the 0–5 score range."),
          tags$li("For Cosine: the cosine similarity multiplied by 100%.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Two random subjects by default
  all_subjects <- combined_data$Subject
  set.seed(42)
  selected_subjects <- reactiveVal(sample(all_subjects, 2))

  output$axis_select_ui <- renderUI({
    axes <- if (input$method == "PCA") c("PCoA1", "PCoA2") else c("NMDS1", "NMDS2")
    tagList(
      selectInput("axis1", "X Axis", choices = axes, selected = axes[1]),
      selectInput("axis2", "Y Axis", choices = axes, selected = axes[2])
    )
  })

  # Prepare community matrix based on selected similarity index
  get_comm_for_distance <- reactive({
    dm <- tolower(input$dist_method)
    if (dm == "cosine") {
      vegan::decostand(as.matrix(aspect_data), method = "normalize", MARGIN = 1)
    } else {
      as.matrix(aspect_data)
    }
  })

  # --- Ordination based on selected index ---
  current_ord <- reactive({
    dm <- tolower(input$dist_method)
    comm <- get_comm_for_distance()
    dist_method <- if (dm == "cosine") "euclidean" else dm

    if (input$method == "PCA") {
      vegan::capscale(comm ~ 1, distance = dist_method, add = FALSE)
    } else {
      set.seed(input$nmds_seed)
      vegan::metaMDS(comm, distance = dist_method, trymax = 500, k = 2, autotransform = FALSE)
    }
  })

  site_scores <- reactive({
    ord <- current_ord()
    s <- scores(ord, display = "sites")
    colnames(s) <- if (input$method == "PCA") paste0("PCoA", seq_len(ncol(s))) else paste0("NMDS", seq_len(ncol(s)))
    df <- as.data.frame(s)
    df$Subject <- rownames(s)
    index <- match(df$Subject, combined_data$Subject)
    df$Percentage <- combined_data$Percentage[index]
    df$Average <- combined_data$Average[index]
    df$TraitColor <- if (input$selectedTrait != "None") {
      present <- binary_matrix[[input$selectedTrait]][match(df$Subject, binary_matrix$Subject)] == 1
      ifelse(present, input$selectedTrait, "None")
    } else {
      "None"
    }
    df$IsSelected <- df$Subject %in% selected_subjects()
    df
  })

  output$skyPlot <- renderPlotly({
    df <- site_scores()
    palette <- c("None" = "gray60")
    if (input$selectedTrait != "None") palette[input$selectedTrait] <- "orange"

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

  # Similarity panel (reflects selected index) + Wikipedia link
  output$similarityPanel <- renderUI({
    sel <- selected_subjects()
    if (length(sel) < 2) return(NULL)

    a <- combined_data %>% dplyr::filter(Subject == sel[1]) %>% dplyr::select(all_of(aspect_vars))
    b <- combined_data %>% dplyr::filter(Subject == sel[2]) %>% dplyr::select(all_of(aspect_vars))

    dm <- tolower(input$dist_method)
    sim <- distance_pct(as.numeric(a[1, ]), as.numeric(b[1, ]), dm)
    sim_txt <- if (is.na(sim)) "—" else sprintf("%.1f%%", sim)
    meta <- index_meta(dm)

    tags$div(
      style = "margin-bottom: 12px; padding: 10px; border: 1px solid #444; border-radius: 8px; background:#111;",
      tags$h4(
        tags$a(href = meta$url, target = "_blank", style = "color:#F4C542; text-decoration: none;", meta$label),
        style = "margin-top:0;"
      ),
      tags$p(HTML(sprintf("<b>%s</b> \u2194 <b>%s</b>: <span style='color:#F4C542;font-size:1.2em;'>%s</span>",
                          sel[1], sel[2], sim_txt)))
    )
  })

  make_profile_ui <- function(subj) {
    if (is.null(subj)) return(NULL)
    data <- combined_data %>% filter(Subject == subj)
    aspects <- data[aspect_vars]
    binaries <- data[binary_vars]
    is_active <- subj %in% selected_subjects()

    trait_display <- lapply(names(binaries), function(name) {
      val <- binaries[[name]]
      if (!is.na(val)) {
        prefix <- if (val == -1) "anti-" else if (val == 0.5) "minor " else if (val == 2) "major " else ""
        trait_name <- paste0(prefix, name)
        bg_color <- if (val == -1) {
          "steelblue"
        } else if (val == 0.5) {
          "darkorange"
        } else if (val == 2) {
          "red"
        } else {
          "orange"
        }

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

      tags$h5("Gurometer Scores:"),
      lapply(names(aspects), function(name) {
        val <- aspects[[name]]
        tags$div(style = "margin-bottom: 4px;",
                 tags$span(style = "width: 130px; display: inline-block;", name),
                 tags$progress(value = val, max = 5, style = "width: 150px;")
        )
      }),

      tags$h5("Traits:"),
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

  output$decoderScores1 <- renderUI({
    selected <- selected_subjects()
    if (length(selected) >= 1) {
      tbl <- all_data_clean %>%
        filter(Subject == selected[1]) %>%
        select(decoder, all_of(aspect_vars)) %>%
        arrange(decoder)
      tagList(tags$h5(selected[1]), renderTable(tbl, digits = 2))
    }
  })

  output$decoderScores2 <- renderUI({
    selected <- selected_subjects()
    if (length(selected) >= 2) {
      tbl <- all_data_clean %>%
        filter(Subject == selected[2]) %>%
        select(decoder, all_of(aspect_vars)) %>%
        arrange(decoder)
      tagList(tags$h5(selected[2]), renderTable(tbl, digits = 2))
    }
  })
}

# run the app
guroscope <- shinyApp(ui, server)
guroscope
