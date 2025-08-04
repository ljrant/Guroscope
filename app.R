# ---- 1. Load Required Packages ----
install_and_load <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
install_and_load(c("googlesheets4", "dplyr", "readr", "stringr", "shiny", "vegan", "ggplot2", "RColorBrewer", "tidyr", "ggrepel", "bslib"))

# ---- 2. Read Google Sheet with all values as characters ----
gs4_deauth()
url <- "https://docs.google.com/spreadsheets/d/1Oe-af4_OmzLJavktcSKGfP0wmxCX0ppP8n_Tvi9l_yc"
raw <- read_sheet(url, col_types = "c", col_names = FALSE, skip = 1)

# Replace fraction and special symbols before parsing
raw <- raw %>%
  mutate(across(everything(), ~ str_replace_all(., c("\u00bd" = "0.5", "1/2" = "0.5"))))

# ---- 3. Extract decoder blocks using CK header ----
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

  tags$style(HTML("#skyPlot:hover { cursor: url('https://img.icons8.com/fluency-systems-regular/48/FFFFFF/illuminati-symbol.png') 16 16, auto; }")),

  tags$style(HTML("
    h1, h2, h3, h4, h5, h6, label, .btn {
      font-family: 'Montserrat', sans-serif;
    }

    .btn {
      background-color: #F24C0C;
      border-color: #F24C0C;
      color: white;
    }

    .btn:hover {
      background-color: #d03f00;
      border-color: #d03f00;
    }

    .form-check-input:checked {
      background-color: #F24C0C;
      border-color: #F24C0C;
    }

    .progress {
      background-color: #333;
    }

    .progress::-webkit-progress-value {
      background-color: #F4C542;
    }

    table, th, td {
      color: white;
    }
  ")),

  titlePanel("Guroscope"),

  fluidRow(
    column(width = 1,
      radioButtons("method", "Ordination Method", choices = c("PCA", "NMDS"), inline = TRUE),
      numericInput("nmds_seed", "NMDS Seed", value = 42),
      uiOutput("axis_select_ui"),
      radioButtons("selectedTrait", "Highlight Trait", choices = c("None", binary_vars), selected = "None")
    ),

    column(width = 7,
      plotOutput("skyPlot", height = "1000px", click = "plot_click")
    ),

    column(width = 4,
      fluidRow(
        column(width = 6, uiOutput("subjectProfile1")),
        column(width = 6, uiOutput("subjectProfile2"))
      )
    )
  ),

  fluidRow(
    column(width = 12,
      tags$h4("Gurometry Scores"),
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
)





server <- function(input, output, session) {
  selected_subjects <- reactiveVal(c())

  observeEvent(input$plot_click, {
    coords <- nearPoints(site_scores(), input$plot_click, threshold = 10, maxpoints = 1)
    if (nrow(coords) > 0) {
      clicked <- coords$Subject[1]
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

  output$axis_select_ui <- renderUI({
    axes <- if (input$method == "PCA") c("PCA1", "PCA2") else c("NMDS1", "NMDS2")
    tagList(
      selectInput("axis1", "X Axis", choices = axes, selected = axes[1]),
      selectInput("axis2", "Y Axis", choices = axes, selected = axes[2])
    )
  })

  current_ord <- reactive({
    if (input$method == "PCA") {
      rda(aspect_data, scale = TRUE)
    } else {
      set.seed(input$nmds_seed)
      metaMDS(aspect_data, distance = "euclidean", trymax = 500, k = 2, autotransform = FALSE)
    }
  })

  site_scores <- reactive({
    ord <- current_ord()
    s <- scores(ord, display = "sites")
    colnames(s) <- if (input$method == "PCA") paste0("PCA", seq_len(ncol(s))) else paste0("NMDS", seq_len(ncol(s)))
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
    df
  })

  output$skyPlot <- renderPlot({
    df <- site_scores()
    palette <- c("None" = "gray60")
    if (input$selectedTrait != "None") palette[input$selectedTrait] <- "orange"

    ggplot(df, aes_string(x = input$axis1, y = input$axis2)) +
      geom_point(aes(color = TraitColor, alpha = Percentage, size = Average), shape = 8) +
      geom_text_repel(aes(label = Subject), size = 4.2, color = "white") +
      scale_color_manual(values = palette) +
      scale_alpha(range = c(0.1, 1)) +
      scale_size(range = c(2, 8)) +
      labs(x = input$axis1, y = input$axis2) +
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
  })

  make_profile_ui <- function(subj) {
    if (is.null(subj)) return(NULL)
    data <- combined_data %>% filter(Subject == subj)
    aspects <- data[aspect_vars]
    binaries <- data[binary_vars]

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
      tags$h4(subj),
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

#run the app
guroscope <- shinyApp(ui, server)
guroscope



