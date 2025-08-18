library(shiny)
library(bslib)
library(plotly)

# Bonus traits (server relies on this too)
binary_vars <- c(
  "Monomania","Shilling Supplements","Broicity","Charisma","Neologism",
  "Strategic Disclaimers","Rebranding Theories","Loquaciousness","Never admitting error"
)

# ---------------- About panel helper ----------------
about_panel <- function() {
  tags$div(
    # compact styling for About
    tags$style(HTML("
      .about h3, .about h4 { margin-top: 16px; margin-bottom: 6px; }
      .about p { margin-bottom: 8px; }
      .about ul { margin-top: 4px; margin-bottom: 8px; }
      .about li { margin-bottom: 4px; }
      .about .muted { opacity: .8; }
      .about code { color: #F4C542; }
    ")),
    tags$div(class = "about",
             h4("How to read gurusphere ordination plot"),
             tags$ul(
               tags$li("An ordination plot displays all rated gurus as points in two dimensions based on first two axis principal component analysis. The Distances between points roughly summarize how similar their overall profiles are based on the chosen similarity index. Manhattan distance is used by default. Tap points to select two gurus to compare in comparison chart below for more detailed breakdown of guru characteristics and their differences."),
               tags$li("Right panel: mirrored bar chart comparing their graded characteristics. Toggle raw vs. differences at the top."),
               tags$li("Bonus traits awarded by decoders are listed under the bar chart and may be optionally highlighted in ordination from the settings panel. All distance measures use averaged values of all decoders. Raw decoding scores for each decoder may be viewed as table below the comparison chart")
             ),
             h4("Data & methods"),
             p(
               "Characteristic scores come from multiple decoders and are averaged per subject. ",
               "Euclidean / Manhattan / Cosine distances are available; ordination uses ",
               code("vegan::capscale"), ". ",
               "Similarity % is normalized from the chosen distance matrix."
             ),
  
             h3("References"),
             h4("Data Source"),
             p(
               "Data is fetched directly from this public Google Sheet: ",
               tags$a(
                 href = "https://docs.google.com/spreadsheets/d/1Oe-af4_OmzLJavktcSKGfP0wmxCX0ppP8n_Tvi9l_yc",
                 target = "_blank",
                 "Gurometer Scores – Decoding the Gurus"
               ),
             ),
             h4("Used libraries"),
             tags$ul(
               tags$li(tags$b("R Shiny"), " – Interactive UI and web deployment"),
               tags$li(tags$b("ggplot2"), " – Visualization"),
               tags$li(tags$b("vegan"), " – Ordination (PCA/NMDS)"),
               tags$li(tags$b("googlesheets4"), " – Real-time data import"),
               tags$li(tags$b("bslib"), " – Custom Bootstrap 5 theme")
             ),
             
             h3("Credits"),
             tags$ul(
               tags$li("Gurometry by Chris Kavanagh & Matt Browne (Decoding the Gurus)"),
               tags$li("Cursor icon: ",
                       tags$a(href="https://icons8.com/icons/set/illuminati", target="_blank",
                              "Illuminati icon by Icons8"))
             ),
             p(class = "muted",
               "This project is for entertainment purposes only. It is not affiliated with or endorsed by the ",
               "Decoding the Gurus podcast."
             ),
             
             tags$hr(),
             p(
               "Authored by Lauri Rantanen — ",
               tags$a(
                 href = "https://github.com/ljrant/Guroscope",
                 target = "_blank",
                 "Guroscope GitHub page"
               )
             )
    )
  )
}

# ---------------- Main UI ----------------
fluidPage(
  theme = bs_theme(
    version   = 5,
    bg        = "#0E0E0E",
    fg        = "#FFFFFF",
    primary   = "#F24C0C",
    secondary = "#F4C542",
    base_font = font_google("Montserrat")
  ),
  
  tags$head(tags$style(HTML("
    .container-fluid, .col, .row, .card, .form-group, .form-check, .btn { margin-bottom: .35rem; }
    .btn { padding: .25rem .55rem; }
    .form-select, .form-control { padding: .25rem .5rem; }
    .form-label { margin-bottom: .25rem; }
    .panel-box { border: 1px solid #2b2b2b; border-radius: 12px; background: #111; }
    .panel-tight { padding: 10px; }

    /* Ordination container (kept flexible; server controls scaling) */
    .square { aspect-ratio: 1 / 1; width: 100%; min-height: 320px; border: 1px solid #222; border-radius: 12px; background: #0b0b0b; overflow: hidden; }
    .plot-fill { height: 100%; width: 100%; }

    /* Tighter mobile spacing */
    @media (max-width: 992px) {
      .col-lg-8 { width: 100% !important; }
      .col-lg-4 { width: 100% !important; margin-top: .5rem; }
    }
    @media (max-width: 576px) {
      body { font-size: 0.95rem; }
      h4, .h4 { font-size: 1.05rem; }
      .btn, .form-select, .form-control { font-size: .9rem; }
      .panel-tight { padding: 8px; }
      .panel-box { border-radius: 10px; }
    }

    #skyplot-wrapper:hover {
      cursor: url('https://img.icons8.com/fluency-systems-regular/48/FFFFFF/illuminati-symbol.png') 16 16, auto;
    }
  "))),
  
  titlePanel("Explore the Gurusphere"),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "Click to select",
      fluidRow(
        # LEFT: ordination + settings (hidden by default) below it
        column(
          width = 8, class = "col-12 col-lg-8",
          
          # Ordination
          div(id = "skyplot-wrapper", class = "square panel-box",
              div(class = "plot-fill", plotlyOutput("skyPlot", height = "100%"))
          ),
          
          br(),
          
          # Display settings (hidden by default; server renders toggle+box)
          div(class = "panel-box panel-tight",
              uiOutput("settingsPanel")
          )
        ),
        
        # RIGHT: comparison card
        column(
          width = 4, class = "col-12 col-lg-4",
          uiOutput("comparePanel")
        )
      )
    ),
    
    tabPanel(
      "About",
      about_panel()
    )
  )
)
