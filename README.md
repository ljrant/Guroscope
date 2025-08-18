Guroscope

Interactive R Shiny web app that visualizes Gurometry score profiles of secular gurus rated by the Decoding the Gurus podcast using ordination methods.

In the app, gurus appear as selectable “stars” in a 2D Gurusphere where similarly scored figures group closer together. Clicking a star allows side-by-side comparison of guru profiles (decoder averages). Individual gurometry scores by each decoder are also displayed in a table below the comparison view.

For plotting the 2D Gurusphere, gurometry scores from all decoders are averaged, and then a dimension reduction technique (currently the first two major axes of PCA) is used for ordination.

Features

Data pulled and cleaned directly from Google Sheets (see precompute.R)

Interactive Gurusphere (2D ordination plot with selectable distance metrics)

Clickable stars (gurus) to explore individual profiles

Guru profile comparison in the right-hand panel; raw decoder scores table below

Binary bonus traits (e.g., Monomania, Broicity) can be highlighted as colored stars in the Gurusphere

Deployable to a server or website using R Shiny

Installation

Install R and RStudio.

Install the required packages in R:

install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "readr", "stringr",
  "googlesheets4", "vegan", "ggplot2", "RColorBrewer", "ggrepel", "plotly"
))

Clone this repository:

git clone https://github.com/ljrant/Guroscope.git


Run the app from RStudio or the R console:

shiny::runApp("Guroscope")

The app structure is modular:

precompute.R – loads and processes Google Sheets data

global.R – shared variables and constants

ui.R – defines the user interface

server.R – server logic

Data Source

Data is fetched directly from this public Google Sheet:
Gurometer Scores – Decoding the Gurus

Used libraries

R Shiny – Interactive UI and web deployment

ggplot2 – Visualization

vegan – Ordination (PCA/NMDS)

googlesheets4 – Real-time data import

bslib – Custom Bootstrap 5 theme

plotly – Interactive plots

Credits

Gurometry by Chris Kavanagh & Matt Browne (Decoding the Gurus podcast)

Cursor icon: Illuminati icon by Icons8
