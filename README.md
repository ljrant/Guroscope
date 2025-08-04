# Guroscope
Interactive R Shiny web app that visualizes Gurometry score profiles of secular gurus rated by the Decoding the Gurus podcast. Decoding scores are averaged, and profiles are compared using ordination methods (PCA or NMDS. Subjects appear as selectable "stars" in 2D guruscape, side-by-side comparisons of profiles and and guru traits. in side panel.

## 🔭 Features
- Data pulled directly from Google Sheets
- Interactive Guruscape  (2D ordination plot using either PCA or NMDS of averaged gurumetry scores for ordination). Axis can be rotated for alternative guroscopy. by default PCA1 and PCA2 are used, for PCA the highest scoring Gurus cluster  on the right field of Guruscape. 
- Clickable stars (gurus) to explore individual profiles. The Guru profiles view on right hand panel. individual gurometry scores by different decoders display in bottom panel.
- Binary traits (e.g., "Monomania", "Broicity") color-coded may be highligted as different coloured stars in the guroscape. 

## 📦 Installation

1.Make sure you have R and RStudio installed. 
2.Then install required packages in R:

install.packages(c("shiny", "bslib", "dplyr", "tidyr", "readr", "stringr",
                   "googlesheets4", "vegan", "ggplot2", "RColorBrewer", "ggrepel"))
                
3. Clone the repo and run the app from your R console or RStudio. Or simply copy paste all the code in R-studio and run the app.R script if all dependencies are installed.

### Data Source
Data is fetched directly from this public Google Sheet:
[Gurometer Scores - Decoding the Gurus](https://docs.google.com/spreadsheets/d/1Oe-af4_OmzLJavktcSKGfP0wmxCX0ppP8n_Tvi9l_yc/edit?gid=0#gid=0) 

#### Used libraries:
R Shiny – Interactive UI and web deployment
ggplot2 – Visualization
vegan – Ordination (PCA/NMDS)
googlesheets4 – Real-time data import
bslib – Custom Bootstrap 5 theme

##### 🧠 What Is Gurometry?

*Gurometry* is a satirical rating framework developed by [Chris Kavanagh](https://twitter.com/C_Kavanagh) and [Matt Browne](https://twitter.com/ArthurCDent) for their podcast *Decoding the Gurus*. It rates self-proclaimed intellectuals or “secular gurus” based on recurring traits like "Galaxy Brainness" and "Conspiracy Mongering."

For more on Gurometry, listen to the podcast or view their [Google Sheet](https://docs.google.com/spreadsheets/d/1Oe-af4_OmzLJavktcSKGfP0wmxCX0ppP8n_Tvi9l_yc).

##### Credits
Gurometry: by Chris Kavanagh & Matt Browne, from the Decoding the Gurus podcast
Cursor icon: Illuminati icon by Icons8

This project is for entertainment purposes only. It is not affiliated with or endorsed by the Decoding the Gurus podcast.
