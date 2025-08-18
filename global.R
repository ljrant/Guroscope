# Shared packages & data
library(shiny)
library(shinyMobile)
library(plotly)
library(vegan)
library(dplyr)
library(ggplot2)
library(tidyr)

# Data
precomputed     <- readRDS("distances.rds")
metadata        <- readRDS("metadata.rds")
decoder_scores  <- tryCatch(readRDS("decoder_scores.rds"), error = function(e) NULL)

# Vars
graded_vars <- c(
  "Galaxy Brainness","Cultishness","Anti-Establishment","Grievance Mongering",
  "Self-Aggrandisement and Narcicissm","Cassandra Complex","Revolutionary Theories",
  "Pseudo Profound Bullshit","Conspiracy Mongering","Profiteering","Moral Grandstanding"
)
binary_vars <- c(
  "Monomania","Shilling Supplements","Broicity","Charisma","Neologism",
  "Strategic Disclaimers","Rebranding Theories","Loquaciousness","Never admitting error"
)
