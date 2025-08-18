
# This script is loads the and precalculates the distances in separate files for less computation intensive server deployment. needs to be run every time original data googlesheets is updated or new guru decoded to update the shinyapp.io

# Packages
install_and_load <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    library(p, character.only = TRUE)
  }
}
install_and_load(c("googlesheets4","dplyr","stringr","vegan","readr"))

gs4_deauth()
url <- "https://docs.google.com/spreadsheets/d/1Oe-af4_OmzLJavktcSKGfP0wmxCX0ppP8n_Tvi9l_yc"
raw <- read_sheet(url, col_types = "c", col_names = FALSE, skip = 1)
raw <- raw %>% mutate(across(everything(), ~ str_replace_all(., c("\u00bd" = "0.5", "1/2" = "0.5"))))

decoder_rows <- which(raw[[1]] %in% c("CK","MB","RK"))
decoder_data <- list()
ck_header_row <- decoder_rows[1]
ck_header <- as.character(unlist(raw[ck_header_row, ]))
ck_header[is.na(ck_header) | ck_header == ""] <- paste0("X", which(is.na(ck_header) | ck_header == ""))
ck_header[1] <- "Subject"

for (i in seq_along(decoder_rows)) {
  start_row <- decoder_rows[i] + 1
  end_row   <- if (i < length(decoder_rows)) decoder_rows[i+1] - 1 else nrow(raw)
  block <- raw[start_row:end_row, ]
  colnames(block) <- ck_header
  decoder_data[[ raw[[1]][decoder_rows[i]] ]] <- block
}

manual_fixes <- c(
  "Zizek" = "Slavoj Zizek",
  "MIkhaila Peterson" = "Mikhaila Peterson",
  "Michael O' Fallon (Aaron)" = "Michael O' Fallon",
  "Anna" = "Anna Khachiyan",
  "Dasha" = "Dasha Nekrasova",
  "Sabine" = "Sabine Hossenfelder"
)
for (d in names(decoder_data)) {
  decoder_data[[d]] <- decoder_data[[d]] %>%
    mutate(Subject = dplyr::recode(Subject, !!!manual_fixes),
           decoder = d)
}

graded_vars <- c(
  "Galaxy Brainness","Cultishness","Anti-Establishment","Grievance Mongering",
  "Self-Aggrandisement and Narcicissm","Cassandra Complex","Revolutionary Theories",
  "Pseudo Profound Bullshit","Conspiracy Mongering","Profiteering","Moral Grandstanding"
)
binary_vars <- c(
  "Monomania","Shilling Supplements","Broicity","Charisma","Neologism",
  "Strategic Disclaimers","Rebranding Theories","Loquaciousness","Never admitting error"
)

all_data_clean <- bind_rows(decoder_data) %>%
  filter(!is.na(Subject) & str_trim(Subject) != "") %>%
  relocate(decoder, .before = Subject) %>%
  mutate(across(all_of(graded_vars), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(binary_vars), ~ case_when(
    . %in% c("0.5") ~ 0.5,
    . == "-1" ~ -1,
    TRUE ~ as.numeric(as.character(.))
  )))

combined_data <- all_data_clean %>%
  group_by(Subject) %>%
  summarise(across(all_of(graded_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rowwise() %>%
  mutate(Average = mean(c_across(all_of(graded_vars)), na.rm = TRUE),
         Percentage = Average / 5 * 100) %>%
  ungroup()

binary_summary <- all_data_clean %>%
  group_by(Subject) %>%
  summarise(across(all_of(binary_vars), ~ suppressWarnings(max(.x, na.rm = TRUE))), .groups = "drop") %>%
  mutate(across(all_of(binary_vars), ~ ifelse(is.infinite(.), NA, .)))

metadata <- left_join(combined_data, binary_summary, by = "Subject")

graded_scaled <- as.matrix(metadata[graded_vars]) / 5
rownames(graded_scaled) <- metadata$Subject
comm_norm <- vegan::decostand(graded_scaled, method = "normalize", MARGIN = 1)

distances <- list(
  euclidean = dist(graded_scaled, method = "euclidean"),
  manhattan = dist(graded_scaled, method = "manhattan"),
  cosine    = dist(comm_norm,     method = "euclidean")
)

decoder_scores <- all_data_clean %>%
  select(decoder, Subject, all_of(graded_vars)) %>%
  arrange(Subject, decoder)

saveRDS(distances,      "distances.rds")
saveRDS(metadata,       "metadata.rds")
saveRDS(decoder_scores, "decoder_scores.rds")
cat("✅ Saved distances.rds, metadata.rds, decoder_scores.rds\n")

