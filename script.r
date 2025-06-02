##################################################
# Per-file Sentiment Analysis of FED Speeches
# Authosr: [Artem Derenko, Daniil Yarovyi]
##################################################

# ==== 1. Load Required Libraries ====
required_packages <- c("tidyverse", "tidytext", "textdata", "tm", "ggplot2", "wordcloud", "gridExtra", "RColorBrewer")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ==== 2. Set Working Directory and Load Files ====
folder_path <- "speeches"
files <- list.files(path = folder_path, pattern = "\\.txt$|\\.csv$", full.names = TRUE)

# ==== 3. Load Sentiment Lexicons ====
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

# ==== 4. Define Processing Function ====
process_file <- function(file_path) {
  file_name <- basename(file_path)
  message("Processing: ", file_name)
  
  # Read file
  text_df <- if (grepl("\\.csv$", file_path)) {
    read_csv(file_path, col_names = FALSE) %>%
      unite("text", everything(), sep = " ", na.rm = TRUE)
  } else {
    tibble(text = readLines(file_path, warn = FALSE))
  }
  
  # Preprocessing and tokenization
  tokens <- text_df %>%
    mutate(text = tolower(text),
           text = str_replace_all(text, "[[:punct:]]", " "),
           text = str_replace_all(text, "[[:digit:]]", " "),
           text = str_squish(text)) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word")
  
  # Word Frequencies
  word_freq <- tokens %>%
    count(word, sort = TRUE)
  
  # Wordcloud
  set.seed(123)
  wordcloud(words = word_freq$word,
            freq = word_freq$n,
            min.freq = 3,
            max.words = 100,
            random.order = FALSE,
            colors = brewer.pal(8, "Dark2"),
            main = paste("Wordcloud -", file_name))
  
  # Sentiment Analysis
  sentiment_nrc <- tokens %>% inner_join(nrc, by = "word")
  sentiment_afinn <- tokens %>% inner_join(afinn, by = "word")
  sentiment_bing <- tokens %>% inner_join(bing, by = "word")
  
  # NRC Plot
  nrc_plot <- sentiment_nrc %>%
    count(sentiment, sort = TRUE) %>%
    ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = paste("NRC Sentiments -", file_name), x = "Sentiment", y = "Count") +
    theme_minimal()
  
  # AFINN Trend Plot
  afinn_plot <- sentiment_afinn %>%
    mutate(index = row_number()) %>%
    ggplot(aes(index, value)) +
    geom_line(color = "steelblue") +
    geom_smooth(method = "loess", se = FALSE, color = "darkred") +
    labs(title = paste("AFINN Trend -", file_name), x = "Word Index", y = "Sentiment Score") +
    theme_minimal()
  
  # Bing Plot (positive vs negative)
  bing_plot <- sentiment_bing %>%
    count(sentiment, sort = TRUE) %>%
    ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "firebrick")) +
    labs(title = paste("Bing Sentiment -", file_name), x = "Sentiment", y = "Word Count") +
    theme_minimal()
  
  
  # Display Plots
  grid.arrange(nrc_plot, afinn_plot, bing_plot, ncol = 3)
  
}

# ==== 5. Run Analysis for Each File ====
for (file in files) {
  process_file(file)
}

# ==== 6. Session Info ====
sessionInfo()
