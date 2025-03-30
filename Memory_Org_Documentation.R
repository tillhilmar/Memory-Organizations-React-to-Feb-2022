# R code documentation for the Manuscript "Signifying the Present in Links to the Past: Memory Organizations React to the February 24, 2022 Russian Full-Scale Invasion of Ukraine"
# Note: Portions of this analysis were conducted using Python (refer to the provided Python code)

# Load necessary libraries
library("academictwitteR")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("ggthemes")
library("httr")
library("jsonlite")
library("magrittr")
library("purrr")
library("tibble")
library("plyr")
library("dplyr")
library("tidyr")
library("data.table")
library("readxl")
library("writexl")
library("stopwords")
library("quanteda")
library("quanteda.textstats")
library("quanteda.textmodels")
library("kableExtra")
library("igraph")
library("knitr")

# Twitter Data Download Example
# ----------------------------------------------------------------------------------

# Download tweets from a specific user (in this example, USHMM) within a given date range.
USHMM_tweets <- get_all_tweets(
  users = "8487622", 
  start_tweets = "2007-08-01T00:00:00Z", 
  end_tweets = "2023-03-28T23:59:59Z", 
  bearer_token = "x",  # Replace 'x' with actual bearer token
  data_path = "data2/", 
  n = 900000  # Maximum number of tweets to fetch
)

# Fetch user profile information.
USHMM_user <- get_user_profile("8487622", bearer_token = "x")

# Retrieve list of accounts the user is following.
following_USHMM <- get_user_following("8487622", bearer_token = "x")

# Save fetched data to RDS files for later use.
saveRDS(USHMM_tweets, file = "USHMM_tweets.rds")
saveRDS(USHMM_user, file = "USHMM_user.rds")
saveRDS(following_USHMM, file = "following_USHMM.rds")

# Data Preparation and Cleaning
# ----------------------------------------------------------------------------------
# Load the dataset containing tweets from 139 memory organizations
ALL_memoryorg_final <- readRDS("ALL_memoryorg_final.rds")

# Remove duplicate entries based on unique tweet IDs
ALL_memoryorg_final <- ALL_memoryorg_final[!duplicated(ALL_memoryorg_final[, c("unique_tweet_id")]),]

# Inspect the range of tweet creation dates and the diversity of data
max(ALL_memoryorg_final$created_at)
min(ALL_memoryorg_final$created_at)
sapply(ALL_memoryorg_final, function(x) length(unique(x)))
unique(ALL_memoryorg_final$name)

# Manual Data Annotation for Country Information
# ----------------------------------------------------------------------------------
# Export the data for manual annotation of country names
write_xlsx(ALL_memory_final_unique_short,"ALL_memory_final_unique_short_new.xlsx")

# Read the annotated data back into R
country_coding <- read_excel("ALL_memory_final_unique_short_new.xlsx")
ALL_memoryorg_final <- left_join(ALL_memoryorg_final, country_coding[, c("country", "author_id")], by = "author_id")

# Data Analysis: Tweets and Organizations by Country
# ----------------------------------------------------------------------------------
# Analyze tweet counts and the number of organizations per country
table(ALL_memoryorg_final$country)

# Data Visualization: Account Creation Dates
# ----------------------------------------------------------------------------------
# Visualize the distribution of account creation dates for the organizations
ALL_memoryorg_plot <- ALL_memoryorg_final
ALL_memoryorg_plot$user_created_at <- as.Date(ALL_memoryorg_plot$user_created_at)
ggplot(ALL_memoryorg_plot, aes(x = user_created_at)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(x = "Date", y = "Tweet Count") +
  ggtitle("Distribution of Tweets over Time") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Tweeting Activity Related to Specific Events
# ----------------------------------------------------------------------------------
# Subset data before specific dates to analyze tweeting activity around key events
subset_before_Feb_2014 <- subset(ALL_memoryorg_final, created_at < '2014-02-20 00:00:00')
subset_before_Feb_2014_unique  <- subset_before_Feb_2014[!duplicated(subset_before_Feb_2014[, c("author_id")]),]

# More subsets for detailed analysis
subset_before_Feb_2022 <- subset(ALL_memoryorg_final, created_at < '2022-02-24 00:00:00')
subset_before_Feb_2022_unique  <- subset_before_Feb_2022[!duplicated(subset_before_Feb_2022[, c("author_id")]),]

# Data Visualization: Tweet Distribution Over Time
# ----------------------------------------------------------------------------------
# Use a custom color palette for visualizations
wsjPal <- c('#1C366B','#C4CFD0','#1DACE8','#F24D29','#76A08A','#9A872D')

# Create a plot to visualize tweet distribution over time (this plot appears in Appendix B)
ggplot(ALL_memoryorg_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "Distribution of Tweets over Time", subtitle = "n = 740 720") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))

# Higher resolution for publication
plot_publ <- ggplot(ALL_memoryorg_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "Distribution of Tweets over Time", subtitle = "n = 740 720") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))

ggsave(
  filename = "total_tweet_distribution.png",
  plot = plot_publ,
  dpi = 600,
  width = 10,
  height = 6,
  units = "in"
)


# Detailed Analysis and Subsetting for Specific Periods
# ----------------------------------------------------------------------------------
# Subsets for analyses around specific dates, comparing tweeting activity to average
subset_Feb24 <- subset(ALL_memoryorg_final, created_at > '2022-02-24 00:00:00' & created_at <= '2022-03-03 00:00:00')
subset_post_Feb24 <- subset(ALL_memoryorg_final, created_at > '2022-02-24 00:00:00')
subset_pre_Feb24 <- subset(ALL_memoryorg_final, created_at < '2022-02-24 00:00:00')


# Analysis of Twitter Activity for Memory Organizations
# ----------------------------------------------------------------------------------
# Calculating the total number of tweets per organization
number_tweets <- ALL_memoryorg_final %>% 
  group_by(author_id) %>% 
  tally()

ALL_memoryorg_final <- left_join(ALL_memoryorg_final, number_tweets, by = "author_id")
ALL_memoryorg_final$tweet_count <- as.numeric(ALL_memoryorg_final$tweet_count)

# Calculating the average number of tweets per week since account creation
max_date_ALL <- as_date("2023-03-30")
diff_dates_ALL <- as.numeric(difftime(max_date_ALL, ALL_memoryorg_final$user_created_at, units = "weeks"))
ALL_memoryorg_final$weeks_existed <- diff_dates_ALL
average_tweets_total <- ALL_memoryorg_final$tweet_count / ALL_memoryorg_final$weeks_existed
ALL_memoryorg_final$average_tweets_total <- average_tweets_total

# Analyzing tweet activity following Feb 24, 2022
count_by_org <- subset_Feb24 %>% 
  group_by(author_id) %>% 
  tally() %>%
  rename(number_tweets_criticalperiod = n)

count_by_org1 <- left_join(count_by_org, ALL_memoryorg_final[, c("author_id", "username")], by = "author_id")
count_by_org1_unique <- count_by_org1[!duplicated(count_by_org1[, c("author_id")]),]

subset_Feb24 <- left_join(subset_Feb24, count_by_org, by = "author_id")
subset_Feb24$average_tweets_fourweeks <- subset_Feb24$average_tweets_total * 4
diff_tweeting_activity <- subset_Feb24$number_tweets_criticalperiod / subset_Feb24$average_tweets_fourweeks
subset_Feb24$diff_tweeting_activity <- diff_tweeting_activity
subset_Feb24_unique_author_id <- subset_Feb24[!duplicated(subset_Feb24[, c("author_id")]), ]

# Preparing an overview table of organizations
ALL_memoryorg_final_table <- subset(ALL_memoryorg_final_unique[c("name", "username", "country", "location", "user_created_at", "n", "average_tweets_total")])
names(ALL_memoryorg_final_table)[names(ALL_memoryorg_final_table) == "n"] <- "number of tweets in total"
names(ALL_memoryorg_final_table)[names(ALL_memoryorg_final_table) == "user_created_at"] <- "account created"
names(ALL_memoryorg_final_table)[names(ALL_memoryorg_final_table) == "average_tweets_total"] <- "average number of tweets in four weeks"

ALL_memoryorg_final_table <- ALL_memoryorg_final_table %>%
  left_join(subset_Feb24_unique_author_id[, c("name", "number_tweets_criticalperiod", "diff_tweeting_activity")], by = "name")
ALL_memoryorg_final_table$number_tweets_criticalperiod[is.na(ALL_memoryorg_final_table$number_tweets_criticalperiod)] <- 0
ALL_memoryorg_final_table$diff_tweeting_activity[is.na(ALL_memoryorg_final_table$diff_tweeting_activity)] <- 0
ALL_memoryorg_final_table$diff_tweeting_activity <- round(ALL_memoryorg_final_table$diff_tweeting_activity, 1)

ALL_memoryorg_final_table$`account created` <- substr(ALL_memoryorg_final_table$`account created`, 1, 10)
ALL_memoryorg_final_table <- ALL_memoryorg_final_table %>% arrange(country)
ALL_memoryorg_final_table$`average number of tweets in four weeks` <- round(ALL_memoryorg_final_table$`average number of tweets in four weeks`, 1)

# Exporting the overview table
ALL_memoryorg_final_table[1:139, 1:9] %>%
  kbl(caption = "Overview of Organizations in this Sample") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "Overview_Organizations_new.html", self_contained = T)


# Analysis of Tweeting Activity Regarding Ukraine
# ----------------------------------------------------------------------------------
# Identify tweets mentioning Ukraine in various languages (28 European languages)

translations_Ukraine <- c(
  "Ukraine", "Ukraina", "Україна", "Украина", "Ucraina", "Ukraine", "Ukraina", "Ukraina", "Ukraina", "Ucrania", "Oekraïne", "Ukrajina",
  "Ukraina", "Ukrajna", "Ukrajina", "Ukrajina", "Ukrajna", "Ukraina", "Ukraina", "Ukrajina", "Ukrajina", "Ukrajina", "Ukrajina",
  "Ukrajina", "Ukrajina", "Ukrajina", "Ukraina", "Ukraina", "Ukraina", "Ukrajna", "Ukrajina", "Ucraina", "Ucraina", "Ukrajina")
subset_Feb24_keyword_Ukraine <- subset_Feb24[grep(paste(translations_Ukraine, collapse = "|"), subset_Feb24$text), ]

# Combine tweets mentioning Ukraine with those from Ukrainian organizations for comprehensive analysis
subset_Ukrainian_organizations <- subset(ALL_memoryorg_final, country =="Ukraine")
ALL_Ukraine_subset <- rbind(subset_Ukrainian_organizations, subset_keyword_Ukraine)
ALL_Ukraine_subset <- ALL_Ukraine_subset[!duplicated(ALL_Ukraine_subset[, c("unique_tweet_id")]),]

# Save and load the comprehensive Ukraine-related tweet dataset
saveRDS(ALL_Ukraine_subset, "ALL_Ukraine_subset.rds")
ALL_Ukraine_subset <- readRDS("ALL_Ukraine_subset.rds")

# Analyzing the Ukraine-Related Subset
# ----------------------------------------------------------------------------------

# Assess the diversity of data within the Ukraine-related subset
sapply(ALL_Ukraine_subset, function(x) length(unique(x)))

# Calculate the number of tweets per organization within this subset
number_tweets_Ukraine <- ALL_Ukraine_subset %>% 
  group_by(author_id) %>% 
  tally()

# Merge the tweet counts back into the main dataset
ALL_Ukraine_subset <- left_join(ALL_Ukraine_subset, number_tweets_Ukraine, by = "author_id")

# Convert tweet counts to numeric for further analysis
ALL_Ukraine_subset$tweet_count <- as.numeric(ALL_Ukraine_subset$tweet_count)

# Remove duplicates based on author ID for focused analysis
ALL_Ukraine_subset_unique_author_id <- ALL_Ukraine_subset[!duplicated(ALL_Ukraine_subset[, c("author_id")]),]

# Calculate the percentage of Ukraine-related tweets for each organization
ALL_Ukraine_subset_unique_author_id <- ALL_Ukraine_subset_unique_author_id %>%
  mutate(share_ukraine_talk = (n.y / n.x) * 100)

# Determine the median percentage of Ukraine-related tweets
median(ALL_Ukraine_subset_unique_author_id$share_ukraine_talk)

# Tweet Distribution by Country
# ----------------------------------------------------------------------------------

# Group and count tweets by country, then order by count
tweetsbycountry_Ukraine <- ALL_Ukraine_subset %>% 
  group_by(country) %>% 
  tally() %>% 
  arrange(desc(n))

# Create a table to visualize the tweet distribution by country
tweetsbycountry_Ukraine[1:17, 1:2] %>%
  kbl(caption = "Tweets Subcorpus Ukraine by Country") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "Tweets_Subcorpus_Ukraine_Country.html", self_contained = T)

# Date Analysis for Highest Activity
# ----------------------------------------------------------------------------------

# Frequency table of creation dates for the Ukraine-related tweets
created_at_freq <- table(as.Date(ALL_Ukraine_subset$created_at))

# Sort the frequencies to identify dates with the highest traffic
sorted_created_at_freq <- sort(created_at_freq, decreasing = TRUE)

# Extract the top 60 dates with the highest tweet traffic
top_created_at <- head(sorted_created_at_freq, n = 60)

# Display the dates with the highest traffic
print(top_created_at)

# Visualizing Ukraine-Related Tweets Over Time
# ----------------------------------------------------------------------------------

# Prepare the data for plotting
ALL_Ukraine_subset_plot <- ALL_Ukraine_subset
ALL_Ukraine_subset_plot$created_at <- as.Date(ALL_Ukraine_subset$created_at)

# Create a plot to visualize the distribution of Ukraine-related tweets over time
ggplot(ALL_Ukraine_subset_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "Distribution of Tweets that Mention Ukraine or are authored by Ukrainian Organizations", subtitle = "n = 26 571") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))

# Higher resolution for publication
plot_publ1 <- ggplot(ALL_Ukraine_subset_plot, aes(x = created_at)) +
  geom_histogram(binwidth = 1, fill = wsjPal[1], color = "#3A5489") +
  labs(x = "Date", y = "Tweet Count", title = "Distribution of Tweets that Mention Ukraine or are authored by Ukrainian Organizations", subtitle = "n = 26 571") +
  scale_x_date(breaks = "1 year", date_labels= "%b-%Y") +
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 10, family="sans"),
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust = 1))

ggsave(
  filename = "ukraine_tweet_distribution.png",
  plot = plot_publ1,
  dpi = 600,
  width = 10,
  height = 6,
  units = "in"
)

# Comparison with Tweeting Activity in 2014
# ----------------------------------------------------------------------------------

# Subset tweets around the time of the Crimea occupation by Russian troops in 2014
subset_Feb20_2014 <- subset(ALL_Ukraine_subset, created_at > '2014-02-20 00:00:00' & created_at <= '2014-03-06 00:00:00')

# Group and count tweets by country for the specified period
tweetsbycountry_subset_Feb20_2014 <- subset_Feb20_2014 %>% 
  group_by(country) %>% 
  tally() %>% 
  arrange(desc(n))

# Extend the analysis period to six months post-occupation and analyze tweet content related to Ukraine
subset_Feb20_2014_extendedtimeframe <- subset(ALL_memoryorg_final, created_at > '2014-02-20 00:00:00' & created_at <= '2014-08-20 00:00:00')
subset_keyword_Ukraine_Feb20_2014_extendedtimeframe <- subset_Feb20_2014_extendedtimeframe[grep(paste(translations_Ukraine, collapse = "|"), subset_Feb20_2014_extendedtimeframe$text),]

# Analyze tweeting activity by German organizations during the same period
table(subset_Feb20_2014_extendedtimeframe$country)

# Identify active organizations by country during the extended timeframe
subset_Feb20_2014_extendedtimeframe_unique <- subset_Feb20_2014_extendedtimeframe[!duplicated(subset_Feb20_2014_extendedtimeframe[, c("author_id")]),]
subset_Feb20_2014_extendedtimeframe_unique %>% group_by(country) %>% tally()

# Comparison with Tweeting Activity in 2023
# ----------------------------------------------------------------------------------

# Subset tweets for a comparison period in 2023
subset_Feb24_2023 <- subset(ALL_memoryorg_final, created_at > '2023-02-24 00:00:00' & created_at <= '2023-03-03 00:00:00')
subset_keyword_Ukraine_Feb24_2023 <- subset_Feb24_2023[grep(paste(translations_Ukraine, collapse = "|"), subset_Feb24_2023$text),]

# Language Translation of the Ukraine Subset
# ----------------------------------------------------------------------------------

# Note: Translations of the "ALL_Ukraine_subset" df were exported, then uploaded to DeepL for translation
# The translated tweets were then re-imported into R

# ----------------------------------------------------------------------------------

# Read the translated Ukraine subset data
ALL_Ukraine_subset_TRANS <- readRDS("ALL_Ukraine_subset_TRANS.rds")

# Ensure unique identifiers for each tweet
install.packages("digest")
library(digest)
ALL_Ukraine_subset_TRANS$new_id <- sapply(ALL_Ukraine_subset_TRANS$unique_tweet_id, function(x) {
  substr(digest(paste(x), algo = "sha1"), 1, 10)
})
length(unique(ALL_Ukraine_subset_TRANS$new_id)) == nrow(ALL_Ukraine_subset_TRANS)

# Analyze tweets for the month following February 24th, 2022
ALL_Ukraine_subset_TRANS_Feb24_MONTH <- subset(ALL_Ukraine_subset_TRANS, created_at > '2022-02-24 00:00:00' & created_at <= '2022-03-24 00:00:00')
table(ALL_Ukraine_subset_TRANS_Feb24_MONTH$country)
unique(ALL_Ukraine_subset_TRANS_Feb24_MONTH$username)

# Subset Analysis by Country
# ----------------------------------------------------------------------------------

# Create a subset for German tweets within the specified month.
ALL_Ukraine_subset_TRANS_Feb24_MONTH_germansubset <- subset(ALL_Ukraine_subset_TRANS_Feb24_MONTH, country == "Germany")

# Display the first 100 tweets' text from the German subset for review.
ALL_Ukraine_subset_TRANS_Feb24_MONTH_germansubset$text[1:100]

# Create subsets for Polish and Ukrainian tweets within the same timeframe.
ALL_Ukraine_subset_TRANS_Feb24_MONTH_polishsubset <- subset(ALL_Ukraine_subset_TRANS_Feb24_MONTH, country == "Poland")
ALL_Ukraine_subset_TRANS_Feb24_MONTH_ukrainiansubset <- subset(ALL_Ukraine_subset_TRANS_Feb24_MONTH, country == "Ukraine")

# Data Ordering and Export - Preparing the Data for the NER model and the qualitative analysis
# ----------------------------------------------------------------------------------

# Order the entire month's dataset by tweet creation date.
ALL_Ukraine_subset_TRANS_Feb24_MONTH_ordered <- ALL_Ukraine_subset_TRANS_Feb24_MONTH[order(ALL_Ukraine_subset_TRANS_Feb24_MONTH$created_at), ]

# Select relevant columns for a concise dataset.
ALL_Ukraine_subset_TRANS_Feb24_MONTH_ordered_short <- subset(ALL_Ukraine_subset_TRANS_Feb24_MONTH_ordered[c("country", "text", "username", "new_id", "created_at")])

# Export the ordered and shortened dataset to an Excel file.
write_xlsx(ALL_Ukraine_subset_TRANS_Feb24_MONTH_ordered_short, "ALL_Ukraine_subset_TRANS_Feb24_MONTH_ordered_short.xlsx")

# Quanteda Text Analysis of the Translated Subcorpus (not used in the analysis)
# For explorative purposes
# ----------------------------------------------------------------------------------

# Load English stopwords and quanteda text plotting library
stopwords_english <- stopwords("english")

# Create a quanteda corpus from the translated Ukraine subset
ALL_Ukraine_subset_TRANS_corp <- quanteda::corpus(ALL_Ukraine_subset_TRANS)

# Tokenize corpus, removing punctuation, symbols, URLs, and applying other filters
ALL_Ukraine_subset_TRANS_tokens <- tokens(ALL_Ukraine_subset_TRANS_corp, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE)

# Create a document-feature matrix and clean it by removing stopwords and '@' mentions
ALL_Ukraine_subset_TRANS_dfm <- dfm(ALL_Ukraine_subset_TRANS_tokens)
ALL_Ukraine_subset_TRANS_dfm_clean <- dfm_remove(ALL_Ukraine_subset_TRANS_dfm, stopwords_english)
ALL_Ukraine_subset_TRANS_dfm_clean <- dfm_remove(ALL_Ukraine_subset_TRANS_dfm_clean, pattern = "@*")

# Comparative keyness analysis before and after Feb 2022 for the entire dataset and by country
pop <- ifelse(docvars(ALL_Ukraine_subset_TRANS_dfm_clean, "created_at") >= '2022-02-24', "after_Feb2022", "before_Feb2022")
dfmat1 <- dfm(ALL_Ukraine_subset_TRANS_dfm_clean, groups = pop)
tstat1 <- textstat_keyness(dfmat1, target = "after_Feb2022")
textplot_keyness(tstat1, color = c("darkblue", "lightblue"), n = 15)

# Repeat keyness analysis for subsets: Ukraine, Poland (excluding KresySiberia), and the USA
# Each subset involves grouping by date, calculating keyness, and plotting results

# Export country-specific subsets to Excel, with special handling for Poland to exclude KresySiberia
Germany <- subset(ALL_Ukraine_subset_TRANS, country == "Germany")
write_xlsx(Germany[,c("unique_tweet_id", "text", "created_at")], "Germany.xlsx")

Ukraine <- subset(ALL_Ukraine_subset_TRANS, country == "Ukraine")
write_xlsx(Ukraine[,c("unique_tweet_id", "text", "created_at")], "Ukraine.xlsx")

Poland <- subset(ALL_Ukraine_subset_TRANS, country == "Poland" & username != "KresySiberia")
write_xlsx(Poland[,c("unique_tweet_id", "text", "created_at")], "Poland_noKresy.xlsx")

Poland_Kresy <- subset(ALL_Ukraine_subset_TRANS, country == "Poland" & username == "KresySiberia")
write_xlsx(Poland_Kresy[,c("unique_tweet_id", "text", "created_at")], "Poland_Kresy.xlsx")

# Part of Speech Tagging and Named Entity Recognition
# ----------------------------------------------------------------------------------

# Preparing to Export and use in Python to run NER model
# ----------------------------------------------------------------------------------

# Select relevant columns for NER and convert author IDs to character type.
df_NER_translated <- subset(ALL_Ukraine_subset_TRANS[c("author_id", "new_id", "text", "username", "country", "created_at")])
df_NER_translated$author_id <- as.character(df_NER_translated$author_id)

write_xlsx(df_NER_translated, "df_NER_translated_Aug1.xlsx")

# ----------------------------------------------------------------------------------

# Part of Speech Tagging and Named Entity Recognition - not performed in R
# Consult the Python code for preprocessing and the Spacy model

# ----------------------------------------------------------------------------------

# Load NER results and join with main dataset
entityrecognition_results_trf_basic_Dec6 <- readxl::read_xlsx("entity_recognition_results_trf_basic_Dec6_cleaned.xlsx")
ALL_Ukraine_subset_NER_Dec6 <- left_join(ALL_Ukraine_subset_TRANS, entityrecognition_results_trf_basic_Dec6)

# Preprocessing Short Texts
preprocessed <- readxl::read_xlsx("df_NER_translated_preprocessed_Aug1.xlsx")
preprocessed_short <- preprocessed[c("new_id", "clean_text")]

# Joining preprocessed short texts with NER results
entityrec_Dec06 <- left_join(ALL_Ukraine_subset_NER_Dec6, preprocessed_short)

# Save preprocessed short texts for AI re-evaluation
write_xlsx(preprocessed_short, "preprocessed_short.xlsx")

# Load refined NER results
entityrecognition_results_trf_Dec7 <- readxl::read_xlsx("entity_recognition_results_trf_Dec7_cleaned.xlsx")

# Define and Apply Cleaning Functions
# Function to remove shorter duplicates within semicolon-separated lists
remove_duplicates <- function(s) {
  words <- unlist(strsplit(s, ";"))
  words <- trimws(words) # Trims leading and trailing whitespaces
  
# Function to check if a word is a subset of another
  is_subset <- function(word, phrase) {
    word_trimmed <- trimws(word)
    phrase_trimmed <- trimws(phrase)
    return(word_trimmed != phrase_trimmed && grepl(paste0("\\b", word_trimmed, "\\b"), phrase_trimmed))
  }
  
  # Remove shorter duplicates
  for (i in seq_along(words)) {
    for (j in seq_along(words)) {
      if (i != j && is_subset(words[i], words[j])) {
        words[i] <- ""
        break
      }
    }
  }
  
  # Remove empty elements and return the result
  words <- words[words != ""]
  return(paste(words, collapse = "; "))
}

# Function to remove exact duplicates from a semicolon-separated string
remove_exact_duplicates <- function(s) {
  words <- unlist(strsplit(s, ";"))
  words <- trimws(words) # Trims leading and trailing whitespaces
  unique_words <- unique(words)
  return(paste(unique_words, collapse = "; "))
}

# Function to remove duplicates by keeping the longer version
remove_substring_duplicates <- function(s) {
  words <- unlist(strsplit(s, ";"))
  words <- trimws(words) # Trims leading and trailing whitespaces
  
  # Function to check if a word is a subset of another
  is_subset <- function(word, phrase) {
    word_trimmed <- trimws(word)
    phrase_trimmed <- trimws(phrase)
    return(word_trimmed != phrase_trimmed && grepl(paste0("\\b", word_trimmed, "\\b"), phrase_trimmed))
  }
  
  # Remove shorter duplicates
  for (i in seq_along(words)) {
    for (j in seq_along(words)) {
      if (i != j && is_subset(words[i], words[j])) {
        words[i] <- ""
        break
      }
    }
  }
  
  # Remove empty elements and return the result
  words <- words[words != ""]
  return(paste(words, collapse = "; "))
}

# Apply cleaning functions to 'norp' and 'event' columns
entityrecognition_results_trf_Dec7$norp <- sapply(entityrecognition_results_trf_Dec7$norp, remove_duplicates)
entityrecognition_results_trf_Dec7$event <- sapply(entityrecognition_results_trf_Dec7$event, remove_exact_duplicates)
entityrecognition_results_trf_Dec7$event <- sapply(entityrecognition_results_trf_Dec7$event, remove_substring_duplicates)

# Convert "NA" string values to actual NA in 'event' and 'norp' columns
entityrecognition_results_trf_Dec7 <- entityrecognition_results_trf_Dec7 %>%
  mutate(event = na_if(event, "NA"), norp = na_if(norp, "NA"))

# Save Cleaned NER Results
saveRDS(entityrecognition_results_trf_Dec7, "entityrecognition_results_trf_Dec7.rds")

# Final Dataset Preparation
# Join cleaned NER results with main dataset and preprocessed texts
ALL_Ukraine_subset_NER_Dec7 <- left_join(ALL_Ukraine_subset_TRANS, entityrecognition_results_trf_Dec7) %>%
  left_join(preprocessed_short)

# Save the Final NER Dataset
saveRDS(ALL_Ukraine_subset_NER_Dec7, "ALL_Ukraine_subset_NER_Dec7.rds")

# ----------------------------------------------------------------------------------
## NER Results Analysis with DFM
# ----------------------------------------------------------------------------------

# NER EVENTS Data
# ----------------------------------------------------------------------------------
ALL_Ukraine_subset_NER_Dec7_event <- ALL_Ukraine_subset_NER_Dec7[c("new_id", "created_at", "country", "username", "event")]

# Standardizing event names by replacing spaces with underscores for consistency
ALL_Ukraine_subset_NER_Dec7_event$event <- gsub(" ", "_", ALL_Ukraine_subset_NER_Dec7_event$event)
# Correcting semicolon-space separation for event names
ALL_Ukraine_subset_NER_Dec7_event$event <- gsub(";_", " ", ALL_Ukraine_subset_NER_Dec7_event$event)

# Renaming 'event' column to 'text' for analysis
ALL_Ukraine_subset_NER_Dec7_event <- dplyr::rename(ALL_Ukraine_subset_NER_Dec7_event, text = event)

# Creating Corpus and Tokens
corp <- corpus(ALL_Ukraine_subset_NER_Dec7_event)  # Convert to corpus
toks <- tokens(corp)  # Tokenize the corpus

# DFM Analysis for Events Before February 24, 2022
corp_pre22 <- corpus_subset(corp, created_at < "2022-02-24T00:00:00.000Z")

# Germany
dfmat1_pre22 <- dfm(corpus_subset(corp_pre22, country == "Germany"), remove_punct = TRUE) %>% dfm_trim(min_termfreq = 1)
quanteda.textplots::textplot_wordcloud(dfmat1_pre22, min_count = 2, scale=c(2, .5))

# Poland
dfmat2_pre22 <- dfm(corpus_subset(corp_pre22, country == "Poland")) %>% dfm_trim(min_termfreq = 5)
quanteda.textplots::textplot_wordcloud(dfmat2_pre22, scale=c(2, .5))

# Ukraine
dfmat3_pre22 <- dfm(corpus_subset(corp_pre22, country == "Ukraine"), remove_punct = TRUE) %>% dfm_trim(min_termfreq = 8)
quanteda.textplots::textplot_wordcloud(dfmat3_pre22, scale=c(2, .5))

# DFM Analysis for Events After February 24, 2022
corp_post22 <- corpus_subset(corp, created_at > "2022-02-24T00:00:00.000Z")

# Germany
dfmat1_post22 <- dfm(corpus_subset(corp_post22, country == "Germany"),remove_punct = TRUE) %>% dfm_trim(min_termfreq = 1)
quanteda.textplots::textplot_wordcloud(dfmat1_post22, scale=c(2, .5))

# Poland
dfmat2_post22 <- dfm(corpus_subset(corp_post22, country == "Poland"),remove_punct = TRUE) %>%dfm_trim(min_termfreq = 3)
quanteda.textplots::textplot_wordcloud(dfmat2_post22, scale=c(2, .5))

# Ukraine
dfmat3_post22 <- dfm(corpus_subset(corp_post22, country == "Ukraine"), remove_punct = TRUE) %>% dfm_trim(min_termfreq = 4)
quanteda.textplots::textplot_wordcloud(dfmat3_post22, scale=c(2, .5))

# DFM Analysis for the Four Weeks Following February 24, 2022
corp_post22_4weeks <- corpus_subset(corp, created_at > '2022-02-24T00:00:00.000Z' & created_at <= '2022-03-24 00:00:00')

# Germany
dfmat1_post22_4weeks <- dfm(corpus_subset(corp_post22_4weeks, country == "Germany"),remove_punct = TRUE) %>% dfm_trim(min_termfreq = 1)
quanteda.textplots::textplot_wordcloud(dfmat1_post22_4weeks, scale=c(2, .5))

# Poland
dfmat2_post22_4weeks <- dfm(corpus_subset(corp_post22_4weeks, country == "Poland"),remove_punct = TRUE) %>%dfm_trim(min_termfreq = 3)
quanteda.textplots::textplot_wordcloud(dfmat2_post22_4weeks, scale=c(2, .5))

# Ukraine
dfmat3_post22_4weeks <- dfm(corpus_subset(corp_post22_4weeks, country == "Ukraine"), remove_punct = TRUE) %>% dfm_trim(min_termfreq = 4)
quanteda.textplots::textplot_wordcloud(dfmat3_post22_4weeks, scale=c(2, .5))

# NER DATES data
# ----------------------------------------------------------------------------------

ALL_Ukraine_subset_NER_Dec7_date <- ALL_Ukraine_subset_NER_Dec7[c("new_id", "created_at", "country", "username", "date")]

# Extract only the years (four digits)
years <- regmatches(ALL_Ukraine_subset_NER_Dec7_date$date, 
                    gregexpr("\\b\\d{4}\\b", ALL_Ukraine_subset_NER_Dec7_date$date))

# Concatenate all years found in each cell
concatenate_years <- function(years_vector) {
  if (length(years_vector) > 0) {
    return(paste(years_vector, collapse = ", "))
  } else {
    return(NA)
  }
}

# Extract instances of a year followed by a whitespace and a hyphen
year_with_hyphen_and_following <- regmatches(ALL_Ukraine_subset_NER_Dec7_date$date, 
                                             gregexpr("\\b\\d{4}\\s-\\s(\\d{2}|\\d{4})\\b", ALL_Ukraine_subset_NER_Dec7_date$date))

# Create dataframes for results including 'new_id'
df_years <- data.frame(new_id = ALL_Ukraine_subset_NER_Dec7_date$new_id, 
                       years = sapply(years, concatenate_years))

df_year_with_hyphen_and_following <- data.frame(new_id = ALL_Ukraine_subset_NER_Dec7_date$new_id, 
                                                year_with_hyphen_and_following = sapply(year_with_hyphen_and_following, function(x) if(length(x)>0) x[1] else NA))

# Merge df_years with the original dataframe
merged_df <- merge(ALL_Ukraine_subset_NER_Dec7_date, df_years, by = "new_id", all.x = TRUE)

# Merge df_year_with_hyphen_and_following with the merged dataframe
ALL_Ukraine_subset_NER_Dec7_date_extended <- merge(merged_df, df_year_with_hyphen_and_following, by = "new_id", all.x = TRUE)

# Rename the columns
names(ALL_Ukraine_subset_NER_Dec7_date_extended)[names(ALL_Ukraine_subset_NER_Dec7_date_extended) == "year_with_hyphen_and_following"] <- "periods"

# Function to remove years from 'years' column that appear in 'periods' column
remove_overlapping_years <- function(years, periods) {
  if (is.na(years) || is.na(periods)) {
    return(years)
  }
  
  # Split years into a vector
  years_vector <- unlist(strsplit(years, split = ",\\s*"))
  # Split periods into a vector
  periods_vector <- unlist(strsplit(periods, split = ",\\s*"))
  
  # Extract all years from periods
  periods_years <- unlist(sapply(strsplit(periods_vector, split = "\\s-\\s"), function(x) x))
  
  # Remove years that appear in both vectors
  filtered_years <- years_vector[!years_vector %in% periods_years]
  
  # Return the remaining years as a comma-separated string
  if (length(filtered_years) == 0) {
    return(NA)
  } else {
    return(paste(filtered_years, collapse = ", "))
  }
}

# Apply the function to each row of the dataframe
ALL_Ukraine_subset_NER_Dec7_date_extended$years <- mapply(remove_overlapping_years, 
                                                          ALL_Ukraine_subset_NER_Dec7_date_extended$years, 
                                                          ALL_Ukraine_subset_NER_Dec7_date_extended$periods)


# Merge 'years' and 'periods' into a new column 'text'
ALL_Ukraine_subset_NER_Dec7_date_extended$text <- apply(ALL_Ukraine_subset_NER_Dec7_date_extended[, c("years", "periods")], 1, function(x) {
  # Concatenate non-NA and non-empty strings
  text <- paste(na.omit(x[x != ""]), collapse = " ")
  if (text == "") return(NA)  # Return NA if the result is an empty string
  return(text)
})


# Prepare for DFM conversion
# First, remove all whitespaces
ALL_Ukraine_subset_NER_Dec7_date_extended$text <- gsub("\\s+", "", ALL_Ukraine_subset_NER_Dec7_date_extended$text)
# Second, turn all commas into whitespaces
ALL_Ukraine_subset_NER_Dec7_date_extended$text <- gsub(",", " ", ALL_Ukraine_subset_NER_Dec7_date_extended$text)

##
saveRDS(ALL_Ukraine_subset_NER_Dec7_date_extended, "ALL_Ukraine_subset_NER_Dec7_date_extended.rds")
##

# Convert the extended NER dataset to a corpus
corp <- corpus(ALL_Ukraine_subset_NER_Dec7_date_extended)

# Tokenize the corpus
toks <- tokens(corp)

# Analyzing DFM Before February 24, 2022
corp_pre22 <- corpus_subset(corp, created_at < "2022-02-24T00:00:00.000Z")

# Germany Analysis
dfmat1_pre22 <- dfm(corpus_subset(corp_pre22, country == "Germany"),
                    remove = c(stopwords("english")), 
                    remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 1)
quanteda.textplots::textplot_wordcloud(dfmat1_pre22, scale=c(2, .5))
freq <- textstat_frequency(dfmat1_pre22)
sum(freq$frequency)

# Poland Analysis
dfmat2_pre22 <- dfm(corpus_subset(corp_pre22, country == "Poland"),
                    remove = c(stopwords("english")), 
                    remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 2)
quanteda.textplots::textplot_wordcloud(dfmat2_pre22, scale=c(2, .5))
freq <- textstat_frequency(dfmat2_pre22)
sum(freq$frequency)

# Ukraine Analysis
dfmat3_pre22 <- dfm(corpus_subset(corp_pre22, country == "Ukraine"),
                    remove = c(stopwords("english")), 
                    remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 1)
quanteda.textplots::textplot_wordcloud(dfmat3_pre22, scale=c(2, .5))
freq <- textstat_frequency(dfmat3_pre22)
sum(freq$frequency)

# Analyzing DFM After February 24, 2022
corp_post22 <- corpus_subset(corp, created_at > "2022-02-24T00:00:00.000Z")

# Germany Analysis
dfmat1_post22 <- dfm(corpus_subset(corp_post22, country == "Germany"),
                     remove = c(stopwords("english")), 
                     remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 1)
quanteda.textplots::textplot_wordcloud(dfmat1_post22, scale=c(2, .5))
freq <- textstat_frequency(dfmat1_post22)
sum(freq$frequency)

# Poland Analysis
dfmat2_post22 <- dfm(corpus_subset(corp_post22, country == "Poland"),
                     remove = c(stopwords("english")), 
                     remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 1)
quanteda.textplots::textplot_wordcloud(dfmat2_post22, scale=c(2, .5))
freq <- textstat_frequency(dfmat2_post22)
sum(freq$frequency)

# Ukraine Analysis
dfmat3_post22 <- dfm(corpus_subset(corp_post22, country == "Ukraine"),
                     remove = c(stopwords("english")), 
                     remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 2)
quanteda.textplots::textplot_wordcloud(dfmat3_post22, scale=c(2, .5))
freq <- textstat_frequency(dfmat3_post22)
sum(freq$frequency)


# ----------------------------------------------------------------------------------
# Validating the NER model -----
# ----------------------------------------------------------------------------------

# First, validating the dates extraction. 

val_date_not_preprocessed <- ALL_Ukraine_subset_TRANS %>%
  select(new_id, text)

val_date_not_preprocessed <- val_date_not_preprocessed %>%
  mutate(extracted_dates = str_extract_all(text, pattern = "\\b((?!2222|2800|1520)(1[3-9]\\d{2}|2(?!88)\\d{3}|19[0-9][^09]\\d|20[01][^288][^0]\\d)(?: ?- ?(?:\\d{2}|(1[3-9]\\d{2}|2(?!88)\\d{3}|19[0-9][^09]\\d|20[01][^288][^0]\\d)))?)\\b") %>%
           sapply(function(dates) paste(unique(dates), collapse = " ")))

# now, I am joining the extracted dates with the results from the NER model
val_date_not_preprocessed1 <- val_date_not_preprocessed %>%
  left_join(select(ALL_Ukraine_subset_NER_Dec7_date_extended, new_id, text), by = "new_id")

# Filter out rows where either 'text' or 'extracted_dates' is NA
val_date_not_preprocessed1_filtered <- val_date_not_preprocessed1 %>%
  filter(!is.na(text.y) & !is.na(extracted_dates))

# Calculate the number of identical values
identical_count <- sum(val_date_not_preprocessed1_filtered$text.y == val_date_not_preprocessed1_filtered$extracted_dates)
# Calculate the total number of rows for comparison
total_rows <- nrow(val_date_not_preprocessed1_filtered)
# Compute the percentage of identical values
percentage_identical <- (identical_count / total_rows) * 100
# Print the percentage
percentage_identical

# computing F1 for "events" extraction ----

# Randomly select 200 observations
random_sample_ALL_Ukraine_subset_TRANS <- ALL_Ukraine_subset_TRANS %>% sample_n(200)

val_event <- random_sample_ALL_Ukraine_subset_TRANS %>%
  select(new_id, text)

write_xlsx(val_event, "val_event_manual_coding.xlsx")
val_event_coded <- readxl::read_xlsx("val_event_manual_coding_Feb20_2024.xlsx")

# Select only 'new_id' and 'text' from ALL_Ukraine_subset_NER_Dec7_event
ner_events_text <- select(ALL_Ukraine_subset_NER_Dec7_event, new_id, text)
# Perform a left join to merge the text into val_event_coded
val_event_with_text <- left_join(val_event_coded, ner_events_text, by = "new_id")

# Begin computing the validation scores
# Initialize counters
TP <- 0
FP <- 0
FN <- 0

# Loop through each row of the dataframe
for (i in 1:nrow(val_event_with_text)) {
  # Skip the iteration if either value is NA
  if (is.na(val_event_with_text$text.y[i]) || is.na(val_event_with_text$manual_event[i])) {
    next
  }
  
  # Check for True Positive
  if (val_event_with_text$text.y[i] != "" && val_event_with_text$text.y[i] == val_event_with_text$manual_event[i]) {
    TP <- TP + 1
  }
  # Check for False Positive
  else if (val_event_with_text$text.y[i] != "" && val_event_with_text$text.y[i] != val_event_with_text$manual_event[i]) {
    FP <- FP + 1
  }
  # Check for False Negative
  if (val_event_with_text$manual_event[i] != "" && val_event_with_text$manual_event[i] != val_event_with_text$text.y[i]) {
    FN <- FN + 1
  }
}

# Calculate Precision and Recall
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

# Print the results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

# Calculate F1 Score
F1_score <- 2 * ((precision * recall) / (precision + recall))

# Print the F1 score
cat("F1 Score:", F1_score, "\n")

##

# Follower Networks Analysis
# ----------------------------------------------------------------------------------

# Set the directory path containing the follower data files
path <- "x"

# Retrieve all .rds file names within the specified path
rds_files <- list.files(path, pattern = "*.rds", full.names = TRUE)

# Initialize an empty dataframe to combine all follower data
combined_df <- data.frame()

# Combine follower data from multiple .rds files into one dataframe
for (file in rds_files) {
  temp_df <- readRDS(file)  # Load each .rds file
  combined_df <- bind_rows(combined_df, temp_df)  # Append to the combined dataframe
}

# Standardize column names for consistency
combined_df <- dplyr::rename(combined_df, author_id = from_id, target_name = name, 
                             target_username = username, target_id = id)

# Merge follower data with unique memory organization data to include usernames
ALL_memoryorg_final_unique_short <- ALL_memoryorg_final_unique[, c("author_id", "username", "name")]
combined_df <- merge(combined_df, ALL_memoryorg_final_unique_short, by = "author_id", all.x = TRUE)

# Filter out follower connections that don't link back to the original set of memory organizations
subset_combined_df <- subset(combined_df, target_id %in% author_id)

# Identify and print usernames missing in the final subset to check for lost data
missing_usernames <- setdiff(ALL_memoryorg_final_unique_short$username, subset_combined_df$username)
print(missing_usernames)

missing_author_ids <- setdiff(ALL_memoryorg_final_unique_short$author_id, community_df_merged$author_id)
# Print the missing author_id values
print(missing_author_ids)

# Create an edge list from the subset data frame
edge_list <- subset_combined_df[, c("author_id", "target_id", "username")]

# Construct a directed graph from the edge list
g_following <- graph_from_data_frame(edge_list, directed = TRUE)

# Define a list of node IDs to remove from the graph
nodes_to_remove <- c("1055472233261490176", "106930827", "112767813", "1484443334592585729",
                     "16454127", "1704755826", "19633066", "22027992", "31458706", "338921863",
                     "383825182", "40915723", "41596136", "43677757", "81217087", "862603906840502272",
                     "988487908347826176", "767918491", "841603276835586048", "397289808", "196551396",
                     "973921441824624642", "2372014495", "1179292298", "465904889", "242358927",
                     "3917211929", "2166314695", "15514241", "258724078", "39547629", "816400763639496705",
                     "903195034291462144", "1703972024")

# Remove specified nodes from the graph
g_following <- delete.vertices(g_following, nodes_to_remove)

# Convert the directed graph to an undirected graph for community detection
g_following_undirected <- as.undirected(g_following, mode = "collapse")

# Detect communities within the undirected graph
communities_following <- cluster_louvain(g_following_undirected)

# Assign community membership back to the original directed graph's vertices
V(g_following)$community <- communities_following$membership

# Match vertex names in the graph with 'author_id' from memory organizations data
matched_labels1 <- ALL_memoryorg_final_unique_short$username[match(V(g_following)$name, ALL_memoryorg_final_unique_short$author_id)]

# Assign matched labels as vertex labels in the graph
V(g_following)$label <- matched_labels1

# Export the graph to a GraphML file including community information
write.graph(g_following, file="following_network.graphml", format="graphml")

# Validating the follower network:
# checking all the outgoing ties in the follower network

outgoing_ties_df <- combined_df %>%
  dplyr::group_by(target_id) %>%
  dplyr::summarise(
    count = n(),  # Count the number of occurrences
    target_username = first(target_username),  # Get the first occurrence of target_username
    target_name = first(target_name),  # Get the first occurrence of target_name
    description = first(description)  # Get the first occurrence of description
  )

mean(outgoing_ties_df$count)
median(outgoing_ties_df$count)
summary(outgoing_ties_df$count)

outgoing_ties_df <- arrange(outgoing_ties_df, desc(count))
outgoing_ties_df$number <- 1:nrow(outgoing_ties_df_short)

outgoing_ties_df_short <- outgoing_ties_df[, c("number", "target_name", "description", "count")]
outgoing_ties_df_short[1:20, 1:4] %>%
  kbl(caption = "20 Most Followed Organizations in the Broader Network") %>%
  kable_classic(full_width = F, html_font = "Cambria")  %>%
  save_kable(file = "Most_Followed_Organizations_Broader_Network.html", self_contained = T)

outgoing_ties_shared_df <- inner_join(outgoing_ties_df, ALL_memoryorg_final_unique_short, by = c("target_username" = "username")) %>%
  select(target_username, count)

# Compute deciles for outgoing_ties_df
deciles_outgoing_ties <- quantile(outgoing_ties_df$count, probs = seq(0, 1, 0.1))
# Compute deciles for outgoing_ties_shared_df
deciles_outgoing_ties_shared <- quantile(outgoing_ties_shared_df$count, probs = seq(0, 1, 0.1))

print(deciles_outgoing_ties)
print(deciles_outgoing_ties_shared)

# Mention Networks
# ----------------------------------------------------------------------------------
# Focus on Ukrainian organizations and overall mention dynamics

# Define Ukrainian organizations for mention analysis
ukrainian_organizations <- c("@babynyarhmc", "@HolodomorMuseum", "@TerritoryTerror", "@Uinp_gov_ua")

# Filter tweets mentioning Ukrainian organizations
mentions_ukrainian_organizations <- ALL_memoryorg_final[grep(paste(ukrainian_organizations, collapse = "|"), ALL_memoryorg_final$mentions), ]

# Analyze the diversity of mentions for Ukrainian organizations
sapply(mentions_ukrainian_organizations, function(x) length(unique(x)))

# Identify unique organizations mentioning Ukrainian ones
unique(mentions_ukrainian_organizations$name)

# Tweet activity analysis by country
table(ALL_memoryorg_final$country)

# Organization count by country
table(ALL_memoryorg_final_unique$country)

# Prepare mention data for network analysis
mentions_full <- ALL_memoryorg_final[, c("unique_tweet_id", "username", "mentions")]
mentions_full$mentions <- gsub("@", "", mentions_full$mentions)  # Clean mentions field

# Retain only mentions that match with existing usernames
mentions_full <- mentions_full[mentions_full$mentions %in% mentions_full$username, ]

# Expand mentions to individual rows for network edge creation
mentions_full <- mentions_full %>%
  separate_rows(mentions, sep = ",") %>%
  mutate(mentions = trimws(mentions))

# Count mentions to prepare for edge list creation
mentions_full <- mentions_full %>%
  group_by(username, mentions) %>%
  count() %>%
  ungroup()

# Save the prepared mentions data
saveRDS(mentions_full, "mentions_full.rds")

# Construct edge list from mentions, excluding self-mentions and duplicates
edge_list1 <- mentions_full[, c("username", "mentions", "n")]
edge_list1 <- edge_list1[edge_list1$username != edge_list1$mentions, ]
edge_list1 <- edge_list1[!duplicated(edge_list1), ]

# Analyze unique usernames involved in mentions
count(unique(edge_list1$username))

# Community detection and graph export

# Assign community membership to vertices in graph g1
V(g1)$community <- communities1$membership

# Match vertex names in g1 with labels in mentions_network_nodes for labeling
matched_labels <- mentions_network_nodes$label[match(V(g1)$name, mentions_network_nodes$label)]
V(g1)$label <- matched_labels

# Export the mentions network graph to a GraphML file
write.graph(g1, file="mentions_network.graphml", format="graphml")

# Identify nodes not included in the graph g1
graph_nodes <- V(g1)$name
dropped_nodes <- setdiff(mentions_network_nodes$label, graph_nodes)
print(dropped_nodes)  # Print nodes that were dropped from the graph

# Graph construction with weights and community detection

# Prepare nodes data frame with unique labels and IDs
mentions_network_nodes <- unique(data.frame(label = ALL_memoryorg_final_unique_short$username))
mentions_network_nodes$id <- seq_len(nrow(mentions_network_nodes))

# Construct a weighted directed graph from edge list
g_weights <- graph_from_data_frame(edge_list1, directed = TRUE)
E(g_weights)$weight <- edge_list1$n  # Assign weights from edge list

# Convert to undirected graph for community detection
g_weights_undirected <- as.undirected(g_weights, mode = "collapse")
communities_weights <- cluster_louvain(g_weights_undirected)  # Detect communities

# Assign community memberships to the original directed graph
V(g_weights)$community <- communities_weights$membership

# Match vertex names in g_weights with labels for vertex labeling
matched_labels <- mentions_network_nodes$label[match(V(g_weights)$name, mentions_network_nodes$label)]
V(g_weights)$label <- matched_labels

# Export the weighted mentions network graph with community information
write.graph(g_weights, file="mentions_network_weights.graphml", format="graphml")


# Network of mentions only after February 24, 2022 ----
# ----------------------------------------------------------------------------------

# Select relevant columns
mentions_full_afterFeb2022 <- subset_post_Feb24[, c("unique_tweet_id", "username", "mentions")]
# Remove "@" from "mentions" variable
mentions_full_afterFeb2022$mentions <- gsub("@", "", mentions_full_afterFeb2022$mentions)
# Separate "mentions" into multiple rows
mentions_full_afterFeb2022 <- mentions_full_afterFeb2022 %>%
  separate_rows(mentions, sep = ",") %>%
  mutate(mentions = trimws(mentions))  # Remove leading and trailing whitespaces
# Filter out entries in "mentions" that do not appear in "username"
mentions_full_afterFeb2022 <- mentions_full_afterFeb2022[mentions_full_afterFeb2022$mentions %in% mentions_full_afterFeb2022$username, ]
# Count the number of times each combination of "username" and "mentions" appears
mentions_full_afterFeb2022 <- mentions_full_afterFeb2022 %>%
  group_by(username, mentions) %>%
  dplyr::count() %>%
  ungroup()

#saveRDS(mentions_full_afterFeb2022, "mentions_full_afterFeb2022.rds")
mentions_full_afterFeb2022 <- readRDS("mentions_full_afterFeb2022.rds")

# Create an edge list from the dataframe
edge_list2 <- mentions_full_afterFeb2022[, c("username", "mentions", "n")]
# Remove rows where 'username' and 'mentions' are identical
edge_list2 <- edge_list2[edge_list2$username != edge_list2$mentions, ]
edge_list2 <- edge_list2[!duplicated(edge_list2), ]
#any(is.na(edge_list2$mentions))
count(unique(edge_list2$username))

# Create mentions_network_nodes dataframe
mentions_full_afterFeb2022_network_nodes <- unique(data.frame(label = ALL_memoryorg_final_unique_short$username))
mentions_full_afterFeb2022_network_nodes$id <- seq_len(nrow(mentions_full_afterFeb2022_network_nodes))

# Create a directed graph with weights
g2_weights <- graph_from_data_frame(edge_list2, directed = TRUE)
E(g2_weights)$weight <- edge_list2$n
# Create a temporary undirected version for community detection
g2_weights_undirected <- as.undirected(g2_weights, mode = "collapse")
communities_weights2 <- cluster_louvain(g2_weights_undirected)
# Assign the community memberships back to the original directed graph
V(g2_weights)$community <- communities_weights2$membership
# Match the vertex names of g_weights with the id column of mentions_full_afterFeb2022_network_nodes
matched_labels2 <- mentions_full_afterFeb2022_network_nodes$label[match(V(g2_weights)$name, mentions_full_afterFeb2022_network_nodes$label)]
V(g2_weights)$label <- matched_labels2
# Export the directed graph with community information
write.graph(g2_weights, file="mentions_network_weights_afterFeb2022.graphml", format="graphml")

# Network of mentions only before February 24, 2022 ----
# ----------------------------------------------------------------------------------

# Select relevant columns
mentions_full_beforeFeb2022 <- subset_pre_Feb24[, c("unique_tweet_id", "username", "mentions")]
# Remove "@" from "mentions" variable
mentions_full_beforeFeb2022$mentions <- gsub("@", "", mentions_full_beforeFeb2022$mentions)
# Separate "mentions" into multiple rows
mentions_full_beforeFeb2022 <- mentions_full_beforeFeb2022 %>%
  separate_rows(mentions, sep = ",") %>%
  mutate(mentions = trimws(mentions))  # Remove leading and trailing whitespaces
# Filter out entries in "mentions" that do not appear in "username"
mentions_full_beforeFeb2022 <- mentions_full_beforeFeb2022[mentions_full_beforeFeb2022$mentions %in% mentions_full_beforeFeb2022$username, ]
# Count the number of times each combination of "username" and "mentions" appears
mentions_full_beforeFeb2022 <- mentions_full_beforeFeb2022 %>%
  group_by(username, mentions) %>%
  dplyr::count() %>%
  ungroup()

# Create an edge list from the dataframe
edge_list3 <- mentions_full_beforeFeb2022[, c("username", "mentions", "n")]
# Remove rows where 'username' and 'mentions' are identical
edge_list3 <- edge_list3[edge_list3$username != edge_list3$mentions, ]
edge_list3 <- edge_list3[!duplicated(edge_list3), ]
#any(is.na(edge_list3$mentions))
count(unique(edge_list3$username))

# Create mentions_network_nodes dataframe
mentions_full_beforeFeb2022_network_nodes <- unique(data.frame(label = ALL_memoryorg_final_unique_short$username))
mentions_full_beforeFeb2022_network_nodes$id <- seq_len(nrow(mentions_full_beforeFeb2022_network_nodes))

# Create a directed graph with weights
g3_weights <- graph_from_data_frame(edge_list3, directed = TRUE)
E(g3_weights)$weight <- edge_list3$n
# Create a temporary undirected version for community detection
g3_weights_undirected <- as.undirected(g3_weights, mode = "collapse")
communities_weights3 <- cluster_louvain(g3_weights_undirected)
# Assign the community memberships back to the original directed graph
V(g3_weights)$community <- communities_weights3$membership
# Match the vertex names of g_weights with the id column of mentions_full_beforeFeb2022_network_nodes
matched_labels3 <- mentions_full_beforeFeb2022_network_nodes$label[match(V(g3_weights)$name, mentions_full_beforeFeb2022_network_nodes$label)]
V(g3_weights)$label <- matched_labels3
# Export the directed graph with community information
write.graph(g3_weights, file="mentions_network_weights_beforeFeb2022.graphml", format="graphml")

# Additional Qualitative Analysis of Mentions Network Data
# ----------------------------------------------------------------------------------

# Define Ukrainian organizations for analysis
ukrainian_organizations <- c("@babynyarhmc", "@HolodomorMuseum", "@TerritoryTerror", "@Uinp_gov_ua")

# Create regex pattern from Ukrainian organizations for mention filtering
pattern <- paste(ukrainian_organizations, collapse = "|")

# Analyze mentions of Ukrainian organizations in the post-Feb 24, 2022 dataset
filtered_df <- subset_post_Feb24[grepl(pattern, subset_post_Feb24$mentions), ]
# Examine country distribution of organizations mentioning Ukrainian ones post-Feb 24, 2022
table(filtered_df$country)

# Similar analysis for the period before Feb 24, 2022
filtered_df_pre2022 <- subset_pre_Feb24[grepl(pattern, subset_pre_Feb24$mentions), ]
# Compare country distribution before and after Feb 24, 2022
table(filtered_df_pre2022$country)
