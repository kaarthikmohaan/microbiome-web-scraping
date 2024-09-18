

################################ WEB SCRAPPING #################################


library(rvest)
library(dplyr)
library(stringr)
library(purrr) # for mapping
library(xml2) # for reading HTML

# Define base URL
base_url <- "https://microbiomejournal.biomedcentral.com"

# Function to safely read HTML using xml2 and handle errors gracefully
safe_read_html <- function(url) {
  tryCatch({
    page <- xml2::read_html(url)
    return(page)
  }, error = function(e) {
    return(NULL) # Return NULL on error
  })
}

# Function to extract data from a single article page
extract_article_data <- function(article_link) {
  article_page <- safe_read_html(article_link)
  if(is.null(article_page)) return(tibble(correspondence_author = NA, keywords = NA, correspondance_author_email = NA))
  
  # Extract correspondence author
  correspondence_author <- article_page %>%
    html_nodes("#corresponding-author-list a") %>%
    html_text() %>%
    paste(collapse = ",")
  if (length(correspondence_author) == 0) {
    correspondence_author <- NA
  }
  
  # Extract keywords
  keywords <- article_page %>%
    html_nodes(".c-article-subject-list__subject") %>%
    html_text() %>%
    paste(collapse = ",")
  if (length(keywords) == 0) {
    keywords <- NA
  }
  
  # Extract author emails
  correspondance_author_email <- article_page %>%
    html_nodes("#corresponding-author-list a[href]") %>%
    html_attr("href") %>%
    str_subset("mailto:") %>%
    str_remove_all("mailto:") %>%
    paste(collapse = ",")
  if (length(correspondance_author_email) == 0) {
    correspondance_author_email <- NA
  }
  
  return(tibble(correspondence_author = correspondence_author, keywords = keywords, correspondance_author_email = correspondance_author_email))
}

# Function to get articles data from one page
get_articles_data_from_page <- function(page_number) {
  page_link <- paste0(base_url, "/articles?searchType=journalSearch&sort=PubDate&page=", page_number)
  page <- safe_read_html(page_link)
  if(is.null(page)) return(tibble(Title = NA, Authors = NA, Publish_Date = NA, Abstract = NA, Article_Link = NA, correspondence_author = NA, keywords = NA, correspondance_author_email = NA))
  
  article_links <- page %>%
    html_nodes(".c-listing__title a") %>%
    html_attr("href") %>%
    paste0(base_url, .)
  
  Title <- page %>% html_nodes(".c-listing__title a") %>% html_text()
  Authors <- page %>% html_nodes(".c-listing__authors-list") %>% html_text()
  Publish_Date <- page %>% html_nodes(".c-listing__metadata span+ span") %>% html_text()
  Abstract <- page %>% html_nodes(".c-listing__title+ p") %>% html_text()
  
  # Ensure vectors are the same length
  num_articles <- length(article_links)
  Authors <- ensure_vector_length(Authors, num_articles)
  Publish_Date <- ensure_vector_length(Publish_Date, num_articles)
  Abstract <- ensure_vector_length(Abstract, num_articles)
  
  additional_data <- map_df(article_links, extract_article_data)
  
  data <- tibble(Title = Title, Authors = Authors, Publish_Date = Publish_Date, Abstract = Abstract, Article_Link = article_links)
  bind_cols(data, additional_data)
}

# Ensure vectors are of equal length by filling with NA
ensure_vector_length <- function(vec, desired_length) {
  length(vec) <- desired_length
  vec
}

# Iterating over pages and combining all data
raw_articles_data <- map_df(1:36, get_articles_data_from_page)

print(head(raw_articles_data))


################################ PRE PROCESSING ################################


library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

cleaned_articles_data <- raw_articles_data

if(!is.character(raw_articles_data$keywords)) {
  raw_articles_data$keywords <- as.character(raw_articles_data$keywords)
}

# Clean the Keywords column separately
cleaned_keywords <- sapply(raw_articles_data$keywords, function(k) {
  k <- str_trim(k) # Trim leading and trailing whitespace
  k <- str_replace_all(k, "\\s*,\\s*", ", ") # Ensure consistent comma spacing
  return(k)
})

# Reassign the cleaned keywords back to the dataframe
cleaned_articles_data$keywords <- cleaned_keywords

cleaned_articles_data <- cleaned_articles_data %>%
  mutate(Publish_Date = str_remove(Publish_Date, "Published on: ")) %>%
  mutate(Publish_Date = dmy(Publish_Date), Publish_Date = format(Publish_Date, "%m-%d-%Y")) %>%
  mutate(Title = str_trim(Title), Abstract = str_trim(Abstract)) %>%
  mutate(correspondance_author_email = str_replace_all(correspondance_author_email, "\\s*,\\s*", ", "))

# Replace empty values with NA in the entire dataframe
cleaned_articles_data[cleaned_articles_data == ""] <- NA

# Handling NA values by dropping rows with any NA
cleaned_articles_data <- drop_na(cleaned_articles_data)


################################ Visualization ################################


library(dplyr)
library(ggplot2)
library(lubridate)

# Extract the year from 'Publish_Date' and count publications per year
publications_per_year <- cleaned_articles_data %>%
  mutate(Year = format(as.Date(Publish_Date, "%m-%d-%Y"), "%Y")) %>%
  group_by(Year) %>%
  summarise(Num_Publications = n()) %>%
  ungroup()

# Plotting the trend of publications over time
ggplot(publications_per_year, aes(x = Year, y = Num_Publications)) +
  geom_line(aes(group=1), colour="blue") + 
  geom_point(colour="red") + 
  theme_minimal() + 
  labs(title = "Publications Trend Over Time", x = "Year", y = "Number of Publications")

# Prepare the keywords by splitting and unlisting them
all_keywords <- unlist(strsplit(as.character(cleaned_articles_data$keywords), ",\\s*"))
# Remove empty entries if there are any
all_keywords <- all_keywords[all_keywords != ""]
# Create a data frame of keyword frequencies
keyword_freq <- data.frame(table(all_keywords)) %>%
  rename(Keyword = all_keywords, Frequency = Freq) %>%
  arrange(desc(Frequency)) 

# Select the top n keywords
top_n <- 10
top_keywords <- head(keyword_freq, top_n)

# Plotting the bar chart
ggplot(top_keywords, aes(x = reorder(Keyword, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Keyword") +
  ylab("Frequency") +
  ggtitle("Top Keywords Frequency") +
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

# Prepare the authors by splitting and unlisting them
all_authors <- unlist(strsplit(as.character(cleaned_articles_data$Authors), ",\\s*"))
# Remove empty entries if there are any
all_authors <- all_authors[all_authors != ""]

# Create a data frame of author frequencies
author_freq <- data.frame(table(all_authors)) %>%
  rename(Author = all_authors, Frequency = Freq) %>%
  arrange(desc(Frequency)) # Arrange by frequency

# Select the top n authors
top_n <- 10
top_authors <- head(author_freq, top_n)

# Plotting the bar chart
ggplot(top_authors, aes(x = reorder(Author, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Author") +
  ylab("Number of Publications") +
  ggtitle("Top Authors by Number of Publications") +
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
