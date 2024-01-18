#################################################################################################
##################################### DATA SCRAPING #############################################
#################################################################################################

rm(list = ls())

cat("Hello! The following code helps you to scrape the data used for this project.", "\n",
    "In case you already have the data with you, Please proceed to the 'LOADING' section.")

# Load the necessary libraries
if (!require("rvest")) {
  install.packages("rvest")
}
library(rvest)

if (!require("XML")) {
  install.packages("XML")
}
library(XML)

# Defining the path to the CSV file containing movie names and URLs
csv_file_path <- "C:/Teja/MSBA/Semester 3/Project Capstone/Project/Movies_List.csv"

# Defining the local folder path where you want to save the downloaded content
folder_path <- "C:/Teja/MSBA/Semester 3/Project Capstone/Project/Topic_Modeling_Data/"

# Reading the CSV file into a data frame
movies_data <- read.csv(csv_file_path, header = TRUE, stringsAsFactors = FALSE)

# Creating a loop to download content for each URL from the CSV file and clean the HTML
for (i in 1:nrow(movies_data)) {
  # Get the URL and name from the CSV data frame
  url <- movies_data[i, 2]  # Use the second column (URL)
  name <- movies_data[i, 1]  # Use the first column (Name)
  
  # Defining the local file path and add .txt extension
  filename <- paste0(name, ".txt")
  file_path <- file.path(folder_path, filename)
  
  # Downloading the contents from the URL
  download.file(url, destfile = file_path, mode = "wb")
  
  # Reading the HTML content from the text file
  html_content <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  
  # Parsing the HTML
  parsed_html <- htmlParse(html_content)
  
  # Extracting the text content
  cleaned_text <- xpathSApply(parsed_html, "//text()", xmlValue)
  
  # Removing JavaScript code
  cleaned_text <- gsub("<script.*?</script>", "", cleaned_text, perl = TRUE)
  
  # Combining the text content into a single string
  cleaned_text <- paste(cleaned_text, collapse = " ")
  
  # Writing the cleaned text back to the file
  writeLines(cleaned_text, file(file_path))
  
  # Printing a message
  cat("Downloaded and cleaned:", filename, "\n")
}

##################################################################################################
######################### LOADING DATA  ##########################################################
##################################################################################################

rm(list=ls()) 

# Load the required libraries

if (!require("tidytext")) {install.packages("tidytext")}

library(tidytext)

if (!require("topicmodels")) {install.packages("topicmodels")}

library(topicmodels)

if (!require("tm")) {install.packages("tm")}

library(tm)

# Loading the input directory 
input_dir <- "C:/Teja/MSBA/Semester 3/Project Capstone/Project/Topic_Modeling_Data" 
files_v <- dir(path = input_dir, pattern = ".*txt")

#Obtaining access to custom functions in corpus
source("C:/Teja/MSBA/Semester 3/Project Capstone/Downloads/corpus_functions_2.R")

# Creating an empty df
documents_df <- NULL

# Loading and Pre-processing the files into the df
for (i in seq_along(files_v)) {
  file_path <- file.path(input_dir, files_v[i])
  text_doc <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  
  # Joining the lines of the document into a single string
  document_text <- tolower(paste(text_doc, collapse = " "))
  
  # Creating a data frame for the document
  file_df <- data.frame(
    id = gsub("\\..*", "", files_v[i]),
    text = document_text,
    stringsAsFactors = FALSE
  )
  
  documents_df <- rbind(documents_df, file_df)
  
  cat(
    "Done Processing", 
    files_v[i], 
    "which is file", 
    i,
    "of", 
    length(files_v), 
    "\n" 
  )
}

# Creating a Corpus from the text data for further processing
corpus <- Corpus(VectorSource(documents_df$text))

#################################################################################################
######################## TEXT PRE-PROCESSING #################################################### 
#################################################################################################

# Setting  paths for stoplists
stoplist_path <- "C:/Teja/MSBA/Semester 3/Project Capstone/Project/Processing/stoplists"
characternames_stoplist <- read.csv(file.path(stoplist_path, "characternames.csv"), header = FALSE)$V1
stoplist <- read.csv(file.path(stoplist_path, "stoplist.csv"), header = FALSE)$V1
scriptkeywords_stoplist <- read.csv(file.path(stoplist_path, "scriptkeywords.csv"), header = FALSE)$V1

# Step 1: Converting the corpus text into lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Step 2: Replacing ('s) with blankspaces
corpus <- tm_map(corpus, content_transformer(function(x) gsub("'s", "", x)))

# Step 3: Removing stopwords from "characternames.csv"
# Split the characternames_stoplist into chunks
char_chunk_size <- 100
char_chunks <- split(characternames_stoplist, ceiling(seq_along(characternames_stoplist) / char_chunk_size))

# Removing stopwords from each chunk
corpus <- tm_map(corpus, content_transformer(function(x) {
  cat("Removing character names from the corpus may take upto 5 minutes")
  for (chunk in char_chunks) {
    x <- removeWords(x, chunk)    }
  return(x)} )
)

# Step 4: Removing stopwords from "stoplist.csv"
corpus <- tm_map(corpus, content_transformer(function(x) removeWords(x, stoplist)))

# Step 5: Removing stopwords from "scriptkeywords.csv"
corpus <- tm_map(corpus, content_transformer(function(x) removeWords(x, scriptkeywords_stoplist)))

# Step 6: Removing punctuations
corpus <- tm_map(corpus, content_transformer(function(x) removePunctuation(x)))

# Step 7: Removing default English stopwords
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Step 8: Removing numbers
corpus <- tm_map(corpus, removeNumbers)

# Loading the custom stoplist
custom_stoplist_path <- file.path(stoplist_path, "customstoplist.csv")
custom_stoplist <- read.csv(custom_stoplist_path, header = FALSE, stringsAsFactors = FALSE)
custom_stopwords <- tolower(custom_stoplist$V1)

# Removing custom stop words from the corpus
corpus <- tm_map(corpus, content_transformer(function(x) removeWords(x, custom_stopwords)))

# Converting the Corpus back to a data frame
documents_df$text <- sapply(corpus, as.character)


#################################################################################################
############################# TOPIC MODELLING ###################################################
################################################################################################

# Creating a document-term matrix with symbol removal and no repetitive words
dtm <- DocumentTermMatrix(
  Corpus(VectorSource(documents_df$text)),
  control = list(
    tolower = FALSE,  # Prevent lowercase conversion
    removeNumbers = TRUE,  # Keep numbers
    removePunctuation = TRUE,  # Remove punctuation, including parentheses
    wordLengths = c(3, Inf),  # Remove short words (e.g., less than 3 characters)
    bounds = list(global = c(2, Inf))  # Ensure that words occur in at least 2 documents
  )
)

############# FINDING OPTIMAL NUMBER OF TOPICS #################################################

if (!require("ldatuning")) install.packages("ldatuning", dependencies=TRUE)
install.packages("ldatuning")

library(ldatuning)

# Converting DTM to matrix
mat <- as.matrix(dtm)

# Creating a function to calculate coherence for a given number of topics
calculate_coherence <- function(num_topics, mat) {
  lda_model <- LDA(mat, k = num_topics, control = list(seed = 1234))
  topics <- as.matrix(topics(lda_model))
  coherence_score <- topicmodels::coherence(topics, mat, method = "CaoJuan2009")
  return(coherence_score)
}

# Finding the optimal number of topics using ldatuning
  tune_result <- FindTopicsNumber(
  dtm = mat,
  topics = seq(5, 20, by = 1),
  metrics = c("CaoJuan2009"),
  method = "Gibbs",
  control = list(seed = 1234)) #LDA Tuning may take a few minutes...

  
# Loading the necessary library
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(ggplot2)

# Creating a data frame from the results
tune_results_df <- as.data.frame(tune_result)

# Plotting the results with ggplot2
plot <- ggplot(tune_results_df, aes(x = topics, y = CaoJuan2009)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Topics", y = "Coherence", title = "Optimal Number of Topics") +
  theme_minimal()

# Displaying the plot
print(plot)

########################## CREATING AN LDA MODEL ################################################

num_topics <- 5  

# Creating a topic model
lda_model <- LDA(
  dtm,
  k = num_topics,
  control = list(seed = 999)  # Creating an LDA model may take a minute...
  )


# Converting the LDA model to a data frame for 'tidytext'
lda_df <- tidy(lda_model)

# Displaying the top terms for each topic
if (!require("dplyr")) {
  install.packages("dplyr")
}

library(dplyr)


if (!require("knitr")) {
  install.packages("knitr")
}

library(knitr)

# Displaying the top terms for each topic
num_terms <- 30

top_terms <- lda_df %>%
  group_by(topic) %>%
  top_n(num_terms, beta) %>%
  arrange(topic, -beta)

# Creating a data frame for top terms
top_terms_df <- as.data.frame(matrix(nrow = num_terms, ncol = num_topics))
colnames(top_terms_df) <- paste("Topic", 1:num_topics)

# Populating the data frame with top terms
for (i in 1:num_topics) {
  top_terms_df[, i] <- top_terms$term[top_terms$topic == i]
}


if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}

library(writexl)

# Saving the data frame to an Excel workbook
output_excel_file <- "C:/Teja/MSBA/Semester 3/Project Capstone/Project/Output/lda_results.xlsx"
write_xlsx(top_terms_df, path = output_excel_file)

cat("Topic Modeling results have been saved to", output_excel_file, "\n")

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

library(readxl)

# Reading the Excel file into a data frame
lda_results <- read_xlsx(output_excel_file)

# Printing the data frame
print(lda_results, n=10)

#################################################################################################
################################ WORD CLOUD #####################################################
#################################################################################################

if (!require("wordcloud")) install.packages("wordcloud", dependencies=TRUE)

library(wordcloud)


# Creating a function to generate word cloud for a specific topic
generate_wordcloud <- function(topic_number, lda_df) {
  # Extracting terms for the selected topic
  topic_terms <- lda_df %>%
    filter(topic == topic_number) %>%
    top_n(50, beta) %>%
    arrange(-beta) %>%
    select(term, beta)
  
  # Creating a word cloud
  wordcloud(
    words = topic_terms$term, 
    freq = topic_terms$beta, 
    scale = c(3, 0.5), 
    min.freq = 1, 
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
  text(x = 0.5, y = 0.9, paste("Word Cloud for Topic", topic_number), cex = 2, col = "black", font = 2)
}


#Generating word cloud for topic 1
generate_wordcloud(5, lda_df)


#################################################################################################
################################ GROUPED BAR PLOT ###############################################
#################################################################################################

# Bar Plot of Top 30 Words Overall
top_words_overall <- lda_df %>%
  top_n(30, beta) %>%
  arrange(-beta)

# Creating the bar plot
ggplot(top_words_overall, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  labs(title = "Top 30 Words Across All Documents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability


#################################################################################################
################################ TOPIC WISE DOCUMENT PROPORTIONS ################################
#################################################################################################

# Creating a function to plot document proportion for a given topic with fill specification
plot_document_proportion <- function(topic_number, color) {
  # Extracting document-topic proportions from the LDA model
  document_topic_proportions <- as.data.frame(lda_model@gamma)
  
  # Renaming columns for better readability
  colnames(document_topic_proportions) <- paste("Topic", 1:num_topics, sep = "_")
  
  # Adding document names as a new column
  document_topic_proportions$Document <- rownames(document_topic_proportions)
  
  # Reshaping data for plotting (using pivot_longer instead of gather)
  document_topic_long <- tidyr::pivot_longer(
    document_topic_proportions,
    cols = starts_with("Topic"),
    names_to = "Topic",
    values_to = "Proportion"
  )
  
  # Subsetting data for the specified topic
  topic_subset <- document_topic_long[document_topic_long$Topic == paste("Topic", topic_number, sep = "_"), ]
  
  # Reordering documents by proportion in descending order
  topic_subset <- topic_subset[order(-topic_subset$Proportion), ]
  
  # Creating a bar plot with specified fill color
  bar_plot <- ggplot(topic_subset, aes(x = reorder(Document, -Proportion), y = Proportion, fill = factor(Topic))) +
    geom_bar(stat = "identity", fill = color) +  # Specify the fill color parameter here
    labs(x = "Document", y = "Proportion", title = paste("Document Proportion for Topic", topic_number)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Displaying the plot
  print(bar_plot)
}

#Generating Plots for each topic

plot_document_proportion(1, "lightcoral")

plot_document_proportion(2, "green")

plot_document_proportion(3, "seagreen")

plot_document_proportion(4, "royalblue")

plot_document_proportion(5, "violet")


#################################################################################################
################################ DOCUMENT WISE STACKED BAR PLOT #################################
#################################################################################################

# Creating a function to plot a stacked bar plot for each document
plot_stacked_bar <- function() {
  # Extracting document-topic proportions from the LDA model
  document_topic_proportions <- as.data.frame(lda_model@gamma)
  
  # Renaming columns for better readability
  colnames(document_topic_proportions) <- paste("Topic", 1:num_topics, sep = "_")
  
  # Adding document names as a new column
  document_topic_proportions$Document <- rownames(document_topic_proportions)
  
  # Reshaping data for plotting (using pivot_longer)
  document_topic_long <- tidyr::pivot_longer(
    document_topic_proportions,
    cols = starts_with("Topic"),
    names_to = "Topic",
    values_to = "Proportion"
  )
  
  # Reordering documents by the sum of proportions in descending order
  document_topic_long <- document_topic_long[order(-ave(document_topic_long$Proportion, document_topic_long$Document, FUN = sum)), ]
  
  # Creating a stacked bar plot
  stacked_bar_plot <- ggplot(document_topic_long, aes(x = Document, y = Proportion, fill = factor(Topic))) +
    geom_bar(stat = "identity") +
    labs(x = "Document", y = "Proportion", title = "Document-wise Topic Composition") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "right") +
    guides(fill = guide_legend(title = "Topic"))
  
  # Displaying the plot
  print(stacked_bar_plot)
}

# Calling the function to plot the stacked
plot_stacked_bar()


#################################################################################################
################################ PIE CHART - TOPIC DISTRIBUTION #################################
#################################################################################################

# Creating a Function to plot a pie chart for the percentage of total documents that deal with each topic
plot_topic_pie_chart <- function() {
  # Extracting document-topic proportions from the LDA model
  document_topic_proportions <- as.data.frame(lda_model@gamma)
  
  # Renaming columns for better readability
  colnames(document_topic_proportions) <- paste("Topic", 1:num_topics, sep = "_")
  
  # Calculating the percentage of documents for each topic
  topic_percentages <- colMeans(document_topic_proportions > 0.5) * 100
  
  # Creating a data frame for plotting
  pie_data <- data.frame(
    Topic = paste("Topic", 1:num_topics, sep = "_"),
    Percentage = topic_percentages
  )
  
  # Sorting data by percentage in descending order
  pie_data <- pie_data[order(-pie_data$Percentage), ]
  
  # Creating a pie chart with percentage labels
  pie_chart <- ggplot(pie_data, aes(x = "", y = Percentage, fill = factor(Topic))) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = paste0(round(Percentage, 2), "%")), position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    labs(title = "Topic Distribution Across Corpus") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right") +
    guides(fill = guide_legend(title = "Topic"))
  
  # Displaying the plot
  print(pie_chart)
}

# Calling the function to plot the pie chart
plot_topic_pie_chart()



#################################################################################################
################################ DENDROGRAM ###############################################
#################################################################################################

# Extracting the document-topic matrix from the LDA model
doc_topic_matrix <- as.data.frame(topics(lda_model))

# Setting document names as row names
row.names(doc_topic_matrix) <- documents_df$id

# Performing hierarchical clustering
dist_matrix <- dist(doc_topic_matrix, method = "euclidean")
hierarchical_cluster <- hclust(dist_matrix, method = "ward.D2")

# Plotting the dendrogram
plot(hierarchical_cluster, main = "Hierarchical Clustering of Scripts Based on Topics")


# Cutting the dendrogram to form clusters
num_clusters <- 5  # You can adjust this based on the visual inspection of the dendrogram
script_clusters <- cutree(hierarchical_cluster, num_clusters)

# Adding the cluster information to the documents_df
documents_df$cluster <- script_clusters

# Displaying the cluster assignment
print(documents_df[, c("id", "cluster")])


##################################################################################################


