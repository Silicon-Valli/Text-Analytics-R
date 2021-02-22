#Import libraries
#install.packages("NLP")
library(readxl)
library(dplyr)
library(tidytext)
library(tm)

#Import Excel 
jobs <- read_excel("Add XLSX Path here",
                   col_types = c("text", "text"))
View(jobs)


#Create Tibble for Description Column
jobs_df <- data_frame(jobs$Description)
jobs_df#View

#unnest Tokens
jobs_tokens <- unnest_tokens(
  jobs_df,
  tokens,
  `jobs$Description`,
  token = "words",
  format = c("text"),
  to_lower = TRUE,
  drop = TRUE,
  collapse = NULL
)
jobs_tokens

#stop words
library(stringr)
data(stop_words)
clean_df <- jobs_tokens %>%
  anti_join(stop_words, c("tokens" = "word"))

clean_df

#word count
text_wordcounts <- clean_df  %>% 
  count(tokens, sort = TRUE)

text_wordcounts 

library(ggplot2)
word_bar <- text_wordcounts %>% 
  filter(n>3)%>%
  mutate(tokens=reorder(tokens,n))%>%
  ggplot(aes(tokens,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
word_bar


####################### ####### ####### ####### ####### ####### ####### 




#install.packages("pdftools")
library(pdftools)



####### Importing all PDF files from the same folder####### 
setwd("ADD PDF Resume here")
nm <- list.files(path="/Users/valentinvoelckel/Desktop/ReportR/pdf")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
Rpdf <- readPDF(control = list(text = "-layout"))
reports <- Corpus(URISource(nm), 
                  readerControl = list(reader = Rpdf))

resume <- pdf_text(nm)%>%
  readr::read_lines()%>%
  str_squish()%>%
  str_replace_all(",","")%>%
  strsplit(split=" ")
  
head(resume)

var_lines <- resume%>%
  unlist()
var_lines

#Create Tibble for resume

resume_df <- data_frame(var_lines)
resume_df#View

#install.packages("extrafont")
library(extrafont)
library(tm)
df <- resume_df
df$var_lines = removeNumbers(df$var_lines)

#unnest Tokens
df <- unnest_tokens(
  df,
  tokens,
  var_lines,
  token = "words",
  format = c("text"),
  to_lower = TRUE,
  drop = TRUE,
  collapse = NULL
)
df


#stop words
data("stop_words")
clean_resume <- df %>%
  anti_join(stop_words, c("tokens" = "word"))

clean_resume


#word count
resume_wordcounts <- clean_resume  %>% 
  count(tokens, sort = TRUE)

resume_wordcounts 


resume_bar <- resume_wordcounts %>% 
  filter(n>2)%>%
  mutate(tokens=reorder(tokens,n))%>%
  ggplot(aes(tokens,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
resume_bar




#########################################################
corpus <- SimpleCorpus(VectorSource(clean_resume))
# And lets see what we have
corpus

# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus[[1]]$content
DTM <- DocumentTermMatrix(corpus)
inspect(DTM)

#wordcloud 
plotWordcloud <- function(DTM){
  m <- as.matrix(t(DTM))
  v <- sort(rowSums(m),decreasing = TRUE)
  d <- data.frame(word = names(v),freq=v)
  # Wordcloud of 200 most frequently used words
  library(wordcloud2)
  set.seed(123)
  wordcloud2(d)}
plotWordcloud(DTM)


#sentimental Analysis
library(tm)
library(SentimentAnalysis)
sent <- analyzeSentiment(DTM, language = "english")

head(sent)
summary(sent$SentimentGI)

#install.packages("syuzhet")
library(syuzhet)
sent2 <- get_nrc_sentiment(clean_resume$tokens)
# Let's look at the corpus as a whole again:
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")
ggplot(sent3, aes(x = emotion, y = count, fill = emotion)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank()) + labs( x = "Emotion", y = "Total Count") + ggtitle("Sentiment of Resume") + theme(plot.title = element_text(hjust=0.5))

###########################################################################

preprocessing <- function(text){
  # Create the toSpace content transformer
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
  # Perform necessary operations to corpus
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, toSpace, "/")
  corpus <- tm_map(corpus, toSpace, "-")
  # Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower)) 
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace) 
  # Remove stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english")) 
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Stem the corpus
  corpus <- tm_map(corpus, stemDocument, "english") 
  # Create document term matrix (dtm)
  dtm <- DocumentTermMatrix(corpus) 
  # Removes terms with sparsity >=  99%
  dtm <- removeSparseTerms(dtm, 0.99)}
  

# Load the job descriptions
text.summary <- clean_df$tokens
# Create document-term matrices for the summaries
dtm.summary <- preprocessing(text.summary)
# Combine respective DTMs into collective matrix
dtm <- c(dtm.summary)
set.seed(1111)

plotWordcloud <- function(dtm){
  m <- as.matrix(t(dtm))
  v <- sort(rowSums(m),decreasing = TRUE)
  d <- data.frame(word = names(v),freq=v)
  # Wordcloud of 200 most frequently used words
  library(wordcloud2)
  set.seed(123)
  wordcloud2(d)}
  
plotWordcloud(dtm.summary)
#####################################################################

######## correlating words #########
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
content_job <- clean_df
content_resume <- clean_resume
content <- bind_rows(content_job %>%
                       mutate(source = "Jobs"),
                     content_resume %>%
                       mutate(source = "Resume"))
content

frequency <- content %>% 
  group_by(source) %>% 
  count(tokens, sort = TRUE) %>% 
  left_join(content %>%
              group_by(source) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency
#install.packages("tidyr")
library(tidyr)
library(dplyr)


frequency <- frequency %>%
  select(source, tokens, freq) %>%
  spread(source, freq) %>%
  arrange(Jobs, Resume)

#visual correlation tokens
library(scales)
ggplot(frequency, aes(Jobs, Resume)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = tokens), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")
#####


