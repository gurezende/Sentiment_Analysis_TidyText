library(tidyverse)
library(tidytext)
library(rvest)
library(textdata)

# Scrape text
url <- 'https://tinyurl.com/2rut3j8z'
news <- read_html(url)

# Extract the paragraphs from the page content
text <- tibble(
  news %>% 
    #extract the text
    html_elements('p') %>% 
    html_text()
  ) %>% 
  rename( 'text' = 1) # rename the colum to "text"


# Get Sentiments AFINN
afin <- get_sentiments('afinn')

# Join sentiments with the text
text_sentiments <- text %>% #dataset
  unnest_tokens(input= text, output = word) %>% # tokenize 1 word = 1 token
  inner_join(afin, by='word') %>% #join the sentiments
  count(word, value, sort = TRUE) %>% #count the words by sentiment value
  mutate(score = n * value) %>%  # create score by multiplying score * value
  arrange( desc(score)) # sort

# Plot
text_sentiments %>% 
  ggplot( aes(x= score, y= reorder(word, score),
              fill= score > 0) ) +
  geom_col(show.legend = F) +
  labs( x= 'Sentiment Score',
        y= 'WORD',
        subtitle = 'Negative versus positive sentiments') +
  ggtitle('Sentiment Score by Word in the Text')+
  theme(plot.subtitle =  element_text(color = "gray", face = "italic")) +
  theme_light()
