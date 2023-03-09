library(tidyverse)
library(tidytext)
library(textdata)


##### Functions ##########

# Function for frequency count
text_freq_counter <- function(text){
  "Input a text string and it returns a frequency table clean of stopwords"
  # Transform to tibble
  df_text <- tibble(text)
  
  # Tokenizing the text
  tokens <- df_text %>% 
    unnest_tokens(input = text, #name of the input column
                  output = word) #name of the output column
  
  
  # Removing stopwords and counting frequencies
  freq_count <- tokens %>% 
    anti_join(stop_words) %>% 
    count(word, sort = TRUE)
  
  # Return
  return(freq_count)
  
  
}#close function


# Function for frequency count
text_freq_sentiment <- function(text, .sentiment='afinn'){
  "This function takes in a text string and the sentiments package from tidytext and
  returns a frequency table and graphic with the sentiments present in the text.
  * Inputs:
  + text: string
  + .sentiment= 'afinn' or 'nrc' or 'bing'
  * Output:
  Graphic with sentiments
  Frequency table
  "
  
  # get sentiments
  sentiments <- get_sentiments(.sentiment)
  
  # Transform to tibble
  df_text <- tibble(text)
  
  # Tokenizing the text
  tokens <- df_text %>% 
    unnest_tokens(input = text, #name of the input column
                  output = word) #name of the output column
  
  # If sentiment is AFINN
  if (.sentiment == 'afinn') {
    # Counting frequencies and joining sentiments
    freq_count <- tokens %>% #dataset
      inner_join(sentiments, by='word') %>% #join the sentiments
      count(word, value, sort = TRUE) %>% #count the words by sentiment value
      mutate(score = n * value) %>%  # create score by multiplying score * value
      arrange( desc(score)) # sort
    
    # Plot
    g <- freq_count %>% 
      ggplot( aes(x= score, y= reorder(word, score),
                  fill= score > 0) ) +
      geom_col(show.legend = F) +
      labs( x= 'Sentiment Score',
            y= 'WORD',
            subtitle = 'Negative versus positive sentiments') +
      ggtitle('Sentiment Score by Word in the Text')+
      theme(plot.subtitle =  element_text(color = "gray", face = "italic")) +
      theme_light()
    
    # Return
    return(list(freq_count, g))
  } #close if afinn
  
  
  # If sentiment is BING
  else if (.sentiment == 'bing') {
    
    # Counting frequencies and joining sentiments
    freq_count <- tokens %>% #dataset
      inner_join(sentiments, by='word') %>% #join the sentiments
      count(sentiment, sort = TRUE) #count the words by sentiment value
    
    # Plot
    g <- freq_count %>% 
      ggplot( aes(x= sentiment, y= n,
                  fill= sentiment) ) +
      geom_col(show.legend = F) +
      geom_text(aes(x= sentiment, y= n, label=n), size=8) +
      labs( x= 'Sentiment',
            y= 'Frequency',
            subtitle = 'Negative versus positive sentiments') +
      ggtitle('Count of Sentiments in the Text')+
      theme(plot.subtitle =  element_text(color = "gray", face = "italic")) +
      theme_light()
    
    # Return
    return(list(freq_count, g))
  } #close if bing
  
  
  # If sentiment is NRC
  else if (.sentiment == 'nrc') {
    # Counting frequencies and joining sentiments
    freq_count <- tokens %>% #dataset
      inner_join(sentiments, by='word', multiple = "all") %>% #join the sentiments
      count(sentiment, sort = TRUE) #count the words by sentiment value
    
    # Plot
    g <- freq_count %>% 
      ggplot( aes(x= sentiment, y= n,
                  fill= sentiment) ) +
      geom_col(show.legend = F) +
      geom_text(aes(x= sentiment, y= n, label=n), size=8) +
      labs( x= 'Sentiment',
            y= 'Frequency',
            subtitle = 'Negative versus positive sentiments') +
      ggtitle('Count of Sentiments in the Text')+
      theme(plot.subtitle =  element_text(color = "gray", face = "italic")) +
      theme_light()
    
    # Return
    return(list(freq_count, g))
    
    
  } # close nrc
  
  
  
}#close function


#-------------------------------


######## Text 1##############
# Find this text in Wikipedia, here: https://en.wikipedia.org/wiki/R_(programming_language)
text1 <- "R is a programming language for statistical computing and graphics supported by
the R Core Team and the R Foundation for Statistical Computing. Created by
statisticians Ross Ihaka and Robert Gentleman, R is used among data miners,
bioinformaticians and statisticians for data analysis and developing 
statistical software.[7] Users have created packages to augment the functions
of the R language.
According to user surveys and studies of scholarly literature databases, 
R is one of the most commonly used programming languages in data mining.[8]
As of December 2022, R ranks 11th in the TIOBE index, a measure of programming
language popularity, in which the language peaked in 8th place in August 2020.
The official R software environment is an open-source free software environment
within the GNU package, available under the GNU General Public License. 
It is written primarily in C, Fortran, and R itself (partially self-hosting). 
Precompiled executables are provided for various operating systems. R has a 
command line interface.[11] Multiple third-party graphical user interfaces are 
also available, such as RStudio, an integrated development environment, 
and Jupyter, a notebook interface."
#-------------------------------

# Applying the function
text_freq_counter(text1)
text_freq_counter(text2)


######### Text 2 ###################
text2 <- "Text everywhere! Since the Internet was spread around the world, the amount of textual data 
we generate everyday is ginormous. Only textual messages sent everyday, it is estimated that there are
around 18 Billion of them circulating on a daily basis.
Now imagine the amount of news generated as well. It's a so overwhelming amount that there are whole 
businesses built around news clipping, separating the best information about a given topic to help 
companies in their marketing strategies.
How is AI helping that? Certainly, NLP plays a huge part on that providing good tools and algorithms 
to analyze textual information. As Data Scientists, we can profit of tidytext, an excellent library 
from R to help us building quick analytical tools to check the content of a text.
Let's see that in practice, next."
#-------------------------------

# Applying the function
text_freq_sentiment(text1, .sentiment = 'bing')


############### Text 3 #######################


# News text
text3 <- "Economic data released Wednesday added color and contour to our picture of the labor market, with reports from ADP and the Bureau of Labor Statistics showing robust hiring at the start of the year. However, signs of a cool-off are also coming into focus: layoffs surged and job openings declined in January, per the BLS. The LinkedIn News team continues to keep an eye on hiring, and the latest job cuts are compiled below. If you’ve been impacted by a layoff, find our best tips here.
 
Layoffs making headlines this past week:

More than 350 Company employees in California will be laid off as part of a global revamp of the country's infectious diseases and vaccine groups.
Company, which eliminated 500 salaried positions in late February, is now offering voluntary buyouts to a majority of corporate employees based in the U.S.
Company has laid off 40 additional staffers after undergoing a 14% workforce reduction in November.
Though Company clients will receive their deposits back as the crypto bank winds down operations, its employees are out of work. The same is true for employees of Company, as the health care startup is also shutting down.
LinkedIn members are posting about layoffs at payroll and workforce management provider Company.
Fitness company Company has let go of 15% of its workforce, or about 80 people.
Company publisher Take-Two Interactive confirmed to PC Gamer that it has laid off employees, but did not say how many.
Health care company Company is reducing its headcount and office footprint, but more details are not known.
A restructuring operation at Company will cost 125 people their jobs, including 65 retail staffers.
Company, the business-software maker behind Trello and Jira, is letting go of 500 full-time employees, or 5% of its workforce. The company said the cuts are not financially driven and instead reflect a shift in focus.
Company is feeling the slowdown in car sales, as subscriptions to the satellite radio network are often included with new vehicles. The company is laying off 475 people, or 8% of its headcount.
The autonomous truck group Company is laying off 70% of its staff and closing two of its offices. The company said the remaining 30% will focus on winding down operations.
Education streaming platform Company has reportedly let go of 79 people, following a round of job cuts last June.
On the heels of its first profitable year in 2022, Company laid off 30% of its recruiting staff (0.4% of its total headcount of 6,800).
Following an acquisition that will see it merge with fellow Company has reportedly cut 63 jobs.
Company, a provider of cybersecurity tools that has doubled its headcount over the past 18 months, says it will restructure and cut 177 jobs — about 3% of its 5,900 employees.
Company is laying off hundreds of staffers across its investment banking, operations, IT and mortgage teams, Company reports, citing anonymous sources. The cuts affect less than 1% of staff of 240,000.
Company, let go of 400 people employed by the television-shopping networks.
About a dozen people, including longtime staffers, have been let go from Company.
New leadership at Company reportedly intends to cut 100 jobs as part of a hedge fund-wide reorganization.
Company said it hopes to save $30 million via cost-cutting efforts and layoffs, though the aviation company has not yet said how many will lose their jobs.
A 2,600-person workforce reduction at Company was expected to mostly affect European workers, but LinkedIn posts suggest U.S. workers are also feeling the German chemicals giant's cuts.
Two months after being acquired by Company, note-taking app Evernote is losing 129 employees to layoffs.
Instead of adding 1,000 workers in 2022 as planned, Company has now conducted its second round of layoffs in six months, leaving the conversational startup 15% smaller than it was six months ago.
Company, which once had a valuation of $2 billion, has let go of 100 people, or 14% of its headcount. The short-term rentals startup previously shed 21% of corporate staff and 7% of frontline staff last June.
In a second round of 2023 job cuts, Company has laid off 137 employees, bringing its personnel losses for the calendar year to 8%.
Company, another subsidiary parent company, was shut down. It's not known how many of the unit's 200-plus staff members were laid off — only that both humans and robots lost their jobs.
Despite CEO assertion in 2022 that live events are recession-proof, Company is implementing a workforce reduction of 8% and relocating 30% of roles to Spain and India.
Company, which creates lending software for financial institutions, is shrinking the size of its team by 9%.
Company confirmed to Company what several former employees had shared on LinkedIn: The Chicago tech consultancy has laid off 500 workers, or about 4% of its workforce.
Company company is said to have let go of more than 200 quality testers in its Baton Rouge, Louisiana office.
Job cuts at farm tech startup Company have been described as massive by employees.
Layoffs at Company, which got underway in February, will be much larger than anticipated. The Company employer is cutting 15% of its 49,000-person workforce — more than 7,200 people — after previously saying 10%.
"







