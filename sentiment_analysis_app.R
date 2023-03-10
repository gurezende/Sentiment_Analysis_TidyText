"Project: Shiny App for Text Sentiment Analysis
 Description: This app can analyze an input text and return a table of word frequency
together with a graphic of sentiment analysis, helping people to get a sense of what's 
the text main idea and sentiment or writing tone.
Developer: Gustavo R Santos"


#### Imports #####
#------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(textdata)
library(tidytext)
library(plotly)

#------------------------


#### Function Text Sentiment Analysis #####
#------------------------

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
  # Get Sentiments table
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
    g <- ggplotly(freq_count %>% 
      ggplot( aes(x= score, y= reorder(word, score),
                  fill= score > 0) ) +
      geom_col(show.legend = F) +
      labs( x= 'Sentiment Score',
            y= 'WORD',
            subtitle = 'Negative versus positive sentiments') +
      ggtitle('Sentiment Score by Word in the Text')+
      theme(plot.subtitle =  element_text(color = "gray", face = "italic")) +
      theme_light() )
    
    # Return
    return(list(freq_count, g))
  } #close if afinn
  
  
  # If sentiment is BING
  else if (.sentiment == 'bing') {
    
    # Counting frequencies and joining sentiments
    freq_count <- tokens %>% #dataset
      inner_join(sentiments, by='word') %>% #join the sentiments
      count(sentiment, sort = TRUE) %>%  #count the words by sentiment value
      mutate(n = ifelse(sentiment == "negative", n*-1, n) )
      
    
    # Plot
    g <- ggplotly(freq_count %>% 
      ggplot( aes(y= sentiment, x= n,
                  fill= sentiment) ) +
        geom_col(show.legend = F) +
        geom_text(aes(y= sentiment, x= n, label=n), size=8) +
        labs( x= 'Sentiment',  y= 'Frequency',
              subtitle = 'Negative versus positive sentiments') +
        ggtitle('Count of Sentiments in the Text') +
        theme(plot.subtitle =  element_text(color = "gray", face = "italic")) +
        theme_light() )
    
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
    g <- ggplotly(freq_count %>% 
      ggplot( aes(x= sentiment, y= n,
                  fill= sentiment) ) +
      geom_col(show.legend = F) +
      geom_text(aes(x= sentiment, y= n, label=n), size=8) +
      labs( x= 'Sentiment',
            y= 'Frequency',
            subtitle = 'Negative versus positive sentiments') +
      ggtitle('Count of Sentiments in the Text')+
      theme(plot.subtitle =  element_text(color = "gray", face = "italic")) +
      theme_light() )
    
    # Return
    return(list(freq_count, g))
    
    
  } # close nrc
  
  
  
}#close function


#------------------------



#### User Interface #####
#------------------------

ui <- fluidPage( 
  # Define app theme
  theme= shinytheme('darkly'),
  # Creating a Navigation Bar
  navbarPage(
    theme= 'darkly',
    title = '[ TEXT SENTIMENT ANALYZER ]',
    
    #---------------------
    # Tab About
    #---------------------
    
    tabPanel("About the Project",
             mainPanel(
               fluidRow(
                 # Tab title
                 h2('Text Sentiment Analyzer Project'),
                 # Texts - About the project
                 p('This project - created by ', strong(a("Gustavo R Santos", href="https://www.linkedin.com/in/gurezende/")),
                   '- to quickly create a text analysis to count word frequencies and plot the sentiments graphic.'),
                 p('There are three sentiment datasets available for the analysis that the user can choose'),
                 p(strong('* Bing:'), 'Classifies the words in', em('positive or negative')),
                 p(strong('* Afinn:'), 'Classifies the words in numerical values', em('-2, -1, 2, 3.')),
                 p(strong('* NRC:'), 'Classifies the words in sentiment names like', em('trust, fear, joy'))
                 )# close fluidRow-About
               ) #close mainPanel-About
             ), #close tabPanel-About
    
    
    #---------------------
    # Tab Text Analyzer
    #---------------------
    
    tabPanel("Text Analyzer",
             mainPanel(
               fluidRow(
                 # Page Title 1
                 h3("| Instructions"),
                 # Instructions to use the app
                 helpText("This app receives a text as input. Write down or paste a text in the input box,
                 select a dataset for sentiment classification, and press the button submit. 
                 The app will return the table with word frequencies and the sentiments graphic."),
                 # Line break
                 br(),
                 # Line division
                 span('--------------------------------------------------------------------------------------------------', style = "color:darkgray"),
                 # Input text box title
                 h4("Input your text here:"),
                 #text Input
                 textAreaInput(inputId = "text", NULL, " ", width = "1000px", height="120px"),
                 # Radio Button to select the sentiment dataset to be used
                 radioButtons(inputId = 'sentiments', 
                              label= 'Sentiments datasets',
                              choices= c('afinn', 'bing', 'nrc'), 
                              inline = T, selected = 'bing'),
                 # Add submit Button
                 submitButton(text= 'Submit'),
                 br(),
                 span('--------------------------------------------------------------------------------------------------', style = "color:darkgray"),
                 br(),
                 # Results - Column 1, Sentiments table frequencies
                 column(4,
                        h4("| Sentiment Analysis"),
                        h6("Frequency Table"),
                        h6( tableOutput('freq_table') ) ), #close column 1
                 # Results - Column 2, Sentiments graphic
                 column(8, 
                        h3( "Graphic" ),
                        plotlyOutput(outputId = 'sentiments_plot') ), #close column2
                 br(),
                 br(),
                 helpText(em('Text Analyzer app by ', em('Gustavo R Santos')))
               )# close fluidRow-TextAnalyzer
             ) #close mainPanel-TextAnalyzer
    ) #close tabPanel-TextAnalyzer
    
    ) #close NavbarPage
  )# Close FluidPage


#------------------------


#### Server #####
#------------------------

server <- function(input, output) {
  

  # Piece of code for the Prediction
  output$freq_table <- renderTable({ 
    # If there is no text, show an empty table
    if (input$text == " ") {input$text} 
    else {
      
      # Prepare data for input in the model
      datatext <- text_freq_sentiment(input$text, .sentiment = input$sentiments)
      
      data.frame(datatext[[1]])
    }#end if
    
  }) #output prediction
  
  # Piece of code for the Graphic
  output$sentiments_plot <- renderPlotly({
    # If there is no text, show a dummy graphic
    if (input$text == " ") {
      datatext <- data.frame(variables=c('1','2'), values= c(0,0))
      g <- plot_ly(data = datatext, x = ~values, y = ~variables, type = 'bar',
                   name='Bar', alpha = 0.6)
      g <- g %>% layout(width = 400, height = 200)}
    else {
      
      # Prepare data as a dataset
      datatext <- text_freq_sentiment(input$text, .sentiment = input$sentiments)
      
      # Create graphic
      g <- datatext[[2]]
      plotly_build(g)
      
    }#end if
    
  }) #output bar graphic
  
  
  
} #close server

#------------------------


#### Call Function #####
#------------------------

shinyApp(ui= ui, server = server)

#------------------------

