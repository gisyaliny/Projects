library(shiny);library(data.table);library(textstem)
library(lubridate);library(tm);library(syuzhet);library(DT)
library(ggplot2);library(hrbrthemes);library(wordcloud)
library(wordcloud2);library(fmsb);library(shinycssloaders)


# Define UI
ui <- fluidPage(
  titlePanel("Twitter Sentiment Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("keyword", "Enter a keyword:", value = "apple"),
      actionButton("submit", "Submit"),  # Submit button
      sliderInput("numTweets", "Number of Tweets:", 
                  min = 5, max = 50, value = 50)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top Tweets", dataTableOutput("topTweets")),
        tabPanel("Word Cloud", uiOutput("wordcloud") %>% withSpinner()),
        tabPanel("Sentiment Analysis", plotOutput("sentimentPlot") %>% withSpinner()),
        tabPanel("Trend Analysis", plotOutput("trendPlot") %>% withSpinner())
      )
    )
  )
)

# Define server function
server <- function(input, output) {
  
  # Function to search tweets
  search_tweets <- function(tweets_df, keyword, ignore.case = TRUE) {
    tryCatch({
      # Ensure tweets_df is a data.table
      setDT(tweets_df)
      # Vectorized search for keyword
      suppressWarnings(keyword_filtered <- tweets_df[grepl(keyword, text, ignore.case = ignore.case)])
      return(keyword_filtered)
    }, error = function(e) {
      stop("An error occurred: ", conditionMessage(e))
    })
  }
  
  # Function to preprocess text
  preprocess_text <- function(text_column) {
    text_column <- tolower(text_column)                   # Convert text to lowercase
    text_column <- removePunctuation(text_column)         # Remove punctuation
    text_column <- removeNumbers(text_column)             # Remove numbers
    text_column <- removeWords(text_column, stopwords("en")) # Remove stopwords
    text_column <- stripWhitespace(text_column)           # Strip extra whitespace
    text_column <- lemmatize_strings(text_column)         # Apply lemmatization
    return(text_column)
  }
  
  # Function to analyze sentiment
  analyze_sentiment <- function(preprocessed_text) {
    sentiment_scores <- get_nrc_sentiment(preprocessed_text)
    sentiment_scores$overall_sentiment <- sentiment_scores$positive - sentiment_scores$negative
    return(sentiment_scores)
  }
  
  # Reactive values to store processed data
  values <- reactiveValues()
  
  # Load and process data when submit button is clicked
  observeEvent(input$submit, {
    data <- read.csv("./inputs.csv", header = FALSE, col.names = c("target", "ids", "date", "flag", "user", "text"), encoding = "UTF-8")
    keyword_filtered <- search_tweets(data, input$keyword)
    keyword_filtered[, text_clean := preprocess_text(text)]
    sentiment_results <- analyze_sentiment(keyword_filtered$text_clean)
    
    values$keyword_filtered <- keyword_filtered
    values$sentiment_results <- sentiment_results
  })
  
  # Output for top tweets
  output$topTweets <- renderDataTable({
    req(values$keyword_filtered)
    top_tweets <- head(values$keyword_filtered, input$numTweets)
    datatable(top_tweets[, c("date", "user", "text")], options = list(pageLength = input$numTweets))
  })
  
  
  # Render Word Cloud
  output$wordcloud <- renderUI({
    if (!is.null(values$keyword_filtered)) {
      # Create a corpus  
      docs <- Corpus(VectorSource(values$keyword_filtered$text_clean))
      dtm <- TermDocumentMatrix(docs) 
      matrix <- as.matrix(dtm) 
      words <- sort(rowSums(matrix), decreasing=TRUE) 
      df_words <- data.frame(word = names(words), freq=words)
      
      wordcloud2(data=df_words, size=4, color='random-dark')
    }
  })
  
  # Render Sentiment Analysis Plot
  output$sentimentPlot <- renderPlot({
    if (!is.null(values$sentiment_results)) {
      emotion_averages <- data.frame(
        Anger = mean(values$sentiment_results$anger),
        Anticipation = mean(values$sentiment_results$anticipation),
        Disgust = mean(values$sentiment_results$disgust),
        Fear = mean(values$sentiment_results$fear),
        Joy = mean(values$sentiment_results$joy),
        Sadness = mean(values$sentiment_results$sadness),
        Surprise = mean(values$sentiment_results$surprise),
        Trust = mean(values$sentiment_results$trust)
      )
      # Add a row for the minimum value for each category for plotting
      emotion_df <- rbind(rep(0, ncol(emotion_averages)),rep(1, ncol(emotion_averages)), emotion_averages)
      
      # Create the radar chart
      fmsb::radarchart(emotion_df, axistype = 1,
                       #custom polygon
                       pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4,
                       #custom the grid
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,5), cglwd=0.8,
                       #custom labels
                       vlcex=1.2)
      
    }
  })
  
  # Render Trend Analysis Plot
  output$trendPlot <- renderPlot({
    if (!is.null(values$keyword_filtered) && !is.null(values$sentiment_results)) {
      values$keyword_filtered$date_parsed <- as.Date(strptime(values$keyword_filtered$date, format = "%a %b %d %H:%M:%S PDT %Y"))
      values$keyword_filtered$overall_sentiment <- values$sentiment_results$overall_sentiment
      daily_avg_sentiment <- aggregate(overall_sentiment ~ date_parsed, data = values$keyword_filtered, mean)
      # Fit a linear model
      model <- lm(overall_sentiment ~ date_parsed, data = daily_avg_sentiment)
      
      # Extract slope and R^2 value
      slope <- coef(model)[2]
      r_squared <- summary(model)$r.squared
      
      # Your original ggplot code
      p <- ggplot(daily_avg_sentiment, aes(x = date_parsed, y = overall_sentiment)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Daily Average Overall Sentiment Score",
             x = "Date",
             y = "Average Sentiment Score") +
        theme_minimal()
      
      # Add annotations for slope and R^2
      p + annotate("text", x = as.Date(quantile(as.numeric(daily_avg_sentiment$date_parsed),0.75)), y = 1.2, 
                   label = paste("Slope:", round(slope, 4), "\nR^2:", round(r_squared, 4)),
                   hjust = 0, vjust = 0)
      p
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)






