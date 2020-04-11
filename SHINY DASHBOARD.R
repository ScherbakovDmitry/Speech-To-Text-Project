library(shiny)
library(shinydashboard)

#USER INTERFACE
ui <- dashboardPage(
  #HEADER
  dashboardHeader(title = 'Text Analytics - Team 5'),
  
  #SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      menuItem("Database", tabName = "database", icon = icon("database")),
      menuItem("Frequencies", tabName = "freq", icon = icon("chart-bar")),
      menuItem("Sentiment", tabName = "sentiment", icon = icon("heart")),
      menuItem("TF-IDF", tabName = "tf-idf", icon = icon("sort-amount-down")),
      menuItem("Map", tabName = "map", icon = icon("map"))
    )
  ),
  
  #BODY
  dashboardBody(
    tabItems(
      # 1st tab content
      tabItem(tabName = "database",
              fluidRow(
                box(
                  title = "Database", 
                  width = 12,
                  tableOutput('survey'))
                
              )
      ),
      
      # 2nd tab content
      tabItem(tabName = "freq",
              fluidRow(
                box(
                  title = "Where do you live?",
                  plotOutput("q1freq"),
                ),
                box(
                  title = "How much time do you spend in your commute per day?",
                  plotOutput("q2freq")
                )
              ),
              fluidRow(
                box(
                  title = "What is your opinion about working remotely?",
                  width = 12,
                  plotOutput("q3freq")
                )
              )
      ),
      
      # 3rd tab content
      tabItem(tabName = "sentiment",
              fluidRow(  
                box(
                  title = "What is your opinion about working remotely? - nrc Word Cloud",
                  width = 6,
                  plotOutput("q3nrc"),
                ),
                box(
                  title = "What is your opinion about working remotely? - bing Word Cloud",
                  width = 6,
                  plotOutput("q3bing")
                )
              ),
              fluidRow(
                infoBoxOutput('q3afinn'),
                infoBoxOutput('q4afinn')
              ),
              fluidRow(  
                box(
                  title = "Would you like to work at the San Jose office at least once per week? Why? - nrc Word Cloud",
                  width = 6,
                  plotOutput("q4nrc"),
                ),
                box(
                  title = "Would you like to work at the San Jose office at least once per week? Why? - bing Word Cloud",
                  width = 6,
                  plotOutput("q4bing")
                )
              )
      ),
      
      # 4th tab content
      tabItem(tabName = "tf-idf",
              column(
                width = 12,
                plotOutput("tfidf")
              ),
              fluidRow(
                box(
                  title = "Tokens order by tf-idf (Descending)", 
                  width = 12,
                  align = "center",
                  tableOutput('tftable'))
                
              )
              
      ),
      
      # Last tab content
      tabItem(tabName = "map",
              
              fluidRow(
                box(title = "Explore our Interactive Map", 
                    width = 12, 
                    leafletOutput("coolmap")
                )
              )#,
              
              #fluidRow(
               # box(title = "Predictive Model", 
                #    width = 12 
                #)
              #)
      )
      
    ) #Closing tab items
  ) #Closing Dashboard Body
) ##Closing Dashboard Page


#SERVER
server <- function(input, output) {
  
  #**********
  #GENERAL CODE    vvvvvvvvvv
  #**********
  library(tidytext)
  library(textreadr)
  library(tidyverse)
  library(tm)
  library(Matrix)
  library(textdata)
  library(reshape2)
  library(wordcloud)
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(leaflet.extras)
  library(DT)
  library(ggplot2)
  data("stop_words")
  
  #######################################################################################################################################
  
  #Loading the file that contains raw data
  workplace_survey_answers <- read_document(file="/Users/robertmusci/Desktop/Parker Portfolio Work/Text Analytics Survey Project R/Survey_Answers.docx")
  
  #Creating a dataframe from the loaded file
  a <- 23 #how many observations do you have
  b <- 5 #how many variables do you have
  survey_df <- as.data.frame(matrix(nrow=a, ncol=b))
  
  for(z in 1:b){
    for(i in 1:a){
      survey_df[i,z]<- workplace_survey_answers[i*b+z-b]
    }#closing z loop
  }#closing i loop
  
  #Renaming the variables
  names(survey_df)[1] <- 'Where do you live?'
  names(survey_df)[2] <- 'How much time do you spend in your commute per day?'
  names(survey_df)[3] <- 'What is your opinion about working remotely?'
  names(survey_df)[4] <- 'Would you like to work at the San Jose office at least once per week? Why?'
  names(survey_df)[5] <- 'Would you relocate to the San Jose office?'
  
  cust_stop <- data_frame(word = c("live", "it’s", "i’m", "let’s", "that’s", "there’s", "they’re", "yeah", "you’re"), lexicon = rep("custom", each = 9))
  
  theme_survey <- function(){
    theme_minimal() +
      theme(
        text = element_text(color = "gray25"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(color = "gray30"),
        plot.background = element_rect(fill = "gray95"),
        plot.margin = unit(c(5, 10, 5, 10), units = "mm")
      )
  }
  
  question_1 <- survey_df$`Where do you live?`
  question_1_df <- data_frame(line=1:a, text=question_1)
  
  question_1_token_freq <- question_1_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE)
  
  question_1_bigram <- question_1_df %>%
    unnest_tokens(bigram,  text, token = "ngrams", n=2)
  
  question_1_bigram %>% count(bigram, sort = T)
  
  question_1_bigram_separated <- question_1_bigram %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  question_1_bigram_filtered <- question_1_bigram_separated %>%
    filter(!word1 %in% c(stop_words$word, cust_stop$word)) %>%
    filter(!word2 %in% c(stop_words$word, cust_stop$word))
  
  question_1_bigram_united_freq <- question_1_bigram_filtered %>%
    unite(bigram, word1, word2, sep=" ") %>%
    count(bigram, sort = T)
  
  freq_hist_question_1_bigram <- question_1_bigram_united_freq %>%
    filter(n > 1) %>%
    mutate(bigram = reorder(bigram,n)) %>%
    ggplot(aes(bigram, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + theme_survey()
  
  ###########
  
  question_2 <- survey_df$`How much time do you spend in your commute per day?`
  question_2_df <- data_frame(line=1:a, text=question_2)
  
  question_2_token_freq <- question_2_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE)
  
  question_2_bigram <- question_2_df %>%
    unnest_tokens(bigram,  text, token = "ngrams", n=2)
  question_2_bigram %>% count(bigram, sort = T)
  
  question_2_bigram_separated <- question_2_bigram %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  question_2_bigram_filtered <- question_2_bigram_separated %>%
    filter(!word1 %in% c(stop_words$word, cust_stop$word)) %>%
    filter(!word2 %in% c(stop_words$word, cust_stop$word))
  
  
  question_2_bigram_united_freq <- question_2_bigram_filtered %>%
    unite(bigram, word1, word2, sep=" ") %>%
    count(bigram, sort = T)
  
  freq_hist_question_2_bigram <- question_2_bigram_united_freq %>%
    filter(n > 1) %>%
    mutate(bigram = reorder(bigram,n)) %>%
    ggplot(aes(bigram, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + theme_survey()
  
  ###########
  
  question_3 <- survey_df$`What is your opinion about working remotely?`
  question_3_df <- data_frame(line=1:a, text=question_3)
  
  question_3_token_freq <- question_3_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE)
  
  #RUN A QUADROGRAM
  question_3_quadrogram <- question_3_df %>%
    unnest_tokens(quadrogram,  text, token = "ngrams", n=4)
  
  question_3_quadrogram_freq <- question_3_quadrogram %>% count(quadrogram, sort = T)
  
  freq_hist_question_3_quadrogram <- question_3_quadrogram_freq %>%
    filter(n > 1) %>%
    mutate(quadrogram = reorder(quadrogram,n)) %>%
    ggplot(aes(quadrogram, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + theme_survey()
  
  
  ###########
  
  question_4 <- survey_df$`Would you like to work at the San Jose office at least once per week? Why?`
  question_4_df <- data_frame(line=1:a, text=question_4)
  
  question_4_token_freq <- question_4_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    anti_join(cust_stop) %>%
    count(word, sort=TRUE)
  
  question_4_bigram <- question_4_df %>%
    unnest_tokens(bigram,  text, token = "ngrams", n=2) %>% 
    count(bigram, sort = T)
  
  question_4_bigram_separated <- question_4_bigram %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  cust_stop_1 <- data_frame(word = c("san", "jose", "francisco"), lexicon = rep("custom", each = 3))
  
  question_4_bigram_filtered <- question_4_bigram_separated %>%
    filter(!word1 %in% c(cust_stop_1$word)) %>%
    filter(!word2 %in% c(cust_stop_1$word))
  
  question_4_bigram_united_freq <- question_4_bigram_filtered %>%
    unite(bigram, word1, word2, sep=" ")
  
  freq_hist_question_4_bigram <- question_4_bigram_united_freq %>%
    filter(n > 4) %>%
    mutate(bigram = reorder(bigram,n)) %>%
    ggplot(aes(bigram, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + theme_survey()
  
  ###########
  
  question_5 <- survey_df$`Would you relocate to the San Jose office?`
  question_5_df <- data_frame(line=1:a, text=question_5)
  
  question_5_token_freq <- question_5_df %>%
    unnest_tokens(word, text) %>%  #Removing stopwords will remove Yes and No answer
    count(word, sort=TRUE)
  
  freq_hist_question_5 <-question_5_token_freq %>%
    filter(n > 1) %>% 
    mutate(word = reorder(word,n )) %>%
    ggplot(aes(word, n)) + geom_col(fill = "steelblue") + xlab(NULL) + theme_survey()
  
  #######################################################################################################################################
  survey_df_tf_idf <- bind_rows(
    mutate(question_1_token_freq, question = "1. First"),
    mutate(question_2_token_freq, question = "2. Second"),
    mutate(question_3_token_freq, question = "3. Third"),
    mutate(question_4_token_freq, question = "4. Fourth")
    #mutate(question_5_token_freq, question = "5. Fifth")
  )
  
  survey_df_tf_idf_1 <- survey_df_tf_idf %>% 
    bind_tf_idf(word, question, n)%>%
    arrange(desc(tf_idf))
    
  
  survey_df_tf_idf_1 %>%
    arrange(desc(tf_idf)) %>%
    mutate(word=factor(word, levels=rev(unique(word)))) %>%
    group_by(question) %>%
    top_n(8, wt=tf_idf) %>%
    ungroup %>%
    ggplot(aes(word, tf_idf, fill=question))+
    geom_col(show.legend=FALSE)+
    labs(x=NULL, y="tf-idf")+
    facet_wrap(~question, ncol=2, scales="free")+
    coord_flip()
  #######################################################################################################################################
  
  #Creating a document term matrix and a sparse matrix
  question_1_tidy <- question_1_df %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(line, word, sort=TRUE)
  question_1_dtm <- question_1_tidy %>% cast_dtm(line, word, n)
  question_1_dtm
  question_1_sparse_matrix <- question_1_tidy %>% cast_sparse(line, word, n)
  class(question_1_sparse_matrix)
  dim(question_1_sparse_matrix)
  
  question_2_tidy <- question_2_df %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(line, word, sort=TRUE)
  question_2_dtm <- question_2_tidy %>% cast_dtm(line, word, n)
  question_2_dtm
  question_2_sparse_matrix <- question_2_tidy %>% cast_sparse(line, word, n)
  class(question_2_sparse_matrix)
  dim(question_2_sparse_matrix)
  
  question_3_tidy <- question_3_df %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(line, word, sort=TRUE)
  question_3_dtm <- question_3_tidy %>% cast_dtm(line, word, n)
  question_3_dtm
  question_3_sparse_matrix <- question_3_tidy %>% cast_sparse(line, word, n)
  class(question_3_sparse_matrix)
  dim(question_3_sparse_matrix)
  
  question_4_tidy <- question_4_df %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(line, word, sort=TRUE)
  question_4_dtm <- question_4_tidy %>% cast_dtm(line, word, n)
  question_4_dtm
  question_4_sparse_matrix <- question_4_tidy %>% cast_sparse(line, word, n)
  class(question_4_sparse_matrix)
  dim(question_4_sparse_matrix)
  
  question_5_tidy <- question_5_df %>% unnest_tokens(word, text) %>% count(line, word, sort=TRUE)
  question_5_dtm <- question_5_tidy %>% cast_dtm(line, word, n)
  question_5_dtm
  question_5_sparse_matrix <- question_5_tidy %>% cast_sparse(line, word, n)
  class(question_5_sparse_matrix)
  dim(question_5_sparse_matrix)
  
  #######################################################################################################################################
  
  #Sentiment analysis
  afinn <- get_sentiments("afinn")
  bing <- get_sentiments("bing")
  nrc <- get_sentiments("nrc")
  sentiments_combined <- bind_rows(mutate(afinn, lexicon = "afinn"), mutate(nrc, lexicon = "nrc"), mutate(bing, lexicon = "bing"))
  
  question_3_token_freq %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(colors = c("grey20", "gray80"),
                     max.words=100, scale = c(1, 1), fixed.asp = T, title.size = 1)
  
  question_3_token_freq %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(colors = c("grey20", "gray80"),
                     max.words=100, scale = c(1, 1), fixed.asp = TRUE, title.size = 1)
  
  question_3_token_freq %>% inner_join(afinn) %>% summarise(avg_afinn_value = mean(value))
  
  ##############
  
  question_4_token_freq %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(colors = c("grey20", "gray80"),
                     max.words=100, scale = c(1, 1), fixed.asp = T, title.size = 1)
  
  question_4_token_freq %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort=TRUE) %>%
    acast(word ~sentiment, value.var="n", fill=0) %>%
    comparison.cloud(colors = c("grey20", "gray80"),
                     max.words=100, scale = c(1, 1), fixed.asp = TRUE, title.size = 1)
  
  
  question_4_token_freq %>% inner_join(afinn) %>% summarise(avg_afinn_value = mean(value))
  
  
  #**********
  #GENERAL CODE    ^^^^^^^^^^^^
  #**********
  
  
  #----------
  #DATABASE OUTPUT
  #----------
  output$survey <- renderTable({
    survey_df
  })
  
  #----------
  #FREQUENCIES OUTPUT
  #----------
  output$q1freq <- renderPlot({
    freq_hist_question_1_bigram
  })
  
  output$q2freq <- renderPlot({
    freq_hist_question_2_bigram
  })
  
  output$q3freq <- renderPlot({
    freq_hist_question_3_quadrogram
  })
  
  #----------
  #SENTIMENT OUTPUT
  #----------
  output$q3nrc <- renderPlot({
    question_3_token_freq %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors = c("grey20", "gray80"),
                       max.words=100, scale = c(1, 1), fixed.asp = T, title.size = 1)
  })
  
  output$q3bing <- renderPlot({
    question_3_token_freq %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors = c("grey20", "gray80"),
                       max.words=100, scale = c(1, 1), fixed.asp = TRUE, title.size = 1)
  })
  
  output$q3afinn <- renderInfoBox({
    infoBox(
      "afinn value 'Working Remotely'", "0.5", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$q4nrc <- renderPlot({
    question_4_token_freq %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors = c("grey20", "gray80"),
                       max.words=100, scale = c(1, 1), fixed.asp = T, title.size = 1)
  })
  
  output$q4bing <- renderPlot({
    question_4_token_freq %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors = c("grey20", "gray80"),
                       max.words=100, scale = c(1, 1), fixed.asp = TRUE, title.size = 1)
  })
  
  output$q4afinn <- renderInfoBox({
    infoBox(
      "afinn value 'Working in SJ Office'", "-0.57", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  #----------
  #TF-IDF OUTPUT
  #----------
  output$tfidf <- renderPlot({
    survey_df_tf_idf_1 %>%
      arrange(desc(tf_idf)) %>%
      mutate(word=factor(word, levels=rev(unique(word)))) %>%
      group_by(question) %>%
      top_n(8, wt=tf_idf) %>%
      ungroup %>%
      ggplot(aes(word, tf_idf, fill=question))+
      geom_col(show.legend=FALSE)+
      labs(x=NULL, y="tf-idf")+
      facet_wrap(~question, ncol=2, scales="free")+
      coord_flip()
  })
  
  output$tftable <- renderTable({
    head(survey_df_tf_idf_1,10)
  })
  
  #----------
  #MAP OUTPUT
  #----------
  output$coolmap <- renderLeaflet({
    leaflet()%>%
      addTiles()%>%
      setView(-122.3255, 37.5630, zoom = 9)%>% #Location of San Mateo
      addPopups(-122.4194, 37.7749, 'San Francisco')%>%
      addPopups(-121.8863, 37.3382, 'San Jose')%>%
      #addProviderTiles(providers$CartoDB.Positron)%>%
      addDrawToolbar( editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())) %>% 
      addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers")
  })
}

shinyApp(ui, server)

