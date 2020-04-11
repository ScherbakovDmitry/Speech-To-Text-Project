# Speech-To-Text-Project
This project was conducted using R Studio for a Text Analytics project in which survey responses were collected, sentiment analysis was conducted, and a Shiny Dashboard was produced containing the main insights gained.

## About / Synopsis

This repo contains the text analysis of survey responses collected from students in the Bay Area with the following questions:

1. Where do you currently live?
2. How much time do you spend in your commute per day?
3. What is your opinion about working remotely?
4. Would you like to work at the San Jose office at least once per week? Why?
5. Would you relocate to the San Jose office?

The main objective was to develop a Shiny app from the unstructured text data containing the business insight gained from the quantitative frameworks used throughout the analysis. Please use this link to view te final dashboard: [ShinyApp](http://127.0.0.1:3664/)   

## Table of contents

1. Title / Repository Name: #Speech-To-Text-Project
2. About / Synopsis
3. Table of contents
4. Libraries Used
5. Summary of Processes/Frameworks Used
6. Repository Files Included
7. Preview Screenshots of Final Shiny App

## Technical Information/Packages Used

**Built with** [R Studio](https://rstudio.com/)

### Libraries Used

- tidytext
- textreadr
- tidyverse
- topicmodels
- tm
- Matrix
- textdata
- reshape2
- wordcloud
- shiny
- shinydashboard
- leaflet
- leaflet.extras
- DT
- ggplot2
- Sentiment libraries: AFINN, Bing, NRC

## Summary of Processes/Frameworks Used

1. Survey responses were collected as unstructured text data
2. Each of the five questions asked were placed in separate dataframes to further structure using tidy format
3. Responses were tokenized and standard stop words were removed
4. A custom stop word dataframe was created to further remove stop words
5. Tokens were recounted and sorted by frequency
6. Bigram and quadrogram analysis conducted
7. Tf-idf framework implemented to quantify importance of various words as a whole
8. Each question cast into a DTM to examine sparse matrix
9. NRC, Bing, and AFINN sentiment libraries were loaded and joined to examine frequency per question of positive and negative sentiments
10. Final visuals were identified and created 
11. Shiny dashboard, server.R, and ui.R scripts were solidified

## Repository Files Included

- Survey_Answers.docx (word document containing unstructed text survey responses)
- SHINY DASHBOARD.R
- server.R
- ui.R
- 5 Preview Screenshots of Shiny app

## Preview Screenshots of Final Shiny App

![image](https://github.com/pschooley/Speech-To-Text-Project/blob/master/Dashboard1.png)
![image](https://github.com/pschooley/Speech-To-Text-Project/blob/master/Dashboard2.png)
![image](https://github.com/pschooley/Speech-To-Text-Project/blob/master/Dashboard3.png)
![image](https://github.com/pschooley/Speech-To-Text-Project/blob/master/Dashboard4.png)
![image](https://github.com/pschooley/Speech-To-Text-Project/blob/master/Dashboard5.png)


