#Required libaries

suppressPackageStartupMessages(c(library(shiny),
                                 library(shinythemes),
                                 library(dplyr),
                                 library(tidytext),
                                 library(plotly)))

#Word Frequency Dictionaries

unigram <- readRDS("unigram.rds")
bigram <- readRDS("bigram.rds")
trigram <- readRDS("trigram.rds")
quadgram <- readRDS("quadgram.rds")

#Function to transform input from shiny into separate words
cleanInput <- function(a){ 
        if(length(a) == 0) {
                word <- tibble(x = "",
                               y = "",
                               z = "")
        }
        
        else if(length(a) == 1){
                word <- tibble(sentence = a) %>% unnest_tokens(output = word,
                                                               input = sentence,
                                                               to_lower = TRUE,
                                                               strip_punct = TRUE,
                                                               strip_numeric = TRUE)
                
                if(nrow(word) == 1){ 
                        word <- tibble(x = word[1,] %>% pull(),
                                       y = "",
                                       z = "")
                }
                else if(nrow(word) == 2){
                        word <- tibble(x = word[1,] %>% pull(),
                                       y = word[2,] %>% pull(),
                                       z = "")
                }
                else if(nrow(word) >= 3){
                        word = tibble(x = tail(word, 3)[1,] %>% pull(),
                                      y = tail(word, 2)[1,] %>% pull(),
                                      z = tail(word, 1) %>% pull())
                        
                }
        }
}

#Function for text Prediction
NextWord <- function(x = "", y = "", z = "", n = 5){
        
        if(length(x) == 0 & length(y) == 0 & length(z) == 0) print("Please ignore the warning, it will disapear if you insert some Text")
        
        else if (x %in% quadgram$word1 & y %in% quadgram$word2 & z %in% quadgram$word3){
                quadgram %>% filter(x == word1, y == word2, z == word3) %>% select(Next_Word, Probability) %>% head(n)
        }
        else if (x %in% trigram$word1 & y %in% trigram$word2){
                trigram %>% filter(x == word1, y == word2) %>% select(Next_Word, Probability) %>% head(n)
        }
        else if (x %in% bigram$word1){
                bigram %>% filter(x == word1) %>% select(Next_Word, Probability) %>% head(n)
        }
        else unigram[1:n,]
        
}

# Define UI for application that Predicts Words
ui <- fluidPage(theme = shinytheme("cerulean"),
        tabsetPanel(
        tabPanel(title = "App",
   
   # Application title
   titlePanel("Words Prediction With R"),
   
   # Inputs
   fluidRow(column(width = 6,
      
         textInput(inputId = "sentence",
                   label = "Predict the Next Word of this text",
                   value = "hi"),
         
         sliderInput(inputId = "suggestions",
                     label = "Nº of Suggestions",
                     min = 1,
                     max = 15, 
                     value = 5),
         
         submitButton(text = "Submit Text")
      ),
      
      # Outputs
      column(width = 6,
         tableOutput("prediction")
      )
   ),
   
   fluidRow(column(width = 12,
                   plotlyOutput("bar")
                   )
            )
),
#About this App Tab
tabPanel(title = "About this App", 
         withMathJax(includeHTML("about_app.html"))
         ))
)

# Server logic required to Make the Prediction and plot it
server <- function(input, output) {
        
        prediction  <- reactive({
                
          word <- cleanInput(input$sentence)
          NextWord(word$x,word$y,word$z, input$suggestions)
   })
        #Table
        output$prediction <- renderTable(prediction(),
                                         bordered = TRUE,
                                         hover = TRUE,
                                         striped = TRUE,
                                         colnames = TRUE)
        #Plot
        output$bar <- renderPlotly({
                plot_ly(prediction(), 
                        y = ~Probability,
                        x = ~Next_Word,
                        type = "bar",
                        color = ~Probability) 
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

