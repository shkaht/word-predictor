library(shiny)
library(shinythemes)
library(quanteda)
library(dplyr)
library(stringr)

load("data/prediction-tables.RData")

ui <- fluidPage(
    theme = shinytheme("superhero"),
    
    tabsetPanel(
        tabPanel(
            title = "Next Word Helper",
            br(),
            br(),
            h2("Not sure what to say next?  Let us help!!"),
            br(),
            textInput(inputId = "phrase", 
                      label = "Type your words here and we'll suggest the most popular word to keep the thought alive:",
                      placeholder = "your text here",
                      width = "600px"),
            
            h2(strong(
                textOutput(outputId = "answer"), 
                tags$head(tags$style("#answer{color: yellow;font-size: 50px;font-style: italic;}"
                )
                )
            )
            )
        ),
        
    tabPanel(title = "About the App", 
             br(),
             h1("A quick look under the hood"),
             br(),
             h3("The data source"),
             p("The app runs on a dictionary of the most frequent words
               and phrases of sample text from the following project-provided sources:"),
             p(" - 77,259 lines from news in en.US.news.txt"),
             p(" - 899,288 lines from blogs in en.US.blogs.txt"),
             p(" - 2,360,148 lines from Twitter in en.US.blog.txt"),
             br(),
             h3("The focus: speed and a contextually relevant next word"),
             p("From these text sources, a sample of 50% of the lines was cleaned and used 
               to generate the following n-gram totals, each with a unique n-1 phrase, and 
               only the most frequent final word following n-1 phrase."),
             p("   5-grams of the top 520,097 5-word phrases"),
             p("   4-grams of the top 848,244 4-word phrases"),
             p("   3-grams of the top 649,835 3-word phrases"),
             p("   2-grams of the top 432,946 2-word phrases"),
             p("   1-grams of the top 10  individual words"), 
             br(), 
             h3("The algorithm"),
             p("The next word prediction algorithm first attempts to use the 5-grams by checking for 
               a match in the dictionary for the last 4 user-entered words, and returning the top match
               for the 5th word.  If there is no match for the last 4 words (or less than 4 words were
               entered) the algorithm will try the same process using 4-grams, and so on. If no matches 
               are found with the entered text and any of the n-grams, one of the 10 most popluar words 
               will be randomly selected and returned."),
             br(), 
             h3("More info"),
             a("Link to app source code", 
               href = "https://github.com/shkaht/word-predictor/blob/master/app.R"),
             br(),
             a("Link to raw text processing script", 
               href = "https://github.com/shkaht/word-predictor/blob/master/read-text-lines2.R")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$answer <- renderText({
        
        if(nchar(input$phrase) == 0) {
            ifnull <- "[predicted next word will appear here]"
            ifnull
            
        } else {
        
        inputphrase <- tokens(input$phrase) 
        count.words <- ntoken(inputphrase)
        ptable <- data.frame()
        
        if(count.words >= 4) {
            last4 <- tail(inputphrase$text1, n = 4)
            last4 <- str_c(last4, collapse = " ")
            p5 <- filter(five.grams, last4 == V1) 
            ptable <- bind_rows(ptable, p5)
        }
        
        if(count.words >= 3) {
            last3 <- tail(inputphrase$text1, n = 3)
            last3 <- str_c(last3, collapse = " ")
            p4 <- filter(four.grams, last3 == V1) 
            ptable <- bind_rows(ptable, p4)
        }
        
        if(count.words >= 2) {
            last2 <- tail(inputphrase$text1, n = 2) %>%
                str_c(collapse = " ")
            p3 <- filter(three.grams, last2 == V1)
            ptable <- bind_rows(ptable, p3)
        }
        
        if(count.words >= 1) {
            last1 <- tail(inputphrase$text1, n = 1) 
            p2 <- filter(two.grams, last1 == V1) 
            ptable <- bind_rows(ptable, p2)
        }
        
        if(is.na(ptable[1,1] == TRUE)) {
            p1 <- one.grams %>%
                sample_n(1) 
            ptable <- bind_rows(ptable, p1)
        }
        
        ptable[1,2]
        
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
