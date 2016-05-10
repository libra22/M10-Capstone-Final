# This is the minimum viable product for the app.
# Add error handling code, and your own enhancements

library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",#get more bootstrap themes from http://bootswatch.com/
  # Set the page title
  titlePanel("  Text Predictor"),
  
  sidebarPanel(
    textInput("entry",
              h5("Type your phrase below and click 'Submit'"),
              "your phrase"),
    radioButtons("radio", 
                 h5("Prediction Method"),
                 choices = list("Simple back-off" = 1, "Simple Good-Turing " = 2),
                 selected = 1),
    submitButton("Submit"),
    br()
    
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Prediction", 
                         h4(" "),
                         p("You entered:"),
                         span(h4(textOutput('sent')),style = "color:blue"),
                         p('and the predicted next word is:'),
                         span(h4(textOutput('text')),style = "color:blue")
                        ),
                tabPanel("About", 
                         h4(" "),
                         p("This application predicts the next word based on a phrase that user enters."),
                         p("User can use either Simple Back-off or Simple Good-Turing method for the prediction."),
                         p("Regardless of the length of user input and type of method choosen, the application only uses the last two words of user input. For example, if user enters 'time to make a decision on whether to', the application will use 'whether to' to make the prediction."),
                         p("Simple Back-off check the possible words in a 3-word table (trigram). If a match is found, it returns the matched word. If the are more than one match, it returns the word with the highest occurence. If no match is found, it then checks on a 2-word table (bigram). If a match is found, it returns the matched word.If the are more than one match, it returns the word with the highest occurence. If no match is found, it then returns the word with the highest occurence in a 1-word table (unigram)."),
                         p("Simple Good-Turing takes into consideration that a word not in our dictionary may be entered by user. thus it calculates these probabilities to make a better prediction of the word. It checks in 3-word table, and if no match is found, it then checks in 2-word table. However if no match is found in either table, it returns a \"not found\" message.")
                         
                         
                         
                )              
                )
               
                
    ))
)