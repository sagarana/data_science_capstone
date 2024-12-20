# App and server sections for predictive text app in R Shiny

library(shiny)
library(wordcloud2)
library(dplyr)
library(shinyjs)
library(DT)

# Make sure app can access trigrams
load("trigram_data.Rda")

# Function for generating predictions
generate_predictions <- function(last_two_words) {
        prediction <- trigram_probabilities %>%
                filter(w1 == last_two_words[1] &
                               w2 == last_two_words[2]) %>%
                arrange(desc(probability)) %>%
                slice(1:7) %>%
                select(w3, probability)
        
        row_count <- nrow(prediction)
        
        # If fewer than 7 trigram matches, match on bigrams (last word only)
        if (row_count < 7) {
                fallback_prediction <- trigram_probabilities %>%
                        filter(w2 == last_two_words[2]) %>%
                        anti_join(prediction, by = "w3") %>% # remove possible duplicates already in `prediction`
                        select(-w1) %>%
                        group_by(w3) %>%
                        summarise(probability = sum(probability*.1)) %>% # weight bigrams less than trigrams if/when joined
                        arrange(desc(probability)) %>%
                        slice(1:(7-row_count)) %>%
                        select(w3, probability)
                
                # Join trigram and bigram matches
                filled_prediction <- rbind(prediction,fallback_prediction) %>%
                        group_by(w3)
                
                # If no trigram or bigram matches, return empty data frame
                if (nrow(filled_prediction) == 0) {
                        return(data.frame(
                                w3 = character(0),
                                probability = numeric(0)
                        ))
                }
                
                return(filled_prediction)
        }
        
        return(prediction)
}

# Define UI
ui <- fluidPage(useShinyjs(),
                tags$style(
                        HTML("
      body {
        padding-top: 20px; /* Adds space at the top of the page */
      }
    ")
                ),
                
                tabsetPanel(
                        tabPanel("Text Prediction", sidebarLayout(
                                sidebarPanel(
                                        textAreaInput(
                                                "input_text",
                                                "",
                                                value = "",
                                                placeholder = "Type something here...",
                                                rows = 5
                                        ),
                                        
                                        # Conditionally rendered caption and table
                                        uiOutput("caption"),
                                        DT::dataTableOutput("predictions_table")  # Table of predictions in sidebar
                                ),
                                mainPanel(fluidRow(column(
                                        12,
                                        wordcloud2Output("word_cloud", width = "100%", height = "400px")  # Word cloud using wordcloud2
                                )))
                        )),
                        
                        tabPanel("About This App", fluidPage(
                                h3("About this app"),
                                p(
                                        "This Shiny app predicts the next word(s) based on the last two words entered by the user. It uses trigram probabilities to generate word predictions and displays 
                                        the top seven predictions as word cloud and in a table. Users can click a word in the table to add it to the text or type a new word themselves."
                                ),
                                p(
                                        "Predictions are based on trigrams that appear in a sample of English language blogs, news stories, and twitter/X posts. If there is no matching trigram in the 
                                        sample data the prediction function looks for bigrams instead."
                                ),
                                p(
                                        "This app was developed in December 2024 as the capstone project to the Data Science Specialization offered on Coursera by Johns Hopkins University."
                                ),
                                h4("How to use the app"),
                                p("1. Enter some text in the input box."),
                                p("2. See predicted next words in the word cloud and table."),
                                p("3. Click a word in the table to add it to the text, or add a new word by typing.")
                        ))
                ))

server <- function(input, output, session) {
        # Extract the last two words
        last_two_words <- reactive({
                input_text <- tolower(input$input_text)
                words <- strsplit(input_text, "\\s+")[[1]]
                if (length(words) >= 2) {
                        tail(words, 2)
                } else {
                        words
                }
        })
        
        # Generate predictions based on the last two words
        predictions <- reactive({
                last_words <- last_two_words()
                if (length(last_words) >= 2) {
                        generate_predictions(last_words)
                } else {
                        data.frame(w3 = character(0),
                                   probability = numeric(0))
                }
        })
        
        # Render the word cloud
        output$word_cloud <- renderWordcloud2({
                pred_data <- predictions()
                if (nrow(pred_data) > 0) {
                        wordcloud2(
                                data = pred_data,
                                size = 1,
                                minSize = 0
                        )
                }
        })
        
        # Render the caption above predictions table
        output$caption <- renderUI({
                pred_data <- predictions()
                if (nrow(pred_data) > 0) {
                        return(h5("Select to add word"))  # only show caption if predictions exist
                }
        })
        
        # Render the predictions table
        output$predictions_table <- DT::renderDataTable({
                pred_data <- predictions()
                if (nrow(pred_data) > 0) {
                        DT::datatable(
                                pred_data %>%
                                        select(w3),
                                options = list(
                                        pageLength = 5,
                                        searching = FALSE,
                                        paging = FALSE,
                                        info = FALSE,
                                        columnDefs = list(list(
                                                targets = 0,
                                                visible = TRUE
                                        ))  
                                ),
                                rownames = FALSE,
                                colnames = NULL  
                        )
                }
        })

        # Update the input text with the clicked word
        observeEvent(input$predictions_table_rows_selected, {
                selected_row <- input$predictions_table_rows_selected
                if (length(selected_row) > 0) {
                        clicked_word <- predictions()[selected_row, "w3"]
                        updateTextInput(
                                session,
                                "input_text",
                                value = paste(input$input_text, clicked_word)
                        )
                }
        })
}

# Run Shiny App
shinyApp(ui = ui, server = server)