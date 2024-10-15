# Load necessary libraries for Shiny and Random Forest
library(shiny)
library(randomForest)

# Set working directory (optional, but ensure this path is correct)
setwd("C:/Users/XaviourAluku.BERRY/Documents/CREDITAPP/CREDITAPP101")

# Load the saved Random Forest model
rf_model <- readRDS("rf_model.rds")

# Define the User Interface (UI)
ui <- fluidPage(
    titlePanel("Credit Risk Prediction"),
    sidebarLayout(
        sidebarPanel(
            numericInput("age", "Age:", value = 30, min = 18, max = 80),
            numericInput("income", "Annual Income:", value = 50000, min = 10000),
            numericInput("loan_amount", "Loan Amount:", value = 10000, min = 1000),
            numericInput("credit_score", "Credit Score:", value = 700, min = 300, max = 850),
            selectInput("gender", "Gender:", choices = c("Male", "Female")),
            actionButton("predict", "Predict Risk")
        ),
        mainPanel(
            textOutput("result")
        )
    )
)

# Define the Server Logic
server <- function(input, output) {
    observeEvent(input$predict, {
        # Create a new data frame based on user inputs
        new_data <- data.frame(
            Age = input$age, 
            Income = input$income,
            Loan_Amount = input$loan_amount, 
            Credit_Score = input$credit_score,
            Gender = factor(input$gender, levels = c("Male", "Female"))  # Ensure factor levels match training data
        )
        
        # Check for NA values in new_data
        if (any(is.na(new_data))) {
            output$result <- renderText("Please provide valid inputs.")
            return()
        }
        
        # Predict the default risk using the loaded Random Forest model
        prediction <- tryCatch({
            predict(rf_model, new_data, type = "prob")
        }, error = function(e) {
            output$result <- renderText(paste("Prediction error:", e$message))
            return(NULL)
        })
        
        # Display the result if prediction was successful
        if (!is.null(prediction)) {
            output$result <- renderText({
                paste("Predicted Default Risk:", round(prediction[2] * 100, 2), "%")
            })
        }
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
