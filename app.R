# Load necessary libraries for Shiny and Random Forest
library(shiny)
library(randomForest)

# Load dataset
past_defaults <-read.csv("https://raw.githubusercontent.com/DataGuy-Kariuki/Credit-Risk-Modelling/refs/heads/main/past_defaults.csv")

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
            
            # Add Gender input: Male, Female
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
        # Create a new data frame based on user inputs, including Gender
        new_data <- data.frame(
            Age = input$age, 
            Income = input$income,
            Loan_Amount = input$loan_amount, 
            Credit_Score = input$credit_score,
            Gender = input$gender  # Adding Gender to the data frame
        )
        
        # Predict the default risk using the loaded Random Forest model
        prediction <- predict(rf_model, new_data, type = "prob")
        
        # Display the result
        output$result <- renderText({
            paste("Predicted Default Risk:", round(prediction[2] * 100, 2), "%")
        })
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
