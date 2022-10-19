library(shiny)
library(shinyFeedback)


mydata = read.csv("cleaned_data.csv")

lm.model = lm(BODYFAT ~ ABDOMEN+WEIGHT, data=mydata)

ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    titlePanel("BodyFat Prediction"),
    
    sidebarLayout(
        sidebarPanel("This Shinyapp is designed to predict BodyFat (%) with your weight and abdomen. 
        You can input your Abdomen (cm) and weight(lbs) and get your estimated BodyFat and the corresponding confidence interval."),
        mainPanel("If you have any questions, please contact us by czhou255@wisc.edu")
    ),
    
    numericInput("weight", "Enter your weight (lbs)", 150),
    helpText("Weight between 100-300(lbs)"),
    numericInput("abdomen", "Enter your abdomen (cm)", 85),
    helpText("ABDOMEN between 60-150(cm)"),
    
    textOutput("bodyfat")
    
)


server = function(input, output){
    
    sample = reactive({
        weight_range = (input$weight <= 300 & input$weight > 100)
        shinyFeedback::feedbackWarning(
            "weight", 
            !weight_range,
            "Please input weight between 100~300lbs, the current estimate is not convincing."
        )
        abdomen_range = (input$abdomen <= 150 & input$abdomen > 60)
        shinyFeedback::feedbackWarning(
            "abdomen", 
            !abdomen_range,
            "Please input abdomen between 60~150cm, the current estimate is not convincing."
        )
        
        predict(lm.model, newdata=data.frame(ABDOMEN=input$abdomen, WEIGHT=input$weight), interval="predict", level=0.95)
    })

    output$bodyfat = renderText({
        
        if (is.na(input$abdomen) | is.na(input$weight)) {
            paste0("Please input appropriate abdomen(cm) and weight(lbs) so that your bodyfat can be estimated.")
        }
        else if (input$abdomen <= 0 & input$weight > 0) {
            paste0("Nonpositive abdomen is impossible, please input appropriate abdomen.")
            
        }
        else if (input$weight <= 0 & input$abdomen > 0) {
            paste0("Nonpositive weight is impossible, please input appropriate weight.")
            
        }
        else if (input$weight <= 0 & input$abdomen <= 0) {
            paste0("Nonpositive weight and nonpositive abdomen are impossible, please input appropriate values.")
            
        }
        else{
            paste0("Your bodyfat(%) is ", round(sample()[1],2) ,
                   ", and the 95% confidence interval is [", round(sample()[2],2), ",", round(sample()[3],2), "]")
        }
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
