
library(shiny)
library(ggplot2)
library(biogrowth)


  ui <- fluidPage(
  
      # Application title
      titlePanel("biogrowth"),
  
      # Sidebar with a slider input for logN0, C, mu, lambda 
      sidebarLayout(
          sidebarPanel(
            uiOutput("dynamicSlider"),
             sliderInput("logN0",
                          "logarithm of the population size at t = 0, Logarithm of the initial population size",
                          min = 0,
                          max = 100,
                          value = 0),
            sliderInput("C",
                        "difference between log10 N0 and the logarithm of the maximum population size in thestationary phase, Log-increase in population size ",
                        min = 1,
                        max = 50,
                        value = 6),
            sliderInput("mu",
                        "slope of the growth curve during the exponential phase, Maximum growth rate",
                        min = 0,
                        max = 1,
                        value = 0.2),
            sliderInput("lambda",
                        "duration of the lag phase",
                        min = 1,
                        max = 100,
                        value = 20)
          ),
  
          # Show a plot of the predicted growth
          mainPanel(
             plotOutput("my_prediction")
          )
      )
  )
  

  server <- function(input, output) {
  
#Creating prediction of growth using predict_growth (example 1)
    prediction <- reactive({ 
      
      primary_model_data()
      
      my_model <- "modGompertz"  # we will use the modified-Gompertz
      
      ## The keys of the model parameters can also be obtained from primary_model_data()
      
      primary_model_data(my_model)$pars
      
      ## We define the primary model as a list, using slider inputs for values
      
      logN0 <- input$logN0
      C <- input$C
      mu <- input$mu
      lambda <- input$lambda
      
      my_model <- list(model = "modGompertz", logN0 = logN0, C = C, mu = mu, lambda = lambda)
      
      ## We can now make the predictions
      
      my_time <- seq(0, 100, length = 1000)  # Vector of time points for the calculations
      
      predict <- predict_growth(my_time, my_model, environment = "constant")
      
      
      })
    
#defining plot of prediction
      output$my_prediction <- renderPlot({
        prediction_data <- prediction()
        ## The instance of IsothermalGrowth includes several S3 methods 
        plot(prediction_data)
        
      })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
