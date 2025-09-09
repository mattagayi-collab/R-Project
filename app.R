#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Calling of libraries
library(shiny)
library(ggplot2)
library(dplyr)
#loading the nykaa dataset
nykaa <- read.csv("Nykaa_Product_Review.csv", stringsAsFactors = FALSE)

#creation of the ShinyApp user interface
ui <- fluidPage(
  #creating main title for the page
  titlePanel("Analyzing different types of products and its related factors of the Cosmetics Products from Nykaa"),
  #creating a sidebar layout
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Product Price", 0, 5000, c(250, 500), pre = "INR"), #creating slider filter for the product price
      uiOutput("ratingOutput"), #creating dropdown for the ratings available for the products
      uiOutput("retailerOutput") #creating dropdown menu for the available retailer names
    ),
    #creating main panel layout
    mainPanel(
      plotOutput("plot"), #Plotting of histogram of filtered values
      br(), br(),
      tableOutput("results") #output of the table after applying data filter
    )
  )
)
#creation of the server side functionalities of the ShinyApp
server <- function(input, output) {
  #Taking input of the selected value of the Product rating dropdown
  output$ratingOutput <- renderUI({
    selectInput("ratingInput", "Product Rating",
                sort(unique(nykaa$Product.Rating)),
                selected = "5")
  }) 
  #Taking input of the selected value of the Product retailer dropdown
  output$retailerOutput <- renderUI({
    selectInput("retailerInput", "Product Retailer",
                sort(unique(nykaa$Retailer)),
                selected = "")
  }) 
  
  #filtering the data based on the rating input
  filtered <- reactive({
    if (is.null(input$ratingInput)) {
      return(NULL)
    }    
    #filering the entire dataset based on the price, retailer and rating
    nykaa %>%
      filter(Product.Price >= input$priceInput[1],
             Product.Price <= input$priceInput[2],
             Retailer == input$retailerInput,
             Product.Rating == input$ratingInput
      )
  })
  #creation of the histogram plot with the product review count
  output$plot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Product.Reviews.Count)) +
      geom_histogram()
  })
  #creating the table with the filtered data
  output$results <- renderTable({
    filtered()
  })
}
#running of the ShinyApp
shinyApp(ui = ui, server = server)
