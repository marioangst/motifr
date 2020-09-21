library(shiny)
library(motifr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Explore the motif zoo"),

    # Sidebar with UI
    sidebarLayout(
        sidebarPanel(
            shiny::radioButtons(inputId = "directed",label = "Directed",choices = c(TRUE,FALSE), selected = FALSE),
            uiOutput("signatureControl"),
            uiOutput("classControl"),

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("motif")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$classControl <- renderUI({
        available_classes <- supported_classes(input$signature)
        selectInput("class", label = "Motif class", choices = available_classes, multiple = FALSE)
    })

    output$signatureControl <- renderUI({
        available_signatures <- supported_signatures()[supported_signatures()$directed == input$directed,"signature"]
        shiny::selectInput(inputId = "signature",label = "Signature",
                           choices = available_signatures,
                           multiple = FALSE)
    })

    output$motif <- renderPlot({
            show_motif(paste(input$signature,"[",input$class,"]",sep = ""),
                       nodesize = 7)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
