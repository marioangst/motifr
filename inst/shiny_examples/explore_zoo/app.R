library(shiny)
library(motifr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Explore the motif zoo"),

    # Sidebar with UI
    sidebarLayout(
        sidebarPanel(
            uiOutput("directionControl"),
            uiOutput("n_levelsControl"),
            uiOutput("signatureControl"),
            uiOutput("classControl"),
            radioButtons("show_label","Show labels?",c(TRUE,FALSE), selected = FALSE)

        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2(textOutput("motif_text")),
           plotOutput("motif_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$directionControl <- renderUI({
        if(!is.null(net)){
            if (network::is.network(net)) {
                detected_direction <- ifelse(network::is.directed(net),
                                             "Directed","Undirected")
            }
            if (igraph::is.igraph(net)) {
                detected_direction <- ifelse(igraph::is.directed(net),
                                             "Directed","Undirected")
            }
            text_prompt <- paste("Type of network.
                                             Hint: The currently supplied network seems to be ",
                                 detected_direction)
        }
        else{
            text_prompt <- "Type of network"
        }
        shiny::radioButtons(inputId = "directed",label = text_prompt,
                            choices = c("Directed","Undirected"), selected = "Undirected", )
    })

    direction <- reactive({
        if(input$directed == "Directed"){
            return(TRUE)
        }
        else{
            return(FALSE)
        }
    })

    n_levels_set <- reactive({
        input$n_levels
    })

    output$n_levelsControl <- renderUI({
        if(!is.null(net)){
            if (network::is.network(net)) {
                detected_levels <- length(
                    unique(network::get.vertex.attribute(net, attrname = lvl_attr)))
            }
            if (igraph::is.igraph(net)) {
                detected_levels <- length(
                    unique(igraph::get.vertex.attribute(net, name = lvl_attr)))
            }
            text_prompt <- paste("Number of levels in network.
                                             Hint: The currently supplied network seems to have ",
                                 detected_levels," levels, based on the attribute ",lvl_attr)
        }
        else{
            text_prompt <- "Number of levels in network"
        }

        shiny::numericInput("n_levels", text_prompt,
                            value = 2,
                            min = 1,
                            max = max(supported_signatures()$n_levels))
    })

    output$classControl <- renderUI({
        available_classes <- supported_classes(input$signature,
                                               directed = direction())
        selectInput("class", label = "Motif class",
                    choices = available_classes,
                    multiple = FALSE)
    })

    output$signatureControl <- renderUI({
        available_signatures <- supported_signatures()[supported_signatures()$directed == direction() &
                                                           supported_signatures()$n_levels <= n_levels_set(),
                                                       "signature"]
        shiny::selectInput(inputId = "signature",label = "Signature",
                           choices = available_signatures,
                           multiple = FALSE)
    })

    output$motif_plot <- renderPlot({
            show_motif(paste(input$signature,"[",input$class,"]",sep = ""),
                       nodesize = 7,
                       net = net,
                       lvl_attr = lvl_attr,
                       label = input$show_label)
    })

    output$motif_text <- renderText({
        paste(input$signature,"[",input$class,"]",sep = "")
        })
}

# # Run the application (do not because called from external)
# shinyApp(ui = ui, server = server)
