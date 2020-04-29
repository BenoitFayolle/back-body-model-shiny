#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tabsetPanel(
        tabPanel(title = "Original Image",
            sidebarLayout(
                sidebarPanel(fileInput("jpg_name", "Choose a picture", 
                                       multiple = FALSE, accept = NULL,
                                       width = NULL, buttonLabel = "Browse...",
                                       placeholder = "No file selected"),
                             plotOutput("raw_patches",height="700px"),
                             textOutput("prediction"),
                             passwordInput("input_pw", label="Enter password", value = "gagli", width = NULL,
                                           placeholder = NULL)
                            ),
            mainPanel(plotOutput("original_image")))
        ),
        tabPanel(title = "Convolutions",
                 plotOutput("convolution_plot",height = "800px"),
        ),
        tabPanel(title = "Segments extraction",
                 plotOutput("line_plot",height = "800px"),
        ),
        
        tabPanel(title = "Treatment",
                 plotOutput("processing_plot",height="800px"))
    )
))
