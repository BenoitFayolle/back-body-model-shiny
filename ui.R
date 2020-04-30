
shinyUI(fluidPage(
    tabsetPanel(
        tabPanel(title = "Patch & Prediction",
            sidebarLayout(
                sidebarPanel(fileInput("jpg_name", "Choose a picture", 
                                       multiple = FALSE, accept = NULL,
                                       width = NULL, buttonLabel = "Browse...",
                                       placeholder = "No file selected"),
                             textOutput("prediction"),
                             plotOutput("raw_patches",height="700px"),
                             passwordInput("input_pw", label="Enter password", value = "", width = NULL,
                                           placeholder = NULL)
                            ),
            mainPanel(plotOutput("original_image")))
        ),
        tabPanel(title = "Convolution task",
                 plotOutput("convolution_plot",height = "800px"),
        ),
        tabPanel(title = "Contours & Linear fit",
                 sidebarLayout(
                 sidebarPanel(sliderInput("fit_rsquare","R² \ncoefficient of goodness\n threshold",
                                          0,1,value=0.85,step=0.01),width_percent=0.3),
                 mainPanel(plotOutput("line_plot",height = "800px")))
        ),
        
        tabPanel(title = "Treatment",
                 plotOutput("processing_plot",height="800px"))
    )
))
