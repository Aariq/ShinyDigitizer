library(shiny)
library(readbitmap)

ui <- fluidPage(
  fileInput("filename", "Choose a plot"),
  actionButton("go", "Begin!"),
  actionButton("cal_done", "Calibrate!"),
  plotOutput("image", click = "plot_click", brush = "area")
)

server <- function(input, output) {
  #load the image only after clicking the button
  img1 <- eventReactive(input$go, {
    read.bitmap(input$filename$datapath)
  })
  
  #Initialize clicks
  click_saved <- reactiveValues(singleclick = NULL, x = NULL, y = NULL)
  
  #Save a single click
  observeEvent(input$plot_click, { click_saved$singleclick <- input$plot_click })
  
  #Save multiple clicks
  observeEvent(input$plot_click, {
    click_saved$x <- c(click_saved$x, input$plot_click$x)
    click_saved$y <- c(click_saved$y, input$plot_click$y)
  })
  observeEvent(input$go, {
    output$image <- renderPlot({
      #plot the uploaded image
      plot.new()
      rasterImage(img1(), 0, 0, 1, 1)

      #plot a clicked point
      points(click_saved$singleclick$x, click_saved$singleclick$y, col = "red")
      
      #continuously plot clicked points
      points(click_saved$x, click_saved$y, col = "blue")
      
    }, width = dim(img1())[2], height = dim(img1())[1])
    })

}

shinyApp(ui = ui, server = server)

