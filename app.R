#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Trajectories in a Triangle (Mathematical Billiards)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("a1",
                  "Angle 1:",
                  min = 0,
                  max = 90,
                  value = 30,
                  step = 1,
                  animate = animationOptions(interval = 100)),
      
      sliderInput("a2",
                  "Angle 2:",
                  min = 0,
                  max = 90,
                  value = 30,
                  step = 1,
                  animate = animationOptions(interval = 100)),
      
      sliderInput("xb",
                  "Initial Position on Base:",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = 0.005,
                  animate = animationOptions(interval = 100)),
      
      sliderInput("a",
                  "Initial Firing Angle:",
                  min = 0,
                  max = 180,
                  value = 30,
                  step = 1,
                  animate = animationOptions(interval = 100)),
      
      sliderInput("bounces",
                  "Number of Bounces:",
                  min = 0,
                  max = 500,
                  value = 10,
                  step = 10,
                  animate = animationOptions(interval = 100))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("periodicity")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    a1 = pi/180 * input$a1
    a2 = pi/180 * input$a2
    
    xL = -1
    yL = 0
    
    xR = 1
    yR = 0
    
    xT = (tan(a2) - tan(a1)) / (tan(a2) + tan(a1))
    yT = tan(a2) - xT*tan(a2)
    
    plot(c(xL,xT,xR), c(yL,yT,yR), xlab = 'x-axis', ylab = 'y-axis', asp = 1, pch = 20)
    segments(xL,yL, xT,yT, col = 'black', lwd = 2)
    segments(xT,yT, xR,yR, col = 'black', lwd = 2)
    segments(xR,yR, xL,yL, col = 'black', lwd = 2)
    
    xb = input$xb
    yb = 0
    bounces = input$bounces
    a = pi/180 * input$a
    dig = 10
    line = 0
    
    periodicity = "none"
    for (i in c(1:bounces)) {
      
      if (line == 0) {
        
        tempx = (tan(a1) + xb[i]*tan(a) - yb[i]) / (tan(a) - tan(a1))
        tempy = tempx*tan(a1) + tan(a1)
        if (tempx >= xL && tempx <= xR && tempy >= yL && tempy <= yT) {
          
          xb = c(xb, tempx)
          yb = c(yb, tempy)
          a = 2*a1 - a
          line = 1
          
        } else {
          
          xb = c(xb, (tan(a2) + xb[i]*tan(a) - yb[i]) / (tan(a) + tan(a2)))
          yb = c(yb, -xb[i+1]*tan(a2) + tan(a2))
          a = pi - a - 2*a2
          line = 2
        }
        
      } else {
        
        if (line == 1) {
          
          tempx = (xb[i]*tan(a) - yb[i]) / tan(a)
          tempy = 0
          if (tempx >= xL && tempx <= xR && tempy >= yL && tempy <= yT)  {
            
            xb = c(xb, tempx)
            yb = c(yb, tempy)
            a = pi - a
            line = 0
            
          } else {
            
            xb = c(xb, (tan(a2) + xb[i]*tan(a) - yb[i]) / (tan(a) + tan(a2)))
            yb = c(yb, -xb[i+1]*tan(a2) + tan(a2))
            a = pi - a - 2*a2
            line = 2
          }
          
        } else {
          
          tempx = (tan(a1) + xb[i]*tan(a) - yb[i]) / (tan(a) - tan(a1))
          tempy = tempx*tan(a1) + tan(a1)
          if (tempx >= xL && tempx <= xR && tempy >= yL && tempy <= yT) {
            
            xb = c(xb, tempx)
            yb = c(yb, tempy)
            a = 2*a1 - a
            line = 1
            
          } else {
            
            xb = c(xb, (xb[i]*tan(a) - yb[i]) / tan(a))
            yb = c(yb, 0)
            a = pi - a
            line = 0
            
          }
        }
        
        
      }
      
      #periodicity calculation - REMOVE if you want cleaner animations
      segments(xb[i],yb[i], xb[i+1],yb[i+1], col = 'red', lwd = 1)
      if (round(xb[i+1], digits = dig) == round(xb[2], digits = dig) && round(yb[i+1], digits = dig) == round(yb[2], digits = dig)
          && round(xb[i], digits = dig) == round(xb[1], digits = dig) && round(yb[i], digits = dig) == round(yb[1], digits = dig)
          && periodicity == "none" && i > 1) {
        periodicity = as.character(i - 1)
      }
      output$periodicity = renderText(c("Periodicity: ", periodicity))
    } 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

