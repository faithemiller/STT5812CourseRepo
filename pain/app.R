#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
powerg <- function(n = 36, delta = 1, sd = 6, alpha = 0.10, alternative = c("two.sided","less","greater"), sig.known = c(TRUE,FALSE)){
  ####BEGIN T-SCORE STUFF#####  
  gamma <- (delta)/(sd/sqrt(n))
  if(sig.known==TRUE){
    dv <- if(gamma < 0){
      ll <- -5*sd/sqrt(n) + gamma
      ul <- 5*sd/sqrt(n)
    } else {
      ll <- -5*sd/sqrt(n)
      ul <- 5*sd/sqrt(n) + gamma
    }
  }
  else {
    dv <- if(gamma < 0){
      ll <- -5*sqrt(n/(n-2)) + gamma
      ul <- 5*sqrt(n/(n-2))
    } else {
      ll <- -5*sqrt(n/(n-2))
      ul <- 5*sqrt(n/(n-2)) + gamma
    }
  }
  p <- ggplot(data = data.frame(x = c(ll, ul)), aes(x = x))
  
  if(sig.known == FALSE){ 
    
    alternative <- match.arg(alternative)
    if(alternative == "less"){
      dt_fun1 <- function(x){
        y <- dt(x, n - 1)
        y[x > qt(alpha, n - 1)] <- NA
        return(y)
      }
      
      dt_fun2 <- function(x){
        y <- dt(x, n - 1, gamma)
        y[x > qt(alpha, n - 1)] <- NA
        return(y)
      }
      POWER <- round(pt(qt(alpha, n - 1), n-1, gamma), 4)
    }
    else if (alternative == "greater"){
      dt_fun1 <- function(x){
        y <- dt(x, n - 1)
        y[x < qt(1 - alpha, n - 1)] <- NA
        return(y)
      }
      
      dt_fun2 <- function(x){
        y <- dt(x, n - 1, gamma)
        y[x < qt(1 - alpha, n - 1)] <- NA
        return(y)
      }
      POWER <- round(pt(qt(1- alpha, n - 1), n-1, gamma, lower.tail = FALSE), 4)
    }
    ####2-TAIL STUFF####
    else{
      dt_fun1 <- function(x){
        y <- dt(x, n - 1)
        y[x > qt(alpha/2, n - 1) & x < qt(1 - alpha/2, n - 1)] <- NA
        return(y)
      }
      
      dt_fun2 <- function(x){
        y <- dt(x, n - 1, gamma)
        y[x > qt(alpha/2, n - 1) & x < qt(1 - alpha /2, n - 1)] <- NA
        return(y)
      } 
      POWER <- round(pt(qt(alpha/2, n-1), n-1, gamma) + pt(qt(1- alpha/2, n - 1), n-1, gamma, lower.tail = FALSE), 4)
    }
    
    p + stat_function(fun = dt_fun1, geom = "area", n = 500, fill = "red", alpha = 0.5) + 
      stat_function(fun = dt_fun2, geom = "area", n = 500, fill = "darkorchid4", alpha = 0.5) + 
      stat_function(fun = dt, args = list(n - 1), n = 500, color = "black") + 
      stat_function(fun = dt, args = list(n - 1, gamma), n = 500, color = "black") + 
      geom_hline(yintercept = 0) + 
      theme_bw() + 
      labs(x = " ", y = " ", title = paste0("Power ","(",POWER,") is the sum of all blue and purple shaded areas"))
  }
  ###BEGIN Z-SCORE STUFF###  
  else{
    
    alternative <- match.arg(alternative)
    if(alternative == "less"){
      dnorm_fun1 <- function(x){
        y <- dnorm(x,0,sd/sqrt(n))
        y[x > qnorm(alpha, 0, sd/sqrt(n),lower.tail = TRUE)] <- NA
        return(y)
      }
      
      dnorm_fun2 <- function(x){
        y <- dnorm(x, gamma, sd/sqrt(n))
        y[x > qnorm(alpha, 0, sd/sqrt(n))] <- NA
        return(y)
      }
      POWER <- round(pnorm(qnorm(alpha, 0, sd/sqrt(n)), gamma, sd/sqrt(n)), 4)
    }
    else if (alternative == "greater"){
      dnorm_fun1 <- function(x){
        y <- dnorm(x,0,sd/sqrt(n))
        y[x < qnorm(1-alpha, 0, sd/sqrt(n),lower.tail = TRUE)] <- NA
        return(y)
      }
      
      dnorm_fun2 <- function(x){
        y <- dnorm(x, gamma, sd/sqrt(n))
        y[x < qnorm(1-alpha, 0, sd/sqrt(n))] <- NA
        return(y)
      }
      POWER <- round(pnorm(qnorm(1-alpha, 0, sd/sqrt(n)), gamma, sd/sqrt(n),lower.tail = FALSE), 4)
    }
    ####2-TAIL STUFF####
    else{
      dnorm_fun1 <- function(x){
        y <- dnorm(x,0, sd/sqrt(n))
        y[x > qnorm(alpha/2, 0, sd/sqrt(n)) & x < qnorm(1 - alpha/2,0,sd/sqrt(n) )] <- NA
        return(y)
      }
      
      dnorm_fun2 <- function(x){
        y <- dnorm(x, gamma, sd/sqrt(n))
        y[x > qnorm(alpha/2, 0,sd/sqrt(n)) & x < qnorm(1 - alpha /2, 0,sd/sqrt(n))] <- NA
        return(y)
      } 
      POWER <- round(pnorm(qnorm(alpha/2, 0,sd/sqrt(n)), gamma, sd/sqrt(n)) + 
                       pnorm(qnorm(1- alpha/2,0,sd/sqrt(n)), gamma, sd/sqrt(n), lower.tail = FALSE), 4)
    }
    
    p + stat_function(fun = dnorm_fun1, geom = "area", n = 500, fill = "red", alpha = 0.5) + 
      stat_function(fun = dnorm_fun2, geom = "area", n = 500, fill = "darkorchid4", alpha = 0.5) + 
      stat_function(fun = dnorm, args = list(0,sd/sqrt(n)), n = 500, color = "black") + 
      stat_function(fun = dnorm, args = list( gamma,sd/sqrt(n)), n = 500, color = "black") + 
      geom_hline(yintercept = 0) + 
      theme_bw() + 
      labs(x = " ", y = " ", title = paste0("Power ","(",POWER,") is the sum of all blue and purple shaded areas"))
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Z/T Power Analyzer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("Delta",
                   "Difference in mean (max=100)",
                   min = -100,
                   max = 100,
                   value = 0
      ),
      numericInput("Samp",
                   "Sample Size (postitive integer between 3 and 10,000)",
                   min = 3,
                   max = 10000,
                   value = 30,
                   step = 5
      ),
      numericInput("std",
                   "Standard Deviation (max=10,000)",
                   min = 0,
                   max = 10000,
                   value = 1
      ),
      radioButtons(inputId = "sigma",
                   label = "Is this the population standard deviation?",
                   choiceValues=c(TRUE,FALSE) ,
                   selected = FALSE,
                   inline=TRUE,
                   choiceNames=c("Yes","No")
      ),
      numericInput("alpha",
                   "Alpha",
                   min = 0,
                   max = 1,
                   value = .05,
                   step = .01
      ),
      selectInput(inputId = "Alt",
                  label = "Alternative Hypothesis",
                  selected = "Left Tailed",
                  choices=c("Left Tailed","Right Tailed","Two Tailed")
      )
    ), 
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  alter<-reactive({switch(input$Alt,
                          "Left Tailed"="less",
                          "Right Tailed" = "greater",
                          "Two Tailed"="two.sided") })
  output$distPlot <- renderPlot({
    powerg(n = input$Samp, delta = input$Delta, sd = input$std, alpha = input$alpha, alternative = alter(), sig.known = input$sigma)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
