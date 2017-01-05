library(shiny)
library(ggplot2)
library(effsize)
library(data.table)

ui <- fluidPage(
  
  # Application title
  titlePanel("Simulating Effect Size"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput('sample_size',
                  'Sample size:',
                  min=10,
                  max=1000,
                  step=10,
                  value=30),
      sliderInput('true_es',
                  'True Effect Size:',
                  min=0,
                  max=2,
                  step=.1,
                  value=0),
      sliderInput('replications',
                  'Replications:',
                  min=1000,
                  max=10000,
                  step=1000,
                  value=3000),
      actionButton('sim',"Simulate")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type='tabs',
          tabPanel('Effect Size', plotOutput("esHist"), textOutput('esHist_guide')),
          tabPanel('P value', plotOutput('pHist'), textOutput('pHist_guide'))
      )
    )
  )
)

server <- function(input, output) {
  action <- reactiveValues(results=NULL, last_click=-1)
  
  sim.es <- function() {
    g1 <- rnorm(input$sample_size, input$true_es, 1)
    g2 <- rnorm(input$sample_size, 0, 1)
    return(data.frame('es'=cohen.d(g1, g2, paired=FALSE)$estimate,
                      'p'=t.test(g1, g2, paired=FALSE)$p.value))
  }
  
  observe({
    if (input$sim > action$last_click) {
      action$last_click <- input$sim
      withProgress(message='Simulating...', value=0.45, {
        action$results <- rbindlist(replicate(input$replications, sim.es(), simplify=FALSE))
      })
    }
  })
  #plot proportions, not frequency
  output$esHist <- renderPlot({
    results <- action$results
    ggplot() +
      theme_minimal() +
      geom_histogram(aes(x=results$es, y=..count../sum(..count..)), 
               color='darkblue', fill='darkblue', position='identity') +
      geom_vline(xintercept=c(isolate({input$true_es}),
                              sort(results$es)[.975*nrow(results)], 
                              sort(results$es)[.025*nrow(results)]), 
                 size=1.5, color=c('white', 'red', 'red'), linetype=c('solid', 'longdash','longdash')) +
      xlab("Effect size") +
      ylab('Proportion')
  })
  output$esHist_guide <- renderText({
    "The white line represents the true effect size, while the red lines delineate the 95% quantile."
  })
  output$pHist <- renderPlot({
    results <- action$results
    ggplot() +
      theme_minimal() +
      geom_histogram(aes(x=results$p, y=..count../sum(..count..)), breaks=seq(0, 1, by=.01),
                     color='darkred', fill='darkred', position='identity') +
      geom_vline(xintercept=.05, size=1.5, color='lightgray') +
      xlab("P value") +
      ylab('Proportion')
  })
  output$pHist_guide <- renderText({
    "The gray line marks the traditional significance level, p = .05."
  })
}

# Run the application 
shinyApp(ui = ui, server = server)