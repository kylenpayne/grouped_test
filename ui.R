
### ----- ui.R

library(shiny)
library(magrittr)

# Define UI for the seed purity app


shinyUI(fluidPage(
  
  #Application title
  titlePanel("Seed Purity Group Testing Application"),
  
  # ------- sidebar with various input and options
  sidebarLayout(
    
    
    # ------- the siderbar panel
    sidebarPanel(
      
      # ------- checkbox for sample size est, confidence interval est,
      
      selectInput("type_of_app", label="Application Type",
                  choices= c("Group Size Estimation" = "samp_size_est" ,
                              "Confidence Interval Estimation" = "confint_est")),
      
      # ------- create a conditional panel
      conditionalPanel(
        condition = "input.type_of_app == 'samp_size_est'",
        
        # --------- Enter in values for the sample Size Estimation
        
        
        numericInput("num_seed_pool", "Number of Seed Pools:",min=1, max=1000, value=1),
        # this is what we are estimating
        # numericInput("num_seed_per_pool", "Number of Seeds per Pool:", min=1, max=10000, value=1),
        
        # if population size is NA, then we know that the value wasn't entered
        # and we know to use the infinite population assumption and the binomial
        # model. Otherwise, we use the pooled hypergeometric distribution
        
        
        numericInput("pop_size", "Population Size:", min=1, max=100000, value=NA),
        numericInput("num_dev_pools", "Number Of Deviant Pools", min=1, max=1000, value=NA),
        sliderInput("thresh", "Threshold", min=0, max=1, value=1),
        sliderInput("true_prop", "True Proportion of Deviant Pools", min=0, max=1, value=0.5),
        sliderInput("pwr", "Desired Power", min=0, max=1, value=0.01),
        selectInput("type_of_ci", "Type of Confidence Interval",
                    choices=c("Clopper-Pearson" = "CP", 
                              "Agresti-Coull" = "AC",
                              "Blaker" = "Blaker",
                              "Score" = "Score",
                              "Second Order Corrected" = "SOC"))
      ),
      
     
      
      
      # -------- start of confidence interval estimator
      
      
      
      # ------- create a conditional panel
      
      conditionalPanel(
        condition = "input.type_of_app == 'confint_est'",
        
        # ------ Enter in values for confidence interval estimation
        numericInput("num_seed_pool", "Number of Seed Pools:",min=1, max=1000, value=1),
        numericInput("num_seed_per_pool", "Number of Seeds per Pool:", min=1, max=10000, value=1),
        numericInput("pop_size", "Population Size:", min=1, max=100000, value=1),
        numericInput("num_dev_pools", "Number Of Deviant Pools", min=1, max=1000, value=1),
        numericInput("confint_level", "Confidence Level", min=0, max=1, value=.95),
        selectInput("hypoth", "Hypothesis",
                    choices=c("Two-Sided" = "two.sided",
                              "Greater" = "greater",
                              "Lesser" = "lesser")),
        selectInput("type_of_ci", "Type of Confidence Interval",
                    choices=c("Wald" = "wald", 
                              "Agresti-Coull" = "AC",
                              "Clopper Pearson" = "CP"))
        
      )
      
 
    ),
    # ------- Main panel 
    # this is where the results will display for each select of the options in the sidebar :)  
    
    mainPanel(
      h1("Shiny app"),
      
      textOutput("text1")
    )
    
  )
 )
)
        
        