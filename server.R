#source('dph.R')
#source('sampling.R')
#source('opt.R')
library(binGroup)

shinyServer(function(input,output){
  ## ---- input


  #  if(input$pop_size >= 5000 || is.na(input$pop_size)){
      # make sure the pop size is no smaller than 5000
      # pop_size <- max(input$pop_size, 5000)
      
      # estimate the group sizes
      if(input$type_of_ci != "SOC"){
        output$text1 <- renderText({
          if(input$type_of_app == 'samp_size_est'){ 
          #paste("this is a test", input$true_prop)
          est <- sDesign(n=input$num_seed_pool, 
                  smax=floor(input$pop_size/input$num_seed_pool),
                  p.hyp=input$true_prop,
                  delta=abs(input$true_prop - input$thresh),
                  power=input$pwr,
                  alternative="greater",
                  method=input$type_of_ci)
          paste(
            ifelse(est$power.reached == T,
                   "Power was reached in specified range of group sizes ",
                   "Power was not reached in specified range of group sizes "),
            ",maximal power reached with s =", as.character(est$sout), " and ",
            "Power = ", as.character(est$powerout) )
          }
        
        })
      }
      else{
      ### this section is for the sampled pooled hyper-geometric distribution
        
        
        
        
      }

  
})
