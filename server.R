#source('dph.R')
#source('sampling.R')
#source('opt.R')
library(binGroup)

shinyServer(function(input,output){
  ## ---- input


  #  if(input$pop_size >= 5000 || is.na(input$pop_size)){
      # make sure the pop size is no smaller than 5000
      # pop_size <- max(input$pop_size, 5000)
      
      # estimate the group size
        output$text1 <- renderText({
          if(input$type_of_app == 'grp_size_est'){ 
          #paste("this is a test", input$true_prop)
          est <- sDesign(n=input$num_seed_pool, 
                  smax=floor(input$pop_size/input$num_seed_pool),
                  p.hyp=input$true_prop,
                  delta=abs(input$true_prop - input$thresh),
                  alternative="greater",
                  method=input$type_of_ci)
          paste(
            "maximal power reached, with group size =", as.character(est$sout), " and ",
            "Power = ", as.character(round(est$powerout, 3)))
          }
      # estimate the number of groups
          if(input$type_of_app == 'num_grp_est'){
            est <- nDesign(nmax=input$pop_size/input$num_seed_per_pool,
                           s=input$num_seed_per_pool, 
                           p.hyp =input$true_prop, 
                           delta=abs(input$true_prop - input$thresh),
                           alternative="greater",
                           method=input$type_of_ci,
                           biasrest=0.05)
          }
        
          })
      

})
