#source('dph.R')
#source('sampling.R')
#source('opt.R')
library(binGroup)
library(reshape)
library(ggplot2)
library(gridExtra)

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

            
        })
      

      output$text2 <- renderText({
        # estimate the number of groups
        if(input$type_of_app == 'num_grp_est'){
          est <- nDesign(nmax=input$pop_size/input$num_seed_per_pool,
                         s=input$num_seed_per_pool, 
                         p.hyp =input$true_prop, 
                         delta=abs(input$true_prop - input$thresh),
                         alternative="greater",
                         method=input$type_of_ci,
                         biasrest=0.05)
          paste(
            "Maximal power reached, with group size =", as.character(est$sout), " and ",
            "Power = ", as.character(round(est$powerout, 3)))
        }
      })
      
      output$plot1 <- renderPlot({  
        if(input$type_of_app == 'grp_size_est'){ 
          
          ## --- delta vs. power plot
          delta <- seq(0, .1, by=0.001)
          power <- numeric(length(delta))

          for(l in 1:length(delta)){
            est <- sDesign(n=input$num_seed_pool, 
                           smax=floor(input$pop_size/input$num_seed_pool),
                           p.hyp=input$true_prop,
                           delta=delta[l],
                           alternative="greater",
                           method=input$type_of_ci)
            power[l] <- est$powerout
          }
          power_curve <- data.frame(delta=delta, power=power)
          p <- ggplot(aes(x=delta, y=power), data=power_curve) + geom_line(size=2, linetype=1, alpha=0.5)
          p <- p + xlab("Difference Between True Proportion and Threshold") + ylab("Power")
        
          ## --- delta fixed, group size varies
          
          ns <- seq(2, 2*input$num_seed_pool, by=1)
          power <- numeric(length(ns))
          
          for(l in 1:length(ns)){
            est <- sDesign(n=ns[l], 
                           smax=floor(input$pop_size/input$num_seed_pool),
                           p.hyp=input$true_prop,
                           delta=abs(input$true_prop - input$thresh),
                           alternative="greater",
                           method=input$type_of_ci)
            power[l] <- est$powerout
          }
          power_curve <- data.frame(lot_size=ns, power=power)
          q <- ggplot(aes(x=lot_size, y=power), data=power_curve) + geom_line(size=2, linetype=1, alpha=0.5)
          q <- q + xlab("Seed Lot size") + ylab("Power")
          
          grid.arrange(p,q, ncol=2)
          
          }
      })
      
      output$plot2 <- renderPlot({  
        if(input$type_of_app == 'num_grp_est'){ 
          delta <- seq(0, .1, by=0.001)
          power <- numeric(length(delta))
          
          for(l in 1:length(delta)){
            est <- nDesign(nmax=input$pop_size/input$num_seed_per_pool,
                           s=input$num_seed_per_pool, 
                           p.hyp =input$true_prop, 
                           delta=delta[l],
                           alternative="greater",
                           method=input$type_of_ci,
                           biasrest=0.05)
            power[l] <- est$powerout
          }
          power_curve <- data.frame(delta=delta, power=power)
          p <- ggplot(aes(x=delta, y=power), data=power_curve) + geom_line(size=2, linetype=1, alpha=0.5)
          p + xlab("Difference Between True Proportion and Threshold") + ylab("Power")
        }
      })
      
    
})
