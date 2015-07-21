# install shiny apps
# library('devtools')

# install github
# devtools::install_github('rstudio/shinyapps')


shinyapps::setAccountInfo(name='kylenpayne',
                          token='3FD6B729FB733A32DE024741444BF2FB',
                          secret='3RaTsOefYek9iyvo74MyCbtJ97vWLalwWUG/pHla')



#deploy the app
shinyapps::deployApp('/home/nb93358/projects/seed_purity/seed_purity_shiny')
Y

# terminate the app
# shinyapps::terminateApp('seed_purity_shiny')