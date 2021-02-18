#shinyApp(ui = ui, server = server)
#getwd()

# Option 1:
#setwd("/home/ad/Data_science/R_studio/Git/FQC/deploy/")
setwd("/home/dnn/Data_science/Git/Factory/FQC/deploy3/")
#setwd("C:/Users/DNN/Data_science/Git/FQC/deploy/")
options(shiny.host = '0.0.0.0')
options(shiny.port = 4414)

# Option 2:
#shinyApp(ui = ui, server = server)
shiny::runApp(host="0.0.0.0",port=4414)

