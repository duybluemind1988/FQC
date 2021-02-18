#### server
library(data.table)
library(tidyverse)
library(lubridate)
library(shiny)
#library(plotly)
library(psych)
library(dygraphs)
library(highcharter)
library(xts)
library(qcc)
library(anytime)
#options(shiny.host = '0.0.0.0')
#options(shiny.port = 4414)

server <- function(input, output, session) {
  
  #Head some rows for select columns
  data_head_df<-reactive({
    req(input$go_read_file)
    fread(input$file_path,fill=TRUE,nrows=100)
  })
  
  output$data_head_DT<-renderDataTable(data_head_df(),
                                       options =list(pageLength = 5)
  )
  observeEvent(data_head_df(),updateSelectInput(session, "product_type_column", choices=names(data_head_df()),selected='V3'))
  #Select top product
  all_product <-reactive({
    req(input$go_product_type)
    #req(input$file_path,input$product_type_column) # wait for select
    top_product<-savefunc2(input$file_path,input$product_type_column)
    product_all <- as.data.frame(table(top_product)) %>%
      arrange(desc(Freq))
    head(product_all,20) # 2 columns: top_product and Freq
  })
  # Bar chart plot top product:
  output$top_product_plot<-renderPlot({all_product() %>%
      ggplot(aes(x= reorder(top_product, -Freq),y=Freq)) +
      geom_bar(stat="identity",fill="steelblue")+
      theme(text = element_text(size=15),axis.text.x=element_text(angle=90))
  })
  
  observeEvent(all_product(),updateSelectInput(session, "product_type_choose", choices=all_product()$top_product))
  observeEvent(data_head_df(),updateSelectInput(session, "parameter_column_name", choices=names(data_head_df()),selected='V65'))
  observeEvent(data_head_df(),updateSelectInput(session, "parameter_column_value", choices=names(data_head_df()),selected='V66'))
  observeEvent(data_head_df(),updateSelectInput(session, "date_column", choices=names(data_head_df()),selected='V6'))
  
  #Data after filter with all date
  data_all_date <-reactive({
    req(input$go_data_analyze)
    #req(input$file_path,input$top_product,input$product_type_column,input$date_column,input$parameter_column_name,input$parameter_column_value)
    dt<-savefunc2(input$file_path,input$product_type_column,input$product_type_choose,input$date_column,input$parameter_column_name,input$parameter_column_value)
    colnames(dt) <- c("product_type", "date","parameter_freq","parameter_value")
    dt<-dt %>%
      mutate( date_trans=mdy_hms(date), # must have mdy_hms for convert date time
              date_filter=as_date(date_trans),
      )
  })
  # Show min max date
  output$min_date_text<-renderText({min(data_all_date()$date_trans)})
  output$max_date_text<-renderText({max(data_all_date()$date_trans)})
  
  # Show head and tail data with all date
  output$data_process_head <- renderTable({
    head(data_all_date(),2)})
  output$data_process_tail <- renderTable({
    tail(data_all_date(),2)})
  
  # Data with only one date
  data_one_date<-reactive({
    req(input$go_data_analyze_date)
    data_all_date() %>%
      select(date,date_trans,date_filter,parameter_freq,parameter_value) %>%
      filter(date_filter==ymd(input$date_choose))
  })
  # show some data one date:
  output$data_one_date_head <- renderTable({
    tail(data_one_date(),2)})
  
  # show descriptive ststistics data one date:
  output$descriptives_stat <- renderTable({
    psych::describe(data_one_date() %>% select(parameter_freq,parameter_value))
  })
  
  # Highcharter chart
  output$highcharter_normal<- renderUI({
    plot_line_high_charter(data_one_date(),input$remove_frequency_chart,input$go_data_analyze_date,input$USL,input$LSL)
  })
  # Filter outlier:
  data_one_date_no_outlier<-reactive({
    req(input$go_filter_outlier)
    select_data<-data_one_date()$parameter_value
    Q1 <- quantile(select_data, .25)
    Q3 <- quantile(select_data, .75)
    IQR <- IQR(select_data)
    k=3
    subset(data_one_date(), select_data> (Q1 - k*IQR) & select_data< (Q3 + k*IQR))
  })
  
  data_all_date_no_outlier<-reactive({
    req(input$go_filter_outlier)
    select_data<-data_all_date()$parameter_value
    Q1 <- quantile(select_data, .25)
    Q3 <- quantile(select_data, .75)
    IQR <- IQR(select_data)
    k=3
    subset(data_all_date(), select_data> (Q1 - k*IQR) & select_data< (Q3 + k*IQR))
  })
  # show descriptive ststistics data one date filter outlier:
  output$descriptives_stat_remove_outlier <- renderTable({
    psych::describe(data_one_date_no_outlier() %>% select(parameter_freq,parameter_value))
  })
  
  # High charter chart after filter outlier:
  output$highcharter_filter_outlier <- renderUI({
    plot_line_high_charter(data_one_date_no_outlier(),input$remove_frequency_chart,input$go_data_analyze_date,input$USL,input$LSL)
  })
  
  # Qcc chart after filter outlier:
  output$qcc_chart<-renderPlot({
    qcc(data_one_date_no_outlier()$parameter_value, type="xbar.one",
        labels=format(data_one_date_no_outlier()$date_trans,"%b-%d-%H"),axes.las = 2,xlab="")
  })
  # Summary qcc chart
  output$qcc_summary <-renderPrint({
    summary(qcc(data_one_date_no_outlier()$parameter_value, type="xbar.one",
                labels=format(data_one_date_no_outlier()$date_trans,"%b-%d-%H"),axes.las = 2,xlab=""))
  })
  
  #------------------Many data investigate--------------
  
  # High charter chart after filter outlier:
  output$highcharter_box_filter_outlier <- renderUI({
    plot_box_high_charter(data_all_date_no_outlier(),input$remove_frequency_chart,input$go_data_analyze_date,input$USL,input$LSL)
  })
  
  #-----------------------------FUNCTION----------------------------
  #High charter function
  plot_line_high_charter <-function(data_one_date,check_input_remove_frequency_chart,
                               check_go_data_analyze_date,USL,LSL){
    req(check_go_data_analyze_date)
    # USL, LSL
    length_data <- length(data_one_date$parameter_value)
    USL_vector<-rep(USL,each=length_data)
    LSL_vector<-rep(LSL,each=length_data)
    # UCL, LCL
    Mean <- mean(data_one_date$parameter_value)
    mR <- mean(abs(diff(data_one_date$parameter_value)))
    Sigma <- mR/1.128
    UCL <- Mean + 3 * Sigma
    LCL <- Mean - 3 * Sigma
    UCL_vector<-rep(UCL,each=length_data)
    LCL_vector<-rep(LCL,each=length_data)
    Mean_vector<-rep(Mean,each=length_data)
    
    res = list()
    res[[1]] <- data_one_date%>%
      hchart(type = "line", hcaes(x = date, y = parameter_value),name='Value') %>% 
      hc_add_series(data=USL_vector,color = "red",name = "USL") %>% 
      hc_add_series(data=LSL_vector,color = "red",name = "LSL") %>% 
      hc_add_series(data=UCL_vector,color = "blue",name = "UCL") %>% 
      hc_add_series(data=LCL_vector,color = "blue",name = "LCL") %>% 
      hc_add_series(data=Mean_vector,color = "blue",name = "Mean")
    if (!check_input_remove_frequency_chart){
      res[[2]] <- data_one_date%>%
        hchart(type = "line", hcaes(x = date, y = parameter_freq))
    }
    # render the dygraphs objects using htmltools
    res <- htmltools::tagList(res)
    #htmltools::browsable(htmltools::tagList(dy_graph))
  }
  
  #High charter function
  plot_box_high_charter <-function(data_all_date,check_input_remove_frequency_chart,
                                    check_go_data_analyze_date,USL,LSL){
    req(check_go_data_analyze_date)
    # USL, LSL
    length_data <- length(data_all_date$parameter_value)
    USL_vector<-rep(USL,each=length_data)
    LSL_vector<-rep(LSL,each=length_data)
    
    data_all_date%>%
      hcboxplot(x = parameter_value, var=date_filter) %>%
      hc_chart(type = "column")%>% 
      hc_add_series(data=USL_vector,color = "red",name = "USL") %>% 
      hc_add_series(data=LSL_vector,color = "red",name = "LSL") 
  }
  
  # Function to data with filter product, date, column (no need to keep)
  savefunc2 <- function(file_path,product_type_column,product_type_choose=NULL,date_column=NULL,
                        parameter_column_name=NULL,parameter_column_value=NULL){
    tryCatch(
      expr    = {
        if ( is.null(product_type_choose)){
          dt2 <<- fread(file_path,fill=TRUE,select=c(product_type_column,date_column,parameter_column_name,parameter_column_value))}
        else{
          dt2 <<- fread(file_path,fill=TRUE,select=c(product_type_column,date_column,parameter_column_name,parameter_column_value)) %>%
            filter(V3 ==product_type_choose)}
        
      },
      warning = function(w){
        cat('Warning: ', w$message, '\n\n');
        n_line2 <- as.numeric(gsub('Stopped early on line (\\d+)\\..*','\\1',w$message))
        if (!is.na(n_line2)) {
          cat('Found ', n_line2,'\n')
          dt2_part1 <- fread(file_path,fill=TRUE, nrows=n_line2-1,select=c(product_type_column,date_column,parameter_column_name,parameter_column_value))
          dt2_part2 <- fread(file_path,fill=TRUE,skip=n_line2,select=c(product_type_column,date_column,parameter_column_name,parameter_column_value))
          if ( is.null(product_type_choose)){
            dt2 <<- rbind(dt2_part1, dt2_part2, fill=T)
          }else{
            dt2 <<- rbind(dt2_part1, dt2_part2, fill=T)%>%
              filter(V3 ==product_type_choose)  
          }
        }
      },
      finally = cat("\nFinished. \n")
    );
  }
}
