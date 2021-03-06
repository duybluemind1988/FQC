#### server
library(data.table)
library(tidyverse)
library(lubridate)
library(shiny)
#library(plotly)
library(psych)
library(dygraphs)
library(xts)
library(qcc)

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
  output$min_date_text<-renderText({min(data_all_date()$date)})
  output$max_date_text<-renderText({max(data_all_date()$date)})
  
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
  
  # Dygraph chart
  output$dygraph_normal<- renderUI({
    plot_dygraph(data_one_date(),input$remove_frequency_chart,input$go_data_analyze_date)
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
  # show descriptive ststistics data one date filter outlier:
  output$descriptives_stat_remove_outlier <- renderTable({
    psych::describe(data_one_date_no_outlier() %>% select(parameter_freq,parameter_value))
  })
  # Dygraph chart after filter outlier:
  output$dygraph_filter_outlier <- renderUI({
    plot_dygraph(data_one_date_no_outlier(),input$remove_frequency_chart,input$go_data_analyze_date)
  })
  
  # Qcc chart after filter outlier:
  output$qcc_chart<-renderPlot({
    qcc(data_one_date_no_outlier()$parameter_value, type="xbar.one", std.dev = "SD",
        labels=format(data_one_date_no_outlier()$date_trans,"%b-%d-%H"),axes.las = 2,xlab="")
  })
  # Summary qcc chart
  output$qcc_summary <-renderPrint({
    summary(qcc(data_one_date_no_outlier()$parameter_value, type="xbar.one", std.dev = "SD",
                labels=format(data_one_date_no_outlier()$date_trans,"%b-%d-%H"),axes.las = 2,xlab=""))
  })
  
  #dygraphs function 2
  plot_dygraph <-function(data_one_date,check_input_remove_frequency_chart,
                          check_go_data_analyze_date){
    req(check_go_data_analyze_date)
    res = list()
    res[[1]] <- dygraph(data_one_date %>% select(date_trans,parameter_value), main = "db", group = "lung-deaths")%>%
      dyOptions( drawPoints = TRUE, pointSize = 4,useDataTimezone = TRUE )%>% dyRangeSelector()
    if (!check_input_remove_frequency_chart){
      res[[2]] <- dygraph(data_one_date %>% select(date_trans,parameter_freq), main = "Frequency", group = "lung-deaths")%>%
        dyOptions( drawPoints = TRUE, pointSize = 4,useDataTimezone = TRUE )%>% dyRangeSelector()
    }
    # render the dygraphs objects using htmltools
    res <- htmltools::tagList(res)
    #htmltools::browsable(htmltools::tagList(dy_graph))
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