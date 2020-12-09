path<-'/media/ad/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_3000_head.dat' # 25 MB OK
#path<-'/media/ad/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_3000_tail.dat' # 25 MB OK
#path<-'/media/ad/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_3000.dat' # 3.6GB OK
#path<-'/media/ad/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_E_series.dat' # 2.9 GB crack session (ram?)
#path<-'/media/ad/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_Ric.dat' # 2.8 GB OK

#path test from windown:
#path<-'//Vn01w2k16v18/data/Copyroom/Test_software/Data/V04R-V04R-SQLData_3000_head.dat'

ui <- fluidPage(

  titlePanel("FQC DATA ANALYTIC"),
  h5("Created by: DNN - Version: 1.0"),
  sidebarLayout(

    sidebarPanel(
      "Sidebar DNN",
      textInput("file_path", h3("File input"),
                value =path ),
      actionButton("go_read_file", "Read file",style="color: #fff; background-color: #337ab7;"),
      selectInput("product_type_column", "Choose product type column", choices=NULL, selected=NULL),
      actionButton("go_product_type", "Show product type (wait ~ 1 min)",style="color: #fff; background-color: #337ab7;"),
      selectInput("product_type_choose", "Choose product", choices=NULL, selected=NULL),
      selectInput("parameter_column_name", "Choose parameter column name", choices=NULL, selected=NULL),
      selectInput("parameter_column_value", "Choose parameter column value", choices=NULL, selected=NULL),
      selectInput("date_column", "Choose date column", choices=NULL, selected=NULL),
      actionButton("go_data_analyze", "Process Data (wait ~ 1 min)",style="color: #fff; background-color: #337ab7;"),
      dateInput("date_choose", "Date (y-m-d):", value = "2017-12-22"),
      "Min and Max date:",
      textOutput('min_date_text'),
      textOutput('max_date_text'),
      actionButton("go_data_analyze_date", "Analyze data and draw chart",style="color: #fff; background-color: #337ab7;"),
      actionButton("go_filter_outlier", "Filter outlier",style="color: #fff; background-color: #337ab7;"),
      #actionButton("delete", "delete data to free ram"),
      #tableOutput('all_data_memory'),
    ), #endsidebarpanel

    mainPanel(
      "First some rows of data:",
      dataTableOutput("data_head_DT"),
      "Top 20 products in this file:",
      plotOutput("top_product_plot"),
      "Data all date head (m/d/y):",
      tableOutput("data_process_head"),
      "Data all date tail (m/d/y):",
      tableOutput("data_process_tail"),
      #"Date one date filter head:",
      #tableOutput("data_one_date_head"),
      "Descriptives statistics:",
      tableOutput("descriptives_stat"),
      checkboxInput("remove_frequency_chart", "Remove_frequency_chart", value = TRUE),
      htmlOutput("dygraph_normal"),
      "Descriptives statistics remove outlier:",
      tableOutput("descriptives_stat_remove_outlier"),
      "Filter outlier for smooth chart:",
      htmlOutput("dygraph_filter_outlier")


    )#end mainpanel
  )# end sidebarlayout
)