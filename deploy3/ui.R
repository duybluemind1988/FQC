library(plotly)
library(highcharter)
#### user interface
#path<-'/mnt/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_3000_head.dat' # 25 MB OK
#path<-'/mnt/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_3000_tail.dat' # 25 MB OK
#path<-'/mnt/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_4000_tail.dat' # 0.2 GB OK
#path<-'/mnt/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_3000.dat' # 3.6GB OK
#path<-'/mnt/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_E_series.dat' # 2.9 GB crack session (ram?)
#path<-'/mnt/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_Ric.dat' # 2.8 GB OK
#path<-'/mnt/01D6B57CFBE4DB20/1.Linux/Data/FQC/V04R-V04R-SQLData_4000.dat' # 0.8 GB OK

#path test from windown:
#path<-'//Vn01w2k16v18/data/Copyroom/Test_software/Data/V04R-V04R-SQLData_3000_head.dat'
path<-'//Vn01w2k16v18/data/Copyroom/Test_software/Data/V04R-V04R-SQLData_4000_tail.dat'

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
      actionButton("go_data_analyze", "Read data with above parameter (wait ~ 1 min)",style="color: #fff; background-color: #337ab7;"),
      #actionButton("delete", "delete data to free ram"),
      #tableOutput('all_data_memory'),
    ), #endsidebarpanel
    
    mainPanel(
      h3("1. First some rows of data:"),
      dataTableOutput("data_head_DT"),
      h3("2. Top 20 products in this file:"),
      plotOutput("top_product_plot"),
      #------------------------------
      h3("3. Data all day information:"),
      "Data all date head (m/d/y):",
      tableOutput("data_process_head"),
      "Data all date tail (m/d/y):",
      tableOutput("data_process_tail"),
      #"Date one date filter head:",
      #tableOutput("data_one_date_head"),
      
      #-----------Data analyze all day-------------
      h3("4. Data analysis all day"),
      numericInput("USL","Please input USL",value=96),
      numericInput("LSL","Please input LSL",value=90),
      "Please choose date range for data analyze: ",
      dateInput("date_choose_start", "Date (y-m-d):", value = "2020-06-20"),
      dateInput("date_choose_end", "Date (y-m-d):", value = "2020-08-15"),
      
      checkboxInput("remove_frequency_chart", "Remove_frequency_chart", value = TRUE),
      actionButton("go_filter_outlier", "Filter outlier",style="color: #fff; background-color: #337ab7;"),
      actionButton("go_data_analyze_all_date", "Analyze data all day",style="color: #fff; background-color: #337ab7;"),
      
      htmlOutput("highcharter_box_filter_outlier"),
      #plotlyOutput("plotly_box_filter_outlier"),
      tableOutput("descriptives_stat_all_date"),
      "Line chart:",
      #plotlyOutput("plotly_line_filter_outlier"),
      highchartOutput("highcharter_line_filter_outlier"),
      "QCC chart:",
      plotOutput("all_day_qcc_chart"),
      "QCC summary:",
      #verbatimTextOutput("all_day_qcc_summary"),
      
      #---------Data analyze one day-------------
      h3("5. Data analysis one day"),
      "Please choose one date for data analyze: ",
      dateInput("date_choose", "Date (y-m-d):", value = "2020-06-20"),
      "Min and Max date:",
      textOutput('min_date_text'),
      textOutput('max_date_text'),
      actionButton("go_data_analyze_one_date", "Analyze data one day",style="color: #fff; background-color: #337ab7;"),
      #h4("Descriptives statistics:"),
      #tableOutput("descriptives_stat"),
      #htmlOutput("dygraph_normal"), # Not used anymore
      #htmlOutput("highcharter_normal"),
      
      h4("Descriptives statistics remove outlier:"),
      tableOutput("descriptives_stat_remove_outlier"),
      "Filter outlier for smooth chart:",
      #htmlOutput("dygraph_filter_outlier"),
      htmlOutput("highcharter_filter_outlier"),
      "QCC chart:",
      plotOutput("qcc_chart"),
      "QCC summary:",
      verbatimTextOutput("qcc_summary"),
      
      
      
    )#end mainpanel
  )# end sidebarlayout
)