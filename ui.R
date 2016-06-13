library(shiny)
library(shinydashboard)
header <- dashboardHeader(title = "visual R studio")
#//////////// Begain of sidebar ///////////////#---------------- 
sidebar <- dashboardSidebar(
  sidebarMenu(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
#//////////// IMPORT sidebar UI ///////////////#----------------
      menuItem("Data Import", tabName = "DataImport", icon = icon("dashboard")),
      ##menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new", badgeColor = "green"),
#//////////// DATA CLEANSING sidebar UI ///////////////#----------------
      menuItem("Data Cleansing",icon = icon("th-list"),tabName = "DataCleansing",
               
               #//////////// Summarization sidebar UI ///////////////#
          menuItem("Summarization",icon = icon("th-list"),tabName = "Summarization",
                   
                    #//////////// GROUP_BY sidebar UI ///////////////#
                   menuSubItem("Group By",icon = icon("folder-open"), tabName = "GroupBy"),
                    #//////////// Add New Column sidebar UI ///////////////#
                   menuSubItem("Add New Column",icon = icon("folder-open"), tabName = "AddNewColumn"),
                    #//////////// Filter sidebar UI ///////////////#
                   menuSubItem("Filter",icon = icon("folder-open"), tabName = "Filter"),
                    #//////////// Arrange sidebar UI ///////////////#
                   menuSubItem("Arrange",icon = icon("folder-open"), tabName = "Arrange"),
                    #//////////// Select sidebar UI ///////////////#
                   menuSubItem("Select",icon = icon("folder-open"), tabName = "Select"),
                    #//////////// Join sidebar UI ///////////////#
                   menuSubItem("Join",icon = icon("folder-open"), tabName = "Join")),
          
           #//////////// Formating sidebar UI ///////////////#
          menuItem("Formating",icon = icon("th-list"),tabName = "Formating",
                   
                   #//////////// Date Formating sidebar UI ///////////////#           
                   menuSubItem("Date Formating",icon = icon("folder-open"), tabName = "DateFormating"),
                   #//////////// numerical Formating sidebar UI ///////////////#
                   menuSubItem("numerical Formating",icon = icon("folder-open"), tabName = "numericalFormating")
                   )),
#//////////// DATA VISUALAIZATION UI ///////////////#---------------- 

menuItem("Data visualitazion",icon = icon("th-list"),tabName = "Datavisualitazion",
         
         #//////////// Common Charts sidebar UI ///////////////#
         menuItem("Common Charts",icon = icon("th-list"),tabName = "CommonCharts",
                  
                  #//////////// line Chart sidebar UI ///////////////#
                  menuSubItem("line Chart",icon = icon("folder-open"), tabName = "lineChart"),
                  #//////////// Bar Chart sidebar UI ///////////////#
                  menuSubItem("Bar Chart",icon = icon("folder-open"), tabName = "BarChart"),
                  #//////////// Single column Chart sidebar UI ///////////////#
                  menuSubItem("Single column Chart",icon = icon("folder-open"), tabName = "SinglecolumnChart"),
                  #//////////// Stack column Chart sidebar UI ///////////////#
                  menuSubItem("Stack column Chart",icon = icon("folder-open"), tabName = "StackcolumnChart"),
                  #//////////// Dodge column Chart sidebar UI ///////////////#
                  menuSubItem("Dodge column Chart",icon = icon("folder-open"), tabName = "DodgecolumnChart"),
                  #//////////// Pie Chart sidebar UI ///////////////#
                  menuSubItem("Pie Chart",icon = icon("folder-open"), tabName = "PieChart")),
         
         #//////////// Other Charts sidebar UI ///////////////#
         menuItem("Other Charts",icon = icon("th-list"),tabName = "OtherCharts",
                  
                  #//////////// bubble chart sidebar UI ///////////////#           
                  menuSubItem("bubble chart",icon = icon("folder-open"), tabName = "bubblechart"),
                  #//////////// radar chart sidebar UI ///////////////#
                  menuSubItem("radar chart",icon = icon("folder-open"), tabName = "radarchart"),
                  #//////////// time series chart sidebar UI ///////////////#           
                  menuSubItem("time series chart",icon = icon("folder-open"), tabName = "timeserieschart"),
                  #//////////// Scatterplot sidebar UI ///////////////#
                  menuSubItem("Scatterplot",icon = icon("folder-open"), tabName = "Scatterplot"),
                  #//////////// Box plot sidebar UI ///////////////#           
                  menuSubItem("Box plot",icon = icon("folder-open"), tabName = "Box plot")
         ))))
#//////////// End of sidebar ///////////////#---------------- 

#//////////// Begain of Body ///////////////#---------------- 
body <- dashboardBody(
  tabItems(
#//////////// IMPORT UI ///////////////#----------------
    tabItem(tabName = "DataImport",
            fluidRow(
              box(
              title = "Inputs", status = "warning", solidHeader = TRUE,collapsible = TRUE,width = 3,
              fileInput('IMPORT', 'Choose CSV file to upload',
                        accept = c('text/csv',
                                   'text/comma-separated-values',
                                   'text/tab-separated-values',
                                   'text/plain',
                                   '.csv',
                                   '.tsv',
                                   '.xlsx')
              ),
              tags$hr(),
              checkboxInput('HEADER', 'Header', TRUE),
              radioButtons('SEPERATOR', 'Separator',
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           ','),
              radioButtons('QUOTE', 'Quote',
                           c(None='',
                             'Double Quote'='"',
                             'Single Quote'="'"),
                           '"'),
              tags$hr()
              
            ),
            box(
              title = "Selected DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 6,
              dataTableOutput('DATASET_TABLE')
              ),
            box(title = "Summary", status = "primary", solidHeader = TRUE,collapsible = TRUE,width = 3,
              fluidRow(
                infoBoxOutput("table_rows_summary")
              ),
              fluidRow(
                infoBoxOutput("table_variables_summary")
              )
            )
            )  
            ),
#//////////// GroupBy UI ///////////////#----------------
    tabItem(tabName = "GroupBy",
            fluidRow(
              tabBox(width = 12,title = "Group By",
                tabPanel("Group by",
                         fluidRow(
                     box(
                       title = "Select Coulmns", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
                     uiOutput('GROUP_BY_COLUMNS'),
                     tags$hr(),
                     textOutput("GROUP_BY_TEXT")
                     ),
                     box(
                       title = "Result DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 8,
                     dataTableOutput('GROUP_BY_TABLE')
                     )
                         )
                   ),
                
                tabPanel("how to use", "Tab content 2")
              )
            )
          ),
#//////////// AddNewColumn UI ///////////////#----------------
    tabItem(tabName = "AddNewColumn",
            
            fluidRow(
              box(
              title = "Select Coulmns", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
                     uiOutput('MUTATE_COLUMNS'),
                     textOutput("MUTATE_TEXT"),
                     tags$hr(),
                     textInput("MUTATE_COLUMN_NAME","Column Name",NULL),
                     tags$hr(),
                     selectInput("MUTATE_OPERATION", "Operation",
                                 choices = c("Add" = "+","Multiply" = "*","Subtract"="-"),
                                 selected = "*")
                     
              ),box(
                title = "MUTATED DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 8,
                     dataTableOutput('MUTATE_TABLE'))
            )
    ),
#//////////// Filter UI ///////////////#----------------    
    tabItem(tabName = "Filter",
            fluidRow(
              box(
              title = "Select Coulmns", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
                     uiOutput('FILTER_COLUMNS'),
                     textOutput("FILTER_TEXT"),
                     tags$hr(),
                     #textInput("val","Type Values Sperated with - OR ,",NULL)
                     uiOutput("FILTER_COLUMN_VALUES")
              ),box(
                title = "RESULT DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 8,
                     dataTableOutput('FILTER_TABLE')
              )
              
            )
    ),
#//////////// Arrange UI ///////////////#----------------
    tabItem(tabName = "Arrange",
            fluidRow(
              box(
                title = "Select Coulmns", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
                   uiOutput('ARRANGE_COLUMNS')
                ),
              box(
                title = "RESULT DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 8,
                   dataTableOutput('ARRANGE_TABLE')
              )
            )
    ),
#//////////// Select UI ///////////////#----------------
    tabItem(tabName = "Select",
            fluidRow(
              box(
              title = "Select Coulmns", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
                     uiOutput('SELECT_COLUMNS'),
                     selectInput("SELECT_OPERATION",label = "Select_Operation", 
                                 list(":" = 1, "~" = 2,"rename"=3,"distict"=4,"select"=5,
                                      "start_with" = 6, "end_with" = 7,"contains"=8
                                      ,"match"=9,"every"=12,"edit"=13),
                                 selected = 1,multiple = FALSE),
                     textInput("SELECT_FEATURE",label = "feature"),
                     uiOutput("SELECT_COLUMN_VALUES",label = NULL)
              ),
              box(
                title = "RESULT DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 8,
                     dataTableOutput('SELECT_TABLE')
                )
            )
    ),
#//////////// Join UI ///////////////#----------------
    tabItem(tabName = "Join",
            fluidRow(
              box(
              title = "Input First DataSet", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
                
              fileInput('tbl1', 'Choose CSV file 1 to upload',
                                 accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   'text/tab-separated-values',
                                   'text/plain',
                                   '.csv',
                                   '.tsv'
                                 )),
              checkboxInput('header', 'Header', TRUE),
              selectInput('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
              selectInput('quote', 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"')
              ),
              box(
                title = "Input Second DataSet", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
              fileInput('tbl2', 'Choose CSV file 2 to upload',
                                 accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   'text/tab-separated-values',
                                   'text/plain',
                                   '.csv',
                                   '.tsv'
                                 )),
              checkboxInput('header', 'Header', TRUE),
              selectInput('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
              selectInput('quote', 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"')
              ),
            box(
              title = "Type of Join", status = "primary",collapsible = TRUE, solidHeader = TRUE,width = 4,
            selectInput("jointy",label = NA, 
                                   choices = list("left_join","right_join"," inner_join","full_join",
                                                  "semi_join","anti_join","intersect","union","setdiff"))
              )
            ),
            fluidRow(
              box(
                title = "RESULT DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 12,
                     tableOutput('result')
              )
            )
    ),
#//////////// DateFormating UI ///////////////#----------------
    tabItem(tabName = "DateFormating",
            fluidRow(
            box(
              title = "Select Coulmns", status = "warning",collapsible = TRUE, solidHeader = TRUE,width = 4,
            uiOutput('DATE_COLUMN'),
            
            tags$hr(),
            selectInput("select_original_format", "select original format:", 
                        choices = c("ydm", "ymd", "myd", "mdy", "dmy", "dym"))
            ),
            box(
              title = "RESULT DataSet", status = "success", solidHeader = TRUE,collapsible = TRUE,width = 8,                                      
                dataTableOutput('DATE_NEW_TABLE')
            )
            )
            
    ),
#//////////// numericalFormating UI ///////////////#----------------
    tabItem(tabName = "numericalFormating",
            h2("numericalFormating tab content")
    ),
#//////////// lineChart UI ///////////////#----------------
    tabItem(tabName = "lineChart",
            h2("lineChart tab content")
    ),
#//////////// BarChart UI ///////////////#----------------
    tabItem(tabName = "BarChart",
            h2("BarChart tab content")
    ),
#//////////// SinglecolumnChart UI ///////////////#----------------
    tabItem(tabName = "SinglecolumnChart",
            h2("SinglecolumnChart tab content")
    ),
#//////////// StackcolumnChart UI ///////////////#----------------
    tabItem(tabName = "StackcolumnChart",
            h2("StackcolumnChart tab content")
    ),
#//////////// DodgecolumnChart UI ///////////////#----------------
    tabItem(tabName = "DodgecolumnChart",
            h2("DodgecolumnChart tab content")
    ),
#//////////// PieChart UI ///////////////#----------------
    tabItem(tabName = "PieChart",
            h2("PieChart tab content")
    )
  )
)
#//////////// End of Body ///////////////#---------------- 

# Put them together into a dashboardPage
dashboardPage(header,sidebar,body)