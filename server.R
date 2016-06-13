library(dplyr)
library(ggplot2)
library(lubridate)
library(lazyeval)
library(pracma)
library(base)
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9 * 1024 ^ 2)

shinyServer(function(input, output) {
  
  
  
#//////////// IMPORT ///////////////#----------------
  
  ############################ IMPORT_reactive 
  Data_set<- reactive({
    imported_file <- input$IMPORT
    if (is.null(imported_file))
      return(NULL)
    read.csv(
      imported_file$datapath,
      header = input$HEADER,
      sep = input$SEPERATOR,
      quote = input$QUOTE
    )
  })
  ############################ IMPORT_table
  output$DATASET_TABLE <- renderDataTable({
    Data_set()
  })
  output$table_variables_summary <- renderInfoBox({
    infoBox(
      title = NA,subtitle = "Variables",value = length(Data_set()), icon = icon("credit-card")
    )
  })
  output$table_rows_summary <- renderInfoBox({
    infoBox(
      title = NA,subtitle = "Rows",value = ifelse(test =length(Data_set())>0 ,yes = nrow(Data_set()) ,no = 0)  , icon = icon("list"),color = "purple"
    )
  })
#//////////// GROUP_BY ///////////////#----------------
  
  ############################ GROUP_BY_columns 
  output$GROUP_BY_COLUMNS = renderUI({
    df <- data.frame(Data_set())
    selectInput("GROUP_BY_DYNAMIC","choice Columns",choices = names(df),
                multiple = TRUE)
  })
  ############################ GROUP_BY_selection 
  Group_by_column_selection <- reactive({input$GROUP_BY_DYNAMIC })
  ############################ GROUP_BY_reactive 
  Group_by_reactive <- reactive({
    df <- data.frame(Data_set())
    output$GROUP_BY_TEXT <- renderPrint({
      length(Group_by_column_selection())
    })
    if (length(Group_by_column_selection()) > 0) {
      # Convert character vector to list of symbols
      dots <- lapply(Group_by_column_selection(), as.symbol)
      # Perform frequency counts
      df %>% group_by_(.dots = dots) %>% summarise(number = n())
    }
  })
  ############################ GROUP_BY_table 
  output$GROUP_BY_TABLE <- renderDataTable({Group_by_reactive()})
#//////////// ADD_NEW_COLUMN ///////////////#----------------  
  ############################ MUTATE_columns 
  output$MUTATE_COLUMNS= renderUI({
    df <- data.frame(Data_set())
    selectInput("MUTATE_DYNAMIC","choice Columns",choices = names(df),
                multiple = TRUE)
  })
  ############################ MUTATE_selection
  Mutate_column_selection<- reactive({ input$MUTATE_DYNAMIC})
  ############################ MUTATE_function
  Mutate_function <- function(df, mutate_new_column) {
    if (length(Mutate_column_selection()) > 1) {
      operation <- reactive({
        data = input$MUTATE_OPERATION
      })
      mutate_expression <- paste(Mutate_column_selection(), collapse = operation())
      df %>% mutate_(.dots = setNames(list(mutate_expression),mutate_new_column))
    }
  }
  ############################ MUTATE_reactive
  Mutate_reactive <- reactive({
    df <- data.frame(Data_set())
    mutate_column_name<- noquote(as.character(input$MUTATE_COLUMN_NAME))
    Mutate_function(df, mutate_column_name)
  }) 
  ############################ MUTATE_table 
  output$MUTATE_TABLE <- renderDataTable({ Mutate_reactive()})
  
#//////////// FILTER ///////////////#----------------
  ############################ FILTER_columns 
  output$FILTER_COLUMNS = renderUI({
    df <- data.frame(Data_set())
    selectInput("FILTER_DYNAMIC","choice Columns", choices = names(df),multiple = TRUE)
  })
  ############################ FILTER_column_values 
  output$FILTER_COLUMN_VALUES = renderUI({
    if(length(Filter_column_selection())>0){
      numIndividuals <- as.numeric(length(Filter_column_selection()))
      
      lapply(1:numIndividuals, function(i) {
        text <- "text"
        value_of <- "value of "
        text_num <- paste(text, i, sep = "")
        column_value <- paste(value_of, Filter_column_selection()[i])
        textInput(text_num, column_value, width = 70)
        
      })
    }
  })
  ############################ FILTER_selection
  Filter_column_selection <- reactive({input$FILTER_DYNAMIC})
  ############################ FILTER_function
  Filter_function <- function(df, value) {
    i <- 1
    text <- "text"
    if (length(Filter_column_selection())>0) {#start of outer if 
      
      if (is.character(input$text1)) {#start of inner if 
        for (i in 1:length(Filter_column_selection())) {#start of second inner
          text_num <- paste0(text, i , sep = "")
          z <- "=="
          val <- input[[text_num]]#get the value from the text box
          l <- paste("'", val, "'", sep = "")#put the value in ''
          n <- paste0(z, l)#make the value of text == something
          
          if (i == 1) {#start of third inner
            #ex is ahmed == value of text by seperate them with val 
            ex <- paste0(Filter_column_selection()[i], sep = n)
          }#end of third inner
          else{#else of third inner
            #append the ex with another conditons if the input is multiple column
            ex <- paste0(append(ex, paste0(Filter_column_selection()[i], sep = n)), sep = "")          
            ex <- paste(ex, collapse = "&")
          }
          i <- i + 1
        }#end of second inner
        
        filter_(df, ex)
      }#end of inner if  
      else
      {#else of inner if 
        for (i in 1:length(Filter_column_selection())) {#start of for
          va <- paste0(text, i , sep = "")
          z <- "%in%"
          
          val <- input[[va]]
          s <- paste0(z, val)
          
          if (i == 1) {#start of if 
            ex <- paste0(Filter_column_selection(), sep = s)
          }#end of if
          else{#else of if
            ex <- append(ex, paste0(Filter_column_selection(), sep = s))
            ex <- paste(ex, collapse = "|")
          }
          i <- i + 1
        }#end of for
        
        expr = interp(ex)
        df %>% filter_(expr)
        
      }
    }
  }
  ############################ FILTER_reactive 
  Filter_reactive <- reactive({
    df <- data.frame(Data_set())
    Filter_function(df)
  })
  
  ############################ FILTER_table 
  output$FILTER_TABLE <- renderDataTable({
    Filter_reactive()
  })
  
#//////////// ARRANGE ///////////////#----------------
  ############################ ARRANGE_columns 
  output$ARRANGE_COLUMNS = renderUI({
    df <- data.frame(Data_set())
    selectInput("ARRANGE_DYNAMIC","choice Columns",choices = names(df),
                multiple = TRUE)
  })
  ############################ ARRANGE_selection
  Arrange_column_selection<- reactive({ input$ARRANGE_DYNAMIC})
  ############################ ARRANGE_function
  Arrange_function <- function(df) {
    if (length(Arrange_column_selection()) > 0) {
      dots <- lapply(Arrange_column_selection(), as.symbol)
      df %>% arrange_(.dots = dots)
    }
    
  }
  ############################ ARRANGE_reactive
  Arrange_reactive<- reactive({
    df <- data.frame(Data_set())
    Arrange_function(df)
  })
  ############################ ARRANGE_table 
  output$ARRANGE_TABLE <- renderDataTable({ Arrange_reactive()})
  
  
#//////////// SELECT ///////////////#----------------
  ############################ SELECT_columns 
  output$SELECT_COLUMNS = renderUI({
    df<- data.frame(Data_set())
    
    selectInput('SELECT_CHECK', 'Columns', names(df),multiple = TRUE)
  })
  ############################ SELECT_selection
  Select_column_selection <-reactive({data<-input$SELECT_CHECK})
  ############################ FILTER_column_values
  output$SELECT_COLUMN_VALUES<-renderUI({ 
    if(length(Select_column_selection())>0){
      num <-as.numeric(length(Select_column_selection()))
      lapply(1:num, function(i) {
        o<-"text"
        l<-"value of "
        d<-paste(o,i,sep = "")
        p<-paste(l,Select_column_selection()[i])
        textInput(d, p, width = 70 )
      })
    }
  })
  ############################ SELECT_function
  Select_function<- function(df) {
    
    if(length(Select_column_selection())>0){#start of if
      if(input$SELECT_OPERATION ==1)
      {#start of inner if 
        #paste the operation : with the selected column
        s<-paste0(Select_column_selection(),collapse =":")
        df %>% select_(s) 
      }#end of inner if 
      else if(input$SELECT_OPERATION==2)
      {#start of first inner
        #paste the operation : and ~ with the selected column means not ~(a:b)
        s<-paste0(Select_column_selection(),collapse =":")
        m<-paste0("-","(",s,")",sep ="")
        
        df %>% select_(m) 
      }#end of first if
      else  if(input$SELECT_OPERATION==3)
      {#start of second if
        i<-1
        # for(i in 1:5)
        #{
        x<-1
        s<-"text"
        
        if(length(Select_column_selection())==1)
        {#start of third if 
          
          #get the selected colmn
          lt<-Select_column_selection()
          #get the first input in the text
          lx<-input$text1
        }#end of third if 
        else{#else of third if 
          #get the selected colmn
          lt<-c(Select_column_selection())
          #get the first input in the text
          lx<-input$text1
          for(x in 2:length(Select_column_selection()))
          {#start of for
            
            vs<-paste0(s,x,sep="")
            lx<-append(lx,vals<-input[[vs]])
            x<-x+1
          }#end of for 
        }#end of else
        
        plyr::rename(df,setNames(lx,lt))
        #}  
        
        
      }
      else if(input$SELECT_OPERATION==4)
      {
        dots=lapply(Select_column_selection(), as.symbol)
        
        distinct( select_(df,.dots=dots)) 
      }
      else if(input$SELECT_OPERATION==5){
        dots=lapply(Select_column_selection(), as.symbol)
        
        df %>% select_(.dots=dots)
      }
      
      else if(input$SELECT_OPERATION==6){
        #get the condition that will get the start with what for example
        feature1<-input$SELECT_FEATURE
        select(df, starts_with(feature1, ignore.case = TRUE))
      }
      else if(input$SELECT_OPERATION==7){
        #get the condition that will get the start with what for example
        feature2<-input$SELECT_FEATURE
        
        select(df, ends_with(feature2, ignore.case = TRUE))
      }
      
      else if(input$SELECT_OPERATION==8){
        #get the condition that will get the start with what for example
        feature3<-input$SELECT_FEATURE
        
        select(df, contains(feature3, ignore.case = TRUE))
        
      }
      else if(input$SELECT_OPERATION==9){
        #get the condition that will get the start with what for example
        feature4<-input$SELECT_FEATURE
        
        select(df, matches(feature4, ignore.case = TRUE))
      }
      
      
      else if(input$SELECT_OPERATION==12){
        
        select(df, everything())
      }
      else if(input$SELECT_OPERATION ==13)
      {
        edit(df)
      }
    }#end of if
  }
  ############################ SELECT_reactive 
  Select_reactive<-reactive({df<-data.frame(Data_set())
  Select_function(df)
  })
  ############################ SELECT_table 
  output$SELECT_TABLE <- renderDataTable({
    Select_reactive()
  })
#//////////// DATE ///////////////#----------------
  
  ############################ DATE_columns
  output$DATE_COLUMN = renderUI({
    df2 <- data.frame(Data_set())
    selectInput("DATE_DYNAMIC",
                "choice Columns",
                choices = names(df2),
                multiple = FALSE)
  })
  Original_format_type_reactive <- reactive({
    switch(input$select_original_format,
           ymd = "ymd",
           ydm= "ydm",
           myd = "myd",
           mdy = "mdy",
           dym = "dym",
           dmy = "dmy"
    )
  })
  
  Date_cleansing_function <- function() {
    corresponding_function<-get(Original_format_type_reactive())
    corresponding_function( Data_set()[[as.character(input$DATE_DYNAMIC)]] ) 
  }
  
  Date_reactive<-reactive({  df<-as.data.frame(Data_set())
  df[[as.character(input$DATE_DYNAMIC)]]<-Date_cleansing_function()
  df
  })
  ############################ DATE_table 
  output$DATE_OLD_TABLE <- renderTable({
    Data_set()
  })
  output$DATE_NEW_TABLE <- renderDataTable({
    Date_reactive()
  })
  output$UPDATED_DATE_TABLE <- renderDataTable({
    Save_reactive()
    Date_reactive()
  })
  ############################ DOWNLOAD 
  output$DOWNLOAD_DATA <- downloadHandler(
    filename =  function()
    {
      paste(input$FILE_NAME, '.csv', sep='')
    } ,
    content = function(file) {
      write.csv(Date_reactive(), file)
    }
  )
  ############################ SAVE 
  Save_reactive<-reactive({
    if(input$SAVE){
      dir<-choose.files(getwd(),"choose")
      new_dir<-strRep(dir,"\\" ,"/" )
      write.csv(date(),new_dir)
    }
  })
  
  
  
  
#//////////// JOIN ///////////////#----------------
  
  Join_type_reactive <- reactive({
    switch(input$jointy,
           
           left_join= "left_join",
           right_join = "right_join",
           inner_join="inner_join",
           full_join = "full_join",
           anti_join = "anti_join",
           semi_join = "semi_join",
           intersect ="intersect",
           union ="union",
           setdiff ="setdiff"
    )
  })
  Join_cleansing_function <- function(df1,df2) {
    if(length(input$jointy)>0){
      corresponding_function<-get(Join_type_reactive())
      corresponding_function(df1,df2) 
    }
  }
  ############################ JOIN_import
  #data1
  D1 <-reactive({ 
    inF1 <- input$tbl1
    
    if (is.null(inF1))
      return(NULL)
    read.csv(inF1$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    
  })
  #data2
  D2 <-reactive({ 
    inF2 <- input$tbl2
    
    if (is.null(inF2))
      return(NULL)
    read.csv(inF2$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    
  })
  
  
  ############################ JOIN_function
  join<-function (d1,d2){
    
    s<-as.numeric(input$jointy)
    if(s==1)
    {
      d1 %>% inner_join(d2)
    }
    else if(s==2)
      d1 %>% left_join(d2)
  }
  ############################ JOIN_reactive
  Join_reactive<-reactive({
    
    df1<-data.frame(D1())
    df2<-data.frame(D2())
    Join_cleansing_function(df1,df2)
    
  })
  
  ############################ JOIN_table
  
  output$result <- renderTable({
    Join_reactive()
  })
  
  
  
  
  
  
  
  
  
})