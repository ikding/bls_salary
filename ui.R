library(shiny)
library(ggplot2)  # for the diamonds dataset

shinyUI(pageWithSidebar(
    headerPanel("Where Should You Work?"),
    sidebarPanel(
        helpText("Data Source:"),
        helpText("Salary: Bureau of Labor Statistics Salary Survey (May 2014)"),
        helpText("Cost-of-Living: C2ER-COLI 2014 Annual Average"),
        
        textInput("job_title", label = h3("Search Job Title"),
                  value = "" ),
        
        selectInput("select", label = h3("Select job"), 
                    choices = list("Choice 1" = 1, "Choice 2" = 2,
                                   "Choice 3" = 3), selected = 1),
        
        checkboxGroupInput('show_vars', 'Columns in diamonds to show:', names(diamonds),
                           selected = names(diamonds)),
        helpText('For the diamonds data, we can select variables to show in the table;
                 for the mtcars example, we use orderClasses = TRUE so that sorted
                 columns are colored since they have special CSS classes attached;
                 for the iris data, we customize the length menu so we can display 5
                 rows per page.')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('diamonds',
               dataTableOutput("mytable1")),
      tabPanel('second panel',
               helpText("Test"))
    )
  )
))