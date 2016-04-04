library(shiny)
library(rCharts)

# subset of variable names to display in table
var_names_subset <- c("metro_name", "occ_job_title", "total_employment", "job_per_1000", "wage_mean", "wage_median", "wage_median_adj", "coli_composite")

shinyUI(pageWithSidebar(
    headerPanel("Where Should You Work?"),
    
    sidebarPanel(
        textInput("job_search_string", label = h3("Search Job Title"),
                  value = "*" ),
        helpText("Case insensitive. Use '*' character to select all job titles. Accepts Regular Expressions"),
        
        downloadButton('downloadData', 'Download Salary Table'),
        
        uiOutput("job_selector"),
        htmlOutput("text_metro_insight"),
        
        checkboxGroupInput('show_vars', 
                           label = h3('Columns in data to show:'), 
                           names(salary),
                           selected = var_names_subset),
        
        helpText("Data Source:"),
        helpText("Salary: Bureau of Labor Statistics Salary Survey (May 2014)"),
        helpText("Cost-of-Living: C2ER-COLI 2014 Annual Average")
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel('Salary Data Table',
                     dataTableOutput("table1")),
            tabPanel('Bar Chart',
                     htmlOutput("text_job_title"),
                     uiOutput("chart_var_select"),
                     numericInput("top_n", "Top N Metros:", 7,
                                  min = 1, max = 20),
                     showOutput("barchart1", "nvd3")
            )
        )
    )
))
