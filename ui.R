library(shiny)

# subset of variable names to display in table
var_names_subset <- c("metro_name", "occ_job_title", "total_employment", "job_per_1000", "wage_mean", "wage_median", "wage_median_adj", "coli_composite")

shinyUI(pageWithSidebar(
    headerPanel("Where Should You Work?"),
    sidebarPanel(
        textInput("job_search_string", label = h3("Search Job Title"),
                  value = "*" ),
        helpText("Case insensitive. Use '*' character to select all job titles. Accepts Regular Expressions"),
        
        uiOutput("job_selector"),
        
        textOutput("text_tot_emp"),
        textOutput("text_job_1000"),
        textOutput("text_wage_median"),
        textOutput("text_wage_median_adj"),
        
        checkboxGroupInput('show_vars', label = h3('Columns in data to show:'), names(salary),
                           selected = var_names_subset),
        
        helpText("Data Source:"),
        helpText("Salary: Bureau of Labor Statistics Salary Survey (May 2014)"),
        helpText("Cost-of-Living: C2ER-COLI 2014 Annual Average")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('Salary Data',
               dataTableOutput("table1")),
      tabPanel('second panel',
               helpText("Test"))
    )
  )
))