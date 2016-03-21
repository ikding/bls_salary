library(shiny)
library(rCharts)
library(BH) # needed for dplyr - load it here so shiny knows to build it during deployment
library(dplyr)

salary <- readRDS("data/salary_coli.RDS")
job_names <- unique(salary$occ_job_title)

shinyServer(function(input, output, session) {
    
    salary_data <- reactive({
        salary[salary$occ_job_title == input$select_job, input$show_vars, drop = F]
    })
    
    job_names_subset <- reactive({
        job_names[grepl(pattern = input$job_search_string, x = job_names, ignore.case = T)]
    })
    
    # a large table, reative to input$show_vars
    output$table1 = renderDataTable({
        salary_data()
    }, options = list(orderClasses = TRUE))
    
    output$job_selector <- renderUI({
        selectInput("select_job", label = h3("Select job"), 
                    as.list(job_names_subset()))
    })
    
    output$text_metro_insight <- renderUI({
        text1 <- paste("<b>Highest total employment: </b>", 
                       arrange(salary_data(), desc(total_employment))[, "metro_name"][1])
        text2 <- paste("<b>Highest number of", input$select_job, "employment per 1000 overall jobs: </b>",
                       arrange(salary_data(), desc(job_per_1000))[, "metro_name"][1])
        text3 <- paste("<b>Highest median wage: </b>",
                       arrange(salary_data(), desc(wage_median))[, "metro_name"][1])
        text4 <- paste("<b>Highest median wage (after cost-of-living adj.):</b>",
                       arrange(salary_data(), desc(wage_median_adj))[, "metro_name"][1])
        HTML(paste(text1, text2, text3, text4, sep = '<br/><br/>'))
    })
    
#     output$text_tot_emp <- renderUI({
#         paste("<b>Highest total employment: </b>", 
#               arrange(salary_data(), desc(total_employment))[, "metro_name"][1])
#     })
#     
#     output$text_job_1000 <- renderText({
#         paste("Highest number of", input$select_job, "employment per 1000 overall jobs:",
#               arrange(salary_data(), desc(job_per_1000))[, "metro_name"][1])
#     })
# 
#     output$text_wage_median <- renderText({
#         paste("Highest median wage:",
#               arrange(salary_data(), desc(wage_median))[, "metro_name"][1])
#     })
#     
#     output$text_wage_median_adj <- renderText({
#         paste("Highest median wage (after cost-of-living adj.):",
#               arrange(salary_data(), desc(wage_median_adj))[, "metro_name"][1])
#     })
    
    output$text_job_title <- renderUI({
        job_title <- paste("<h3>", input$select_job, "</h3>")
        HTML(paste(" ", job_title, " ", sep = "</br>"))
    })
    
    output$chart_var_select <- renderUI({
        
        selectInput("select_var", label = "Bar chart variable", 
                    c("Total employment" = "total_employment",
                      "Employment per 1000 over all jobs" = "job_per_1000",
                      "Median wage" = "wage_median",
                      "Median wage (after COLI adj.)" = "wage_median_adj")
        )
    })
    
    output$barchart1 <- renderChart2({
        salary_subset = salary_data()[order(-salary_data()[, input$select_var]), ]
        salary_subset = salary_subset[c(1:input$top_n),]
        
        p <- nPlot(x = "metro_name", y = input$select_var,
              data = salary_subset,
              type = 'multiBarChart')
#         p$params$width <- 500
#         p$params$height <- 800
        p$chart(reduceXTicks = FALSE)
        p$xAxis(staggerLabels = TRUE)
        
        return(p)
    })
})
