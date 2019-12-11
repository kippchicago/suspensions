#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(dplyr)
library(tidyr)
library(formattable)
library(forcats)
library(shiny)
library(shinydashboard)
library(googleCloudStorageR) 
library(kippcolors)


gcs_global_bucket("idea_deanslist")
gcs_load("dl_suspensions.Rda")

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "Suspensions"),
    dashboardSidebar(
        checkboxInput(inputId = "show_prior_year_susps",
                      label = "Show prior year's cumulative suspensions?"),
        hr(),
        tags$div(style = "color:black; margin-left: 5%",
                 tags$p("The graph to the right shows cumulative suspensions from the beginning of the year (colored bars) as well as total suspensions for each month (gray interior bars)."),
                 tags$p("The table below shows the details of OSSs and ISSs"),
                 p("Click-and-dragging on graph filters that school's and the selected months' suspensions details "),
                 br(),
                 tags$em("Data gatherd from ", tags$a(href="https://kippchicago.deanslistsoftware.com", "Deanslist", target="_blank") )
                 ), # end div
        hr(),
        bookmarkButton()
                 
    ), #Sidebar end
    dashboardBody(
        # KIPP Chicago CSS
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "kippcolors_green.css")),
        fluidRow(
          tabBox(selected = "OSSs",
              id = "susps_tabs",
              width = NA, 
              tabPanel(title = "OSSs",
                       plotOutput(outputId = "monthly_oss", click = "oss_click", 
                                  brush = brushOpts(id = "oss_brush",direction = "x")) %>%
                           shinycssloaders::withSpinner()
                       ), # OSS tabPanel end
              tabPanel(title = "ISSs",
                       plotOutput(outputId = "monthly_iss", click = "iss_click",
                                  brush = brushOpts(id = "iss_brush",direction = "x")) %>%
                           shinycssloaders::withSpinner()
                       ), # ISS tabPanel end
              tabPanel(title = "OSSs + ISSs",
                       plotOutput(outputId = "monthly_oss_plus_iss", click = "oss_iss_click",
                                  brush = brushOpts(id = "oss_iss_brush",direction = "x")) %>%
                         shinycssloaders::withSpinner()
              ) # OSS + ISS tabPanel end
          ) # tabBox end  
        ), # End row
        
        fluidRow(
                DT::dataTableOutput("oss_iss_dt", height = 400)
        )
        
    ) # Body end
) # Page end


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(ggplot2)
    
    # OSS  plot ####
    output$monthly_oss <- renderPlot({
        p <- oss_rates %>% 
            filter(SY == "SY18-19") %>%
            ggplot(aes(x = month, y = cum_susps)) +
            geom_col(fill = NA) +
            geom_col(data = oss_rates %>% 
                         filter(SY == "SY19-20"),
                     fill = kipp_colors$brown) +
            geom_col(data = oss_rates %>% 
                         filter(SY == "SY19-20"),
                     aes(y = N_susps), 
                     width = .5,
                     fill = kipp_colors$darkgray) +
            facet_grid(. ~ school_name) +
            theme_kipp_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major.x = element_blank()
            ) #+
        #geom_point(shape = 95, stroke = 6, color = kipp_colors$darkblue) 
        if(input$show_prior_year_susps) {
            p <- p + geom_col(fill = NA, color = kipp_colors$gray) 
        }
        p + 
            labs(x = "Month", y = "# of Suspensions\n(month total in gray, YTD in color)")
    })
    
    #  ISS plot ####
    output$monthly_iss <- renderPlot({
        p <- iss_rates %>% 
            filter(SY == "SY18-19") %>%
            ggplot(aes(x = month, y = cum_susps)) +
            geom_col(fill = NA) +
            geom_col(data = iss_rates %>% 
                         filter(SY == "SY19-20"),
                     fill = kipp_colors$lightgreen) +
            geom_col(data = iss_rates %>% 
                         filter(SY == "SY19-20"),
                     aes(y = N_susps), 
                     width = .5,
                     fill = kipp_colors$darkgray) +
            facet_grid(. ~ school_name) +
            theme_kipp_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major.x = element_blank()
            ) #+
        #geom_point(shape = 95, stroke = 6, color = kipp_colors$darkblue) 
        if(input$show_prior_year_susps) {
            p <- p + geom_col(fill = NA, color = kipp_colors$gray) 
        }
        p + 
            labs(x = "Month", y = "# of Suspensions\n(month total in gray, YTD in color)")
    })
    
    # OSS + ISS plot ####
    output$monthly_oss_plus_iss <- renderPlot({
        
        months <- c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")
        oss_iss_rates <- bind_rows(oss_rates %>% mutate(month = as.character(month), 
                                                        type = "OSS"), 
                                   iss_rates%>% mutate(month = as.character(month), 
                                                       type = "ISS")) %>%
            mutate(month = factor(month, levels = months, ordered = TRUE),
                   type_cum_col = if_else(type == "ISS", kipp_colors$darkorange, kipp_colors$orange),
                   type_actul_col = if_else(type == "ISS", kipp_colors$darkgray, kipp_colors$gray))# %>%
           # group_by(SY, school_name, month, type) %>%
            #summarize(N_susps = sum(N_susps),
             #         adm = mean(adm)) %>%
            #mutate(cum_susps = cumsum(N_susps),
             #      susp_rate = N_susps/adm*100,
              #     cum_susp_rate = cum_susps/adm*100)

        
        
        
        p <- oss_iss_rates %>% 
            filter(SY == "SY18-19") %>%
            ggplot(aes(x = month, y = cum_susps)) +
            geom_col(fill = NA) +
            geom_col(data = oss_iss_rates %>% 
                         filter(SY == "SY19-20"),
                     aes(fill = type_cum_col)) +
            geom_col(data = oss_iss_rates %>% 
                         filter(SY == "SY19-20"),
                     aes(y = N_susps, 
                         fill = type_actul_col) , 
                     width = .5#,
                     #fill = kipp_colors$darkgray
                     ) +
            scale_fill_identity() +
            facet_grid(. ~ school_name) +
            theme_kipp_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major.x = element_blank()
            ) #+
        #geom_point(shape = 95, stroke = 6, color = kipp_colors$darkblue) 
        if(input$show_prior_year_susps) {
            p <- p + geom_col(fill = NA, color = kipp_colors$gray) 
        }
        p + 
            labs(x = "Month", y = "# of Suspensions\n(month total in gray, YTD in color)")
    })
    
    ## suspensions table ####
    
    output$oss_iss_dt <- DT::renderDT({
        
        
        if(input$susps_tabs == "ISSs"){
            output_dt <- iss %>% filter(SY == max(SY))
            
            if(!is.null(input$iss_brush)){
                clicked_month_min <- as.integer(round(input$iss_brush$xmin, 0))
                clicked_month_max <- as.integer(round(input$iss_brush$xmax, 0))
                clicked_months <- c(clicked_month_min:clicked_month_max)
                clicked_panel <- input$iss_click$panelvar1
                
                output_dt <- output_dt %>% 
                    mutate(month_int = as.integer(month)) %>%
                    filter(month_int %in% clicked_months,
                           school_name == clicked_panel)
            }
            
            
            output_dt <- output_dt %>%
                filter(SY == max(SY)) %>%
                arrange(rev(startdate)) %>%
                select(School = school_name,
                       Grade = grade_level_short,
                       Last = student_last,
                       First = student_first,
                       Month = month,
                       Date = startdate,
                       Length = numdays,
                       Category = category,
                       Summary = admin_summary) %>%
                mutate(School = as_factor(School),
                       Grade = as_factor(Grade),
                       Category = as_factor(Category))
        
        
        }
        
        if(input$susps_tabs == "OSSs"){
            output_dt <- oss %>% filter(SY == max(SY))
            
            if(!is.null(input$oss_brush)) {
                clicked_month_min <- as.integer(round(input$oss_brush$xmin, 0))
                clicked_month_max <- as.integer(round(input$oss_brush$xmax, 0))
                clicked_months <- c(clicked_month_min:clicked_month_max)
                clicked_panel <- input$oss_click$panelvar1
                
                output_dt <- output_dt %>% 
                    mutate(month_int = as.integer(month)) %>%
                    filter(month_int %in% clicked_months,
                           school_name == clicked_panel)    
            }
            
            
            
            output_dt <- output_dt %>%
                #filter(SY == max(SY)) %>%
                arrange(rev(startdate)) %>%
                select(School = school_name,
                       Grade = grade_level_short,
                       Last = student_last,
                       First = student_first,
                       Month  = month,
                       Date = startdate,
                       Length = numdays,
                       Category = category,
                       Summary = admin_summary) %>%
                mutate(School = as_factor(School),
                       Grade = as_factor(Grade),
                       Category = as_factor(Category))
            
        
            
        }
        
        if(input$susps_tabs == "OSSs + ISSs"){
            oss <- oss %>% mutate(Type = "OSS")
            iss <- iss %>% mutate(Type = "ISS")
            
            output_dt <- rbind(oss, iss) %>% filter(SY == max(SY))
            
            if(!is.null(input$oss_iss_brush)) {
                clicked_month_min <- as.integer(round(input$oss_iss_brush$xmin, 0))
                clicked_month_max <- as.integer(round(input$oss_iss_brush$xmax, 0))
                clicked_months <- c(clicked_month_min:clicked_month_max)
                clicked_panel <- input$oss_iss_brush$panelvar1
                
                output_dt <- output_dt %>% 
                    mutate(month_int = as.integer(month)) %>%
                    filter(month_int %in% clicked_months,
                           school_name == clicked_panel)    
            }
            
            
            
            output_dt <- output_dt %>%
                #filter(SY == max(SY)) %>%
                arrange(rev(startdate)) %>%
                select(Type, 
                       School = school_name,
                       Grade = grade_level_short,
                       Last = student_last,
                       First = student_first,
                       Month  = month,
                       Date = startdate,
                       Length = numdays,
                       Category = category,
                       Summary = admin_summary) %>%
                mutate(Type = as_factor(Type),
                       School = as_factor(School),
                       Grade = as_factor(Grade),
                       Category = as_factor(Category))
            
            
            
        }
        
        
        DT::datatable(output_dt,
                      rownames = FALSE,
                      filter = "top",
                      style = "bootstrap",
                      extensions = "FixedColumns",
                      fillContainer = TRUE,
                      options = list(paging = FALSE)) %>%
            DT::formatDate(~Date)
    })
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")
