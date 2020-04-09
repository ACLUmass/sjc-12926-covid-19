library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(stringr)
library(shinycssloaders)
library(showtext)
library(leaflet)
library(leafsync)
library(httr)
library(readxl)
library(tidyr)
library(ggfittext)
library(DT)
library(tigris)

# Set ggplot settings
theme_set(theme_minimal())

# Load ggplot-friendly font using show_text
font_add("gtam", "GT-America-Standard-Regular.ttf",
         bold = "GT-America-Standard-Bold.ttf")
showtext_auto()

# Download county data
mass_cntys <- counties(state="massachusetts", cb=T)

# Define list of counties
counties <- c("DOC", "Barnstable", "Berkshire", "Bristol", "Dukes", "Essex", 
              "Franklin", "Hampden", "Hampshire", "Middlesex", "Norfolk", 
              "Plymouth", "Suffolk", "Worcester")
# Make list for drop-downs
county_choices <- c("--", "All", counties)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# UI
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ui <- fluidPage(theme = "sjc_12926_app.css",
  
  # App title ----
  titlePanel("SJC 12926: Tracking COVID-19 in Prisons"),
  
  div(
    navlistPanel(widths = c(3, 9), id="panels",
                 
      tabPanel("About", 
               h3("Explore Massachusetts Prisons and Jails' Reaction to SJC 12926"),
               p("View plots in the different tabs to track testing, positive",
                 "cases, and releases in prisons and jails across Massachusetts",
                 "during the COVID-19 pandemic after the Supreme Judicial Court (SJC)", 
                 a(href="https://www.mass.gov/doc/sjc-12926-opinion/download", "decision"), 
                 "in Case 12926, requiring daily reports from state and county facilities."),
               
               h3("About the Data"),
               p("The data displayed here are compiled from daily reports sent",
                 "out by 13 county sheriffs, not including Nantucket County,",
                 "and by the State Department of Corrections (DOC). They contain",
                 "daily reporting data starting on April 5th, 2020."),
               p("The data sourced for all visualizations on this site are available for download",
                 actionLink("link_to_download", "here."), style="margin-top: 1rem;"),
               
               div(id="dev-warning",
                   wellPanel(
                     icon('exclamation-triangle'),
                     h4("Disclaimer"),
                     em("All data and analysis presented here is subject to change",
                       "based on updates and amendments received from the counties",
                       "and the state.")
                     )
               )
               
               # h4("Source Code"),
               # p("Interested programmers can view the source code for this app, written in R, on", 
               #   a("GitHub.", href="https://github.com/ACLUmass/bpd-covid-19-tracker"))
               ),
      
      tabPanel("Total Releases", 
               h2(textOutput("n_releases_str"), align="center"),
               p("Inmates and detainees released under SJC 12926 since April 5th", align="center"),
               withSpinner(plotOutput("all_releases_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Total Positives", 
               h2(textOutput("n_positive_str"), align="center"),
               p("Inmates, detainees, correctional officers, and staff tested", strong("positive"),
                 "for COVID-19 since April 5th", align="center"),
               withSpinner(plotOutput("all_positives_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Total Tests", 
               h2(textOutput("n_tests_str"), align="center"),
               p("Inmates, detainees, correctional officers, and staff tested for COVID-19 since April 5th", 
                 align="center"),
               withSpinner(plotOutput("all_tests_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      
      tabPanel("Trends Over Time by Location",
               wellPanel(
                 p("Select value to plot versus time."),
                 selectInput("select_y_v_time", label = NULL, 
                             choices = c("Releases", "Tests", "Positive Cases"),
                               selected = "Releases", multiple=FALSE),
                 p("Select up to three locations to plot versus time."),
                 splitLayout(
                   selectInput("select_county1", label = NULL, choices = county_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_county2", label = NULL, choices = county_choices,
                               selected = "DOC", multiple=FALSE),
                   selectInput("select_county3", label = NULL, choices = county_choices,
                               selected = "Barnstable", multiple=FALSE)
                 )),
               withSpinner(plotOutput("releases_v_time_plot"), type=4, color="#b5b5b5", size=0.5)
               ),

      tabPanel("Mapping County Trends",
               wellPanel(
                 p("Select value to plot."),
                 selectInput("select_y_plot", label = NULL, 
                             choices = c("Releases", "Tests", "Positive Cases"),
                             selected = "Releases", multiple=FALSE)
                 ),
               withSpinner(leafletOutput("county_maps"),
                           type=4, color="#b5b5b5", size=0.5)
               ),
      # 
      # tabPanel("Major & Minor Incidents Comparison", 
      #          withSpinner(plotOutput("major_minor_plot"), 
      #                      type=4, color="#b5b5b5", size=0.5)),
      # 
      # tabPanel("Incidents Over Time", 
      #          withSpinner(plotOutput("incidents_v_time_plot"), 
      #                      type=4, color="#b5b5b5", size=0.5)),
      # 
      tabPanel("Explore Data",
               em("Note: Empty cells denote that the given county",
                  "did not report that value, while cells with value 0 mean 0 was reported."),
               br(), br(),
               withSpinner(dataTableOutput("df_table"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Download Data",
               p("Data analysts at the ACLU of Massachusetts compile the daily",
                 "reports recieved from the counties and DOC into a single",
                 "spreadsheet. This spreadsheet is what we source for all",
                 "visualizations on this site."),
               p(strong("Current file size: "), textOutput("n_rows", inline=T), "entries"),
               p("The accumulated database is available for download here:"),
               downloadButton("downloadData", "Download XLSX"))
      ),
    
    em("Latest data update:", textOutput("latest_time_str", inline=T), 
       align="right", style="opacity: 0.6;")
    ),
  
  br(),
  hr(),
  img(src="OneLineLogo_RGB_Massachusetts.png", width="300px", 
      style="opacity: 0.5; display: block; margin-left: auto; margin-right: auto;"),
  p("Please contact lchambers@aclum.org with questions.", align="center", style="opacity: 0.6;")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Server
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
server <- function(input, output, session) {

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Load Data
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Define file locations
  sjc_dropbox_url <- "https://www.dropbox.com/s/xutf23fy40nwjb3/prison_data_SJC12926.xlsx?dl=1"
  
  # Download excel spreadsheet from URL and read as DF
  GET(sjc_dropbox_url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  sjc_df <- read_excel(tf) %>%
    # Turn string "NA" to real NA
    mutate_if(is.character, ~na_if(., 'NA')) %>%
    # Make all count columns numeric
    mutate_at(vars(starts_with("N ")), 
              as.numeric) %>%
    # Render dates as such
    mutate(Date = as.Date(Date))
  
  output$n_rows <- renderText({nrow(sjc_df)})
  
  # Determine last update time (not sure how accurate this is, but it might make 
  # users feel better)
  update_time <- file.info(tf)$mtime %>%
    as_datetime(tz="America/New_York")
  output$latest_time_str <- renderText({
    update_time %>%
    format(format="%A %B %e, %Y at %I:%M %p %Z")
  })
  
  # For plotting, replace NAs with 0s to allow arithmetic
  sjc_num_df <- sjc_df %>%
    mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
    # Render dates as such
    mutate(Date = as.Date(Date, origin=lubridate::origin),
           # Calculate totals of released, positive, tested
           all_released = `N Released Pre-Trial` + `N Released Sentenced`,
           all_positive = `N Positive - Detainees/Inmates` + 
             `N Positive - COs` + 
             `N Positive - Staff`,
           all_tested = `N Tested - Detainees/Inmates` + 
             `N Tested - COs` + 
             `N Tested - Staff`)
  
  # Calculate totals
  n_released <- sum(sjc_num_df$all_released)
  n_positive <- sum(sjc_num_df$all_positive)
  n_tested <- sum(sjc_num_df$all_tested)
  output$n_releases_str <- renderText({n_released})
  output$n_positive_str <- renderText({n_positive})
  output$n_tests_str <- renderText({n_tested})
  
  # Calculate sums by county
  sum_sjc_num_df <- sjc_num_df %>%
    filter(County != "DOC") %>%
    group_by(County) %>%
    summarize(all_positive = sum(all_positive),
              all_tested = sum(all_tested),
              all_released = sum(all_released))
  
  all_df_all <- sjc_num_df %>%
    group_by(Date) %>%
    summarize(all_released = sum(all_released),
              all_positive = sum(all_positive),
              all_tested = sum(all_tested)) %>%
    mutate(County = "All")
  
  df_by_county <- sjc_num_df %>%
    dplyr::select(Date, County, all_released, all_positive, all_tested) %>%
    rbind(all_df_all)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # All Releases
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$all_releases_plot <- renderPlot({
    
    sjc_num_df %>%
      mutate(all_released = `N Released Pre-Trial` + `N Released Sentenced`) %>%
      group_by(County) %>%
      summarize(all_released_cumul = sum(all_released)) %>%
    ggplot(aes(x=factor(County, levels=counties), 
               y=all_released_cumul, 
               fill =all_released_cumul >0, 
               label=all_released_cumul)) +
      geom_col(show.legend = FALSE) +
      geom_bar_text(contrast=T, family="gtam", size=16) +
      labs(y = "", x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            axis.text.y = element_blank(),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=16)) +
      scale_fill_manual(values = c("white", "#0055aa"))
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # All Positives
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$all_positives_plot <- renderPlot({
    
    sjc_num_df %>%
      group_by(County) %>%
      summarize(all_positive_cumul = sum(all_positive)) %>%
    ggplot(aes(x=factor(County, levels=counties), 
                 y=all_positive_cumul, 
                 fill = all_positive_cumul > 0,
                 label = all_positive_cumul)) +
      geom_col(show.legend=F) +
      geom_bar_text(contrast=T, family="gtam", size=16) +
      labs(y = "", x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            axis.text.y = element_blank(),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=16)) +
      scale_fill_manual(values = c("white", "#0055aa"))
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # All Tests
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$all_tests_plot <- renderPlot({
    
    sjc_num_df %>%
      group_by(County) %>%
      summarize(Tested = sum(all_tested)) %>%
      ggplot(aes(x=factor(County, levels=counties), 
                 y=Tested, 
                 fill = Tested > 0, 
                 label = Tested)) +
      geom_col(show.legend=F) +
      geom_bar_text(contrast=T, family="gtam", size=16) +
      labs(y = "", x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            axis.text.y = element_blank(),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=16),
            legend.title = element_blank(),
            legend.position=c(.9, .8),
            legend.justification="right",
            legend.background = element_rect(fill="white", color=NA)) +
      scale_fill_manual(values = c("white", "#0055aa"))
    
  })
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Releases v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # Determine which incidents to plot
  cnty_to_plot <- reactive({
    c(input$select_county1,
      input$select_county2,
      input$select_county3)
  })
  
  # Determine which variable to plot
  y_to_plot_time <- reactive({ input$select_y_v_time })
  
  # Plot
  output$releases_v_time_plot <- renderPlot({
    
    if (y_to_plot_time() == "Releases") {
      df_by_county <- df_by_county %>%
        mutate(value = all_released)
      y_axis_label <- "Number Released"
    } else if (y_to_plot_time() == "Tests") {
      df_by_county <- df_by_county %>%
        mutate(value = all_tested)
      y_axis_label <- "Number Tested"
    } else if (y_to_plot_time() == "Positive Cases") {
      df_by_county <- df_by_county %>%
        mutate(value = all_positive)
      y_axis_label <- "Number Tested Positive"
    }
    
    df_by_county %>%
      filter(County %in% cnty_to_plot()) %>%
      ggplot(aes(x=Date, y = cumsum(value), color=County)) +
      geom_path(size=1.3, show.legend = T, alpha=0.7) +
      geom_point() +
      labs(x = "", y = paste("Total", y_axis_label), color="",
           title = paste(y_to_plot_time(), "over Time"),
           subtitle="Cumulative since April 5th, 2020") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), legend.direction="horizontal",
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#ef404d", "#0055aa")) +
      coord_cartesian(clip = 'off')

  })

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 🌍 Incidents by Location 🌍
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which variable to plot
  y_to_plot <- reactive({ input$select_y_plot })
  
  # Plot map
  output$county_maps <- renderLeaflet({

    if (y_to_plot() == "Releases") {
      sum_sjc_num_df <- sum_sjc_num_df %>%
        mutate(value = all_released)
      y_axis_label <- "Number Released"
    } else if (y_to_plot() == "Tests") {
      sum_sjc_num_df <- sum_sjc_num_df %>%
        mutate(value = all_tested)
      y_axis_label <- "Number Tested"
    } else if (y_to_plot() == "Positive Cases") {
      sum_sjc_num_df <- sum_sjc_num_df %>%
        mutate(value = all_positive)
      y_axis_label <- "Number Tested Positive"
    }
    
    mass_cntys_joined <- geo_join(mass_cntys, sum_sjc_num_df, "NAME", "County")
    
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = sum_sjc_num_df$value
    )
    
    leaflet(mass_cntys_joined) %>% 
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(fillColor = ~pal(value),
                  fillOpacity = .6,
                  weight=1,
                  color = "#b2aeae",
                  label = ~paste(NAME, "County:", value, y_to_plot()),
                  group="circle_marks") %>%
      addLegend(pal = pal, 
                values = sum_sjc_num_df$value, 
                position = "bottomleft", 
                title = y_axis_label
      )  %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){
               var groupLayer = map.layerManager.getLayerGroup('circle_marks');
               map.fitBounds(groupLayer.getBounds());
           }")))

  })

  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # # 🙅🏽 Major & Minor Incidents v. Time 🤷🏽
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # output$major_minor_plot <- renderPlot({
  #   
  #   last_date_value_major <- df_all %>%
  #     filter(date(OCCURRED_ON_DATE) == last_date_to_plot,
  #            Lauren_says_minor == FALSE) %>%
  #     count() %>%
  #     pull()
  #   
  #   last_date_value_minor <- df_all %>%
  #     filter(date(OCCURRED_ON_DATE) == last_date_to_plot,
  #            Lauren_says_minor == TRUE) %>%
  #     count() %>%
  #     pull()
  #   
  #   df_all %>%
  #     filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
  #     group_by(date = date(OCCURRED_ON_DATE), Lauren_says_minor) %>%
  #     summarize(n = n()) %>%
  #     ggplot(aes(x=date, y = n, alpha = date >= ymd(20200310))) +
  #     geom_vline(aes(xintercept=ymd(20200310)), 
  #                linetype="dashed", color = "#fbb416", size=1.2, alpha=0.5) +
  #     geom_path(aes(color = Lauren_says_minor, group=Lauren_says_minor), 
  #               size=1.3, show.legend = FALSE) +
  #     ylim(0, 200) +
  #     labs(x = "", y = "Number of Incidents", color="") +
  #     theme(plot.title= element_text(family="gtam", face='bold'),
  #           text = element_text(family="gtam", size = axis_label_fontsize),
  #           plot.margin = unit(c(1,5,1,1), "lines")) +
  #     scale_x_date(date_labels = "%b %e ", 
  #                  limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
  #     scale_color_manual(values=c("#ef404d", "#0055aa")) +
  #     scale_alpha_manual(values=c(0.3, 1)) +
  #     annotate("text", x=ymd(20200310)-2.5, y = 60, angle=90, hjust=0.5,
  #              color="#fbb416", family="gtam", size = MA_label_fontsize,
  #              lineheight = MA_label_lineheight,
  #              label = "State of Emergency\ndeclared in MA") +
  #     annotate("text", x = last_date_to_plot, y = last_date_value_major, hjust=-.1, vjust = 0.5,
  #              family="gtam", size = year_label_fontsize, color="#ef404d", fontface="bold",
  #              label = "Major\nincidents") +
  #     annotate("text", x = last_date_to_plot, y = last_date_value_minor, hjust=-.1, vjust = -0.5,
  #              family="gtam", size = year_label_fontsize, fontface="bold", color="#0055aa",
  #              label = "Minor\nincidents") +
  #     coord_cartesian(clip = 'off')
  #   
  # })
  # 
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # # 📉 Incidents v. Time 📉
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # output$incidents_v_time_plot <- renderPlot({
  #   
  #   df_all %>%
  #     filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
  #     group_by(date = date(OCCURRED_ON_DATE)) %>%
  #     summarize(n = n()) %>%
  #   ggplot(aes(x=date, y = n, color = date >= ymd(20200310))) +
  #     geom_vline(aes(xintercept=ymd(20200310)), 
  #                linetype="dashed", color = "#fbb416", size=1.2, alpha=0.5) +
  #     geom_path(aes(group = 1), size=1.3, show.legend = FALSE) +
  #     geom_point(size=.6, show.legend = FALSE) +
  #     ylim(0, 350) +
  #     labs(x = "", y = "Daily Number of Incidents", color="") +
  #     theme(plot.title= element_text(family="gtam", face='bold'),
  #           text = element_text(family="gtam", size = axis_label_fontsize)) +
  #     scale_color_manual(values=c("black", "#fbb416")) +
  #     annotate("text", x=ymd(20200310)-2.5, y = 100, angle=90, hjust=0.5,
  #              color="#fbb416", family="gtam", size = MA_label_fontsize,
  #              lineheight = MA_label_lineheight,
  #              label = "State of Emergency\ndeclared in MA") +
  #     scale_x_date(date_labels = "%b %e ", 
  #                  limits = c(last_date_to_plot - months(2), last_date_to_plot))
  #   
  # })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Explore Data w/ Table
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$df_table <- renderDataTable(
    {sjc_df },
    
    options = list(scrollX = TRUE), 
    filter = 'top'
    )
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # ⬇️ Download XLSX ⬇️
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  observeEvent(input$link_to_download, {
    updateTabsetPanel(session, "panels", "Download Data")
  })

  output$downloadData <- downloadHandler(

    filename = function() {
      timestamp <- update_time %>%
        format(format = "%Y%m%d_%H%M")
      paste0(timestamp, "_prison_data_SJC12926.xlsx")
    },
    content = function(file) {
      withProgress(message = 'Downloading...', value = 1, {
        GET(url, write_disk(file))
        # write.csv(df_all, file)
      })
    }
  )
  
}

shinyApp(ui = ui, server = server)
