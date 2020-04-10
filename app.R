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
# infection_choices <- c("--", "MA Total", "MA Prisoner Total", counties)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# UI
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ui <- fluidPage(theme = "sjc_12926_app.css",
  
  # App title ----
  div(id="title",
    titlePanel("Tracking COVID-19 in Massachusetts Prison & Jails")
  ),
  
  div(
    navlistPanel(widths = c(3, 9), id="panels",
                 
      tabPanel("About", 
               h3("Explore Massachusetts Prisons and Jails' Response to COVID-19"),
               p("View plots in the different tabs to track testing, positive",
                 "cases, and releases in prisons and jail during the COVID-19",
                 "pandemic, as documented in reports made by Massachusetts",
                 "prisons and jails to the Supreme Judicial Court (SJC)."),
               
               h3("About the Data"),
               p("The data displayed here are compiled from reports sent to the Court",
                 "by 13 county sheriffs, not including Nantucket County,",
                 "and by the State Department of Corrections (DOC), starting on",
                 "March 27th, 2020. Daily reports are required as a result of the SJC",
                 a(href="https://www.mass.gov/doc/sjc-12926-opinion/download", "ruling"),
                 "in case 12926."),
               p("Learn more about the history, details, and outcomes of the ",
                 "case at the ACLU of Massachusetts website:",
                 a(href="https://www.aclum.org/en/cases/committee-public-counsel-services-v-chief-justice-trial-court",
                   "Committee for Public Counsel Services (CPCS) v. Chief Justice of the Trial Court.")),
               p("The data sourced for all visualizations on this site are available for download",
                 actionLink("link_to_download", "here."), style="margin-top: 1rem;"),
               
               h3("Source Code"),
               p("Interested programmers can view the source code for this app, written in R, on",
                 a("GitHub.", href="https://github.com/ACLUmass/sjc-12926-covid-19")),
               
               div(id="dev-warning",
                   wellPanel(
                     icon('exclamation-triangle'),
                     h4("Disclaimer"),
                     em("All data and analysis presented here is subject to change",
                       "based on updates and amendments received from the counties",
                       "and the state. For more resources on dealing with inconsistencies",
                       "in these data, visit our page on", 
                       actionLink("link_to_external", "external resources."))
                   )
               )
           ),
      
      tabPanel("External Resources", 
               h3("Inconsistencies in Reporting"),
               p("This site exists as a resource to document data which Massachusets",
                 "prisons, jails, and houses of corrections report to the SJC",
                 "amidst the COVID-19 crisis."),
               p("However, it became evident in the first few days of reports that",
                 "there were inconsistencies in data collection from the various",
                 "institutions."),
               h3("Recommended Resources"),
               p("To gain additional perspectives on how the COVID-19 crisis is",
                 "affecting incarcerated people in Massachusetts, we recommend",
                 "the following resources:"),
               HTML("<ul>
                  <li><a href='https://www.middlesexsheriff.org/covid19' target='_blank'>Middlesex Sheriff's Office Daily COVID-19 Reports</a></li>
                  <li><a href='https://twitter.com/BristolSheriff' target='_blank'>Bristol Sheriff's Office Twitter</a></li>
                  <li>Various Reporting
                      <ul>
                        <li><a href='https://www.wbur.org/commonhealth/2020/04/01/mass-prisons-jails-coronavirus' target='_blank'>WBUR: DOC 4/1/2020</a></li>
                        <li><a href='https://www.southcoasttoday.com/news/20200408/two-bristol-county-sheriffs-officers-test-positive-for-covid-19' target='_blank'>SouthCoast Today: Bristol 4/8/2020</a></li>
                        <li><a href='https://www.wbur.org/news/2020/04/09/covid-19-sheriff-essex-county-jail-soars' target='_blank'>WBUR: Essex 4/9/2020</a></li>
                      </ul>
                  </li>
                  <li>Tweets from Reporters
                      <ul>
                        <li><a href='https://twitter.com/jbmckim' target='_blank'>Jenifer Mckim, WGBH</a></li>
                        <li><a href='https://twitter.com/sweetadelinevt' target='_blank'>Sarah Betancourt, Commonwealth Magazine</a></li>
                      </ul>
                  </li>
                </ul>")
               ),
      
      tabPanel("Total Releases", 
               wellPanel(
                 p("Select kind of prisoner:"),
                 selectInput("select_release", label = NULL, 
                           choices = c("All", "Pre-Trial", "Sentenced", "Parole", "Total"),
                           selected = "All", multiple=FALSE)
                 ),
               h2(textOutput("n_releases_str"), align="center"),
               p("Prisoners released pursuant to SJC 12926", align="center"),
               withSpinner(plotOutput("all_releases_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Releases Over Time",
               wellPanel(
                 p("Select up to three locations to plot versus time."),
                 splitLayout(
                   selectInput("select_county1_rel", label = NULL, choices = county_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_county2_rel", label = NULL, choices = county_choices,
                               selected = "DOC", multiple=FALSE),
                   selectInput("select_county3_rel", label = NULL, choices = county_choices,
                               selected = "Barnstable", multiple=FALSE)
                 )),
               withSpinner(plotOutput("releases_v_time_plot"), type=4, color="#b5b5b5", size=0.5)
      ),
      
      tabPanel("Total Positives", 
               wellPanel(
                 p("Select kind of individual:"),
                 selectInput("select_positive", label = NULL, 
                             choices = c("All", "Prisoners", "Staff", "Total"),
                             selected = "All", multiple=FALSE)
               ),
               h2(textOutput("n_positive_str"), align="center"),
               p("Reports of",
                 textOutput("type_positive", inline=T),
                 "tested", strong("positive"),
                 "for COVID-19 pursuant to SJC 12926", align="center"),
               withSpinner(plotOutput("all_positives_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Total Tests", 
               wellPanel(
                 p("Select kind of individual:"),
                 selectInput("select_tested", label = NULL, 
                             choices = c("All", "Prisoners", "Staff", "Total"),
                             selected = "All", multiple=FALSE)
               ),
               h2(textOutput("n_tests_str"), align="center"),
               p("Reports of",
                 textOutput("type_tested", inline=T),
                 "tested for COVID-19  pursuant to SJC 12926", 
                 align="center"),
               withSpinner(plotOutput("all_tests_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      # tabPanel("Infection Rates", align="center",
      #          wellPanel(
      #            p("Select up to three locations to plot versus time."),
      #            splitLayout(
      #              selectInput("select_county_inf1", label = NULL, 
      #                          choices = infection_choices,
      #                          selected = "MA Total", multiple=FALSE),
      #              selectInput("select_county_inf2", label = NULL, 
      #                          choices = infection_choices,
      #                          selected = "MA Prisoner Total", multiple=FALSE),
      #              selectInput("select_county_inf3", label = NULL, 
      #                          choices = infection_choices,
      #                          selected = "DOC", multiple=FALSE)
      #            )),
      #          withSpinner(plotOutput("infections_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
      #          em('Data on COVID-19 cases in Massachusetts ("MA Total") from', 
      #             a(href="https://www.mass.gov/info-details/covid-19-cases-quarantine-and-monitoring", 
      #               "mass.gov")
      #             )
      # ),
      
      # tabPanel("Trends Over Time by Location",
      #          wellPanel(
      #            p("Select value to plot versus time."),
      #            selectInput("select_y_v_time", label = NULL, 
      #                        choices = c("Releases", "Tests", "Positive Cases"),
      #                          selected = "Releases", multiple=FALSE),
      #            p("Select up to three locations to plot versus time."),
      #            splitLayout(
      #              selectInput("select_county1", label = NULL, choices = county_choices,
      #                          selected = "All", multiple=FALSE),
      #              selectInput("select_county2", label = NULL, choices = county_choices,
      #                          selected = "DOC", multiple=FALSE),
      #              selectInput("select_county3", label = NULL, choices = county_choices,
      #                          selected = "Barnstable", multiple=FALSE)
      #            )),
      #          withSpinner(plotOutput("releases_v_time_plot"), type=4, color="#b5b5b5", size=0.5)
      #          # ),

      tabPanel("Mapping County Trends", align="center",
               wellPanel(
                 p("Select value to plot."),
                 selectInput("select_y_plot", label = NULL, 
                             choices = c("Releases", "Tests", "Positive Cases"),
                             selected = "Releases", multiple=FALSE)
                 ),
               em("All maps reflect data from county jails and HOCs alone,",
                  "as the Massachsuetts DOC is reporting only state-level, not facility-level, data."),
               em("Maps of tests and positive cases include both prisoner and staff data."),
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
               div(id="dev-warning",
                   wellPanel(
                     fluidRow(
                     column(1, icon('exclamation-triangle')),
                     column(11, em("Please be aware that the data below is entered in a",
                        "non-cumulative manner. Values in each row reflect not updated tallies of",
                        "cases, tests, or releases on the noted day, but rather the number of", strong("new"),
                        "cases, tests, or releases in a 24-hour period.", style="margin-top:0px"))
                     )
                   )
               ),
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
    
    em("Data last downloaded:", textOutput("latest_time_str", inline=T), 
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
    mutate_at(vars(starts_with("N "), starts_with("Total"), 
                   matches("Population")), 
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
           all_released = `N Released Pre-Trial` + 
             `N Released Sentenced` + 
             `N Released Parole`,
           all_positive = `Total Positive`,
           all_tested = `Total Tested`)
  
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
  
  # Calc MA rates
  ma_dropbox_url = "https://www.dropbox.com/s/xwp85c0efmlgq5r/MA_infection_rates.xlsx?dl=1"
  
  # Download excel spreadsheet from URL and read as DF
  GET(ma_dropbox_url, write_disk(tf_ma <- tempfile(fileext = ".xlsx")))
  ma_df <- read_excel(tf_ma) %>% 
    dplyr::select(Date, cumul_rate_10000) %>%
    mutate(cumul_rate_10000 = as.numeric(cumul_rate_10000),
           Date = as.Date(Date),
           County = "MA Total")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # All Releases
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  released_df <- sjc_num_df %>%
    pivot_longer(cols=matches("Released", ignore.case=F),
                 names_to="release_type",
                 names_prefix="N Released ")
  
  # Determine which variable to plot
  select_release <- reactive({ input$select_release })

  output$all_releases_plot <- renderPlot({
    
    if (select_release() == "All") {
      g <- released_df %>%
        group_by(County, release_type) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, 
                   fill =release_type, 
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = T) + 
        labs(fill = "") + 
        theme(legend.position = "top",
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA))
    
      output$n_releases_str <- renderText({n_released})
      
    } else if (select_release() %in% c("Parole", "Pre-Trial", "Sentenced")) {
      g <- released_df %>%
        filter(release_type == select_release()) %>%
        group_by(County, release_type) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, fill = as.factor(1),
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = F) +
        geom_bar_text(contrast=T, family="gtam")
      
      output$n_releases_str <- renderText({
        released_df %>%
          filter(release_type == select_release()) %>%
          pull(value) %>%
          sum()
        })
      
    } else if (select_release() == "Total") {
      g <- released_df %>%
        group_by(County) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, fill = as.factor(1),
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = F) +
        geom_bar_text(contrast=T, family="gtam")
      
      output$n_releases_str <- renderText({n_released})
      
    }
    
    g +
      labs(y = "Prisoners Released", x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14)) +
      scale_fill_manual(values = c("#0055aa", "#fbb416", "#a3dbe3"))
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Releases v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which counties to plot
  cnty_to_plot_rel <- reactive({
    c(input$select_county1_rel,
      input$select_county2_rel,
      input$select_county3_rel)
  })
  
  # Plot
  output$releases_v_time_plot <- renderPlot({
    
    df_by_county %>%
      filter(County %in% cnty_to_plot_rel()) %>%
      group_by(County) %>%
      mutate(cumul_rel = cumsum(all_released)) %>%
    ggplot(aes(x=Date, y = cumul_rel, color=County)) +
      geom_path(size=2, show.legend = T, alpha=0.8) +
      geom_point(size=3) +
      labs(x = "", y = "Total Prisoners Released", color="",
           title = paste("Prisoners Released over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
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
  # All Positives
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  positive_df <- sjc_num_df %>%
    mutate(`N Positive - Staff` = `N Positive - COs` + 
             `N Positive - Staff` + `N Positive - Contractor`) %>%
    rename(`N Positive - Prisoners`=`N Positive - Detainees/Inmates`) %>%
    dplyr::select(-`N Positive - COs`, -`N Positive - Contractor`) %>%
    pivot_longer(cols=matches("N Positive", ignore.case=F),
                 names_to="positive_type",
                 names_prefix="N Positive - ")
  
  # Determine which variable to plot
  select_positive <- reactive({ input$select_positive })

  output$all_positives_plot <- renderPlot({
    
    if (select_positive() == "All") {
      g <- positive_df %>%
        group_by(County, positive_type) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, 
                   fill =positive_type, 
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = T) +
        labs(fill = "") + 
        theme(legend.position = "top",
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA))
      
      output$n_positive_str <- renderText({n_positive})
      output$type_positive <- renderText({"prisoners and staff"})
      
    } else if (select_positive() %in% c("Prisoners", "Staff")) {
      g <- positive_df %>%
        filter(positive_type == select_positive()) %>%
        group_by(County, positive_type) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, fill = as.factor(1),
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = F) +
        geom_bar_text(contrast=T, family="gtam")
      
      output$n_positive_str <- renderText({
        positive_df %>%
          filter(positive_type == select_positive()) %>%
          pull(value) %>%
          sum()
      })
      
      output$type_positive <- renderText({tolower(select_positive())})
      
    } else if (select_positive() == "Total") {
      g <- positive_df %>%
        group_by(County) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, fill = as.factor(1),
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = F) +
        geom_bar_text(contrast=T, family="gtam")
      
      output$n_positive_str <- renderText({n_positive})
      output$type_positive <- renderText({"prisoners and staff"})
      
    }
    
    g +
      labs(y = "Tested Positive for COVID-19", x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14)) +
      scale_fill_manual(values = c("#0055aa", "#fbb416", "#a3dbe3"))
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # All Tests
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  tested_df <- sjc_num_df %>%
    mutate(`N Tested - Staff` = `N Tested - COs` + 
             `N Tested - Staff` + `N Tested - Contractors`) %>%
    rename(`N Tested - Prisoners`=`N Tested - Detainees/Inmates`) %>%
    dplyr::select(-`N Tested - COs`, -`N Tested - Contractors`) %>%
    pivot_longer(cols=matches("N Tested", ignore.case=F),
                 names_to="tested_type",
                 names_prefix="N Tested - ")
  
  # Determine which variable to plot
  select_tested <- reactive({ input$select_tested })
  
  output$all_tests_plot <- renderPlot({
    
    if (select_tested() == "All") {
      g <- tested_df %>%
        group_by(County, tested_type) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, 
                   fill =tested_type, 
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = T) +
        labs(fill = "") + 
        theme(legend.position = "top",
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA))
      
      output$n_tests_str <- renderText({n_tested})
      output$type_tested <- renderText({"prisoners and staff"})
      
    } else if (select_tested() %in% c("Prisoners", "Staff")) {
      g <- tested_df %>%
        filter(tested_type == select_tested()) %>%
        group_by(County, tested_type) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, fill = as.factor(1),
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = F) +
        geom_bar_text(contrast=T, family="gtam")
      
      output$n_tests_str <- renderText({
        tested_df %>%
          filter(tested_type == select_tested()) %>%
          pull(value) %>%
          sum()
      })
      
      output$type_tested <- renderText({tolower(select_tested())})
      
    } else if (select_tested() == "Total") {
      g <- tested_df %>%
        group_by(County) %>%
        summarize(sum_value = sum(value)) %>%
        ggplot(aes(x=factor(County, levels=counties), 
                   y=sum_value, fill = as.factor(1),
                   label=sum_value)) +
        geom_col(position = "stack", show.legend = F) +
        geom_bar_text(contrast=T, family="gtam")
      
      output$n_tests_str <- renderText({n_tested})
      output$type_tested <- renderText({"prisoners and staff"})
      
    }
    
    g +
      labs(y = "Tested for COVID-19", x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14)) +
      scale_fill_manual(values = c("#0055aa", "#fbb416", "#a3dbe3"))
    
  })
  
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # # 😷 Infection Rates 😷
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 
  # infection_df_all <- sjc_num_df %>%
  #   group_by(Date) %>%
  #   summarize(in_det_positive = sum(`N Positive - Detainees/Inmates`),
  #             pop = sum(`Total Population`)) %>%
  #   mutate(cumul_in_det_positive = cumsum(in_det_positive)) %>%
  #   mutate(County = "MA Prisoner Total",
  #          cumul_rate_10000 = cumul_in_det_positive / pop * 10000) %>%
  #   dplyr::select(Date, County, cumul_rate_10000)
  # 
  # infection_df_by_county <- sjc_num_df %>%
  #   mutate(in_det_positive = `N Positive - Detainees/Inmates`,
  #          pop = `Total Population`) %>%
  #   group_by(County) %>%
  #   mutate(cumul_in_det_positive = cumsum(in_det_positive),
  #          cumul_rate_10000 = cumul_in_det_positive / pop * 10000) %>%
  #   dplyr::select(Date, County, cumul_rate_10000) %>%
  #   bind_rows(infection_df_all) %>%
  #   bind_rows(ma_df)
  # 
  # # Determine which incidents to plot
  # cnty_to_plot_inf <- reactive({
  #   c(input$select_county_inf1,
  #     input$select_county_inf2,
  #     input$select_county_inf3)
  # })
  # 
  # # Plot
  # output$infections_v_time_plot <- renderPlot({
  #   
  #   infection_df_by_county %>%
  #     filter(County %in% cnty_to_plot_inf()) %>%
  #     ungroup() %>%
  #     mutate(County = factor(County, levels=infection_choices)) %>%
  #   ggplot(aes(x=Date, y = cumul_rate_10000, color = County)) +
  #     geom_path(size=1.3, show.legend = T) +
  #     geom_point() +
  #     labs(x = "", y = "Infection Rate per 10,000", color="",
  #          title = "Prisoner Infection Rate Over Time",
  #          subtitle = "Postive Cases per 10,000 Prisoners") +
  #     theme(plot.title= element_text(family="gtam", face='bold'),
  #           text = element_text(family="gtam", size = 16),
  #           plot.margin = unit(c(1,1,4,1), "lines"),
  #           legend.position = c(.5, -.22), legend.direction="horizontal",
  #           legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
  #           legend.key.width = unit(1, "cm"),
  #           legend.text = element_text(size=16)) +
  #     scale_x_date(date_labels = "%b %e ") +
  #     scale_color_manual(values=c("black", "#ef404d", "#0055aa")) +
  #     coord_cartesian(clip = 'off')
  #   
  # })
  
  
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # # X v. Time
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 
  # # Determine which incidents to plot
  # cnty_to_plot <- reactive({
  #   c(input$select_county1,
  #     input$select_county2,
  #     input$select_county3)
  # })
  # 
  # # Determine which variable to plot
  # y_to_plot_time <- reactive({ input$select_y_v_time })
  # 
  # # Plot
  # output$releases_v_time_plot <- renderPlot({
  #   
  #   if (y_to_plot_time() == "Releases") {
  #     df_by_county <- df_by_county %>%
  #       mutate(value = all_released)
  #     y_axis_label <- "Number Released"
  #   } else if (y_to_plot_time() == "Tests") {
  #     df_by_county <- df_by_county %>%
  #       mutate(value = all_tested)
  #     y_axis_label <- "Number Tested"
  #   } else if (y_to_plot_time() == "Positive Cases") {
  #     df_by_county <- df_by_county %>%
  #       mutate(value = all_positive)
  #     y_axis_label <- "Number Tested Positive"
  #   }
  #   
  #   df_by_county %>%
  #     filter(County %in% cnty_to_plot()) %>%
  #     ggplot(aes(x=Date, y = cumsum(value), color=County)) +
  #     geom_path(size=1.3, show.legend = T, alpha=0.7) +
  #     geom_point() +
  #     labs(x = "", y = paste("Total", y_axis_label), color="",
  #          title = paste(y_to_plot_time(), "over Time"),
  #          subtitle="Cumulative pursuant to SJC 12926") +
  #     theme(plot.title= element_text(family="gtam", face='bold'),
  #           text = element_text(family="gtam", size = 16),
  #           plot.margin = unit(c(1,1,4,1), "lines"),
  #           legend.position = c(.5, -.22), legend.direction="horizontal",
  #           legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
  #           legend.key.width = unit(1, "cm"),
  #           legend.text = element_text(size=16)) +
  #     scale_x_date(date_labels = "%b %e ") +
  #     scale_color_manual(values=c("black", "#ef404d", "#0055aa")) +
  #     coord_cartesian(clip = 'off')
  # 
  # })

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
    {sjc_df},
    options = list(scrollX = TRUE), 
    filter = 'top'
    )
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # ⬇️ Download XLSX ⬇️
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  observeEvent(input$link_to_external, {
    updateTabsetPanel(session, "panels", "External Resources")
  })
  
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
