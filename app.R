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
library(plotly)

source("plotly_builders.R")

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
fac_choices <- c("--", "DOC Total**", "All DOC Facilities", 'Boston Pre', 'BSH', 
                 'LSH', 'MASAC', 'MCI-C', 'MCI-CJ', 'MCI-F', 'MCI-Norfolk', 
                 'MCI-Shirley', 'MTC', 'NCCI-Gardn', 'OCCC', 'Pondville', 
                 'SBCC', 'SMCC')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# UI
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ui <- fluidPage(theme = "sjc_12926_app.css",
      
  # Add favicon          
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
  ),
  
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
               p("The data displayed here are compiled from information supplied by",
                 "county sheriffs and the Massachusetts Department of Correction (DOC) in",
                 a(href="https://www.aclum.org/en/cases/committee-public-counsel-services-v-chief-justice-trial-court",
                   target="_blank",
                   "Committee for Public Counsel Services (CPCS) v. Chief Justice of the Trial Court, SJC-12926,"),
                 "a case brought by the Committee for Public Counsel Services",
                 "(the Massachusetts public defenders), the Massachusetts",
                 "Association of Criminal Defense Lawyers, and the ACLU of",
                 "Massachusetts. This case seeks releases of incarcerated people",
                 "to protect them, correctional staffs, and the public from the COVID-19 pandemic."),
               
               p("Following an",
                 a(href="https://www.mass.gov/doc/sjc-12926-opinion/download",
                   target="_blank", "opinion"),
                 "by the Massachusetts Supreme Judicial Court in this case on",
                 "April 3, 2020, sheriffs and the DOC have been required to",
                 "provide daily reports that shed light on the COVID-19 situation",
                 "inside Massachusetts prisons, jails, and houses of correction."),
               
               p("Learn more about the history, details, and outcomes of the ",
                 "case at the ACLU of Massachusetts website:",
                 a(href="https://www.aclum.org/en/cases/committee-public-counsel-services-v-chief-justice-trial-court",
                   target="_blank",
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
                </ul>")
               ),
      
      tabPanel("Total Releases", 
               wellPanel(id="internal_well",
                 p("Select kind of prisoner:"),
                 selectInput("select_release", label = NULL, 
                           choices = c("All", "Pre-Trial", "Sentenced", "Total"),
                           selected = "All", multiple=FALSE),
                 em('Exact number of releases per county annotated in',
                    '"Pre-Trial", "Sentenced", and "Total" plots.')
                 ),
               h2(textOutput("n_releases_str"), align="center"),
               p("Prisoners released pursuant to SJC 12926", align="center"),
               withSpinner(plotlyOutput("all_releases_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
      
      tabPanel("Releases Over Time",
               conditionalPanel(
                 condition = "input.select_county1_rel == 'Hampshire'|input.select_county2_rel == 'Hampshire'|input.select_county3_rel == 'Hampshire'",
                 div(id="dev-warning",
                     wellPanel(
                       fluidRow(
                         column(1, icon('exclamation-triangle')),
                         column(11, h4("Hampshire County Releases"),
                                em("We are awaiting verification from Hampshire County",
                                   "that might lead to changes in their daily reports of releases.", 
                                   style="margin-top:0px"))
                       )
                     )
                 )
               ),
               wellPanel(id="internal_well",
                 p("Select up to three locations to plot versus time."),
                 splitLayout(
                   selectInput("select_county1_rel", label = NULL, choices = county_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_county2_rel", label = NULL, choices = county_choices,
                               selected = "DOC", multiple=FALSE),
                   selectInput("select_county3_rel", label = NULL, choices = county_choices,
                               selected = "Barnstable", multiple=FALSE)
                 )),
               withSpinner(plotlyOutput("releases_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
      ),
      
      tabPanel("Total Tests", 
               conditionalPanel(
                 condition = "input.select_tested == 'All'|input.select_tested == 'Staff'|input.select_tested == 'Total'",
                 div(id="dev-info",
                     wellPanel(
                       fluidRow(
                         column(1, icon('info-circle')),
                         column(11, h4("DOC Staff Testing"),
                                em("Please note the DOC is not reporting tests of staff.", 
                                   style="margin-top:0px"))
                       )
                     )
                 )
               ),
               wellPanel(id="internal_well",
                 p("Select kind of individual:"),
                 selectInput("select_tested", label = NULL, 
                             choices = c("All", "Prisoners", "Staff", "Total"),
                             selected = "All", multiple=FALSE),
                 em('Exact number of tests per county annotated in "Prisoners",',
                    '"Staff", and "Total" plots.')
               ),
               h2(textOutput("n_tests_str"), align="center"),
               p("Reports of",
                 textOutput("type_tested", inline=T),
                 "tested for COVID-19  pursuant to SJC 12926", 
                 align="center"),
               withSpinner(plotlyOutput("all_tests_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
      
      tabPanel("Tests Over Time",
               wellPanel(id="internal_well",
                 p("Select up to three locations to plot versus time."),
                 splitLayout(
                   selectInput("select_county1_test", label = NULL, choices = county_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_county2_test", label = NULL, choices = county_choices,
                               selected = "DOC", multiple=FALSE),
                   selectInput("select_county3_test", label = NULL, choices = county_choices,
                               selected = "Barnstable", multiple=FALSE)
                 )),
               withSpinner(plotlyOutput("tests_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
      ),
      
      tabPanel("Total Positive Tests", 
               wellPanel(id="internal_well",
                         p("Select kind of individual:"),
                         selectInput("select_positive", label = NULL, 
                                     choices = c("All", "Prisoners", "Staff", "Total"),
                                     selected = "All", multiple=FALSE),
                         em('Exact number of positive tests per county annotated in',
                            '"Prisoners", "Staff", and "Total" plots.')
               ),
               h2(textOutput("n_positive_str"), align="center"),
               p("Reports of",
                 textOutput("type_positive", inline=T),
                 "tested", strong("positive"),
                 "for COVID-19 pursuant to SJC 12926", align="center"),
               withSpinner(plotlyOutput("all_positives_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
      
      tabPanel("Positive Tests Over Time",
               wellPanel(id="internal_well",
                         p("Select up to three locations to plot versus time."),
                         splitLayout(
                           selectInput("select_county1_pos", label = NULL, choices = county_choices,
                                       selected = "All", multiple=FALSE),
                           selectInput("select_county2_pos", label = NULL, choices = county_choices,
                                       selected = "DOC", multiple=FALSE),
                           selectInput("select_county3_pos", label = NULL, choices = county_choices,
                                       selected = "Barnstable", multiple=FALSE)
                         )),
               withSpinner(plotlyOutput("positives_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
      ),
      
      tabPanel("Incarcerated Population Over Time",
               wellPanel(id="internal_well",
                         p("Select up to three locations to plot versus time."),
                         splitLayout(
                           selectInput("select_county1_pop", label = NULL, choices = county_choices,
                                       selected = "All", multiple=FALSE),
                           selectInput("select_county2_pop", label = NULL, choices = county_choices,
                                       selected = "--", multiple=FALSE),
                           selectInput("select_county3_pop", label = NULL, choices = county_choices,
                                       selected = "--", multiple=FALSE)
                         )),
               withSpinner(plotlyOutput("pop_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
      ),
      
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
      #          withSpinner(plotlyOutput("infections_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
      #          em('Data on COVID-19 cases in Massachusetts ("MA Total") from', 
      #             a(href="https://www.mass.gov/info-details/covid-19-cases-quarantine-and-monitoring", 
      #               "mass.gov")
      #             )
      # ),
      
      tabPanel("Mapping County Trends", 
               wellPanel(id="internal_well",
                 p("Select value to plot."),
                 selectInput("select_y_plot", label = NULL, 
                             choices = c("Releases", "Tests", "Positive Cases"),
                             selected = "Releases", multiple=FALSE)
                 ),
               em("All maps reflect data from county jails and HOCs alone,",
                  "as the Massachsuetts DOC is reporting only state-level, not facility-level, data."),
               em("Maps of tests and positive cases include both prisoner and staff data."),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data."),
               withSpinner(leafletOutput("county_maps"),
                           type=4, color="#b5b5b5", size=0.5)
               ),
      
      tabPanel("DOC Facilities: Positive Test Totals", 
               wellPanel(id="internal_well",
                         p("Select kind of individual:"),
                         selectInput("select_positive_fac", label = NULL, 
                                     choices = c("All", "Prisoners", "Staff", "Total"),
                                     selected = "All", multiple=FALSE),
                         em('Exact number of positive tests per facility annotated in',
                            '"Prisoners", "Staff", and "Total" plots.')
               ),
               div(align="center",
                 h2(textOutput("n_positive_DOC_str")),
                 p("Reports of",
                   textOutput("type_positive_fac", inline=T),
                   "tested", strong("positive"),
                   "for COVID-19 at individual DOC facilities pursuant to SJC 12926", align="center"),
                 em("*The DOC only began reporting facility-level data on April 13.",
                    "See the Total Positive Tests page for longer-term totals.")
                 ),
               withSpinner(plotlyOutput("DOC_positives_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
      
      tabPanel("DOC Facilities: Positive Tests v. Time", 
               wellPanel(id="internal_well",
                         p("Select up to three facilities to plot versus time.*"),
                         splitLayout(
                           selectInput("select_fac1", label = NULL, choices = fac_choices,
                                       selected = "DOC Total**", multiple=FALSE),
                           selectInput("select_fac2", label = NULL, choices = fac_choices,
                                       selected = "All DOC Facilities", multiple=FALSE),
                           selectInput("select_fac3", label = NULL, choices = fac_choices,
                                       selected = "MTC", multiple=FALSE) 
                         ),
                         em("*The DOC only began reporting facility-level prisoner data on April 13,",
                            "and facility-level staff data on April 15.",
                            "See the Positive Tests Over Time page for longer-term DOC tracking"),
                         em('**DOC Total includes staff categorized as "Other"',
                            'while the facility total does not. Additionally,',
                            'some DOC staff are not assigned to a particular facility.',
                            style="display: block; margin-top: 1rem;")
                         ),
               withSpinner(plotlyOutput("DOC_time_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
      
      tabPanel("Explore Data",
               div(id="dev-warning",
                   wellPanel(
                     fluidRow(
                       column(1, icon('exclamation-triangle')),
                       column(11, h4("Hampshire County Releases"),
                              em("We are awaiting verification from Hampshire County",
                                 "that might lead to changes in their daily reports of releases.", 
                                 style="margin-top:0px"))
                     )
                   )
               ),
               div(id="dev-info",
                   wellPanel(
                     fluidRow(
                     column(1, icon('info-circle')),
                     column(11, em("Please be aware that the data below is entered in a",
                        "non-cumulative manner. Values in each row reflect not updated tallies of",
                        "cases, tests, or releases on the noted day, but rather the number of", strong("new"),
                        "cases, tests, or releases in a 24-hour period.", style="margin-top:0px"))
                     )
                   )
               ),
               em("Note: Empty cells denote that the given county",
                  "did not report that value, while cells with value 0 mean 0 was reported."),
               em("Additionally, please note that prisoner deaths due to COVID-19 are not included in these data."),
               br(), br(),
               tabsetPanel(type = "tabs",
                           tabPanel("Counties and DOC", 
                                    withSpinner(dataTableOutput("df_table"), type=4, color="#b5b5b5", size=0.5)),
                           tabPanel("DOC Facilities", 
                                    withSpinner(dataTableOutput("DOC_df_table"), type=4, color="#b5b5b5", size=0.5))
                           )
      ),
      
      tabPanel("Download Data",
               p("Data analysts at the ACLU of Massachusetts compile the daily",
                 "reports recieved from the counties and DOC into a single",
                 "spreadsheet. This spreadsheet is what we source for all",
                 "visualizations on this site."),
               p(strong("Current file size: "), textOutput("n_rows", inline=T), "entries"),
               p("The accumulated database is available for download here:"),
               downloadButton("downloadData", "Download XLSX"))
      )
    ),
  
  div(id="footer",
    em("\n\nData last downloaded:", textOutput("latest_time_str", inline=T), 
         align="right", style="opacity: 0.6;"),
    br(),
    hr(),
    div(align="center",
      a(href="https://www.aclum.org/", target="_blank",
        img(src="Logo_CMYK_Massachusetts_Massachusetts.png", height="50px", 
            style="display: inline; margin: 10px;")),
      a(href="https://www.data.aclum.org/",  target="_blank",
        img(src="D4J-logo.png", height="50px", 
            style="display: inline; margin: 10px;"))),
    p("Please contact lchambers@aclum.org with questions.", align="center", style="opacity: 0.6;")
  )
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
    mutate(Date = as.Date(Date)) %>%
    # Protect against empty cells messing things up
    filter(Date >= ymd(20200327))
  
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
             `N Released Sentenced`,
           all_positive = `Total Positive`,
           all_tested = `Total Tested`,
           County = factor(County, levels=counties))
  
  # Determine whether to plot last day
  last_date_entered <- max(sjc_num_df$Date, na.rm=T)
  
  all_reports_in <- sjc_num_df %>%
    filter(Date == last_date_entered) %>%
    nrow() > 12
  
  more_than_2_days_since <- as.Date(now()) - last_date_entered >= days(2)
  
  show_last_day <- all_reports_in | more_than_2_days_since
  
  if (!show_last_day) {
    sjc_num_df <- sjc_num_df %>%
      filter(Date < last_date_entered) 
  }
  
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
  
  # # Download excel spreadsheet from URL and read as DF
  # GET(ma_dropbox_url, write_disk(tf_ma <- tempfile(fileext = ".xlsx")))
  # ma_df <- read_excel(tf_ma) %>% 
  #   dplyr::select(Date, cumul_rate_10000) %>%
  #   mutate(cumul_rate_10000 = as.numeric(cumul_rate_10000),
  #          Date = as.Date(Date),
  #          County = "MA Total")
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # All Releases
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  released_df <- sjc_num_df %>%
    pivot_longer(cols=matches("Released", ignore.case=F),
                 names_to="type",
                 names_prefix="N Released ")
  
  # Determine which variable to plot
  select_release <- reactive({ input$select_release })

  output$all_releases_plot <- renderPlotly({
    
    if (select_release() == "All") {
      
      output$n_releases_str <- renderText({n_released})
      
      stacked_bar_plot(released_df, 
                      "Prisoners Released",
                      "County")
      
    } else if (select_release() %in% c("Pre-Trial", "Sentenced")) {
      
      output$n_releases_str <- renderText({
        released_df %>%
          filter(type == select_release()) %>%
          pull(value) %>%
          sum()
      })
      
      single_bar_plot(released_df, 
                      select_release(), 
                      paste(select_release(), "Prisoners Released"),
                      "County")
      
    } else if (select_release() == "Total") {
      
      output$n_releases_str <- renderText({n_released})
      
      single_bar_plot(released_df, 
                      select_release(), "Prisoners Released",
                      "County")
      
    }
    
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
  output$releases_v_time_plot <- renderPlotly({
    
    g <- df_by_county %>%
      filter(County %in% cnty_to_plot_rel()) %>%
      group_by(County) %>%
      mutate(cumul = cumsum(all_released)) %>%
    ggplot(aes(x=Date, y = cumul, color=County)) +
      geom_path(size=2, show.legend = T, alpha=0.8) +
      geom_point(size=3) +
      labs(x = "", y = "Total Prisoners Released", color="",
           title = paste("Prisoners Released over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="GT America", face='bold'),
            text = element_text(family="GT America", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), legend.direction="horizontal",
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off') +
      ylim(0, NA)
    
    lines_plotly_style(g, "Prisoners Released", "County")
    
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
                 names_to="type",
                 names_prefix="N Positive - ")
  
  # Determine which variable to plot
  select_positive <- reactive({ input$select_positive })

  output$all_positives_plot <- renderPlotly({
    
    if (select_positive() == "All") {
      output$n_positive_str <- renderText({n_positive})
      output$type_positive <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(positive_df, 
                       "Tested Positive",
                       "County")
      
    } else if (select_positive() %in% c("Prisoners", "Staff")) {
      output$n_positive_str <- renderText({
        positive_df %>%
          filter(type == select_positive()) %>%
          pull(value) %>%
          sum()
      })
      output$type_positive <- renderText({tolower(select_positive())})
      
      single_bar_plot(positive_df, 
                      select_positive(), 
                      paste(select_positive(), "Tested Positive"),
                      "County")

    } else if (select_positive() == "Total") {
      output$n_positive_str <- renderText({n_positive})
      output$type_positive <- renderText({"prisoners and staff"})
      
      single_bar_plot(positive_df, 
                      select_positive(), "Prisoners & Staff Tested Positive",
                      "County")
    }
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Positives v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which counties to plot
  cnty_to_plot_pos <- reactive({
    c(input$select_county1_pos,
      input$select_county2_pos,
      input$select_county3_pos)
  })
  
  # Plot
  output$positives_v_time_plot <- renderPlotly({
    
    annotate_tests_df <- df_by_county %>%
      filter(County %in% cnty_to_plot_pos()) %>%
      group_by(County) %>%
      summarize(all_tests = sum(all_tested))
    
    annotate_tests <- paste0(paste0(annotate_tests_df$County, ": ", 
                                    annotate_tests_df$all_tests, " tested"), 
                             collapse="\n")
    
    pos_to_plot <- df_by_county %>%
      filter(County %in% cnty_to_plot_pos()) %>%
      group_by(County) %>%
      mutate(cumul = cumsum(all_positive))
      
    g <- pos_to_plot %>%
      ggplot(aes(x=Date, y = cumul, color=County)) +
        geom_path(size=2, show.legend = T, alpha=0.8) +
        geom_point(size=3) +
        annotate("label", min(pos_to_plot$Date), Inf, label=annotate_tests,
                 vjust=2, hjust=0, fill="grey", alpha=0.5, 
                 label.size=NA, label.r=unit(0, "cm"), label.padding = unit(0.5, "lines")) +
        labs(x = "", y = "Total Prisoners & Staff\nTested Positive", color="",
             title = paste("Positive COVID-19 Tests over Time"),
             subtitle="Cumulative pursuant to SJC 12926") +
        theme(plot.title= element_text(family="GT America", face='bold'),
              text = element_text(family="GT America", size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              legend.position = c(.5, -.22), legend.direction="horizontal",
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
              legend.key.width = unit(1, "cm"),
              legend.text = element_text(size=16)) +
        scale_x_date(date_labels = "%b %e ") +
        scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
        coord_cartesian(clip = 'off') +
        ylim(0, NA)
    
    lines_plotly_style(g, "Prisoners & Staff Tested Positive", "County", 
                       annotation=TRUE)
    
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
                 names_to="type",
                 names_prefix="N Tested - ")
  
  # Determine which variable to plot
  select_tested <- reactive({ input$select_tested })
  
  output$all_tests_plot <- renderPlotly({
    
    if (select_tested() == "All") {
      
      output$n_tests_str <- renderText({n_tested})
      output$type_tested <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(tested_df, 
                       "Tested",
                       "County")
      
    } else if (select_tested() %in% c("Prisoners", "Staff")) {
      output$n_tests_str <- renderText({
        tested_df %>%
          filter(type == select_tested()) %>%
          pull(value) %>%
          sum()
      })
      output$type_tested <- renderText({tolower(select_tested())})
      
      single_bar_plot(tested_df, 
                      select_tested(), 
                      paste(select_tested(), "Tested"),
                      "County")
      
    } else if (select_tested() == "Total") {
      output$n_tests_str <- renderText({n_tested})
      output$type_tested <- renderText({"prisoners and staff"})
      
      single_bar_plot(tested_df, 
                      select_tested(),
                      "Prisoners & Staff Tested",
                      "County")
      
    }
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Tests v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which counties to plot
  cnty_to_plot_test <- reactive({
    c(input$select_county1_test,
      input$select_county2_test,
      input$select_county3_test)
  })
  
  # Plot
  output$tests_v_time_plot <- renderPlotly({
    
    g <- df_by_county %>%
      filter(County %in% cnty_to_plot_test()) %>%
      group_by(County) %>%
      mutate(cumul = cumsum(all_tested)) %>%
    ggplot(aes(x=Date, y = cumul, color=County)) +
      geom_path(size=2, show.legend = T, alpha=0.8) +
      geom_point(size=3) +
      labs(x = "", y = "Total Prisoners & Staff Tested", color="",
           title = paste("COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="GT America", face='bold'),
            text = element_text(family="GT America", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), legend.direction="horizontal",
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off') +
      ylim(0, NA)
    
    lines_plotly_style(g, "Prisoners & Staff Tested", "County")
    
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Populations v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which counties to plot
  cnty_to_plot_pop <- reactive({
    c(input$select_county1_pop,
      input$select_county2_pop,
      input$select_county3_pop)
  })
  
  all_pop_df <- sjc_num_df %>%
    mutate(pop = `Total Population`) %>%
    filter(pop != 0) %>%
    group_by(Date) %>%
    filter(n() == 14) %>%
    summarize(pop = sum(pop)) %>%
    mutate(County = "All")
  
  pop_df <-  sjc_num_df %>%
    mutate(pop = `Total Population`) %>%
    group_by(Date, County) %>%
    summarize(pop = sum(pop)) %>%
    bind_rows(all_pop_df)
  
  # Plot
  output$pop_v_time_plot <- renderPlotly({
    
    g <-pop_df %>%
      mutate(pop = na_if(pop, 0)) %>%
      filter(Date >= ymd(20200407),
             County %in% cnty_to_plot_pop(),
             !is.na(pop)) %>%
      ggplot(aes(x=Date, y = pop, color=County)) +
      geom_path(size=2, show.legend = T, alpha=0.8) +
      geom_point(size=3) +
      labs(x = "", y = "Total Prisoners", color="",
           title = paste("Incarcerated Populations over Time")) +
      theme(plot.title= element_text(family="GT America", face='bold'),
            text = element_text(family="GT America", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), legend.direction="horizontal",
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ", limits=c(ymd(20200407),NA)) +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off') 
    
    lines_plotly_style(g, "Incarcerated Population", "County",
                       subtitle=F)
    
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
  # output$infections_v_time_plot <- renderPlotly({
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
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # DOC Data
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  sjc_DOC_df <- read_excel(tf, sheet=2) %>%
    mutate_if(is.character, ~na_if(., 'NA')) %>%
    mutate(Date = as.Date(Date)) %>%
    mutate_at(vars(starts_with("N "), starts_with("Total")),
              as.numeric)
  
  sjc_DOC_num_df <- sjc_DOC_df %>%
    rename(fac = `DOC Facility`,
           all_positive = `Total Positive`) %>%
    select(-Notes)
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # DOC Facility Totals
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  fac_positive_df <- sjc_DOC_num_df %>%
    rename(`N Positive - Prisoners`=`N Positive - Detainees/Inmates`,
           Facility = fac) %>%
    pivot_longer(cols=matches("N Positive", ignore.case=F),
                 names_to="type",
                 names_prefix="N Positive - ") %>%
    filter(!is.na(value))
    
  
  # Determine which variable to plot
  select_positive_fac <- reactive({ input$select_positive_fac })
  
  # Plot
  output$DOC_positives_plot <- renderPlotly({
    
    if (select_positive_fac() == "All") {
      
      output$n_positive_DOC_str <- renderText({
        paste0(as.character(sum(sjc_DOC_num_df$all_positive, na.rm=T)), "*")
      })
      
      output$type_positive_fac <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(fac_positive_df, 
                       "Tested Positive",
                       "Facility")
      
    } else if (select_positive_fac() %in% c("Prisoners", "Staff")) {
      
      output$n_positive_DOC_str <- renderText({
        fac_positive_df %>%
          filter(type == select_positive_fac()) %>%
          pull(value) %>%
          sum(na.rm=T) %>%
          as.character() %>%
          paste0("*")
      })
      output$type_positive_fac <- renderText({tolower(select_positive_fac())})
      
      single_bar_plot(fac_positive_df, 
                      select_positive_fac(), 
                      paste(select_positive_fac(), "Tested Positive"),
                      "Facility")
      
    } else if (select_positive_fac() == "Total") {
      
      output$n_positive_DOC_str <- renderText({
        paste0(as.character(sum(sjc_DOC_num_df$all_positive, na.rm=T)), "*")
      })
      output$type_positive_fac <- renderText({"prisoners and staff"})
      
      single_bar_plot(fac_positive_df, 
                      select_positive_fac(),
                      "Prisoners & Staff Tested Positive",
                      "Facility")
    }
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # DOC Facilities v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which facilities to plot
  fac_to_plot <- reactive({
    c(input$select_fac1,
      input$select_fac2,
      input$select_fac3)
  })
  
  DOC_total_df <- sjc_num_df %>%
    filter(County == "DOC") %>%
    rename(fac = County) %>%
    mutate(fac = "DOC Total**") %>%
    dplyr::select(Date, fac, all_positive)
  
  DOC_fac_total_df <- sjc_DOC_num_df %>%
    group_by(Date) %>%
    summarize(all_positive = sum(all_positive)) %>%
    mutate(fac = "All DOC Facilities")
  
  df_by_fac <- sjc_DOC_num_df %>%
    dplyr::select(Date, fac, all_positive) %>%
    rbind(DOC_total_df) %>%
    rbind(DOC_fac_total_df)
  
  # Plot
  output$DOC_time_plot <- renderPlotly({
    
    g <- df_by_fac %>%
      filter(fac %in% fac_to_plot()) %>%
      group_by(fac) %>%
      mutate(cumul = cumsum(all_positive)) %>%
      filter(fac == "DOC Total**" | (fac != "DOC Total**" & Date >= ymd(20200415))) %>%
    ggplot(aes(x=Date, y = cumul, 
                 color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.7) +
      geom_point(size = 3) +
      labs(x = "", y = "Total Prisoners & Staff\nTested Positive", color="",
           title = paste("Positive COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="GT America", face='bold'),
            text = element_text(family="GT America", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), legend.direction="horizontal",
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Prisoners & Staff Tested Positive", "Facility")
    
  })
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Explore Data w/ Table
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$df_table <- renderDataTable(
    {sjc_df},
    options = list(scrollX = TRUE), 
    filter = 'top'
    )
  
  output$DOC_df_table <- renderDataTable(
    {sjc_DOC_df},
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
        GET(sjc_dropbox_url, write_disk(file))
      })
    }
  )
  
}

shinyApp(ui = ui, server = server)
