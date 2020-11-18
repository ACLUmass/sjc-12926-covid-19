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
library(plotly)

source("plotly_builders.R")
source("filter_by_pop.R")

# Initialization --------------------------------------------------------------

# Set ggplot settings
theme_set(theme_minimal())

# Load ggplot-friendly font using show_text
font_add("gtam", "GT-America-Standard-Regular.ttf",
         bold = "GT-America-Standard-Bold.ttf")
showtext_auto()

# Download Massachusetts county data
mass_cntys <- tigris::counties(state=25, cb=T)

# Define list of counties
counties <- c("DOC", "Barnstable", "Berkshire", "Bristol", "Dukes", "Essex", 
              "Franklin", "Hampden", "Hampshire", "Middlesex", "Norfolk", 
              "Plymouth", "Suffolk", "Worcester")

# Make list for drop-downs
county_choices <- c("--", "All", "All Counties", counties)
fac_choices <- c("--", "DOC Total**", "All DOC Facilities", 'Boston Pre', 'BSH', 
                 'LSH', 'MASAC', 'MCI-C', 'MCI-CJ', 'MCI-F', 'MCI-Norfolk', 
                 'MCI-Shirley', 'MCI-Shirley Min', 'MTC',  "NECC", 'NCCI-Gardn', 'OCCC', 'Pondville', 
                 'SBCC', 'SMCC')
fac_staff <- c('Boston Pre', 'BSH', 
               'LSH', 'MASAC', 'MCI-C', 'MCI-CJ', 'MCI-F', 'MCI-Norfolk', 
               'MCI-Shirley', 'MCI-Shirley Min', 'MTC', "NECC", 'NCCI-Gardn', 'OCCC', 'Pondville', 
               'SBCC', 'SMCC', "Non-Facility")

pop_choices <- c("--", 'All', 'All Counties', "DOC", 'DOC: Boston Pre', 'DOC: BSH', 
                 'DOC: LSH', 'DOC: MASAC', 'DOC: MCI-C', 'DOC: MCI-CJ', 'DOC: MCI-F', 
                 'DOC: MCI-Norfolk', 'DOC: MCI-Shirley', 'DOC: MCI-Shirley Min', 'DOC: MTC', 
                 'DOC: NCCI-Gardn', 'DOC: NECC', 'DOC: OCCC', 'DOC: Pondville', 
                 'DOC: SBCC', 'DOC: SMCC', 'Barnstable', 'Berkshire', 'Bristol',
                 'Dukes', 'Essex', 'Franklin', 'Hampden', 'Hampshire', 'Middlesex', 
                 'Norfolk', 'Plymouth', 'Suffolk', 'Worcester')

cty_facs <- c('Bristol - Ash Street Jail', 'Bristol - DHOC', 'Essex - Middleton', 
              'Essex - Prerelease', 'Essex - Women in Transition', 'Hampden - Ludlow', 
               'Hampden - Mill Street', "Hampden - Women's Facility", "Hampden - Section 35", 
              'Suffolk - HOC', 'Suffolk - Jail')
ctyfac_choices <- c("--", "All Counties", cty_facs)

# UI --------------------------------------------------------------------------

ui <- fluidPage(theme = "sjc_12926_app.css",
      
  # Add favicon          
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
  ),
  
  # App title
  div(id="title",
    titlePanel("Tracking COVID-19 in Massachusetts Prison & Jails")
  ),
  
  div(
    navlistPanel(widths = c(3, 9), id="panels",
                 
      tabPanel("About", 
               div(id="dev-info",
                   wellPanel(
                     icon('info-circle'),
                     h4("New Weekly Reporting"),
                     em("On June 23, the SJC issued an", 
                        a("order", href="https://data.aclum.org/wp-content/uploads/2020/07/SJC-12926-Order-Appendix-B.pdf"), 
                        "modifying the DOC and HOC 
                        reporting responsibilities. Starting the week of July 6,
                        facilities will submit COVID-19 reports once per week rather 
                        than once per day. This order also required 
                        county and state facilities to report additional metrics, 
                        including:",
                        tags$ol(tags$li("The number of active COVID-19 case reported by the counties/DOC"),
                                tags$li("The number of COVID-19 deaths reported by the counties/DOC"),
                                tags$li("[DOC] The number of individuals released to home confinement reported by the DOC")),
                        "As such, starting July 8, this site will be updated on a 
                        weekly rather than daily basis, with new data published Thursday mornings.")
                   )
               ),
               
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
               h3("Dates & Locations of COVID Deaths"),
               p("Though the June 23 amendment requires reporting of deaths going forward,
                 it did not require retroactive reporting of the time and place of COVID-19
                 related deaths of incarcerated folks in Massachusetts. The below list reflects
                 the ACLU-MA's best recreation of when and where these deaths occurred, as 
                 reported in the media."),
               tags$ol(
                 tags$li(a("April 2, 2020 - DOC - Massachusetts Treatment Center (MTC)", 
                           href="https://www.wbur.org/commonhealth/2020/04/01/mass-prisons-jails-coronavirus")),
                 tags$li(a("April 3, 2020 - DOC - Massachusetts Treatment Center (MTC)",
                           href="https://www.masslive.com/coronavirus/2020/04/coronavirus-2nd-inmate-at-massachusetts-treatment-center-dies-after-testing-positive-for-covid-19.html")),
                 tags$li(a("April 4, 2020 - DOC - Massachusetts Treatment Center (MTC)",
                           href="https://www.bostonglobe.com/2020/04/05/metro/third-inmate-bridgewater-treatment-center-dies-covid-19/")),
                 tags$li(a("April ~13, 2020 - DOC - Massachusetts Treatment Center (MTC)",
                           href="https://www.wbur.org/news/2020/04/15/jails-prisons-latest-coronavirus-cases-mtc")),
                 tags$li(a("April 16, 2020 - DOC - MCI – Shirley",
                           href="https://www.wbur.org/news/2020/04/16/coronavirus-deaths-jails-prisons-update")),
                 tags$li("April ~22, 2020 - DOC  - Massachusetts Treatment Center (MTC)"),
                 tags$li(a("April 22, 2020 - DOC - MCI – Shirley", 
                           href="https://www.masslive.com/coronavirus/2020/04/7th-prisoner-in-massachusetts-dies-from-coronavirus-doc-says-127-inmates-53-correction-staff-in-total-infected.html")),
                 tags$li(a("April 30, 2020 - Essex County - Middleton House of Correction (HOC)",
                           href="https://www.wbur.org/commonhealth/2020/04/30/essex-jail-dies-coronavirus")),
                 tags$li(a("May 15, 2020 - DOC - MCI – Shirley", 
                           href="https://www.wbur.org/news/2020/05/15/massachusetts-prisoner-dies-covid-19-coronavirus")),
                 tags$li(a("July 2, 2020 - Norfolk County Jail", 
                           href="https://www.patriotledger.com/news/20200702/norfolk-county-jail-inmate-dies-in-hospital-from-covid-19-month-after-hospitalization"))
               ),
               em("Please note that potentially COVID-19-related",
                  a(href="https://www.wwlp.com/news/local-news/hampshire-county/20-inmates-4-staff-have-tested-positive-for-covid-19-at-hampshire-county-jail/", 
                    "deaths of previous inmates"), 
                  "which occurred outside of a correctional facility are not reported here."),
               
               h3("Inconsistencies in Reporting"),
               p("This site exists as a resource to document data which Massachusets",
                 "prisons, jails, and houses of corrections report to the SJC",
                 "amidst the COVID-19 crisis."),
               p("However, it became evident in the first few days of reports that",
                 "there were inconsistencies in data collection from the various",
                 "institutions."),
               h4("Recommended Resources"),
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
                        <li><a href='https://commonwealthmagazine.org/health-care/virus-notes-22-mayors-back-vote-by-mail/' target='_blank'>Commonwealth Magazine: DOC 4/22/2020</a></li>
                        <li><a href='https://www.masslive.com/coronavirus/2020/04/doc-yet-to-provide-missing-data-about-prisoners-and-staff-with-coronavirus-dating-back-to-april-3-public-defender-agency-of-mass-claims.html' target='_blank'>MassLive: DOC 4/23/2020</a></li>
                        <li><a href='https://commonwealthmagazine.org/criminal-justice/inmates-testify-about-covid-19-dangers/' target='_blank'>Commonwealth Magazine: DOC 4/27/2020</a></li>
                        <li><a href='https://framinghamsource.com/index.php/2020/05/01/36-percent-of-mci-framingham-inmates-positive-for-coronavirus/' target='_blank'>Framingham Source: MCI-Framingham 5/1/2020</a></li>
                      </ul>
                  </li>
                </ul>")
               ),

      "Data by County/Facility",
      
      tabPanel("Incarcerated Population Over Time",
               wellPanel(id="internal_well",
                         p("Select up to three locations to plot versus time."),
                         splitLayout(
                           selectInput("select_county1_pop", label = NULL, choices = pop_choices,
                                       selected = "All", multiple=FALSE),
                           selectInput("select_county2_pop", label = NULL, choices = pop_choices,
                                       selected = "--", multiple=FALSE),
                           selectInput("select_county3_pop", label = NULL, choices = pop_choices,
                                       selected = "--", multiple=FALSE)
                         )),
               checkboxInput("checkbox_pop", label = "Show transition to weekly reporting", value = TRUE),
               withSpinner(plotlyOutput("pop_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
      ),
      
      # tabPanel("Compare Active Cases & Recent Tests", 
      #          wellPanel(id="internal_well",
      #                    p("Select location to plot versus time.*"),
      #                    splitLayout(
      #                      selectInput("select_both_active", label = NULL, 
      #                                  choices = tail(pop_choices, -1),
      #                                  selected = "All", multiple=FALSE)
      #                    ),
      #                    em("*Facilities only began reporting active cases on July 8.")
      #          ),
      #          withSpinner(plotlyOutput("both_plot_active"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Compare Tests & Positives", 
               wellPanel(id="internal_well",
                         p("Select population to plot.", id="radio_prompt"),
                         radioButtons("both_radio", label = NULL, 
                                      selected = "ps" , inline = T, 
                                      choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                      choiceValues = c("p", "s", "ps")),
                         p("Select location to plot versus time.*"),
                         splitLayout(
                           selectInput("select_both", label = NULL, choices = tail(pop_choices, -1),
                                       selected = "All", multiple=FALSE)
                         ),
                         em("*The DOC only began reporting facility-level prisoner positives on April 13,",
                            "facility-level staff positives on April 15, and facility-level testing data on April 25.")
               ),
               checkboxInput("checkbox_both", label = "Show transition to weekly reporting", value = TRUE,
                             width="100%"),
               withSpinner(plotlyOutput("both_plot"), type=4, color="#b5b5b5", size=0.5),
               em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
      
      # UI: Deaths ---------------------------------------------
      navbarMenu("Deaths",
                 
                 tabPanel("County + DOC Deaths", 
                          h2(textOutput("n_deaths_str"), align="center"),
                          p("Reports of COVID-19 prisoner deaths pursuant to SJC 12926", align="center"),
                          withSpinner(plotlyOutput("all_deaths_plot"), type=4, color="#b5b5b5", size=0.5),
                          h3("Dates & Locations of COVID-19 Deaths"),
                          tags$ul(
                            tags$li("Before June 23: 10 deaths:"),
                            tags$ul(
                              tags$li("DOC: MTC -  5 deaths"),
                              tags$li("DOC: MCI-Shirley -  3 deaths"),
                              tags$li("Essex - 1 deaths"),
                              tags$li("Norfolk - 1 deaths")
                            )
                          ),
                          em("For information regarding the dates and locations of deaths before
                    June 23, please visit our",
                             actionLink("link_to_external2", "external resources page."))
                          
                 ),
                 
                 tabPanel("DOC Facility Deaths", 
                          h2(textOutput("n_deaths_fac_str"), align="center"),
                          p("Reports of COVID-19 prisoner deaths in DOC facilities pursuant to SJC 12926", align="center"),
                          withSpinner(plotlyOutput("all_deaths_fac_plot"), type=4, color="#b5b5b5", size=0.5),
                          h3("Dates & Locations of DOC COVID-19 Deaths"),
                          tags$ul(
                            tags$li("Before June 23: 8 deaths:"),
                            tags$ul(
                              tags$li("DOC: MTC -  5 deaths"),
                              tags$li("DOC: MCI-Shirley -  3 deaths")
                            )
                          ),
                          em("For information regarding the dates and locations of deaths before
                    June 23, please visit our",
                             actionLink("link_to_external3", "external resources page."))
                 )
                 
      ),
      
      
    # UI: DOC + County Aggregates ---------------------------------------------
    navbarMenu("Counties + DOC Aggregates",
      
        tabPanel("Total Releases", 
                 wellPanel(id="internal_well",
                   p("Select kind of prisoner:"),
                   selectInput("select_release", label = NULL, 
                             choices = c("All", "Pre-Trial", "Sentenced","Home Confinements", "Total"),
                             selected = "All", multiple=FALSE),
                   em('Exact number of releases per county annotated in',
                      '"Pre-Trial", "Sentenced", "Home Confinements", and "Total" plots.')
                   ),
                 h2(textOutput("n_releases_str"), align="center"),
                 p("Prisoners released pursuant to SJC 12926", align="center"),
                 withSpinner(plotlyOutput("all_releases_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
        
        tabPanel("Releases Over Time",
                 wellPanel(id="internal_well",
                   p("Select up to three locations to plot versus time."),
                   splitLayout(
                     selectInput("select_county1_rel", label = NULL, choices = county_choices,
                                 selected = "All", multiple=FALSE),
                     selectInput("select_county2_rel", label = NULL, choices = county_choices,
                                 selected = "All Counties", multiple=FALSE),
                     selectInput("select_county3_rel", label = NULL, choices = county_choices,
                                 selected = "DOC", multiple=FALSE)
                   )),
                 checkboxInput("checkbox_rel", label = "Show transition to weekly reporting", value = TRUE),
                 withSpinner(plotlyOutput("releases_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
        ),
        
        "----",
        
        tabPanel("Total Tests", 
                 wellPanel(id="internal_well",
                   p("Select kind of individual:"),
                   selectInput("select_tested", label = NULL, 
                               choices = c("All", "Prisoners", "Staff", "Total"),
                               selected = "All", multiple=FALSE),
                   em('Exact number of tests per county annotated in "Prisoners",',
                      '"Staff", and "Total" plots.')
                 ),
                 checkboxInput("checkbox_hideDOC_tests", label = "Hide DOC column", value = F),
                 h2(textOutput("n_tests_str"), align="center"),
                 p("Reports of",
                   textOutput("type_tested", inline=T),
                   "tested for COVID-19  pursuant to SJC 12926", 
                   align="center"),
                 withSpinner(plotlyOutput("all_tests_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
        
        tabPanel("Tests Over Time",
                 wellPanel(id="internal_well",
                   p("Select population to plot.", id="radio_prompt"),
                   radioButtons("test_radio", label = NULL, 
                                selected = "ps" , inline = T, 
                                choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                choiceValues = c("p", "s", "ps")),
                   p("Select up to three locations to plot versus time."),
                   splitLayout(
                     selectInput("select_county1_test", label = NULL, choices = county_choices,
                                 selected = "All", multiple=FALSE),
                     selectInput("select_county2_test", label = NULL, choices = county_choices,
                                 selected = "All Counties", multiple=FALSE),
                     selectInput("select_county3_test", label = NULL, choices = county_choices,
                                 selected = "DOC", multiple=FALSE)
                   )),
                 checkboxInput("checkbox_test", label = "Show transition to weekly reporting", value = TRUE),
                 withSpinner(plotlyOutput("tests_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
        ),
        
        "----",
        
        tabPanel("Total Positive Tests", 
                 wellPanel(id="internal_well",
                           p("Select kind of individual:"),
                           selectInput("select_positive", label = NULL, 
                                       choices = c("All", "Prisoners", "Staff", "Total"),
                                       selected = "All", multiple=FALSE),
                           em('Exact number of positive tests per county annotated in',
                              '"Prisoners", "Staff", and "Total" plots.')
                 ),
                 checkboxInput("checkbox_hideDOC_pos", label = "Hide DOC column", value = F),
                 h2(textOutput("n_positive_str"), align="center"),
                 p("Reports of",
                   textOutput("type_positive", inline=T),
                   "tested", strong("positive"),
                   "for COVID-19 pursuant to SJC 12926", align="center"),
                 withSpinner(plotlyOutput("all_positives_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
        
        tabPanel("Positive Tests Over Time",
                 wellPanel(id="internal_well",
                   p("Select population to plot.", id="radio_prompt"),
                   radioButtons("positive_radio", label = NULL, 
                                selected = "ps" , inline = T, 
                                choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                choiceValues = c("p", "s", "ps")),
                   p("Select up to three locations to plot versus time."),
                   splitLayout(
                     selectInput("select_county1_pos", label = NULL, choices = county_choices,
                                 selected = "All", multiple=FALSE),
                     selectInput("select_county2_pos", label = NULL, choices = county_choices,
                                 selected = "All Counties", multiple=FALSE),
                     selectInput("select_county3_pos", label = NULL, choices = county_choices,
                                 selected = "DOC", multiple=FALSE)
                   )),
                 checkboxInput("checkbox_pos", label = "Show transition to weekly reporting", value = TRUE),
                 withSpinner(plotlyOutput("positives_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
        ),
        
        "----",
        
        tabPanel("Active Positive Cases", 
                 h2(textOutput("n_active_str"), align="center"),
                 p("Reports of prisoners currently infected with COVID-19, pursuant to SJC 12926",
                   br(),
                   em("Showing active cases as reported on", 
                      textOutput("last_date_str", inline=T),
                      align="center"),
                   align="center"),
                 withSpinner(plotlyOutput("all_active_plot"), type=4, color="#b5b5b5", size=0.5)
                 ),
        
        tabPanel("Active Positive Cases Over Time",
                 wellPanel(id="internal_well",
                           p("Select up to three locations to plot versus time."),
                           splitLayout(
                             selectInput("select_active1", label = NULL, choices = county_choices,
                                         selected = "All", multiple=FALSE),
                             selectInput("select_active2", label = NULL, choices = county_choices,
                                         selected = "All Counties", multiple=FALSE),
                             selectInput("select_active3", label = NULL, choices = county_choices,
                                         selected = "DOC", multiple=FALSE)
                           )),
                 withSpinner(plotlyOutput("active_v_time_plot"), type=4, color="#b5b5b5", size=0.5)
        )
      ),
      
    # UI: DOC Facilities ------------------------------------------------------
     navbarMenu("DOC Facilities",
     
       tabPanel("Total Releases", 
                div(align="center",
                    h2(textOutput("n_releases_DOC_str")),
                    p("Reports of prisoners released at individual DOC facilities pursuant to SJC 12926", align="center"),
                    em("*The DOC only began reporting facility-level releases on April 29.",
                       "See the Counties + DOC: Total Releases page for longer-term totals.")
                ),
                withSpinner(plotlyOutput("all_releases_DOC_plot"), type=4, color="#b5b5b5", size=0.5),
                em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
       
       tabPanel("Releases Over Time",
                wellPanel(id="internal_well",
                          p("Select up to three facilities to plot versus time.*"),
                          splitLayout(
                            selectInput("select_fac1_rel", label = NULL, choices = fac_choices,
                                        selected = "All DOC Facilities", multiple=FALSE),
                            selectInput("select_fac2_rel", label = NULL, choices = fac_choices,
                                        selected = "DOC Total**", multiple=FALSE),
                            selectInput("select_fac3_rel", label = NULL, choices = fac_choices,
                                        selected = "MCI-Norfolk", multiple=FALSE)
                          ),
                          em("*The DOC only began reporting facility-level releases on April 29.",
                             "See the Counties + DOC: Releases Over Time page for longer-term totals."),
                          em('**DOC Total reflects the cumulative count of prisoner releases submitted in DOC-wide',
                             "reports going back to March 27.",
                             style="display: block; margin-top: 1rem;")
                      ),
                checkboxInput("checkbox_fac_rel", label = "Show transition to weekly reporting", value = TRUE),
                withSpinner(plotlyOutput("DOC_releases_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
                em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
       ),
       
       "----",
      
        tabPanel("Total Tests", 
                 wellPanel(id="internal_well",
                           p("Select kind of individual:"),
                           selectInput("select_tests_fac", label = NULL, 
                                       choices = c("All", "Prisoners", "Staff", "Total"),
                                       selected = "All", multiple=FALSE),
                           em('Exact number of tests per facility annotated in',
                              '"Prisoners", "Staff", and "Total" plots.')
                 ),
                 div(align="center",
                     h2(textOutput("n_tests_DOC_str")),
                     p("Reports of",
                       textOutput("type_tests_fac", inline=T),
                       "tested for COVID-19 at individual DOC facilities pursuant to SJC 12926", align="center"),
                     em("*The DOC only began reporting facility-level testing data on April 25.",
                        "See the Total Tests page for longer-term totals.")
                 ),
                 withSpinner(plotlyOutput("DOC_tests_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
       
       tabPanel("Tests Over Time",
                wellPanel(id="internal_well",
                          p("Select population to plot.", id="radio_prompt"),
                          radioButtons("doc_test_radio", label = NULL, 
                                       selected = "ps" , inline = T, 
                                       choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                       choiceValues = c("p", "s", "ps")),
                          p("Select up to three facilities to plot versus time.*"),
                          splitLayout(
                            selectInput("select_fac1_test", label = NULL, choices = fac_choices,
                                        selected = "All DOC Facilities", multiple=FALSE),
                            selectInput("select_fac2_test", label = NULL, choices = fac_choices,
                                        selected = "DOC Total**", multiple=FALSE),
                            selectInput("select_fac3_test", label = NULL, choices = fac_choices,
                                        selected = "MCI-Shirley", multiple=FALSE)
                          ),
                          em("*The DOC only began reporting facility-level testing data on April 25.",
                             "See the Counties + DOC Tests Over Time page for longer-term DOC tracking"),
                          em('**DOC Total reflects the cumulative count of prisoner tests submitted in DOC-wide',
                             "reports going back to March 27.",
                             style="display: block; margin-top: 1rem;")
                          ),
                checkboxInput("checkbox_fac_test", label = "Show transition to weekly reporting", value = TRUE),
                withSpinner(plotlyOutput("doc_tests_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
                em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
       ),
       
       "----",
        
        tabPanel("Total Positive Tests", 
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
                   em("*The DOC only began reporting facility-level positive data on April 13.",
                      "See the Total Positive Tests page for longer-term totals.")
                   ),
                 withSpinner(plotlyOutput("DOC_positives_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")),
        
        tabPanel("Positive Tests Over Time", 
                 wellPanel(id="internal_well",
                   p("Select population to plot.", id="radio_prompt"),
                   radioButtons("doc_positive_radio", label = NULL, 
                                selected = "ps" , inline = T, 
                                choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                choiceValues = c("p", "s", "ps")),
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
                      "See the Counties + DOC Positive Tests Over Time page for longer-term DOC tracking"),
                   em('**DOC Total reflects DOC-wide reports, and might undercount prisoner',
                      "cases as compared to the facility total due to the DOC reporting",
                      "active, rather than total, cases.",
                      'Additionally, DOC Total staff includes staff categorized as "Other"',
                      'while the facility total does not.',
                      style="display: block; margin-top: 1rem;")
                   ),
                 checkboxInput("checkbox_fac_pos", label = "Show transition to weekly reporting", value = TRUE),
                 withSpinner(plotlyOutput("DOC_time_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
             ),
       
       "----",
       
       tabPanel("Active Positive Cases", 
                h2(textOutput("n_active_fac_str"), align="center"),
                p("Reports of DOC prisoners currently infected with COVID-19, pursuant to SJC 12926",
                  br(),
                  em("Showing active DOC cases as reported on", 
                     textOutput("last_date_DOC_str", inline=T)),
                  align="center"),
                withSpinner(plotlyOutput("all_active_fac_plot"), type=4, color="#b5b5b5", size=0.5)
       ),
       
       tabPanel("Active Positive Cases Over Time",
                wellPanel(id="internal_well",
                          p("Select up to three locations to plot versus time."),
                          splitLayout(
                            selectInput("select_active_fac1", label = NULL, 
                                        choices = fac_choices[fac_choices != "DOC Total**"],
                                        selected = "All DOC Facilities", multiple=FALSE),
                            selectInput("select_active_fac2", label = NULL, 
                                        choices = fac_choices[fac_choices != "DOC Total**"],
                                        selected = "MCI-Norfolk", multiple=FALSE),
                            selectInput("select_active_fac3", label = NULL, 
                                        choices = fac_choices[fac_choices != "DOC Total**"],
                                        selected = "MCI-F", multiple=FALSE)
                          )),
                withSpinner(plotlyOutput("active_v_time_fac_plot"), type=4, color="#b5b5b5", size=0.5)
       )
     ),
    
    # UI: County Facilities --------------------------------------------------
    navbarMenu("Multi-Facility Counties",
               "Bristol, Essex, Hampden, & Suffolk",
               "----",
               
               tabPanel("Total Tests", 
                        wellPanel(id="internal_well",
                                  p("Select kind of individual:"),
                                  selectInput("select_tests_cty", label = NULL,
                                              choices = c("All", "Prisoners", "Staff", "Total"),
                                              selected = "All", multiple=FALSE),
                                  em('Exact number of tests per facility annotated in',
                                     '"Prisoners", "Staff", and "Total" plots.')
                        ),
                        div(align="center",
                            h2(textOutput("n_tests_cty_str")),
                            p("Reports of",
                              textOutput("type_tests_cty", inline=T),
                              "tested for COVID-19 at individual county facilities pursuant to SJC 12926", align="center"),
                            em("*Only some counties began reporting facility-level testing data on May 8.",
                               "See the Counties + DOC Aggregates: Total Tests page for longer-term totals.")
                        ),
                        withSpinner(plotlyOutput("cty_tests_plot"), type=4, color="#b5b5b5", size=0.5),
                        em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
                  ),
               
               tabPanel("Tests Over Time",
                        wellPanel(id="internal_well",
                                  p("Select population to plot.", id="radio_prompt"),
                                  radioButtons("cty_tests_radio", label = NULL,
                                               selected = "ps" , inline = T,
                                               choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                               choiceValues = c("p", "s", "ps")),
                                  p("Select up to three facilities to plot versus time.*"),
                                  splitLayout(
                                    selectInput("select_ctyfac_test1", label = NULL, choices = ctyfac_choices,
                                                multiple=FALSE),
                                    selectInput("select_ctyfac_test2", label = NULL, choices = ctyfac_choices,
                                                multiple=FALSE),
                                    selectInput("select_ctyfac_test3", label = NULL, choices = ctyfac_choices,
                                                multiple=FALSE)
                                  ),
                                  em("*Only some counties began reporting facility-level testing data on May 8.",
                                     "See the Counties + DOC Aggregates: Total Tests page for longer-term totals.",
                                     style="display: block; margin-top: 1rem;")
                        ),
                        checkboxInput("checkbox_ctyfac_test", label = "Show transition to weekly reporting", value = TRUE),
                        withSpinner(plotlyOutput("cty_tests_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
                        em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
               ),
               
               "----",
               
               tabPanel("Total Positive Tests", 
                        wellPanel(id="internal_well",
                                  p("Select kind of individual:"),
                                  selectInput("select_positive_cty", label = NULL,
                                              choices = c("All", "Prisoners", "Staff", "Total"),
                                              selected = "All", multiple=FALSE),
                                  em('Exact number of positive tests per facility annotated in',
                                     '"Prisoners", "Staff", and "Total" plots.')
                        ),
                        div(align="center",
                            h2(textOutput("n_positive_cty_str")),
                            p("Reports of",
                              textOutput("type_positive_cty", inline=T),
                              "tested", strong("positive"),
                              "for COVID-19 at individual county facilities pursuant to SJC 12926", align="center"),
                            em("*Only some counties began reporting facility-level positive data on May 8.",
                               "See the Counties + DOC Aggregates: Total Positive Tests page for longer-term totals.")
                        ),
                        withSpinner(plotlyOutput("cty_positives_plot"), type=4, color="#b5b5b5", size=0.5),
                        em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
                  ),
               
               tabPanel("Positive Tests Over Time", 
                        wellPanel(id="internal_well",
                                  p("Select population to plot.", id="radio_prompt"),
                                  radioButtons("cty_positive_radio", label = NULL,
                                               selected = "ps" , inline = T,
                                               choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                               choiceValues = c("p", "s", "ps")),
                                  p("Select up to three facilities to plot versus time.*"),
                                  splitLayout(
                                    selectInput("select_ctyfac_pos1", label = NULL, choices = ctyfac_choices,
                                                multiple=FALSE),
                                    selectInput("select_ctyfac_pos2", label = NULL, choices = ctyfac_choices,
                                                multiple=FALSE),
                                    selectInput("select_ctyfac_pos3", label = NULL, choices = ctyfac_choices,
                                                multiple=FALSE)
                                  ),
                                  em("*Only some counties began reporting facility-level positive data on May 8.",
                                     "See the Counties + DOC Aggregates: Total Positive Tests page for longer-term totals.",
                                     style="display: block; margin-top: 1rem;")
                        ),
                        checkboxInput("checkbox_ctyfac_pos", label = "Show transition to weekly reporting", value = TRUE),
                        withSpinner(plotlyOutput("ctyfac_pos_time_plot"), type=4, color="#b5b5b5", size=0.5),
                        em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
                ),
               
               "----",

               tabPanel("Active Positive Cases",
                        p(em("Showing active cases as reported on"),
                           textOutput("last_date_str2", inline=T),
                          br(),
                          em("Please note that Hampden County is not reporting active cases by facility."),
                           align="center"
                          ),
                        withSpinner(plotlyOutput("all_active_cty_plot"), type=4, color="#b5b5b5", size=0.5)
               ),

               tabPanel("Active Positive Cases Over Time",
                        wellPanel(id="internal_well",
                                  p("Select up to three locations to plot versus time."),
                                  splitLayout(
                                    selectInput("select_active_cty1", label = NULL, choices = ctyfac_choices,
                                                selected = "All Counties", multiple=FALSE),
                                    selectInput("select_active_cty2", label = NULL, choices = ctyfac_choices,
                                                selected = "Bristol - DHOC", multiple=FALSE),
                                    selectInput("select_active_cty3", label = NULL, choices = ctyfac_choices,
                                                selected = "Essex - Middleton", multiple=FALSE)
                                  ),
                                  em("Please note that Hampden County is not reporting active cases by facility.")
                                  ),
                        withSpinner(plotlyOutput("active_cty_fac_v_time_plot"), type=4, color="#b5b5b5", size=0.5)
               ),
               
               "----",
               
               tabPanel("Mapping County Trends", 
                        wellPanel(id="internal_well",
                                  p("Select value to plot."),
                                  selectInput("select_y_plot", label = NULL,
                                              choices = c("Releases", "Tests", "Positive Cases"),
                                              selected = "Releases", multiple=FALSE)
                        ),
                        em("All maps reflect data from county jails and HOCs alone,",
                           "as the Massachusutts DOC has only been reporting",
                           "facility-level data since April 25th."),
                        em("Maps of tests and positive cases include both prisoner and staff data."),
                        em("Please note that prisoner deaths due to COVID-19 are not included in these data."),
                        withSpinner(leafletOutput("county_maps"),
                                    type=4, color="#b5b5b5", size=0.5)
               )
    ),
    
    
    "Data Source",
      
      tabPanel("Explore Data",
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
                                    withSpinner(dataTableOutput("DOC_df_table"), type=4, color="#b5b5b5", size=0.5)),
                           tabPanel("County Facilities", 
                                    withSpinner(dataTableOutput("cty_df_table"), type=4, color="#b5b5b5", size=0.5))
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
      em("\n\nMost recent report:", textOutput("last_date_str3", inline=T), 
         align="right", style="opacity: 0.6;"),
      br(),
      em("Database last accessed:", textOutput("latest_time_str", inline=T), 
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

# Server ----------------------------------------------------------------------

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

server <- function(input, output, session) {
  
  # Session Counter -----------------------------------------------------------
  
  # Increment the number of sessions when one is opened.
  # We use isolate() here to:
  #  a.) Provide a reactive context
  #  b.) Ensure that this expression doesn't take a reactive dependency on
  #      vals$count -- if it did, every time vals$count changed, this expression
  #      would run, leading to an infinite loop.
  isolate({
    vals$count <- vals$count + 1
    # Update log to reflect current number of sessions
    line = paste(query_time = now('America/New_York'), "\t", 
                 vals$count, "active sessions", "\t", 
                 Sys.getpid())
    write(line, file = "n_sessions.txt", append=TRUE)
    })
  
  # When a session ends, decrement the counter.
  session$onSessionEnded(function(){
    # We use isolate() here for the same reasons as above.
    isolate({
      vals$count <- vals$count - 1
      # Update log to reflect current number of sessions
      line = paste(query_time = now('America/New_York'), "\t", 
                   vals$count, "active sessions", "\t", 
                   Sys.getpid())
      write(line, file = "n_sessions.txt", append=TRUE)
    })
  })
  
  # Define last date entered for page footer
  output$last_date_str3 <- renderText({
    strftime(last_date_entered, format="%A, %B %d, %Y"
    )})

  # Load Data -----------------------------------------------------------
  
  # Define file locations
  sjc_googledrive_url <- "https://docs.google.com/spreadsheets/d/1nmZ84rjOxQgdTL0PdV7SrbyDTbD7nROQ/export#gid=1419540291"

  # Download excel spreadsheet from URL and read as DF
  GET(sjc_googledrive_url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  
  sjc_df <- read_excel(tf) %>%
    # Turn string "NA" to real NA
    mutate_if(is.character, ~na_if(., 'NA')) %>%
    # Make all count columns numeric
    mutate_at(vars(starts_with("N "), starts_with("Total"), 
                   matches("Population"), matches("Active"), matches("Death")), 
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
  output$n_releases_str <- renderText({format(n_released, big.mark=",")})
  output$n_positive_str <- renderText({format(n_positive, big.mark=",")})
  output$n_tests_str <- renderText({format(n_tested, big.mark=",")})
  
  # Calculate sums by county
  sum_sjc_num_df <- sjc_num_df %>%
    filter(County != "DOC") %>%
    group_by(County) %>%
    summarize(all_positive = sum(all_positive),
              all_tested = sum(all_tested),
              all_released = sum(all_released))
  
  # Load DOC Data -----------------------------------------------------------
  
  sjc_DOC_df <- read_excel(tf, sheet=2) %>%
    mutate_if(is.character, ~na_if(., 'NA')) %>%
    mutate(Date = as.Date(Date)) %>%
    mutate_at(vars(starts_with("N "), starts_with("Total"), 
                   matches("Active"), matches("Death")),
              as.numeric)
  
  sjc_DOC_num_df <- sjc_DOC_df %>%
    rename(fac = `DOC Facility`,
           all_positive = `Total Positive`,
           all_tested = `Total Tested`,
           all_released = `N Released`) %>%
    mutate(fac = factor(fac, levels=fac_staff)) %>%
    filter(!is.na(all_positive)) %>%
    dplyr::select(-Notes)
  
  # Load County Data -----------------------------------------------------------
  
  sjc_county_df <- read_excel(tf, sheet=3) %>%
    mutate_if(is.character, ~na_if(., 'NA')) %>%
    mutate(Date = as.Date(Date)) %>%
    mutate_at(vars(starts_with("N "), starts_with("Total"), 
                   matches("Active"), matches("Death")),
              as.numeric)
  
  sjc_county_num_df <- sjc_county_df %>%
    rename(fac = `County Facility`,
           all_positive = `Total Positive`,
           all_tested = `Total Tested`) %>%
    mutate(fac = factor(fac, levels=cty_facs)) %>%
    filter(!is.na(all_positive)) %>%
    dplyr::select(-Notes)
  
  counties_with_breakdowns <- sjc_county_num_df %>%
    separate(fac, c("County", NA), sep=" - ") %>%
    pull(County) %>%
    unique()
  
  # Determine county facility selector
  ctyfac_choices <- c("--", "All Counties", 
                      sort(c(counties_with_breakdowns, cty_facs)))
  
  # Update selectors
  observe({
    updateSelectInput(session, "select_ctyfac_test1",
                      choices = ctyfac_choices,
                      selected = "All Counties")
    updateSelectInput(session, "select_ctyfac_test2",
                      choices = ctyfac_choices,
                      selected = "Bristol")
    updateSelectInput(session, "select_ctyfac_test3",
                      choices = ctyfac_choices,
                      selected = "Bristol - DHOC")
    updateSelectInput(session, "select_ctyfac_pos1",
                      choices = ctyfac_choices,
                      selected = "All Counties")
    updateSelectInput(session, "select_ctyfac_pos2",
                      choices = ctyfac_choices,
                      selected = "Bristol")
    updateSelectInput(session, "select_ctyfac_pos3",
                      choices = ctyfac_choices,
                      selected = "Bristol - DHOC")
    
  })
  
  # Population v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  cnty_to_plot_pop <- reactive({
    c(input$select_county1_pop,
      input$select_county2_pop,
      input$select_county3_pop)
  })
  
  sjc_df_fix_date_pop <- sjc_num_df %>%
    filter(`Total Population` != 0) %>%
    mutate(Date = if_else(County == "DOC" & Date >= ymd(20200707),
                         Date + days(1), Date))
  
  all_pop_df <- sjc_df_fix_date_pop %>%
    mutate(pop = `Total Population`) %>%
    filter(pop != 0) %>%
    group_by(Date) %>%
    filter(n() == 14) %>%
    summarize(pop = sum(pop)) %>%
    mutate(County = "All")
  
  all_counties_pop_df <- sjc_df_fix_date_pop %>%
    mutate(pop = `Total Population`) %>%
    filter(pop != 0,
           County != "DOC") %>%
    group_by(Date) %>%
    filter(n() == 13) %>%
    summarize(pop = sum(pop)) %>%
    mutate(County = "All Counties")
  
  doc_fac_pop_df <- sjc_DOC_num_df %>%
    mutate(pop = `Total Population`,
           County = paste("DOC:", fac),
           Date = if_else(Date >= ymd(20200707),
                         Date + days(1), Date)) %>%
    filter(!is.na(pop)) %>%
    group_by(Date, County) %>%
    summarize(pop = sum(pop)) 
  
  pop_df <-  sjc_df_fix_date_pop %>%
    mutate(pop = `Total Population`,
           County = as.character(County)) %>%
    group_by(Date, County) %>%
    summarize(pop = sum(pop)) %>%
    bind_rows(all_pop_df) %>%
    bind_rows(all_counties_pop_df) %>%
    bind_rows(doc_fac_pop_df)
  
  # Plot
  output$pop_v_time_plot <- renderPlotly({
    
    g <-pop_df %>%
      mutate(pop = na_if(pop, 0)) %>%
      filter(Date >= ymd(20200407),
             County %in% cnty_to_plot_pop(),
             !is.na(pop)) %>%
      rename(Location = County) %>%
      ggplot(aes(x=Date, y = pop, color=Location)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      labs(x = "", y = "Total Prisoners", color="",
           title = paste("Incarcerated Populations over Time")) +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22),
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ", limits=c(ymd(20200407), NA)) +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off') 
    
    lines_plotly_style(g, "Incarcerated Population", "County",
                       subtitle=F, show_weekly = input$checkbox_pop)
    
  })
  
  # # Compare Active Cases & Recent Tests -------------------------------------------------
  # 
  # # Determine which counties to plot
  # loc_to_plot_both_active <- reactive({input$select_both_active})
  # 
  # # Get testing/active cases data by county
  # df_by_county_active <- get_df_by_county(sjc_num_df, "ps") %>%
  #   filter(!County %in% c("All", "DOC"))
  # 
  # # Get testing/active cases data by DOC facility
  # df_by_fac_active <- get_df_by_fac(sjc_num_df, sjc_DOC_num_df, "ps") %>%
  #   filter(!fac %in% c("DOC Total**", "Non-Facility"),
  #          !is.na(fac)) %>%
  #   mutate(fac = 
  #    case_when(
  #       !str_detect(fac, "DOC") ~ paste("DOC:", fac),
  #       T ~ as.character(fac)
  #     ),
  #     all_active = replace_na(all_active, 0),
  #     Date = Date + days(1)
  #     ) %>%
  #   rename(loc = fac)
  # 
  # # Combine fac & county data
  # df_by_loc_active <- df_by_county_active %>%
  #   mutate(loc = as.character(County)) %>%
  #   bind_rows(df_by_fac_active) %>%
  #   dplyr::select(Date, loc, all_tested, all_active)
  # 
  # df_all_all_active <- df_by_loc_active %>%
  #   filter(startsWith(loc, "All")) %>%
  #   group_by(Date) %>%
  #   summarize(all_tested = sum(all_tested),
  #             all_active = sum(all_active)) %>%
  #   mutate(loc = "All")
  # 
  # df_by_loc_active <- df_by_loc_active %>%
  #   bind_rows(df_all_all_active) %>%
  #   mutate(loc = ifelse(loc == "All DOC Facilities", "DOC", loc)) %>%
  #   group_by(loc) %>%
  #   # Calculate number of tests in the last 2 weeks
  #   complete(Date = full_seq(Date, period = 1), fill = list(all_tested = 0)) %>%
  #   mutate(all_tested_rolling14 = zoo::rollapplyr(all_tested, width = 14, FUN = sum, partial = TRUE)) %>%
  #   # Only plot the weekly number
  #   filter((interval(ymd(20200708), Date) / days(1)) %% 7 == 0,
  #          Date >= ymd(20200708)) %>%
  #   dplyr::select(-all_tested)
  # 
  # output$both_plot_active <- renderPlotly({
  #     
  #   y_label_tests = "Prisoners Tested in Preceding 2 Weeks"
  #   
  #   gg_plot_tests <- df_by_loc_active %>%
  #     filter(loc == loc_to_plot_both_active()) %>%
  #     ggplot(aes(x=Date, y = all_tested_rolling14)) +
  #     geom_path(size=1.3, show.legend = T, alpha=0.8, color="#0055aa") +
  #     geom_point(size=1.5, color="#0055aa") +
  #     labs(x = "", y = "Prisoners", color="",
  #          title = y_label_tests,
  #          subtitle="Cumulative pursuant to SJC 12926") +
  #     theme(plot.title= element_text(family="gtam", face='bold'),
  #           text = element_text(family="gtam", size = 16),
  #           plot.margin = unit(c(1,1,4,1), "lines"),
  #           legend.position = c(.5, -.22), 
  #           legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
  #           legend.key.width = unit(1, "cm"),
  #           legend.text = element_text(size=16)) +
  #     scale_x_date(date_labels = "%b %e ") +
  #     coord_cartesian(clip = 'off')
  #   
  #   plotly_tests <- lines_plotly_style(gg_plot_tests, y_label_tests, 
  #                                      "Location", active_and_recent=T, 
  #                                      show_weekly=F) %>%
  #     add_annotations(
  #       text = "<b>Prisoners Tested in Preceding 2 Weeks</b>",
  #       x = 0,
  #       y = 1.05,
  #       yref = "paper",
  #       xref = "paper",
  #       xanchor = "middle",
  #       yanchor = "bottom",
  #       showarrow = FALSE,
  #       font = list(size = 20, color="#0055aa")
  #     )
  #   
  #   y_label_active = "Prisoners with Active Cases"
  #   
  #   gg_plot_active <- df_by_loc_active %>%
  #     filter(loc == loc_to_plot_both_active()) %>%
  #     ggplot(aes(x=Date, y = all_active)) +
  #     geom_path(size=1.3, show.legend = T, alpha=0.8, color="#fbb416") +
  #     geom_point(size=1.5, color="#fbb416") +
  #     labs(x = "", y = "Prisoners", color="",
  #          title = y_label_active,
  #          subtitle="Cumulative pursuant to SJC 12926") +
  #     theme(plot.title= element_text(family="gtam", face='bold'),
  #           text = element_text(family="gtam", size = 16),
  #           plot.margin = unit(c(1,1,4,1), "lines"),
  #           legend.position = c(.5, -.22), 
  #           legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
  #           legend.key.width = unit(1, "cm"),
  #           legend.text = element_text(size=16)) +
  #     scale_x_date(date_labels = "%b %e ") +
  #     coord_cartesian(clip = 'off')
  #   
  #   plotly_active <- lines_plotly_style(gg_plot_active, "", 
  #                                       "Location", active_and_recent=T, 
  #                                       show_weekly=F, subtitle=F) %>%
  #     add_annotations(
  #       text = "<b>Prisoners with Active Cases</b>",
  #       x = 0,
  #       y = 1.05,
  #       yref = "paper",
  #       xref = "paper",
  #       xanchor = "middle",
  #       yanchor = "bottom",
  #       showarrow = FALSE,
  #       font = list(size = 20, color="#fbb416")
  #     )
  #   
  #   subplot(plotly_tests, plotly_active, 
  #           nrows = 2, margin = 0.07, shareX = TRUE)%>%
  #     layout(height = 500)
  #   
  # })
  
  # Compare Tests & Positives -------------------------------------------------
  
  # Determine which counties to plot
  loc_to_plot_both <- reactive({input$select_both})
  
  # Determine which population to plot
  pop_to_plot_both <- reactive({input$both_radio})
  
  output$both_plot <- renderPlotly({
  
    # Apply population filter
    df_by_county <- get_df_by_county(sjc_num_df, pop_to_plot_both()) %>%
      rename(loc = County) %>%
      mutate(loc = as.character(loc))
    
    df_by_fac <- get_df_by_fac(sjc_num_df, sjc_DOC_num_df, pop_to_plot_both()) %>%
      filter(fac != "DOC Total**", 
             fac != "All DOC Facilities",
             fac != "Non-Facility") %>%
      filter(!is.na(fac)) %>%
      mutate(fac = paste("DOC:", fac)) %>%
      rename(loc = fac)
    
    # Combine fac & county data
    df_by_loc <- df_by_county %>%
      bind_rows(df_by_fac)
    
    # Determine what label is
    if (pop_to_plot_both() == "ps") {
      y_label = "Prisoners & Staff Tested & Positive"
    } else if (pop_to_plot_both() == "p") {
      y_label = "Prisoners Tested & Positive"
    } else if (pop_to_plot_both() == "s") {
      y_label = "Staff Tested & Positive"
    }
    
    # Pull out string for what population we're plotting
    pop_to_annotate <- str_split(y_label, " Tested")[[1]][1]
    
    g <- df_by_loc %>%
      filter(loc == loc_to_plot_both()) %>%
      mutate(cumul_Tested = NA, cumul_Positive = NA)
    
    # Deal with cumsum NA issue
    g$cumul_Tested[!is.na(g$all_tested)] <- cumsum(g$all_tested[!is.na(g$all_tested)])
    g$cumul_Positive[!is.na(g$all_positive)] <- cumsum(g$all_positive[!is.na(g$all_positive)])
    
    g <- g %>%
      dplyr::select(-starts_with("all")) %>%
      pivot_longer(cols = starts_with("cumul"), names_to = "type", 
                   names_prefix = "cumul_") %>%
    ggplot(aes(x=Date, y = value, color=type)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      labs(x = "", y = pop_to_annotate, color="",
           title = paste("Positive COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), 
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
  
    lines_plotly_style(g, y_label, "Location", pos_and_test=T, 
                       show_weekly=input$checkbox_both)
    
  })
  
  # Total Releases -------------------------------------------------------
  released_df <- sjc_num_df %>%
    pivot_longer(cols=matches("Released", ignore.case=F),
                 names_to="type",
                 names_prefix="N Released ")
  
  # Determine which variable to plot
  select_release <- reactive({ input$select_release })

  output$all_releases_plot <- renderPlotly({
    
    if (select_release() == "All") {
      
      output$n_releases_str <- renderText({format(n_released, big.mark=",")})
      
      stacked_bar_plot(released_df, 
                      "Prisoners Released",
                      "County")
      
    } else if (select_release() %in% c("Pre-Trial", "Sentenced", "Home Confinements")) {
      
      output$n_releases_str <- renderText({
        released_df %>%
          filter(type == select_release()) %>%
          pull(value) %>%
          sum() %>%
          format(big.mark=",")
      })
      
      single_bar_plot(released_df, 
                      select_release(), 
                      paste(select_release(), "Prisoners Released"),
                      "County")
      
    } else if (select_release() == "Total") {
      
      output$n_releases_str <- renderText({format(n_released, big.mark=",")})
      
      single_bar_plot(released_df, 
                      select_release(), "Prisoners Released",
                      "County")
      
    }
    
  })
  
  # Releases v. Time -------------------------------------------------------
  
  df_by_county_rel <- get_df_by_county(sjc_num_df, "p")
  
  # Determine which counties to plot
  cnty_to_plot_rel <- reactive({
    c(input$select_county1_rel,
      input$select_county2_rel,
      input$select_county3_rel)
  })
  
  # Plot
  output$releases_v_time_plot <- renderPlotly({
    
    g <- df_by_county_rel %>%
      filter(County %in% cnty_to_plot_rel()) %>%
      group_by(County) %>%
      mutate(cumul = cumsum(all_released)) %>%
    ggplot(aes(x=Date, y = cumul, color=County)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      labs(x = "", y = "Total Prisoners Released", color="",
           title = paste("Prisoners Released over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22),
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Prisoners Released", "County", show_weekly=input$checkbox_rel)
    
  })
  
  # Total Positives -------------------------------------------------------
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
    
    # Hide DOC column?
    if (input$checkbox_hideDOC_pos) {
      positive_df_to_plot <- positive_df %>%
        filter(County != "DOC")
    } else {
      positive_df_to_plot <- positive_df
    }
    
    
    if (select_positive() == "All") {
      output$n_positive_str <- renderText({format(n_positive, big.mark=",")})
      output$type_positive <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(positive_df_to_plot, 
                       "Tested Positive",
                       "County")
      
    } else if (select_positive() %in% c("Prisoners", "Staff")) {
      output$n_positive_str <- renderText({
        positive_df %>%
          filter(type == select_positive()) %>%
          pull(value) %>%
          sum() %>%
          format(big.mark=",")
      })
      output$type_positive <- renderText({tolower(select_positive())})
      
      single_bar_plot(positive_df_to_plot, 
                      select_positive(), 
                      paste(select_positive(), "Tested Positive"),
                      "County")

    } else if (select_positive() == "Total") {
      output$n_positive_str <- renderText({format(n_positive, big.mark=",")})
      output$type_positive <- renderText({"prisoners and staff"})
      
      single_bar_plot(positive_df_to_plot, 
                      select_positive(), "Prisoners & Staff Tested Positive",
                      "County")
    }
  })
  
  # Positives v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  cnty_to_plot_pos <- reactive({
    c(input$select_county1_pos,
      input$select_county2_pos,
      input$select_county3_pos)
  })
  
  # Determine which population to plot
  pop_to_plot_pos <- reactive({input$positive_radio})
  
  # Plot
  output$positives_v_time_plot <- renderPlotly({
    
    # annotate_tests_df <- df_by_county %>%
    #   filter(County %in% cnty_to_plot_pos()) %>%
    #   group_by(County) %>%
    #   summarize(all_tests = sum(all_tested))
    # 
    # annotate_tests <- paste0(paste0(annotate_tests_df$County, ": ", 
    #                                 annotate_tests_df$all_tests, " tested"), 
    #                          collapse="\n")
    
    # Apply population filter
    df_by_county <- get_df_by_county(sjc_num_df, pop_to_plot_pos())
    
    # Determine what label is
    if (pop_to_plot_pos() == "ps") {
      y_label = "Prisoners & Staff Tested Positive"
    } else if (pop_to_plot_pos() == "p") {
      y_label = "Prisoners Tested Positive"
    } else if (pop_to_plot_pos() == "s") {
      y_label = "Staff Tested Positive"
    }
    
    pos_to_plot <- df_by_county %>%
      filter(County %in% cnty_to_plot_pos()) %>%
      group_by(County) %>%
      mutate(cumul = cumsum(all_positive))
      
    g <- pos_to_plot %>%
      ggplot(aes(x=Date, y = cumul, color=County)) +
        geom_path(size=1.3, show.legend = T, alpha=0.8) +
        # annotate("label", min(pos_to_plot$Date), Inf, label=annotate_tests,
        #          vjust=2, hjust=0, fill="grey", alpha=0.5, 
        #          label.size=NA, label.r=unit(0, "cm"), label.padding = unit(0.5, "lines")) +
        labs(x = "", y = y_label, color="",
             title = paste("Positive COVID-19 Tests over Time"),
             subtitle="Cumulative pursuant to SJC 12926") +
        theme(plot.title= element_text(family="gtam", face='bold'),
              text = element_text(family="gtam", size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              legend.position = c(.5, -.22), 
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
              legend.key.width = unit(1, "cm"),
              legend.text = element_text(size=16)) +
        scale_x_date(date_labels = "%b %e ") +
        scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
        coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "County", 
                       annotation=TRUE, show_weekly=input$checkbox_pos)
    
  })
  
  # Active Positives -------------------------------------------------------
  
  active_doc_df_to_add <- sjc_DOC_num_df %>%
    group_by(fac) %>%
    filter(Date == max(Date)) %>%
    mutate(value = as.numeric(`Active Prisoner Cases`),
           County = factor("DOC", levels=counties),
           value = replace_na(value, 0))
  
  active_df <- sjc_num_df %>%
    filter_at(vars(-County, -Date), any_vars(. != 0)) %>%
    group_by(County) %>%
    filter(Date == max(Date)) %>%
    mutate(value = as.numeric(`Active Prisoner Cases`)) %>%
    bind_rows(active_doc_df_to_add)
  
  output$last_date_str <- renderText({
    strftime(last_date_entered, format="%B %d, %Y"
    )})
  
  output$all_active_plot <- renderPlotly({
    
    n_active <- sum(active_df$value)
    output$n_active_str <- renderText({format(n_active, big.mark=",")})
    
    single_bar_plot(active_df, "Total", "Active Positive Prisoners", "County")
  
  })
  
  # Active Positives v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  cnty_to_plot_active <- reactive({
    c(input$select_active1,
      input$select_active2,
      input$select_active3)
  })
  
  active_DOC_to_add <- sjc_DOC_num_df %>%
    filter(Date >= ymd(20200707)) %>%
    mutate(active = `Active Prisoner Cases`,
           County = factor("DOC", levels=counties)) %>%
    filter(!is.na(active)) %>%
    dplyr::select(Date, County, active)
  
  active_v_time <- sjc_num_df %>%
    filter(Date >= ymd(20200707),
           County != "DOC") %>%
    mutate(active = `Active Prisoner Cases`) %>%
    filter(!is.na(active)) %>%
    dplyr::select(Date, County, active) %>%
    bind_rows(active_DOC_to_add) %>%
    group_by(Date, County) %>%
    summarize(active=sum(active))
  
  all_active_v_time <- active_v_time %>%
    ungroup() %>%
    mutate(Date = if_else(County == "DOC", Date + days(1), Date),
           County = "All") %>%
    group_by(Date, County) %>%
    summarize(active = sum(active))
  
  all_cty_active_v_time <- active_v_time %>%
    ungroup() %>%
    filter(County != "DOC") %>%
    mutate(County = "All Counties") %>%
    group_by(Date, County) %>%
    summarize(active = sum(active))
  
  active_v_time <- active_v_time %>%
    bind_rows(all_active_v_time) %>%
    bind_rows(all_cty_active_v_time)
  
  # Plot
  output$active_v_time_plot <- renderPlotly({
    
    g <- active_v_time %>%
      filter(County %in% cnty_to_plot_active()) %>%
    ggplot(aes(x=Date, y = active, color=County)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      geom_point(size=1.5) +
      labs(x = "", y = "Active Positive Prisoners", color="",
           title="placeholder") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), 
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Active Positive Prisoners", "County", 
                       show_weekly=F, subtitle=F)
    
  })
  
  # Total Tests -------------------------------------------------------
  
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
    
    # Hide DOC column?
    if (input$checkbox_hideDOC_tests) {
      tested_df_to_plot <- tested_df %>%
        filter(County != "DOC")
    } else {
      tested_df_to_plot <- tested_df
    }
    
    if (select_tested() == "All") {
      
      output$n_tests_str <- renderText({format(n_tested, big.mark=",")})
      output$type_tested <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(tested_df_to_plot, 
                       "Tested",
                       "County")
      
    } else if (select_tested() %in% c("Prisoners", "Staff")) {
      output$n_tests_str <- renderText({
        tested_df %>%
          filter(type == select_tested()) %>%
          pull(value) %>%
          sum() %>%
          format(big.mark=",")
      })
      output$type_tested <- renderText({tolower(select_tested())})
      
      single_bar_plot(tested_df_to_plot, 
                      select_tested(), 
                      paste(select_tested(), "Tested"),
                      "County")
      
    } else if (select_tested() == "Total") {
      output$n_tests_str <- renderText({format(n_tested, big.mark=",")})
      output$type_tested <- renderText({"prisoners and staff"})
      
      single_bar_plot(tested_df_to_plot, 
                      select_tested(),
                      "Prisoners & Staff Tested",
                      "County")
      
    }
    
  })
  
  # Tests v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  cnty_to_plot_test <- reactive({
    c(input$select_county1_test,
      input$select_county2_test,
      input$select_county3_test)
  })
  
  # Determine which population to plot
  pop_to_plot_test <- reactive({input$test_radio})
  
  # Plot
  output$tests_v_time_plot <- renderPlotly({
    
    # Apply population filter
    df_by_county <- get_df_by_county(sjc_num_df, pop_to_plot_test())
    
    # Determine what label is
    if (pop_to_plot_test() == "ps") {
      y_label = "Prisoners & Staff Tested"
    } else if (pop_to_plot_test() == "p") {
      y_label = "Prisoners Tested"
    } else if (pop_to_plot_test() == "s") {
      y_label = "Staff Tested"
    }
    
    g <- df_by_county %>%
      filter(County %in% cnty_to_plot_test()) %>%
      group_by(County) %>%
      mutate(cumul = cumsum(all_tested)) %>%
    ggplot(aes(x=Date, y = cumul, color=County)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      labs(x = "", y = y_label, color="",
           title = paste("COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), 
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "County", show_weekly=input$checkbox_test)
    
  })
  
  # Total Deaths -------------------------------------------------------
  
  deaths_doc_df_to_add <- sjc_DOC_num_df %>%
    mutate(value = as.numeric(`N Deaths`),
           County = factor("DOC", levels=counties)) %>%
    dplyr::select(Date, County, value)
  
  deaths_df <- sjc_num_df %>%
    mutate(value = as.numeric(`N Deaths`)) %>%
    dplyr::select(Date, County, value) %>%
    bind_rows(deaths_doc_df_to_add) %>%
    mutate(value = replace_na(value, 0))
  
  # Calculate totals
  n_deaths <- sum(deaths_df$value)
  output$n_deaths_str <- renderText({format(n_deaths, big.mark=",")})
  
  # Plot deaths
  output$all_deaths_plot <- renderPlotly({
    
    single_bar_plot(deaths_df, 
                    "Total", "Prisoner Deaths", "County")
    
  })
  
 # DOC: Total Releases -----------------------------------------------------
  DOC_released_df <- sjc_DOC_num_df %>%
    rename(value = all_released,
           Facility = fac) %>%
    dplyr::select(Date, Facility, value) %>%
    filter(!is.na(value))
  
  # Calculate totals
  n_released_DOC <- sum(DOC_released_df$value)
  output$n_releases_DOC_str <- renderText({
    paste0(format(n_released_DOC, big.mark=","), 
           "*")
  })
  
  output$all_releases_DOC_plot <- renderPlotly({
  
    single_bar_plot(DOC_released_df, 
                    "Total", "Prisoners Released",
                    "Facility")
  
  })
  
  # DOC: Releases v. Time -----------------------------------------------------
  df_by_fac_rel <- get_df_by_fac(sjc_num_df, sjc_DOC_num_df, "p")
  
  # Determine which counties to plot
  fac_to_plot_rel <- reactive({
    c(input$select_fac1_rel,
      input$select_fac2_rel,
      input$select_fac3_rel)
  })
  
  # Plot
  output$DOC_releases_v_time_plot <- renderPlotly({
    
    g <- df_by_fac_rel %>%
      filter(fac %in% fac_to_plot_rel(),
             !is.na(all_released)) %>%
      group_by(fac) %>%
      mutate(cumul = cumsum(all_released)) %>%
    ggplot(aes(x=Date, y = cumul, color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      labs(x = "", y = "Total Prisoners Released", color="",
           title = paste("Prisoners Released over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22),
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Prisoners Released", "Facility",
                       show_weekly=input$checkbox_fac_rel)
    
  })
  
  # DOC: Total Tests -----------------------------------------------------
  fac_tests_df <- sjc_DOC_num_df %>%
    rename(`N Tested - Prisoners`=`N Tested - Detainees/Inmates`,
           `N Tested - Staff`=`N Tested - COs`,
           Facility = fac) %>%
    pivot_longer(cols=matches("N Tested", ignore.case=F),
                 names_to="type",
                 names_prefix="N Tested - ") %>%
    filter(!is.na(value))
  
  
  # Determine which variable to plot
  select_tests_fac <- reactive({ input$select_tests_fac })
  
  # Plot
  output$DOC_tests_plot <- renderPlotly({
    
    if (select_tests_fac() == "All") {
      
      output$n_tests_DOC_str <- renderText({
        paste0(format(sum(sjc_DOC_num_df$all_tested, na.rm=T), big.mark=","), 
               "*")
      })
      
      output$type_tests_fac <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(fac_tests_df, 
                       "Tested",
                       "Facility")
      
    } else if (select_tests_fac() %in% c("Prisoners", "Staff")) {
      
      output$n_tests_DOC_str <- renderText({
        fac_tests_df %>%
          filter(type == select_tests_fac()) %>%
          pull(value) %>%
          sum(na.rm=T) %>%
          format(big.mark=",") %>%
          paste0("*")
      })
      output$type_tests_fac <- renderText({tolower(select_tests_fac())})
      
      single_bar_plot(fac_tests_df, 
                      select_tests_fac(), 
                      paste(select_tests_fac(), "Tested"),
                      "Facility")
      
    } else if (select_tests_fac() == "Total") {
      
      output$n_tests_DOC_str <- renderText({
        paste0(format(sum(sjc_DOC_num_df$all_tested, na.rm=T), big.mark=","), 
               "*")
      })
      output$type_tests_fac <- renderText({"prisoners and staff"})
      
      single_bar_plot(fac_tests_df, 
                      select_tests_fac(),
                      "Prisoners & Staff Tested",
                      "Facility")
    }
  })
  
  # DOC: Tests v. Time -----------------------------------------------------
  # Determine which counties to plot
  fac_to_plot_test <- reactive({
    c(input$select_fac1_test,
      input$select_fac2_test,
      input$select_fac3_test)
  })
  
  # Determine which population to plot
  pop_to_plot_test_doc <- reactive({input$doc_test_radio})
  
  # Plot
  output$doc_tests_v_time_plot <- renderPlotly({
    
    # Apply population filter
    df_by_fac <- get_df_by_fac(sjc_num_df, sjc_DOC_num_df, pop_to_plot_test_doc())
    
    # Determine what label is
    if (pop_to_plot_test_doc() == "ps") {
      y_label = "Prisoners & Staff Tested"
    } else if (pop_to_plot_test_doc() == "p") {
      y_label = "Prisoners Tested"
    } else if (pop_to_plot_test_doc() == "s") {
      y_label = "Staff Tested"
    }
    
    g <- df_by_fac %>%
      filter(fac %in% fac_to_plot_test(),
             !is.na(all_tested)) %>%
      group_by(fac) %>%
      mutate(cumul = cumsum(all_tested)) %>%
    ggplot(aes(x=Date, y = cumul, color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      labs(x = "", y = y_label, color="",
           title = paste("COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), 
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "Facility", 
                       show_weekly=input$checkbox_fac_test)
    
    
  })
  
  # DOC: Total Positives -----------------------------------------------------
  fac_positive_df <- sjc_DOC_num_df %>%
    # TEMPORARY combine DOC & Non-DOC staff
    rowwise() %>%
    mutate(`N Positive - Staff` = sum(`N Positive - COs`, `N Positive - Other Staff`, na.rm=T)) %>%
    dplyr::select(-`N Positive - COs`, -`N Positive - Other Staff`) %>%
    # Rename prisoner column to work with bar_plot() functions
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
        paste0(format(sum(sjc_DOC_num_df$all_positive, na.rm=T), big.mark=","), 
               "*")
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
          format(big.mark=",") %>%
          paste0("*")
      })
      output$type_positive_fac <- renderText({tolower(select_positive_fac())})
      
      single_bar_plot(fac_positive_df, 
                      select_positive_fac(), 
                      paste(select_positive_fac(), "Tested Positive"),
                      "Facility")
      
    } else if (select_positive_fac() == "Total") {
      
      output$n_positive_DOC_str <- renderText({
        paste0(format(sum(sjc_DOC_num_df$all_positive, na.rm=T), big.mark=","),
               "*")
      })
      output$type_positive_fac <- renderText({"prisoners and staff"})
      
      single_bar_plot(fac_positive_df, 
                      select_positive_fac(),
                      "Prisoners & Staff Tested Positive",
                      "Facility")
    }
  })
  
  # DOC: Positives v. Time -----------------------------------------------------
  # Determine which facilities to plot
  fac_to_plot <- reactive({
    c(input$select_fac1,
      input$select_fac2,
      input$select_fac3)
  })
  
  # Determine which population to plot
  pop_to_plot_doc_pos <- reactive({input$doc_positive_radio})
  
  # Plot
  output$DOC_time_plot <- renderPlotly({
    
    # Apply population filter
    df_by_fac <- get_df_by_fac(sjc_num_df, sjc_DOC_num_df, 
                               pop_to_plot_doc_pos())
    
    # Determine what label is
    if (pop_to_plot_doc_pos() == "ps") {
      y_label = "Prisoners & Staff Tested Positive"
    } else if (pop_to_plot_doc_pos() == "p") {
      y_label = "Prisoners Tested Positive"
    } else if (pop_to_plot_doc_pos() == "s") {
      y_label = "Staff Tested Positive"
    }
    
    g <- df_by_fac %>%
      filter(fac %in% fac_to_plot()) %>%
      group_by(fac) %>%
      mutate(cumul = cumsum(all_positive)) %>%
      filter(fac == "DOC Total**" | (fac != "DOC Total**" & Date >= ymd(20200415))) %>%
    ggplot(aes(x=Date, y = cumul, 
                 color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.7) +
      labs(x = "", y = y_label, color="",
           title = paste("Positive COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22),
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "Facility", 
                       show_weekly=input$checkbox_fac_pos)
    
  })
  
  # DOC: Active Positives -------------------------------------------------------
  
  active_fac_df <- sjc_DOC_num_df %>%
    group_by(fac) %>%
    filter(Date == max(Date)) %>%
    mutate(value = `Active Prisoner Cases`,
           value = replace_na(value, 0),
           Facility = fac)
  
  output$all_active_fac_plot <- renderPlotly({
    
    output$last_date_DOC_str <- renderText({
      strftime(max(sjc_DOC_num_df$Date), format="%B %d, %Y"
      )})
    
    n_active_fac <- sum(active_fac_df$value)
    output$n_active_fac_str <- renderText({format(n_active_fac, big.mark=",")})
    
    single_bar_plot(active_fac_df, "Total", "Active Positive Prisoners", "Facility")
    
  })
  
  # DOC: Active Positives v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  fac_to_plot_active <- reactive({
    c(input$select_active_fac1,
      input$select_active_fac2,
      input$select_active_fac3)
  })
  
  active_fac_v_time <- sjc_DOC_num_df %>%
    filter(Date >= ymd(20200707)) %>%
    mutate(active = `Active Prisoner Cases`,
           fac = as.character(fac)) %>%
    filter(!is.na(active)) %>%
    dplyr::select(Date, fac, active)
  
  all_active_fac_v_time <- active_fac_v_time %>%
    ungroup() %>%
    mutate(fac = "All DOC Facilities") %>%
    group_by(Date, fac) %>%
    summarize(active = sum(active))
  
  active_fac_v_time <- active_fac_v_time %>%
    bind_rows(all_active_fac_v_time)
  
  # Plot
  output$active_v_time_fac_plot <- renderPlotly({
    
    g <- active_fac_v_time %>%
      filter(fac %in% fac_to_plot_active()) %>%
    ggplot(aes(x=Date, y = active, color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      geom_point(size=1.5) +
      labs(x = "", y = "Active Positive DOC Prisoners", color="",
           title="placeholder") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), 
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Active Positive DOC Prisoners", "Facility", 
                       show_weekly=F, subtitle=F)
    
  })
  
  # DOC: Total Deaths -------------------------------------------------------
  
  deaths_fac_df <- sjc_DOC_num_df %>%
    mutate(value = as.numeric(`N Deaths`),
           value = replace_na(value, 0),
           Facility = factor(fac, levels=fac_staff)) %>%
    dplyr::select(Date, Facility, value)
  
  # Calculate totals
  n_deaths_fac <- sum(deaths_fac_df$value)
  output$n_deaths_fac_str <- renderText({format(n_deaths_fac, big.mark=",")})
  
  # Plot deaths
  output$all_deaths_fac_plot <- renderPlotly({
    
    single_bar_plot(deaths_fac_df, 
                    "Total", "Prisoner Deaths", "Facility")
    
  })
  
  # Counties: Total Tests -------------------------------------------------------
  cty_tests_df <- sjc_county_num_df %>%
    group_by(Date, fac) %>%
    mutate(`N Tested - Staff` = sum(`N Tested - COs`,
                                    `N Tested - Staff`, 
                                    `N Tested - Contractors`, na.rm=T)) %>%
    rename(`N Tested - Prisoners`=`N Tested - Detainees/Inmates`,
           Facility = fac) %>%
    dplyr::select(-`N Tested - COs`, -`N Tested - Contractors`) %>%
    pivot_longer(cols=matches("N Tested", ignore.case=F),
                 names_to="type",
                 names_prefix="N Tested - ") %>%
    filter(!is.na(value))
  
  # Determine which variable to plot
  select_tests_cty <- reactive({ input$select_tests_cty })
  
  output$cty_tests_plot <- renderPlotly({
    
    if (select_tests_cty() == "All") {
      output$n_tests_cty_str <- renderText({
        paste0(format(sum(sjc_county_num_df$all_tested, na.rm=T), 
                      big.mark=","), 
               "*")
      })
      output$type_tests_cty <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(cty_tests_df, 
                       "Tested",
                       "County Facility")
      
    } else if (select_tests_cty() %in% c("Prisoners", "Staff")) {
      output$n_tests_cty_str <- renderText({
        cty_tests_df %>%
          filter(type == select_tests_cty()) %>%
          pull(value) %>%
          sum(na.rm=T) %>%
          format(big.mark=",") %>%
          paste0("*")
      })
      output$type_tests_cty <- renderText({tolower(select_tests_cty())})
      
      single_bar_plot(cty_tests_df, 
                      select_tests_cty(), 
                      paste(select_tests_cty(), "Tested"),
                      "County Facility")
      
    } else if (select_tests_cty() == "Total") {
      output$n_tests_cty_str <- renderText({
        paste0(format(sum(sjc_county_num_df$all_tested, na.rm=T), 
                      big.mark=","), 
               "*")
      })
      output$type_tests_cty <- renderText({"prisoners and staff"})
      
      single_bar_plot(cty_tests_df, 
                      select_tests_cty(), "Prisoners & Staff Tested",
                      "County Facility")
    }
  })
  
  # Counties: Tests v. Time -------------------------------------------------------
  # Determine which facilities to plot
  ctyfac_to_plot <- reactive({
    c(input$select_ctyfac_test1,
      input$select_ctyfac_test2,
      input$select_ctyfac_test3)
  })
  
  # Determine which population to plot
  pop_to_plot_cty <- reactive({input$cty_tests_radio})
  
  # Plot
  output$cty_tests_v_time_plot <- renderPlotly({
    
    # Apply population filter
    df_by_county_fac <- get_df_by_county_fac(sjc_num_df, sjc_county_num_df, 
                                             pop_to_plot_cty())
    
    # Determine what label is
    if (pop_to_plot_cty() == "ps") {
      y_label = "Prisoners & Staff Tested"
    } else if (pop_to_plot_cty() == "p") {
      y_label = "Prisoners Tested"
    } else if (pop_to_plot_cty() == "s") {
      y_label = "Staff Tested"
    }
    
    g <- df_by_county_fac %>%
      filter(fac %in% ctyfac_to_plot()) %>%
      group_by(fac) %>%
      mutate(cumul = cumsum(all_tested)) %>%
      ggplot(aes(x=Date, y = cumul, 
                 color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.7) +
      labs(x = "", y = y_label, color="",
           title = paste("COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22),
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "Location",
                       show_weekly=input$checkbox_ctyfac_test)
    
  })
  
  # Counties: Total Positives -------------------------------------------------------
  cty_positive_df <- sjc_county_num_df %>%
    group_by(Date, fac) %>%
    mutate(`N Positive - Staff` = sum(`N Positive - COs`,
                                    `N Positive - Staff`,
                                    `N Positive - Contractors`, na.rm=T)) %>%
    rename(`N Positive - Prisoners`=`N Positive - Detainees/Inmates`,
           Facility = fac) %>%
    dplyr::select(-`N Positive - COs`, -`N Positive - Contractors`) %>%
    pivot_longer(cols=matches("N Positive", ignore.case=F),
                 names_to="type",
                 names_prefix="N Positive - ") %>%
    filter(!is.na(value))
  
  # Determine which variable to plot
  select_positive_cty <- reactive({ input$select_positive_cty })
  
  output$cty_positives_plot <- renderPlotly({
    
    if (select_positive_cty() == "All") {
      output$n_positive_cty_str <- renderText({
        paste0(format(sum(sjc_county_num_df$all_positive, na.rm=T),
                      big.mark=","), 
               "*")
      })
      output$type_positive_cty <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(cty_positive_df, 
                       "Tested Positive",
                       "County Facility")
      
    } else if (select_positive_cty() %in% c("Prisoners", "Staff")) {
      output$n_positive_cty_str <- renderText({
        cty_positive_df %>%
          filter(type == select_positive_cty()) %>%
          pull(value) %>%
          sum(na.rm=T) %>%
          format(big.mark=",") %>%
          paste0("*")
      })
      output$type_positive_cty <- renderText({tolower(select_positive_cty())})
      
      single_bar_plot(cty_positive_df, 
                      select_positive_cty(), 
                      paste(select_positive_cty(), "Tested Positive"),
                      "County Facility")
      
    } else if (select_positive_cty() == "Total") {
      output$n_positive_cty_str <- renderText({
        paste0(format(sum(sjc_county_num_df$all_positive, na.rm=T), 
                      big.mark=","),
               "*")
      })
      output$type_positive_cty <- renderText({"prisoners and staff"})
      
      single_bar_plot(cty_positive_df, 
                      select_positive_cty(), "Prisoners & Staff Tested Positive",
                      "County Facility")
    }
  })
  
  # Counties: Positives v. Time -------------------------------------------------------
  # Determine which facilities to plot
  ctyfac_to_plot_pos <- reactive({
    c(input$select_ctyfac_pos1,
      input$select_ctyfac_pos2,
      input$select_ctyfac_pos3)
  })
  
  # Determine which population to plot
  pop_to_plot_cty_pos <- reactive({input$cty_positive_radio})
  
  # Plot
  output$ctyfac_pos_time_plot <- renderPlotly({
    
    # Apply population filter
    df_by_county_fac_pos <- get_df_by_county_fac(sjc_num_df, sjc_county_num_df, 
                                             pop_to_plot_cty_pos())
    
    # Determine what label is
    if (pop_to_plot_cty_pos() == "ps") {
      y_label = "Prisoners & Staff Tested Positive"
    } else if (pop_to_plot_cty_pos() == "p") {
      y_label = "Prisoners Tested Positive"
    } else if (pop_to_plot_cty_pos() == "s") {
      y_label = "Staff Tested Positive"
    }
    
    g <- df_by_county_fac_pos %>%
      filter(fac %in% ctyfac_to_plot_pos()) %>%
      group_by(fac) %>%
      mutate(cumul = cumsum(all_positive)) %>%
    ggplot(aes(x=Date, y = cumul, 
                 color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.7) +
      labs(x = "", y = y_label, color="",
           title = paste("Positive COVID-19 Tests over Time"),
           subtitle="Cumulative pursuant to SJC 12926") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22),
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "Location",
                       show_weekly=input$checkbox_ctyfac_pos)
    
  })
  
  # # Counties: Active Positives -------------------------------------------------------
  
  active_cty_df <- sjc_county_num_df %>%
    group_by(fac) %>%
    filter(Date == max(Date)) %>%
    mutate(value = `Active Prisoner Cases`,
           Facility = fac) %>%
    filter(!is.na(value))
  
  output$last_date_str2 <- renderText({
    strftime(last_date_entered, format="%B %d, %Y"
    )})

  output$all_active_cty_plot <- renderPlotly({

    single_bar_plot(active_cty_df, "Total",
                    "Active Positive Prisoners", "County Facility")

  })
  
  # Counties: Active Positives v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  cty_fac_to_plot_active <- reactive({
    c(input$select_active_cty1,
      input$select_active_cty2,
      input$select_active_cty3)
  })
  
  active_cty_fac_v_time <- sjc_county_num_df %>%
    filter(Date >= ymd(20200708)) %>%
    mutate(active = `Active Prisoner Cases`,
           fac = as.character(fac)) %>%
    dplyr::select(Date, fac, active) %>%
    bind_rows(active_v_time %>% rename(fac=County)) %>%
    filter(fac %in% ctyfac_choices,
           !is.na(active))
  
  # Plot
  output$active_cty_fac_v_time_plot <- renderPlotly({
    
    g <- active_cty_fac_v_time %>%
      filter(fac %in% cty_fac_to_plot_active()) %>%
    ggplot(aes(x=Date, y = active, color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      geom_point(size=1.5) +
      labs(x = "", y = "Active Positive = Prisoners", color="",
           title="placeholder") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16),
            plot.margin = unit(c(1,1,4,1), "lines"),
            legend.position = c(.5, -.22), 
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"),
            legend.text = element_text(size=16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Active Positive Prisoners", "County Facility", 
                       show_weekly=F, subtitle=F)
    
  })
  
  # Counties: Maps ------------------------------------------------------
  
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
    
    mass_cntys_joined <- tigris::geo_join(mass_cntys, sum_sjc_num_df, "NAME", "County")
    
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
  
   # Explore Data -----------------------------------------------------
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
  
  output$cty_df_table <- renderDataTable(
    {sjc_county_df},
    options = list(scrollX = TRUE), 
    filter = 'top'
  )
  
  # Download Data -----------------------------------------------------
  observeEvent(input$link_to_external, {
    updateTabsetPanel(session, "panels", "External Resources")
  })
  observeEvent(input$link_to_external2, {
    updateTabsetPanel(session, "panels", "External Resources")
  })
  observeEvent(input$link_to_external3, {
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
        GET(sjc_googledrive_url, write_disk(file))
      })
    }
  )
  
}

shinyApp(ui = ui, server = server)
