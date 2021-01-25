library(shiny)
library(shinycssloaders)
library(showtext)
library(leaflet)
library(leafsync)

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
                 'MCI-Shirley', 'MTC',  "NECC", 'NCCI-Gardn', 'OCCC', 'Pondville', 
                 'SBCC', 'SMCC')
fac_staff <- c('Boston Pre', 'BSH', 
               'LSH', 'MASAC', 'MCI-C', 'MCI-CJ', 'MCI-F', 'MCI-Norfolk', 
               'MCI-Shirley', 'MTC', "NECC", 'NCCI-Gardn', 'OCCC', 'Pondville', 
               'SBCC', 'SMCC', "Non-Facility")

pop_choices <- c("--", 'All', 'All Counties', "DOC Aggregate", 'DOC: Boston Pre', 'DOC: BSH', 
                 'DOC: LSH', 'DOC: MASAC', 'DOC: MCI-C', 'DOC: MCI-CJ', 'DOC: MCI-F', 
                 'DOC: MCI-Norfolk', 'DOC: MCI-Shirley', 'DOC: MTC', 
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
               
               div(id="dev-info2",
                   wellPanel(
                     icon('info-circle'),
                     h4("New Daily DOC Reporting"),
                     em("On November 9, in response to multiple new outbreaks, the 
                        Department of Correction agreed to return to daily reporting from its facilities. 
                        (Recall that counties and the DOC have been reporting weekly since an", 
                        a("SJC order", href="https://data.aclum.org/wp-content/uploads/2020/07/SJC-12926-Order-Appendix-B.pdf"), 
                        "on June 23.)"), 
                        em("As such, starting November 11, DOC data on this site will be updated on a 
                        daily rather than weekly basis (excluding weekends and holidays). County 
                        data will still update weekly, published by Thursday mornings.")
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
      # UI: Population -----
      tabPanel("Incarcerated Population Over Time",
               wellPanel(id="internal_well",
                         p("Select population to plot.", id="radio_prompt"),
                         radioButtons("pop_radio", label = NULL, 
                                      selected = "pso" , inline = T, 
                                      choiceNames = c("Pretrial", "Sentenced", "Other", "Total"),
                                      choiceValues = c("p", "s", "o","pso")),
                         p("Select up to three locations to plot versus time."),
                         em('While DOC provides', tags$u('total'), 'population counts by facility, its reported numbers of pretrial, sentenced, and other populations are not broken down by facility. As a result, the pretrial, sentenced and other breakdowns under the "All" and "DOC Aggregate" selections below include DOC data in aggregate for all DOC facilities, but this information is not available for each individual DOC facility.'), br(),
                         splitLayout(
                           selectInput("select_county1_pop", label = NULL, choices = pop_choices,
                                       selected = "All", multiple=FALSE),
                           selectInput("select_county2_pop", label = NULL, choices = pop_choices,
                                       selected = "--", multiple=FALSE),
                           selectInput("select_county3_pop", label = NULL, choices = pop_choices,
                                       selected = "--", multiple=FALSE)
                         ),
                         em('Only the following entities report "other" populations:'),
                         tags$ul(
                           tags$li("DOC (in aggregate, not by individual facility): civil commitments"),
                           tags$li("Bristol: ICE detainees on-site"),
                           tags$li("Hampden: Section 35 civil commitments"),
                           tags$li("Plymouth: USMS, ICE, WMS, parole detainer, civil contempt")
                         )),
               checkboxInput("checkbox_pop", label = "Show transition to weekly reporting", value = TRUE),
               withSpinner(plotlyOutput("pop_v_time_plot"), type=4, color="#b5b5b5", size=0.5)
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
                            "facility-level staff positives on April 15, and facility-level testing data on April 25."),
                         em('County "staff" includes correctional officers, contractors, and other staff. DOC "staff" includes both DOC correctional officers and other non-DOC staff.')
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
                            htmlOutput("deaths_list"),
                            tags$li("Before June 23: 10 deaths:"),
                            tags$ul(
                              tags$li("[DOC] MTC -  5 deaths"),
                              tags$li("[DOC] MCI-Shirley -  3 deaths"),
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
                            htmlOutput("deaths_list_DOC"),
                            tags$li("Before June 23: 8 deaths:"),
                            tags$ul(
                              tags$li("MTC -  5 deaths"),
                              tags$li("MCI-Shirley -  3 deaths")
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
                      '"Staff", and "Total" plots.'),
                   em('County "staff" includes correctional officers, contractors, and other staff. DOC "staff" testing data includes only DOC correctional officers, not other non-DOC staff.')
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
                   ),
                   em('County "staff" includes correctional officers, contractors, and other staff. DOC "staff" testing data includes only DOC correctional officers, not other non-DOC staff.')),
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
                              '"Prisoners", "Staff", and "Total" plots.'),
                           em('County "staff" includes correctional officers, contractors, and other staff. DOC "staff" includes both DOC correctional officers and other non-DOC staff.')
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
                   ),
                   em('County "staff" includes correctional officers, contractors, and other staff. DOC "staff" includes both DOC correctional officers and other non-DOC staff.')),
                 checkboxInput("checkbox_pos", label = "Show transition to weekly reporting", value = TRUE),
                 withSpinner(plotlyOutput("positives_v_time_plot"), type=4, color="#b5b5b5", size=0.5),
                 em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
        ),
        
        "----",
        
        tabPanel("Active Positive Cases", 
                 wellPanel(id="internal_well",
                           p("Select kind of individual:"),
                           selectInput("select_active", label = NULL, 
                                       choices = c("All", "Prisoner", "Staff", "Total"),
                                       selected = "All", multiple=FALSE),
                           em('Exact number of tests per county annotated in "Prisoners",',
                              '"Staff", and "Total" plots. Note that the DOC is not 
                              reporting active staff cases. County "staff" includes correctional officers, contractors, and other staff.')
                 ),
                 h2(textOutput("n_active_str"), align="center"),
                 p("Reports of",
                   textOutput("type_active", inline=T),
                   "currently infected with COVID-19, pursuant to SJC 12926",
                   br(),
                   em("Showing active cases as reported on", 
                      textOutput("last_date_str_DOC", inline=T), "(DOC),",
                      textOutput("last_date_str_counties", inline=T), "(counties)",
                      align="center"),
                   align="center"),
                 withSpinner(plotlyOutput("all_active_plot"), type=4, color="#b5b5b5", size=0.5)
                 ),
        
        tabPanel("Active Positive Cases Over Time",
                 wellPanel(id="internal_well",
                           p("Select population to plot.", id="radio_prompt"),
                           radioButtons("active_radio", label = NULL, 
                                        selected = "ps" , inline = T, 
                                        choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                        choiceValues = c("p", "s", "ps")),
                           p("Select up to three locations to plot versus time."),
                           splitLayout(
                             selectInput("select_active1", label = NULL, choices = county_choices,
                                         selected = "All", multiple=FALSE),
                             selectInput("select_active2", label = NULL, choices = county_choices,
                                         selected = "All Counties", multiple=FALSE),
                             selectInput("select_active3", label = NULL, choices = county_choices,
                                         selected = "DOC", multiple=FALSE)
                           ),
                           em('Note that the DOC is not reporting active staff cases. County "staff" includes correctional officers, contractors, and other staff.')),
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
                             "reports going back to March 27.")
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
                              '"Prisoners", "Staff", and "Total" plots.'),
                           em('DOC "staff" testing data includes only DOC correctional officers, not other non-DOC staff.')
                 ),
                 div(align="center",
                     h2(textOutput("n_tests_DOC_str")),
                     p("Reports of",
                       textOutput("type_tests_fac", inline=T),
                       "tested for COVID-19 at individual DOC facilities pursuant to SJC 12926", align="center"),
                     em("*The DOC only began reporting facility-level testing data for prisoners and 
                             DOC staff on April 25. Reports include only tests of DOC correctional officers, not for other staff.",
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
                          em("*The DOC only began reporting facility-level testing data for prisoners and 
                             DOC staff on April 25. Reports include only tests of DOC
                             correctional officers, not for other staff.",
                             "See the Counties + DOC Tests Over Time page for longer-term DOC tracking"),
                          em('**DOC Total reflects DOC-wide reports, and might undercount',
                             "cases as compared to the facility total due to the DOC-wide data reporting",
                             "active, rather than total, cases."),
                          em('DOC "staff" testing data includes only DOC correctional officers, not other non-DOC staff.')
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
                              '"Prisoners", "Staff", and "Total" plots.'),
                           em('DOC "staff" includes both DOC correctional officers and other non-DOC staff.')
                 ),
                 div(align="center",
                   h2(textOutput("n_positive_DOC_str")),
                   p("Reports of",
                     textOutput("type_positive_fac", inline=T),
                     "tested", strong("positive"),
                     "for COVID-19 at individual DOC facilities pursuant to SJC 12926", align="center"),
                   em("*The DOC only began reporting facility-level prisoner data on April 13,",
                      'facility-level DOC staff data on April 15, and facility-level 
                      non-DOC "other" staff data on November 11.',
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
                      "facility-level DOC staff data on April 15, and facility-level non-DOC staff data on November 11.",
                      "See the Counties + DOC Positive Tests Over Time page for longer-term DOC tracking"),
                   em('**DOC Total reflects DOC-wide reports, and might undercount',
                      "cases as compared to the facility total due to the DOC-wide data reporting",
                      "active, rather than total, cases."),
                   em('DOC "staff" includes both DOC correctional officers and other non-DOC staff.')
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
                withSpinner(plotlyOutput("all_active_fac_plot"), type=4, color="#b5b5b5", size=0.5),
                em("The DOC does not report active staff cases.")
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
                          ),
                          em("The DOC does not report active staff cases.")),
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
                                     '"Prisoners", "Staff", and "Total" plots.'),
                                  em('County "staff" includes correctional officers, contractors, and other staff.')
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
                                     "See the Counties + DOC Aggregates: Total Tests page for longer-term totals."),
                                  em('County "staff" includes correctional officers, contractors, and other staff.')
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
                                     '"Prisoners", "Staff", and "Total" plots.'),
                                  em('County "staff" includes correctional officers, contractors, and other staff.')
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
                                     "See the Counties + DOC Aggregates: Total Positive Tests page for longer-term totals."),
                                  em('County "staff" includes correctional officers, contractors, and other staff.')
                        ),
                        checkboxInput("checkbox_ctyfac_pos", label = "Show transition to weekly reporting", value = TRUE),
                        withSpinner(plotlyOutput("ctyfac_pos_time_plot"), type=4, color="#b5b5b5", size=0.5),
                        em("Please note that prisoner deaths due to COVID-19 are not included in these data.")
                ),
               
               "----",

               tabPanel("Active Positive Cases",
                        wellPanel(id="internal_well",
                                  p("Select kind of individual:"),
                                  selectInput("select_active_cty", label = NULL,
                                              choices = c("All", "Prisoner", "Staff", "Total"),
                                              selected = "All", multiple=FALSE),
                                  em('Exact number of tests per facility annotated in',
                                     '"Prisoner", "Staff", and "Total" plots. Essex County
                                     does not report staff positives by facility.'),
                                  em('County "staff" includes correctional officers, contractors, and other staff.')
                        ),
                        p(em("Showing active cases as reported on",
                          textOutput("last_date_str2_counties", inline=T)),   
                           align="center"
                          ),
                        withSpinner(plotlyOutput("all_active_cty_plot"), type=4, color="#b5b5b5", size=0.5)
               ),

               tabPanel("Active Positive Cases Over Time",
                        
                        wellPanel(id="internal_well",
                                  p("Select population to plot.", id="radio_prompt"),
                                  radioButtons("cty_active_radio", label = NULL,
                                               selected = "ps" , inline = T,
                                               choiceNames = c("Prisoners", "Staff", "Prisoners & Staff"),
                                               choiceValues = c("p", "s", "ps")),
                                  p("Select up to three locations to plot versus time."),
                                  splitLayout(
                                    selectInput("select_active_cty1", label = NULL, choices = ctyfac_choices,
                                                selected = "All Counties", multiple=FALSE),
                                    selectInput("select_active_cty2", label = NULL, choices = ctyfac_choices,
                                                selected = "Bristol - DHOC", multiple=FALSE),
                                    selectInput("select_active_cty3", label = NULL, choices = ctyfac_choices,
                                                selected = "Essex - Middleton", multiple=FALSE)
                                  ),
                                  em("Essex County does not report staff positives by facility."),
                                  em('County "staff" includes correctional officers, contractors, and other staff.')
                                  ),
                        withSpinner(plotlyOutput("active_cty_fac_v_time_plot"), type=4, color="#b5b5b5", size=0.5)
               ),
               
               "----",
               # UI: Extras --------------------------------------------------
               
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
      em("\n\nMost recent DOC reports:", textOutput("last_date_str3_DOC", inline=T), 
         align="right", style="opacity: 0.6;"),
      br(),
      em("\n\nMost recent county reports:", textOutput("last_date_str3_counties", inline=T), 
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
