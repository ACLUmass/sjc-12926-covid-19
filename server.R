library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(stringr)
library(showtext)
library(leaflet)
library(leafsync)
library(httr)
library(readxl)
library(tidyr)
library(ggfittext)
library(DT)
library(plotly)
library(scales)

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
                 'MCI-Shirley', 'MTC',  "NECC", 'NCCI-Gardn', 'OCCC', 'Pondville', 
                 'SBCC', 'SMCC')
fac_staff <- c('Boston Pre', 'BSH', 
               'LSH', 'MASAC', 'MCI-C', 'MCI-CJ', 'MCI-F', 'MCI-Norfolk', 
               'MCI-Shirley', 'MTC', "NECC", 'NCCI-Gardn', 'OCCC', 'Pondville', 
               'SBCC', 'SMCC', "Non-Facility")

cty_facs <- c('Bristol - Ash Street Jail', 'Bristol - DHOC', 'Essex - Middleton', 
              'Essex - Prerelease', 'Essex - Women in Transition', 'Hampden - Ludlow', 
               'Hampden - Mill Street', "Hampden - Women's Facility", "Hampden - Section 35", 
              'Suffolk - HOC', 'Suffolk - Jail')
ctyfac_choices <- c("--", "All Counties", cty_facs)

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

function(input, output, session) {
  
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
  output$last_date_str3_DOC <- renderText({
    strftime(last_date_entered_DOC, format="%B %e, %Y"
    )})
  output$last_date_str3_counties <- renderText({
    strftime(last_date_entered_counties, format="%B %e, %Y"
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
  
  output$n_rows <- renderText({
    nrow(sjc_df) %>%
      number(big.mark=",")
    })
  
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
  last_date_entered_DOC <- sjc_num_df %>%
    filter(County == "DOC") %>%
    pull(Date) %>%
    max()
  last_date_entered_counties <- sjc_num_df %>%
    filter(County != "DOC") %>%
    pull(Date) %>%
    max()
  
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
                   matches("Active"), matches("Death"), contains("Population")),
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
  
  # Load Parole Data ----------------------------------------------------------
  
  facs_df <- read.csv("parole_locs.csv")
  
  parole_df <- read_excel(tf, sheet=4) %>%
    rename(date = `Date (Friday)`, value = `N Released Parole`) %>%
    mutate(Date = as.Date(date),
           value = as.numeric(value)) %>%
    filter(!is.na(value)) %>%
    merge(facs_df, by.x=c("Facility", "County"), 
          by.y=c("facility_raw", "County"), all.x=T)
  
  # Load Vaccination Data -----------------------------------------------------------
  
  vax_df <- read_excel(tf, sheet=5) %>%
    mutate_at(vars(-Date, -County, -Notes), as.numeric) %>%
    mutate(Date = as.Date(Date))
  
  # Population v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  cnty_to_plot_pop <- reactive({
    c(input$select_county1_pop,
      input$select_county2_pop,
      input$select_county3_pop)
  })
  
  # Determine which population to plot
  pop_to_plot_pop <- reactive({input$pop_radio})
  
  # Pull out all pop data
  sjc_df_fix_date_pop <- sjc_num_df %>%
    filter(`Total Population` != 0) %>%
    mutate(Date = if_else(County == "DOC" & Date >= ymd(20200707) & Date < ymd(20201109),
                          Date + days(1), Date))
  
  all_pop_df <- sjc_df_fix_date_pop %>%
    select(Date, County, contains("Population"), -`Total Population`) %>%
    pivot_longer(cols=contains("Population"), names_pattern="(.*) Population", 
                 names_to = "pop_type") %>%
    group_by(Date, pop_type) %>%
    filter(n() == 14) %>%
    summarize(value = sum(value)) %>%
    mutate(County = "All")
  
  all_counties_pop_df <- sjc_df_fix_date_pop %>%
    filter(County != "DOC") %>%
    select(Date, County, contains("Population"), -`Total Population`) %>%
    pivot_longer(cols=contains("Population"), names_pattern="(.*) Population", 
                 names_to = "pop_type") %>%
    group_by(Date, pop_type) %>%
    filter(n() == 13) %>%
    summarize(value = sum(value)) %>%
    mutate(County = "All Counties") %>%
    filter(value !=0)
  
  doc_fac_pop_df <- sjc_DOC_num_df %>%
    mutate(value = `Total Population`,
           pop_type = "Total",
           County = paste("DOC:", fac),
           Date = if_else(Date >= ymd(20200707) & Date < ymd(20201109),
                          Date + days(1), Date)) %>%
    select(Date, County, pop_type, value) %>%
    filter(!is.na(value))
  
  county_fac_pop_df <- sjc_county_df %>%
    mutate(County = factor(`County Facility`, levels=cty_facs)) %>%
    select(Date, County, contains("Population"), -`Total Population`) %>%
    pivot_longer(cols=contains("Population"), names_pattern="(.*) Population", 
                 names_to = "pop_type") %>%
    filter(!is.na(value)) 
  
  pop_df <- sjc_df_fix_date_pop %>%
    select(Date, County, contains("Population"), -`Total Population`) %>%
    pivot_longer(cols=contains("Population"), names_pattern="(.*) Population", 
                 names_to = "pop_type") %>%
    bind_rows(all_pop_df) %>%
    bind_rows(all_counties_pop_df) %>%
    bind_rows(doc_fac_pop_df) %>%
    bind_rows(county_fac_pop_df) %>%
    mutate(pop_type = str_replace(pop_type, "Pre-Trial", "Pretrial"),
           County = str_replace(County, "^DOC$", "DOC Aggregate"))
  
  # Plot
  output$pop_v_time_plot <- renderPlotly({
  
    # Determine what label is
    if (pop_to_plot_pop() == "pso") {
      y_label = "Total Prisoners"
      
      pop_df <- pop_df %>%
        group_by(Date, County) %>%
        summarize(value = sum(value))
      
    } else if (pop_to_plot_pop() == "p") {
      y_label = "Total Pretrial Prisoners"
      
      pop_df <- pop_df %>%
        filter(pop_type == "Pretrial")
      
    } else if (pop_to_plot_pop() == "s") {
      y_label = "Total Sentenced Prisoners"
      
      pop_df <- pop_df %>%
        filter(pop_type == "Sentenced")
      
    } else if (pop_to_plot_pop() == "o") {
      y_label = "Total Other Prisoners"
      
      pop_df <- pop_df %>%
        filter(pop_type == "Other")
    }
    
    # Determine if it's trying to show breakdowns for DOC facilities (no such data)
    only_doc_facs <- data.frame(locs = cnty_to_plot_pop()) %>%
      filter(locs != "--",
             !str_detect(locs, "DOC:")) %>%
      nrow() == 0
    
    no_pop <- (pop_to_plot_pop() != "pso") & only_doc_facs
    
    # OR if there simply isn't data for the given thing
    no_data <- pop_df %>%
      filter(County %in% cnty_to_plot_pop()) %>%
      filter(value != 0) %>%
      nrow() == 0
    
    if (no_pop | no_data) {
      
      g <- data.frame(Date="", active="", County="") %>%
        ggplot(aes(x=Date, y = active, color=County)) +
        annotate("text", x=.5, y=.5, 
                 label=ifelse(no_pop, "Individual DOC facilities only report total population.",
                              'No "other" population at selected location(s)'),
                 fontface="italic") +
        labs(x = "", y = y_label, color="",
             title="placeholder") +
        theme(plot.title= element_text(family="gtam", face='bold'),
              text = element_text(family="gtam", size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              axis.text = element_blank()) +
        coord_cartesian(clip = 'off')
      
      lines_plotly_style(g, y_label, "County", 
                         show_weekly=F, pop=T)
    } else {
      
      g <- pop_df %>%
        mutate(value = na_if(value, 0)) %>%
        filter(Date >= ymd(20200405),
               County %in% cnty_to_plot_pop(),
               !is.na(value)) %>%
        rename(Location = County) %>%
      ggplot(aes(x=Date, y = value, color=Location,
                 text = paste0("Date: ", Date, "\n",
                               y_label, ": ", number(value, big.mark=","), "\n",
                               "Location: ", Location))) +
        geom_path(size=1.3, show.legend = T, alpha=0.8, group=1) +
        labs(x = "", y = y_label, color="",
             title = paste(y_label, "over Time")) +
        theme(plot.title= element_text(family="gtam", face='bold'),
              text = element_text(family="gtam", size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              legend.position = c(.5, -.22),
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
              legend.key.width = unit(1, "cm"),
              legend.text = element_text(size=16)) +
        scale_x_date(date_labels = "%b %e ", limits=c(ymd(20200405), NA)) +
        scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
        coord_cartesian(clip = 'off') 
      
      lines_plotly_style(g, y_label, "County",
                         subtitle=F, show_weekly = input$checkbox_pop,
                         pop=T)
    }
    
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
      bind_rows(df_by_fac) %>%
      mutate(loc = str_replace(loc, "^DOC$", "DOC Aggregate"))
    
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

  # Parole Releases -----------------------------------------------------------
  
  # Plot DOC Facilities
  output$par_rels_DOC_plot <- renderPlotly({
    
    output$n_par_rels_DOC_str <- renderText({
      parole_df %>%
        filter(County == "DOC") %>%
        pull(value) %>%
        sum() %>%
        number(big.mark=",")
    })
    
    parole_df %>%
      filter(County == "DOC") %>%
      select(Date, Facility = facility_match, value) %>%
      single_bar_plot("Total", 
                      "Total Released on Parole",
                      "Facility")
  })
  
  # Plot Counties
  output$par_rels_counties_plot <- renderPlotly({
    
    output$n_par_rels_str <- renderText({
      parole_df %>%
        pull(value) %>%
        sum() %>%
        number(big.mark=",")
    })
    
    parole_df %>%
      group_by(County, Date) %>%
      summarize(value = sum(value)) %>%
      single_bar_plot("Total", 
                      "Total Released on Parole",
                      "County")
  })
  
  # Plot county facilities
  output$par_rels_countyfacs_plot <- renderPlotly({
    parole_df %>%
      filter(County %in% c("Bristol", "Essex", "Hampden", "Suffolk")) %>%
      select(Date, Facility = facility_match, value) %>%
      single_bar_plot("Total", 
                      "Total Released on Parole",
                      "Facility")
  })
  
  # Parole Releases v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  fac_to_plot_parole <- reactive({
    c(input$select_parole1_pop,
      input$select_parole2_pop,
      input$select_parole3_pop)
  })
  
  # Pull out all parole data
  all_parole_df <- parole_df %>%
    group_by(Date) %>%
    summarize(value = sum(value)) %>%
    arrange(Date) %>%
    mutate(Facility = "All",
           cumul = cumsum(value))
  
  all_counties_parole_df <- parole_df %>%
    filter(County != "DOC") %>%
    group_by(Date) %>%
    summarize(value = sum(value)) %>%
    arrange(Date) %>%
    mutate(Facility = "All Counties",
           cumul = cumsum(value))
  
  counties_parole_df <- parole_df %>%
    group_by(County, Date) %>%
    summarize(value = sum(value)) %>%
    arrange(Date) %>%
    mutate(Facility = County,
           Facility = ifelse(Facility == "DOC", "DOC Aggregate", Facility),
           cumul = cumsum(value)) %>%
    ungroup() %>%
    select(-County)
  
  parole_v_time_df <- parole_df %>%
    filter(County %in% c("DOC", "Bristol", "Essex", "Hampden", "Suffolk")) %>%
    mutate(Facility = ifelse(County == "DOC", paste0("DOC: ", facility_match), as.character(facility_match))) %>%
    select(-County, -date, -facility_match) %>%
    group_by(Facility) %>%
    arrange(Date) %>%
    mutate(cumul = cumsum(value)) %>%
    bind_rows(all_parole_df) %>%
    bind_rows(all_counties_parole_df) %>%
    bind_rows(counties_parole_df)
  
  # Plot
  output$par_rels_v_time_plot <- renderPlotly({
    
    y_label <- "Total Released on Parole"
    
    # Determine if there simply isn't data for the given thing
    no_data <- parole_v_time_df %>%
      filter(Facility %in% fac_to_plot_parole()) %>%
      filter(value != 0) %>%
      nrow() == 0
    
    if (no_data) {
      
      g <- data.frame(Date="", active="", County="") %>%
        ggplot(aes(x=Date, y = active, color=County)) +
        annotate("text", x=.5, y=.5, 
                 label='No parole releases at selected location(s)',
                 fontface="italic") +
        labs(x = "", y = y_label, color="",
             title="placeholder") +
        theme(plot.title= element_text(family="gtam", face='bold'),
              text = element_text(family="gtam", size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              axis.text = element_blank()) +
        coord_cartesian(clip = 'off')
      
      lines_plotly_style(g, y_label, "County", 
                         show_weekly=F, pop=T)
    } else {
      
      g <- parole_v_time_df %>%
        filter(Date >= ymd(20200405),
               Facility %in% fac_to_plot_parole()) %>%
        rename(Location = Facility) %>%
        ggplot(aes(x=Date, y = cumul, color=Location,
                   text = paste0("Date: ", Date, "\n",
                                 y_label, ": ", number(cumul, big.mark=","), "\n",
                                 "Location: ", Location))) +
        geom_path(size=1.3, show.legend = T, alpha=0.8, group=1) +
        labs(x = "", y = y_label, color="",
             title = paste(y_label, "over Time")) +
        theme(plot.title= element_text(family="gtam", face='bold'),
              text = element_text(family="gtam", size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              legend.position = c(.5, -.22),
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
              legend.key.width = unit(1, "cm"),
              legend.text = element_text(size=16)) +
        scale_x_date(date_labels = "%b %e ", limits=c(ymd(20200405), NA)) +
        scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
        coord_cartesian(clip = 'off') 
      
      lines_plotly_style(g, y_label, "County",
                         subtitle=T, show_weekly = F,
                         pop=T)
    }
      
    })

  # Total Vaccinations -------------------------------------------------------
  vax_num_df <- vax_df %>%
    select(-contains("Total"), -Notes) %>%
    pivot_longer(cols=starts_with("N"), names_to = c("dose", "pop"), 
                 names_pattern="N (.*) - (.*)") %>%
    mutate(dose_pop = paste0(dose, " - ", pop),
           County = factor(County,
                           levels = c("DOC", "Barnstable", "Berkshire", "Bristol", "Dukes", "Essex*", 
                                      "Franklin", "Hampden", "Hampshire", "Middlesex*", "Norfolk*", 
                                      "Plymouth", "Suffolk", "Worcester")))
  
  n_vax <- vax_num_df %>%
    pull(value) %>%
    sum(na.rm=T)
  
  # Determine which variable to plot
  select_vax <- reactive({ input$select_vax })
  
  output$all_vax_plot <- renderPlotly({
    
    if (input$checkbox_hideDOC_vax) {
      vax_num_df <- vax_num_df %>%
        filter(County != "DOC")
    }
    
    if (select_vax() %in% c("Prisoners", "Staff")) {
      
      output$n_vax_str <- renderText({
        vax_num_df %>%
          filter(str_detect(dose_pop, select_vax())) %>%
          pull(value) %>%
          sum(na.rm=T) %>%
          format(big.mark=",")
      })
      output$type_vax <- renderText({
        ifelse(input$checkbox_hideDOC_vax, 
               paste("county", select_vax() %>% tolower()),
               select_vax() %>% tolower())
      })
      
      vax_num_df %>%
        filter(str_detect(dose_pop, paste0(select_vax(), "|Unspecified"))) %>%
        rename(type = dose) %>%
        stacked_bar_plot(paste(select_vax(), "Vaccines"),
                         "County", vax=T)
      
    } else if (select_vax() == "Total") {
      
      output$n_vax_str <- renderText({format(n_vax, big.mark=",")})
      output$type_vax <- renderText({
        ifelse(input$checkbox_hideDOC_vax, 
               "county prisoners and staff",
               "prisoners and staff")
        })
      
      vax_num_df %>%
        rename(type = dose) %>%
        stacked_bar_plot("Vaccines",
                         "County", vax=T)
    }
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
  
  # Determine which variable to plot
  select_active <- reactive({ input$select_active })
  
  active_doc_df_to_add <- sjc_DOC_num_df %>%
    group_by(fac) %>%
    filter(Date == max(Date)) %>%
    mutate(value = as.numeric(`Active Prisoner Cases`),
           type = "Prisoner",
           County = factor("DOC", levels=counties),
           value = replace_na(value, 0))
  
  active_df <- sjc_num_df %>%
    filter_at(vars(-County, -Date), any_vars(. != 0)) %>%
    filter(County != "DOC") %>%
    group_by(County) %>%
    filter(Date == max(Date)) %>%
    pivot_longer(cols=matches("Active", ignore.case=F),
                 names_to="type",
                 names_pattern="Active (.*) Cases") %>%
    bind_rows(active_doc_df_to_add)
  
  n_active <- sum(active_df$value)
  
  output$last_date_str_DOC <- renderText({
    strftime(last_date_entered_DOC, format="%B %e, %Y"
    )})
  output$last_date_str_counties <- renderText({
    strftime(last_date_entered_counties, format="%B %e, %Y"
    )})
  
  output$all_active_plot <- renderPlotly({
    
    if (select_active() == "All") {
      output$n_active_str <- renderText({format(n_active, big.mark=",")})
      output$type_active <- renderText({"prisoners and staff"})
      
      stacked_bar_plot(active_df, 
                       "Active Cases",
                       "County")
      
    } else if (select_active() %in% c("Prisoner", "Staff")) {
      output$n_active_str <- renderText({
        active_df %>%
          filter(type == select_active()) %>%
          pull(value) %>%
          sum() %>%
          format(big.mark=",")
      })
      output$type_active <- renderText({tolower(select_active())})
      
      single_bar_plot(active_df, 
                      select_active(), 
                      paste("Active", select_active(), "Cases"),
                      "County")
      
    } else if (select_active() == "Total") {
      output$n_active_str <- renderText({format(n_active, big.mark=",")})
      output$type_active <- renderText({"prisoners and staff"})
      
      single_bar_plot(active_df, 
                      select_active(), "Prisoners & Staff Tested Positive",
                      "County")
    }
  
  })
  
  # Active Positives v. Time -------------------------------------------------------
  
  # Determine which counties to plot
  cnty_to_plot_active <- reactive({
    c(input$select_active1,
      input$select_active2,
      input$select_active3)
  })
  
  # Determine which population to plot
  pop_to_plot_active <- reactive({input$active_radio})
  
  active_DOC_to_add <- sjc_DOC_num_df %>%
    filter(Date >= ymd(20200707)) %>%
    mutate(active = `Active Prisoner Cases`,
           County = factor("DOC", levels=counties),
           type = "Prisoner") %>%
    filter(!is.na(active)) %>%
    dplyr::select(Date, County, active, type)
  
  active_v_time <- sjc_num_df %>%
    filter(Date >= ymd(20200707),
           County != "DOC") %>%
    pivot_longer(cols=matches("Active", ignore.case=F),
                 names_to="type", values_to="active",
                 names_pattern="Active (.*) Cases") %>%
    # mutate(active = `Active Prisoner Cases`) %>%
    filter(!is.na(active)) %>%
    dplyr::select(Date, County, active, type) %>%
    bind_rows(active_DOC_to_add) %>%
    group_by(Date, County, type) %>%
    summarize(active=sum(active))
  
  all_active_v_time <- active_v_time %>%
    ungroup() %>%
    mutate(Date = if_else(County == "DOC", Date + days(1), Date)) %>%
    complete(Date = seq.Date(ymd(20201109), max(Date), by="day"), County, type) %>%
    group_by(County, type) %>%
    tidyr::fill(active) %>%
    filter(!is.na(active)) %>%
    ungroup() %>%
    mutate(County = "All") %>%
    group_by(Date, County, type) %>%
    summarize(active = sum(active))
  
  all_cty_active_v_time <- active_v_time %>%
    ungroup() %>%
    filter(County != "DOC") %>%
    mutate(County = "All Counties") %>%
    group_by(Date, County, type) %>%
    summarize(active = sum(active))
  
  active_v_time <- active_v_time %>%
    bind_rows(all_active_v_time) %>%
    bind_rows(all_cty_active_v_time)
  
  # Plot
  output$active_v_time_plot <- renderPlotly({
    
    if (pop_to_plot_active() == "p") {
      a <- active_v_time %>%
        filter(type == "Prisoner")
      
      y_label <- "Active Prisoner Cases"
    } else if (pop_to_plot_active() == "s") {
      a <- active_v_time %>%
        filter(type == "Staff")
      
      y_label <- "Active Staff Cases"
    } else {
      a <- active_v_time %>%
        group_by(Date, County) %>%
        summarize(active = sum(active))
      
      y_label <- "Active Prisoner & Staff Cases"
    }
    
    # Determine if it's trying to show only DOC staff (no such data)
    only_doc_staff <- pop_to_plot_active() == "s" & 
      identical(setdiff(cnty_to_plot_active(), c("DOC", "--", "--")), character(0))
    
    if (only_doc_staff) {
      
      g <- data.frame(Date="", active="", County="") %>%
      ggplot(aes(x=Date, y = active, color=County)) +
        annotate("text", x=.5, y=.5, label="No Active DOC staff data.",
                 fontface="italic") +
        labs(x = "", y = y_label, color="",
             title="placeholder") +
        theme(plot.title= element_text(family="gtam", face='bold'),
              text = element_text(family="gtam", size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              axis.text = element_blank()) +
        coord_cartesian(clip = 'off')
      
      lines_plotly_style(g, y_label, "County", 
                         show_weekly=F)
      
    } else {
    
      g <- a %>%
        filter(County %in% cnty_to_plot_active()) %>%
      ggplot(aes(x=Date, y = active, color=County)) +
        geom_path(size=1.3, show.legend = T, alpha=0.8) +
        geom_point(size=1.5) +
        labs(x = "", y = y_label, color="",
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
      
      lines_plotly_style(g, y_label, "County", 
                         show_weekly=F, subtitle=F)
    }
    
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
  
  # Include death date & locations
  death_dates_df <- sjc_num_df %>%
    filter(`N Deaths` > 0,
           Date > ymd(20200708)) %>%
    select(Date, County, `N Deaths`) %>%
    mutate(DOC=F) %>%
    rename(loc=County) %>%
    rbind(
      sjc_DOC_num_df %>% 
        filter(`N Deaths` > 0,
               Date > ymd(20200708)) %>%
        select(Date, fac, `N Deaths`) %>%
        mutate(DOC=T) %>%
        rename(loc=fac)
    ) %>%
    arrange(desc(Date))
  
  output$deaths_list <- renderUI({
    get_death_li <- function(row) {
      DOC <- row["DOC"]
      date <- row["Date"] %>%
        strftime(format="%B %e")
      
      if (DOC) {
        paste0(date, ": [DOC] ", row["loc"], " - ", row["N Deaths"], " death") %>%
          tags$li() %>%
          return()
      } else {
        paste0(date, ": ", row["loc"], " - ", row["N Deaths"], " death") %>%
          tags$li() %>%
          return()
      }
    }
    
    apply(death_dates_df, MARGIN = 1, get_death_li) %>%
      tagList()
    
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
           `N Tested - Staff`= `N Tested - COs`,
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
    # Rename prisoner column to work with bar_plot() functions
    rename(`N Positive - Prisoners`=`N Positive - Detainees/Inmates`,
           Facility = fac) %>%
    # Sum together various staff
    rowwise() %>%
    mutate(`N Positive - Staff` = sum(`N Positive - COs`, 
                                      `N Positive - Other Staff`, na.rm=T)) %>%
    ungroup() %>%
    dplyr::select(-`N Positive - COs`, -`N Positive - Other Staff`) %>%
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
      strftime(max(sjc_DOC_num_df$Date), format="%B %e, %Y"
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
  
  # Include death dates & locations
  output$deaths_list_DOC <- renderUI({
    
    get_death_li_DOC <- function(row) {
      date <- row["Date"] %>%
        strftime(format="%B %e")

      paste0(date, ": ", row["loc"], " - ", row["N Deaths"], " death") %>%
        tags$li() %>%
        return()
    }
    
    apply(death_dates_df %>% filter(DOC==T), MARGIN = 1, get_death_li_DOC) %>%
      tagList()
    
  })
  
  # DOC: Hospitalizations -------------------------------------------------------
  
  hosps_df <- sjc_df %>%
    mutate(Date = as.Date(Date, origin=lubridate::origin)) %>%
    mutate(value = as.numeric(`Active Hospitalizations`)) %>%
    filter(County == "DOC",
           !is.na(value)) %>%
    dplyr::select(Date, value)
  
  # Calculate totals
  output$n_hosps_str <- renderText({
    hosps_df %>%
      filter(Date == max(Date)) %>%
      pull(value) %>%
      format(big.mark=",")
    })
  
  # Plot hospitalizations v. time
  output$hosp_plot <- renderPlotly({
  
    y_label <- "Active DOC Hospitalizations"

    g <- hosps_df %>%
      ggplot(aes(x=Date, y = value)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      geom_point(size=1.5) +
      labs(x = "", y = y_label, color="",
           title="placeholder") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "County", 
                       show_weekly=F, subtitle=F)
  })
  
  # DOC: Medical Parole -------------------------------------------------------
  
  med_par_df <- sjc_df %>%
    mutate(Date = as.Date(Date, origin=lubridate::origin)) %>%
    mutate(value = as.numeric(`N Approved Medical Parole`)) %>%
    filter(County == "DOC",
           !is.na(value)) %>%
    mutate(cumul = cumsum(value)) %>%
    dplyr::select(Date, cumul, value)
  
  # Calculate totals
  n_med_par <- sum(med_par_df$value)
  output$n_med_par_str <- renderText({format(n_med_par, big.mark=",")})
  
  # Plot hospitalizations v. time
  output$med_par_plot <- renderPlotly({
  
    y_label <- "Medical Parole Approvals"

    g <- med_par_df %>%
      ggplot(aes(x=Date, y = cumul)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      labs(x = "", y = y_label, color="",
           title="placeholder") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = 16)) +
      scale_x_date(date_labels = "%b %e ") +
      scale_color_manual(values=c("black", "#0055aa", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, y_label, "County", 
                       show_weekly=F, subtitle=T)
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
  
  # Determine which variable to plot
  select_active_cty <- reactive({ input$select_active_cty })
  
  active_cty_df <- sjc_county_num_df %>%
    group_by(fac) %>%
    filter(Date == max(Date)) %>%
    pivot_longer(cols=matches("Active", ignore.case=F),
                 names_to="type",
                 names_pattern="Active (.*) Cases") %>%
    rename(Facility = fac) %>%
    filter(!is.na(value))
  
  output$last_date_str2_DOC <- renderText({
    strftime(last_date_entered_DOC, format="%B %e, %Y"
    )})
  output$last_date_str2_counties <- renderText({
    strftime(last_date_entered_counties, format="%B %e, %Y"
    )})

  output$all_active_cty_plot <- renderPlotly({
    
    if (select_active_cty() == "All") {
      
      stacked_bar_plot(active_cty_df, 
                       "Active Cases",
                       "County Facility")
      
    } else if (select_active_cty() %in% c("Prisoner", "Staff")) {
      
      single_bar_plot(active_cty_df, 
                      select_active_cty(), 
                      paste("Active", select_active_cty(), "Cases"),
                      "County Facility")
      
    } else if (select_active_cty() == "Total") {
      
      single_bar_plot(active_cty_df, 
                      select_active_cty(), "Prisoners & Staff Tested Positive",
                      "County Facility")
    }

  })
  
  # Counties: Active Positives v. Time -------------------------------------------------------
  
  # Determine which population to plot
  pop_to_plot_cty_active <- reactive({input$cty_active_radio})
  
  # Determine which counties to plot
  cty_fac_to_plot_active <- reactive({
    c(input$select_active_cty1,
      input$select_active_cty2,
      input$select_active_cty3)
  })
  
  active_cty_fac_v_time <- sjc_county_num_df %>%
    filter(Date >= ymd(20200708)) %>%
    mutate(fac = as.character(fac)) %>%
    pivot_longer(cols=matches("Active", ignore.case=F),
                 names_to="type", values_to = "active",
                 names_pattern="Active (.*) Cases") %>%
    dplyr::select(Date, fac, active, type) %>%
    bind_rows(active_v_time %>% rename(fac=County)) %>%
    filter(fac %in% ctyfac_choices,
    !is.na(active))
  
  # Plot
  output$active_cty_fac_v_time_plot <- renderPlotly({
    
    if (pop_to_plot_cty_active() == "p") {
      a <- active_cty_fac_v_time %>%
        filter(type == "Prisoner")
      
      y_label <- "Active Prisoner Cases"
    } else if (pop_to_plot_cty_active() == "s") {
      a <- active_cty_fac_v_time %>%
        filter(type == "Staff")
      
      y_label <- "Active Staff Cases"
    } else {
      a <- active_cty_fac_v_time %>%
        group_by(Date, fac) %>%
        summarize(active = sum(active))
      
      y_label <- "Active Prisoner & Staff Cases"
    }
    
    g <- a %>%
      filter(fac %in% cty_fac_to_plot_active()) %>%
    ggplot(aes(x=Date, y = active, color=fac)) +
      geom_path(size=1.3, show.legend = T, alpha=0.8) +
      geom_point(size=1.5) +
      labs(x = "", y = y_label, color="",
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
    
    lines_plotly_style(g, y_label, "County Facility", 
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
  output$df_table <- DT::renderDataTable(
    {sjc_df},
    options = list(scrollX = TRUE), 
    filter = 'top'
    )
  
  output$DOC_df_table <- DT::renderDataTable(
    {sjc_DOC_df},
    options = list(scrollX = TRUE), 
    filter = 'top'
  )
  
  output$cty_df_table <- DT::renderDataTable(
    {sjc_county_df},
    options = list(scrollX = TRUE), 
    filter = 'top'
  )
  
  output$parole_df_table <- DT::renderDataTable(
    {read_excel(tf, sheet=4) %>%
        mutate(Date = as.Date(`Date (Friday)`),
               `N Released Parole` = as.numeric(`N Released Parole`)) %>%
        merge(facs_df, by.x=c("Facility", "County"), 
              by.y=c("facility_raw", "County"), all.x=T) %>%
        select(-`Date (Friday)`, -Facility) %>%
        select(Date, County, Facility=facility_match, `N Released Parole`) %>%
        arrange(Date, County, Facility)},
    options = list(scrollX = TRUE), 
    filter = 'top'
  )

  output$vax_df_table <- DT::renderDataTable(
    {vax_df},
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
