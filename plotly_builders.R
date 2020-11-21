#!/usr/local/bin/Rscript
# R module to build interactive plots with ggplotly
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: April 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load ggplot-friendly font using show_text
font_add("gtam", "GT-America-Standard-Regular.ttf",
         bold = "GT-America-Standard-Bold.ttf")
showtext_auto()

# Define ggplotly settings
label_lightback <- list(
  bordercolor = "white",
  font = list(
    family = "gtam",
    size = 15,
    color="black"
  )
)
label_darkback <- list(
  bordercolor = "white",
  font = list(
    family = "gtam",
    size = 15,
    color="white"
  )
)
modeBarButtonsToRemove <- c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", 
                            "select2d", "lasso2d", "autoScale2d",
                            "hoverClosestCartesian",
                            "hoverCompareCartesian", "toggleSpikelines")
modeBarButtonsToRemove_time <- c("select2d","zoom2d","lasso2d", 
                                 "hoverClosestCartesian","autoScale2d",
                            "hoverCompareCartesian", "toggleSpikelines")
legend_layout_top <- list(orientation = "h", 
                          x = 0.5, y=1.2,
                          xanchor="center",
                          bgcolor = alpha('lightgray', 0.4))
legend_layout_bottom <- list(orientation = "h", 
                          x = 0.5, y=-.2,
                          xanchor="center",
                          bgcolor = alpha('lightgray', 0.4))

# Define x axes
counties <- c("DOC", "Barnstable", "Berkshire", "Bristol", "Dukes", "Essex", 
              "Franklin", "Hampden", "Hampshire", "Middlesex", "Norfolk", 
              "Plymouth", "Suffolk", "Worcester")
fac_staff <- c('Boston Pre', 'BSH', 
               'LSH', 'MASAC', 'MCI-C', 'MCI-CJ', 'MCI-F', 'MCI-Norfolk', 
               'MCI-Shirley', 'MTC', 'NCCI-Gardn', 'OCCC', 'Pondville', 
               'SBCC', 'SMCC', "Non-Facility")
cty_facs <- c('Bristol - Ash Street Jail', 'Bristol - DHOC', 'Essex - Middleton', 
              'Essex - Prerelease', 'Essex - Women in Transition', 'Hampden - Ludlow', 
              'Hampden - Mill Street', "Hampden - Women's Facility", 'Suffolk - HOC', 
              'Suffolk - Jail')

# Function for plots with one kind of bar
single_bar_plot <- function(data, filter_value, y_label, location_to_plot) {

  if (filter_value != "Total") {
    data <- data %>%
        filter(type == filter_value) 
  }

  if (location_to_plot == "County") {
    label_source = counties
  } else if (location_to_plot == "Facility") {
    label_source = data$Facility %>% unique() %>% sort()
  } else if (location_to_plot == "County Facility") {
    label_source <- cty_facs
    location_to_plot <- "Facility"
  }

  data <- data %>%
    rename(loc = location_to_plot)
  
  all_zeros <- data %>%
    group_by(loc) %>%
    summarize(sum_value = sum(value)) %>%
    pull(sum_value) %>% 
    unique() %>%
      all.equal(0)== T
  
  if (all_zeros) {
    g <- data.frame(loc = label_source, sum_value = 0) %>%
    ggplot(aes(x=loc, 
               y=sum_value, 
               label = 0,
               text = paste0(location_to_plot, ": ", loc, "\n",
                             y_label, ": 0"))) +
      geom_col(show.legend=F) +
      geom_text(label=0) + 
      ylim(0, 10) +
      labs(y = y_label, x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            axis.text.y = element_blank(),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14))
  
    traces_to_hide <- 0
    traces_lightback <- 0
    traces_darkback <- 0
    
  } else {
    label_threshold <- data %>%
      group_by(loc) %>%
      summarize(sum_value = sum(value)) %>%
      pull(sum_value) %>%
      max() * .07
    
    g <- data%>%
      group_by(loc) %>%
      summarize(sum_value = sum(value)) %>%
      ungroup() %>%
      mutate(label_vjust = ifelse(sum_value < label_threshold | sum_value > 1000, 
                                  sum_value + max(sum_value) * .025, 
                                  sum_value - max(sum_value) * .0375),
             label_color = ifelse(sum_value < label_threshold | sum_value > 1000, 
                                  "black", "white")) %>%
    ggplot(aes(x=loc,
               y=sum_value,
               fill = as.factor(1),
               text = paste0(location_to_plot, ": ", loc, "\n",
                             y_label, ": ", number(sum_value, big.mark=",")))) +
      geom_col(position = "stack", show.legend = F) +
      geom_text(aes(label=number(sum_value, big.mark=","), 
                    color=label_color, y = label_vjust),
                family="gtam") +
      theme(legend.position = "none") +
      scale_color_manual(values = c("black", "white")) + 
      labs(y = y_label, x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14)) +
      scale_fill_manual(values = c("#0055aa", "#fbb416", "#a3dbe3")) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
                         limits= c(0, NA))
    
    traces_lightback <- 0
    traces_darkback <- 1
    
    p_json <- ggplotly(g) %>%
      plotly_json()
    n_traces <- fromJSON(p_json$x$data)$data$type %>%
      length()
    
    if (n_traces == 2) {
      traces_to_hide <- 2
    } else {
      traces_to_hide <- 2:3
    }
  }
  
  g <- ggplotly(g, tooltip=c("text"),
                dynamicTicks = T) %>%
    config(modeBarButtonsToRemove = modeBarButtonsToRemove) %>%
    style(hoverinfo = "none", traces = traces_to_hide)
  
  if (!all_zeros) {
    g %>%
      style(hoverlabel = label_lightback, traces = traces_lightback) %>%
      style(hoverlabel = label_darkback, traces = traces_darkback)
  }
}

# Function for plots with multiple bars stacked
stacked_bar_plot <- function(data, y_label, location_to_plot) {
  
  doc_releases <- y_label == "Prisoners Released" & location_to_plot == "County"
  doc_positives <- y_label == "Tested Positive" & location_to_plot == "Facility"

  if (location_to_plot == "County") {
    label_source <- counties
  } else if (location_to_plot == "Facility") {
    label_source <- fac_staff
  } else if (location_to_plot == "County Facility") {
    label_source <- cty_facs
    location_to_plot <- "Facility"
  }
  
  if (doc_releases) {
    traces_to_hide <- 0
    traces_lightback <- 2:3
    traces_darkback <- 1
    
    data <- data %>%
      mutate(type=factor(type, levels=c("Pre-Trial", "Sentenced", "Home Confinements")))
    
  } else if (doc_positives) {
    traces_to_hide <- 0
    traces_lightback <- 2:3
    traces_darkback <- 1
    
    data <- data %>%
      mutate(type=factor(type, levels=c("Prisoners", "COs", "Other Staff")))
    
  } else {
    
    traces_to_hide <- 0
    traces_lightback <- 2
    traces_darkback <- 1
    
  }

  g <- data%>%
      rename(loc = location_to_plot) %>%
      group_by(loc, type) %>%
      summarize(sum_value = sum(value)) %>%
    ggplot(aes(x=loc,
               y=sum_value,
               fill = type,
               text = paste0(location_to_plot, ": ", loc, "\n",
                            paste(type, y_label), ": ", number(sum_value, big.mark=",")))) +
      geom_col(position = "stack") + 
      labs(y = y_label, x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14)) +
      scale_fill_manual(values = c("#0055aa", "#fbb416", "#a3dbe3")) +
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
                         limits= c(0, NA))
    
    g <- ggplotly(g, tooltip=c("text"),
                  dynamicTicks = T) %>%
      config(modeBarButtonsToRemove = modeBarButtonsToRemove) %>%
      style(hoverinfo = "none", traces = traces_to_hide) %>%
      layout(legend = legend_layout_top)  
    
    g %>%
      style(hoverlabel = label_lightback, traces = traces_lightback) %>%
      style(hoverlabel = label_darkback, traces = traces_darkback)
}

# Convert lines to ggplotly
lines_plotly_style <- function(gg_plot, y_label, location_to_plot, 
  annotation=FALSE, subtitle=TRUE, pos_and_test=FALSE, active_and_recent=FALSE,
  show_weekly=TRUE) {
  
  if (show_weekly) {
    gg_plot <- gg_plot +
      geom_vline(xintercept=as.numeric(ymd(20200708)), 
                 color="darkgrey", alpha=.6, size=1.1, linetype="dashed")
  }

  # Generate bold title from y-axis label
  title_html <- paste0("<b>", y_label, "</b>")

  # If a subtitle is provided, append HTML to title
  if (subtitle) {
    title_html <- paste0(title_html,
                        '<br>',
                        '<sup>',
                        "Cumulative pursuant to SJC 12926",
                        '</sup>')
  }
  
  # Fix which variables are shown in the tooltip
  if (pos_and_test | active_and_recent) {
    g <- ggplotly(gg_plot, tooltip = c("x", "y"),
                  dynamicTicks = T)
  } else {
    g <- ggplotly(gg_plot,
                  dynamicTicks = T)
  }

  # Remove control buttons, set legend layout and title text
  g <- g %>%
      config(modeBarButtonsToRemove = modeBarButtonsToRemove_time) %>%
      layout(legend = legend_layout_bottom) %>%
      layout(title = list(text = title_html, 
                          font=list(family = "gtam")))
  
  # Add annotation for weekly reporting
  if (show_weekly) {
    g <- g %>%
      layout(annotations = list(x = as.numeric(ymd(20200708)), 
                                y = 1, text = "Weekly\nReporting\nBegins", 
                                showarrow = F, xref='x', yref='paper', 
                                xanchor='center', yanchor='bottom', 
                                font=list(size=12, color="darkgrey", family="gtam")))
  }
             
  if (pos_and_test) {
    # Pull out string for what population we're plotting
    pop_to_annotate <- str_split(y_label, " Tested")[[1]][1]
    
    # Replace "value" with population tested or positive
    for (i in 1:length(g$x$data)) {
      text_rep <- g$x$data[[i]]$text %>%
        gsub("value", paste(pop_to_annotate, g$x$data[[i]]$name), .)
      
      g <- g %>%
        style(text = text_rep, traces=i)
    }
    
  } else if (active_and_recent) {
    # Replace "value" with population tested or positive
    for (i in 1:length(g$x$data)) {
      text_rep <- g$x$data[[i]]$text %>%
        gsub("all_tested_rolling14", "Prisoners Tested in Preceding 14 Days", .) %>%
        gsub("all_active", "Prisoners with Active Cases", .)

      g <- g %>%
        style(text = text_rep, traces=i)
    }
  } else {
    # Replace tooltip key with better names
    for (i in 1:length(g$x$data)) {
      
      # If the data is from after 7/14, show date range
      tooltip_text <- g$x$data[[i]]$text
      
      if (!str_detect(y_label, "Active") & !str_detect(y_label, "Population")) {
        for (t in tooltip_text) {
          
          data_date <- t %>%
            str_extract("(?<=Date: ).*?(?=<br \\/>c)") %>%
            ymd()
          
          is_DOC <- location_to_plot == "Facility" | str_detect(t, "DOC|All$")
          
          weekly <- !is.na(data_date) & 
            ((is_DOC & data_date >= ymd(20200707) & data_date < ymd(20201111)) |
            (!is_DOC & data_date >= ymd(20200707)))
          
          if (weekly) {
            date_replace <- paste("Week of", data_date)
          } else {
            date_replace <- data_date
          } 
          
          tooltip_text[tooltip_text == t] <- t %>%
            gsub(data_date, date_replace, .)
          
        }
      }
      
      # Replace tooltip key with better names
      text_rep <- tooltip_text %>%
        gsub("pop", y_label, .) %>%
        gsub("cumul", y_label, .) %>%
        gsub("fac", location_to_plot, .) %>%
        gsub("active", y_label, .)

      g <- g %>%
        style(text = text_rep, traces=i)
    }
  }
    
  if (length(g$x$data) == 1) {
    traces_lightback <- 0
    traces_darkback <- 1
  } else if (length(g$x$data) == 2) {
    traces_lightback <- 0
    traces_darkback <- 1:2
  } else if (length(g$x$data) >= 3) {
    traces_lightback <- 3
    traces_darkback <- 1:2
  }
     
  g %>%
    style(hoverlabel = label_lightback, traces = traces_lightback) %>%
    style(hoverlabel = label_darkback, traces = traces_darkback)

}
