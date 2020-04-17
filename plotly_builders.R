#!/usr/local/bin/Rscript
# R module to build interactive plots with ggplotly
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: April 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Define ggplotly settings
label_lightback <- list(
  bordercolor = "white",
  font = list(
    family = "GT America",
    size = 15,
    color="black"
  )
)
label_darkback <- list(
  bordercolor = "white",
  font = list(
    family = "GT America",
    size = 15,
    color="white"
  )
)
modeBarButtonsToRemove <- c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", 
                            "select2d", "lasso2d", "autoScale2d",
                            "resetScale2d", "hoverClosestCartesian",
                            "hoverCompareCartesian", "toggleSpikelines")
legend_layout_top <- list(orientation = "h", 
                          x = 0.5, y=1.2,
                          xanchor="center",
                          bgcolor = alpha('lightgray', 0.4))

# Function for plots with one kind of bar
single_bar_plot <- function(data, filter_value, y_label) {
  if (filter_value != "Total") {
    data <- data %>%
        filter(type == filter_value) 
  }

  label_threshold <- data %>%
    group_by(County) %>%
    summarize(sum_value = sum(value)) %>%
    pull(sum_value) %>%
    max() * .07

  g <- data%>%
        group_by(County) %>%
        summarize(sum_value = sum(value)) %>%
        ungroup() %>%
        mutate(label_vjust = ifelse(sum_value < label_threshold, 
                                    sum_value + max(sum_value) * .025, 
                                    sum_value - max(sum_value) * .0375),
               label_color = ifelse(sum_value < label_threshold, "black", "white")) %>%
      ggplot(aes(x=County,
                   y=sum_value,
                   fill = as.factor(1))) +
        geom_col(position = "stack", show.legend = F) +
        geom_text(aes(label=sum_value, color=label_color, y = label_vjust),
                  family="GT America") +
        theme(legend.position = "none") +
        scale_color_manual(values = c("black", "white"))
      
      traces_to_hide <- 2:3
      traces_lightback <- 0
      traces_darkback <- 1

    g <- g + 
      labs(y = y_label, x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14)) +
      scale_fill_manual(values = c("#0055aa", "#fbb416", "#a3dbe3"))
    
    g <- ggplotly(g, tooltip=c("x", "y")) %>%
      config(modeBarButtonsToRemove = modeBarButtonsToRemove) %>%
      style(hoverinfo = "none", traces = traces_to_hide)

    text_x <- paste0("County: ", counties[g$x$data[[1]]$x])
    text_y <- paste0(y_label, ": ", g$x$data[[1]]$y)
    
    g %>%
      style(text = paste0(text_x, "</br></br>", text_y), traces=1) %>%
      style(hoverlabel = label_lightback, traces = traces_lightback) %>%
      style(hoverlabel = label_darkback, traces = traces_darkback)
}

# Function for plots with multiple bars stacked
stacked_bar_plot <- function(data, y_label) {

  g <- data%>%
        group_by(County, type) %>%
        summarize(sum_value = sum(value)) %>%
      ggplot(aes(x=County,
                   y=sum_value,
                   fill = type)) +
        geom_col(position = "stack") 
        labs(fill = "") + 
        theme(legend.position = "top",
              legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA))
      
    traces_to_hide <- 0
    traces_lightback <- 2
    traces_darkback <- 1

    g <- g + 
      labs(y = y_label, x="") +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=14)) +
      scale_fill_manual(values = c("#0055aa", "#fbb416", "#a3dbe3"))
    
    g <- ggplotly(g, tooltip=c("x", "y")) %>%
      config(modeBarButtonsToRemove = modeBarButtonsToRemove) %>%
      style(hoverinfo = "none", traces = traces_to_hide) %>%
      layout(legend = legend_layout_top)  

    text_x1 <- paste0("County: ", counties[g$x$data[[1]]$x])
    text_y1 <- paste0(g$x$data[[1]]$name, " ", y_label, ": ", g$x$data[[1]]$y)
    text_x2 <- paste0("County: ", counties[g$x$data[[2]]$x])
    text_y2 <- paste0(g$x$data[[2]]$name, " ", y_label, ": ", g$x$data[[2]]$y)
    
    g %>%
      style(text = paste0(text_x1, "</br></br>", text_y1), traces=1) %>%
      style(text = paste0(text_x2, "</br></br>", text_y2), traces=2) %>%
      style(hoverlabel = label_lightback, traces = traces_lightback) %>%
      style(hoverlabel = label_darkback, traces = traces_darkback)
}
