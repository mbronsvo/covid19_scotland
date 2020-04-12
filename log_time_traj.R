#library(tidyverse)
#library(gganimate)


#' Plot a trajectory plot
#' 
#' 
#' @param df Dataframe to plot
#' @param index Unquoted column containing index e.g. days
#' @param outcome Unquoted column for y axis e.g. cases
#' @param group Unquoted column for groups e.g. country
#' @param flag Unquoted column of type logical to mark points
#' @param periods Numeric vector of doubling periods to plot
#' @param fill_colour Fill colour for labels
#' @param breaks Breaks for y-axis
#' 
#' @return A ggplot which can be further customised
#' 
#' @example 
#' cov_trajplot(df = ts, index = days, outcome = cases, group = health_board)
ref <- 10

cov_trajplot <- function(df,
                     index = days,
                     outcome = cases,
                     group,
                     flag,
                     periods = c(2, 3, 7),
                     fill_colour = "#DFDCD3",
                     breaks = sort(outer(c(1,2,5), c(1, 10, 100, 1000, 10000, 100000)))){

  df <- filter(df, !is.na({{outcome}}))
  
  # make doublings df
  doublings <- tibble(doubling = periods) %>% 
    crossing(tibble({{index}} := seq(0, pull(df, {{index}}) %>%
                                       max(na.rm = TRUE)))) %>% 
    mutate({{outcome}} := ref * exp(log(2) / doubling) ^ {{index}}) %>% 
    filter({{outcome}} < max(pull(df, {{outcome}}), na.rm = TRUE)) %>% 
    group_by(doubling) %>% 
    mutate(label = if_else({{index}} == max({{index}}),
                            str_glue("doubling in {doubling} days"),
                            NA_character_)) %>% 
    mutate({{group}} := NA)
  
  df %>% 
    group_by({{group}}) %>% 
    mutate(label = if_else({{index}} == max({{index}}),
                           as.character({{group}}),
                           NA_character_)) %>%
    ggplot() +
    aes(x = {{index}}, {{outcome}},
        colour = {{group}}) +
    geom_line(lwd = 0.75, alpha = 0.75) +
    geom_point(size = 1.5, shape = 1) +
    geom_point(aes(shape = {{flag}},
                   size = {{flag}})) +
    geom_label_repel(aes(label = label),
                     na.rm = TRUE,
                     fill = fill_colour) +
    scale_size_manual(values = c(1, 2.5)) +
    scale_shape_manual(values = c(1, 5)) +
    scale_x_continuous(breaks = function(x) seq(0, round(max(x)), 5)) +
    scale_y_continuous(trans = "log10",
                       breaks = breaks,
                       labels = scales::comma_format()) +
    scale_color_discrete(l = 40) +
    geom_line(data = doublings,
              aes(x = {{index}},
                  y = {{outcome}},
                  group = doubling),
              lty = 3) +
    geom_text_repel(data = doublings,
                    colour = "grey30",
                    aes(label = label),
                    nudge_x = -1,
                    na.rm = TRUE) +
    theme_bw(base_size = 18) +
    theme(legend.position = "none")
}


#' Index data from groups to first n events per group
#' 
#' @param df Source dataframe
#' @param index Unquoted column containing index e.g. days
#' @param var Unquoted column for y axis e.g. cases
#' @param ref The cutpoint for outcome

cov_offset <- function(df,
                       index = date,
                       var = cases,
                       to = days,
                       ref){
  df <- df %>% 
    arrange({{index}})
  
  day_zero <- df %>% 
    arrange({{index}}) %>% 
    filter({{var}} >= ref) %>% 
    pull({{index}}) %>% 
    min()
  
  df %>% 
    mutate({{to}} := {{index}} - day_zero) %>% 
    filter({{to}} >= 0) %>% 
    mutate({{to}} := as.numeric({{to}}))
}


#' #' Plot a log log change vs case/deaths plot to track progress
#' #' 
#' #'  As per link from Thibaud
#' #'  
#' #'   @param df 
#' 
#' cov_progress <- function(df,
#'                          index = days,
#'                          outcome = cases,
#'                          group,
#'                          smooth_by = 4,
#'                          fill_colour = "white"){
#'   if(smooth_by < 2) stop("Smoothing period must be 2 or greater")
#'   
#'   df <- df %>% 
#'     filter(!is.na({{outcome}})) %>%   
#'     group_by({{group}}, round({{index}} / smooth_by)) %>%
#'     summarise(outcome = min({{outcome}}),
#'               change = max({{outcome}}) - min({{outcome}}),
#'               change = change / (max({{index}}) - min({{index}})),
#'               index = max({{index}})) %>% 
#'     ungroup() %>% 
#'     filter(!is.na(change))   
#' 
#' 
#'   df %>% 
#'   group_by({{group}}) %>% 
#'     ggplot() +
#'     aes(x = outcome, y = change, colour = {{group}}) +
#'     geom_line() +
#'     geom_point() +
#'     scale_x_continuous(trans = "log10") +
#'     scale_y_continuous(trans = "log10") +
#'     geom_label_repel(aes(label = {{group}}),
#'                      na.rm = TRUE,
#'                      fill = fill_colour,
#'                      segment.color = NA) +
#'     theme(legend.position = "none") +
#'     labs(y = str_glue("mean change per day over {smooth_by} days"),
#'          x = as_label(enquo(outcome))) +
#'     transition_reveal(index)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
