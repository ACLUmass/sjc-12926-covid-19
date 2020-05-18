#!/usr/local/bin/Rscript
# R module to filter data by county/facility, population, & date
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: April 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_df_by_county <- function(sjc_num_df, population) {
  
  if (population == "p") {
    sjc_num_df <- sjc_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - Detainees/Inmates`,
             all_tested = `N Tested - Detainees/Inmates`)
  } else if (population == "s") {
    sjc_num_df <- sjc_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - COs` + `N Positive - Staff` + `N Positive - Contractor`,
             all_tested = `N Tested - COs` + `N Tested - Staff` + `N Tested - Contractors`)
  }
  
  all_df_all <- sjc_num_df %>%
    group_by(Date) %>%
    summarize(all_released = sum(all_released),
              all_positive = sum(all_positive),
              all_tested = sum(all_tested)) %>%
    mutate(County = "All")
  
  all_df_all_counties <- sjc_num_df %>%
    filter(County != "DOC") %>%
    group_by(Date) %>%
    summarize(all_released = sum(all_released),
              all_positive = sum(all_positive),
              all_tested = sum(all_tested)) %>%
    mutate(County = "All Counties")
  
  df_by_county <- sjc_num_df %>%
    dplyr::select(Date, County, all_positive, all_tested, all_released) %>%
    rbind(all_df_all) %>%
    rbind(all_df_all_counties)
  
  return(df_by_county)
}

get_df_by_fac <- function(sjc_num_df, sjc_DOC_num_df, population) {
  
  if (population == "p") {
    sjc_num_df <- sjc_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - Detainees/Inmates`,
             all_tested = `N Tested - Detainees/Inmates`)
    
    sjc_DOC_num_df <- sjc_DOC_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - Detainees/Inmates`,
             all_tested = `N Tested - Detainees/Inmates`) %>%
      filter(!is.na(all_positive))
    
  } else if (population == "s") {
    sjc_num_df <- sjc_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - COs` + `N Positive - Staff` + `N Positive - Contractor`,
             all_tested = `N Tested - COs` + `N Tested - Staff` + `N Tested - Contractors`)
    
    sjc_DOC_num_df <- sjc_DOC_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - Staff`,
             all_tested = `N Tested - Staff`) %>%
      filter(!is.na(all_positive))
  }
  
  DOC_total_df <- sjc_num_df %>%
    filter(County == "DOC") %>%
    rename(fac = County) %>%
    mutate(fac = "DOC Total**") %>%
    dplyr::select(Date, fac, all_positive, all_tested, all_released)
  
  DOC_fac_total_df <- sjc_DOC_num_df %>%
    group_by(Date) %>%
    summarize(all_released = sum(all_released),
              all_positive = sum(all_positive),
              all_tested = sum(all_tested)) %>%
    mutate(fac = "All DOC Facilities")
  
  df_by_fac <- sjc_DOC_num_df %>%
    dplyr::select(Date, fac, all_positive, all_tested, all_released) %>%
    rbind(DOC_total_df) %>%
    rbind(DOC_fac_total_df)
  
  return(df_by_fac)
}
