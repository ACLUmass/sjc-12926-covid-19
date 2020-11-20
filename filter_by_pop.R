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
              all_tested = sum(all_tested),
              all_active = sum(`Active Prisoner Cases`)) %>%
    mutate(County = "All")
  
  all_df_all_counties <- sjc_num_df %>%
    filter(County != "DOC") %>%
    group_by(Date) %>%
    summarize(all_released = sum(all_released),
              all_positive = sum(all_positive),
              all_tested = sum(all_tested),
              all_active = sum(`Active Prisoner Cases`)) %>%
    mutate(County = "All Counties")
  
  df_by_county <- sjc_num_df %>%
    mutate(all_active = `Active Prisoner Cases`) %>%
    dplyr::select(Date, County, all_positive, all_tested, all_released, all_active) %>%
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
      rowwise() %>%
      mutate(all_positive = sum(`N Positive - COs`, `N Positive - Other Staff`, na.rm=T),
             all_tested = `N Tested - COs`) %>%
      filter(!is.na(all_positive))
  }
  
  DOC_total_df <- sjc_num_df %>%
    filter(County == "DOC") %>%
    rename(fac = County) %>%
    mutate(fac = "DOC Total**",
           all_active = `Active Prisoner Cases`) %>%
    dplyr::select(Date, fac, all_positive, all_tested, all_released, all_active)
  
  DOC_fac_total_df <- sjc_DOC_num_df %>%
    mutate(all_active = `Active Prisoner Cases`,
           all_active = replace_na(all_active, 0)) %>%
    group_by(Date) %>%
    summarize(all_released = sum(all_released),
              all_positive = sum(all_positive),
              all_tested = sum(all_tested),
              all_active = sum(all_active)) %>%
    mutate(fac = "All DOC Facilities")
  
  df_by_fac <- sjc_DOC_num_df %>%
    mutate(all_active = `Active Prisoner Cases`) %>%
    dplyr::select(Date, fac, all_positive, all_tested, all_released, all_active) %>%
    rbind(DOC_total_df) %>%
    rbind(DOC_fac_total_df)
  
  return(df_by_fac)
}

get_df_by_county_fac <- function(sjc_num_df, sjc_county_num_df, population) {
  
  if (population == "p") {
    sjc_num_df <- sjc_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - Detainees/Inmates`,
             all_tested = `N Tested - Detainees/Inmates`)
    
    sjc_county_num_df <- sjc_county_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - Detainees/Inmates`,
             all_tested = `N Tested - Detainees/Inmates`) %>%
      filter(!is.na(all_positive))
    
  } else if (population == "s") {
    sjc_num_df <- sjc_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      mutate(all_positive = `N Positive - COs` + `N Positive - Staff` + `N Positive - Contractor`,
             all_tested = `N Tested - COs` + `N Tested - Staff` + `N Tested - Contractors`)
    
    sjc_county_num_df <- sjc_county_num_df %>%
      dplyr::select(-all_positive, -all_tested) %>%
      group_by(Date, fac) %>%
      mutate(all_positive = sum(`N Positive - COs`, 
                                `N Positive - Staff`, 
                                `N Positive - Contractors`, na.rm=T),
             all_tested = sum(`N Tested - COs`,
                              `N Tested - Staff`, 
                              `N Tested - Contractors`, na.rm = T))
  }
  
  counties_with_breakdowns <- sjc_county_num_df %>%
    separate(fac, c("County", NA), sep=" - ") %>%
    pull(County) %>%
    unique()
  
  all_df_all_counties <- sjc_num_df %>%
    filter(County != "DOC") %>%
    group_by(Date) %>%
    summarize(all_positive = sum(all_positive),
              all_tested = sum(all_tested)) %>%
    mutate(fac = "All Counties")
  
  county_total_df <- sjc_num_df %>%
    filter(County %in% counties_with_breakdowns) %>%
    rename(fac = County) %>%
    dplyr::select(Date, fac, all_positive, all_tested)
  
  df_by_county_fac <- sjc_county_num_df %>%
    dplyr::select(Date, fac, all_positive, all_tested) %>%
    bind_rows(all_df_all_counties) %>%
    bind_rows(county_total_df)
  
  return(df_by_county_fac)
}
