# Load required libraries
library(jsonlite)
library(stringr)

find_items_dfs <- function(d, key_to_match, replacement_value = NULL) {
  search_dfs <- function(x) {
    if (is.list(x) || is.environment(x)) {
      if (key_to_match %in% names(x)) {
        value <- x[[key_to_match]]
        return(if (is.null(value) || identical(value, '')) replacement_value else value)
      }
      for (item in x) {
        result <- search_dfs(item)
        if (!identical(result, replacement_value)) {
          return(result)
        }
      }
    } else if (is.vector(x) && !is.atomic(x)) {
      for (item in x) {
        result <- search_dfs(item)
        if (!identical(result, replacement_value)) {
          return(result)
        }
      }
    }
    return(replacement_value)
  }
  
  tryCatch({
    search_dfs(d)
  }, error = function(e) {
    message("Error: ", e$message)
    return(replacement_value)
  })
}



library(reticulate)
reticulate::source_python("days.py")

parse_audience <- function(out_raw) {
  
  
  summary_dat <- out_raw %>%
    purrr::pluck("ad_library_page_targeting_summary") %>%
    dplyr::bind_rows()
  
  if(nrow(summary_dat) > 1){
    
    summary_dat <- summary_dat %>%
      dplyr::slice(which(summary_dat$detailed_spend$currency == summary_dat$main_currency)) %>%
      dplyr::select(-detailed_spend)
    
  }
  
  targeting_details_raw <- out_raw[!(names(out_raw) %in% c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]
  
  # names(targeting_details_raw)
  
  res <- targeting_details_raw %>%
    purrr::discard(purrr::is_empty) %>%
    purrr::imap_dfr(~{.x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))}) %>%
    dplyr::bind_cols(summary_dat) 
  
  return(res)
  
}

# days30("DE", "248913988520756", output_file =  "output.json")

get_page_insights_py <- function(cntry = "US", id, output_file =  "output.json", timeframe = "LAST_90_DAYS") {
  
  if(timeframe == "LAST_7_DAYS"){
    days7(cntry, id, output_file =  "output.json")
    
  } else if (timeframe == "LAST_30_DAYS"){
    days30(cntry, id, output_file =  "output.json")
    
  } else if (timeframe == "LAST_90_DAYS"){
    days90(cntry, id, output_file =  "output.json")
    
  }
  
  read_lines2 <- possibly(read_lines, otherwise = NULL, quiet = FALSE)
  
  read_lines2("output.json") %>% 
    keep(~str_detect(.x, "ad_library_page_targeting_insight")) %>% 
    str_extract('\\{"require":.*\\}') %>% 
    jsonlite::fromJSON() %>% 
    list() %>% 
    find_items_dfs("ad_library_page_targeting_insight") %>% 
    parse_audience()
  
}

# library(tidyverse)



scraper2 <- function(internal, time = tf) {
  
  try({
    
    if((which(scrape_dat$page_id == internal$page_id) %% round(nrow(scrape_dat)/4, -1)) == 0){
      print(paste0(internal$page_name,": ", round(which(scrape_dat$page_id == internal$page_id)/nrow(scrape_dat)*100, 2)))
    }
    
  })
  
  
  # if(is.null(fin$error)){
  
  fin <<-
    # get_targeting(internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    get_page_insights_py(sets$cntry, internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>% 
    mutate(tstamp = tstamp) %>% 
    mutate(internal_id = internal$page_id)
  
  if (nrow(fin) != 0) {
    if (!dir.exists(glue::glue("targeting/{time}"))) {
      dir.create(glue::glue("targeting/{time}"), recursive = T)
    }
    
    path <-
      paste0(glue::glue("targeting/{time}/"), internal$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(fin %>% bind_rows(ol), file = path)
    # } else {
    
    saveRDS(fin, file = path)
    # }
  } else {
    fin <- tibble(internal_id = internal$page_id, no_data = T) %>%
      mutate(tstamp = tstamp)
  }
  
  # print(nrow(fin))
  # })
  return(fin)
  
  # }
  
}

scraper2 <- possibly(scraper2, otherwise = NULL, quiet = F)


# library(collections)  # For deque functionality