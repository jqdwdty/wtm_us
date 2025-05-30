
try({
  
  outcome <- commandArgs(trailingOnly = TRUE)
  
  tf <- outcome[1]
  the_cntry <- outcome[2]
  
  print(outcome)
  
  if (Sys.info()[["effective_user"]] %in% c("fabio", "favstats")) {
    ### CHANGE ME WHEN LOCAL!
    tf <- "30"
    the_cntry <- "DE"
    print(paste0("TF: ", tf))
    print(paste0("cntry: ", the_cntry))
    
  }
  
  
  source("utils.R")
  
  
  library(httr)
  library(httr2)
  library(tidyverse)
  library(lubridate)
  library(rvest)
  library(piggyback)
  library(openssl)
  library(jsonlite)
  
  
  eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                    "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                    "NL", "PL", "PT", "RO", "SE", "SI", "SK", "US", "MX", "NZ", 
                    "CA", "AU")
  
  full_cntry_list <-
    read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>%
    rename(iso2c = iso2,
           country = cntry) %>%
    sample_n(n()) %>% 
    mutate(iso2c = fct_relevel(iso2c, eu_countries)) %>% 
    arrange(iso2c)
  
  
  print("################ CHECK LATEST REPORT ################")
  
  
  try({
    
    timeframes <- c("yesterday", "7", "30", "90")
    # Create all combinations of country codes and timeframes
    combinations <- expand.grid(country_code = the_cntry, timeframe = timeframes, stringsAsFactors = FALSE)
    
    # Apply the function to each combination
    ress <- pmap(combinations, ~ retrieve_reports_data(..1, ..2))
    
    # Combine results into a single data frame
    latest <- bind_rows(ress)      
    
    
    then_this <- latest %>%
      group_by(country, timeframe) %>%
      slice(1) %>%
      ungroup() %>% 
      filter(str_detect(timeframe, "last_90_days"))
    
    
    last7 <- get_report_db(the_cntry, timeframe = 90, then_this$day) %>%
      mutate(sources = "report") %>%
      mutate(party = "unknown")
    
  })
  
  if (!exists("last7")) {
    last7 <- tibble()
  }
  
  
  togetstuff <- last7 %>% 
    select(contains("page_id") , contains("amount")) %>% 
    set_names("page_id", "spend") %>% 
    mutate(spend = parse_number(spend)) %>% 
    arrange(desc(spend))
  
  
  
  for (i in 1:length(togetstuff$page_id)) {
    # Get insights for the current page ID
    jb <- get_page_insights(
      togetstuff$page_id[i], 
      timeframe = glue::glue("LAST_90_DAYS"), 
      include_info = "targeting_info"
    )
    
    # Check if `jb` is not NULL
    if (!is.null(jb)) {
      # print("is not null")
      if(nrow(jb) == 0){
        # print("but is zero")
        next
      } else {
        # Extract the `new_ds` value
        new_ds <- jb %>% 
          arrange(ds) %>% 
          slice(1) %>% 
          pull(ds)
        
        # Break the loop if `new_ds` is successfully assigned
        if (!is.null(new_ds)) {
          # message("New `ds` found, breaking the loop.")
          break
        }
        
      }
    } 
  }
  
  to_get <- latest %>%
    filter(day == new_ds) %>%
    filter(str_detect(timeframe, tf))
  
  if (nrow(to_get) != 0) {
    # debugonce(retrieve_reports_data)
    last7 <- get_report_db(the_cntry, readr::parse_number(to_get$timeframe), to_get$day)
    
    togetstuff <-
      last7 %>% select(page_id , contains("amount")) %>%
      set_names("page_id", "spend") %>%
      mutate(spend = parse_number(spend)) %>%
      arrange(desc(spend))
    
    report_matched = T
  } else {
    report_matched = F
    
  }
  
  print("################ LATEST TARGETING DATA ################")
  
  try({
    
    # Combine results into a single data frame
    thosearethere <- retrieve_targeting_metadata(the_cntry, tf)
    
    try({
      
      latest_elex <- get_targeting_db(the_cntry, tf, thosearethere$ds[1], verbose = T)
      
    })
    
    if (!exists("latest_elex")) {
      latest_elex <- tibble()
    }
    
    if (!("ds" %in% names(latest_elex))) {
      latest_elex <- latest_elex %>% mutate(ds = "")
    }
    
    latest_ds <- thosearethere$ds[1]
    
  })
  
  
  if (!exists("latest_ds")) {
    latest_ds <- "2023-01-01"
  } else if (is.na(latest_ds)) {
    latest_ds <- "2023-01-01"
  }
  
  
  tstamp <- Sys.time()
  
  write_lines(lubridate::as_date(tstamp), "tstamp.txt")
  
  country_codes <- c(
    "AD", "AL", "AM", "AR", "AT", "AU", "BA",
    "BE", "BG", "BR", "CA", "CH", "CL", "CO", 
    "CY", "CZ", "DE", "DK", "EC", "EE", "ES", 
    "FI", "FR", "GB", "GR", "GT", "HR", "HU",
    "IE", "IN", "IS", "IT", "LI", "LT", "LU",
    "LV", "MD", "ME", "MK", "MT", "MX", "NL", 
    "NO", "NZ", "PL", "PT", "RO", "RS", "SE",
    "SI", "SK", "SM", "TR", "UA", "US", "VE",
    "ZA"
  )
  
  print("################ WTM DATA ################")
  
  
  # try({
  #   download.file(
  #     paste0(
  #       "https://data-api.whotargets.me/advertisers-export-csv?countries.alpha2=",
  #       str_to_lower(the_cntry)
  #     ),
  #     destfile = "data/wtm_advertisers.csv"
  #   )
  #   
  #   thedat <- read_csv("data/wtm_advertisers.csv")
  #   
  # })
  
  if (!exists("thedat")) {
    thedat <- tibble(no_data = NULL)
  }
  
  
  if (the_cntry %in% country_codes & nrow(thedat) != 0) {
    wtm_data <- read_csv("data/wtm_advertisers.csv") %>% #names
      select(page_id = advertisers_platforms.advertiser_platform_ref,
             page_name = name,
             party = entities.short_name)  %>%
      mutate(page_id = as.character(page_id)) %>%
      mutate(sources = "wtm")
    
  } else {
    wtm_data <-  tibble(no_data = T)
  }
  
  polsample <- readRDS("data/polsample.rds")
  
  tep_dat <- polsample %>%
    filter(cntry %in% the_cntry) %>%
    mutate(sources = "tep") %>%
    rename(party = name_short)
  
  
  all_dat <- bind_rows(wtm_data) %>%
    bind_rows(tep_dat) %>%
    bind_rows(last7) %>%
    distinct(page_id, .keep_all = T) %>%
    add_count(page_name, sort  = T) %>%
    mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
    filter(!remove_em) %>%
    select(-n,-contains("no_data"))  %>% 
    mutate(total_n = n()) %>% 
    filter(page_id != 0) 
  
  
  the_amount <- all_dat %>% names() %>% keep(~str_detect(.x, "amount_spent")) %>% .[1]
  
  
  
  all_dat <- all_dat %>% 
    mutate(amount_spent = parse_number(as.character(all_dat[[the_amount]]))) %>% 
    arrange(desc(amount_spent)) %>%
    distinct(page_id, .keep_all = T)
  
  
  saveRDS(all_dat, "data/all_dat.rds")
  
  scrape_dat <- all_dat
  
  fin <<- tibble(no_data = T)
  
  scraper <- function(internal, time = tf) {
    try({
      
      if((which(scrape_dat$page_id == internal$page_id) %% round(nrow(scrape_dat)/4, -1)) == 0){
        
        print(paste0(internal$page_name,": ", round(which(scrape_dat$page_id == internal$page_id)/nrow(scrape_dat)*100, 2)))
        
      }
      
    })
    
    # if(is.null(fin$error)){
    
    fin <<-
      # get_targeting(internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
      get_page_insights(internal$page_id, timeframe = glue::glue("LAST_{time}_DAYS"), include_info = "targeting_info", iso2c = the_cntry) %>% 
      mutate(tstamp = tstamp)
    
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
    
    
    
    # if (Sys.info()[["effective_user"]] %in% c("fabio", "favstats")) {
    ### CHANGE ME WHEN LOCAL!
    # print(nrow(fin))
    
    # }# 
    # })
    return(fin)
    
    # }
    
  }
  
  scraper <- possibly(scraper, otherwise = NULL, quiet = F)
  
  
  print("################ RETRIEVE AUDIENCES ################")
  
  try({
    
    current_date <-
      paste0("historic/",
             as.character(new_ds),
             "/",
             "last_",
             tf,
             "_days")
    
    if (new_ds == latest_ds) {
      print(glue::glue("New DS: {new_ds}: Old DS: {latest_ds}"))
      
      scrape_dat <- all_dat %>%
        # arrange(page_id) %>%
        # slice(1:150) %>%
        filter(!(page_id %in% latest_elex$page_id))  %>%
        filter(page_id %in% last7$page_id) %>% 
        mutate(total_n = n())
      
      the_rows_to_be_checked <- nrow(scrape_dat)
      
      print(paste0("Number of remaining pages to check: ", nrow(scrape_dat)))
      
      ### save seperately
      enddat <-  scrape_dat %>%
        split(1:nrow(.)) %>%
        map_dfr(scraper)
      
      if (nrow(enddat) == 0) {
        
        print("same length! will just save the same parquet!")
        
        election_dat <- latest_elex
        
        dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
        
        
        arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
        
      } else {
        
        print("new data to be uploaded")
        
        if(is.null(enddat$page_id)){
          enddat$page_id <- enddat$internal_id
        }
        
        new_elex <- enddat
        
        print(glue::glue("Old Number of Page IDs: {length(unique(new_elex$page_id))}"))
        
        try({
          
          latest_elex <- get_targeting_db(the_cntry, tf, thosearethere$ds[1], verbose = T)
          
        })
        
        library(dplyr)
        
        distinct_if <- function(data, ..., .keep_all = TRUE) {
          # Capture column names as symbols
          vars <- rlang::ensyms(...)
          
          # Filter for variables that exist in the dataset
          present_vars <- vars[sapply(vars, function(x) rlang::as_string(x) %in% names(data))]
          
          # If no variables are present, return the data unchanged
          if (length(present_vars) == 0) {
            warning("None of the specified variables are present in the data. Returning the original data.")
            return(data)
          }
          
          # Apply distinct on the present variables
          data %>%
            distinct(across(all_of(sapply(present_vars, rlang::as_string))), .keep_all = .keep_all)
        }
        
        # ones <- jb %>% 
        #   mutate(id = 1:n()) %>% 
        #   as_tibble() 
        # 
        # twos <- jb %>% 
        #   mutate(id = 1:n()) %>% 
        #   as_tibble() %>% 
        #   distinct_if(page_id, total_num_ads, total_spend_formatted, is_exclusion,
        #               value, type, detailed_type, custom_audience_type, location_type, .keep_all = T) 
        # 
        # ones %>% anti_join(twos %>% select(id)) %>% View()
        # ones %>% View()
        election_dat  <- enddat %>%
          mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) %>%
          # rename(page_id = internal_id) %>%
          left_join(all_dat) %>%
          bind_rows(latest_elex %>% filter(!(page_id %in% enddat$page_id))) %>% 
          distinct_if(page_id, total_num_ads, total_spend_formatted, is_exclusion,
                      value, type, detailed_type, custom_audience_type, location_type, .keep_all = T) 
        
        dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
        
        print(glue::glue("New Number of Rows: {length(unique(election_dat$page_id))}"))
        
        
        arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
        
        
      }
      
      
    } else {
      
      print(glue::glue("Complete new Data. New DS: {new_ds}: Old DS: {latest_ds} 2"))
      
      print(paste0("Number of pages to check: ", nrow(scrape_dat)))
      
      the_rows_to_be_checked <- nrow(scrape_dat)
      
      # debugonce(scraper)
      ### save seperately
      election_dat <- all_dat %>%
        # arrange(page_id) %>%
        # slice(1:2) %>%
        split(1:nrow(.)) %>%
        map_dfr(scraper)  %>%
        mutate_at(vars(contains("total_spend_formatted")), ~ parse_number(as.character(.x))) 
      
      if(is.null(election_dat$page_id)){
        election_dat$page_id <- election_dat$internal_id
      }
      
      
      election_dat <- election_dat %>% 
        left_join(all_dat)
      
      
      print(glue::glue("Number of Rows: {nrow(election_dat)}"))
      
      dir.create(paste0("historic/",  as.character(new_ds)), recursive = T)
      
      
      arrow::write_parquet(election_dat, paste0(current_date, ".parquet"))
      
      
    }
  })
  
  
  the_tag <- paste0(the_cntry, "-", "last_", tf, "_days")
  the_date <- new_ds
  
  
  releases <- readRDS("data/releases.rds")
  
  cntry_name <- full_cntry_list %>%
    filter(iso2c == the_cntry) %>%
    pull(country)
  
  if(!(the_tag %in% releases$tag_name)){
    try({
      pb_release_create_fr(
        repo = "favstats/meta_ad_targeting",
        tag = the_tag,
        body = paste0(
          "This release includes ",
          cntry_name , " '", "last_", tf, "_days" , "' Meta ad target audiences."
        ),
        releases = releases
      )    # Sys.sleep(5)
    })
  }
  
  
  file.copy(paste0(current_date, ".parquet"),
            paste0(the_date, ".parquet"),
            overwrite = T)
  
  print(file.exists(paste0(the_date, ".parquet")))
  
  if("no_data" %in% names(election_dat)){
    election_dat <- election_dat %>% filter(is.na(no_data))
  }
  
  if("no_data" %in% names(latest_elex)){
    latest_elex <- latest_elex %>% filter(is.na(no_data))
  }
  
  
  
  
  if(!(identical(latest_elex, election_dat))){
    
    print("################ UPLOAD FILE ################")
    
    
    try({
      # print(paste0(the_date, ".rds"))
      # print(the_tag)
      # debugonce(pb_upload_file_fr)
      rsd <- pb_upload_file_fr(
        paste0(the_date, ".parquet"),
        repo = "favstats/meta_ad_targeting",
        tag = the_tag,
        releases = releases
      )
      # pb_upload_file_fr(paste0(the_date, ".zip"), repo = "favstats/meta_ad_reports", tag = the_tag, releases = full_repos)
      try({
        the_status_code <- httr::status_code(rsd)
      })
    })
    
    print(paste0("################ UPLOADED FILE ################: ", the_cntry))
    
    
  } else {
    print("File is identical, will not be uploaded")
  }
  
  file.remove(paste0(the_date, ".parquet"))
  
  
  gc()
  
  # }
  # # .[1:7] %>%
  # walk_progress( ~ {
  #
  #
  # })
  
  # unzip("report/TN/2023-11-28.zip", exdir = "extracted", overwrite = T)
  
  # unzip(dir(paste0("report/",cntry_str), full.names = T, recursive = T), exdir = "extracted")
  
  
  
  unlink("targeting", recursive = T, force = T)
  unlink("historic", recursive = T, force = T)
  
  print("################ FIN ################")
  
  # }
  
  # unlink("node_modules", recursive = T, force = T)
  # unlink("out", recursive = T, force = T)
  
  
})

if(!exists("new_elex")){
  new_elex <- tibble()
} else {
  if("no_data" %in% names(new_elex)){
    new_elex <- new_elex %>% filter(is.na(no_data))
  }
}

if(!exists("the_rows_to_be_checked")){
  the_rows_to_be_checked <- tibble()
} 

if(!exists("the_status_code")){
  the_status_code <- "no status code"
} 




# Telegram bot setup
TELEGRAM_BOT_ID <- Sys.getenv("TELEGRAM_BOT_ID")
TELEGRAM_GROUP_ID <- Sys.getenv("TELEGRAM_GROUP_ID")

# Function to log final statistics with Telegram integration
log_final_statistics <- function(stage, tf, cntry, new_ds, latest_ds,
                                 the_rows_to_be_checked, election_dat, new_elex,
                                 pushed_successfully, togetstuff, report_matched) {
  # Check if ds was already present
  ds_present <- ifelse(new_ds == latest_ds, "Yes", "No")
  
  # Calculate statistics
  total_rows <- length(unique(election_dat$page_id))
  new_rows <- length(unique(new_elex$page_id))
  lag_days <- as.numeric(Sys.Date() - lubridate::ymd(new_ds))
  
  # Spending coverage statistics
  page_ids_in_togetstuff <- sum(togetstuff$page_id %in% election_dat$page_id)
  total_spend_in_togetstuff <- sum(togetstuff$spend, na.rm = TRUE)
  election_dat <- distinct(election_dat, page_id, .keep_all = T)
  covered_spend <- sum(election_dat$amount_spent[election_dat$page_id %in% togetstuff$page_id], na.rm = TRUE)
  
  spend_coverage_pct <- round((covered_spend / total_spend_in_togetstuff) * 100)
  coverage_status <- ifelse(spend_coverage_pct == 100, "✅", "❌")
  
  # Check GitHub push status
  push_status <- ifelse(pushed_successfully, "✅ Yes", "❌ No")
  report_status <- ifelse(report_matched, "✅ Yes", "❌ No")
  
  
  if(page_ids_in_togetstuff==nrow(togetstuff) | new_rows == 0){
    should_continue <- update_workflow_schedule(F)
  } else {
    should_continue <- update_workflow_schedule(T)
  }
  
  
  if (should_continue) {
    writeLines("changes_detected", glue::glue("status_{tf}.txt"))
    print(glue::glue("Status for timeframe {tf}: changes_detected"))
  } else {
    writeLines("no_changes", glue::glue("status_{tf}.txt"))
    print(glue::glue("Status for timeframe {tf}: no_changes"))
  }

  what_it_gets <- nrow(togetstuff)
  bad_hombres <- page_ids_in_togetstuff / what_it_gets

  if(bad_hombres <= 0.65){
    should_continue <- TRUE
  }
  
  should_continue <- ifelse(should_continue, "✅ Yes", "❌ No")
  
  # Construct details message
  details <- glue::glue(
    "   \t\t📌 *Newest DS:* {new_ds}\n",
    "   \t\t📌 *Latest DS:* {latest_ds}\n",
    "   \t\t📌 *DS Already Present:* {ds_present}\n",
    "   \t\t🔋 *Page IDs Checked:* {the_rows_to_be_checked}\n",
    "   \t\t📊 *Total Page IDs:* {total_rows}\n",
    "   \t\t➕ *New Page IDs Added:* {new_rows}\n",
    "   \t\t🕒 *Days Lagging:* {lag_days} days\n",
    "   \t\t🚀 *GitHub Push Successful:* {push_status}\n",
    "   \t\t😎 *Report Matched:* {report_status}\n",
    "   \t\t🔍 *Page IDs Present (of Report):* {page_ids_in_togetstuff}/{nrow(togetstuff)}\n",
    "   \t\t💰 *Spending Coverage:* {covered_spend}/{total_spend_in_togetstuff} ({spend_coverage_pct}% {coverage_status})\n",
    "   \t\t📌 *Continue Today:* {should_continue}\n",
    "   \t\t💰 *Source:* jqdwdty"
  )
  
  # Construct the full message
  the_message <- glue::glue(
    "🔹 *{stage}* 🔹\n",
    "🌍 *Country:* {cntry}\n",
    "⏳ *Timeframe:* {tf}\n",
    "🕒 *Time:* {Sys.time()}\n",
    "{details}"
  )
  
  print(the_message)
  
  # Send the message to Telegram
  url <- paste0("https://api.telegram.org/bot", Sys.getenv("TELEGRAM_BOT_ID"), "/sendMessage")
  out <<- httr::POST(url, body = list(chat_id = Sys.getenv("TELEGRAM_GROUP_ID"), text = the_message, parse_mode = "Markdown"), encode = "form")
  if (httr::http_error(out)) {
    print(httr::content(out))
    print(httr::headers(out))
  }
  
  # # Data for the tibble
  # the_data <- tibble::tibble(
  #   Field = c(
  #     "stage",
  #     "country",
  #     "timeframe",
  #     "time",
  #     "newest_ds",
  #     "latest_ds",
  #     "ds_already_present",
  #     "page_ids_checked",
  #     "total_page_ids",
  #     "new_page_ids_added",
  #     "days_lagging",
  #     "github_push_successful",
  #     "report_matched",
  #     "page_ids_present_audience",
  #     "page_ids_present_report",
  #     "spending_coverage"
  #   ),
  #   Value = c(
  #     stage,
  #     cntry,
  #     tf,
  #     as.character(Sys.time()),
  #     new_ds,
  #     latest_ds,
  #     ds_present,
  #     the_rows_to_be_checked,
  #     total_rows,
  #     new_rows,
  #     lag_days,
  #     push_status,
  #     report_status,
  #     page_ids_in_togetstuff,
  #     nrow(togetstuff),
  #     paste(covered_spend, "/", total_spend_in_togetstuff, "(", spend_coverage_pct, "%", coverage_status, ")")
  #   )
  # )
  
  
}

update_workflow_schedule <- function(should_continue = TRUE, thetf = tf, verbose = TRUE) {
  all_timeframes <- c(7, 30, 90)  # Define all timeframes
  other_timeframes <- setdiff(all_timeframes, as.numeric(thetf))  # Timeframes to modify
  
  if (should_continue) {
    # Remove `on: push` from other timeframes
    for (tf in other_timeframes) {
      workflow_file <- glue::glue(".github/workflows/r{tf}.yml")
      if (verbose) print(glue::glue("Processing workflow file: {workflow_file}"))
      
      if (!file.exists(workflow_file)) {
        if (verbose) print(glue::glue("Workflow file does not exist: {workflow_file}. Skipping."))
        next
      }
      
      workflow_content <- readLines(workflow_file)
      
      # Remove `push` block if it exists
      push_start_idx <- which(str_detect(workflow_content, "^  push:"))
      if (length(push_start_idx) > 0) {
        branches_end_idx <- push_start_idx + 3  # Assuming the block has 3 lines
        workflow_content <- workflow_content[-(push_start_idx:branches_end_idx)]
        if (verbose) print(glue::glue("'push' block removed from {workflow_file}."))
      } else {
        if (verbose) print(glue::glue("No 'push' block found in {workflow_file}. Skipping."))
      }
      
      writeLines(workflow_content, workflow_file)
    }
    
    # Ensure `on: push` exists for the current timeframe
    workflow_file <- glue::glue(".github/workflows/r{thetf}.yml")
    if (verbose) print(glue::glue("Processing current workflow file: {workflow_file}"))
    
    if (!file.exists(workflow_file)) {
      if (verbose) print(glue::glue("Workflow file does not exist: {workflow_file}. Exiting."))
      return(FALSE)
    }
    
    workflow_content <- readLines(workflow_file)
    
    # Check if `push` block exists
    push_start_idx <- which(str_detect(workflow_content, "^  push:"))
    if (length(push_start_idx) == 0) {
      # Add `push` block after `on:`
      on_idx <- which(str_detect(workflow_content, "^on:"))
      if (length(on_idx) == 1) {
        if (verbose) print(glue::glue("Adding 'push' block to {workflow_file}."))
        push_block <- c(
          "  push:",
          "    branches:",
          "      - master",
          "      - main"
        )
        workflow_content <- append(workflow_content, push_block, after = on_idx)
        writeLines(workflow_content, workflow_file)
        if (verbose) print(glue::glue("'push' block added successfully to {workflow_file}."))
      } else {
        if (verbose) print(glue::glue("Could not find 'on:' block in {workflow_file}. Skipping."))
      }
    } else {
      if (verbose) print(glue::glue("'push' block already exists in {workflow_file}. No changes made."))
    }
  } else {
    # For `should_continue = FALSE`, remove `on: push` and update cron schedule for the current timeframe
    workflow_file <- glue::glue(".github/workflows/r{thetf}.yml")
    if (verbose) print(glue::glue("Processing current workflow file: {workflow_file}"))
    
    if (!file.exists(workflow_file)) {
      if (verbose) print(glue::glue("Workflow file does not exist: {workflow_file}. Exiting."))
      return(FALSE)
    }
    
    workflow_content <- readLines(workflow_file)
    
    # Remove `push` block if it exists
    push_start_idx <- which(str_detect(workflow_content, "^  push:"))
    if (length(push_start_idx) > 0) {
      branches_end_idx <- push_start_idx + 3  # Assuming the block has 3 lines
      workflow_content <- workflow_content[-(push_start_idx:branches_end_idx)]
      if (verbose) print(glue::glue("'push' block removed from {workflow_file}."))
    }
    
    # Update cron schedule
    cron_line_idx <- which(str_detect(workflow_content, "cron:"))
    settimer <- sample(1:12, 1)
    new_cron <- glue::glue("    - cron: '0 {settimer} * * *'")
    
    if (length(cron_line_idx) > 0) {
      workflow_content[cron_line_idx] <- new_cron
      if (verbose) print(glue::glue("Updated cron schedule in {workflow_file} to: {new_cron}"))
    } else {
      if (verbose) print("No cron line found. This may cause issues.")
    }
    
    writeLines(workflow_content, workflow_file)
    if (verbose) print(glue::glue("Workflow file updated: {workflow_file}"))
  }
  
  if (verbose) print("Workflow update process complete.")
  return(should_continue)
}

# tf <- 30
# update_workflow_schedule(T)


try({
  # Example integration (call this after processing):
  log_final_statistics(
    stage = "Process Complete",
    tf = tf,
    cntry = the_cntry,
    new_ds = new_ds,
    latest_ds = latest_ds,
    the_rows_to_be_checked = the_rows_to_be_checked,
    election_dat = election_dat,
    new_elex = new_elex,
    pushed_successfully = the_status_code,
    togetstuff = togetstuff,
    report_matched = report_matched
  )
})




print("################ VERY END ################")
