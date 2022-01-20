library(rvest)
library(dplyr)
library(polite)
library(httr)
library(RSelenium)


# Creating Reference Metadata ---------------------------------------------

# Dataframe with id, name, term_start, term_end and number of speeches as per website
# id column is the ID assigned to each PM by the website.
# Scott Morrison's id 500 was selected arbitarily.

pm_meta <- data.frame(matrix(ncol = 5, nrow = 18))
colnames(pm_meta) <- c("id", "name", "term_start", "term_end", "n_speech")
pm_meta["id"] <- c(
  2, 1, 12, 7, 10, 5, 11, 14, 3,
  6, 9, 8, 13, 4, 13, 422, 427, 500
)
pm_meta["name"] <- c(
  "Curtin, John", "Chifley, Ben", "Menzies, Robert", "Holt, Harold",
  "McEwen, John", "Gorton, John", "McMahon, William", "Whitlam, Gough",
  "Fraser, Malcolm", "Hawke, Robert", "Keating, Paul", "Howard, John",
  "Rudd, Kevin", "Gillard, Julia", "Rudd, Kevin", "Abbott, Tony",
  "Turnbull, Malcolm", "Morrison, Scott"
)
pm_meta["term_start"] <- as.Date(c(
  "1941-10-07", "1945-06-13", "1949-12-19", "1966-01-26",
  "1967-12-19", "1968-01-10", "1971-03-10", "1972-12-05",
  "1975-11-11", "1983-03-11", "1991-12-20", "1996-03-11",
  "2007-12-03", "2010-06-24", "2013-06-27", "2013-10-18",
  "2015-10-15", "2018-09-24"
))
pm_meta["term_end"] <- as.Date(c(
  "1945-07-05", "1949-12-19", "1966-01-26", "1967-12-19",
  "1968-01-10", "1971-03-10", "1972-12-05", "1975-11-11",
  "1983-03-11", "1991-12-20", "1996-03-11", "2007-12-03",
  "2010-06-24", "2013-06-27", "2013-10-18", "2015-10-15",
  "2018-09-24", NA
))
pm_meta["n_speech"] <- c(
  4, 11, 1207, 507, 16, 625, 349, 1239, 2081, 2321,
  1582, 5868, 1735, 2072, 1735, 1372, 1752, NA
)


# Scraping ----------------------------------------------------------------

# All pm_home_ objects correspond to PM's main homepage
# which is the root directory for each PM.

# --- Link Selector for both websites ---
# Current PM Link = https://www.pm.gov.au/media?body_value=&field_media_type_value=transcript&page=2
# Older PM Link = https://pmtranscripts.pmc.gov.au/?combine=&field_release_date_value[min]=&field_release_date_value[max]=&field_long_title_value=&body_value=&field_prime_minister_target_id=8&items_per_page=All

# Starting RSelenium Client
rD <- rsDriver(browser = "firefox") #select your browser, chrome or firefox pref
# Initializing client object
remDr <- rD[["client"]]

# Initializing main Dataframe
all_pm_df <- data.frame()

# Main scraping loop - Loop through Prime ministers
# Select 1:17 to loop through the first 17 prime ministers in pm_meta.
for (id in pm_meta$id[1:18]) {
  if (id <= 17) { # For PM' before current (Scott Morrison)
  # Initializing new home page link based on PM id
  pm_home_link <- paste0(
    "https://pmtranscripts.pmc.gov.au/?combine=&field_release_date_value[min]=&field_release_date_value[max]=&field_long_title_value=&body_value=&field_prime_minister_target_id=", # nolint
    id, "&items_per_page=All" # from link selector
  )
  # Navigating to PM homepage
  remDr$navigate(pm_home_link)
  # Assigning new character HTML object from link
  pm_home_src <- remDr$getPageSource()[[1]]

  # Extracting meta for each PM using PM homescreen
  # Extracted table [rx4] contains data,title,PM_name, original_doc columns
  current_pm_df <- read_html(pm_home_src) %>% html_table()
  current_pm_df <- current_pm_df[[1]]
  # Extracting links for all speeches of selected PM from HTML object
  pm_speech_links <- read_html(pm_home_src) %>%
    html_nodes("td.views-field-field-release-date a") %>%
    html_attr("href")
  current_pm_df["ID"] <- pm_speech_links #  ID column houses links for corresponding speech title # nolint
  current_pm_df["Source"] <- as.character(NA) # Initialising column for HTML source of individual speeches # nolint
  }
  else{ # For Scott Morrison
    current_pm_df <- data.frame()
    for (page_id in 0:137) { # change 137 to last page
      
      # Initializing new home page link based on PM id
      pm_home_link <- paste0("https://www.pm.gov.au/media?body_value=&field_media_type_value=transcript&page=",page_id # from link selector
      )
      # Navigating to PM homepage
      remDr$navigate(pm_home_link)
      # Assigning new character HTML object from link
      pm_home_src <- remDr$getPageSource()[[1]]
      
      # Extracting meta for each PM using PM homescreen
      # Extracted table [rx4] contains data,title,PM_name, original_doc columns
      pm_speech_dates <- read_html(pm_home_src) %>% html_nodes(".date-display-single") %>% html_text() %>% as.Date("%d %b %Y")
      pm_speech_titles <- read_html(pm_home_src) %>% html_nodes(".media-title a") %>% html_text()
      pm_speech_links <- read_html(pm_home_src) %>%
        html_nodes(".media-title a") %>%
        html_attr("href")
      current_pm_df <- current_pm_df[[1]]
      temp_df <- data.frame(pm_speech_dates, pm_speech_titles) 
      temp_df['Prime Minister'] <- "Morrison, Scott"
      temp_df['Original Document'] <- NA
      temp_df['ID'] <- pm_speech_links
      temp_df <- temp_df %>% 
        rename('Release Date' = 'pm_speech_dates',
               'Title' = 'pm_speech_titles')
      temp_df['Source'] <- as.character(NA)
      current_pm_df <- bind_rows(current_pm_df, temp_df)
      }
  }
  # Sub loop - Loop through Documents
  # Loop through individual PM table to extract HTML source from every document
  for (i in seq_len(nrow(current_pm_df))) {
    # Initialize individual speech URL
    speech_url <- paste0(
      "https://pmtranscripts.pmc.gov.au", # from link selector
      current_pm_df[i, 5] # ID column
    )
    # Start Scraping
    Sys.sleep(runif(1, 4.0, 7.5)) # Crawl Delay as mentioned by the website
    # Navigate to speech webpage
    remDr$navigate(speech_url)
    # Get source
    pm_speech_src <- remDr$getPageSource()[[1]]
    current_pm_df[i, 6] <- pm_speech_src # Populating 'Source' column
  }
  # Getting PM's meta for savefile name
  current_pm_meta <- pm_meta %>%
    filter(name %in% current_pm_df$`Prime Minister`) %>%
    head(1)

  # Saving individual PM's speeches in separate files
  write.csv(current_pm_df, paste(current_pm_meta$term_start,
    tolower(strsplit(current_pm_meta$name, ",")[[1]][1]),
    sep = "-"
  ))
  # Adding New Prime minister's speeches to the main dataframe
  all_pm_df <- bind_rows(all_pm_df, current_pm_df)
}



# Extracting Text from HTML Source ----------------------------------------

# Function for extraction
extract_text <- function(df) {
  pb <- txtProgressBar(min = 0, max = 22741, initial = 0, style = 3) # progress bar
  # Intializing column for extracted text for individual speeches of each PM
  df["Document"] <- as.character(NA) 
  # Looping through all speeches
  stepi = 0
  for (src in df$Source) {
    stepi <- stepi + 1 # increment step for progressbar
    # error handler for broken encodings
    doc <- tryCatch({
        read_html(src) %>%
          html_nodes("p") %>%
          html_text()
      },
      error = function(e) {
        # cor_enc <- html_encoding_guess(src)$encoding[1]
        # print(cor_enc)
        src_fixed <- rvest::repair_encoding(src, from = "utf-8") # enforcing utf-8 encoding
        read_html(src_fixed) %>%
          html_nodes("p") %>%
          html_text()
      }
    )
    #print(stepi)
    df$Document[df$Source == src] <- paste(doc, collapse = " ")
    setTxtProgressBar(pb, stepi)
  }
  return(df)
}


# Calling the function ----------------------------------------------------

# Calling the function
all_pm_speech <- extract_text(all_pm_df)
all_pm_speech <- all_pm_speech %>% 
  select(-4,-6,-8) %>% # Keeping only required columns
  rename(date = `Release Date`,
         title = Title,
         pm = `Prime Minister`,
         id = ID,
         document = Document) %>%
  mutate(date = as.Date(date, "%d/%m/%Y"), # Fixing date format
         id = as.character(str_extract(id, "[[:digit:]]+"))) # Extract document ID from link

# Validate Encodings
# Even after implement a checker to validate encoding while scraping, this is still needed.
inv_enc <- all_pm_speech[!validUTF8(all_pm_speech$Document),] 
# Fix Encoding
inv_enc$Document <- stringi::stri_encode(inv_enc$Document, "", "utf-8")
# Merge fix with the main file
all_pm_speech[all_pm_speech$ID %in% inv_enc$ID, 7] = inv_enc$Document

# Saving speech file to disk
write.csv(all_pm_speech, "Data/all_pm_speech.csv")
saveRDS(all_pm_speech, "Data/all_pm_speech.Rds")
