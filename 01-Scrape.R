library(rvest)
library(dplyr)
library(polite)
library(httr)

# Scrape using polite and rvest

# pmt_session = bow("https://pmtranscripts.pmc.gov.au/release/transcript-1")
# pmt_data = scrape(pmt_session)
# page = read_html(link)

# Spoof it with common user agent
#
# ua <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15") # nolint
# seesion_with_ua <- html_session("https://pmtranscripts.pmc.gov.au/release/transcript-41750",ua) # nolint
#
# pmt_session_ua <- read_html("https://pmtranscripts.pmc.gov.au/release/transcript-41750", ua) # nolint
# pmt_session_ua <- read_html("https://pmtranscripts.pmc.gov.au/release/transcript-41750", # nolint
#                          user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15") # nolint

# Using RSelenium


library(RSelenium)

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


# All pm_home_ objects correspond to PM's main homepage
# which is the root directory for each PM.

rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]

all_pm_df <- data.frame()

for (pm_id in pm_meta$id[1:17]) {
  pm_home_link <- paste0(
    "https://pmtranscripts.pmc.gov.au/?combine=&field_release_date_value[min]=&field_release_date_value[max]=&field_long_title_value=&body_value=&field_prime_minister_target_id=", # nolint
    8, "&items_per_page=All"
  )
  remDr$navigate(pm_home_link)
  pm_home_src <- remDr$getPageSource()[[1]]

  current_pm_df <- read_html(pm_home_src) %>% html_table()
  current_pm_df <- current_pm_df[[1]]
  pm_speech_links <- read_html(pm_home_src) %>%
    html_nodes("td.views-field-field-release-date a") %>%
    html_attr("href")
  current_pm_df["ID"] <- pm_speech_links #  ID column houses links for corresponding speech title # nolint
  current_pm_df["Source"] <- as.character(NA) # Initialising column for individual speech urls # nolint

  for (i in seq_len(nrow(current_pm_df))) {
    speech_url <- paste0(
      "https://pmtranscripts.pmc.gov.au",
      current_pm_df[i, 5]
    )
    Sys.sleep(runif(1, 4.0, 7.5)) # Crawl Delay
    remDr$navigate(speech_url)
    pm_speech_src <- remDr$getPageSource()[[1]]
    current_pm_df[i, 6] <- pm_speech_src
  }
  current_pm_meta <- pm_meta %>%
    filter(name %in% current_pm_df$`Prime Minister`) %>%
    head(1)

  write.csv(current_pm_df, paste(current_pm_meta$term_start,
    tolower(strsplit(current_pm_meta$name, ",")[[1]][1]),
    sep = "-"
  ))
  all_pm_df <- bind_rows(all_pm_df, current_pm_df)
}

pm_speech_title <- read_html(pm_home_src) %>%
  html_nodes("td.views-field-field-release-date a") %>%
  html_text

fix_howard <- tibble::tibble(pm_speech_title, pm_speech_links)
all_pm_speech[all_pm_speech$Title %in% fix_howard$pm_speech_title, 5] = fix_howard[fix_howard$pm_speech_title %in% all_pm_speech$Title, 2]

fixed_howard <- dplyr::left_join(all_pm_speech,fix_howard,by=c("Title" = "pm_speech_title"))

# Validate Encodings
inv_enc <- all_pm_speech[!validUTF8(all_pm_speech$Document),]
inv_enc$Document <- stringi::stri_encode(inv_enc$Document, "", "utf-8")
all_pm_speech[all_pm_speech$ID %in% inv_enc$ID, 7] = inv_enc$Document

# Short documents
short_doc <- all_pm_speech[stringi::stri_length(all_pm_speech$Document) < 50,]

write.csv(all_pm_speech_fix, "all_pm_speech.csv")


write.csv(all_pm_df, "all_pm_df.csv")

for(i in seq_len(nrow(howard2))) {

howard2[i,5] = read_html(pull(howard2[i,6])) %>% 
                    html_elements(":nth-child(3) .col-lg-8") %>% 
                    html_text()
}


# Extract Text from HTML

extract_text <- function(df) {
  pb <- txtProgressBar(min = 0, max = 22741, initial = 0, style = 3)
  df["Document"] <- as.character(NA)
  for (src in df$Source) {
    stepi <- stepi + 1
    doc <- tryCatch({
        read_html(src) %>%
          html_nodes("p") %>%
          html_text()
      },
      error = function(e) {
        # cor_enc <- html_encoding_guess(src)$encoding[1]
        # print(cor_enc)
        src_fixed <- rvest::repair_encoding(src, from = "utf-8")
        read_html(src_fixed) %>%
          html_nodes("p") %>%
          html_text()
      }
    )
    print(stepi)
    df$Document[df$Source == src] <- paste(doc, collapse = " ")
    setTxtProgressBar(pb, stepi)
  }
  return(df)
}

all_pm_speech <- extract_text(all_pm_df)

write.csv(all_pm_speech, "all_pm_speech.csv")

all_pm_speech <- readr::read_csv("data/all_pm_speech.csv")

textreadr::as_transcript("34    The New York Times reports a lot of words here",
        col.names = c("NO", "ARTICLE"), sep = "   ")

test <- filter(all_pm_speech, `Prime Minister` == "Curtin, John")

test[,c("ID","Document")]

library(mtcars)

test2<-all_pm_speech[validUTF8(all_pm_speech$Document),]

test3 <- test2[stringi::stri_length(test2$Document) < 50,]

test_inv<-all_pm_speech[!validUTF8(all_pm_speech$Document),]

test_inv_2<-test_inv[!validUTF8(test_inv$Document),]


test_inv$Document <- stringi::stri_encode(test_inv$Document, "", "utf-8")
test5 <- test_inv[stringi::stri_length(test_inv$Document) < 50,]


all_pm_speech[all_pm_speech$`...1` == 12666,6] = "release/transcript-12866"
all_pm_speech[all_pm_speech$`...1` == 12666,8] = "Following extensive consultation the Government has selected four National Research Priorities to focus our investment on research in key areas that can deliver significant economic, social and environmental benefits to Australia.
This is the first time that the Commonwealth has set national research priorities, an exercise that will build on our national research strengths while seeking new opportunities in emerging areas.
The National Research Priorities are:
- An Environmentally Sustainable Australia; - Promoting and Maintaining Good Health; - Frontier Technologies for Building and Transforming Australian Industries; and - Safeguarding Australia.
These priorities are aspirational in nature and will be recognised by all Australians as areas of endeavour that will help to deliver the kind of future we want.
Equally important is that a focus on excellence will underpin success in these priority areas. A broadly based and high quality research system that pursues excellence, particularly in the enabling sciences, remains fundamental.
Each of the themes has a number of priority goals:
An environmentally sustainable Australia is about transforming the way Australians use the nation';s land, water, mineral and energy resources. This will depend on a better understanding of the environment and the application of new technologies to natural resource industries.
Promoting and maintaining good health focuses on promoting the healthy development of young Australians and on ensuring that older Australians enjoy healthy and productive lives. It is also about adopting healthier attitudes and lifestyles to promote well being across the lifecycle.
Frontier technologies for building and transforming Australian industries is about fostering creativity and innovation by supporting leading edge research in areas such as information and communication technology (ICT), bio-and geo-informatics, nanotechnology and biotechnology. ICT has an important role to play as a platform for ensuring the use and application of new technology to many industry sectors. Support for these areas of research will help stimulate vibrant new industries and ensure our future competitiveness.
Safeguarding Australia focuses on a range of research relevant to protecting Australia from terrorism, crime, invasive diseases and pests and threats to our critical infrastructure
As a first step towards implementation, all Commonwealth research and research funding bodies will be asked to submit plans to the Government by May 2003 outlining how they propose to support the four priorities. The Government has also announced a major exercise to take stock of the state of Australian science by mapping science and innovation activities across the public and private sectors.
I have written today to State and Territory Premiers and Chief Ministers outlining the priority areas and seeking their support for this important national initiative.
The priorities will provide a catalyst for the formation of teams and networks of researchers across many disciplines in Australia and internationally –– thereby stimulating further innovation and creativity.
The setting of National Research Priorities was foreshadowed in the Government';s $3 billion Innovation Statement Backing Australia';s Ability last year. Initial public consultations with around 800 researchers, industry groups and community representatives were followed by an examination of more than 180 public submissions. An expert committee chaired by Dr Jim Peacock, President of the Academy of Science, and including the Chief Scientist Dr Robin Batterham, submitted a short-list to the Government.
Although the priorities require particularly strong input from science, engineering and technology-based research, the social sciences and humanities will also play an important collaborative role in their implementation.
Science and Innovation is one of the Government';s key strategic policy priorities. This year the Commonwealth will invest a record $5.1 billion on science and innovation, as initiatives in Backing Australia';s Ability continue to be rolled out.
By harnessing the talents of our best and brightest we can entrench our current economic strength and create even greater opportunities for our children."


mtcars[,c("mpg","cyl")]

# Rough Code - Debugging

# library(data.table)
#
# tbl <-
#   list.files(pattern = "*.csv") %>%
#   map_df(~read_csv(., col_types = cols(.default = "c")))

# all_pm_df$Source[all_pm_df$`Prime Minister` == "Howard, John"] = X1996_03_11_howard$Source
# write.csv(all_pm_df, "all_pm_pmt.csv")
# write.csv(X1996_03_11_howard, "1996_03_11_howard.csv")
#
# pm_home_link <- paste0("https://pmtranscripts.pmc.gov.au/?combine=&field_release_date_value[min]=&field_release_date_value[max]=&field_long_title_value=&body_value=&field_prime_minister_target_id=",
#                                  8, "&items_per_page=All")
# remDr$navigate("https://pmtranscripts.pmc.gov.au/release/transcript-10157")
# X1996_03_11_howard[5868, 7] <- remDr$getPageSource()[[1]]
#
# howard <- read_html(pm_home_src) %>% html_table()
# howard <- howard[[1]]
# howard_links <- read_html(pm_home_src) %>% html_nodes("td.views-field-field-release-date a") %>% html_attr("href")
# howard["ID"] = howard_links #  ID column houses links for corresponding speech title
# howard["Source"] <- as.character(NA)
#
# first_speech <- X1941_10_07_curtin[1,7] %>% pull()
# stringi::stri_enc_detect(str = pull(X1941_10_07_curtin[4,7]))
# html_encoding_guess(sec_speech)
# rvest::repair_encoding(sec_speech, from = "ISO-8859-1")

# for (src in test_df$Source){
#   doc <- tryCatch({read_html(src) %>% html_nodes("p") %>% html_text()},
#                   error = function(e){
#                     cor_enc <- html_encoding_guess(src)$encoding[1]
#                     src_fixed <- repair_encoding(src, from = cor_enc)
#                     read_html(src_fixed) %>% html_nodes("p") %>% html_text()
#                   })
#   if(inherits(doc, "try-error"))
#   {
#     cor_enc <- html_encoding_guess(src)$encoding[1]
#     src_fixed <- repair_encoding(src, from = cor_enc)
#     doc <- read_html(src) %>% html_nodes("p") %>% html_text()
#   }
#   test_df$document[test_df$Source == src] <- paste(doc, collapse = "     ")
# }