#' ---
#' title: "Scrap FC Zurich Spectator Data"
#' author: "Lucas Jamar"
#' ---

#' setup, include=FALSE
knitr::opts_chunk$set(eval = FALSE)

#' Load libraries
library(pbapply)
library(data.table)
library(stringr)
library(xml2)
library(rvest)
library(parsedate)

#' Read the pages of each season containing the matches of that season
seasons <- data.table(start = 2016:2020)
seasons[, end := start - floor((start + 1)/100)*100 + 1]
seasons[, season := paste0(start, "-", end)]
seasons[, link := paste0("https://www.fcz.ch/de/profis/spielplan/?season=", 1:5, "#schedule")]

matches <- data.table()
urls <- data.table()

for(link in seasons$link) {
  page <- xml2::read_html(link)
  match <- rvest::html_table(page)
  match <- data.table::as.data.table(match)
  match <- match[, 1:5]
  match[, link := link]
  matches <-  data.table::rbindlist(list(matches, match), fill = TRUE)
  
  url <- rvest::html_nodes(page, "a")
  url <- rvest::html_attr(url,  "href")
  url <- data.table::as.data.table(url)
  urls <- rbindlist(list(urls, url))
}

setnames(matches, c("match_start", "league", "match_number", "match", "results", "link"))
matches[seasons, on = .(link), season := season]
matches[, link := NULL]

#' Parse information from data
matches[, home_team := stringr::str_split(match, " – ", simplify = TRUE)[, 1]]
matches[, away_team := stringr::str_split(match, " – ", simplify = TRUE)[, 2]]
matches[, home_goals := stringr::str_extract_all(results, "\\d+", simplify = TRUE)[, 1]]
matches[, away_goals := stringr::str_extract_all(results, "\\d+", simplify = TRUE)[, 2]]
matches[, home_goals_first_half := stringr::str_extract_all(results, "\\d+", simplify = TRUE)[, 3]]
matches[, away_goals_first_half := stringr::str_extract_all(results, "\\d+", simplify = TRUE)[, 4]]
goals_columns <- c("home_goals",
                   "away_goals",
                   "home_goals_first_half",
                   "away_goals_first_half")
matches[, (goals_columns) := lapply(.SD, as.numeric), .SDcols = goals_columns]
matches[, match_start := parsedate::parse_date(match_start)]
matches[, date := as.Date(match_start)]

#' Get the links of individual matches
setnames(urls, "url", "link")
urls <- urls[link %like% "/de/profis/news/20"]
urls[, link := paste0("https://www.fcz.ch", link)]

get_match_info <- function(link) {
  page <- xml2::read_html(link)
  text <- rvest::html_text(page)
  return(text)
}

#' Extract info out of page
urls[, text := pblapply(link, get_match_info)]
urls[, index := .I]
urls[, title := stringr::str_split(text, " - ", simplify = TRUE)[, 1]]
urls[, text := stringr::str_squish(text)]

#' Title repeats itself twice in text. Take text occurring after second title
urls[, text := stringr::str_split(text, title, simplify = TRUE)[, 3]]

#' Match date is in next 20 words
urls[, date := stringr::word(text, 1, 20, sep = " ")]
urls[, date := parsedate::parse_date(date)]
urls[, date := as.Date(date)]

#' Get the text before last occurrence of "Zuschauer"
urls[, text := stringr::str_split(text, "Zuschauer")]
urls[, text := tail(head(unlist(text), -1), 1), by = index]
urls[, text := unlist(text)]

#' Get last number in text corresponding to Zuschauer number
urls[, spectators := stringr::str_extract_all(text, "\\d+")]
urls[, spectators := data.table::last(unlist(spectators)), by = index]
urls[, spectators := as.numeric(spectators)]
urls[, c("index", "text") := NULL]

#' Merge on match date. Remove missing values.
matches[urls, on = .(date), names(urls) := mget(names(urls))]
lubridate::tz(matches$match_start) <- "Europe/Berlin"

#' Write to file
fwrite(matches, "raw_data/fc_zurich.csv")
