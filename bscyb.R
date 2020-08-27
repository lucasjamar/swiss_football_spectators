#' ---
#' title: "Scrap BSCYB Spectator Data"
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

#' Get match data for 4 seasons
seasons <- data.table(start = 2001:2020)
seasons[, end := as.character(start - floor((start + 1)/100)*100 + 1)]
seasons[end %in% 0:9, end := paste0("0",end)]
seasons[, season := paste0(start, "-", end)]
seasons[, url := paste0("https://www.bscyb.ch/archiv-", season)]

matches <- data.table::data.table()
urls <- data.table::data.table()

for(url in seasons$url){
  response <- httr::GET(url = url)
  if(response$status_code == 200){
    page <- xml2::read_html(url)
    match <- rvest::html_table(page, fill = TRUE)
    match <- data.table::first(match)
    match <- data.table::as.data.table(match)
    match[, url := url]
    matches <- data.table::rbindlist(list(matches, match), fill = TRUE)
    
    url <- rvest::html_nodes(page, "a")
    url <- rvest::html_attr(url,  "href")
    url <- data.table::as.data.table(url)
    urls <- rbindlist(list(urls, url))
  }
}

setnames(matches, colnames(matches)[1:4], c("date", "league", "match", "results"))
matches[seasons, on = .(url), season := season]
matches[, url := NULL]

#' Parse information from data ----
matches[, index := .I]
matches[, date := parsedate::parse_date(date)]
matches[, date := as.Date(date)]
matches[is.na(match), match := stringr::str_split(league, ", ", simplify = TRUE)[, 2]]
matches[, league := stringr::str_split(league, ", ", simplify = TRUE)[, 1]]
matches[date <= as.Date("2008-05-10"), round := match]
matches[date <= as.Date("2008-05-10"), match := results]
matches[, home_team := stringr::str_split(match, " - ", simplify = TRUE)[, 1]]
matches[, away_team := stringr::str_split(match, " - ", simplify = TRUE)[, 2]]
matches[, away_team := stringr::str_remove_all(away_team, "[[:punct:]]+")]
matches[, away_team := stringr::str_remove_all(away_team, "[[:digit:]]+")]
matches <- matches[away_team != ""]
matches[is.na(results), results := match]
matches[X6 == "Details", results := X5]
matches[, results := stringr::str_extract_all(results, "\\d+")]
goals_columns <- c("home_goals",
                   "away_goals",
                   "home_goals_first_half",
                   "away_goals_first_half")
matches[, home_goals := unlist(results)[1], by = index]
matches[, away_goals := unlist(results)[2], by = index]
matches[, home_goals_first_half := unlist(results)[3], by = index]
matches[, away_goals_first_half := unlist(results)[4], by = index]
matches[, (goals_columns) := lapply(.SD, as.numeric), .SDcols = goals_columns]
matches[, c("results", "match", paste0("X", 5:10)) := NULL]

#' Get details from match individual pages ----
setnames(urls, "url", "link")
urls <- urls[link %like% "www.bscyb.ch/news"]

get_match_info <- function(link) {
  page <- xml2::read_html(link)
  text <- rvest::html_text(page)
  return(text)
}

urls[, text := pblapply(link, get_match_info)]
urls <- urls[text %like% "Zuschauer"]
urls[, index := .I]
urls[, text := stringr::str_squish(text)]

#' Parse match date after first html5lightbox
urls[, date := str_split(text, "html5lightbox", simplify = TRUE)[, 2]]
urls[, date := str_split(date, " - ", simplify = TRUE)[, 1]]
urls[, date := str_split(date, " SPIELBERICHT", simplify = TRUE)[, 1]]
urls[, date := stringr::str_remove_all(date, "[\r\n\t]")]
urls[, date := stringr::str_remove_all(date, "^.[^[:punct:]]+")]
urls[, date := parsedate::parse_date(date)]
urls[, date := as.Date(date)]

#' Some pages provide numbers at the end
urls[, data_at_bottom := text %like% "Zuschauer: "]
urls[data_at_bottom == TRUE, text := stringr::str_split(text, "Zuschauer: ", simplify = TRUE)[, 2]]
urls[, text := stringr::str_remove_all(text, "[[:punct:]]+")]

#' Extract spectator numbers either from start or end
urls[data_at_bottom == FALSE, text := stringr::str_split(text, "Zuschauer", simplify = TRUE)[, 1]]
urls[, spectators := stringr::str_extract_all(text, "\\d+")]
urls[data_at_bottom == FALSE, spectators := data.table::last(unlist(spectators)), by = index]
urls[data_at_bottom == TRUE, spectators := data.table::first(unlist(spectators)), by = index]
urls[, c("index", "text", "data_at_bottom") := NULL]
urls[, spectators := as.numeric(spectators)]

#' Merge on match date. Remove missing values.
matches[urls, on = .(date), names(urls) := mget(names(urls))]

fwrite(matches, "raw_data/bscyb.csv")