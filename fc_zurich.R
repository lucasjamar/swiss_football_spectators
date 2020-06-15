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
urls <- paste0("https://www.fcz.ch/de/profis/spielplan/?season=", 1:4, "#schedule")
pages <- lapply(urls, xml2::read_html)
matches <- lapply(pages, rvest::html_table)
matches <- lapply(matches, data.table::as.data.table)
matches <- data.table::rbindlist(matches)
setnames(matches, names(matches), c("match_start", "league", "match_number", "match", "results", "link"))
matches[, link := NULL]

#' Parse information from data
matches[, home_team := stringr::str_split(match, " – ", simplify = TRUE)[, 1]]
matches[, away_team := stringr::str_split(match, " – ", simplify = TRUE)[, 2]]
matches[, home_goals := stringr::str_extract_all(results, "\\d+", simplify = TRUE)[, 1]]
matches[, away_goals := stringr::str_extract_all(results, "\\d+", simplify = TRUE)[, 2]]
matches[, home_goals := as.numeric(home_goals)]
matches[, away_goals := as.numeric(away_goals)]
matches <- matches[home_team == "FC Zürich"]
matches[, match_start := parsedate::parse_date(match_start)]
matches[, date := as.Date(match_start)]

#' Get the links of individual matches
urls <- lapply(pages, rvest::html_nodes, "a")
urls <- lapply(urls, rvest::html_attr, "href")
urls <- lapply(urls, data.table::as.data.table)
urls <- rbindlist(urls)
setnames(urls, "V1", "link")
urls <- urls[link %like% "/de/profis/news/20"]
urls[, link := paste0("https://www.fcz.ch", link)]

get_match_info <- function(link) {
  page <- xml2::read_html(link)
  text <- rvest::html_text(page)
  return(text)
}

#' Keep only pages specifying letzigrund
urls[, text := pblapply(link, get_match_info)]
urls <- urls[text %like% "Letzigrund, Zürich"]
urls[, index := .I]
urls[, title := stringr::str_split(text, " - ", simplify = TRUE)[, 1]]
urls[, text := stringr::str_squish(text)]

#' Title repeats itself twice in text. Take text occurring after second title
urls[, text := stringr::str_split(text, title, simplify = TRUE)[, 3]]

#' Match date is in next 20 words
urls[, date := stringr::word(text, 1, 20, sep = " ")]
urls[, date := parsedate::parse_date(date)]
urls[, date := as.Date(date)]

#' Get the text before last occurence of "Zuschauer"
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
matches <- matches[!is.na(spectators)]

#' Write to file
fwrite(matches, "matches.csv")