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

# Get match data for 4 seasons
urls <- paste0("https://www.bscyb.ch/archiv-", 2017:2018, "-")
urls <- paste0(urls, 18:19)

pages <- lapply(urls, xml2::read_html)
matches <- lapply(pages, rvest::html_table, fill = TRUE)
matches <- lapply(matches, data.table::first)
matches <- lapply(matches, data.table::as.data.table)
matches <- data.table::rbindlist(matches, fill = TRUE)
matches <- matches[, 1:4]
setnames(matches, names(matches), c("date", "league", "match", "results"))

# Parse information from data ----
matches[, index := .I]
matches[, date := parsedate::parse_date(date)]
matches[, date := as.Date(date)]
matches[is.na(match), match := stringr::str_split(league, ", ", simplify = TRUE)[, 2]]
matches[, league := stringr::str_split(league, ", ", simplify = TRUE)[, 1]]
matches[, home_team := stringr::str_split(match, " - ", simplify = TRUE)[, 1]]
matches[, away_team := stringr::str_split(match, " - ", simplify = TRUE)[, 2]]
matches[is.na(results), results := away_team]
matches[, away_team := stringr::str_remove_all(away_team, "[[:punct:]]+")]
matches[, away_team := stringr::str_remove_all(away_team, "[[:digit:]]+")]
matches <- matches[away_team != ""]
matches[, results := stringr::str_extract_all(results, "\\d+")]
matches[, home_goals := unlist(results)[1], by = index]
matches[, away_goals := unlist(results)[2], by = index]
matches[, home_goals := as.numeric(home_goals)]
matches[, away_goals := as.numeric(away_goals)]

# Get details from match individual pages ----
urls <- lapply(pages, rvest::html_nodes, "a")
urls <- lapply(urls, rvest::html_attr, "href")
urls <- lapply(urls, data.table::as.data.table)
urls <- rbindlist(urls)
setnames(urls, "V1", "link")
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

# Parse match date after first html5lightbox
urls[, date := str_split(text, "html5lightbox", simplify = TRUE)[, 2]]
urls[, date := str_split(date, " - ", simplify = TRUE)[, 1]]
urls[, date := parsedate::parse_date(date)]
urls[, date := as.Date(date)]

# Some pages provide numbers at the end
urls[, data_at_bottom := text %like% "Zuschauer: "]
urls[data_at_bottom == TRUE, text := stringr::str_split(text, "Zuschauer: ", simplify = TRUE)[, 2]]
urls[, text := stringr::str_remove_all(text, "[[:punct:]]+")]

# Extract spectator numbers either from start or end
urls[data_at_bottom == FALSE, text := stringr::str_split(text, "Zuschauer", simplify = TRUE)[, 1]]
urls[, spectators := stringr::str_extract_all(text, "\\d+")]
urls[data_at_bottom == FALSE, spectators := data.table::last(unlist(spectators)), by = index]
urls[data_at_bottom == TRUE, spectators := data.table::first(unlist(spectators)), by = index]
urls[, c("index", "text") := NULL]
urls[, spectators := as.numeric(spectators)]

# Merge on match date. Remove missing values.
matches[urls, on = .(date), names(urls) := mget(names(urls))]
matches <- matches[!is.na(spectators)]
fwrite(matches, "matches.csv")