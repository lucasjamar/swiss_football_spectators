#' ---
#' title: "Scrap FC Basel Spectator Data"
#' author: "Lucas Jamar"
#' ---

#' setup, include=FALSE
knitr::opts_chunk$set(eval = FALSE)

#' Load libraries
library(pbapply)
library(data.table)
library(stringr)
library(RSelenium)
library(parsedate)

# 424:4186
matches <- data.table::data.table()
matches[, url := paste0("https://www.fcb.ch/de-CH/Saison/Saisonspiel?m=",424:4186)]
get_status <- function(url){
  status <- httr::GET(url, httr::timeout(15))$status
  return(status)
}
matches[, status := pblapply(url, get_status)]
matches <- matches[status == 200]
matches[, status := NULL]
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

columns <- c(
  "league",
  "stadium",
  "home_team",
  "away_team",
  "home_goals",
  "away_goals",
  "date",
  "match_start",
  "spectators" 
)

nodes <- c(
  "widget-gdt-contest-info-text",
  "widget-gdt-info-stadium",
  "widget-gdt-team-home",
  "widget-gdt-team-guest",
  "widget-gdt-score-home",
  "widget-gdt-score-guest",
  "widget-gdt-info-date",
  "widget-gdt-info-time",
  "widget-gdt-matchinfo-data-audience"
)

#' Get details from match individual pages ----
get_match_info <- function(node, page, remote_driver){
  text <- remote_driver$findElement(using = "class", value = node)
  text <- text$getElementText()
  return(text)
}

driver <- RSelenium::rsDriver(browser = "firefox")
remote_driver <- driver$client
remote_driver$open()

for (row in 1:nrow(matches)) {
  print(row/nrow(matches))
  remote_driver$navigate(url = matches[row, url])
  matches[row, (columns) := lapply(nodes, get_match_info, page, remote_driver)]
  fwrite(matches[row], "raw_data/fc_basel.csv", append = TRUE)
}
remote_driver$close()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

matches[, league := as.character(league)]
matches[, round := stringr::str_split(league,"\n", simplify = TRUE)[,2]]
matches[, league := stringr::str_split(league,"\n", simplify = TRUE)[,1]]
matches[, date := lubridate::dmy(date)]
matches[, match_start := paste(date, match_start)]
matches[, match_start := parsedate::parse_date(match_start)]
matches[, spectators := as.character(spectators)]
matches[, spectators := stringr::str_remove_all(spectators,"'")]
matches[, spectators := as.integer(spectators)]

fwrite(matches, "raw_data/fc_basel2.csv")
