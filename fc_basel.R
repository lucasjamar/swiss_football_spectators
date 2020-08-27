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
library(xml2)
library(rvest)
library(parsedate)

urls <- paste0("https://www.fcb.ch/de-CH/Saison/Saisonspiel?m=",424:4180)

columns <- c(
  "league",
  "match_day",
  "stadium",
  "home_team",
  "away_team",
  "home_goals",
  "away_goals",
  "match_start",
  "spectators" 
)

nodes <- c(
  ".widget-gdt-contest-info-text > span:nth-child(1)",
  ".widget-gdt-contest-info-text > span:nth-child(3)",
  ".widget-gdt-info-stadium",
  ".widget-gdt-team-home",
  ".widget-gdt-team-guest",
  ".widget-gdt-score-home",
  ".widget-gdt-score-guest",
  ".widget-gdt-info-date",
  "span.widget-gdt-matchinfo-data-audience:nth-child(2)"
)

#' Get details from match individual pages ----
get_match_info <- function(node, page){
  text <- html_nodes(page, css = node)
  text <- rvest::html_text(text)
  return(text)
}

matches <- data.table::data.table()

for (url in urls) {
  page <- xml2::read_html(url)
  match <- data.table()
  match[, (columns) := lapply(nodes, get_match_info, page)]
  matches <- rbindlist(list(matches, match), fill = TRUE)
}

matches[, url := urls]

fwrite(matches, "raw_data/fc_basel.csv")