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

urls <- paste0("https://www.fcb.ch/de-CH/Saison/Saisonspiel?m=",424:4050)
urls <- data.table::data.table(link = urls)

get_info <- function(link){
  page <- xml2::read_html(link)
  league <- html_nodes(page, css =".widget-gdt-contest-info-text > span:nth-child(1)")
  match_day <- html_nodes(page, css =".widget-gdt-contest-info-text > span:nth-child(3)")
  stadium <- html_nodes(page, css =".widget-gdt-info-stadium")
  home_team <- html_nodes(page, css = ".widget-gdt-team-home")
  away_team <- html_nodes(page, css = ".widget-gdt-team-guest")
  home_goals <- html_nodes(page, css = ".widget-gdt-score-home")
  away_goals <- html_nodes(page, css = ".widget-gdt-score-guest")
  match_start <- html_nodes(page, css = ".widget-gdt-info-date")
  spectators <- html_node(page, css = "span.widget-gdt-matchinfo-data-audience:nth-child(2)")
  return(text)
}

pages <- pblapply(urls[1,link], get_info)
setnames(matches, names(matches), c("date", "league", "match", "results"))