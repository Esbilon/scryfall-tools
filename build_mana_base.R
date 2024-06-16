library(httr)
library(jsonlite)
library(tidyverse)

# NB: inequality signs do not seem to work with the API, <=, >=, <, >
# leq signs seem fine... ≤, ≥

base_url <- "https://api.scryfall.com"

id_to_types <- function(col_str, query = "t") {
  types <- str_split_1(col_str, "") %>% 
    str_replace_all(c("w" = "plains",
                      "u" = "island",
                      "b" = "swamp",
                      "r" = "mountain",
                      "g" = "forest"))
  
  paste0(query, ":", types) %>% 
    paste(collapse = " or ") %>% 
    paste0("(", ., ")")
  
}

scryfall_list <- function(query_str) {
  query_html <- query_str %>% 
    utf8::as_utf8() %>%
    RCurl::curlPercentEncode() %>% 
    str_replace_all("\\%20", "+")
  
  sf_out <- GET(url = paste0(base_url, 
                             "/cards/search?q=",
                             query_html))
  
  out_vec <- map_chr(content(sf_out)[[4]], ~.x$name)
  
  c(paste0("N = ", length(out_vec)), out_vec) %>% 
    paste(collapse = "\n") %>% 
    cat()
  
  invisible(out_vec)
}

build_mana_base <- function(
    col_str, 
    types = c("fetchland", "shockland", "surveilland", "dual", "triome")
) {
  
  land_nick <- c("bikeland", "cycleland", "bicycleland", "bounceland", "karoo", 
                 "canopyland", "canland", "checkland", "dual", "fastland", 
                 "filterland", "gainland", "painland", "scryland", "shadowland", 
                 "shockland", "storageland", "creatureland", "triland", 
                 "triome", "tangoland", "battleland")
  
  query_vec <- character(0)
  
  if ("fetchland" %in% types) {
    query_vec <- c(query_vec,
                   paste("is:fetchland", id_to_types(col_str, query = "o")))
    
    if (any(str_detect(types, "basic"))) {
      query_vec <- c(query_vec,
                     "Prismatic Vista")
    }
  }
  
  if ("snowbasic" %in% types) {
    query_vec <- c(query_vec,
                   paste("t:snow t:basic", id_to_types(col_str, query = "t")))
  }
  
  if ("basic" %in% types) {
    query_vec <- c(query_vec,
                   paste("-t:snow t:basic", id_to_types(col_str, query = "t")))
  }
  
  if ("surveilland" %in% types) {
    query_vec <- c(query_vec,
                   paste0("o:surveil t:land e:mkm id:", col_str))
  }
  
  if ("channelland" %in% types) {
    query_vec <- c(query_vec,
                   paste0("t:land kw:channel id:", col_str))
  }
  
  query_vec <- c(query_vec,
                 map_chr(intersect(types, land_nick), ~paste0("is:", .x)) %>% 
                   paste(collapse = " or ") %>% 
                   paste0("(", ., ") id:", col_str))
  
  query_str <- paste0("(", paste(query_vec, collapse = ") or ("), ")")
  
  scryfall_list(query_str)
}

build_mana_base("guw", 
                types = c("fetchland", "shockland", "surveilland", "dual", 
                          "triome", "fastland", "canopyland", "snowbasic",
                          "channelland"))

scryfall_list("t:land t:artifact -is:dfc")
scryfall_list("t:land t:artifact -o:'~ enters the battlefield tapped.' -is:dfc")
