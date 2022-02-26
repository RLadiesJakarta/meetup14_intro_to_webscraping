library(rvest)

page <- read_html("https://id.wikipedia.org/wiki/Halaman_Utama") 
today_in_history <- page %>% 
  html_elements("#mf-hids ul > li") %>% 
  html_text()

