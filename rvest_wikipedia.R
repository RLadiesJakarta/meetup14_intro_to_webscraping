library(rvest)

page <- read_html("https://id.wikipedia.org/wiki/Halaman_Utama") 
today_in_history <- page %>% html_element("#mf-hids") %>% html_text()
  