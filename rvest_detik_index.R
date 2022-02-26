library(rvest)

# Open main page
main_page <- read_html("https://news.detik.com/indeks") 

# Find pagination element
pagination_elmts <- main_page %>% html_elements('.pagination a')
# Last page is at 2nd last element
last_page <- pagination_elmts[length(pagination_elmts)-1] %>% html_text()
last_page <- as.integer(last_page)

# data frame to collect result
columns= c("title","url","timestamp") 
articles = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(articles) = columns

# iterate from page 1 to last page
for (page in 1:last_page) {
  # Open page
  page_html <- read_html(paste0('https://news.detik.com/indeks/', page))
  # Find all news
  # news title
  article_title_text <- page_html %>% html_elements('article .media__title a.media__link') %>% html_text()
  # news link
  article_url <- page_html %>% html_elements('article .media__title a.media__link') %>% html_attr('href')
  # news timestamp
  article_timestamp <- page_html %>% html_elements('article .media__date span') %>% html_attr('d-time')
  
  page_articles <- as.data.frame(list(
    title = article_title_text, 
    url = article_url, 
    timestamp = article_timestamp
  ))
  
  # append to articles df
  articles <- rbind(articles, page_articles)
}

print(articles)

write.csv(articles, 'articles.csv')
