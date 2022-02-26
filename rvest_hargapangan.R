library(rvest)  # https://rvest.tidyverse.org/articles/rvest.html
library(stringr) # https://stringr.tidyverse.org/
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# step 1: Read the whole html
html <- read_html("https://hargapangan.id/tabel-harga/")

# step 2: get the list of commodities
id <- html %>% 
  html_elements(xpath='//*[@id="filter_commodity_ids"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()    # library(stringr)

label <- html %>% 
  html_elements(xpath='//*[@id="filter_commodity_ids"]/option') %>% 
  html_text() %>% 
  str_trim()

# commodities <- as.data.frame(list(idkom=idkom, label=idlabel))
commodities <- data.frame(id = id, label = label)

# step 3: get the list of provinces
id <- html %>% 
  html_elements(xpath='//*[@id="filter_province_ids"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="filter_province_ids"]/option') %>% 
  html_text() %>% 
  str_trim()

# provinces <- as.data.frame(list(id=id, label=label))
provinces <- data.frame(id = id, label = label)

# step 4: get the list of regencies
# bit challenging coz it involves lazy loading of regencies based on selected provinces. 
# explore the page to see the method that is used to obtain the data. 
# then, iterate to get the context using API. 

regencies = NULL

for (provid in provinces$id) {
  # get the kabupaten list
  resp <- GET(paste0(
    "https://hargapangan.id/?option=com_gtpihps&task=stats_province.loadRegencies&filter_province_ids%5B%5D=", provid, "&price_type_id=1"
  )) %>% 
    content("text")
  
  # the above resp is not valid html (without html, body, and select tags), we need to concat it
  resp <- paste0("<html><body><select id='kabupaten'>", resp, "</select></body></html>")
  
  # scrape the kabupaten id
  tmp_regencies_id <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="kabupaten"]/option') %>% 
    html_attr("value") %>% 
    str_trim()
  
  # scrape the kabupaten label
  tmp_regencies_label <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="kabupaten"]/option') %>% 
    html_text() %>% 
    str_trim()
  
  tmp_regencies <- data.frame(province = provid, id = tmp_regencies_id, label = tmp_regencies_label)
  
  if (is.null(regencies)) regencies = tmp_regencies
  else regencies = rbind(regencies, tmp_regencies)
}

# step 5: get the list of markets
# same technique as step 4

markets = NULL

for (regid in regencies$id) {
  resp <- GET(paste0(
    "https://hargapangan.id/?option=com_gtpihps&task=stats_province.loadMarkets&filter_regency_ids%5B%5D=", regid, "&price_type_id=1"
  )) %>% 
    content("text")
  
  # -- coz resp from above is not valid html (without html, body, and select tags), we need to concat it
  resp <- paste0("<html><body><select id='market'>", resp, "</select></body></html>")
  
  tmp_markets_id <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="market"]/option') %>% 
    html_attr("value") %>% 
    str_trim()
  
  tmp_markets_label <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="market"]/option') %>% 
    html_text() %>% 
    str_trim()
  
  reg <- regencies %>% dplyr::filter(id == regid)
  
  tmp_markets <- data.frame(id = tmp_markets_id, label = tmp_markets_label)
  if (nrow(tmp_markets)) { # set province or regency only if df is not empty
    tmp_markets$province = reg$province
    tmp_markets$regency = regid
  }
  
  if (is.null(markets)) markets = tmp_markets
  else markets = rbind(markets, tmp_markets)
}

# step 6: get the commodity prices
# take a look at the POST request sent when clicking "Lihat Laporan"

commodity_prices <- NULL
for (prov in provinces$id) {
  for (reg in regencies %>% filter(province == prov) %>% .$id) {
    resp <- POST(
      "https://hargapangan.id/tabel-harga/pasar-tradisional/daerah", 
      content_type("application/x-www-form-urlencoded"), 
      accept("text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"),
      body = paste0(list(
        "task=", 
        "filter_commodity_ids%5B%5D=0", 
        "filter_regency_ids%5B%5D=0", 
        "filter_province_ids%5B%5D=0", 
        "filter_market_ids%5B%5D=0", 
        "filter_all_commodities=0", 
        "format=html", 
        "price_type_id=1", 
        "1b5708ee366b3fcb44c12566d8112508=1", 
        paste0("filter_province_ids%5B%5D=", prov), 
        paste0("filter_regency_ids%5B%5D=", reg), 
        "filter_layout=default", 
        "filter_start_date=01-08-2021", 
        "filter_end_date=30-10-2021"
      ), collapse = "&")
    ) %>% 
      content("text")
    
    tmp_table <- read_html(resp) %>% 
      html_element('#report')
    
    if (! is.null(tmp_table) && ! is.na(tmp_table)) {
      tmp_prices <- tmp_table %>%
        html_table(convert=FALSE) %>%   # convert=FALSE untuk mencegah konversi yang salah untuk separator ribuan
        pivot_longer(!c(No., `Komoditas (Rp)`), names_to = "tanggal", values_to = "harga")
      
      tmp_prices$harga <- gsub("\\.","", as.character(tmp_prices$harga)) # hapus dot pada separator ribuan
      tmp_prices$harga <- gsub("\\-","", as.character(tmp_prices$harga)) # hapus dash pada harga
      tmp_prices$harga <- tmp_prices$harga %>% as.numeric()
      
      tmp_prices$province <- prov
      tmp_prices$regency <- reg
      
      if (is.null(commodity_prices)) {
        commodity_prices <- tmp_prices
      } else {
        commodity_prices <- rbind(commodity_prices, tmp_prices) %>% distinct()
      }
    }
  }
}

if (!is.null(provinces)) write.csv2(provinces, 'provinces.csv')
if (!is.null(regencies)) write.csv2(regencies, 'regencies.csv')
if (!is.null(markets)) write.csv2(markets, 'markets.csv')
if (!is.null(commodities)) write.csv2(commodities, 'commodities.csv')
if (!is.null(commodity_prices)) write.csv2(commodity_prices, 'commodity_prices.csv') 