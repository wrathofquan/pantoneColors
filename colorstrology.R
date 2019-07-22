library(rvest)

colorstrology <- function(i,j){
  body <- list('month' = i,'day' = j)
  url <- 'https://www.pantone.com/pages/iphone/iphone_colorstrology_results.aspx'
  page <- html_session(url) %>%
    rvest:::request_POST(url, body = body, encode = "form") %>%
    read_html()
  date <- page %>% html_node('table table td') %>% html_text() %>% 
    gsub('^\\s+|\\s+$|[\r\n\t]', '', .)
  pantone <- page %>% html_nodes('tr') %>%  `[[`(4) %>% html_text() %>%  
    stringr::str_extract("PANTONE\\s\\d\\d.*\\d\\d") 
  meta <- page %>% html_nodes('#tdBg span') %>% html_text()
  description <- page %>% html_node('tr:nth-of-type(2) div') %>% html_text() %>% 
    gsub('^\\s+|\\s+$|[\r\n\t]', '', .)
  df <- data.frame(date, pantone, meta, description)
}


# get just pantone ID
#  (PANTONE)\s\d\d.*\d\d

 



test <- colorstrology(1, 10)

months <- c(1:12)
days <- c(1:31)

df <- data.frame(date, description, meta)
for (m in months){
  for (d in days){
    temp <- colorstrology(m,d)
    df <- rbind(temp, df)
}
}


## mutate color variable
df <- df %>% 
  group_by(date) %>% 
  mutate(color = first(meta)) 






# 
# 
# ## old selenium approach
# library(RSelenium)
# library(rvest)
# library(tidyverse)
# library(xml2)
# 
# ## first run: docker run -d -p 4445:4444 selenium/standalone-chrome
# ## open a new connection to Chrome
# remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
#                                  port = 4445L,
#                                  browserName = "chrome")
# 
# remDr$open()
# url <- ("https://www.pantone.com/pages/iphone/iphone_colorstrology.html#___1__")
# remDr$navigate("https://www.pantone.com/pages/iphone/iphone_colorstrology.html#___1__") #Entering our URL gets the browser to navigate to the page
# remDr$screenshot(display = TRUE) 
# 
# 
# #### create list of month/days
# month_day<- xml2::read_html(remDr$getPageSource()[[1]])
# page_i <- month_day %>%
#   html_nodes(".list") %>%
#   html_children() %>% 
#   html_text()
# 
# months <- page_i[1:12]
# months <- (paste("'", months,"'", sep=''))
# days <- page_i[13:43]
# days <- as.numeric(days)
# 
# 
# ## create an object that has all unique month xpath values
# for (i in months){
#   elements <- paste0("//option[contains(text(),",months,")]")
# }
# 
# for (i in days){
#   elements_days <- paste0("//select[@id='lstDay']//option[",days,"]")
# }
# 
# ## attempt at loop
# 
# total <- data.frame()
# 
# 
# for (e in elements){
#   remDr$navigate("https://www.pantone.com/pages/iphone/iphone_colorstrology.html#___1__") #Entering our URL gets the browser to navigate to the page
#   print(e)
#   month <- remDr$findElement(using = 'xpath', e)
#   month$clickElement()
#   day <- remDr$findElement(using = 'xpath', "//select[@id='lstDay']//option[5]")
#   day$clickElement()
#   submit <- remDr$findElement(using = 'xpath', "/html[1]/body[1]/form[1]/div[1]/a[1]")
#   submit$clickElement()
#   html <- xml2::read_html(remDr$getPageSource()[[1]])
#   description <- html %>%  html_nodes(xpath = "//tr//tr[2]//td[1]") %>% html_text() %>% gsub("^\\s+|\\s+$", "", .)
#   meta <- html %>% html_nodes(xpath = "//td[@id='tdBg']") %>%  html_text() %>% gsub("^\\s+|\\s+$", "", .) 
#   date <- html %>% html_nodes(xpath = "//td[@id='bgHeaderDate']//div") %>%  html_text() %>% gsub("^\\s+|\\s+$", "", .)
#   df <- data.frame(cbind(description,meta,date))
#   total <- rbind(total, df)
# }
# 
# 
# 
# 
# 
# 
