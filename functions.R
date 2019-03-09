# JS web scraping functions are modified code from a Medium Post by Alex Pavlakis
# https://medium.com/@alex.pavlakis/scraping-and-analyzing-vanguard-etfs-with-r-83bde4b3f410

################################
## START JAVA SCRAPE FUNCTION ##
################################


jsScrape <- function(url, 
                     js_path = "scrape.js", 
                     phantompath = "/usr/local/bin/phantomjs") {
  
  # Javascript for creating a new file, scrape.js
  writeLines(
    "var url = NULL;
    var page = new WebPage();
    var fs = require('fs');
    page.open(url, function (status) {
    just_wait();
    });
    function just_wait() {
    setTimeout(function() {
    fs.write('1.html', page.content, 'w');
    phantom.exit();
    }, 2500);
    }
    ",
    con = "scrape.js"
  )
  
  # Replace url in scrape.js
  lines <- readLines(js_path)
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, js_path)
  
  # Run from command line
  command <- paste(phantompath, js_path, sep = " ")
  system(command)
  
}


##############################
## END JAVA SCRAPE FUNCTION ##
##############################


###############################
## START PARSE DATA FUNCTION ##
###############################


parseHTML <- function() {
  
  require(tidyverse)
  require(rvest)
  
  # Read html
  html <- read_html("1.html")
  
  # Extract fund names
  fund_names <- html %>%
    html_nodes(".productName a") %>%
    html_text()
  
  # Extract tickers
  ticker <- html %>%
    html_nodes(".fixedCol+ .ng-binding") %>%
    html_text()
  
  # Extract asset classes
  asset_class <- html %>%
    html_nodes(".assetClass") %>%
    html_text() %>%
    sapply(., function(x) x[!str_detect(x, "\t")]) %>%
    unlist()
  
  # Extract expense ratios
  expense_ratio <- html %>%
    html_nodes(".expenseRatio") %>%
    html_text()  %>%
    sub("%", "", .) %>%
    as.numeric() %>%
    sapply(., '/', 100) %>%
    sapply(., function(x) x[!is.na(x)]) %>%
    unlist()
  
  # Extract price
  price <- html %>%
    html_nodes(".expenseRatio+ .ng-binding") %>%
    html_text()  %>%
    sub("[$]", "", .) %>%
    as.numeric()
  
  # Extract sec yield
  sec_yield <- html %>%
    html_nodes(".secYield") %>%
    html_text()
  sec_yield_clean <- sec_yield[!str_detect(sec_yield, "SEC")] %>%
    str_replace_all(., "\n", "") %>%
    str_replace_all(., "\t", "") %>%
    str_replace_all(., "â\u0080\u0094", NA_character_)
  
  # Extract ytd returns
  ytd <- html %>%
    html_nodes(".secYield+ .ng-binding") %>%
    html_text() %>%
    sub("%", "", .) %>%
    as.numeric() %>%
    sapply(., '/', 100)
  
  # Extract one yr returns
  one_yr <- html %>%
    html_nodes(".ng-binding:nth-child(11)") %>%
    html_text() %>%
    str_replace_all(., "â\u0080\u0094", NA_character_) %>%
    sub("%", "", .) %>%
    as.numeric() %>%
    sapply(., '/', 100)
  
  # Extract five yr returns
  five_yr <- html %>%
    html_nodes(".ng-binding:nth-child(12)") %>%
    html_text() %>%
    str_replace_all(., "â\u0080\u0094", NA_character_) %>%
    sub("%", "", .) %>%
    as.numeric() %>%
    sapply(., '/', 100)
  
  # Extract ten yr yields
  ten_yr <- html %>%
    html_nodes(".ng-binding:nth-child(13)") %>%
    html_text() %>%
    str_replace_all(., "â\u0080\u0094", NA_character_) %>%
    sub("%", "", .) %>%
    as.numeric() %>%
    sapply(., '/', 100)
  
  # Extract yield since inception
  since <- html %>%
    html_nodes(".right:nth-child(14)") %>%
    html_text() %>%
    str_replace_all(., "\n", "") %>%
    str_replace_all(., "\t", "") %>%
    str_split(., "[(]") %>%
    lapply(., head, 1) %>%
    unlist() %>%
    sub("%", "", .) %>%
    as.numeric() %>%
    sapply(., '/', 100)
  
  # Extract date of inception
  inception <-  html %>%
    html_nodes(".right:nth-child(14)") %>%
    html_text() %>%
    str_replace_all(., "\n", "") %>%
    str_replace_all(., "\t", "") %>%
    str_split(., "[(]") %>%
    lapply(., tail, 1) %>%
    str_replace_all(., "[)]", "") %>%
    unlist() %>%
    as.Date(., "%m/%d/%Y")
  
  # Combine into one data frame
  fund_data <- data.frame(fund_names, ticker, asset_class,
                          expense_ratio, price, sec_yield_clean, 
                          ytd, one_yr, five_yr, ten_yr, since, inception,
                          stringsAsFactors = FALSE)
  
  # Drop duplicate rows
  fund_data <- fund_data %>%
    distinct()
  
  return(fund_data)
  
}


#############################
## END PARSE DATA FUNCTION ##
#############################