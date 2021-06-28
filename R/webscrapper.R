# Function: Get the URL suffix for each blog post ----
get_url <- function(url) {
  vec <- read_html(url) %>%
    html_elements(".u-borderLight, .gc .bv") %>%
    html_attr("href")

  vec_http <- ifelse(grepl("^http", vec), vec, paste0("https://medium.com", vec))
}

# Function: Get Page Content ----
get_page_content <- function(url) {
  page_content <- read_html(url) %>%
    html_elements("p") %>%
    html_text() %>%
    paste(., collapse = "")

  return(page_content)
}

# Function: Get Content Date ----
get_content_date <- function(url) {
  content_date <- read_html(url) %>%
    html_elements(".bx .dw") %>%
    html_text()

  return(content_date[1])
}

# Function: Create a Data Frame containing the webscraped data ----
create_df <- function(url) {

  content_date <- get_content_date(url)

  page_content <- get_page_content(url)

  page_url <- url

  df <- data.frame(content_date, page_content, page_url)

  return(df)
}
