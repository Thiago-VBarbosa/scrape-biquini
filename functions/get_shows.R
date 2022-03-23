get_shows <- function(){
  `%>%` <- magrittr::`%>%`
  page1 <- httr::GET("https://biquini.com.br/na-estrada/")
  
  shows <- xml2::xml_find_all(xml2::read_html(page1), "//table[@id = 'tabela-shows-2']") %>%
    rvest::html_table() %>% 
    as.data.frame() %>% 
    tibble::tibble()
  shows
}