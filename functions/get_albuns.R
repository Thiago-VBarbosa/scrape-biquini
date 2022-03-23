get_albuns <- function(){
  `%>%` <- magrittr::`%>%`
  link_discos <- "https://biquini.com.br/discos/"
  
  page_discos <- httr::GET(link_discos)
  
  link <- page_discos %>% 
    xml2::read_html() %>% 
    xml2::xml_find_all("//a[contains(@class, 'mix album')]") %>% 
    xml2::xml_attr("href")
  
  name <- page_discos %>% 
    xml2::read_html() %>% 
    xml2::xml_find_all("//a[contains(@class, 'mix album')]/img") %>% 
    xml2::xml_attr("title")
  
  table <- data.frame(name, link, stringsAsFactors = FALSE) %>% 
    tibble::tibble()
  table
}