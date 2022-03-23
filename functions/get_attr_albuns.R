get_attr_albuns <- function(link){
  `%>%` <- magrittr::`%>%`
  get_attr <- function(x){
    page <- httr::GET(as.character(x)) %>% 
      xml2::read_html() %>% 
      xml2::xml_find_all("//div[@class = 'album-det']") %>% 
      xml2::xml_contents()
    
    link <- x
    ano <- page %>% 
      .[4] %>% 
      xml2::xml_text() %>% 
      stringr::str_trim() #ano
    
    produzido_por <- page %>% 
      .[7] %>% 
      xml2::xml_text() %>% 
      stringr::str_trim() #produzido_por
    
    gravadora <- page %>% 
      .[10] %>% 
      xml2::xml_text() %>% 
      stringr::str_trim() #gravadora
    table <- data.frame(link, ano, produzido_por,
                        gravadora, stringsAsFactors = FALSE)
    table
  }
  result <- lapply(link, get_attr) %>% 
    do.call("rbind", .) %>% 
    tibble::tibble()
  result
}