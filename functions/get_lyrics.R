get_lyrics <- function(html){
  page <- html %>%
    xml2::read_html()
  
  nome <- page %>%
    xml2::xml_find_all("//ul[@id = 'release-list']/li/span[@class = 'txt']") %>%
    xml2::xml_text() %>%
    stringr::str_trim()
  
  letra <- page %>%
    xml2::xml_find_all("//ul[@id = 'release-list']/li/div/div/div[@class = 'modal-content']/div[@class = 'modal-body w-letra']/div[last()]") %>%
    as.list.data.frame()
  letra_list <- list(nome = nome, letra = letra)
  letra_list
}