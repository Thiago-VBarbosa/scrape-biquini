get_pages <- function(table, diretorio){
  
  for(i in table$link){
    httr::GET(i,
              httr::write_disk(
                file.path(
                  diretorio,
                  paste0(stringr::str_replace_all(table$nome[table$link == i],"/","-"),
                         ".html")
                  )
                )
              )
  }
}