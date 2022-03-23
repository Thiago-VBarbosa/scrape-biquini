library(magrittr)
source("functions/get_shows.R")

#obtendo a tabela de shows e gerando nomes em formato adequado
shows <- get_shows() %>% 
  janitor::clean_names()

dplyr::glimpse(shows)

# Rows: 2,309
# Columns: 4
# $ data_do_show  <chr> "18/04/2020", "14/03/2020", "13/03/2020", "24/02/2020", "14/02/2020", "09/02/2020",...
# $ local         <chr> "Divinópolis Clube - Sede Campestre", "Prime Show", "Deck Music", "Corredor da Foli...
# $ cidade_estado <chr> "Divinópolis/MG", "Caratinga/MG", "Ipatinga/MG", "Cajazeiras/PB", "Guarujá/SP", "Sã...
# $ pais          <chr> "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Br...


#tratando variáveis
shows <- shows %>% 
  dplyr::mutate(data_do_show = as.Date(data_do_show, format = "%d/%m/%Y")) %>% 
  tidyr::separate(cidade_estado, c("cidade", "estado"), "/") %>% 
  dplyr::mutate(estado = toupper(estado)) 

dplyr::glimpse(shows)

# Rows: 2,309
# Columns: 5
# $ data_do_show <date> 2020-04-18, 2020-03-14, 2020-03-13, 2020-02-24, 2020-02-14, 2020-02-09, 2020-02-08,...
# $ local        <chr> "Divinópolis Clube - Sede Campestre", "Prime Show", "Deck Music", "Corredor da Folia...
# $ cidade       <chr> "Divinópolis", "Caratinga", "Ipatinga", "Cajazeiras", "Guarujá", "São João da Barra"...
# $ estado       <chr> "MG", "MG", "MG", "PB", "SP", "RJ", "SP", "RJ", "MG", "RJ", "RJ", "MG", "MG", "MG", ...
# $ pais         <chr> "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Brasil", "Bra...

#verificando shows fora do Brasil
shows %>% 
  dplyr::filter(pais != "Brasil") %>% 
  dplyr::group_by(pais) %>%
  dplyr::count()

# A tibble: 3 x 2
# Groups:   pais [3]
# pais                 n
# <chr>              <int>
# 1 Dinamarca          1
# 2 Estados Unidos     7
# 3 Portugal           3

#gerando diretorio para salvar alguns gráficos
dir.create("output")

png("output/qtde_shows_por_estado.png",
    width = 1060,height = 664)

#vizualizando a quantidade de apresentações realizadas no Brasil por estado
shows %>% 
  dplyr::filter(pais == "Brasil") %>% 
  #dplyr::filter(!(estado %in% c("SP", "RJ", "MG"))) %>% 
  dplyr::group_by(estado) %>%
  dplyr::count() %>% 
  dplyr::arrange(n) %>% 
  ggplot2::ggplot(ggplot2::aes(n, reorder(estado, n))) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "Quantidade", y = "Estados", title = "Distriuição dos Shows do Biquini por Estado - Brasil")

dev.off()

#verificando o histórico de apresentações do Biquini-Cavadão em Mato Grosso
shows %>% 
  dplyr::filter(estado == "MT")
# A tibble: 9 x 5
# data_do_show    local                          cidade       estado    pais  
# <date>          <chr>                          <chr>        <chr>     <chr> 
# 1 2016-10-28   Musiva                         Cuiabá          MT     Brasil
# 2 2015-05-30   Musiva                         Cuiabá          MT     Brasil
# 3 2014-02-21   Centro de Eventos do Pantanal  Cuiabá          MT     Brasil
# 4 2009-05-23   Show                           Cuiabá          MT     Brasil
# 5 2008-07-12   Show                           Barra do Garças MT     Brasil
# 6 2007-09-22   Show                           Cuiabá          MT     Brasil
# 7 2004-12-03   Show                           Cuiabá          MT     Brasil
# 8 1992-10-17   Show                           Cuiabá          MT     Brasil
# 9 1987-07-04   Show                           Cuiabá          MT     Brasil

png("output/qtde_shows_por_cidade_MT.png",
    width = 620,height = 481)

shows %>% 
  dplyr::filter(estado == "MT") %>% 
  dplyr::group_by(cidade) %>% 
  ggplot2::ggplot(ggplot2::aes(cidade)) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Cidade", y = "Quantidade", title = "Histórico de Shows em Mato Grosso por Cidade")

dev.off()

#obtendo os nomes e links das informações dos albuns do Biquini
source("functions/get_albuns.R")
albuns <- get_albuns()

#obtendo informações sobre os albuns do Biquini
source("functions/get_attr_albuns.R")
atrib_albuns <- get_attr_albuns(albuns$link)

dplyr::glimpse(atrib_albuns)

# Rows: 19
# Columns: 4
# $ link          <chr> "https://biquini.com.br/discos/ilustre-guerreiro/", "https://biquini.com.br/discos/...
# $ ano           <chr> "2018", "2017", "2014", "2013", "2008", "2007", "2007", "2007", "2005", "2001", "20...
# $ produzido_por <chr> "Liminha", "Liminha", "Coelho e Marcelo Magal", "Carlos Coelho e Marcelo Magal", "M...
# $ gravadora     <chr> "Independente", "Coqueiro Verde", "Sony Music", "Warner", "RWR (SomLivre)", "Indepe...


#gerando data.frame com as informações sobre os albuns
albuns_info <- dplyr::left_join(albuns, atrib_albuns, "link") %>% 
  dplyr::rename(nome = name) %>% 
  dplyr::relocate(link, .after = gravadora)

dplyr::glimpse(albuns_info)

# Rows: 19
# Columns: 5
# $ nome          <chr> "Ilustre Guerreiro", "As Voltas Que O Mundo Dá", "Me Leve Sem Destino", "Roda Gigan...
# $ ano           <chr> "2018", "2017", "2014", "2013", "2008", "2007", "2007", "2007", "2005", "2001", "20...
# $ produzido_por <chr> "Liminha", "Liminha", "Coelho e Marcelo Magal", "Carlos Coelho e Marcelo Magal", "M...
# $ gravadora     <chr> "Independente", "Coqueiro Verde", "Sony Music", "Warner", "RWR (SomLivre)", "Indepe...
# $ link          <chr> "https://biquini.com.br/discos/ilustre-guerreiro/", "https://biquini.com.br/discos/...


#verificando os albuns por gravadora e por produção
albuns_info %>% 
  dplyr::group_by(gravadora, produzido_por) %>% 
  dplyr::count()

# A tibble: 13 x 3
# Groups:   gravadora, produzido_por [13]
# gravadora           produzido_por                               n
# <chr>               <chr>                                   <int>
# 1 Biquini Cavadão   Tadeu Patolla                               1
# 2 BMG               Carlos Beni, Paul Ralphes e Tuta Aquino     1
# 3 BMG               Paul Ralphes                                1
# 4 Coqueiro Verde    Liminha                                     1
# 5 Deckdisc          Tadeu Patolla                               1
# 6 Epic/Sony Music   Carlos Beni                                 1
# 7 Independente      Liminha                                     1
# 8 Independente      Tadeu Patolla                               2
# 9 Polygram          Carlos Beni                                 6
# 10 RWR (SomLivre)   Marcelo Cortez                              1
# 11 Sony Music       Coelho e Marcelo Magal                      1
# 12 Universal        Tadeu Patolla                               1
# 13 Warner           Carlos Coelho e Marcelo Magal               1

png("output/qtde_albuns_por_gravadora.png",
    width = 1060,height = 664)

albuns_info %>% 
  dplyr::group_by(gravadora) %>% 
  ggplot2::ggplot(ggplot2::aes(gravadora)) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = "Gravadora", y = "Quantidade", title = "Quantidade de albuns por Gravadora")

dev.off()

#exportando dados extraidos
dir.create("data")
saveRDS(object = shows, file = "data/shows.rds")
saveRDS(object = albuns_info, file = "data/albuns_info.rds")


#salvando arquivos html com as letras em diretório temporário
source("functions/get_pages.R")
get_pages(albuns_info, tempdir())


#extraindo o conteudo das letras das musicas com seus respectivos nomes e albuns 
source("functions/get_lyrics.R")

letras_list <- list()
for(l in list.files(tempdir(), ".html$")){
  
  letras_list[[stringr::str_remove_all(l, ".html") %>%
                 stringr::str_replace("-","/")]] <- get_lyrics(file.path(tempdir(), l))
}

#O album "Descivilização" não possui conteúdo estruturado, somente um PDF escaneado. Atribuindo um
#um conteúdo nulo para a letra para gerar um data.frame estruturado
letras_list[["Descivilização"]]$letra <- list(
  xml2::xml_new_document() %>%
    xml2::xml_add_child("p") %>%
    xml2::xml_find_all("//p") %>%
    xml2::xml_set_text(" ")
)

#Gerando data.frame com as letras das musicas
lyrics <- data.frame(album = NULL, musica = NULL, letra = NULL, stringsAsFactors = FALSE)

for(i in 1:length(names(letras_list))){
  
  dados_parciais <- data.frame(album = names(letras_list)[i],
                               
                               musica = letras_list[[names(letras_list)[i]]]$nome,
                               
                               letra = lapply(letras_list[[names(letras_list)[i]]]$letra,
                                              FUN = xml2::xml_contents) %>% 
                                 lapply(FUN = glue::glue_collapse) %>%
                                 unlist(),
                               
                               stringsAsFactors = FALSE)
  
  lyrics <- dplyr::bind_rows(lyrics, dados_parciais)
}

dplyr::glimpse(lyrics)

# Rows: 223
# Columns: 3
# $ album  <chr> "1985/2007 vol.1 – Sucessos Regravados", "1985/2007 vol.1 – Sucessos Regravados", "1985/20...
# $ musica <chr> "Zé Ninguém (2007)", "Tédio (2007)", "Teoria (2007)", "Vou Te Levar Comigo (2007)", "Você ...
# $ letra  <chr> "\n                                                                        <p>Quem foi que...

saveRDS(lyrics, file = "data/lyrics.rds")

rm(albuns, atrib_albuns, albuns_info, shows, lyrics, letras_list, dados_parciais)


