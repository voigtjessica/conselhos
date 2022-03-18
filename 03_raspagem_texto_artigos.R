# Raspagem texto artigos:

# Agora que eu tenho todos os links que levam a cada texto, eu vou raspar os textos em si =D

setwd("C:\\Users\\yvfg3118\\Documents\\R Scripts\\tentativa6")

library(dplyr)
library(rscielo)
library(data.table)
library(rvest)
library(stringr)
library(tidyr)

load("02_tabela_link_artigos.Rdata")

# 63.868 artigos em portugues
# Aqui entraram artigos mas também entraram sessoes das revistas como por exemplo erratas. 
# Nao da para excluir de antemao o que será raspado.

lista_artigos <- list()

log_raspagem_texto_artigos <- tabela_link_artigos %>%
  mutate(status = NA)

# loop apenas para os artigos em português_ log_tabela_link_artigos_pt

#

for(i in 1:nrow(tabela_link_artigos)){
  print(c(i,tabela_link_artigos[i,4]))
  
  tryCatch({
    
    a <- tabela_link_artigos[i,4]
    webpagea <- read_html(a)
    
    article_text <- html_nodes(webpagea, xpath='//*[@id="articleText"]') %>%
      html_text(trim=TRUE)
    
    article_title <- html_nodes(webpagea, xpath='//*[@id="standalonearticle"]/section/div/div/h1')
    
    article_text <- str_c( article_text ,collapse=' ')
    
    
    article_title <- html_nodes(webpagea, xpath='//*[@id="standalonearticle"]/section/div/div/h1/text()') %>%
      html_text(trim=TRUE)
    
    lista_artigos[[i]] <- list(tabela_link_artigos$titulo[i],
                               tabela_link_artigos$ano[i],
                               tabela_link_artigos$link_edicao[i],
                               tabela_link_artigos$link_artigo[i],
                               article_text,
                               article_title)
    
    log_raspagem_texto_artigos[i,5] <- "OK"
    
  }, 
  error=function(e){} )
  #funcao erro que nao faz nada
  
  Sys.sleep(0.5)  
}

# RODAR!!!!"!!!!!!!!!!!!!!!!!

save(lista_artigos, file="03_lista_artigos_raw.Rdata")
save(log_raspagem_texto_artigos, file="03_log_raspagem_texto_artigos.Rdata")

# Alguns artigos que nnao estao em portugues passaram na raspagem. Vou retiralos de acordo com a posicao
# na tabela_link_artihgos, que é tb a posicao na lista.


# selecionando os artigos em ingles
art_ingles <- tabela_link_artigos %>%
  mutate(language = str_sub(link_artigo,-2),
         num = 1:n()) %>%
  filter(language != "br") %>%
  filter(language != "pt") 
  
  
art_ingles <- art_ingles$num

#criando uma lista de artigos em portugues, que retira os elementos em outras linguas
artigos_pt <- lista_artigos
artigos_pt <- artigos_pt[-c(art_ingles)]


save(artigos_pt, file="03_lista_artigos_pt.Rdata")

x <- artigos_pt[5]

#############################
# Vendo quantos caracteres tem cada artigo:

tamanho_artigos <- data.frame(posicao = NA,
                              qtde_palavras = NA)


for(i in 1:length(artigos_pt)){
  print(i)  
  
  tryCatch(
    {
      posicao = i
      a <- artigos_pt[[i]][5]
      a <- gsub("\r?\n|\r", " ", a)
      a <- gsub("\\s{2,}", " ", a)
      qtde_palavras = str_count(a, "\\w+")
      
      ta <- data.frame(posicao, qtde_palavras)
      tamanho_artigos <- rbind(tamanho_artigos, ta)
    },
    error=function(cond) {
      posicao = i
      qtde_palavras = NA
      ta <- data.frame(posicao, qtde_palavras)
      tamanho_artigos <- rbind(tamanho_artigos, ta)
    })
  }


verificacao <- c(1:length(artigos_pt))
tam <- tamanho_artigos$posicao
x <- setdiff(verificacao, tam)   #todos dentro de X sao textos vazios

#construir objeto com  aqueles que ele disse que tinham menos de 2000 palavras

not_artigos <- tamanho_artigos %>%
  filter(qtde_palavras < 2000) # 11206

not_artigos <- not_artigos$posicao
not_artigos <- c(not_artigos,x ) #11236

# Agora vou cirar uma lista final, com os artigos com mais de 2000 palavras em port!

lista_artigos_pt_verificados <- artigos_pt
lista_artigos_pt_verificados <- lista_artigos_pt_verificados[-c(not_artigos)] # 54802 artigos

save(lista_artigos_pt_verificados, file="03_lista_artigos_pt_verificados.Rdata")
