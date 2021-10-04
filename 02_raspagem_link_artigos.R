#02_tabela_link_artigos2

# Objetivo aqui é conseguir o link de cada um dos artigos, raspando dos links das edicoes

setwd("C:/Users/yvfg3118/Documents/R Scripts/tentativa 5")

library(dplyr)
library(rscielo)
library(data.table)
library(rvest)
library(stringr)
library(tidyr)

#abrindo a tabela com os links de cada uma das edicoes de cada uma das revistas

load("_01_tabela_publicacoes.Rdata")     #tabela_publicacoes

tabela_publicacoes <- tabela_publicacoes %>% 
  mutate(link = gsub(" ", "", link),
         link = str_trim(link, side = c("both")))


#criando um arquivo de log para ver se algo deu errado:

log_raspagem <- tabela_publicacoes %>%
  mutate(status = NA)

tabela_link_artigos <- data.frame(titulo = NA,
                                  ano = NA,
                                  link_edicao = NA,
                                  artigos = NA)

for(i in 1:nrow(log_raspagem)){
  
  j <- tabela_publicacoes[i,3]
  
  print(c(i, j))
  
  tla <- data.frame(titulo = tabela_publicacoes[i,1],
                    ano = tabela_publicacoes[i,2],
                    link_edicao = j,
                    artigos = NA)
  
  tryCatch({webpagej <- read_html(j)
  
  articles <- html_nodes(webpagej, xpath='//*[@id="issueIndex"]/div[1]/div[2]/ul/li/ul/li/a')%>%
    html_attr("href")
  
  articles <- articles[!str_detect(articles,pattern="pdf")]
  articles <- articles[str_detect(articles,pattern="pt")]
  articles <- articles[!str_detect(articles,pattern="abstract")]
  articles <- paste0("https://www.scielo.br", articles)
  articles <- str_c( articles ,collapse=' ; ')
  
  tla[1,4] <- articles
  
  tabela_link_artigos <- rbind(tabela_link_artigos, tla)
  
  log_raspagem[i,4] <- "OK"},
  
  
  
  error=function(e){} )
  #funcao erro que nao faz nada
  
  Sys.sleep(2)
}

setwd("C:\\Users\\yvfg3118\\Documents\\R Scripts\\tentativa6")

#RODAR!!!!!!!:

save(log_raspagem, file="02_log_raspagem.Rdata")


#Vou empilhar os artigos. Primeiro vendo quantos artigos tem por periodico:

tabela_link_artigos %>%
  mutate(num_artigos = (str_count(artigos, ";") + 1)) %>%
  arrange(desc(num_artigos)) %>%
  select(num_artigos)

max(x$num_artigos)

#verificando quantos artidos devem aparecer

 y <- tabela_link_artigos %>%
   filter(!is.na(titulo)) %>%
   mutate(num_artigos = (str_count(artigos, ";") + 1))

 y <- sum(y$num_artigos) # devem aparecer o seguinte numero de artigos: 65868

 rm(y)


#criando os nomes das colunas dos links para fazer o spread:

links_var <- c()


for(u in 1:144){ 
  z <- paste0("link", u)
  links_var <- c(links_var,z)
}

#agora vou fazer o spread e o gather pra empilhar tudo

tabela_link_artigos <- tabela_link_artigos %>%
  filter(!is.na(titulo)) %>%
  separate(artigos, into=links_var, sep = " ; " ) %>%
  gather(artigo, link_artigo, links_var) %>%
  filter(!is.na(link_artigo)) %>%
  select(-c(artigo))


save(tabela_link_artigos, file="02_tabela_link_artigos.Rdata")
