
# Lá vamos nós outra vez.

setwd("C:/Users/yvfg3118/Documents/R Scripts/tentativa 5")


#  01tabela_publicacoes

library(dplyr)
library(rscielo)
library(data.table)
library(rvest)
library(stringr)
library(tidyr)

#abrindo arquivo com os links das publicacoes originais

original <- fread("revistas_humanas.csv", encoding="UTF-8")

# a sessao "todos os numeros" têm esse /grid depois do link original:
rev_humanas <- original %>%
  mutate(publi = paste0(link, "grid"))

## Comecando o loop da raspagem: tabela de publicacoes
# Primeiro passo: vou fazer uma tabelona contendo todas as publicacoes e todos
# os anos, para depois trabalhar na coleta dos artigos individualmente. 
# Assim, eu posso filtrar para apenas os anos que eu quero.

tabela_publicacoes <- data.frame(titulo = NA,
                                 ano = NA,
                                 link = NA,
                                 numeros = NA)


for(i in 1:nrow(rev_humanas)){
  url <- as.character(rev_humanas[i,7])
  nome_revista <- as.character(rev_humanas[i,1])
  
  webpage <- read_html(url)
  tabela <- html_nodes(webpage, xpath='//*[@id="issueList"]/table')  %>%
    html_table()
  
  x <- as.data.frame(tabela[[1]]) 
  names(x) <- c("ano", "volume")
  x <- x[-1,]
  x <- x %>%
    select(1) %>%
    mutate(link = NA)
  
  linhas_x <- nrow(x)
  
  # segundo loop, agora para pegar os links:
  
  for(linha in 1:linhas_x){
    xpath <- paste0('//*[@id="issueList"]/table/tbody/tr[', linha, ']/td[2]')
    lks <- html_nodes(webpage, xpath=xpath) %>%
      html_children() %>% 
      html_attr("href")
    lks <- str_c( lks ,collapse=' ; ') 
    x[linha,2] <- lks
  }

  x <- x %>%
    mutate(numeros = (str_count(link, ";") + 1),
           titulo = nome_revista)
  
  tabela_publicacoes <- rbind(tabela_publicacoes,x)
}

#fazendo as colunas para desagregar os links:

col <- c()

for(i in 1:8){
  c <- paste0("link_", i)
  col <- c(col, c)
  
}

tabela_publicacoes <- tabela_publicacoes %>%
  filter(!is.na(link)) %>% # retirando linhas em branco 
  separate(link, col, sep=";" ) %>%
  gather(link_numero, link, link_1, link_2, 
         link_3, link_4, link_5, link_6, link_7, link_8) %>%
  filter(!is.na(link)) %>%
  select(-c(link_numero, numeros)) %>%
  mutate(link = paste0("http://www.scielo.com.br", link))

# pegando apenas publicacoes depois de 1990:

tabela_publicacoes <- tabela_publicacoes %>%
  filter(ano >= 1990)

# existem 4890 edicoes de revistas para serem raspadas

unique(tabela_publicacoes$titulo) # 93 revistas

save(tabela_publicacoes, file="_01_tabela_publicacoes.Rdata")
