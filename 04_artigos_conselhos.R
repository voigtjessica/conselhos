#04_artigos_conselhos

library(dplyr)
library(data.table)
library(stringr)

#Agora queremos colher todos os artigos que têm a ver com participacao.

setwd("/Volumes/KINGSTON/Adrian/scielo_conselhos")
#setwd("C:\\Users\\yvfg3118\\Documents\\R Scripts\\conselhos")
load(file="03_lista_artigos_pt_verificados.Rdata")

#criando lista de artigos com o termo conselho e participacao:

artigos_conselhos <- list()

#inserindo barra de progresso:
pb = txtProgressBar(min = 0, max = length(lista_artigos_pt_verificados), initial = 0) 

#loop:

for(i in 1:length(lista_artigos_pt_verificados)){
  
  #selecionar o texto
  texto <- lista_artigos_pt_verificados[[i]][5]
  
  #buscar conselh$ e termos da participacao, independente da ordem:
  t <- grepl("\\bconselh.*", texto, ignore.case=TRUE) & grepl(termos_participacao, texto, ignore.case=TRUE)
  
  #quando achar:
  if(t == TRUE){
    
    n <- length(artigos_conselhos)
    n <- n + 1
    #adicionar na lista
    artigos_conselhos[[n]] <- lista_artigos_pt_verificados[[i]]
    
  }
  #barra de progresso:
  setTxtProgressBar(pb,i)
}

##### RODAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Salvar script

save(artigos_conselhos, file="04_artigos_conselhos.Rdata")
#load("/Volumes/KINGSTON/Adrian/scielo_conselhos/04_artigos_conselhos.Rdata")


######## AGORA É A PARTE QUE EU VOU CORRIGIR:
# Eu preciso criar um data_frame contendo todos os artigos com pelo menos uma menção ao nome conselho
# e o nome do artigo:

df <- data.frame(termo = character(), 
                 nome_artigo = character(), 
                 url_artigo = character())


for(i in 1:length(artigos_conselhos)){
  
  if(grepl("conselhos?(?:\\s+\\w+){1,2}", artigos_conselhos[[i]][[5]])){
    
    print(paste0(i, " TRUE"))
    
    termos <- artigos_conselhos[[i]][[5]] %>%
      str_extract_all("conselhos?(?:\\s+\\w+){1,3}") %>%
      unlist()
    
    a <- length(termos)
    nome_artigo <- rep(artigos_conselhos[[i]][[6]][[2]], a)
    url_artigo <- rep(artigos_conselhos[[i]][[4]], a)
    
    df1 <- data.frame(termos, nome_artigo, url_artigo)
    df <- rbind(df, df1)
    
  } else
    {
    print(paste0(i, " FALSE"))
  }
}

rm(termos)
rm(df1)
rm(a)
rm(nome_artigo)
rm(url_artigo)
rm(i)


### parte 2
# Tenho que descobrir os textos do que eu já categorizei:
# criar uma lista apenas com os textos:

pb = txtProgressBar(min = 0, max = length(artigos_conselhos), initial = 0) 

objeto_artigos_conselhos <- c()

for(i in 1:length(artigos_conselhos)){
  
  a <- tolower(artigos_conselhos[[i]][5])
  b <- artigos_conselhos[[i]][[4]]
  objeto_artigos_conselhos[[i]] <- list(a, b)
  
  #barra de progresso:
  setTxtProgressBar(pb,i)
}

rm(a)
rm(b)
rm(pb)
rm(i)
### Verificar as ocorrencias para conselho que existem:
# estou criando um objeto de strings termos_conselho, que tem todas as ocorrencias de conselho
# dentro dos 4 mil textos:

dftermos_conselho <- data.frame()

for(i in 1:length(objeto_artigos_conselhos)){
  
  if(grepl("conselho.+", objeto_artigos_conselhos[[i]][[1]])){
   
     termos_conselho <- objeto_artigos_conselhos[[i]][[1]] %>% 
      str_extract_all("conselho.+") %>% 
      word(1,6)
    
    url_artigo <- objeto_artigos_conselhos[[i]][[2]]
    df1 <- data.frame(termos_conselho, url_artigo)
    dftermos_conselho <-rbind(dftermos_conselho, df1)
  }
}


dftermos_conselho <- dftermos_conselho %>%
  distinct()

termos_conselho <- unique(termos_conselho) # 2310 ocorrencias ainda sujas.


dftermos_conselho <- dftermos_conselho %>%
  mutate(termos_conselho = gsub('c\\(', '', termos_conselho),
         termos_conselho = gsub('\\"', '', termos_conselho))
         

# Agora eu vou importar o arquivo que eu já trabalhei e fazer um left_join para 
# saber o que é conselho e o que não é.

classificados <- fread("/Volumes/KINGSTON/Adrian/scielo_conselhos/dftermos_conselho_verificando - Classificacao_final.csv")

cruzamento <- dftermos_conselho %>%
  left_join(classificados, by = "termos_conselho")

# Agora vou fazer três objetos: 1 com os artigos que tem conselho 
# outro com os termos que sabemos ser conselhos e mais um com os termos
# que sabemos não ser conselhos, pra automatizar.

artigos_sobre_conselhos <- cruzamento %>%
  filter(incluir_final == 1)

artigos_sobre_conselhos <- unique(artigos_sobre_conselhos$url_artigo)
length(artigos_sobre_conselhos) #1229

termos_verdadeiros <- cruzamento %>%
  filter(incluir_final == 1)

termos_verdadeiros <- unique(termos_verdadeiros$termos_conselho)
length(termos_verdadeiros) #980

termos_falsos <- cruzamento %>%
  filter(incluir_final == 0) 

termos_falsos <- unique(termos_falsos$termos_conselho)
length(termos_falsos) #

#Finalmente, vou começar a limpar a lista. Vamos verificar o elemento único 
#encontrado no nosso df geralzão, 

df_termos_unicos <- df %>%
  group_by(termos, nome_artigo, url_artigo) %>%
  summarise(total_repeticoes = n(), .groups = "keep") %>%    #7647 termos unicos por artigo, mas eles estão sujos e nao se repetem muito
  ungroup() %>%
  mutate_at(vars(termos, nome_artigo), ~ str_replace_all(., c('"' = '',
                                                              ';' = ',',
                                                              '\\s+' = ' '))) %>%
  mutate(artigo_tem_conselho = ifelse(url_artigo %in% artigos_sobre_conselhos, 1, 0),
         termo_verdadeiro = ifelse(termos %in% termos_verdadeiros, 1, 0),
         termos_falsos = ifelse(termos %in% termos_falsos, 1, 0))  %>%    # não achou nada.
  filter(artigo_tem_conselho == 0, # sobraram 3141
         termo_verdadeiro == 0,    # não achou nada
         termos_falsos == 0)  %>%     # não achou nada
  distinct() %>%
  group_by(nome_artigo, url_artigo) %>%
  summarize(termos_concatenados = str_c(termos, collapse = " | "), .groups = "keep")

# Bom, agora eu tenho uma lista de termos que eu tenho que verificar na mão se
# se tratam de conselhos ou não. Vou salvar:

write.csv2(df_termos_unicos,
           file="revisao_termos_2.csv",
           fileEncoding = "UTF-8",
           row.names = FALSE)

