#04_artigos_conselhos

library(dplyr)

#Agora queremos colher todos os artigos que têm a ver com participacao.

setwd("C:\\Users\\yvfg3118\\Documents\\R Scripts\\conselhos")
load(file="03_lista_artigos_pt_verificados.Rdata")


#primeiro vou pegar os termos que entram, depois eu vou fazer desambiguacao:

termos_participacao <- c("Participação democrática",
                         "Participação popular",
                         "Participação extraparlamentar",
                         "Participação social",
                         "Participação da sociedade civil",
                         "participação cidadã",
                         "Participação de cidadãos",
                         "Participação dos cidadãos",
                         "Participação de movimentos sociais",
                         "Participação dos movimentos sociais",
                         "Participação de movimento social",
                         "Participação do movimentos sociais",
                         "Participação de organizações",
                         "Participação das organizações",
                         "Participação de organização",
                         "Participação da organização",
                         "Participação coletiva",
                         "Participação deliberativa",
                         "Participação na política pública" ,
                         "Participação em política pública",
                         "Participação em políticas públicas",
                         "Participação nas políticas públicas",
                         "Arquitetura da participação",
                         "Governança participativa")


termos_participacao <- paste(tolower(termos_participacao), collapse = "|")

busca <- paste0(".*(conselh.*", termos_participacao, "|",termos_participacao, ".*conselh).*")

#criando lista de artigos com o termo conselho e participacao:

artigos_conselhos <- list()

#inserindo barra de progresso:
pb = txtProgressBar(min = 0, max = length(lista_artigos_pt_verificados), initial = 0) 

#loop:

for(i in 1:length(lista_artigos_pt_verificados)){
  
  t <- grepl(busca, lista_artigos_pt_verificados[[i]][5], ignore.case=TRUE)
  if(t == TRUE){
    
    n <- length(artigos_conselhos)
    n <- n + 1
    artigos_conselhos[[n]] <- lista_artigos_pt_verificados[[i]]
    
  }
  #barra de progresso:
  setTxtProgressBar(pb,i)
}

##### RODAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Salvar script

save(artigos_conselhos, file="04_artigos_conselhos.Rdata")

#################### DESAMBIGUIACAO:

#passo 1: criar uma lista apenas com os textos:

pb = txtProgressBar(min = 0, max = length(artigos_conselhos), initial = 0) 

objeto_artigos_conselhos <- c()

for(i in 1:length(artigos_conselhos)){
  
  a <- tolower(artigos_conselhos[[i]][5])
  objeto_artigos_conselhos[i] <- a
  
  #barra de progresso:
  setTxtProgressBar(pb,i)
}

### PASSO 2, verificar as ocorrencias para conselho que existem:
# estou criando um objeto de strings termos_conselho, que tem todas as ocorrencias de conselho
# dentro dos 4 mil textos:

termos_conselho <- objeto_artigos_conselhos %>% 
  str_extract_all("conselho.+") %>% 
  word(1,6)

termos_conselho <- unique(termos_conselho) # 2310 ocorrencias ainda sujas.
dftermos_conselho <- data.frame(termos_conselho)


dftermos_conselho <- dftermos_conselho %>%
  mutate(termos_conselho = gsub('c\\(', '', termos_conselho),
         termos_conselho = gsub('\\"', '', termos_conselho))
         

# Agora eu vou ver na mao o que que eu quero:

save(dftermos_conselho, file="dftermos_conselho.Rdata")
write.csv(dftermos_conselho, file="dftermos_conselho.csv")

#### Próximos passos:

# fazer um novo elemento quando tem uma vírcula dentro do elemento, porque 
# isso significa que ele achou mais de um conselho




# retirar caracteres sujos e coisas que nao sao nome


# filtrar para MANTER aqueles textos que tem ao menos uma ocorrência dos termos de conselho que 
# a gente aceita, incluindo conselho sem nada. 



