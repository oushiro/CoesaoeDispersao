# Projeto Coesão & Dispersão: Script para juntar trilhas do output do MFA e das codificações de oitiva feitas no ELAN
# O que é necessário para rodá-lo: definir três diretórios de trabalho: o local dos arquivos com o output to MFA; o local dos arquivos com todas as codificações; o local dos arquivos finais que juntam MFA + codificações. 
# fev/2025
# Esta versão é boa quando o MFA gerou o alinhamento da maioria das anotações. Não é tão boa para alinhamentos no MFA que "pulam" muitas anotações (provavelmente pela qualidade baixa do áudio); para esses casos, usar Tentativa 5 do script _script-Conversa-DeepSeek-match-MFA-Codifs, feito pelo Deep Seek
# Warning: takes very long to run!

library(phonfieldwork)
library(act)
library(tidyverse)
library(beepr)

rm(list = ls())

# Definições ####
## MFA: Carregar textgrids alinhados (output MFA) ####
#loc_Amostra_MFA <- "C:/Users/XPS/Desktop/MFA_testes/output_v5"
loc_Amostra_MFA <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/output_PORTAL"


## Codifs: Carregar dados do arquivo eaf com todas as codificações ####
#loc_Amostra <- "C:/Users/XPS/Desktop/testes-acharTrilhasDiferentes/Amostra_todasCodificacoes"
loc_Amostra <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/ProjetoCoesaoeDispersao_PORTAL/PORTAL_todasCodificacoes"

## Output: definir diretório para os quais os arquivos finais serão exportados ####
#loc_Output <- "C:/Users/XPS/Desktop/testes-acharTrilhasDiferentes/Amostra_MFAcCodif"
loc_Output <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/output_PORTAL/output_JuntarTrilhas2"

####
comeco0 <- Sys.time()
setwd(loc_Amostra_MFA)
filesAmostra_MFA <- dir(pattern = ".TextGrid")
filesExport <- paste0(gsub(".TextGrid", "", filesAmostra_MFA), ".txt")

# TODAS TRANSCRICOES #####

## Definir diretório de trabalho das transcrições codificadas ####
setwd(loc_Amostra)
filesAmostra <- dir(pattern = ".eaf")

## Carregar files de cada amostra em uma lista de transcricoes completas ####
Amostra <- list()

for(i in 1:length(filesAmostra)){
  setwd(loc_Amostra)
  # carregar arquivos na lista
  Amostra[[i]] <- eaf_to_df(filesAmostra[i])
  # selecionar trilhas relevantes
  Amostra[[i]] <- Amostra[[i]] %>% 
    filter(tier_name %in% c("S1", "Pret", "R", "S")) %>% 
    select(c(content, tier_name, time_start, time_end, source, media_url)) %>% 
    arrange(time_start)
  # apagar caracteres especiais
  Amostra[[i]]$content <- gsub("\\!|\\.|\\?|\\{|\\}|\\(|\\)|\\/|\"|\'", "", Amostra[[i]]$content)
  Amostra[[i]]$content <- gsub("<>", "", Amostra[[i]]$content)
  # juntar codificação de R e S à palavra precedente
  Amostra[[i]]$content <- gsub("\\s+<", "<", Amostra[[i]]$content)
}


# MFA output ####

## Definir diretório de trabalho ####

for(j in 1:length(filesAmostra_MFA)){ 
  comeco<-Sys.time()
  # report progress
  cat("Processing file", j, "out of", length(filesExport), filesExport[j], "...\n")
  
  setwd(loc_Amostra_MFA)
#  filesAmostra_MFA <- dir(pattern = ".TextGrid")
#  filesExport <- paste0(gsub(".TextGrid", "", filesAmostra_MFA), ".txt")
  

## Carregar files da amostra em uma lista de transcricoes completas ####
Amostra_MFA <- list()

for(i in 1:length(filesAmostra_MFA)){
  # carregar arquivos na lista
  Amostra_MFA[[i]] <- textgrid_to_df(filesAmostra_MFA[i])
  # selecionar trilhas relevantes
  Amostra_MFA[[i]] <- Amostra_MFA[[i]] %>% 
    select(c(content, tier_name, time_start, time_end, source, tier, id)) %>% 
  # ordenar por trilha
    arrange(tier_name)
  # remover anotações vazias
  remover <- which(Amostra_MFA[[i]]$content == "")
  Amostra_MFA[[i]] <- Amostra_MFA[[i]][-remover, ]
}

#View(Amostra_MFA[[j]])

Amostra_MFA_words_list <- list()
Amostra_MFA_words_list[[j]] <- Amostra_MFA[[j]] %>% 
  filter(tier_name == "words" & content != "")

#View(Amostra_MFA_words_list[[j]])
words_MFA <- Amostra_MFA_words_list[[j]]$content


#View(Amostra[[j]])

## Criar vetor de palavras de cada trilha
S1 <- Amostra[[j]] %>% 
  filter(tier_name == "S1") 
words_S1 <- unlist(str_split(S1$content, pattern = " "))
words_S1 <- words_S1[which(words_S1 != "")]

####
# Conversa com ChatGPT -- isso não dá certo; o vetor2b copia a primeira ocorrência da palavra, o que faz com que cada item lexical não tenha variação na pronúncia
#vetor1a <- c("a", "porta", "tá", "aberta")
#vetor2a <- c("a", "é", "porta<R>", "está", "aberta<T>")

#tenho dois vetores, como vetor1a e vetor2a, de palavras semelhantes, mas não idênticos. O segundo vetor tem marcações de algumas palavras e não quero perder essas informações. Mas quero que o segundo vetor tenha as palavras nas mesmas posições que o primeiro vetor, como no output abaixo. 

#vetor1b <- c("a", "porta", "tá", "aberta")
#vetor2b <- c("a", "porta<R>", "está", "aberta<T>")

#---> o ChatGPT deu o comando 
#vetor2b <- vetor2a[match(vetor1a, gsub("<.*?>", "", vetor2a))], adaptado abaixo
###

# tentativa 2 no ChatGPT
#vetor1a <- c("a", "porta", "tá", "aberta", "aberta", "mesmo", "assim", "é", "tipo", "mesmo")
#vetor2a <- c("a", "é", "porta<R>", "está", "aberta<T>", "aberta<R>", "mesmo", "assim", "ahn", "tipo", "mesmo<A>")

#tenho dois vetores, como vetor1a e vetor2a, de palavras semelhantes, mas não idênticos. O segundo vetor tem marcações de algumas palavras e não quero perder essa informação. Mas quero que o segundo vetor tenha as palavras nas mesmas posições que o primeiro vetor, como no output abaixo. 

#vetor1b <- c("a", "porta", "tá", "aberta", "aberta", "mesmo", "assim", "é", "tipo", "mesmo")
#vetor2b <- c("a", "porta<R>", "está", "aberta<T>", "aberta<R>", "mesmo", "assim", "ahn", "tipo", "mesmo<A>")

#note que a mesma palavra pode ter marcações diferentes em posições diferentes. É importante que a marcação daquela posição seja mantida.


## Criar vetor de palavras a partir da trilha codificada R ####
R <- Amostra[[j]] %>% 
  filter(tier_name == "R") 
words_R <- unlist(str_split(R$content, pattern = " "))
words_R <- words_R[which(words_R != "")]

# Criar um vetor auxiliar sem marcações para comparação
words_R_base <- tolower(gsub("<.*?>", "", words_R))  # Remove tags do words_R

# Criar words_R_df alinhado com words_MFA
words_R_df <- character(length(words_MFA))  # Inicializa vetor vazio

# Contador para cada palavra no words_R (para garantir que a marcação correta seja mantida)
contagem <- table(words_R_base)  # Conta quantas vezes cada palavra aparece
uso <- setNames(rep(0, length(contagem)), names(contagem))  # Controle de uso das palavras

# Percorrer words_MFA e buscar a versão correta do words_R
for (i in seq_along(words_MFA)) {
  palavra <- words_MFA[i]  # Palavra atual de words_MFA
  
  # Encontra a próxima ocorrência da palavra no words_R
  posicoes <- which(words_R_base == palavra)  # Todas as posições da palavra em words_R
  
  if (length(posicoes) > 0) {
    posicao_correta <- posicoes[uso[palavra] + 1]  # Seleciona a posição correta
    
    # Salva a palavra com marcação no words_R_df
    words_R_df[i] <- words_R[posicao_correta]
    
    # Atualiza o contador de uso para essa palavra
    uso[palavra] <- uso[palavra] + 1
  } else {
    words_R_df[i] <- NA  # Caso a palavra não seja encontrada, evita erro
  }
}

# Exibir resultado
#print(words_R_df)
#View(cbind(words_MFA, words_R_df, words_R))


## Criar vetor de palavras a partir da trilha codificada S ####
S <- Amostra[[j]] %>% 
  filter(tier_name == "S") 
words_S <- unlist(str_split(S$content, pattern = " "))
words_S <- words_S[which(words_S != "")]
#words_S_df <- words_S[match(words_MFA, gsub("<.*?>", "", tolower(words_S)))]
#View(cbind(words_MFA, words_S_df))

# Criar um vetor auxiliar sem marcações para comparação
words_S_base <- tolower(gsub("<.*?>", "", words_S))  # Remove tags do words_R

# Criar words_S_df alinhado com words_MFA
words_S_df <- character(length(words_MFA))  # Inicializa vetor vazio

# Contador para cada palavra no words_S (para garantir que a marcação correta seja mantida)
contagem <- table(words_S_base)  # Conta quantas vezes cada palavra aparece
uso <- setNames(rep(0, length(contagem)), names(contagem))  # Controle de uso das palavras

# Percorrer words_MFA e buscar a versão correta do words_S
for (i in seq_along(words_MFA)) {
  palavra <- words_MFA[i]  # Palavra atual de words_MFA
  
  # Encontra a próxima ocorrência da palavra no words_S
  posicoes <- which(words_S_base == palavra)  # Todas as posições da palavra em words_R
  
  if (length(posicoes) > 0) {
    posicao_correta <- posicoes[uso[palavra] + 1]  # Seleciona a posição correta
    
    # Salva a palavra com marcação no words_S_df
    words_S_df[i] <- words_S[posicao_correta]
    
    # Atualiza o contador de uso para essa palavra
    uso[palavra] <- uso[palavra] + 1
  } else {
    words_S_df[i] <- NA  # Caso a palavra não seja encontrada, evita erro
  }
}

# Exibir resultado
#print(words_S_df)
#View(cbind(words_MFA, words_S_df, words_S))


## Criar vetor de palavras a partir da trilha codificada Pret ####
Pret <- Amostra[[j]] %>% 
  filter(tier_name == "Pret") 
words_Pret <- unlist(str_split(Pret$content, pattern = " "))
words_Pret <- words_Pret[which(words_Pret != "")]
words_Pret <- gsub("\'", "", words_Pret)

# gerar versao de words_Pret sem codificacao, para comparação
words_Pret_semCodif <- gsub("<[IE3UOC]", "<", words_Pret)
words_Pret_semCodif <- gsub("\'", "", words_Pret_semCodif)

# gerar versao silac de words_MFA, para comparacao
transc <- words_MFA
source("https://raw.githubusercontent.com/oushiro/CoesaoeDispersao/refs/heads/main/silacpret-adapt8-nov23.R") # esta é a mesma versão do silac que gerou os arquivos que foram codificados de oitiva
words_MFA_silac <- transc2
words_MFA_silac <- gsub("\'", "", words_MFA_silac)

# Criar um vetor auxiliar sem marcações para comparação
words_Pret_base <- gsub("<[IE3UOC]", "<", words_Pret)  # Remove tags do words_R

# Criar words_Pret_df alinhado com words_MFA_silac
words_Pret_df <- character(length(words_MFA_silac))  # Inicializa vetor vazio

# Contador para cada palavra no words_Pret (para garantir que a marcação correta seja mantida)
contagem <- table(words_Pret_base)  # Conta quantas vezes cada palavra aparece
uso <- setNames(rep(0, length(contagem)), names(contagem))  # Controle de uso das palavras

# Percorrer words_MFA_silac e buscar a versão correta do words_Pret
for (i in seq_along(words_MFA_silac)) {
  palavra <- words_MFA_silac[i]  # Palavra atual de words_MFA_silac
  
  # Encontra a próxima ocorrência da palavra no words_Pret
  posicoes <- which(words_Pret_base == palavra)  # Todas as posições da palavra em words_R
  
  if (length(posicoes) > 0) {
    posicao_correta <- posicoes[uso[palavra] + 1]  # Seleciona a posição correta
    
    # Salva a palavra com marcação no words_Pret_df
    words_Pret_df[i] <- words_Pret[posicao_correta]
    
    # Atualiza o contador de uso para essa palavra
    uso[palavra] <- uso[palavra] + 1
  } else {
    words_Pret_df[i] <- NA  # Caso a palavra não seja encontrada, evita erro
  }
}

# Exibir resultado
#print(words_Pret_df)
#View(cbind(words_MFA, words_Pret_df, words_Pret))



#----> TODO importar Dados Contextuais para informações de ruído?

# Juntar output MFA com codificações ####
# Criar arquivo final com words, phones e trilhas com marcações das variáveis R, S, E, O
nomesOutputCompleto <- paste0(gsub(".TextGrid", "", filesAmostra_MFA), ".eaf")
nomesOutputCompletoTextGrid <- paste0(gsub(".TextGrid", "", filesAmostra_MFA), ".TextGrid")

#View(Amostra_MFA[[j]])
locWords <- which(Amostra_MFA[[j]]$tier_name == "words")

paraR <- Amostra_MFA[[j]][locWords, ]
paraR$tier_name <- "R"
paraR$content <- words_R_df
paraR$tier <- as.numeric("3")
paraR$id <- 1:length(paraR$id)

paraS <- Amostra_MFA[[j]][locWords, ]
paraS$tier_name <- "S"
paraS$content <- words_S_df
paraS$tier <- as.numeric("4")
paraS$id <- 1:length(paraS$id)

paraPret <- Amostra_MFA[[j]][locWords, ]
paraPret$tier_name <- "Pret"
paraPret$content <- words_Pret_df
paraPret$tier <- as.numeric("5")
paraPret$id <- 1:length(paraPret$id)

#View(Amostra_MFA[[j]])
locWords <- which(Amostra_MFA[[j]]$tier_name == "words")
Amostra_MFA[[j]][locWords, "id"] <- 1:length(locWords)
locPhones <- which(Amostra_MFA[[j]]$tier_name == "phones")
Amostra_MFA[[j]][locPhones, "id"] <- 1:length(locPhones)

dfFinal <- rbind(Amostra_MFA[[j]], paraR, paraS, paraPret)
attributes(dfFinal)$MEDIA_URL <- gsub("file:\\/\\/\\/", "", Amostra[[j]]$media[j])
#View(dfFinal)


## Exportar via phonfieldwork ####

# definir diretório de output
setwd(loc_Output)


df_to_eaf(dfFinal, nomesOutputCompleto[j]) # esta versão não abre, por algum motivo, então...
# importar o arquivo via act para exportá-lo novamente
transcCompleta <- act::import_eaf(nomesOutputCompleto[j])
act::export_eaf(transcCompleta, nomesOutputCompleto[j])
act::export_textgrid(transcCompleta, nomesOutputCompletoTextGrid[j])
fim <- Sys.time()
fim-comeco
beep()

# report progress
cat("   Completed file", j, "out of", length(filesExport), filesExport[j], "...\n")
cat("   It took", as.numeric(fim-comeco, units = "mins"), "minutes to process file", j, "out of", length(filesExport), filesExport[j], "...\n\n")

}

fim0 <- Sys.time()
comeco0 - fim0
cat("It took", as.numeric(fim0-comeco0, units = "mins"), "minutes to process all files\n")
beep(); beep(); beep()

# remover vetores temporarios
#rm(list = ls())


