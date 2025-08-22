# Projeto Coesão & Dispersão: Script para extrair dados codificados de oitiva: Pret e codificar automaticamente as variáveis previsoras linguísticas e sociais
# set/2024, mar/2025, abr/2025
# requer arquivos silacpret-adapt7-ago2018-mar2025.R e chave_fonologica_utf8.csv apenas para geracao da lista de palavras pretonicas convertidas para transcSilac.
  # script com base no output de medições de vogais do _08aVowel-Analyzer-CeD (arq vogais)

rm(list = ls())

# Carregar pacotes ####
#library(phonfieldwork)
#library(act)
library(tidyverse)
library(beepr)
library(lubridate)

# Função usando lubridate para converter segundos em hh:mm:ss.ms (formatar variável LOCALIZACAO); feita com ajuda do ChatGPT mar/2025
converter_tempo <- function(segundos) {
  periodo <- seconds_to_period(segundos) # Converte para período
  horas <- periodo@hour
  minutos <- periodo@minute
  segundos <- periodo@.Data # Segundos, incluindo fração
  
  # Formatar para hh:mm:ss.ms
  sprintf("%02d:%02d:%06.3f", horas, minutos, segundos)
}

# Função para gerar o vetor com o número da sílaba # código gerado pelo DeepSeek, que dá output incorreto se a mesma palavra ocorre duas vezes em sequência
gerar_vetor_silabas <- function(transcSilac) {
  # Inicializar o vetor final
  vetor_final <- c()
  
  # Inicializar variáveis para controle
  palavra_atual <- ""
  contador_silabas <- 0
  num_silabas <- 0  # Número de sílabas da palavra atual
  
  # Iterar sobre cada elemento na coluna transcSilac
  for (i in seq_along(transcSilac)) {
    palavra <- transcSilac[i]
    
    # Verificar se é uma nova ocorrência da palavra
    if (palavra != palavra_atual) {
      # Se for uma nova palavra, calcular o número de sílabas
      num_silabas <- lengths(strsplit(palavra, "-"))
      palavra_atual <- palavra
      contador_silabas <- 1  # Reiniciar o contador de sílabas
    } else {
      # Se for a mesma palavra, incrementar o contador de sílabas
      contador_silabas <- contador_silabas + 1
    }
    
    # Adicionar o número da sílaba ao vetor final
    vetor_final <- c(vetor_final, contador_silabas)
  }
  
  return(vetor_final)
}


# Carregar dados ####
loc_Arquivo_Vogais <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/analises-ProjetoCoesaoeDispersao/dadosVogais/outputVowelAnalyzer"

# Definir pasta de output dos arquivos de dados ##
loc_output_Amostra <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/analises-ProjetoCoesaoeDispersao/dadosVogais"

# Definir pasta com arquivos auxiliares (silac etc.)
loc_arquivosAuxiliares <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/analises-ProjetoCoesaoeDispersao"


nomeArquivoVogaisF <- "vogais_VALPB-F"
nomeArquivoVogaisM <- "vogais_VALPB-M"
nomeAmostra <- "VALPB"

comeco <- Sys.time()

# Definir diretório de trabalho ##
setwd(loc_Arquivo_Vogais)

dadosPretF <- read_delim(paste0(nomeArquivoVogaisF, ".txt")) %>% 
  select(Word, Pret, precPhon, Vowel, nextPhon, F1_30, F1_50, F1_70, F2_30, F2_50, F2_70, F3_30, F3_50, F3_70, duration_ms, ARQUIVO, beginTime_s, endTime_s)
colnames(dadosPretF)[1] <- "ITEM_LEXICAL"
dadosPretM <- read_delim(paste0(nomeArquivoVogaisM, ".txt")) %>% 
  select(Word, Pret, precPhon, Vowel, nextPhon, F1_30, F1_50, F1_70, F2_30, F2_50, F2_70, F3_30, F3_50, F3_70, duration_ms, ARQUIVO, beginTime_s, endTime_s)
colnames(dadosPretM)[1] <- "ITEM_LEXICAL"

# juntar dataframes F e M em dadosPret
dadosPret <- bind_rows(dadosPretF, dadosPretM)
dadosPret <- arrange(dadosPret, ARQUIVO)

# <---- todo: há vários dados em Pret vazios. Descobrir por que!
removerVazios <- which(is.na(dadosPret$Pret) == TRUE)
length(removerVazios)
dadosPret <- dadosPret[-removerVazios, ]
rm(dadosPretF, dadosPretM)


#### CLASSE_MORFOLOGICA/transcSilac script ok - revisar <---  ####
# A classificação de classe morfológica foi feita com ajuda do DeepSeek, a partir do prompt: "Peço que classifique as palavras do arquivo de acordo com sua classe morfológica (substantivo, adjetivo, verbo etc.). Caso a palavra possa pertencer a mais de uma classe, coloque todas as classes separadas por hífen. Por exemplo, a palavra "almoço" pode ser substantivo ou verbo; nesse caso, o output deve ser "substantivo-verbo".  No caso de substantivos, diferencie substantivos comuns de nomes próprios, classificando os primeiros como "substantivo" e os segundos como "substantivoProprio". Vc pode gerar um arquivo csv como output, que contenha, na primeira coluna, a palavra original, e na segunda coluna a classe morfológica?". --> Precisa ser revisada. 


dfPret <- read_delim("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/analises-ProjetoCoesaoeDispersao/palavrasPret-output-DeepSeek-POS-transcSilac.txt") # df gerado pelo script _05script-adaptDictMFA-silac-pCodificacaoCFP, a partir do dicionário do MFA (atual = v6), acrescido da classificação morfológica do Deep Seek. A coluna transcSilac foi gerada a partir da aplicação do script silacpret-adapt7-ago2018-mar2025.R à coluna ITEM_LEXICAL.

pCodifPOS <- left_join(dadosPret, dfPret, by = 'ITEM_LEXICAL')
#View(pCodifPOS[,c("ITEM_LEXICAL", "Vowel", "CLASSE_MORFOLOGICA")])
which(pCodifPOS$CLASSE_MORFOLOGICA == "")
which(pCodifPOS$transcSilac == "")

#checar linha abaixo...
dadosPret$CLASSE_MORFOLOGICA <- pCodifPOS$CLASSE_MORFOLOGICA
dadosPret$transcSilac <- pCodifPOS$transcSilac

#### N.SILABAS #### 
# Não é exatamente uma variável previsora; esta coluna ajuda a codificar tonidade da sílaba, para posterior remoção de vogais tônicas e postônicas

# Verificar número de sílabas de cada palavra
aa <- strsplit(dadosPret$transcSilac, "-")
numero_silabas <- vector()
for(i in 1:length(aa)){
  numero_silabas[i] <- length(aa[[i]])
}
numero_silabas

# Aplicar funcao gerar_vetor_silabas p/ gerar o vetor de número de sílabas em cada palavra
num_silab_DeepSeek <- gerar_vetor_silabas(dadosPret$transcSilac)

# Corrigir o contador do DeepSeek num_silab_DeepSeek
# Código gerado pelo ChatGPT
while (any(num_silab_DeepSeek > numero_silabas)) {
  indices <- which(num_silab_DeepSeek > numero_silabas)  # Encontrar índices onde a condição é verdadeira
  num_silab_DeepSeek[indices] <- num_silab_DeepSeek[indices] - numero_silabas[indices]  # Atualizar os valores
}

# Checar se algum valor em num_silab_DeepSeek é maior do que numero_silabas; resultado tem que ser 0
which(num_silab_DeepSeek > numero_silabas)

dadosPret$N_silaba <- num_silab_DeepSeek


## TONICIDADE DA SÍLABA ok! ####
silabas <- str_split(dadosPret$transcSilac, pattern = "-")

tonicidades <- unlist(gregexpr("\\'", silabas[[1]])); tonicidades
pos.sil.tonica <- which(tonicidades == 1); pos.sil.tonica


tonicidades2 <- vector()
for(i in 1:length(dadosPret$N_silaba)){
  tonicidades <- unlist(gregexpr("\\'", silabas[[i]])); tonicidades
  pos.sil.tonica <- which(tonicidades == 1); pos.sil.tonica
  teste1 <- pos.sil.tonica > dadosPret$N_silaba[i]
  teste2 <- pos.sil.tonica == dadosPret$N_silaba[i]
  if(teste1 == TRUE){
    tonicidades2[i] <- "pretonica"
  } else {
    if(teste2 == TRUE){
      tonicidades2[i] <- "tonica"
    } else {
      tonicidades2[i] <- "postonica"
    }
  }
}
head(tonicidades2, 30)
#View(cbind(dadosPret$transcSilac, dadosPret$Vowel, tonicidades2))

N_silaba2 <- dadosPret$N_silaba
dif <- vector()
for(i in 1:length(N_silaba2)){
  dif[i] <- N_silaba2[i+1] - N_silaba2[i]
  }
bb <- which(dif == 0) # os casos de dif == 0 são potencialmente aqueles com erro, codificados como "pretonica", mas devem ser "postonica".  
tonicidades2[bb] <- "postonica"

dadosPret$Stress <- tonicidades2


## CODIFICAR VIs ####
#### F1 da VOGAL SIL SEGUINTE CONTINUA-- ok!####
##pegar valores de F1 e de F2 de acordo com medicoes da silaba seguinte; 

F1_SIL_SEG<-vector()
for (i in 1:length(dadosPret$F1_50)) {
  F1_SIL_SEG[i]<- as.numeric(dadosPret[i+1,"F1_50"])
}
head(F1_SIL_SEG); length(F1_SIL_SEG)

dadosPret$F1_SIL_SEG <- F1_SIL_SEG

#### F2 da VOGAL SIL SEGUINTE CONTINUA -- ok!####
F2_SIL_SEG<-vector()
for (i in 1:length(dadosPret$F2_50)) {
  F2_SIL_SEG[i] <- as.numeric(dadosPret[i+1,"F2_50"])
}
head(F2_SIL_SEG); length(F2_SIL_SEG)

dadosPret$F2_SIL_SEG <- F2_SIL_SEG

########VOGAL DA SILABA SEGUINTE 2 -- ok!########
VOGAL_SIL_SEG<-vector()
for (i in 1:length(dadosPret$Vowel)) {
  VOGAL_SIL_SEG[i]<-as.character(dadosPret[i+1,"Vowel"])
}
head(VOGAL_SIL_SEG); length(VOGAL_SIL_SEG)

dadosPret$VOGAL_SIL_SEG <- VOGAL_SIL_SEG


#### VOGAL TONICA -- ok!######## 
# Solução fornecida pelo ChatGPT a partir do prompt: "tenho uma sequência de caracteres como k<o>-'me-so. Os hífens marcam separação de sílabas e a aspa simples marca a sílaba tônica. Quero separar a sílaba tônica: neste exemplo, a sílaba é me. Como posso fazer isso no R?"

# Usando regex para extrair a sílaba tônica
vogal_tonica <- regmatches(dadosPret$transcSilac, regexpr("'[a-zA-Z<>]+", dadosPret$transcSilac))

# Removendo a aspa simples *e caracteres que não são vogais* para obter apenas a *vogal da* sílaba
vogal_tonica <- gsub("'|[^aeiouAEIOUjw]", "", vogal_tonica); vogal_tonica

dadosPret$VOGAL_SIL_TONICA <- vogal_tonica
#View(dadosPret[, c("transcSilac", "Vowel", "VOGAL_SIL_TONICA")])

#### DIST_TONICA #### 
silabas <- str_split(dadosPret$transcSilac, pattern = "-")

distancias <- vector()
for(i in 1:length(dadosPret$transcSilac)){
  tonicidades <- unlist(gregexpr("\\'", silabas[[i]]))
  pos.sil.tonica <- which(tonicidades == 1)
  distancias[i] <- pos.sil.tonica - dadosPret$N_silaba[i]
}

dadosPret$DIST_TONICA <- distancias

#### ESTR_SILABA ok!####

EstrSilPret <- vector()
for(i in 1:length(dadosPret$N_silaba)){
  pEstrSilPret <- silabas[[i]][dadosPret$N_silaba[i]]
  pEstrSilPret <- gsub("<|>|\\'", "", pEstrSilPret)
  pEstrSilPret <- gsub("[aeiouAEIOU]", "V", pEstrSilPret)
  pEstrSilPret <- gsub("[wj]", "G", pEstrSilPret)
  pEstrSilPret <- gsub("s$", "&", pEstrSilPret)
  pEstrSilPret <- gsub("r$", "@", pEstrSilPret)
  pEstrSilPret <- gsub("[aeiouAEIOU]", "V", pEstrSilPret)
  pEstrSilPret <- gsub("[bcdfghJklLmNnprRsStvzZ]", "C", pEstrSilPret)
  pEstrSilPret <- gsub("&", "s", pEstrSilPret)
  EstrSilPret[i] <- gsub("@", "r", pEstrSilPret)
}
head(EstrSilPret, 50)

dadosPret$ESTR_SILABA <- EstrSilPret
#View(dadosPret[,c("ITEM_LEXICAL", "Vowel", "ESTR_SILABA")])



#### LOCALIZACAO formatar ok! #### 
# Aplicar funcao converter_tempo ao vetor de localização
tempos_formatados <- sapply(dadosPret$beginTime_s, converter_tempo)

dadosPret$LOCALIZACAO <-tempos_formatados; head(dadosPret$LOCALIZACAO)

#### CODIF_OITIVA ####
dadosPret$Pret
aBuscar <- paste0(dadosPret$Vowel, ">")

codificadosOitiva <- vector()
for(i in 1:length(aBuscar)){
  cc <- gregexpr(aBuscar[i], dadosPret$Pret[i])
  codificadosOitiva[i] <- substr(dadosPret$Pret[i], unlist(cc) - 1,  unlist(cc) - 1)
}
codificadosOitiva[grep("<", codificadosOitiva)] <- "NA"

dadosPret$CODIF_OITIVA <- codificadosOitiva
naoMediasPretonicas <- which(dadosPret$CODIF_OITIVA == "")
dadosPret[naoMediasPretonicas, "CODIF_OITIVA"] <- "NA"

#View(dadosPret[which(codificadosOitiva == ""), c("ITEM_LEXICAL", "Pret", "transcSilac", "Vowel", "CODIF_OITIVA", "N_silaba")])


#### AJUSTES FINAIS #### 
setwd(loc_output_Amostra)

# Exportar uma versão somente com vogais tônicas ....
#dadosTonicas <- dadosPret2 %>% 
#  filter(Stress == "tonica") %>% 
#  droplevels()


# Retirar sílabas postônicas e Retirar sílabas pretônicas com certas estruturas silábicas: V, VG, Vr, Vs, CVG, CVGs, CVV, CVVs; ou manter apenas CCV, CV, CVr, CVs
dadosPret2 <- dadosPret %>% 
  filter(Stress %in% c("pretonica", "tonica") & ESTR_SILABA %in% c("CCV", "CV", "CVr", "CVs")) %>% 
  droplevels()

# Inserir pausa para nextPhon vazias (nextPhon foi extraída a partir do Script Vowel Analyzer, que pegou cont fonico precedente e seguinte à vogal alvo)
pausasSeguintes <- which(is.na(dadosPret2$nextPhon))
dadosPret2$nextPhon[pausasSeguintes] <- "#" 

#### CODIFICAR VPs SOCIAIS <---- todo #### 
dadosPret2$ARQUIVO <- gsub(" ", "", dadosPret2$ARQUIVO)
dadosPret2$AMOSTRA <- nomeAmostra
setwd(loc_arquivosAuxiliares)
dadosInfs <- read_csv("dadosINFS-varSociais.csv", ) %>% 
  select(., AMOSTRA:CLASSE_SOCIAL, QUAL_AUDIO)

pCodifVarSociais <- left_join(dadosPret2, dadosInfs, by = 'ARQUIVO')
dadosPret2[, "ESTADO"] <- pCodifVarSociais[, "ESTADO"]
dadosPret2[, "GENERO"] <- pCodifVarSociais[, "GENERO"]
dadosPret2[, "IDADE"] <- pCodifVarSociais[, "IDADE"]
dadosPret2[, "FAIXA_ETARIA"] <- pCodifVarSociais[, "FAIXA_ETARIA"]
dadosPret2[, "ESCOLARIDADE"] <- pCodifVarSociais[, "ESCOLARIDADE"]
dadosPret2[, "IDADE_MIGRACAO"] <- pCodifVarSociais[, "IDADE_MIGRACAO"]
dadosPret2[, "IDADE_MIGRACAO_fat"] <- pCodifVarSociais[, "IDADE_MIGRACAO_fat"]
dadosPret2[, "TEMPO_RESIDENCIA"] <- pCodifVarSociais[, "TEMPO_RESIDENCIA"]
dadosPret2[, "TEMPO_RESIDENCIA_fat"] <- pCodifVarSociais[, "TEMPO_RESIDENCIA_fat"]
dadosPret2[, "INDICE_SOCIO"] <- pCodifVarSociais[, "INDICE_SOCIO"]
dadosPret2[, "CLASSE_SOCIAL"] <- pCodifVarSociais[, "CLASSE_SOCIAL"]
dadosPret2[, "QUAL_AUDIO"] <- pCodifVarSociais[, "QUAL_AUDIO"]


setwd(loc_output_Amostra)

# Reorganizar ordem das colunas
dadosPretFinal <- dadosPret2 %>% 
  select("ITEM_LEXICAL", "Pret", "transcSilac", "precPhon", "Vowel", "nextPhon", "CODIF_OITIVA", "F1_30", "F1_50", "F1_70", "F2_30", "F2_50", "F2_70", "F1_SIL_SEG", "F2_SIL_SEG", "VOGAL_SIL_SEG", "VOGAL_SIL_TONICA", "DIST_TONICA", "ESTR_SILABA", "CLASSE_MORFOLOGICA", "N_silaba", "Stress", "ARQUIVO", "LOCALIZACAO", "AMOSTRA", "ESTADO", "GENERO", "IDADE", "FAIXA_ETARIA", "ESCOLARIDADE", "IDADE_MIGRACAO", "IDADE_MIGRACAO_fat", "TEMPO_RESIDENCIA", "TEMPO_RESIDENCIA_fat", "INDICE_SOCIO", "CLASSE_SOCIAL", "QUAL_AUDIO", "F3_30", "F3_50", "F3_70")



### Exportar planilha codificada ####

# exportar em formato xlsx
file2 <- paste("dadosVogais_", gsub(".txt", "", nomeAmostra), "-planilha.xlsx", sep="")

N <- 1:nrow(dadosPretFinal)
dados4 <- cbind(N, dadosPretFinal)

library(openxlsx)
right <- createStyle(halign = "right")
left <- createStyle(halign = "left")
center <- createStyle(halign = "center")

## Create a new workbook
wb <- createWorkbook(file)
## Add some worksheets
addWorksheet(wb, "Sheet 1")
addWorksheet(wb, "Sheet 2")
## Write data
writeData(wb, 1, dados4, colNames = T)
#writeData(wb, 2, codigos, colNames = T) # possivelmente colocar variaveis sociais aqui
setColWidths(wb, sheet = 1, cols = 1:ncol(dados4), widths = "auto")
addFilter(wb, 1, row = 1, cols = 1:ncol(dados4))
for(i in 1:ncol(dados4)){
  addStyle(wb, 1, cols = i, style = center, rows = 1:nrow(dados4)+1)
}
## Freeze Panes
freezePane(wb, "Sheet 1" ,  firstRow = T, firstCol = F)
## Save workbook
saveWorkbook(wb, file2, overwrite = TRUE)


fim <- Sys.time()
fim - comeco

#rm(list=setdiff(ls(), c("converter_tempo", "loc_Arquivo_Vogais", "loc_output_Amostra", "Amostra_Pret", "Amostra", "dfPret", "dadosPretFinal", "dadosTonicas")))

beep()
