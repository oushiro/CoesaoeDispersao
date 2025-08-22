# Projeto Coesão & Dispersão: Script para extrair dados codificados de oitiva: R e codificar automaticamente as variáveis previsoras linguísticas e sociais
# set/2024, mar/2025
# requer arquivos dfPalavrasR.csv, silac.R e chave_fonologica_utf8.csv.
# requer dadosINFs.csv

rm(list = ls())

library(phonfieldwork)
library(act)
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

# Carregar dados ####
loc_Amostra <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/ProjetoCoesaoeDispersao_VALPB/VALPB_todasCodificacoes"

# Definir pasta de output dos arquivos de dados ####
loc_output_Amostra <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/analises-ProjetoCoesaoeDispersao/dadosR"

# Definir pasta com arquivos auxiliares (silac etc.)
loc_arquivosAuxiliares <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/analises-ProjetoCoesaoeDispersao"

nomeAmostra <- "VALPB"

# SELECIONAR TUDO E RODAR!

comeco <- Sys.time()

# Definir diretório de trabalho ####
setwd(loc_Amostra)
filesAmostra <- dir(pattern = ".eaf") 

# Carregar files de cada amostra em uma lista de transcricoes completas ####
Amostra <- list()

for(i in 1:length(filesAmostra)){
  # carregar arquivos na lista
  Amostra[[i]] <- eaf_to_df(filesAmostra[i])
}

# Separar trilhas de dados codificados de cada variável junto à trilha D1 ####

## R ####

Amostra_R <- list()
# separar dados de R + D1
for(i in 1:length(filesAmostra)){
  Amostra_R[[i]] <- Amostra[[i]] %>% 
    filter(tier_name %in% c("D1", "R")) %>% 
    select(c(content, tier_name, time_start, time_end, source)) %>% 
    arrange(time_start)
  Amostra_R[[i]]$content <- paste(Amostra_R[[i]]$tier_name, Amostra_R[[i]]$content, sep=" ")
#  Amostra_R[[i]]$content <- gsub("\\.\\.\\.", " \\.\\.\\.", Amostra_R[[i]]$content)
}


# apagar simbolos entre < > que não são VD ou não estão codificados 
for(i in 1:length(filesAmostra)){
  Amostra_R[[i]]$content <- gsub("[xàaáãeéêiíoóôõuú][mf]>", ">", Amostra_R[[i]]$content, ignore.case = T)
  Amostra_R[[i]]$content <- gsub(" <>", "", Amostra_R[[i]]$content, ignore.case = T)
}

#View(Amostra_R[[1]])

# EXTRAIR DADOS ####
setwd(loc_output_Amostra)

## R ####

palavras.contexto <- 5
palavras.ocorrencia <- 2


file <- paste0("dadosR_", nomeAmostra, ".txt")
# Criar nome das variáveis
Cols<-c("VD", "CONT.PREC", "OCORRENCIA", "CONT.SEGUINTE", "ITEM.LEXICAL", "TRANSC.FONOLOGICA.SILAC", "CONT.FON.PREC", "CONT.FON.SEG", "TONICIDADE", "POSICAO.R", "CLASSE.MORFOLOGICA", "ARQUIVO", "LOCALIZACAO", "AMOSTRA", "ESTADO", "GENERO", "IDADE", "FAIXA_ETARIA", "ESCOLARIDADE", "IDADE_MIGRACAO", "IDADE_MIGRACAO_fat", "TEMPO_RESIDENCIA", "TEMPO_RESIDENCIA_fat", "INDICE_SOCIO", "CLASSE_SOCIAL", "QUAL_AUDIO")
#Cria dataframe vazio com nomes das colunas
tabela.dados<-data.frame(matrix(Cols,nrow=1,ncol=length(Cols)))

#Salva dataframe em arquivo .txt, que abarcara os dados extraidos
write.table(tabela.dados, file, quote = F, sep="\t", row.names = F, col.names = F, fileEncoding = "UTF-8", append = F)


for (m in 1:length(Amostra_R)){
  linhas.com.ocorrencias<-grep("<", Amostra_R[[m]][,"content"], value=T, perl=T, ignore.case=T); linhas.com.ocorrencias
  loc.linhas.com.ocorrencias<-grep("<", Amostra_R[[m]][,"content"], value=F, perl=T, ignore.case=T); loc.linhas.com.ocorrencias
  length(loc.linhas.com.ocorrencias)
  
  loc.palavra.ocorr.por.linha<-list()
  for (j in 1:length(linhas.com.ocorrencias)){
    tokens1<-gsub("(\\W)","\\1", linhas.com.ocorrencias[j])
    tokens2<-unlist(strsplit(tokens1," +"))
    loc.palavra.ocorr.por.linha[[j]]<-grep("<",tokens2, perl=T, ignore.case=T)
  }
  
  N.ocorr.por.linha<-vector()
  for (k in 1:length(linhas.com.ocorrencias)){
    N.ocorr.por.linha[k]<-length(loc.palavra.ocorr.por.linha[[k]])
  }
  
  vetor.localizacoes<-rep(Amostra_R[[m]][loc.linhas.com.ocorrencias, "time_start"], N.ocorr.por.linha)
  
  tokens3<-gsub("(\\W)","\\1", Amostra_R[[m]][,"content"])
  tokens4<-unlist(strsplit(tokens3," +"))
  #Busca as ocorrencias do padrao especificado (agora marcados com tag) 
  (matches.in.socio<-grep("<", tokens4, value=F, perl=T, ignore.case=T))
  
  for (i in 1:length(matches.in.socio)){
    cat(
      substr(tokens4[matches.in.socio[i]], 2,2),
      "\t", 
      tokens4[max(0,matches.in.socio[i]-palavras.contexto+1-palavras.ocorrencia-1):max(0,matches.in.socio[i]-palavras.ocorrencia)], #contprec
      "\t", 
      tokens4[max(0,matches.in.socio[i]-palavras.ocorrencia+1):max(0,matches.in.socio[i]+0)], #ocorr 
      "\t", 
      tokens4[(matches.in.socio[i]+1):min(matches.in.socio[i]+palavras.contexto,length(tokens4)+1)], #contseg
      "\t", 
      tokens4[max(0,matches.in.socio[i]-palavras.ocorrencia+1):max(0,matches.in.socio[i]+0)], #para.item.lexical 
      "\t", #para transc.fon
      "\t", #para cont.prec
      "\t", #para cont.seg
      "\t", #para tonicidade
      "\t", #para pos.R
      "\t", #para classe.morf
      "\t", 
      paste(gsub("_codif_EORS.eaf", "", Amostra_R[[m]][1,"source"]), sep=""),
      "\t",
      as.character(vetor.localizacoes[i]),
      "\t",
      nomeAmostra,
      "\t", #p Estado
      "\t", #p Genero
      "\t", #p Idade 
      "\t", #p Faixa Etaria
      "\t", #p Escolaridade
      "\t", #p Idade Migracao
      "\t", #p Idade Migracao fat
      "\t", #p Tempo Residencia
      "\t", #p Tempo Residencia fat
      "\t", #p Indice Socio
      "\t", #p Classe Social
      "\t", #p Qual Audio
      file=file, append=T, "\n"
    )
  }
}

#beep()

### CODIFICAR VIs ####
dados2<-read.table(file, header=T, sep="\t", quote="")
length(dados2[,1])

#### limpar coluna de ITEM.LEXICAL#####
col.item.lexical<-gsub("<0>|<D>|<H>|<M>|<P>|<R>|<T>|<V>|<X>|\\.|\\?|\\!|\\)|\\(|\"|\\'|\\||\\/", "", dados2[,"ITEM.LEXICAL"])
col.item.lexical<-gsub(" ", "", col.item.lexical)
head(col.item.lexical, 20)

col.item.lexical2<-vector()
for (i in 1:length(col.item.lexical)){
  if(aa<-nchar(col.item.lexical[i])==0){
    col.item.lexical2[i]<-col.item.lexical[i-1]
  } else {
    col.item.lexical2[i]<-col.item.lexical[i]
  }
} 

dados3<-dados2
dados3[,"ITEM.LEXICAL"]<-col.item.lexical2; head(dados3, 20)

#### TRANSC.FONOLOGICA.SILAC ####
col.transc.fon<-gsub("\\?|\\)|\\(|\\!|\"|\\'|\\{|\\}|\\/", "", col.item.lexical)
col.transc.fon<-gsub(" ", "", col.transc.fon)
head(col.transc.fon)
transc<-col.transc.fon

setwd(loc_arquivosAuxiliares)
source("silac.R", encoding = "UTF-8")
#beep()
setwd(loc_output_Amostra)

dados3[,"TRANSC.FONOLOGICA.SILAC"]<-transc.fonologica2; head(dados3, 20)


#### TONICIDADE####
para.tonicidade<-transc.fonologica2
para.tonicidade2<-vector()
for (i in 1:length(para.tonicidade)){
  if (nchar(para.tonicidade[i])==0) {
    para.tonicidade2[i]<-para.tonicidade[i-1]
  } else {
    para.tonicidade2[i]<-para.tonicidade[i]
  }
}

head(para.tonicidade2, 20); length(para.tonicidade2)

#para separar palavras com 2R...
espacos.vazios<-which(nchar(transc.fonologica2)==0); espacos.vazios; length(espacos.vazios)

(loc.acento<-regexpr("'", para.tonicidade2, ignore.case=F))
loc.hifens<-gregexpr("-", para.tonicidade2, ignore.case=F); head(loc.hifens)

#loc R em palavras com 1 R
loc.r.coda<-regexpr("r", para.tonicidade, ignore.case=F); head(loc.r.coda, 11) #so 1os rs
loc.r.coda2<-unlist(loc.r.coda); head(loc.r.coda2, 20); length(loc.r.coda2)

#loc R em palavras com 2 R
loc.r.2<-para.tonicidade2[espacos.vazios]
loc.r.coda3<-gregexpr("r", loc.r.2, ignore.case=F); head(loc.r.coda3, 11) #locs 2 rs
loc.r.coda4<-vector()
for (i in 1:length(loc.r.coda3)){
  loc.r.coda4[i]<-max(loc.r.coda3[[i]])
}
loc.r.coda4

loc.r.coda2[espacos.vazios]<-loc.r.coda4; head(loc.r.coda2, 20); length(loc.r.coda2)

comp.loc.r.hifens<-list()
for (i in 1:length(loc.r.coda2)){
  comp.loc.r.hifens[[i]]<-loc.r.coda2[i]<loc.hifens[[i]]
}

comp.loc.acento.hifens<-list()
for (i in 1:length(loc.r.coda2)){
  comp.loc.acento.hifens[[i]]<-loc.acento[i]<loc.hifens[[i]]
}

comp.loc.r.acento<-list()
for (i in 1:length(loc.r.coda2)){
  comp.loc.r.acento[[i]]<-comp.loc.acento.hifens[[i]]==comp.loc.r.hifens[[i]]
}

VouF<-vector()
for (i in 1:length(loc.r.coda2)){
  if (length(grep("FALSE", comp.loc.r.acento[[i]]))>0) 
    VouF[i]<-"F"
  else 
    VouF[i]<-"V"
}
head(VouF); length(VouF)

tonicidade<-vector()
for (i in 1:length(para.tonicidade)){
  if (loc.acento[[i]]<0)
    tonicidade[i]<-"atona"
  else 
    if (VouF[i]=="V")
      tonicidade[i]<-"tonica"
    else 
      tonicidade[i]<-"atona"
}
cbind(tonicidade, para.tonicidade)->ee; head(ee, 400)
dados3[,"TONICIDADE"]<-tonicidade; head(dados3)

#### POS.R #####
para.posR<-transc.fonologica2
para.posR2<-vector()
for (i in 1:length(para.posR)){
  if (nchar(para.posR[i])==0) {
    para.posR2[i]<-para.posR[i-1]
  } else {
    para.posR2[i]<-para.posR[i]
  }
}

head(para.posR2, 20); length(para.posR2)

#para separar palavras com 2R...
espacos.vazios<-which(nchar(transc.fonologica2)==0); espacos.vazios; length(espacos.vazios)

N.caracteres.por.palavra<-vector()
for (i in 1:length(para.posR2)){
  N.caracteres.por.palavra[i]<-nchar(para.posR2[i])
}

#loc.r.coda2 = gerado em Tonicidade acima
pos.R<-vector()
for (i in 1:length(para.posR)){
  if (loc.r.coda2[i]<N.caracteres.por.palavra[i])
    pos.R[i]<-"medial"
  else
    if (loc.r.coda2[i]==N.caracteres.por.palavra[i])
      pos.R[i]<-"final"
    else 
      pos.R[i]<-"checar"
}

cbind(pos.R, transc.fonologica2)->cc; head(cc, 50)

dados3[,"POSICAO.R"]<-pos.R; head(dados3)

#para terminar de completar col transc fonologica (ateh aqui, algumas celulas estavam vazias para identificar palavras com mais de 1 R)
dados3[,"TRANSC.FONOLOGICA.SILAC"]<-para.posR2; head(dados3, 20)

####CONT.FON.SEG####
ocorr.cont.seg<-paste(dados3$OCORRENCIA, dados3$CONT.SEGUINTE, sep="")

ocorr.cont.seg<-gsub("<0>|<D>|<H>|<M>|<P>|<R>|<T>|<V>|<X>|\\?|\\)|\\(|\\!|\"|\\'|\\||\\/", "", ocorr.cont.seg)
ocorr.cont.seg <- gsub(" R ", " ... ", ocorr.cont.seg, ignore.case = F)
ocorr.cont.seg <- gsub(" D1 ", " ... ", ocorr.cont.seg, ignore.case = F)
ocorr.cont.seg<-gsub("\\.\\.\\.", " #", ocorr.cont.seg)
ocorr.cont.seg<-gsub(" +", " ", ocorr.cont.seg)
head(ocorr.cont.seg)

transc<-ocorr.cont.seg

setwd(loc_arquivosAuxiliares)
source("silac.R", encoding = "UTF-8")
setwd(loc_output_Amostra)

para.cont.seg<-gsub("'", "", transc.fonologica2)
(loc.r.coda<-gregexpr("r", para.cont.seg, ignore.case=F))

cons<-vector()
for (i in 1:length(para.cont.seg)){
  if (loc.r.coda[[i]][1]==-1){ # casos de segunda ocorrência em palavras com 2R sem ocorrencia de R em coda no CONT.SEG; equivale a num_matches = 0
    cons[i]<-substr(para.cont.seg[i-1], loc.r.coda[[i-1]][2]+2, loc.r.coda[[i-1]][2]+2)
  } else {
    cons[i]<-substr(para.cont.seg[i], loc.r.coda[[i]][1]+2, loc.r.coda[[i]][1]+2)
  }
}

# corrigir casos de segunda ocorrência em palavras com 2R e com ocorrência de R no CONT.SEG 

transc<-dados3$OCORRENCIA
setwd(loc_arquivosAuxiliares)
source("silac.R", encoding = "UTF-8")
setwd(loc_output_Amostra)
ocorrencia.silac<-gsub("'", "", transc.fonologica2)

cont.seg.silac<-dados3$CONT.SEGUINTE
cont.seg.silac<-gsub("<0>|<D>|<H>|<M>|<P>|<R>|<T>|<V>|<X>|\\?|\\)|\\(|\\!|\"|\\'|\\||\\/", "", cont.seg.silac)
cont.seg.silac <- gsub(" R ", " ... ", cont.seg.silac, ignore.case = F)
cont.seg.silac <- gsub(" D1 ", " ... ", cont.seg.silac, ignore.case = F)
cont.seg.silac<-gsub("\\.\\.\\.", " #", cont.seg.silac)
cont.seg.silac<-gsub(" +", " ", cont.seg.silac)
head(cont.seg.silac)
transc <- cont.seg.silac

setwd(loc_arquivosAuxiliares)
source("silac.R", encoding = "UTF-8")
setwd(loc_output_Amostra)

cont.seg.silac<-gsub("'", "", transc.fonologica2)

(loc.sem.r.coda<-gregexpr("r", ocorrencia.silac, ignore.case=F))
num_matches <- sapply(loc.sem.r.coda, function(x) length(x[x > 0]))
unique(num_matches)
candidatos_correcao1 <- which(num_matches == 0)

(loc.sem.r.coda<-gregexpr("r", cont.seg.silac, ignore.case=F))
num_matches <- sapply(loc.sem.r.coda, function(x) length(x[x > 0]))
unique(num_matches)
candidatos_correcao2 <- which(num_matches >= 1)

aCorrigir <- intersect(candidatos_correcao1, candidatos_correcao2) 

for (i in aCorrigir){
    cons[i]<-substr(cont.seg.silac[i], 2, 2)
}

# Corrigir para pausa casos em que R é final e CONT.SEGUINTE começa com D1 ou R - CFS é pausa 
cont.seg.D1 <- grep("^\\s+D1", dados3$CONT.SEGUINTE)
cont.seg.R <- grep("^\\s+R", dados3$CONT.SEGUINTE, ignore.case = F)
candidatos_correcao3 <- sort(c(cont.seg.D1, cont.seg.R))
candidatos_correcao4 <- which(dados3$POSICAO.R == "final")
aCorrigir <- intersect(candidatos_correcao3, candidatos_correcao4)
cons[aCorrigir] <- "#"

# corrigir manualmente casos como "perspectiva", em que r não está na coda absoluta da palavra (do dicionário v6, as palavras são: perspectiva(s), rangers e rappers)
palavrasRS <- which(dados3$ITEM.LEXICAL %in% c("perspectiva", "perspectivas", "rangers", "rappers"))
cons[palavrasRS] <- "s"

dados3[,"CONT.FON.SEG"]<-cons; head(dados3)

#### CONT.FON.PREC e CLASSE.MORFOLOGICA ####

# carregar df com PalavrasR 
# N.B.: atualizar arquivos com dfs se novas palavras forem incluídas no dicionário do MFA
# A classificação de classe morfológica foi feita com ajuda do DeepSeek, a partir do prompt: "Peço que classifique as palavras no arquivo de acordo com sua classe morfológica (substantivo, adjetivo, verbo etc.). Caso a palavra possa pertencer a mais de uma classe, coloque todas as classes separadas por hífen. Por exemplo, a palavra "acerto" pode ser substantivo ou verbo; nesse caso, o output deve ser "substantivo-verbo".  Além disso, se a palavra for um verbo no infinitivo, como "falar", diferencie essa categoria como "verboInf". Se for um verbo conjugado, classifique como "verboConj". No caso de substantivos, diferencie substantivos comuns de nomes próprios, classificando os primeiros como "substantivo" e os segundos como "substantivoProprio". Vc pode gerar um arquivo csv como output, que contenha, na primeira coluna, a palavra original, e na segunda coluna a classe morfológica?". Precisa ser revisada. 
setwd(loc_arquivosAuxiliares)
dfR <- read_csv2("dfPalavrasR-CFP-POS.csv") # df gerado pelo script _05script-adaptDictMFA-silac-pCodificacaoCFP, a partir do dicionário do MFA (atual = v6)
setwd(loc_output_Amostra)
#View(dfR)

dados4 <- dados3
dados4[, "ITEM.LEXICAL"] <- tolower(dados4[, "ITEM.LEXICAL"])

pCodifCFP <- left_join(dados4, dfR, by = 'ITEM.LEXICAL')
dados4[, "CONT.FON.PREC"] <- pCodifCFP[, "CONT.FON.PREC.y"]
dados4[, "CLASSE.MORFOLOGICA"] <- pCodifCFP[, "CLASSE.MORFOLOGICA.y"]

# Codificar corretamente dados de palavras com dois R
loc.dados2Rdif <- grep("\\b..\\b", dados4$CONT.FON.PREC)
length(loc.dados2Rdif)/2 # resultado tem que ser número inteiro (i.e., total deve ser par); se não for inteiro, codificar manualmente
View(dados4[loc.dados2Rdif,])

# Pegar apenas a primeira ocorrência das palavras com 2R
sequencia_impares <- seq(from = 1, to = length(loc.dados2Rdif)-1, by = 2)
loc1token.dados2Rdif <- loc.dados2Rdif[sequencia_impares]

# transformar sequência de caracteres duplos numa única sequência
temp <- paste(dados4[loc1token.dados2Rdif,"CONT.FON.PREC"], collapse = "")
# quebrar cada caractere e colocá-los num vetor
pCONT.FON.PREC2Rs <- unlist(strsplit(temp, ""))
dados4[loc.dados2Rdif, "CONT.FON.PREC"] <- pCONT.FON.PREC2Rs

#### LOCALIZACAO formatar #### 
# Aplicar funcao converter_tempo ao vetor de localização
tempos_formatados <- sapply(dados4$LOCALIZACAO, converter_tempo)

dados4[,"LOCALIZACAO"]<-tempos_formatados; head(dados3)


#### CODIFICAR VPs SOCIAIS  #### 
dados4$ARQUIVO <- gsub(" ", "", dados4$ARQUIVO)
dados4$AMOSTRA <- gsub(" ", "", dados4$AMOSTRA)
setwd(loc_arquivosAuxiliares)
dadosInfs <- read_csv("dadosINFS-varSociais.csv", ) %>% 
  select(., AMOSTRA:CLASSE_SOCIAL, QUAL_AUDIO)

pCodifVarSociais <- left_join(dados4, dadosInfs, by = 'ARQUIVO')
dados4[, "ESTADO"] <- pCodifVarSociais[, "ESTADO.y"]
dados4[, "GENERO"] <- pCodifVarSociais[, "GENERO.y"]
dados4[, "IDADE"] <- pCodifVarSociais[, "IDADE.y"]
dados4[, "FAIXA_ETARIA"] <- pCodifVarSociais[, "FAIXA_ETARIA.y"]
dados4[, "ESCOLARIDADE"] <- pCodifVarSociais[, "ESCOLARIDADE.y"]
dados4[, "IDADE_MIGRACAO"] <- pCodifVarSociais[, "IDADE_MIGRACAO.y"]
dados4[, "IDADE_MIGRACAO_fat"] <- pCodifVarSociais[, "IDADE_MIGRACAO_fat.y"]
dados4[, "TEMPO_RESIDENCIA"] <- pCodifVarSociais[, "TEMPO_RESIDENCIA.y"]
dados4[, "TEMPO_RESIDENCIA_fat"] <- pCodifVarSociais[, "TEMPO_RESIDENCIA_fat.y"]
dados4[, "INDICE_SOCIO"] <- pCodifVarSociais[, "INDICE_SOCIO.y"]
dados4[, "CLASSE_SOCIAL"] <- pCodifVarSociais[, "CLASSE_SOCIAL.y"]
dados4[, "QUAL_AUDIO"] <- pCodifVarSociais[, "QUAL_AUDIO.y"]


setwd(loc_output_Amostra)

#which(dados4$IDADE_MIGRACAO == NA)


#dados5 <- dados4 %>% replace_na(list(IDADE = "nc", y = "nc"))


remover <- which(dados4$CONT.FON.SEG %in% c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U", "X"))
dados5 <- dados4[-remover, ]

colnames(dados5) <- c("VD", "CONT_PREC", "OCORRENCIA", "CONT_SEGUINTE", "ITEM_LEXICAL", "TRANSC_FONOLOGICA_SILAC", "CONT_FON_PREC", "CONT_FON_SEG", "TONICIDADE", "POSICAO_R", "CLASSE_MORFOLOGICA", "ARQUIVO", "LOCALIZACAO", "AMOSTRA", "ESTADO", "GENERO", "IDADE", "FAIXA_ETARIA", "ESCOLARIDADE", "IDADE_MIGRACAO", "IDADE_MIGRACAO_fat", "TEMPO_RESIDENCIA", "TEMPO_RESIDENCIA_fat", "INDICE_SOCIO", "CLASSE_SOCIAL", "QUAL_AUDIO")



### Exportar planilha codificada ####
# apagar versao temporaria
file.remove(file)

# exportar em formato xlsx
file2 <- paste("dadosR_", nomeAmostra, "-planilha.xlsx", sep="")

N <- 1:nrow(dados5)
dados5 <- cbind(N, dados5)
dados5$ARQUIVO <- gsub(".eaf", "", dados5$ARQUIVO)
dados5$ARQUIVO <- gsub(" ", "", dados5$ARQUIVO)
dados5$VD <- gsub(" ", "", dados5$VD)

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
writeData(wb, 1, dados5, colNames = T)
#writeData(wb, 2, codigos, colNames = T) # possivelmente colocar variaveis sociais aqui
setColWidths(wb, sheet = 1, cols = 1:ncol(dados5), widths = "auto")
addFilter(wb, 1, row = 1, cols = 1:ncol(dados5))
addStyle(wb, 1, cols = 3, style = right, rows = 1:nrow(dados5)+1) # cont precedente
addStyle(wb, 1, cols = 4, style = center, rows = 1:nrow(dados5)+1) # ocorrencia
addStyle(wb, 1, cols = 5, style = left, rows = 1:nrow(dados5)+1) # cont seguinte
for(i in 6:ncol(dados5)){
  addStyle(wb, 1, cols = i, style = center, rows = 1:nrow(dados5)+1)
}
## Freeze Panes
freezePane(wb, "Sheet 1" ,  firstRow = T, firstCol = F)
## Save workbook
saveWorkbook(wb, file2, overwrite = TRUE)


fim <- Sys.time()
fim - comeco

#rm(list=setdiff(ls(), c("converter_tempo", "loc_Amostra", "loc_output_Amostra", "loc_arquivosAuxiliares", "Amostra_R", "Amostra", "dfR", "dados4")))

beep()
