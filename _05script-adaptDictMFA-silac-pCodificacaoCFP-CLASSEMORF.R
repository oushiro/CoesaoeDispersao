# Script para pegar informações de contexto fônico precedente do dicionário do MFA (do Projeto C&D) para codificação automática de CONT.FON.PREC (diferenciar vogais media altas e media baixas); e para pegar informações das palavras que contêm a variável dependente e gerar a lista de palavras para classificação morfológica, feita no DeepSeek

rm(list = ls())
library(tidyverse)

setwd("~/MFA/pretrained_models/dictionary")
dict <- read.table("Dicionario_ProjCoesao_v6.txt", header=F, sep="\t", quote="")
#View(dict)

# ou para S: dicionário com vogais nasais
setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/Testes_scripts/_ProjetoCeD_Codificadas_pTestes/Analises_DadosCategoricos")
dictN <- read.table("Dicionario_ProjCoesao_v6-pVogaisNasais.txt", header=F, sep="\t", quote="")

dict[,2] <- gsub("\\s+", "", dict[,2])

# Pret: extrair apenas a primeira coluna, de palavras
pPalavrasPret <- dict[,1]
## aplicar silac 
transc <- pPalavrasPret
source("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/silacpret-adapt7-ago2018-mar2025.R", encoding = "UTF-8")

transcSilac <- transc.fonologica2
#View(cbind(pPalavrasPret, transcSilac))
loc.pal.pret <- grep("<", transcSilac)
palavrasPret <- pPalavrasPret[loc.pal.pret]

setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal")
cat(palavrasPret, file = "palavrasPret-pDeepSeek-POS-checagem-apagar.txt", sep = "\n")

# R em coda: x -> r
grep("x\\b", dict[,2], value = T)
dict[,2] <- gsub("x\\b", "r", dict[,2])

grep("x[^aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ]", dict[,2], value = T)

dict[,2] <- gsub("x([^aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ])", "r\\1", dict[,2])

# S em coda: s -> $
grep("s\\b", dict[,2], value = T)
dict[,2] <- gsub("s\\b", "$", dict[,2])

grep("s[^aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ]", dict[,2], value = T)
dict[,2] <- gsub("s([^aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ])", "$\\1", dict[,2])


setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/Testes_scripts/_ProjetoCeD_Codificadas_pTestes/Analises_DadosCategoricos")

write.table(dict, file = "dict2.txt", quote = F, sep="\t", row.names = F, col.names = F, fileEncoding = "UTF-8", append = F)

#######Palavras com R em coda######## 
setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/Testes_scripts/_ProjetoCeD_Codificadas_pTestes/Analises_DadosCategoricos")

dict <- read.table("dict2.txt") # criado em script 06 adaptDictMFA-silac
View(dict)

## Criar df com codificação de palavras com R em coda ####
locR <- grep("[aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ]r", dict[,2])
palavrasR <- dict[locR, ]
#View(palavrasR)

## CODIFICAR CONT.FON.PREC##
para.vogal.prec <- palavrasR[,2]

# verificar se há palavras com duas sequências [VOGAL]r
(loc.palavras2R <- grep("[aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ]r.*?[aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ]r", para.vogal.prec, ignore.case=F))
para.vogal.prec[loc.palavras2R]

# ver localizacao de palavras com R
(loc.r.coda<-gregexpr("[aeẽiĩjj̃oõuũww̃ɐɐ̃ɔɛ]r", para.vogal.prec, ignore.case=F))

vogais <- vector()
for (i in 1:length(para.vogal.prec)){
  # se a palavra tiver dois ocorrências de R em coda...
    if (i %in% loc.palavras2R == T) {
    vogais[i] <- paste0(substr(para.vogal.prec[i], loc.r.coda[[i]][1], loc.r.coda[[i]][1]), substr(para.vogal.prec[i], loc.r.coda[[i]][2], loc.r.coda[[i]][2]))
  } else {
  # se tiver apenas uma ocorrências de R em coda...
    vogais[i]<-substr(para.vogal.prec[i], loc.r.coda[[i]][1], loc.r.coda[[i]][1])
  }
}

#View(cbind(palavrasR, vogais))

# inserir "manual" para palavras com duas pronúncias de R
sort(table(palavrasR[,1]), decreasing = T) # pegar palavras que ocorrem 2 vezes
aa <- which(palavrasR[,1] %in% c("acerca", "acerte", "acerto", "colher")); aa
vogais[aa] <- "duas pronúncias"

# tirar uma ocorrência de palavras com duas pronúncias; verificar valores em aa
palavrasR <- palavrasR[-c(56, 63, 66, 606), ]
vogais <- vogais[-c(56, 63, 66, 606)]


# trocar é por 3
vogais <- gsub("ɛ", "3", vogais)
CONT.FON.PREC <- vogais

# trocar ɔ por c
vogais <- gsub("ɔ", "c", vogais)
CONT.FON.PREC <- vogais

# se palavra com dois R contém a mesma vogal nos dois casos, mudar para apenas uma vogal
palavrasR[which(vogais %in% c("aa", "ee", "ii", "oo", "uu", "33", "cc")), ]
vogais <- gsub("aa", "a", vogais)
vogais <- gsub("ee", "e", vogais)
vogais <- gsub("ii", "i", vogais)
vogais <- gsub("oo", "o", vogais)
vogais <- gsub("uu", "u", vogais)
vogais <- gsub("33", "3", vogais)
vogais <- gsub("cc", "c", vogais)
CONT.FON.PREC <- vogais

dfPalavrasR <- cbind(palavrasR, CONT.FON.PREC)
#View(dfPalavrasR)
colnames(dfPalavrasR)[1] <- "ITEM.LEXICAL"

# exportar dfPalavrasR para csv
write_csv(dfPalavrasR[, c(1,3)], file = "dfPalavrasR.csv", col_names = T)


#######Palavras com S em coda######## 
# ou para S: dicionário com vogais nasais
setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/Testes_scripts/_ProjetoCeD_Codificadas_pTestes/Analises_DadosCategoricos")
# N.B.: a versão "pVogaisNasais" troca o símbolo de til por vogais maiusculas, como no silac. Se o dicionário do C&D for atualizado, este arquivo também precisa ser atualizado.
dictN <- read.table("Dicionario_ProjCoesao_v6-pVogaisNasais.txt", header=F, sep="\t", quote="")

dictN[,2] <- gsub("\\s+", "", dictN[,2])

# R em coda: x -> r
grep("x\\b", dictN[,2], value = T)
dictN[,2] <- gsub("x\\b", "r", dictN[,2])

grep("x[^aAeEiIjJoOuUwWɐɔɛ]", dictN[,2], value = T)

dictN[,2] <- gsub("x([^aAeEiIjJoOuUwWɐɔɛ])", "r\\1", dictN[,2])

# S em coda: s -> $
grep("s\\b", dictN[,2], value = T)
dictN[,2] <- gsub("s\\b", "$", dictN[,2])

grep("s[^aAeEiIjJoOuUwWɐɔɛ]", dictN[,2], value = T)
dictN[,2] <- gsub("s([^aAeEiIjJoOuUwWɐɔɛ])", "$\\1", dictN[,2])


setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/Testes_scripts/_ProjetoCeD_Codificadas_pTestes/Analises_DadosCategoricos")

write.table(dictN, file = "dictN2.txt", quote = F, sep="\t", row.names = F, col.names = F, fileEncoding = "UTF-8", append = F)


setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/Testes_scripts/_ProjetoCeD_Codificadas_pTestes/Analises_DadosCategoricos")

dictN <- read.table("dictN2.txt") # criado em script 06 adaptDictMFA-silac
View(dictN)

#grep("ẽ", dictN[,c(1,2)])

## Criar df com codificação de palavras com S em coda ####
locS <- grep("[aAeEiIjJoOuUwWɐɔɛ]\\$", dictN[,2])

#cat(sort(unique(unlist(str_split(dictN[,2], "")))), file = "caracteres.txt", eol = "\n")
#sort(unique(unlist(str_split(dictN[,2], ""))))

#locS <- grep("[ẽĩj̃õũw̃ɐ̃]\\$", dictN[,2])
#locS <- grep("[aeijouwɐɔɛ]\\$", dictN[,2])

palavrasS <- dictN[locS, ]
#View(palavrasR)

## CODIFICAR CONT.FON.PREC##
para.vogal.prec <- palavrasS[,2]

# verificar se há palavras com duas sequências [VOGAL]$
(loc.palavras2S <- grep("[aAeEiIjJoOuUwWɐɔɛ]\\$.*?[aAeEiIjJoOuUwWɐɔɛ]\\$", para.vogal.prec, ignore.case=F))
para.vogal.prec[loc.palavras2S]

# ver localizacao de palavras com S
(loc.s.coda<-gregexpr("[aAeEiIjJoOuUwWɐɔɛ]\\$", para.vogal.prec, ignore.case=F))


vogais <- vector()
for (i in 1:length(para.vogal.prec)){
  # se a palavra tiver dois ocorrências de S em coda...
  if (i %in% loc.palavras2S == T) {
    vogais[i] <- paste0(substr(para.vogal.prec[i], loc.s.coda[[i]][1], loc.s.coda[[i]][1]), substr(para.vogal.prec[i], loc.s.coda[[i]][2], loc.s.coda[[i]][2]))
  } else {
    # se tiver apenas uma ocorrências de S em coda...
    vogais[i]<-substr(para.vogal.prec[i], loc.s.coda[[i]][1], loc.s.coda[[i]][1])
  }
}

#para.vogal.prec[which(vogais == "̃")]
#View(cbind(palavrasS, vogais))

# inserir "manual" para palavras com duas pronúncias de S
sort(table(palavrasS[,1]), decreasing = T) # pegar palavras que ocorrem 2 vezes
aa <- which(palavrasS[,1] %in% c("desespero", "desgosto", "desprezo", "escova", "gosto")); aa
vogais[aa] <- "duas pronúncias"

# tirar uma ocorrência de palavras com duas pronúncias; verificar valores em aa
palavrasS <- palavrasS[-c(1128, 1151, 1225, 1624, 2466), ]
vogais <- vogais[-c(1128, 1151, 1225, 1624, 2466)]


# trocar é por 3
vogais <- gsub("ɛ", "3", vogais)

# trocar ɔ por c
vogais <- gsub("ɔ", "c", vogais)
CONT.FON.PREC <- vogais

# se palavra com dois R contém a mesma vogal nos dois casos, mudar para apenas uma vogal
palavrasS[which(vogais %in% c("ii", "uu", "ee", "33", "aa", "ee", "oo", "cc", "ɐɐ")), ]
# várias das combinações abaixo não ocorrem, mas just in case, mantive
vogais <- gsub("aa", "a", vogais)
vogais <- gsub("ee", "e", vogais)
vogais <- gsub("ii", "i", vogais)
vogais <- gsub("oo", "o", vogais)
vogais <- gsub("uu", "u", vogais)
vogais <- gsub("cc", "c", vogais)
vogais <- gsub("33", "3", vogais)
CONT.FON.PREC <- vogais

#### TRANSC.FONOLOGICA.SILAC ####
transc<-palavrasS[,1]

source("silac.R", encoding = "UTF-8")

###TONICIDADE####
grep("s\\b", transc.fonologica2, value = T)
transc.fonologica3 <- gsub("s\\b", "$", transc.fonologica2)
para.tonicidade<-transc.fonologica3
para.tonicidade2<-vector()
for (i in 1:length(para.tonicidade)){
  if (nchar(para.tonicidade[i])==0) {
    para.tonicidade2[i]<-para.tonicidade[i-1]
  } else {
    para.tonicidade2[i]<-para.tonicidade[i]
  }
}

head(para.tonicidade2, 20); length(para.tonicidade2)

#para separar palavras com 2S...
espacos.vazios<-which(nchar(transc.fonologica2)==0); espacos.vazios; length(espacos.vazios)

(loc.acento<-regexpr("'", para.tonicidade2, ignore.case=F))
loc.hifens<-gregexpr("-", para.tonicidade2, ignore.case=F); head(loc.hifens)

#loc S em palavras com 1 S
loc.s.coda<-regexpr("\\$", para.tonicidade, ignore.case=F); head(loc.s.coda, 11) #so 1os rs
loc.s.coda2<-unlist(loc.s.coda); head(loc.s.coda2, 20); length(loc.s.coda2)

#loc S em palavras com 2 S
loc.s.2<-para.tonicidade2[espacos.vazios]
loc.s.coda3<-gregexpr("\\$", loc.s.2, ignore.case=F); head(loc.s.coda3, 11) #locs 2 Ss
loc.s.coda4<-vector()
for (i in 1:length(loc.s.coda3)){
  loc.s.coda4[i]<-max(loc.s.coda3[[i]])
}
loc.s.coda4

loc.s.coda2[espacos.vazios]<-loc.s.coda4; head(loc.s.coda2, 20); length(loc.s.coda2)

#maior.hifen<-vector()
#for (i in 1:length(loc.hifens)){
#  maior.hifen[i]<-max(loc.hifens[[i]])
#}

#loc.acento2<-unlist(loc.acento); length(loc.acento2)

comp.loc.s.hifens<-list()
for (i in 1:length(loc.s.coda2)){
  comp.loc.s.hifens[[i]]<-loc.s.coda2[i]<loc.hifens[[i]]
}

comp.loc.acento.hifens<-list()
for (i in 1:length(loc.s.coda2)){
  comp.loc.acento.hifens[[i]]<-loc.acento[i]<loc.hifens[[i]]
}

comp.loc.s.acento<-list()
for (i in 1:length(loc.s.coda2)){
  comp.loc.s.acento[[i]]<-comp.loc.acento.hifens[[i]]==comp.loc.s.hifens[[i]]
}

VouF<-vector()
for (i in 1:length(loc.s.coda2)){
  if (length(grep("FALSE", comp.loc.s.acento[[i]]))>0) 
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
## --> vetor `tonicidade`

###POS.S#####
para.posS<-transc.fonologica2
para.posS2<-vector()
for (i in 1:length(para.posS)){
  if (nchar(para.posS[i])==0) {
    para.posS2[i]<-para.posS[i-1]
  } else {
    para.posS2[i]<-para.posS[i]
  }
}

head(para.posS2, 20); length(para.posS2)

#para separar palavras com 2S...
espacos.vazios<-which(nchar(transc.fonologica2)==0); espacos.vazios; length(espacos.vazios)

N.caracteres.por.palavra<-vector()
for (i in 1:length(para.posS2)){
  N.caracteres.por.palavra[i]<-nchar(para.posS2[i])
}

#loc.s.coda2 = gerado em Tonicidade acima
pos.S<-vector()
for (i in 1:length(para.posS)){
  if (loc.s.coda2[i]<N.caracteres.por.palavra[i])
    pos.S[i]<-"medial"
  else
    if (loc.s.coda2[i]==N.caracteres.por.palavra[i])
      pos.S[i]<-"final"
    else 
      pos.S[i]<-"checar"
}

cbind(pos.S, transc.fonologica2)->cc; head(cc, 50)


which(tonicidade == "")
which(pos.S == "")

## Exportar dfPalavrasS ####

dfPalavrasS <- cbind(palavrasS, CONT.FON.PREC, tonicidade, pos.S)
colnames(dfPalavrasS)[1] <- "ITEM.LEXICAL"

loc.O.postonico <- which(dfPalavrasS$tonicidade == "atona" & pos.S == "final" & CONT.FON.PREC == "u")
dfPalavrasS[loc.O.postonico, ]
loc.pal.us <- grep("us\\b", dfPalavrasS$ITEM.LEXICAL)
dfPalavrasS[loc.pal.us, ]
aCorrigir <- setdiff(loc.O.postonico, loc.pal.us)
dfPalavrasS[aCorrigir, ]
dfPalavrasS[loc.O.postonico, "CONT.FON.PREC"] <- "o"

loc.E.postonico <- which(dfPalavrasS$tonicidade == "atona" & pos.S == "final" & CONT.FON.PREC == "i")
dfPalavrasS[loc.E.postonico, ]
loc.pal.is <- grep("is\\b", dfPalavrasS$ITEM.LEXICAL)
aCorrigir <- setdiff(loc.E.postonico, loc.pal.is)
dfPalavrasS[aCorrigir, "CONT.FON.PREC"] <- "e"

loc.EN.postonico <- which(dfPalavrasS$tonicidade == "atona" & pos.S == "final" & CONT.FON.PREC == "J")
dfPalavrasS[loc.EN.postonico, ]
dfPalavrasS[loc.EN.postonico, "CONT.FON.PREC"] <- "E"

#View(dfPalavrasS)

# exportar dfPalavrasR para csv
write_csv(dfPalavrasS[, c(1,3)], file = "dfPalavrasS-CFP-POS.csv", col_names = T)

# para status morfológico de S <----- TODO pegar somente palavras com S em meio de palavra pra checar se é sempre raiz
loc.palavrasSfinal <- grep("\\$$", palavrasS[,2], value = F)
palavrasSfinal <- palavrasS[loc.palavrasSfinal, 1]
cat(palavrasSfinal, file = "palavrasSfinal-pStatusMorfdeS.txt", sep = "\n")





