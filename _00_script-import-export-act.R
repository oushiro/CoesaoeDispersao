# Script para importar e exportar arquivos de diferentes formatos (eaf, TextGrid), tanto arquivos únicos quanto corpora 
# set/2024

## N.B.: importar e exportar corpora faz com que se perca a informação de Participant e Annotator (imagino que importar e exportar arquivos individuais tb)

library(act)

## Corpora ####
# definir como diretório de trabalho pasta em que estão arquivos a serem convertidos
setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/Testes_scripts/_ProjetoCeD_Codificadas_pTestes/testes-phonfieldwork/PORTAL_todasCodificacoes")

corpus <- corpus_new(getwd())

names(corpus@transcripts)

# exportar arquivos

corpus_export(corpus, outputFolder = "eaf", 
              filterTierNames = c("S1", "D1"),
              formats = "textgrid")

## Arquivos isolados ####

# setwd()

transc <- import_eaf("EO_CPS_EdmundoS.eaf")

export_eaf(transc, "EO_CPS_EdmundoS.eaf")
# export_textgrid()

## Tier rename


# definir diretório de trabalho
setwd("C:/Users/XPS/Desktop/_ProjetoCeD_Codificadas_pTestes/ProjetoCoesaoeDispersao_Amostra2/Amostra2_codificacoes_Pret")

corpus <- corpus_new(getwd())

names(corpus@transcripts)
info_summarized(corpus)$tier.names

corpus <- tiers_rename(corpus, "Dados contextuais", "Dados Contextuais", filterTranscriptNames = NULL)
info_summarized(corpus)$tier.names


corpus_export(corpus, outputFolder = "TrilhasRenomeadas", 
              #              filterTierNames = "S1",
              formats = "eaf")


