# Script para exportar, do ELAN, trilha S1 para textgrids para rodar MFA
# set/2024

library(act)
library(beepr)

# definir diretório de trabalho em que está amostra completa
setwd("C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/ProjetoCoesaoeDispersao_Amostra2/Amostra2_todasCodificacoes")
#setwd("C:/Users/XPS/Desktop/act_testes")

# Definir como output o diretório input_AMOSTRA (para input no MFA)
output <- "C:/Users/XPS/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/input_Amostra2"
#output <- "C:/Users/XPS/Desktop/act_testes"

corpus <- corpus_new(getwd())
names(corpus@transcripts)
#info_summarized(corpus)$transcript.names

# renomear arquivos
oldNames <- names(corpus@transcripts)
newNames <- gsub("_codif_EORS", "", oldNames)

corpus <- act::transcripts_rename(corpus, newTranscriptNames = newNames)

names(corpus@transcripts)

# apagar caracteres especiais
corpus <- annotations_replace_copy(corpus, "\\(es\\)", "", filterTierNames = "S1")

corpus <- annotations_replace_copy(corpus, "\\(vo\\)", "", filterTierNames = "S1")

corpus <- annotations_replace_copy(corpus, "\\(|\\)|\\{|\\}|\\'|\\[|\\]|\\!|\\.|\\/|\\?|\"", "", filterTierNames = "S1")


# filtrar trilha S1

corpus_export(corpus, outputFolder = output, 
              filterTierNames = "S1",
              formats = "textgrid")

beep()
