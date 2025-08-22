# Script para renomear múltiplos arquivos em um diretório
# set/2024

# Definir diretório de trabalho
setwd("C:/Users/XPS/Desktop/input")

# Definir nomes antigos e nomes novos
nomesOrig <- dir(getwd())
nomesNovos <- gsub("EO_", "", nomesOrig)

# Renomear
file.rename(from = nomesOrig, to = nomesNovos)

