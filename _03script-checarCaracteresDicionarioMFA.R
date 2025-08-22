# Script para checar se o dicionário somente contém caracteres permitidos para o modelo portuguese_mfa

# carregar arquivo do dicionário MFA
setwd("~/MFA/pretrained_models/dictionary")
dict <- read.table("Dicionario_ProjCoesao_v6.txt", header=F, sep="\t", quote="")
View(dict)

dict[,2] <- gsub("\\s+", "", dict[,2])


caracteres_permitidos <- c("a", "b", "c", "d", "dʒ", "e", "f", "i", "k", "l", "m", "n", "o", "p", "s", "t", "tʃ", "u", "v", "w", "x", "z", "ð", "ɐ", "ɔ", "ɛ", "ɟ", "ɡ", "ɣ", "ɨ", "ɲ", "ɾ", "ʁ", "ʃ", "ʎ", "ʒ", "β", "ĩ", "j", "j̃", "ẽ", "õ", "ũ", "w̃", "ɐ̃")

# Unir o conteúdo em uma única string e extrair os caracteres únicos
todos_caracteres <- unique(unlist(strsplit(paste(dict[,2], collapse = ""), "")))

# Encontrar caracteres não permitidos
nao_permitidos <- setdiff(todos_caracteres, unlist(strsplit(caracteres_permitidos, "")))
nao_permitidos
