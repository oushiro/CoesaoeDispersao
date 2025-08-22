# Script para importar vários eafs, importar trilhas específicas e exportar arquivo único com todas as trilhas
# set/2024

# N.B.: Em todas as pastas, número de arquivos deve ser o mesmo e devem estar na mesma ordem, caso contrário serão juntados arquivos de gravações diferentes

library(phonfieldwork)
library(act)
library(tidyverse)
library(beepr)

rm(list = ls())

comeco <- Sys.time()

# definir diretórios de trabalho em que se localizam arquivos de interesse para juntar trilhas
locCodifEO <- "C:/Users/oushiro/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/ProjetoCoesaoeDispersao_AmostraMourao/AmostraMourao_codificacoes_Pret"

locCodifR <- "C:/Users/oushiro/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/ProjetoCoesaoeDispersao_AmostraMourao/AmostraMourao_codificacoes_R"

locCodifS <- "C:/Users/oushiro/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/ProjetoCoesaoeDispersao_AmostraMourao/AmostraMourao_codificacoes_S"

locExportCodifTodos <- "C:/Users/oushiro/Dropbox/_PESQUISAS/PROJETO_COESAOeDISPERSAO/Corpus_ProjetoCoesaoeDispersao/_ProjetoCeD_CorpusFinal/ProjetoCoesaoeDispersao_AmostraMourao"

### ---> pegar todos os nomes das trilhas
#names(corpus@transcripts)
#info_summarized(corpus)$tier.names


# definir diretório de trabalho e listar files do corpus1 (EO_)
setwd(locCodifS)
filesS <- dir(pattern = ".eaf")

# Criar arquivo base (usando trilha S para isso)
base1 <- list()

for(i in 1:length(filesS)){
  # carregar arquivos na lista
  base1[[i]] <- eaf_to_df(filesS[i])
  # retirar trilhas R e S do arquivo EO_
  loc_trilha_EOR <- which(base1[[i]]$tier_name %in% c("Pret", "R")) # comentar essa linha para amostra 2, que não tem essas trilhas
  if(length(loc_trilha_EOR) == 0){
    next
  } else {
    base1[[i]] <- base1[[i]][-loc_trilha_EOR, ] # comentar essa linha para amostra 2, que não tem essas trilhas
  }
}
rm(loc_trilha_EOR)
beep()

#View(base1[[1]])

#unique(base1[[1]]$tier_name)

# definir diretório de trabalho e listar files do corpus2 (EO_) ####
setwd(locCodifEO)
filesEO <- dir(pattern = ".eaf")

baseEO <- list()

for(i in 1:length(filesEO)){
  # carregar arquivos na lista
  baseEO[[i]] <- eaf_to_df(filesEO[i])
  # importar arquivo de update (EO) e separar apenas trilha de interesse (EO) ####
  loc_trilha_EO_Codif <- which(baseEO[[i]]$tier_name == "Pret")
  baseEO[[i]] <- baseEO[[i]][loc_trilha_EO_Codif, ]
}
rm(loc_trilha_EO_Codif)
beep()

# definir diretório de trabalho e listar files do corpus3 (R_)
setwd(locCodifR)
filesR <- dir(pattern = ".eaf")

baseR <- list()

for(i in 1:length(filesR)){
  # carregar arquivos na lista
  baseR[[i]] <- eaf_to_df(filesR[i])
  # importar arquivo de update (R) e separar apenas trilha de interesse (R) ####
  loc_trilha_R_Codif <- which(baseR[[i]]$tier_name %in% c("R", "TD"))
  baseR[[i]] <- baseR[[i]][loc_trilha_R_Codif, ]
}
rm(loc_trilha_R_Codif)

beep()

# criar novo df com EO, R e S codificados e exportá-lo ####
setwd(locExportCodifTodos)

newFileNamesAct <- paste0(gsub("EO_|\\.eaf", "", filesEO), "_codif_EORS", ".eaf") 
newFileNamesphonfieldwork <- paste0(gsub("EO_|\\.eaf", "", filesEO), "_codif_EORS-phon", ".eaf") 

### ---> pegar todos os nomes das trilhas e definir sua ordem no arquivo exportado

# definir ordem das trilhas no ELAN (1 = primeira trilha etc.)
df_ref <- tibble::tribble(
  ~tier_ELAN, ~tier_name,
  1, "Dados Contextuais",
  2, "D1",
  3, "S1",
  4, "S2",
  5, "S3",
  6, "Pret",
  7, "R",
  8, "S",
  9, "TD"
)

file.create(newFileNamesphonfieldwork)
finalDFs <- list()

for(i in 1:length(filesEO)){
  # report progress
  cat("Processing file", i, "out of", length(filesEO), newFileNamesAct[i], "...\n")
  
  # juntar trilhas num único df
  finalDFs[[i]] <- rbind(base1[[i]], baseEO[[i]], baseR[[i]])
  # selecionar colunas relevantes do df 
  finalDFs[[i]] <- select(finalDFs[[i]], c("tier", "id", "tier_name", "content", "time_start", "time_end", "tier_type", "tier_ref", "event_local_id", "dependent_on"))
  
  # reordenar trilhas
  finalDFs[[i]] <- finalDFs[[i]] %>% 
    left_join(df_ref, by = "tier_name") %>% 
    mutate(tier = coalesce(tier, tier_ELAN)) %>% 
    select(tier_ELAN, id:dependent_on) 
  colnames(finalDFs[[i]])[1] ="tier"
  
  # exportar via phonfieldwork
  df_to_eaf(finalDFs[[i]], newFileNamesphonfieldwork[i]) # esta versão não abre, por algum motivo...
  # importar o arquivo via act para exportá-lo novamente
  transcCompleta <- act::import_eaf(newFileNamesphonfieldwork[i])
  act::export_eaf(transcCompleta, newFileNamesAct[i])
  # remover versão temporária do phonfieldwork
  file.remove(newFileNamesphonfieldwork[i])
  
}

fim <- Sys.time()
fim-comeco

beep()

