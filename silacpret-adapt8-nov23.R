###Script para fazer transcricao fonologica e marcar pretonicas dentro de ProcedimentosMetodologicos da pesquisa pos-doc

pretonicas<-c("i", "e", "a", "o", "u") #orais
#pretonicas<-c("e", "o") #orais

####################marcacoes de silabas##################
#marcar - apos todas as vogais
apagar<-gsub("(a|ã|á|â|à|e|ê|é|i|í|o|õ|ó|ô|u|ú)", "\\1-", transc, ignore.case=T)

#ajustar marcacoes #as regras abaixo valem para o corpus do SP2010; outros corpora podem conter outras palavras em que as regras abaixo precisam ser ajustadas
#eliminar consoantes duplas
apagar<-gsub("bb", "b", apagar, ignore.case=T)
apagar<-gsub("dd", "d", apagar, ignore.case=T)
apagar<-gsub("ff", "f", apagar, ignore.case=T)
apagar<-gsub("gg", "g", apagar, ignore.case=T)
apagar<-gsub("hh", "h", apagar, ignore.case=T)
apagar<-gsub("ll", "l", apagar, ignore.case=T)
apagar<-gsub("mm", "m", apagar, ignore.case=T)
apagar<-gsub("nn", "n", apagar, ignore.case=T)
apagar<-gsub("pp", "p", apagar, ignore.case=T)
apagar<-gsub("tt", "t", apagar, ignore.case=T)
apagar<-gsub("zz", "z", apagar, ignore.case=T)
apagar<-gsub("cc", "kI-s", apagar, ignore.case=T)
#ditongos decrescentes
apagar<-gsub("(a|ã|á|â|e|ê|é|o|õ|ó|ô|i|u)-(i|u|y|h)", "\\1\\2", apagar, ignore.case=T)
apagar<-gsub("(a|ã|á|â|e|ê|é|o|õ|ó|ô|i|u)-(i|u|y|h)", "\\1\\2", apagar, ignore.case=T)
#ditongos crescentes
apagar<-gsub("(u|ã|õ)-(a|ã|á|â|e|ê|é|o|õ|ó|ô|i|u|h)", "\\1\\2", apagar, ignore.case=T)
apagar<-gsub("(i)-(a|o)", "\\1\\2", apagar, ignore.case=T)
#silabas com coda
apagar<-gsub("(a|ã|á|â|e|ê|é|i|í|o|õ|ó|ô|u|ú)-(r|s|l|m|n|z|x)(b|c|ç|d|f|g|j|k|p|q|t|v|x|z|s|m|n|l|\\W|$)", "\\1\\2-\\3", apagar, ignore.case=T)
apagar<-gsub("(a|ã|á|â|e|ê|é|i|í|o|õ|ó|ô|u|ú)-([ns])(r)", "\\1\\2-\\3", apagar, ignore.case=T) #<----- novo
apagar<-gsub("i(a|o)(r|n|l)", "i-\\1\\2", apagar, ignore.case=T)
apagar<-gsub("(a|ã|á|â|e|ê|é|i|í|o|õ|ó|ô|u|ú)-(r)(s)(b|c|ç|d|f|g|h|j|k|p|q|t|v|x|z|\\W)", "\\1\\2\\3-\\4", apagar, ignore.case=T)
#silabas com coda seguidas de consoante -s
apagar<-gsub("(a|ã|á|â|e|ê|é|i|í|o|õ|ó|ô|u|ú)-(r)(s)(a|ã|á|â|e|ê|é|i|í|o|õ|ó|ô|u|ú)", "\\1\\2-\\3\\4", apagar, ignore.case=T)
apagar<-gsub("(a|ã|á|â|e|ê|é|i|í|o|õ|ó|ô|u|ú)(n)-(s)(b|c|ç|d|f|g|h|j|k|p|q|t|v|x|z|\\W)", "\\1\\2\\3-\\4", apagar, ignore.case=T)
#encontro consonantal (mudar pra baixo com epentese?)
apagar<-gsub("-(b|c|ç|d|f|g|j|k|p|q|t|v|x|z)(b|c|ç|d|f|g|j|k|p|q|t|v|x|z)", "-\\1-\\2", apagar, ignore.case=T)
#juntar rr, ss, sc/ç, nh, lh
apagar<-gsub("(r)-(r)", "-\\1\\2", apagar, ignore.case=T)
apagar<-gsub("(s)-(s)", "-\\1\\2", apagar, ignore.case=T)
apagar<-gsub("s-c(i|í|e|ê|é)", "-sc\\1", apagar, ignore.case=T)
apagar<-gsub("s-ç(o|a)", "-sç\\1", apagar, ignore.case=T)
apagar<-gsub("(n|l)-h", "-\\1h", apagar, ignore.case=T)
#eliminar hifen de fins de palavra
apagar<-gsub("-(\\W|$)", "\\1", apagar, ignore.case=T)
#excecoes (palavras estrangeiras)
apagar<-gsub("in-gs\\b", "ins", apagar, ignore.case=T)
apagar<-gsub("in-g\\b", "in", apagar, ignore.case=T)
apagar<-gsub("c-k", "k", apagar, ignore.case=T)
#coda complexa (mudar pra cima?)
apagar<-gsub("(u)(a|o)(r|n|l)", "\\1-\\2\\3", apagar, ignore.case = T)
apagar<-gsub("(q|g)u-a(r|n|l)", "\\1ua\\2", apagar, ignore.case = T)
apagar<-gsub("(i|u)ais\\b", "\\1-ais", apagar, ignore.case = T)
apagar<-gsub("uo-", "u-o", apagar, ignore.case=T)
#tritongos e ditongos
apagar<-gsub("([aeo])ia", "\\1i-a", apagar, ignore.case=T)
apagar<-gsub("oio", "oi-o", apagar, ignore.case=T)
apagar<-gsub("ain", "a-in", apagar, ignore.case=T)
apagar<-gsub("([qg])u-í", "\\1uí", apagar, ignore.case=T)
apagar<-gsub("i([ao])-", "i-\\1-", apagar, ignore.case=T)#nao separa em fim de palavra: cirurgia, alegria etc., mas melhor deixar assim para nao ter problemas com ambulância, angústia, anália etc. acentuador pega acento "corretamente" em todos os casos de "ia" final
apagar<-gsub("([bcçdfjklmnprstvxz])u([aeo])", "\\1u-\\2", apagar, ignore.case=T)
apagar<-gsub("([bcçdfjklmnprstvxz])ui([rnmuz])", "\\1u-i\\2", apagar, ignore.case=T)
apagar<-gsub("aiu", "a-iu", apagar, ignore.case=T)
#itens lexicais especificos
apagar<-gsub("mpb", "e-me-pe-bê", apagar, ignore.case=T)
apagar<-gsub("dvd", "de-ve-dê", apagar, ignore.case=T)
apagar<-gsub("cpe", "ci-pi-í", apagar, ignore.case=T)
apagar<-gsub("bahia", "ba-hia", apagar, ignore.case=T)
#epentese
apagar<-gsub("(b|c|d|f|g|k|p|t)s\\b", "\\1Is",apagar)
apagar<-gsub("bst", "bIs-t",apagar)
apagar<-gsub("\\bs([kmpt])", "Is-\\1", apagar, ignore.case=T)
apagar<-gsub("tran-Is-mi-te", "trans-mi-te", apagar)
apagar<-gsub("(b|d|f|g|k|p|t)(b|c|d|f|g|j|k|m|n|p|q|s|t|v|x|z)", "\\1I-\\2", apagar)
apagar<-gsub("c(b|c|d|f|g|j|k|m|n|p|q|s|t|v|x|z)", "kI-\\1", apagar)
apagar<-gsub("-(b|d|f|g|k|p|t)-", "-\\1I-", apagar)
apagar<-gsub("-c-", "-kI-", apagar)
apagar<-gsub("-(s|z)\\b", "\\1", apagar)
apagar<-gsub("reu-", "re-u-", apagar)
apagar<-gsub("iam\\b", "i-am", apagar)
apagar<-gsub("ã-o", "ão", apagar)
apagar<-gsub("quios-", "qui-os-", apagar)
apagar<-gsub("([oae])i([mnr])", "\\1-i\\2", apagar) #<------ novo
apagar<-gsub("iu([mnr])", "i-u\\1", apagar) #<------ novo
apagar<-gsub("([oaeã])(i-nh)", "\\1-\\2", apagar) #<------ novo
apagar<-gsub("([aeo])i([ao])", "\\1i-\\2", apagar) #<------ novo
apagar<-gsub("i([oa])u", "i-\\1u", apagar) #<------ novo
apagar<-gsub("([pash])u([áãâéêíóôú])", "\\1u-\\2", apagar) #<------ novo palavras com vogal acentuada precedida de vogal u como chuá, mauá, tapabuã, usuário; os caracteres especificos se referem aas palavras do SP2010
apagar<-gsub("proi-b", "pro-i-b", apagar) #<------ novo
apagar<-gsub("nui-da-de", "nu-i-da-de", apagar) #<------ novo
apagar<-gsub("jui-za-", "ju-i-za-", apagar) #<------ novo
apagar<-gsub("e-gois-ti", "e-go-is-ti", apagar) #<------ novo
apagar<-gsub("hai-ti", "ha-i-ti", apagar) #<------ novo
apagar<-gsub("hai-ti", "ha-i-ti", apagar) #<------ novo
apagar<-gsub("miu-de", "mi-u-de", apagar) #<------ novo
apagar<-gsub("rui-nh", "ru-i-nh", apagar) #<------ novo
apagar<-gsub("raiz", "ra-iz", apagar) #<------ novo
apagar<-gsub("coha-b", "co-ha-b", apagar) #<------ novo
apagar<-gsub("os([oa])(s)?\\b", "o-s\\1\\2", apagar) #<------ novo
apagar<-gsub("per-Is-p", "pers-p", apagar) #<------ novo
apagar<-gsub("(s-p)\\b", "\\1I", apagar) #<------ novo
apagar<-gsub("u-ni-cam-p", "u-ni-cam-pI", apagar) #<------ novo
apagar<-gsub("fa-a-p", "fa-a-pI", apagar) #<------ novo
apagar<-gsub("in-ter-ne-t", "in-ter-ne-tI", apagar) #<------ novo
apagar<-gsub("i-po-d", "i-po-dI", apagar) #<------ novo
apagar<-gsub("ner-d", "ner-dI", apagar) #<------ novo
apagar<-gsub("chi-p", "chi-pI", apagar) #<------ novo
apagar<-gsub("ca-tI-chu-p", "ca-tI-chu-pI", apagar) #<------ novo
apagar<-gsub("ci-ne-mar-k", "ci-ne-mar-kI", apagar) #<------ novo
apagar<-gsub("cul-t", "cul-tI", apagar) #<------ novo
apagar<-gsub("me-c\\b", "me-kI", apagar) #<------ novo
apagar<-gsub("u-ni-ci-d", "u-ni-ci-dI", apagar) #<------ novo
apagar<-gsub("boyzi", "boy-zi", apagar) #<------ novo
apagar<-gsub("gaydar", "gay-dar", apagar) #<------ novo
apagar<-gsub("gayzi", "gay-zi", apagar) #<------ novo
apagar<-gsub("playboy", "play-boy", apagar) #<------ novo
apagar<-gsub("playground", "play-ground", apagar) #<------ novo
apagar<-gsub("toyo-", "toy-o", apagar) #<------ novo
apagar<-gsub("f(r)?ios\\b", "f\\1i-os", apagar) #<------ novo #fio, frios, desafios
apagar<-gsub("\\bfria(s)?", "fri-a\\1", apagar) #<------ novo
apagar<-gsub("\\bfria(s)?", "fri-a\\1", apagar) #<------ novo
apagar<-gsub("\\bju-di([ao])(s)?", "ju-di-\\1\\2", apagar) #<------ novo

####################tokenizador####################
tokens1<-gsub("(\\W)","\\1", apagar)
tokens2<-unlist(strsplit(tokens1," +"))
length(tokens2)
#cat(tokens2, file="tokens2.txt",sep = "\n")


#############SEPARAR PALAVRAS COM ACENTO GRAFICO#############
palavras.AG<-grep("ã|á|â|ê|é|í|õ|ó|ô|ú", tokens2, value=T); head(palavras.AG) #palavras ja acentuadas
loc.palavras.AG<-grep("ã|á|â|ê|é|í|õ|ó|ô|ú", tokens2, value=F); head(loc.palavras.AG) #localizacao das palavras ja acentuadas em tokens2

palavras.H<-grep("-", tokens2, value=T); head(palavras.H) #palavras de tokens 2 com hifen (com ou sem acento grafico)
loc.palavras.H<-grep("-", tokens2, value=F); head(loc.palavras.H) #localizacao das palavras com hifem em tokens 2 (com ou sem acento grafico)

overlap.AG.H<-which(palavras.AG %in% palavras.H  == T)
overlap.tokens2.AG<-which(tokens2 %in%  palavras.AG[overlap.AG.H]==T) #localizacao das palavras com acento grafico E hifens(polissil) em tokens2 <--------------- grupo 2 (AG>polissil)
palavras.AG.polissilabos<-tokens2[overlap.tokens2.AG]#palavras com acento grafico E hifens(polissil) em tokens2

palavras.sem.H<-tokens2[-loc.palavras.H]; palavras.sem.H #palavras em tokens 2 sem hifen (com ou sem acento grafico)
loc.palavras.sem.H<-which(tokens2 %in% palavras.sem.H == T)#localizacao das palavras sem hifen em tokens 2 (com ou sem acento grafico)

overlap.AG.sem.H<-which(palavras.AG %in% palavras.sem.H  == T)
overlap.tokens2.AG2<-which(tokens2 %in%  palavras.AG[overlap.AG.sem.H]==T) #localizacao das palavras com acento grafico E sem hifens(monossil) em tokens2 <--------------- grupo 1(AG>monossil)
palavras.AG.monossilabos<-tokens2[overlap.tokens2.AG2]#palavras com acento grafico E sem hifens(monossil) em tokens2

#############PALAVRAS SEM ACENTO GRAFICO#############
palavras.sem.AG<-tokens2[-loc.palavras.AG]; palavras.sem.AG #palavras sem acento grafico em tokens2
extensao.tokens2<-1:length(tokens2)
head(loc.palavras.AG)
loc.palavras.sem.AG<-which(extensao.tokens2 %in% loc.palavras.AG == F) #localizacao das palavras sem acento grafico em tokens2

########PALAVRAS SEM ACENTO GRAFICO > MONOSSILABOS########
#monossilabos: palavras em tokens2 sem hifen 
overlap.semAG.semH<-which(palavras.sem.AG %in% palavras.sem.H == T); head(overlap.semAG.semH)
overlap.tokens2.semAG2<-which(tokens2 %in%  palavras.sem.AG[overlap.semAG.semH]==T) #localizacao das palavras em tokens 2 sem AG e sem hifen(monossil) <-- grupos 3/4
monossilabos.sem.AG<-tokens2[overlap.tokens2.semAG2]#palavras em tokens 2 sem AG e sem hifen(monossil)

##PALAVRAS SEM ACENTO GRAFICO > MONOSSILABOS > ATONOS##
monossilabos.atonos<-c("o", "a", "e", "em", "n[ao]s?", "um", "[ao]s", "d[eao]s?", "pr[ao]s?", "que", "ou", "uns", "nuns", "[stm]e", "lhe", "mas", "com", "por", "[aoe]h", "num", "pra") #... continuar lista ######!!!!!######
monossilabos.atonos.sep<-paste("^", monossilabos.atonos, "$", sep=""); monossilabos.atonos.sep; monossilabos.atonos.collapse<-paste(monossilabos.atonos.sep, collapse="|"); monossilabos.atonos.collapse
loc.monossilabos.atonos<-grep(monossilabos.atonos.collapse, tokens2); loc.monossilabos.atonos#localizacao em tokens2 dos monossilabos atonos <--------------- grupo 3
monossilabos.atonos<-tokens2[loc.monossilabos.atonos]; head(monossilabos.atonos)


##PALAVRAS SEM ACENTO GRAFICO > MONOSSILABOS > TONICOS##
#define-se por exclusao dos monossilabos atonos do grupo monossilabos sem AG
monossilabos.tonicos<-which(monossilabos.sem.AG %in%  monossilabos.atonos == F); head(monossilabos.tonicos)
overlap.mon.semAG.ton<-monossilabos.sem.AG[monossilabos.tonicos]; overlap.mon.semAG.ton
overlap.tokens2.semAG.ton<-which(tokens2 %in% overlap.mon.semAG.ton == T)#localizacao em tokens 2 dos monossilabos tonicos <--------------- grupo 4 (monossilabos tonicos sem AG)
monossilabos.tonicos<-tokens2[overlap.tokens2.semAG.ton] #palavras em tokens2 de monossilabos tonicos


########PALAVRAS SEM ACENTO GRAFICO > POLISSILABOS########
palavras.sem.AG
palavras.H
overlap.semAG.comH<-which(palavras.sem.AG %in% palavras.H == T)
overlap.palavras.semAG.comH<-palavras.sem.AG[overlap.semAG.comH]
loc.polissilabos.semAG<-which(tokens2 %in% overlap.palavras.semAG.comH == T); head(loc.polissilabos.semAG,15) #<---- grupos 5/6
polissilabos.sem.AG<-tokens2[loc.polissilabos.semAG]; head(polissilabos.sem.AG)

##PALAVRAS SEM ACENTO GRAFICO > POLISSILABOS > OXITONOS##
#definicoes e localizacoes de oxitonas em polissilabos
pol.oxit1<-grep("[r|l|z]$", polissilabos.sem.AG, value=T); pol.oxit1
pol.oxit2<-grep("[aeio]us?$", polissilabos.sem.AG, value=T); pol.oxit2
pol.oxit3<-grep("[aeou]is?$", polissilabos.sem.AG, value=T); pol.oxit3
pol.oxit4<-grep("i[aeou]s?$", polissilabos.sem.AG, value=T); pol.oxit4
pol.oxit5<-grep("u[aio]s?$", polissilabos.sem.AG, value=T); pol.oxit5
pol.oxit6<-grep("[ui][mn]s?$", polissilabos.sem.AG, value=T); pol.oxit6
pol.oxit7<-grep("[bcçdfghjklmnprstvwxyz][iu]s?$", polissilabos.sem.AG, value=T); pol.oxit7

pol.oxitonas<-c(pol.oxit1,pol.oxit2,pol.oxit3,pol.oxit4, pol.oxit5, pol.oxit6, pol.oxit7); head(pol.oxitonas)

loc.pol.oxitonas<-which(tokens2 %in% pol.oxitonas == T) #localizacao em tokens2 de polissilabos oxitonos <--------------- #grupo 5
palavras.pol.oxitonas<-tokens2[loc.pol.oxitonas]; head(palavras.pol.oxitonas)

##PALAVRAS SEM ACENTO GRAFICO > POLISSILABOS > PAROXITONOS##
#definidos por exclusao a partir de polissilabos oxitonos
overlap.pol.semAG.pol.paroxit<-which(polissilabos.sem.AG %in% pol.oxitonas == F)
overlap.pal.pol.semAG.paroxit<-polissilabos.sem.AG[overlap.pol.semAG.pol.paroxit]; head(overlap.pal.pol.semAG.paroxit)
loc.polissilabos.parox<-which(tokens2 %in% overlap.pal.pol.semAG.paroxit == T) #localizacao em tokens2 de polissilabos paroxitonos <--------------- #grupo 6
palavras.pol.parox<-tokens2[loc.polissilabos.parox]; head(palavras.pol.parox)

###############fim localizacoes grupos 1:6###############
overlap.tokens2.AG2 # grupo 1 (monossilabos com AG)
overlap.tokens2.AG # grupo 2 (polissilabos com AG)
loc.monossilabos.atonos #grupo 3 (monossilabos atonos sem AG)
overlap.tokens2.semAG.ton #grupo 4 (monossilabos tonicos sem AG)
loc.pol.oxitonas # grupo 5 (polissilabos oxitonos sem AG)
loc.polissilabos.parox # grupo 6 (polissilabos parox sem AG)
loc.todas.palavras<-c(overlap.tokens2.AG2, overlap.tokens2.AG, loc.monossilabos.atonos, overlap.tokens2.semAG.ton, loc.pol.oxitonas, loc.polissilabos.parox)

######palavras#####
palavras.AG.monossilabos #palavras grupo 1 (monossilabos com AG)
palavras.AG.polissilabos #palavras grupo 2 (polissilabos com AG)
monossilabos.atonos #palavras grupo 3 (monossilabos atonos sem AG)
monossilabos.tonicos #palavras grupo 4 (monossilabos tonicos sem AG)
palavras.pol.oxitonas #palavras grupo 5 (polissilabos oxitonos sem AG)
palavras.pol.parox #palavras grupo 6 (polissilabos parox sem AG)
todas.palavras<-c(palavras.AG.monossilabos, palavras.AG.polissilabos, monossilabos.atonos, monossilabos.tonicos, palavras.pol.oxitonas, palavras.pol.parox)
palavras.unicas<-unique(todas.palavras)

transc.final<-apagar
transc.final<-gsub("$", " ", transc.final)
transc.final<-gsub("^", " ", transc.final)

for (l in 1:length(palavras.unicas)) {
  #grupo 1: monossilabos com AG
  if (palavras.unicas[l] %in% palavras.AG.monossilabos == T) {
    asubst<-paste("( )", "(", palavras.unicas[l], ")", "( )", sep="")
    subst<-"\\1\\'\\2\\3"
    transc.final<-gsub(asubst, subst, transc.final)
    transc.final<-gsub("-\\'", "-", transc.final) #aparent ok!
    
    #grupo 2: polissilabos com AG  
  } else {
    
    if (palavras.unicas[l] %in% palavras.AG.polissilabos == T) {
      loc.hifens<-gregexpr("-", palavras.unicas[l]); loc.hifens
      num.hifens<-sapply(loc.hifens, length); num.hifens
      loc.acento<-gregexpr("ã|á|â|ê|é|í|õ|ó|ô|ú", palavras.unicas[l]); loc.acento[[1]][1]
      
      if (length(which(loc.acento[[1]][1]>min(loc.hifens[[1]])))==0) {
        asubst<-paste("( )", "(", palavras.unicas[l], ")", "( )", sep="")
        subst<-"\\1\\'\\2\\3"
        transc.final<-gsub(asubst, subst, transc.final)
      } else {
        asubst<-paste("( )", "(", substr(palavras.unicas[l], 1, loc.hifens[[1]][max(which(loc.hifens[[1]]<loc.acento[[1]]))]), ")", "(", substr(palavras.unicas[l], loc.hifens[[1]][max(which(loc.hifens[[1]]<loc.acento[[1]]))]+1, nchar(palavras.unicas[l])), ")", "( )",  sep="")
        subst<-"\\1\\2\\'\\3\\4"
        transc.final<-gsub(asubst, subst, transc.final)
      }
      
      #grupo 3: monossilabos atonos sem AG
    } else {
      
      if (palavras.unicas[l] %in% monossilabos.atonos == T) {
        next
        
        #grupo 4: monossilabos tonicos sem AG
      } else {
        
        if (palavras.unicas[l] %in% monossilabos.tonicos == T) {
          asubst<-paste("( )", "(", palavras.unicas[l], ")","( )", sep="")
          subst<-"\\1\\'\\2\\3"
          transc.final<-gsub(asubst, subst, transc.final)
          
          #grupo 5: polissilabos oxitonos
        } else {
          
          if (palavras.unicas[l] %in% palavras.pol.oxitonas == T) {
            loc.hifens<-gregexpr("-", palavras.unicas[l]); loc.hifens
            num.hifens<-sapply(loc.hifens, length); num.hifens
            
            asubst<-paste("( )", "(", substr(palavras.unicas[l], 1, loc.hifens[[1]][num.hifens]), ")", "(", substr(palavras.unicas[l], loc.hifens[[1]][num.hifens]+1, nchar(palavras.unicas[l])), ")", "( )", sep="")
            subst<-"\\1\\2\\'\\3\\4"
            transc.final<-gsub(asubst, subst, transc.final)
            
            
            #grupo 6: polissilabos paroxitonos
          } else {
            
            if (palavras.unicas[l] %in% palavras.pol.parox == T) {
              loc.hifens<-gregexpr("-", palavras.unicas[l]); loc.hifens
              num.hifens<-sapply(loc.hifens, length); num.hifens
              
              if (num.hifens==1) {
                asubst<-paste("( )", "(", palavras.unicas[l], ")", "( )", sep="")
                subst<-"\\1\\'\\2\\3"
                transc.final<-gsub(asubst, subst, transc.final)
                
              } else {
                asubst<-paste("( )", "(", substr(palavras.unicas[l], 1, loc.hifens[[1]][num.hifens-1]), ")", "(", substr(palavras.unicas[l], loc.hifens[[1]][num.hifens-1]+1, nchar(palavras.unicas[l])), ")", "( )", sep="")
                subst<-"\\1\\2\\'\\3\\4"
                transc.final<-gsub(asubst, subst, transc.final)
              }
            }
          }
        }
      }
    }
  }
}
transc.final<-gsub(" $", "", transc.final)
transc.final<-gsub("^ ", "", transc.final)

##para amostra 2, usei silacShiny

###PARTE 3
chave <-read.csv("https://raw.githubusercontent.com/oushiro/CoesaoeDispersao/refs/heads/main/chave_fonologica_utf82.csv", sep="\t", encoding = "UTF-8") 
#chave <- read_delim("https://raw.githubusercontent.com/oushiro/CoesaoeDispersao/refs/heads/main/chave_fonologica_utf82.csv", delim = "\t") 
head(chave, 20)

#library(readr)
#chave <- read_delim("chave_fonologica_utf8.csv", 
#                    delim = "\t", escape_backslash = TRUE, 
#                    trim_ws = TRUE)
#View(chave)

transc.fonologica<-transc.final


for(m in 1:nrow(chave)) {
  asubst<-paste(chave[m,1], sep="")
  subst<-paste(chave[m,2], sep="")
#  transc.fonologica<-gsub(asubst, subst, transc.fonologica, ignore.case=F)
  transc.fonologica <- str_replace_all(transc.fonologica, asubst, subst)
  }


#dados.original[loc.marcacoes, 5] <- transc.fonologica; tail(dados.original, 50)
  
###PARTE 4: LOCALIZAR PRETONICAS###
#loc.pretonicas <- grep("marc.pretonicas", dados.original[,1], value = F)
#palavras.p.pretonicas <- scan("transcFonologica.txt", what = "char", sep = "\n")
palavras.p.pretonicas <- unlist(str_split(transc.fonologica, pattern = " "))
#palavras.p.pretonicas <- gsub("~i", "I", palavras.p.pretonicas)
#palavras.p.pretonicas <- gsub("~e", "E", palavras.p.pretonicas)
#palavras.p.pretonicas <- gsub("~a", "A", palavras.p.pretonicas)
#palavras.p.pretonicas <- gsub("~o", "O", palavras.p.pretonicas)
#palavras.p.pretonicas <- gsub("~u", "U", palavras.p.pretonicas)

words<-palavras.p.pretonicas; head(words, 10); length(words)
vogais.pret0<-paste(pretonicas, collapse="")
vogais.pret1<-paste("[",vogais.pret0,"]", sep=""); vogais.pret1
words2<-vector()

loc.acento.words<-gregexpr("'", words); loc.acento.words 
loc.vogais.words<-gregexpr(vogais.pret1, words); loc.vogais.words

for (s in 1:length(words)) {
  if (loc.acento.words[[s]][1]<2 | loc.vogais.words[[s]][1] < 0){
    words2[s]<-words[s]
  } else {
    loc.acento.words[[s]][1] #numero caractere localizacao acento
    loc.vogais.pretonicas.words<-which(loc.vogais.words[[s]][1:length(loc.vogais.words[[s]])]<loc.acento.words[[s]][1])#posicao das quais vogais teem localizacao menor que acento 
    posicao.caracter.words<-loc.vogais.words[[s]][loc.vogais.pretonicas.words]#posicao correta dentro da palavra das vogais que teem localizaco menor que acento
    
    palavras.split<-unlist(strsplit(words[s], ""))
    subst<-paste("<", "\\1", ">", sep="")
    for (t in 1:length(posicao.caracter.words)){
      asubst<-paste("(",palavras.split[posicao.caracter.words[t]],")", sep="")
      palavras.split[posicao.caracter.words[t]]<-gsub(asubst, subst,  palavras.split[posicao.caracter.words[t]])   
    }
    palavras.split  
    words2[s]<-paste(palavras.split, collapse="")
  }
}

grep("^<.>", words2, ignore.case = T, value = T)
words3 <- gsub("^<(.)>", "\\1", words2, ignore.case = T)

grep("<(.)>([wj])", words3, ignore.case = T, value = T)
words4 <- gsub("<(.)>([wj])", "\\1\\2", words3, ignore.case = T)

grep("<.>-\\'?[aeiou]", words4, ignore.case = T, value = T)
words5 <- gsub("<(.)>(-)(\\'?)([aeiou])", "\\1\\2\\3\\4", words4, ignore.case = T)
words6 <- gsub("<(.)>(-)(\\'?)([aeiou])", "\\1\\2\\3\\4", words5, ignore.case = T) # pegar palavras com 2 ocorrencias

grep("-<", words6, ignore.case = T, value = T)
words7 <- gsub("(-)<(.)>", "\\1\\2", words6, ignore.case = T)

grep("[wj]<.>", words7, ignore.case = T, value = T)
words8 <- gsub("([wj])<(.)>", "\\1\\2", words7, ignore.case = T)

###########
words3 <- unlist(str_split(transc, pattern = " "))

tabela.vog<-cbind(words3, words8)
tabela.vog<-unique(tabela.vog)
tabela.vog <- tabela.vog[order(tabela.vog[,1]), ]
View(tabela.vog)
excluir <- grep("\\bb\\b|\\bc\\b|\\bd\\b|\\be\\b|\\bf\\b|\\bg\\b|\\bh\\b|\\bi\\b|\\bj\\b|\\bk\\b|\\bl\\b|\\bm\\b|\\bn\\b|\\bo\\b|\\bp\\b|\\bq\\b|\\br\\b|\\bs\\b|\\bt\\b|\\bu\\b|\\bv\\b|\\bx\\b|\\bz\\b|\\bw\\b|\\by\\b", tabela.vog[,1], ignore.case = T, value = F)
tabela.vog <- tabela.vog[-c(1:5,excluir), ]

transc2 <- transc
for(i in 1:length(tabela.vog[,1])){
  transc2 <- str_replace_all(transc2, paste("\\b", tabela.vog[i,1], "\\b", sep = ""), as.character(tabela.vog[i,2]))
}

head(transc2, 200)
tail(transc2, 50)

grep("\'\'", transc2, ignore.case = T, value = T)
transc2 <- gsub("\'\'", "'", transc2) ### melhorar em novas versões: o duplo '' ocorre em palavras (normalmente truncadas) que estão no dicionário: ti = 'ti, mi = 'mi; ao passar novamente, o script faz nova marcação de '. Melhorar em words8 a lista de palavras que devem ter acento.
transc2 <- gsub("<\'", "<", transc2)

library(audio)
play(sin(1:10000/20))
