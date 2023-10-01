library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
folha.14 <- read.csv2("../../dados_oficiais/folhas_pagamento/2014_09.csv", row.names=NULL)
names(folha.14) <- c("nome", "unidade", "depto", "jornada", "categoria",
      "classe", "ref.MS", "funcao", "funcao.estrutura", "tempo.usp",
      "parcelas.eventuais", "salario.mensal", "salario.liquido", "X")
folha.14$categoria[folha.14$categoria=="Func Aut"] <- "Autarquico"
folha.23 <- read.csv2("../../dados_oficiais/folhas_pagamento/2023_08.csv", fileEncoding="latin1", row.name=NULL)
names(folha.23) <- c("nome", "unidade", "depto", "jornada", "categoria",
                     "data.ingresso.aposentadoria", 
                     "classe", "ref.MS", "funcao", "funcao.estrutura", "data.designacao",
                     "tempo.usp", "parcelas.eventuais", "salario.mensal", "salario.liquido", "X")
folha.23$categoria[folha.23$categoria=="Func Aut"] <- "Autarquico"
folha.14$ano <- 2014
folha.23$ano <- 2023
folha.23.14 <- rbind(folha.14, folha.23[,names(folha.14)])
## Corrige nomes de unidades
folha.23.14$unidade[folha.23.14$unidade=="M A E"] <- "MAE"

## Identifica unidades de ensino e pesquisa
unidades <- read.csv2("../../dados_oficiais/gerais/sigla_unidades_na_folha.csv", as.is=TRUE)
ensino <- unidades$Unidade[unidades$categoria=="Ensino e pesquisa"]
folha.23.14$unid.ensino <- folha.23.14$unidade %in% ensino
folha.23.14 <- folha.23.14[, -14]
folha.23.14$doc.efetivo <- folha.23.14$categoria=="Docente"&folha.23.14$jornada!="12 horas"
## Quantos docentes
filter(folha.23.14, categoria=="Docente") %>%
    count(ano, doc.efetivo, unid.ensino)

## Quanto servidores não docentes
filter(folha.23.14, categoria=="Celetista"|categoria=="Autarquico") %>%
    count(ano, unid.ensino)

## Saidas e entradas
## Homonimos
## Ha 3 ativos entre 2014 e 2023
homonimos <-
    folha.23.14 %>%
    filter(categoria=="Docente"&jornada!="12 horas")%>%
    count(nome) %>%
    filter(n>2)
## Identificando
folha.23.14 %>%
    filter(nome %in% homonimos$nome) %>%
    select(nome, unidade, ano)
## Rubens Buono tem duas filiações , então fica assim
## os outros dois sao homonimos
##Marcelo Santos
folha.23.14$nome[folha.23.14$nome=="Marcelo dos Santos"] <-
    paste(folha.23.14$nome[folha.23.14$nome=="Marcelo dos Santos"],
          folha.23.14$unidade[folha.23.14$nome=="Marcelo dos Santos"])
## Renato de Moraes
folha.23.14$nome[folha.23.14$nome=="Renato de Moraes"] <-
    paste(folha.23.14$nome[folha.23.14$nome=="Renato de Moraes"],
          folha.23.14$unidade[folha.23.14$nome=="Renato de Moraes"])

docentes.14 <-
    filter(folha.23.14, categoria=="Docente"&jornada!="12 horas"&ano==2014)
docentes.23 <-
    filter(folha.23.14, categoria=="Docente"&jornada!="12 horas"&ano==2023)
## Perdas
(saidas <- sum(!(docentes.14$nome %in% docentes.23$nome)))
## Ganhos
(entradas <- sum(!(docentes.23$nome %in% docentes.14$nome)))
## Medias por mes
meses <- 6*12+11 ## meses transcorridos entre set/16 e ago/23
anos <- meses/12
saidas/meses
entradas/meses
## Medias por ano
saidas/anos
entradas/anos
## Saidas - entradas : 987, que bate com o saldo obtido pela adusp
saidas - entradas
## Projecao de saidas nos proximos 4 anos
saidas * 48/meses

## Proporção de gastos com aposentados
folha.23 %>%
    group_by(categoria) %>%
    summarise(salario = sum(salario.mensal))

tot.apos <-
    filter(folha.23, categoria == "Doc Apos" | categoria == "Func Apos" | categoria == "Compl Apos") %>%
    summarise(salario = sum(salario.mensal))
tot <- sum(folha.23$salario.mensal)
tot.apos/tot
################################################################################
doc.func.23.14 <-
    filter(folha.23.14, categoria=="Docente"|categoria=="Celetista"|categoria=="Autarquico") %>%
    group_by(unidade, categoria, ano ) %>%
    summarise(N=n()) %>%
    spread(categoria, value=N, fill=0) %>%
    ungroup %>%
    mutate(Nao.docentes= (Celetista + Autarquico), proporcao = Nao.docentes/(Docente+Nao.docentes)) %>%
    arrange(Nao.docentes+Docente) %>%
    select(unidade, ano, Nao.docentes, Docente, proporcao) %>%
    arrange(unidade, ano) %>%
    as.data.frame() 
doc.func.23.14$unid.ensino <- doc.func.23.14$unidade %in% ensino
write.csv(doc.func.23.14, "N_docentes_servidores_ativos_por_unidade_set_14_ago_23.csv")

##
doc.func.23.14 %<>%
    select(unidade, ano, Nao.docentes, Docente, proporcao) %>%
    melt(id.vars=1:2) %>% 
    dcast(unidade ~ variable + ano) %>%
    mutate(p.14.40 = proporcao_2014>0.6,
           perda.d=Docente_2014-Docente_2023,
           perdap.d=(Docente_2014-Docente_2023)/Docente_2023,
           perda.f=Nao.docentes_2014-Nao.docentes_2023,
           perdap.f=(Nao.docentes_2014-Nao.docentes_2023)/Nao.docentes_2014
           )
doc.func.23.14$unid.ensino <- doc.func.23.14$unidade %in% ensino
## Numeros gerais
with(doc.func.23.14, sum(perda.d, na.rm=TRUE))
with(doc.func.23.14, sum(perda.f, na.rm=TRUE))
with(doc.func.23.14, (perdap.d))


## Situação da USP em 2014
sum(doc.func.23.14$Docente_2014, na.rm =TRUE) - sum(doc.func.23.14$Docente_2023, na.rm =TRUE)


ensino.p.14.23 <- filter(doc.func.23.14, unid.ensino) %>%
    select(unidade, ano, proporcao) %>%
    spread(ano, proporcao) %>%
    as.data.frame()
names(ensino.p.14.23)[2:3] <- c("prop.2014", "prop.2023")
plot(prop.2023 ~ prop.2014, data=ensino.p.14.23)
abline(h=0.6, lty=2, col="red")
abline(v=0.6, lty=2, col="blue")
abline(0,1)

## Unidades que tinham pelo menos 40% de seus ativos em professores

doc.func.23.14   %>%
    filter(unid.ensino) %>%
    group_by(p.14.40) %>%
    summarize(N.unidades= n(),func.14=sum(Nao.docentes_2014), func.23=sum(Nao.docentes_2023),
              doc.14=sum(Docente_2014), doc.23=sum(Docente_2023)) %>%
    mutate(
           perda.f=(func.14-func.23)/func.14,
           perda.d=(doc.14-doc.23)/doc.14,
           p.doc.func.23 = doc.23/(func.23+doc.23),
           p.doc.func.14 = doc.14/(func.14+doc.14)
           ) %>%
    as.data.frame()
