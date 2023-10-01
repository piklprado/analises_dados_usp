library(readxl)
library(tidyverse)
library(janitor)
library(lme4)

##Importante: usei os dados de n de docentes por categoria, das tabelas T2.09 do anuário
## Suspeito que um numero mais adeqaudos eria o n de RDIDP equivalentes, que está na tabela 2.11


## Dados do anuario da USP
anos <- 2012:2022
## N de docentes por unidade e depto
docentes <-
    lapply(anos,
           function(x) read_excel(paste0("tabelas_anuarios/T2.09_",x,".xls"),
                                  skip =6)) %>%
    bind_rows(.id = "ano") %>%
    mutate(ano = 2011 + as.numeric(ano)) %>%
    clean_names() %>%
    mutate(unidade = ifelse(unidade=="M A E", "MAE", unidade))
## Total de docentes de cada categoria, por unidade e depto
doc.tot <-
    docentes %>%
    ##filter(tipo_de_unidade == "A - Ensino e Pesquisa") %>%
    ##mutate(departamento = ifelse(unidade == "EACH", "Artes, Ciências e Humanidades", departamento)) %>%
    group_by(ano, unidade, regime_de_trabalho) %>%
    summarise(N = sum(total)) %>%
    pivot_wider(names_from = regime_de_trabalho, values_from = N, values_fill=0) %>%
    clean_names() %>%
    mutate(doc.tot=rdidp+rtc+rtp+x08_horas)

## N de disciplinas, turmas e n medio por turma da graduacao

f2 <- function(x, ...) read_excel(paste0("tabelas_anuarios/T3.02_",x,".xls"), ...)
## em duas etapas, pq as  planilhas até 2018 tem 9 linhas antes dos dados e 2019-2022 tem 6 ...
d.grad <-
    c(lapply(anos[1:7], f2, skip = 9),
      lapply(anos[8:11], f2, skip = 6)) %>%
    bind_rows(.id = "ano") %>%
    mutate(ano = 2011 + as.numeric(ano)) %>%
    clean_names() %>%
    mutate(unidade = ifelse(unidade=="M A E", "MAE", unidade)) %>%
    ##filter(tipo_de_unidade == "A - Ensino e Pesquisa") %>% 
    pivot_wider( names_from = metrica, values_from = total) %>%
    clean_names() %>% 
    mutate(matriculas_t = numero_de_turmas_teoricas*dimensao_da_turma_teorica,
           matriculas_p = numero_de_turmas_praticas*dimensao_da_turma_pratica) %>%
    group_by(ano, unidade) %>%
    summarise(disciplinas = sum(disciplinas_ministradas),
              turmas_t = sum(numero_de_turmas_teoricas),
              turmas_p = sum(numero_de_turmas_praticas),
              matr_t = sum(matriculas_t),
              matr_p = sum(matriculas_p)) 
## Junta as duas tabelas
doc.grad <-
    merge(d.grad, doc.tot, by = c("ano","unidade"), all=TRUE)

## Ainda temos um problema: disciplinas compartilhadas por mais de uma unidade
## A solução que achei foi dividir pelas unidades de ensino e pesquisa
## Seleciona estas disciplinas, que na tabela fundida tem NAs na variavel doc.tot
inter <- filter(doc.grad, is.na(doc.tot))
nome <- list(n.unid = data.frame(unidade = unique(inter$unidade)),
             unid = list("EEL", c("FFCLRP", "FMRP"), "HRAC", c("IAG", "IO"),
                        c("IFSC", "IQSC", "ICMC"), "IPEN", "Pró-G", c("IB", "CEBIMar"),
                        c("FM", "ICB", "IQ", "IB"),
                        c("EESC", "ICMC"), "IMT", c("FCF" , "ICB"), c("IQ", "FCF"), c("FSP", "FCF")))
## Uma solucao meio POG para juntar estas disciplinas já divididas à tabela, criando uma nova
names(nome$unid) <- unique(inter$unidade)
nome$n.unid$N <- sapply(nome$unid, length)
inter.ind <- left_join(inter[,1:2], nome$n.unid, by="unidade")
inter2 <- inter[rep(1:nrow(inter), inter.ind$N),]
inter2$componente <- unlist(nome$unid[inter$unidade])
inter2$n.unid <- rep(inter.ind$N, inter.ind$N)
inter3 <-
    mutate(inter2,
           disciplinas = disciplinas/n.unid,
           turmas_t = turmas_t/n.unid,
           turmas_p = turmas_p/n.unid,
           matr_t = matr_t/n.unid,
           matr_p = matr_p/n.unid,
           unidade = componente,
           rdidp =0,
           rtc = 0,
           rtp = 0,
           x08_horas =0) %>%
    filter(n.unid>1) %>%
    select(-(componente:n.unid))

## Resumo da ópra: Junta as disciplinas interunidades, num novo objeto
doc.grad2 <-
    filter(doc.grad, !is.na(doc.tot)) %>%
    rbind(inter3) %>%
    group_by(ano, unidade) %>%
    summarise(disciplinas = sum(disciplinas),
              turmas_t = sum(turmas_t),
              turmas_p = sum(turmas_p),
              matr_t = sum(matr_t),
              matr_p = sum(matr_p),
              rdidp = max(rdidp),
              rtc = max(rtc, na.rm=TRUE),
              rtp = max(rtp, na.rm=TRUE),
              doc.tot = max(doc.tot, na.rm=TRUE),
              temp = max(x08_horas, na.rm=TRUE)
              ) %>%
    mutate(turmas = turmas_t + turmas_p,
           matr = matr_t + matr_p,
           di.tc = rdidp + rtc,
           l.di.tc = log(di.tc),
           l.disc = log(disciplinas)) %>%
    drop_na()
## padroniza as variaveis  de n de docentes e disciplinas em escala log, para experimentar com os modelos
doc.grad2$l.di.tc.s <- (doc.grad2$l.di.tc - mean(doc.grad2$l.di.tc))/ sd(doc.grad2$l.di.tc)
doc.grad2$l.disc.s <- (doc.grad2$l.disc - mean(doc.grad2$l.disc))/ sd(doc.grad2$l.disc)

## Graficos exploratórios, bastante bagunçado ainda
## N de disciplinas por soma de RDIDP + RTC (maioria dos docentes)
p1 <- ggplot(doc.grad2, aes(di.tc, disciplinas)) +
    geom_point() +
    facet_wrap(~ano)
p1
## Variacoes para ver tendecias, escala log, etc
p1 +
    geom_smooth() 

p1 +
    scale_x_log10()+
    scale_y_log10()

## As duas variaveis em log e padronizadas
ggplot(doc.grad2, aes(l.di.tc.s, l.disc.s)) +
    geom_point() +
    facet_wrap(~ano)


## Com Docentes totais: nao muito diferente
ggplot(doc.grad2, aes(doc.tot, disciplinas)) +
    geom_point() +
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~ano)

## Contra n de turmas. Tb nao muito diferente
ggplot(doc.grad2, aes(doc.tot, turmas)) +
    geom_point() +
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~ano)
## Com n de matriculas, idem
ggplot(doc.grad2, aes(doc.tot, matr)) +
    geom_point() +
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~ano)

## Total de disciplinas e docentes na usp como um todo ao longo dos anos: padores
## claros de quedas de docentes, e varios padrẽos quanto às
## disciplinas
## Um objeto novo com a soma das disciplians e docentes por toda a USP
doc.grad3 <-
    group_by(doc.grad2, ano) %>%
    summarise(doc = sum(doc.tot), disc = sum(disciplinas), turmas = sum(turmas), matr = sum(matr))
## graficos exploratorios
plot(disc ~doc, data = doc.grad3)
plot(disc ~ ano, data = doc.grad3)
plot(doc ~ ano, data = doc.grad3)

################################################################################
## Padroes ao longo do tempo, por unidades
################################################################################
p3 <-
    doc.grad2 %>%
    ## filter(ano <2020 & doc.tot >30) %>% ## se quiser restringir às maiores unidades
    ggplot(aes(doc.tot, disciplinas)) +
    geom_point() +
    facet_wrap(~unidade, scales="free")

p3

##  variaveis log padronizadas
doc.grad2 %>%
    filter(ano <2020 & doc.tot >30) %>%
    ggplot(aes(l.di.tc.s, l.disc.s)) +
    geom_point() +
    facet_wrap(~unidade, scales="free")

## Tendencia n de docentes
doc.grad2 %>%
    ggplot(aes(ano, doc.tot)) +
    geom_point() +
    facet_wrap(~unidade, scales="free")

doc.grad2 %>%
    mutate(disc.doc = disciplinas/doc.tot) %>%
    ggplot(aes(ano, disc.doc)) +
    geom_point() +
    facet_wrap(~unidade, scales = "free")
## 
