

# Levantamentos adicionais apos reunião do dia 07/11/2022


library(tidyverse)
library(readr)

# import and consolidade data ---------------------------------------------
files <- list.files(path = './data', pattern = '*.csv', full.names = T)

dados1 <- read_csv2(files[1], locale=locale(encoding="latin1")) %>% mutate(fonte = 'Amostra_1')
dados2 <- read_csv2(files[2], locale=locale(encoding="latin1")) %>% mutate(fonte = 'Amostra_2')
dados3 <- read_csv2(files[3], locale=locale(encoding="latin1")) %>% mutate(fonte = 'Amostra_3')

dados2 <- dados2 %>% 
  mutate('Tempo_Av-TAV' = Tempo_avião - Tempo_TAV)

dados3 <- dados3 %>% 
  mutate('Tempo_Av-TAV' = Tempo_avião - Tempo_TAV)

colunas <- c('fonte', 'SbjNum', 'Opção_escolhida', 
             'Tempo_Av-TAV', 'Tempo_avião', 'Tempo_TAV',
             'Valor_avião',	'Valor_TAV',
             'Q_2', 'Q_57', 'Rota TAV' )

convert_money <- function(x){
  y <- as.numeric(gsub(',00','', gsub("([R\\$.])","", x)))
  return(y)
}


dados_all <- dados1 %>% select(colunas) %>% 
  bind_rows(dados2 %>% select(colunas)) %>% 
  bind_rows(dados3 %>% select(colunas)) %>% 
  mutate(Opção_escolhida = ifelse(Opção_escolhida == 'Não sabe/ Não respondeu'
                                  ,'NSR', Opção_escolhida)
         ,dif_av_tav = convert_money(Valor_avião) - convert_money(Valor_TAV)
         ,valor_aviao = convert_money(Valor_avião)
         ,usou_milha = case_when(Q_57 == 'Comprada' ~ 0
                                 ,TRUE ~ 1) ) %>% 
  group_by(`Rota TAV`, fonte) %>% 
  mutate(z_valor = (valor_aviao - mean(valor_aviao))/sd(valor_aviao) ) %>% 
  ungroup()

table(dados_all$Opção_escolhida)
length(unique(dados_all$SbjNum))

dados_unic <- dados_all %>% 
  filter(Opção_escolhida != 'NSR') %>% 
  mutate(Opção_escolhida = case_when(Opção_escolhida == 'Avião' ~ Opção_escolhida
                                     , T ~ 'TAV')
         ,tempo_horas = case_when(Opção_escolhida == 'Avião' ~ Tempo_avião
                                 , T ~ Tempo_TAV)/60
         ,custo = case_when(Opção_escolhida == 'Avião' ~ valor_aviao
                            , T ~ convert_money(Valor_TAV))
         ,custo_horas = custo/tempo_horas
         ,tempo_horas2 = case_when(Opção_escolhida != 'Avião' ~ Tempo_avião
                                  , T ~ Tempo_TAV)/60
         ,custo2 = case_when(Opção_escolhida != 'Avião' ~ valor_aviao
                            , T ~ convert_money(Valor_TAV))
         ,custo_horas2 = custo2/tempo_horas2) # %>% 
  # group_by(SbjNum, Opção_escolhida) %>% 
  # summarise(n_resposta = n()
  #           ,tempo_horas = min(tempo_horas)
  #           ,custo = min(custo)
  #           ,custo_horas = min(custo_horas)
  #           ,tempo_horas2 = min(tempo_horas2)
  #           ,custo2 = min(custo2)
  #           ,custo_horas2 = min(custo_horas2))
  
dados_unic


dados_unic %>% 
  ggplot( aes(x=tempo_horas, y=custo, color=Opção_escolhida)) +
  geom_point(size=2)  + 
  guides(color=guide_legend(title="Escolha"))


ggplot(mapping = aes(x=tempo_horas, y=custo, color=Opção_escolhida)) +
geom_point(data = dados_unic %>% select(SbjNum, tempo_horas = tempo_horas2, custo = custo2)
           , aes(color = 'N_Escolhido')
           , alpha = .4, shape = 3
           , size = 3) +
  geom_point(data = dados_unic, size=2, alpha = .5) + 
  guides(color=guide_legend(title="Escolha"))


dados_unic %>% 
  ggplot( aes(x=Opção_escolhida, y=custo_horas, color=Opção_escolhida)) +
  geom_boxplot() +
  guides(color=guide_legend(title="Escolha"))

### métricas
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


a <- dados_unic[dados_unic$Opção_escolhida == 'Avião','custo'][[1]]
quantile(a)
getmode(a)
mean(a)



metricas <- function(x){
  cat('Quantis \n')
  print(quantile(x))
  cat('moda \n')
  print(getmode(x))
  cat('media \n')
  print(mean(x))
  cat('Intervalo de Confianca \n')
  print(confint(lm(x ~ 1), level=0.95))
  
}

metricas(a)

metricas(dados_unic[dados_unic$Opção_escolhida == 'Avião','custo'][[1]])
metricas(dados_unic[dados_unic$Opção_escolhida != 'Avião','custo'][[1]])

metricas(dados_unic[dados_unic$Opção_escolhida == 'Avião','tempo_horas'][[1]])
metricas(dados_unic[dados_unic$Opção_escolhida != 'Avião','tempo_horas'][[1]])

metricas(dados_unic[dados_unic$Opção_escolhida == 'Avião','custo_horas'][[1]])
metricas(dados_unic[dados_unic$Opção_escolhida != 'Avião','custo_horas'][[1]])



# Identificando mudança de escolha ----------------------------------------
d = dados_all %>% 
  select(Opção_escolhida, SbjNum) %>% 
  mutate(ind = 1) %>% 
  group_by(SbjNum, Opção_escolhida) %>% 
  summarise(FREQ = n())

mudou_ideia <- unlist(unique(d[d$FREQ==1,'SbjNum']))

length(mudou_ideia); length(unique(d$SbjNum))

# Avaliando tipo de escolha -----------------------------------------------
choices <- dados_all %>% 
  filter(SbjNum %in% mudou_ideia
         ,Opção_escolhida != 'NSR') %>% 
  arrange(SbjNum, Opção_escolhida) %>% 
  group_by(SbjNum) %>% 
  mutate(choice = row_number()
         ,opcao_num = case_when(Opção_escolhida == "Avião"  ~ 3
                                ,Opção_escolhida == "NSR"  ~ 2
                                ,Opção_escolhida == "Trem de alta velocidade"  ~ 1)
  ) %>% 
  ungroup() %>% 
  select(fonte, SbjNum, `Tempo_Av-TAV`, dif_av_tav, Opção_escolhida, opcao_num, choice) 

choices <- choices %>% filter(choice == 1) %>% 
  left_join(choices %>% filter(choice == 2), suffix = c(".1", ".2"), by = c("SbjNum" = "SbjNum", 'fonte' = 'fonte'))

choices <- choices %>% 
  mutate(irracional = case_when((dif_av_tav.1 - dif_av_tav.2 >= 0 &
                                   `Tempo_Av-TAV.1` - `Tempo_Av-TAV.2` > 0 &
                                   opcao_num.1 - opcao_num.2 > 0 ) ~ 1 
                                , TRUE ~ 0)
  )


dados_unic_racional <- dados_unic %>%  
  mutate(escolhas = case_when(SbjNum %in% mudou_ideia ~ 'mudou'
                              ,TRUE ~ 'igual' )
         ,tp_escolha = case_when(SbjNum %in% choices[choices$irracional == 1,]$SbjNum ~ 'Irracional',
                                 TRUE ~ 'Racional') ) %>% 
  filter(escolhas == 'mudou' & tp_escolha == 'Racional') %>% 
  group_by(SbjNum) %>% 
  mutate(qtd = n()) %>% 
  filter(qtd == 2)

length(unique(dados_unic_racional$SbjNum))

dados_unic_racional %>% 
  ggplot( aes(x=tempo_horas, y=custo, color=Opção_escolhida)) +
  geom_point(size=2)  + 
  # geom_line(aes(group = SbjNum), alpha=.2) +
  guides(color=guide_legend(title="Escolha"))


ggplot(mapping = aes(x=tempo_horas, y=custo, color=Opção_escolhida)) +
  geom_point(data = dados_unic_racional %>% select(SbjNum, tempo_horas = tempo_horas2, custo = custo2)
             , aes(color = 'N_Escolhido')
             , alpha = .4, shape = 3
             , size = 3) +
  geom_point(data = dados_unic_racional, size=2, alpha = .7) + 
  guides(color=guide_legend(title="Escolha"))


dados_unic_racional %>% 
  ggplot( aes(x=Opção_escolhida, y=custo_horas, color=Opção_escolhida)) +
  geom_boxplot() +
  guides(color=guide_legend(title="Escolha"))

### métricas
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


metricas <- function(x){
  cat('Quantis \n')
  print(quantile(x))
  cat('moda \n')
  print(getmode(x))
  cat('media \n')
  print(mean(x))
  cat('Intervalo de Confianca \n')
  print(confint(lm(x ~ 1), level=0.95))
  
}

metricas(dados_unic_racional[dados_unic_racional$Opção_escolhida == 'Avião','custo'][[1]])
metricas(dados_unic_racional[dados_unic_racional$Opção_escolhida != 'Avião','custo'][[1]])

metricas(dados_unic_racional[dados_unic_racional$Opção_escolhida == 'Avião','tempo_horas'][[1]])
metricas(dados_unic_racional[dados_unic_racional$Opção_escolhida != 'Avião','tempo_horas'][[1]])

metricas(dados_unic_racional[dados_unic_racional$Opção_escolhida == 'Avião','custo_horas'][[1]])
metricas(dados_unic_racional[dados_unic_racional$Opção_escolhida != 'Avião','custo_horas'][[1]])
