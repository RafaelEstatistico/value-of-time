'
2022-07 - Value of Time 
EDA 
'

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
  mutate(z_valor = (valor_aviao - mean(valor_aviao))/sd(valor_aviao)  ) %>% 
  ungroup()

(table(dados_all$fonte,dados_all$usou_milha))

dados_unic <- dados_all %>% 
  filter(dif_av_tav < 500
         ,Opção_escolhida != 'NSR') %>% 
  mutate(tempo = case_when(`Tempo_Av-TAV` >= 0 ~ 'AV', T ~ 'TAV')
         ,dd = dif_av_tav/`Tempo_Av-TAV`) %>% 
  distinct(SbjNum, Opção_escolhida, dif_av_tav, tempo, `Tempo_Av-TAV`, dd)

summary(dados_unic$dif_av_tav)

ggplot(dados_unic, aes(x=dif_av_tav))+
  geom_bar()+
  facet_grid(Opção_escolhida ~ tempo )

ggplot(dados_unic, aes(x=dd, fill = Opção_escolhida))+
  geom_histogram() + 
  facet_grid(Opção_escolhida ~ .)

hist(dados_unic$dd, 50)

# Numero de Respondentes (cada um responde duas vezes)
nrow(dados_all)/2

dados_all %>%  
  ggplot( aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=2)  


dados_all %>%  
  ggplot( aes(x=`Tempo_Av-TAV`, y=z_valor, color=Opção_escolhida)) +
  geom_point(size=2)  





library(ggpubr)
sp <- ggscatter(dados_all, x = "Tempo_Av-TAV", y = "dif_av_tav",
                color = "Opção_escolhida", palette = "jco",
                size = 3, alpha = 0.6)+
  border()                                         
# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggdensity(dados_all, "Tempo_Av-TAV", fill = "Opção_escolhida",
                   palette = "jco")
yplot <- ggdensity(dados_all, "dif_av_tav", fill = "Opção_escolhida", 
                   palette = "jco")+
  rotate()
# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()
# Arranging the plot
ggarrange(xplot, NULL, sp, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)


# Identificando mudança de escolha ----------------------------------------
d = dados_all %>% 
  select(Opção_escolhida, SbjNum) %>% 
  mutate(ind = 1) %>% 
  group_by(SbjNum, Opção_escolhida) %>% 
  summarise(FREQ = n())

prop.table(table(d$FREQ))

mudou_ideia <- unlist(unique(d[d$FREQ==1,'SbjNum']))

length(mudou_ideia); length(unique(d$SbjNum))
length(mudou_ideia)/length(unique(d$SbjNum))


library(ggplot2)
g <- ggplot(dados_all, aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=4)  
  # geom_line(aes(group = SbjNum))
g

dados_all %>%  
  filter(SbjNum %in% mudou_ideia) %>% 
  ggplot( aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=2) + 
  geom_line(aes(group = SbjNum))

dados_all %>% 
  distinct(SbjNum, Q_2, Opção_escolhida) %>% 
  group_by(Q_2) %>% 
  summarise(TOT = n()
            ,Av = sum(ifelse(Opção_escolhida == 'Avião', 1, 0))
            ,p_Av = round(Av/TOT, 3) ) %>% 
  arrange(desc(p_Av))


dados_all %>% 
  distinct(SbjNum, `Rota TAV`, Opção_escolhida) %>% 
  group_by(`Rota TAV`) %>% 
  summarise(TOT = n()
            ,Av = sum(ifelse(Opção_escolhida == 'Avião', 1, 0))
            ,p_Av = round(Av/TOT, 3) ) %>% 
  arrange(desc(p_Av))



# Avaliando tipo de escolha -----------------------------------------------
choices <- dados_all %>% 
  filter(SbjNum %in% mudou_ideia) %>% 
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


dados_all <- dados_all %>%  
  mutate(escolhas = case_when(SbjNum %in% mudou_ideia ~ 'mudou'
                              ,TRUE ~ 'igual' )
         ,tp_escolha = case_when(SbjNum %in% choices[choices$irracional == 1,]$SbjNum ~ 'Irracional',
                                TRUE ~ 'Racional')
         )

dados_all %>% 
  filter(escolhas == 'mudou') %>% 
  ggplot( aes(x=`Tempo_Av-TAV`, y=dif_av_tav, color=Opção_escolhida)) +
  geom_point(size=4) + 
  # geom_line(aes(group = SbjNum)) + 
  facet_grid(tp_escolha ~ .) + 
  theme(legend.position="bottom")

table(choices$irracional)
prop.table(table(choices$irracional))

dados_all %>% 
  distinct(SbjNum, Q_2, escolhas, tp_escolha) %>% 
  group_by(Q_2) %>% 
  summarise(TOT = n()
            ,mudou = sum(ifelse(escolhas == 'mudou', 1, 0))
            ,p_mudou = round(mudou/TOT, 3)
            ,racional = sum(ifelse(tp_escolha == 'Racional' & escolhas == 'mudou', 1, 0))
            ,p_racional = round(racional/TOT, 3)) %>% 
  arrange(desc(p_racional))

# Testes ------------------------------------------------------------------

data("TravelMode",package="AER")

library(mlogit)
library(AER)
TravelMode <- mlogit.data(TravelMode,choice="choice",shape="long",
                          alt.var="mode",chid.var="individual")

TravelMode$avinc <- with(TravelMode,(mode=='air')*income)
ml.TM <- mlogit(choice ~ wait + gcost + avinc, TravelMode,
                reflevel = "car")
hl.TM <- mlogit(choice ~ wait + gcost + avinc, TravelMode,
                reflevel = "car", heterosc = TRUE)
summary(hl.TM)

summary(ml.TM)



TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", 
                  chid.var = "individual", alt.var = "mode", drop.index = TRUE)
head(TM)


# estimate with mlogit
ml.TM <- mlogit(choice ~ gcost +wait +travel, TM, reflevel = "car")
#show results
summary(ml.TM)


#how fitted choice probability match with data
apply(fitted(ml.TM, outcome=FALSE), 2, mean) # fitted mean choice probability




########### 
Hbasic <- mlogit.data(finaldata, shape="long", choice="choice", alt.var = "rota", id="id", opposite = c('custo','morte', 'tempo'))

mxl.basic500<- mlogit(choice~custo+morte+tempo, Hbasic, 
                      rpar=c(custo='cn', morte='cn', tempo='cn'), 
                      R=500, halton=NA, print.level=0, panel=TRUE)
summary(mxl.basic500)
##############

TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", 
                  chid.var = "individual", alt.var = "mode", drop.index = TRUE)
head(TM)

ml.TM <- mlogit(choice ~ 1 +  gcost +wait +travel, TM, reflevel = "car"
                ,rpar=c(gcost='cn', wait = 'n'),
                R=100, halton=NA, print.level=0)
#show results
summary(ml.TM)


#how fitted choice probability match with data
apply(fitted(ml.TM, outcome=FALSE), 2, mean) # fitted mean choice probability







data("Fishing", package = "mlogit")

rpl <- mlogit(mode ~ price + catch | income, Fishing, varying = 2:9,
              rpar = c(price= 'n', catch = 'n'), correlation = TRUE,
              alton = NA, R = 50)
summary(rpl)
rpar(rpl)
cor.mlogit(rpl)
cov.mlogit(rpl)
rpar(rpl, "catch")
summary(rpar(rpl, "catch"))




(-0.072*60)/-0.157
(-.359)/-0.157


