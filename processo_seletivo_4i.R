setwd('C:\\Users\\Hugol\\Downloads\\4intelligence')
install.packages('kable')
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(sidrar)
library(kable)

#leitura da base
#extração do nome das planilhas
sheet_names <- excel_sheets("dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx")          
sheet_names

list_all <- lapply(sheet_names, function(x) {          
  as.data.frame(read_excel("dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx", sheet = x)) } )

names(list_all) <- sheet_names    

diesel=list_all$diesel %>%
  gather('ano','diesel',3:23) %>%
  rename(mes=meses,
         cd_uf=regiao)
gasolina=list_all$gasolina %>%
  gather('ano','gasolina',3:23) %>%
  rename(mes=meses,
         cd_uf=regiao)
etanol=list_all$etanol %>%
  gather('ano','etanol',3:23) %>%
  rename(mes=meses,
         cd_uf=regiao)
#definição de parâmetros
trim1=c(1,2,3)
trim2=c(4,5,6)
trim3=c(7,8,9)
sem1=c(1,2,3,4,5,6)
anos=c(2000:2020)
#unificação das bases
df=diesel %>%
  full_join(gasolina,by=c('cd_uf','ano','mes')) %>%
  full_join(etanol,by=c('cd_uf','ano','mes')) %>%
  mutate(trimestre=ifelse(mes %in% trim1,1,
                          ifelse(mes %in% trim2,2,
                                 ifelse(mes %in% trim3,3,4))),
         semestre=ifelse(mes %in% sem1,1,2))

df_anual=df %>% group_by(ano,cd_uf) %>%
  summarise(gasolina=mean(gasolina),
            etanol=mean(etanol),
            diesel=mean(diesel)) %>%
  mutate(min_diesel=min(diesel),
         max_diesel=max(diesel),
         min_etanol=min(etanol),
         max_etanol=max(etanol),
         min_gasolina=min(gasolina),
         max_gasolina=max(gasolina))

df_trimestral=df %>% group_by(trimestre,cd_uf) %>%
  summarise(gasolina=mean(gasolina),
            etanol=mean(etanol),
            diesel=mean(diesel)) %>%
  mutate(min_diesel=min(diesel),
         max_diesel=max(diesel),
         min_etanol=min(etanol),
         max_etanol=max(etanol),
         min_gasolina=min(gasolina),
         max_gasolina=max(gasolina))

df_mensal=df %>% group_by(mes,cd_uf) %>%
  summarise(gasolina=mean(gasolina),
            etanol=mean(etanol),
            diesel=mean(diesel)) %>%
  mutate(min_diesel=min(diesel),
         max_diesel=max(diesel),
         min_etanol=min(etanol),
         max_etanol=max(etanol),
         min_gasolina=min(gasolina),
         max_gasolina=max(gasolina))

df_semestral=df %>% group_by(semestre,cd_uf) %>%
  summarise(gasolina=mean(gasolina),
            etanol=mean(etanol),
            diesel=mean(diesel)) %>%
  mutate(min_diesel=min(diesel),
         max_diesel=max(diesel),
         min_etanol=min(etanol),
         max_etanol=max(etanol),
         min_gasolina=min(gasolina),
         max_gasolina=max(gasolina))
#ipca
ipca=read.csv2('ipca.csv') %>%
  mutate(n_indice=as.numeric(n_indice),
         ano=as.numeric(ano))


#preços combustiveis anp
ufs=c('DISTRITO FEDERAL','MINAS GERAIS','SAO PAULO','TOCANTINS','GOIAS','MATO GROSSO','MARANHAO','PARA')
combustiveis=c('ETANOL HIDRATADO','GASOLINA COMUM','OLEO DIESEL')
anp_2001_2012=read_excel('precos_anp_2001_2012.xlsx') %>%
  select(mes,uf,unidade_medida,preco_medio_revenda,combustivel)
anp_2013_2022=read_excel('precos_anp_2013_2022.xlsx')  %>%
  select(mes,uf,unidade_medida,preco_medio_revenda,combustivel)
anp_precos=anp_2001_2012 %>% union(anp_2013_2022) %>%
  mutate(ano=substr(mes,1,4),
         mes=substr(mes,6,7),
         mes=as.numeric(mes),
         ano=as.numeric(ano)) %>%
  subset((uf %in% ufs) & (combustivel %in% combustiveis)) %>%
  spread(combustivel,preco_medio_revenda) %>%
  rename(gasolina_preco_revenda_l=`GASOLINA COMUM`,
         etanol_preco_revenda_l=`ETANOL HIDRATADO`,
         diesel_preco_revenda_l= `OLEO DIESEL`) %>%
  mutate(gasolina_preco_revenda_m3=gasolina_preco_revenda_l*1000,
         etanol_preco_revenda_m3=etanol_preco_revenda_l*1000,
         diesel_preco_revenda_m3=diesel_preco_revenda_l*1000
  )

anp_precos_reais=anp_precos %>% full_join(ipca, by=c('ano','mes')) %>%
  mutate(gasolina_preco_real_l=gasolina_preco_revenda_l*n_indice,
         etanol_preco_real_l=etanol_preco_revenda_l*n_indice,
         diesel_preco_real_l=diesel_preco_revenda_l*n_indice,
         gasolina_preco_real_m3=gasolina_preco_revenda_m3*n_indice,
         etanol_preco_real_m3=etanol_preco_revenda_m3*n_indice,
         diesel_preco_real_m3=diesel_preco_revenda_m3*n_indice)

#Domicílios com carro, ou motocicleta, ou carro e motocicleta
cd_ufs=c('1','53','31','35','17','52','51','21','15')
pct_carros=get_sidra(api='/t/6677/n1/all/n3/all/v/10252/p/all/c845/47554/d/v10252%201') %>%
  spread(`Posse de bens`,`Valor`) %>%
  select(`Brasil e Unidade da Federação (Código)`,`Brasil e Unidade da Federação`,`Ano`,`Carro`) %>%
  rename(n_uf=`Brasil e Unidade da Federação (Código)`,
         uf=`Brasil e Unidade da Federação`,
         ano=`Ano`,
         pct_carro=`Carro`)
pct_motos=get_sidra(api='/t/6677/n1/all/n3/all/v/10252/p/all/c845/47555/d/v10252%201') %>%
  spread(`Posse de bens`,`Valor`) %>%
  select(`Brasil e Unidade da Federação (Código)`,`Brasil e Unidade da Federação`,`Ano`,`Motocicleta`) %>%
  rename(n_uf=`Brasil e Unidade da Federação (Código)`,
         uf=`Brasil e Unidade da Federação`,
         ano=`Ano`,
         pct_moto=`Motocicleta`)
pct_motos_e_carros=get_sidra(api='/t/6677/n1/all/n3/all/v/10252/p/all/c845/47556/d/v10252%201') %>%
  spread(`Posse de bens`,`Valor`) %>%
  select(`Brasil e Unidade da Federação (Código)`,`Brasil e Unidade da Federação`,`Ano`,`Carro e motocicleta`) %>%
  rename(n_uf=`Brasil e Unidade da Federação (Código)`,
         uf=`Brasil e Unidade da Federação`,
         ano=`Ano`,
         pct_moto_e_carro=`Carro e motocicleta`)
pct_veiculos=pct_carros %>% left_join(pct_motos, by=c('n_uf','uf','ano')) %>%
  left_join(pct_motos_e_carros, by=c('n_uf','uf','ano')) %>% subset(n_uf %in% cd_ufs)

#população total
pop=get_sidra(api="/t/6407/n3/all/v/606/p/all/c2/6794/c58/95253")

pop2=pop %>%
  select(`Unidade da Federação (Código)`,`Unidade da Federação`,Valor,Variável,Ano) %>%
  spread(Variável,Valor) %>%
  rename(ano=`Ano`,
         uf = `Unidade da Federação`,
         n_uf = `Unidade da Federação (Código)`,
         vl_populacao = `População`) %>% subset(n_uf %in% cd_ufs)

#rendimento médio
rend_medio=get_sidra(api="/t/7534/n3/all/v/10816/p/all/c1042/49283")

rend_medio2=rend_medio %>%
  select(`Unidade da Federação (Código)`,`Unidade da Federação`,Valor,Variável,Ano) %>%
  spread(Variável,Valor) %>%
  rename(ano=`Ano`,
         uf = `Unidade da Federação`,
         n_uf = `Unidade da Federação (Código)`,
         vl_renda_trab_pc = `Rendimento médio mensal real domiciliar per capita, a preços médios do último ano`) %>% subset(n_uf %in% cd_ufs)

#gráficos
g_ufs=ggplot(df_anual%>%subset(cd_uf!='br' & ano>2014), aes(x=ano,y=diesel, colour=cd_uf, group=cd_uf))+
  geom_line() +
  labs(x="Ano",y='m^3 vendidos') + 
  ggtitle('Série histórica dos últimos 5 anos da venda de combustíveis por UF') +
  theme_economist()

g_ufs
df_anual=as.numeric(ano)
variables=c(diesel,gasolina,etanol)
g_br=ggplot(df_anual%>%subset((cd_uf=='br')&(ano>2014)), aes(x=as.numeric(ano)))+
  geom_line(aes(y=diesel/100,color='diesel')) +
  geom_line(aes(y=gasolina/100,color='gasolina')) +
  geom_line(aes(y=etanol/100,color='etanol'))+
  theme_economist() +
  labs(x="Ano",y='m^3 vendidos (em 100 m^3)') + 
  ggtitle('Série histórica dos últimos 5 anos da venda de combustíveis no Brasil') 
  

g_br

g_precos=ggplot(anp_precos_reais%>%subset(as.numeric(ano)>2014)%>%group_by(ano)%>%summarize(etanol_preco_real_l=mean(etanol_preco_real_l),
                                                                              gasolina_preco_real_l=mean(gasolina_preco_real_l),
                                                                              diesel_preco_real_l=mean(diesel_preco_real_l)), aes(x=as.numeric(ano))) +
  geom_line(aes(y=etanol_preco_real_l,color='etanol_preco_real_l')) +
  geom_line(aes(y=gasolina_preco_real_l,color='gasolina_preco_real_l')) +
  geom_line(aes(y=diesel_preco_real_l,color='diesel_preco_real_l')) +
  theme_economist() +
  labs(x="Ano",y='Valor') +
  ggtitle('Série histórica dos últimos 5 anos do preço real dos combustíveis')

g_precos


g=anp_precos_reais %>% subset(ano>2014) %>% 
  group_by(ano,uf) %>% 
  summarize(etanol_preco_real_l=mean(etanol_preco_real_l),
            gasolina_preco_real_l=sum(gasolina_preco_real_l),
            diesel_preco_real_l=sum(diesel_preco_real_l))
g_precos=ggplot(g, aes(x=as.numeric(ano))) +
  geom_line(aes(y=etanol_preco_real_l,color='etanol_preco_real_l')) +
  geom_line(aes(y=gasolina_preco_real_l,color='gasolina_preco_real_l')) +
  geom_line(aes(y=diesel_preco_real_l,color='diesel_preco_real_l')) +
  theme_economist() +
  labs(x="Ano",y='Preço real em R$') +
  ggtitle('Série histórica dos últimos 5 anos do preço real dos combustíveis')

gpreços
grafico=anp_precos_reais %>% subset(ano>2014) %>%
  group_by(ano) %>%
  summarize(gasolina_preco_real_l=mean(as.numeric(gasolina_preco_real_l),na.rm=TRUE),
            diesel_preco_real_l=mean(as.numeric(diesel_preco_real_l),na.rm=TRUE),
            etanol_preco_real_l=mean(as.numeric(etanol_preco_real_l),na.rm=TRUE))


g=ggplot(grafico, aes(x=ano)) +
  geom_line(aes(y=gasolina_preco_real_l, color='gasolina_preco_real_l')) +
  geom_line(aes(y=diesel_preco_real_l, color='diesel_preco_real_l')) +
  geom_line(aes(y=etanol_preco_real_l, color='etanol_preco_real_l'))
g
grafico=anp_precos_reais %>% subset(ano>2014) %>%
  group_by(ano) %>%
  summarize(gasolina_preco_real_l=mean(as.numeric(gasolina_preco_real_l),na.rm=TRUE),
            diesel_preco_real_l=mean(as.numeric(diesel_preco_real_l),na.rm=TRUE),
            etanol_preco_real_l=mean(as.numeric(etanol_preco_real_l),na.rm=TRUE))

g1=anp_precos_reais %>% subset(ano==2020)
