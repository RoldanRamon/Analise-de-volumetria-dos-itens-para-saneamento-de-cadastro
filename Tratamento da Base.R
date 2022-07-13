rm(list = ls())
library(dplyr)
library(ggplot2)
library(janitor)
library(readxl)
library(tidyr)
library(lubridate)


#Importa base de itens ativos
itens <- read_excel(path = '2- base/Itens_Ativos_06_julho.xlsx',sheet = 'base') %>% clean_names() %>% 
  mutate(data_de_inclusao = as_date(data_de_inclusao)) %>% 
  rename(data_criacao_do_item = data_de_inclusao, criador_do_item = nome_usuario_inclusao) %>% 
  select(-nome_empresa)

#Importa gasto com ocs
ocs <- read_excel('2- base/Ordens_De_Compra_Desde_2019.xlsx') %>% clean_names() %>% 
  mutate(data_inclusao = as_date(data_inclusao),ano_oc = year(data_inclusao)) %>% 
  group_by(ano_oc,nome_empresa,nome_filial,ordem_compra,codigo_item,modalidade) %>% 
  summarise(total=1) %>% ungroup() %>% 
  group_by(ano_oc,codigo_item,modalidade) %>% 
  summarise(total=sum(total)) %>%
  pivot_wider(names_from = modalidade,values_from = total,values_fn = sum,values_fill = 0) %>% 
  clean_names() %>% arrange(desc(ano_oc),codigo_item)

#Importa gasto com contratos
contrato <- read_excel('2- base/Contratos_Desde_2019.xlsx') %>% clean_names() %>% 
  mutate(ano_contrato = year(data_inclusao_contrato)) %>% 
  group_by(ano_contrato,numero_contrato,tipo_contrato,codigo_item) %>% summarise(total = 1) %>% ungroup() %>%
  group_by(ano_contrato,codigo_item,tipo_contrato) %>% summarise(total = sum(total)) %>% ungroup() %>% 
  pivot_wider(names_from = tipo_contrato,values_from = total, values_fn = sum,values_fill = 0) %>%
  clean_names() %>% arrange(desc(ano_contrato),codigo_item)
  
#Mostra base de itens ativos e seus gastos
base <- itens %>% left_join(ocs,by=c('codigo_referencia'='codigo_item')) %>% 
  left_join(contrato,by=c('codigo_referencia'='codigo_item')) %>% 
  mutate(analise = if_else(!is.na(ano_oc),'com_gasto',
                          if_else(is.na(ano_contrato), 'sem_gasto','com_gasto')))

#Exporta relatório em excel
writexl::write_xlsx(base,path = '4- relatorio/06_Julho_relatorio_itens_ativos_e_seus_gastos.xlsx')

#grafico para mostrar volume sem gasto das Top 10 subfamilias
graf <- base %>% filter(analise=='sem_gasto') %>% mutate(ano_criacao_item = year(data_criacao_do_item)) %>% 
  group_by(ano_criacao_item,codigo_referencia,nome_familia) %>% summarise(total = 1) %>% ungroup() %>% 
  group_by(ano_criacao_item,nome_familia) %>% summarise(total = sum(total)) %>% arrange(desc(total))

graf_top_10 <- graf %>% pivot_wider(names_from = ano_criacao_item,values_from = total,values_fn = sum) %>% 
  mutate(total = `2017`+`2018`+`2019`+`2020`+`2021`+`2022`) %>% arrange(desc(total)) %>% top_n(total,n = 10) %>% 
  pull(nome_familia)

ggplot(graf %>% filter(nome_familia %in% graf_top_10), aes(x = reorder(nome_familia,total),y = total))+
  geom_col(fill='forestgreen')+
  coord_flip()+
  facet_grid(~ano_criacao_item,scales = 'free')+
  geom_text(aes(label=total),vjust=.3,size=3)+
  xlab('')+ylab('')+labs(title = 'Top 10 subfamilias com mais itens sem utilização',subtitle = 'Por Ano de criação do item no Benner')+
  theme_test()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.text.y = element_text(size = 6))

#Quantidade Total de Itens
graf_2 <- base %>% filter(analise=='sem_gasto') %>% mutate(ano_criacao_item = year(data_criacao_do_item)) %>% 
  group_by(ano_criacao_item,codigo_referencia) %>% summarise(total = 1) %>% ungroup() %>% 
  group_by(ano_criacao_item) %>% summarise(total = sum(total)) %>% arrange(desc(total))

ggplot(graf_2, aes(x = ano_criacao_item, y = total, label = format(x = total,big.mark='.',decimal.mark=',')))+
  geom_point(stat='identity', color="purple", size=17) +
  geom_segment(aes(x = ano_criacao_item, y = 0, xend=ano_criacao_item, yend=total), color='purple')+
  geom_text(color="white", size=4)+
  labs(title="Quantidade de itens sem utilização", 
       subtitle="Por Ano de Criação",x="",y="")+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 15))+
  geom_ribbon(aes(xmin=2017,xmax=2020),fill='#888888',alpha=.20)+ #78,2
  annotate(geom = 'text',x = 2018.5 ,y = 10500, label=paste0('78,2%'), color='#888888',size=6,alpha=.4)+
  geom_ribbon(aes(xmin=2020,xmax=2021),fill='#B9B9B9',alpha=.20)+ #19,3
  annotate(geom = 'text',x = 2020.5 ,y = 10500, label=paste0('19,3%'), color='#B9B9B9',size=6,alpha=.4)+
  geom_ribbon(aes(xmin=2021,xmax=2022),fill='#E2E3E5',alpha=.20)+ #2,4  +
  annotate(geom = 'text',x = 2021.5 ,y = 10500, label=paste0('2,4%'), color='#E2E3E5',size=6,alpha=.8)+
  annotate(geom = 'text',x = 2021.500 ,y = 25000, label=paste0('Total: ',format(x = sum(graf_2$total),big.mark='.',decimal.mark=',') ), color='purple',size=10)


  