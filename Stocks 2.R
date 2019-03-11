library(tidyverse)  
# Parsing of HTML/XML files  
library(rvest)    
# String manipulation
library(stringr)   
# Verbose regular expressions
library(rebus)     
# Eases DateTime manipulation
library(lubridate)
# Manipular dados
library(dplyr)
# Uso para unir colunas
library(tidyr)
# Para Gráficos
library(ggplot2)


# Pega nome das Empresas pelo site "ADVFN" ------------------------------

# Url do site, de A a Z
url <- paste("https://br.advfn.com/bolsa-de-valores/bovespa/",LETTERS,sep="")

# Função para pegar a tabela com o nome das empresas
nomes <- function(url){
  advfn <- read_html(url)
  companies <- advfn %>% 
    # The '.' indicates the class
    html_nodes('.atoz-link-bov') %>%      
    html_table()
  #companies1 <- companies[[1]]
  companies
}

# Cria um Data Frame com os nomes
empresas <- lapply(url, nomes)
empresas <- empresas%>%
  unlist(empresas,recursive=FALSE)%>%
  bind_rows(.id = "Ação")
empresas[is.na(empresas)] = ''
colnames(empresas)[3:26] <- seq(1:24)

# Une as colunas
empresas <- data.frame(empresas[,2],
                 unite(empresas[,3:26], "ticker", sep=''))%>%
  filter(nchar(ticker)==5)


# Fundamentos da ação --------------------------------------------

# Histórico de dividendos
dividendos <- function(nome){
  url2 = paste("http://www.fundamentus.com.br/proventos.php?papel=",nome,"&tipo=2",sep="")
  download.file(url2, destfile = "scrapedpage.html", quiet=TRUE)
  dados1 <- read_html("scrapedpage.html")
  df3 <- tryCatch(html_table(html_nodes(dados1, "table")[[1]]) %>%
                    mutate(Valor=as.numeric(gsub(",",".",Valor)))%>%
                    mutate(Data=dmy(gsub("/","-",Data)))%>%
                    mutate(pr=Valor/as.numeric(`Por quantas ações`))%>%
                    mutate(ano=substr(Data,1,4))%>%
                    mutate(ticker=nome), error = function(e) NA)
  df3
}

# Pl, Div. Yield, Cotação, Dívida, Ativo, setor, roic, dividendos
empresaT <- function(nome){
  
  # Pega a 1 página do fundamentus
  url1 = paste("http://www.fundamentus.com.br/detalhes.php?papel=",nome,"&tipo=2",sep="")
  download.file(url1, destfile = "scrapedpage1.html", quiet=TRUE)
  dados <- read_html("scrapedpage1.html") %>% 
    # The '.' indicates the class
    html_nodes('.txt') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()  
  
  # Pega o histórico de dividendos
  df3 <- dividendos(nome)
  
  # Cria um data.frame com as características da empresa
  nome <- data.frame(as.character(nome))%>%
    mutate(cotacao=sub(",",".",dados[4]))%>%
    mutate(setor=dados[14])%>%
    mutate(valor.mercado=as.numeric(gsub("\\.","",dados[22])))%>%
    mutate(pl=as.numeric(sub(",",".",dados[33])))%>%
    mutate(roic=as.numeric(sub("%","",sub(",",".",dados[65]))))%>%
    mutate(div.yield=as.numeric(sub("%","",(sub(",",".",dados[68])))))%>%
    mutate(divida.liquida=as.numeric(gsub("\\.","",dados[94])))%>%
    mutate(ativos.liq=as.numeric(gsub("\\.","",dados[96])))%>%
    mutate(ticker=as.character(nome))%>%
    mutate(inclinacao=tryCatch(as.numeric(lm(df3$pr~df3$ano)$coefficients[2]),
                               error = function(e) NA)[[1]])%>%
    mutate(anos.div=tryCatch(length(year(df3$Data[!duplicated(year(df3$Data))])), 
                             error = function(e) NA)[[1]])%>%
    mutate(anos.div.min=tryCatch(min((year(df3$Data[!duplicated(year(df3$Data))]))), 
                                 error = function(e) NA)[[1]])%>%
    mutate(anos.div.max=tryCatch(max((year(df3$Data[!duplicated(year(df3$Data))]))), 
                                 error = function(e) NA)[[1]])%>%
    mutate(div.12.meses=tryCatch(df3 %>% filter(Data>=today()-365) %>% summarise(sum(Valor)),
                                 error = function(e) NA)[[1]])
  }


# Dados de todas as empresas
df1 <- lapply(empresas$ticker,empresaT)
y <- colnames(df1[[2]])
df1 <- data.frame(matrix(unlist(df1), nrow=length(df1), byrow=T),stringsAsFactors=FALSE)
colnames(df1) <- y

# Encontra o yield real (o Fundamentus as vezes erra)
df1 <- df1%>%
  mutate(div.yield.real=as.numeric(div.12.meses)/as.numeric(cotacao))

df <- left_join(empresas,df1)
rm(df1)

# Empresas com bom fundamento
df.filter <- df%>%
  filter(as.numeric(div.yield.real)>0.06)%>%
  filter(as.numeric(ativos.liq)-as.numeric(divida.liquida)>0)%>%
  filter(as.numeric(pl)<18&as.numeric(pl)>0)%>%
  filter(as.numeric(anos.div.min)<2009)%>%
  mutate(dif.ano=as.numeric(anos.div.max)-as.numeric(anos.div.min)-as.numeric(anos.div))%>%
  arrange(desc(as.numeric(div.yield.real)))

write.csv2(df,paste(today(),"tudo.csv",sep=""),row.names = FALSE)

# Plot --------------------------------------------------------------------

empresas.final <- df.filter[!duplicated(df.filter$empresas...2.),]

dividendo <- lapply(df.filter$ticker, dividendos)
dividendo <- do.call(rbind.data.frame, dividendo)
dividendo1 <- left_join(dividendo,df,by="ticker")%>%
  mutate(div.sob.cotação=pr/as.numeric(cotacao))

# Base do gráfico para dividendos
ggplot(dividendo1%>% filter(as.numeric(ano)>2005),
       aes(x=ano, y=pr))+geom_bar(stat="identity", fill="steelblue")+
  ggtitle("Dividendos ao longo de 13 anos")+facet_wrap(~ticker)+ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Modelo de gordon --------------------------------------------------------

# retorno no empreendimento (k)
k <- 0.10

# Crescimento do dividendo (g)
g <- 0.01

# Valor da ação: dividendos desse ano/(k-g)
agrupamento <- filter(dividendo1,as.numeric(ano)>2005)%>%
  group_by(ticker,cotacao,ano,div.12.meses)%>%
  summarise(div.ano=sum(pr))%>%
  arrange(div.ano)%>%
  group_by(ticker,cotacao,div.12.meses)%>%
  summarise(dividendo=median(div.ano))%>%
  mutate(valor.ideal=as.numeric(dividendo)/(k-g))%>%
  mutate(crescimento=(valor.ideal-as.numeric(cotacao))/as.numeric(cotacao))
