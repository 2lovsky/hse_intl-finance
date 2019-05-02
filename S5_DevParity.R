---
title: "R Notebook"
output: "Windows-1252"
---

libs <- c('dplyr','ggplot2','curl','gdata', 'tidyr')
install_libs <- libs[!(libs %in% installed.packages())]
for(lib in install_libs) install.packages(lib, dependences = TRUE)
sapply(libs, require, character = TRUE)




#1. Загрузка данных

file_url <- 'https://www.imf.org/external/pubs/ft/weo/2019/01/weodata/WEOApr2019all.xls'




download.file(file_url, destfile = "data.txt")
d0 <- read.table('data.txt',header = TRUE, encoding="Windows-1252", fill = TRUE, sep =  "\t", quote = "", dec =","
                 )


d0[8732,]
d0 <- d0[-8732,]



colnames(d0)


d0$Country %>% unique


str(d0)


num_cols <- c(paste0('X',1980:1999), paste0('X',2000:2024))



d0[, num_cols] <- apply(d0[, num_cols], 2, function(x) { 
    gsub('"', "", x)
    })

d0[, num_cols] <- apply(d0[, num_cols], 2, function(x) { 
    gsub(",", "", x)
    })



d0 %>% filter(Country == "Russia", WEO.Subject.Code == "NGDP") 




d1 <- d0





for(col in num_cols#[1:length(num_cols)]
    ){
  chr <- d1[col][, col]
  num <- as.numeric(chr)
  d1[col][, col] <- num
}


d1 %>% filter(Country == "Russia", WEO.Subject.Code == "NGDP")




d1 %>% str



keeps <- c(c("Country", "WEO.Subject.Code"),num_cols)
df <- d1[keeps]
str(df)





df_g <- df %>% gather(Year, Value, X1980:X2024)



legend <- unique(d1[c("WEO.Subject.Code", "Subject.Descriptor")])


legend




str(df_g)




head(df_g)



df_g$WEO.Subject.Code %>% unique


df_g<-df_g[!(df_g$WEO.Subject.Code==""),]
d <- df_g %>% spread(WEO.Subject.Code, Value)




d$Year <- (gsub('X', '', d$Year))
d$Date <- as.Date(gsub('X', '', d$Year), forma = "%Y")


str(d)



d %>% select(NGDP, Year, Country) %>% filter(Year == 2017) %>% select(NGDP) -> ngdp17
log(ngdp17$NGDP)%>% hist


legend %>% filter(Subject.Descriptor == "Implied PPP conversion rate")



d$EXR = d$NGDP / d$NGDPD
d$NGDPDPC_LOG = log(d$NGDPDPC)
d$DEVPPP <- d$EXR / d$PPPEX -1




dg <- d %>%
  gather(key = "Variable", value = "Value", -c(Country, Year, Date))


str(dg)


dg1 <-  dg %>% filter(Country == "Russia", Variable == "EXR" | Variable == "PPPEX") %>% select(Date, Variable, Value)



head(dg1)



ggplot(dg1, 
       aes(x = Date, y = Value, col = Variable)) + 
  geom_line(aes(color = Variable), size = 1) 



dg0 <- dg %>% filter(Country == "Russia", Variable == "DEVPPP") %>% select(Date, Variable, Value)

ggplot(dg0, 
       aes(x = Date, y = Value, col = Variable)) + 
  geom_line(aes(color = Variable)) 








d_scatter <- d %>% filter(Year == 1994 | Year == 2018) %>% 
  select(Country, NGDPDPC_LOG, DEVPPP, Year)
ggplot(d_scatter, aes(x=NGDPDPC_LOG, y=DEVPPP, color = Year)) + geom_point() + geom_smooth(method = lm)


model <- lm(DEVPPP~NGDPDPC_LOG, d %>% filter(Year == 2018))
summary(model)





