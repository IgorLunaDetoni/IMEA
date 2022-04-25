




# Vamos ajeitar essa série temporal


algodao_timeseries <- Primeira_região %>%select(c(Data,Valor_dólar,Cidade))
algodao_timeseries <- algodao_timeseries %>%  dplyr::filter(Cidade == "Diamantino")
algodao_timeseries <- algodao_timeseries %>%select(c(Data,Valor_dólar))
algodao_timeseries$Data <- ymd(algodao_timeseries$Data)
algodao_timeseries <- algodao_timeseries %>% na.omit()


inds <- seq(as.Date("2019-02-01"), as.Date("2022-02-01"), by = "day")


algodao_timeseries2 <- ts(algodao_timeseries,     # Dados
                          start = c(2019, as.numeric(format(inds[1], "%j"))),
                          frequency = 365)


#Pra não ter probblema com o time series do TBATS
x.msts <- msts(algodao_timeseries[,2],seasonal.periods=c(7,265.4))
decompose(x.msts[,1]) %>% autoplot()



model <- tbats(algodao_timeseries2[,2])


plot(forecast(model,h=90))



ArchTest((algodao_timeseries$Valor_dólar))

adf.test(algodao_timeseries$Valor_dólar)
adf.test(log(algodao_timeseries$Valor_dólar))
adf.test(diff(algodao_timeseries$Valor_dólar))



