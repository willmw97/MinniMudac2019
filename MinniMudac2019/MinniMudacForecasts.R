#March forecast
require(fpp2)
require(forecast)
require(seasonal)

March = read.csv("MarchSoybeans.csv")
names(March)
March_Price=March$Price
March_Price = ts(March_Price)
autoplot(March_Price)
head(March_Price)
autoplot(March_Price)
March.train=head(March_Price,19)
March.test= tail(March_Price,5)
autoplot(March.train)
autoplot(March.test)

fc.mean=meanf(March.train, h=5)
autoplot(fc.mean)
accuracy(fc.mean,March.test)

fc.naive= naive(March.train, h=5)
autoplot(fc.naive)
accuracy(fc.naive,March.test)

fc.drift = rwf(March.train, h=5, drift=T)
autoplot(fc.drift)
accuracy(fc.drift, March.test)

SES_March = ses(March.train, h=5)
autoplot(SES_March)
accuracy(SES_March,March.test)

fc.1 = ses(March.train, alpha = .1, initial = "simple", h=5)
autoplot(fc.1)
accuracy(fc.1,March.test)

fc.2 = ses(March.train, alpha = .2, initial = "simple", h=5)
autoplot(fc.2)
accuracy(fc.2,March.test)

fc.4 = ses(March.train, alpha = .4, initial = "simple", h=5)
autoplot(fc.4)
accuracy(fc.4, March.test)

fc.6 = ses(March.train, alpha = .6, initial = "simple", h=5)
autoplot(fc.6)
accuracy(fc.6, March.test)

fc_holts = holt(March.train, h=5)
autoplot(fc_holts)
accuracy(fc_holts, March.test)

fc_ets = ets(March.train)
autoplot(fc_ets)
fc_ets1 = forecast(fc_ets, h=5)
autoplot(fc_ets1)
accuracy(fc_ets1, March.test)

fc_arima = auto.arima(March.train)
fc_arima1 = forecast(fc_arima, h=5)
autoplot(fc_arima1)
accuracy(fc_arima1, March.test)

March_arima = auto.arima(March_Price)
March_forecast = forecast(March_arima, h=5)
autoplot(March_forecast)

#March_forecast

March_Nov8 = read.csv("MaySoybeans.csv")
names(March_Nov8)
March_Price_Nov8=March_Nov8$March
March_Price_Nov8_1 = ts(March_Price_Nov8)
autoplot(March_Price_Nov8_1)

autoplot(March_Price_Nov8_1)+xlab("Days") + ylab("Price") +
  autolayer(March_forecast$mean, series="Arima(1,0,0)")

#May Forecast
require(fpp2)
require(forecast)
require(seasonal)

May = read.csv(file.choose())
names(May)
May_Price=May$price
May_Price1 = ts(May_Price)
autoplot(May_Price1)

May.train=head(May_Price1,19)
autoplot(May.train)
May.test= tail(May_Price1,5)
autoplot(May.test)

fc_arima_May = auto.arima(May.train)
fc_arima_May1 = forecast(fc_arima_May, h=5)
autoplot(fc_arima_May1)
accuracy(fc_arima_May1, May.test)

May_arima = auto.arima(May_Price1)
May_forecast = forecast(May_arima, h=5)
autoplot(May_forecast)

#May_forecast

May_Nov8 = read.csv("MaySoybeans.csv")
names(May_Nov8)
May_Price_Nov8=May_Nov8$May
May_Price_Nov8_1 = ts(May_Price_Nov8)
autoplot(May_Price_Nov8_1)

autoplot(May_Price_Nov8_1)+xlab("Days") + ylab("Price") +
  autolayer(May_forecast$mean, series="Arima(1,0,0)")


#July
July = read.csv("JulySoybeans.csv")
names(July)


July_Price=July$Price
July_Price1 = ts(July_Price)
autoplot(July_Price1)

July.train=head(July_Price1,19)
autoplot(July.train)
July.test= tail(July_Price1,5)
autoplot(July.test)

fc_arima_July = auto.arima(July.train)
fc_arima_July1 = forecast(fc_arima_July, h=5)
autoplot(fc_arima_July1)
accuracy(fc_arima_July1, July.test)

fc_arima_July1

fc_arima_July2 = auto.arima(July.test)
fc_arima_July3 = forecast(fc_arima_July2, h=5)
autoplot(fc_arima_July3)

fc_arima_July3

July_arima = auto.arima(July_Price1)
July_forecast = forecast(July_arima, h=5)
autoplot(July_forecast)

#July_forecast

July_Nov8 = read.csv("JulySoybeans.csv")
names(July_Nov8)
July_Price_Nov8=July_Nov8$July
July_Price_Nov8_1 = ts(July_Price_Nov8)
autoplot(July_Price_Nov8_1)

autoplot(July_Price_Nov8_1)+xlab("Days") + ylab("Price") +
  autolayer(July_forecast$mean, series="Arima(1,0,0)")
