

install.packages("caret")
install.packages("tidyverse")
install.packages("openxlsx")
install.packages("yardstick")


library(caret)
library(tidyverse)
library(data.table)
library(rsample)
library(yardstick)

setwd("C:/Users/Mingo/Desktop/DataScience/")

red_wine<-data.table(read.delim("Datos/winequality-red.csv",sep=";"))
glimpse(red_wine)
#Previsualizamos la variable target
table(red_wine$quality)
ggplot(red_wine, aes(x = as.factor(quality))) + geom_bar()

#Queremos cambiar el formato de la tabla a largo
#Esto será útil para estudiar por separado las variables

# tidy_wine <- red_wine %>%
#   gather(key, value, -quality) %>%
#   mutate(quality = as.factor(quality))

#Gather agrupa en la variable "key" los nombres de las columnas,
#en "value" su valor, y quality es la que no agrupa

#El siguiente código hace lo mismo en versión datatable.
#id.vars es la que no agrupa

tidy_wine<-melt(red_wine,id.vars = "quality")
#tidy_wine[,quality:=as.factor(quality)]

#Imprimimos las gráficas para estudiar primeras correlaciones

tidy_wine[,mean_value:=mean(value),by=c('variable','quality')]

tidy_wine<-unique(tidy_wine[,list(quality,variable,mean_value)])
tidy_wine[,mean_value:=(mean_value-min(mean_value))/(max(mean_value)-min(mean_value)),by=c('variable')]

ggplot(tidy_wine,aes(quality, mean_value, group = variable)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point(size = 2) +
  facet_wrap(~variable)

# tidy_wine %>%
#   group_by(key, quality) %>%
#   summarise(value = mean(value)) %>%
#   mutate(value = (value-min(value))/(max(value)-min(value))) %>%
#   ggplot(aes(quality, value, group = key)) +
#   geom_line(alpha = 0.5, size = 1.5) +
#   geom_point(size = 2) +
#   facet_wrap(~key)


#Dividimos en entrenamiento y test
#Será más fácil trabajar con un dataframe

red_wine <- as.data.frame(red_wine)

set.seed(1234)
red_wine_split <- red_wine %>%
  initial_split(prop = 0.8, strata = "quality")

red_wine_train <- training(red_wine_split)
red_wine_test <- testing(red_wine_split)

fit_lm <- train(quality ~ ., 
                method = "lm", 
                data = red_wine_train,
                trControl = trainControl(method = "none"))


fit_rf <- train(quality ~ ., 
                method = "rf", 
                data = red_wine_train,
                importance = TRUE,
                trControl = trainControl(method = "none"))

#Evaluamos resultados con el test

results <- red_wine_test %>%
  mutate(`Linear regression` = predict(fit_lm, red_wine_test),
         `Random forest` = predict(fit_rf, red_wine_test))

metrics(results, truth = quality, estimate = `Linear regression`)
metrics(results, truth = quality, estimate = `Random forest`)


#Métodos de remuestreo

set.seed(1234)
red_wine_lm_bt <- train(quality ~ ., 
                        method = "lm", 
                        data = red_wine_train,
                        trControl = trainControl(method = "boot", number = 5))

set.seed(1234)
red_wine_rf_bt <- train(quality ~ ., 
                        method = "rf", 
                        data = red_wine_train,
                        trControl = trainControl(method = "boot", number = 5))


red_wine_lm_bt
red_wine_rf_bt


results <- red_wine_test %>%
  mutate(`Linear regression` = predict(red_wine_lm_bt, red_wine_test),
         `Random forest` = predict(red_wine_rf_bt, red_wine_test))

metrics(results, truth = quality, estimate = `Linear regression`)
metrics(results, truth = quality, estimate = `Random forest`)


#Pintamos las diferencias

results %>%
  gather(Method, Result, `Linear regression`:`Random forest`) %>%
  ggplot(aes(quality, Result, color = Method)) +
  geom_point(size = 1.5, alpha = 0.5) +
  facet_wrap(~Method) +
  geom_abline(lty = 2, color = "gray50") +
  geom_smooth(method = "lm")











