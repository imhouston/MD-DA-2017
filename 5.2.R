###
##Логистическая регрессия
###


#Возможно потребуется установить пакет
if("ISLR" %in% rownames(installed.packages()) == FALSE) 
{
  install.packages("ISLR")
}
library(ISLR)
attach(Smarket)
help("Smarket")


##Разделим данные на два подмножества: для обучения и для проверки
##Выберем для обучения данные ранее 2005г, остальные используем для проверочного подмножества

#Векторы данных, содержат флажки для каждой далее отбираемой строки
train <- Year < 2005
test <- !train


#Выберем данные из 8го столбца (Today)
training_data = Smarket[train, -8]
testing_data = Smarket[test, -8]

#для тестов отберём значения колонки Direction которые будем стараться предсказать
testing_y = Direction[test]

#Выполним подгонку логистической модели. Знак . означает выбор всех предикторов (влияющих переменных)
fit <- glm(Direction ~ ., data = training_data, family = "binomial")
summary(fit)

########
#Дефолты
########
attach(Default)
help("Default")

summary(Default)
train_d <- income < mean(income)
test_d <- !train_d

training_data_d <- Default[train_d,]
testing_data_d <- Default[test_d, -1]

testing_y_d <- default[test_d]
fit_d <- glm(default ~ ., data = training_data_d, family = "binomial")
summary(fit_d)

#предскажем вероятность дефолта
logistic_probabs_d <- predict(fit_d, testing_data_d, type = "response")
head(logistic_probabs_d)

#просматривать вероятности не очень удобно, категоризируем вероятность
#приняв за дефолт вероятность большую чем 50%

#Подготовим вектор с длиной равной вектору проверочных данных 
logistic_pred_y_d <- rep('No',  length(testing_y_d)) #как мы поминм из лаборторных! rep повторяет занчение указанное чилсо раз
#Изменим флаг дефолта для тех у кого вероятность этого больше 50%
logistic_pred_y_d[logistic_probabs_d > 0.5] = 'Yes'

#Покажем таблицу истинности
table(logistic_pred_y_d, testing_y_d)

#процент ошибок классифицирования
mean(logistic_pred_y_d != testing_y_d)



