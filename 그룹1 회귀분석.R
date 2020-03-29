### 그룹 1 회귀분석

## 그룹 1 분류

analysis_1 <- analysis_total_Fixed %>% filter(시도 %in% c('전남' ,'제주'))

analysis_1 <- as.data.frame(analysis_1)


## 2016년 1분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter1 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('01','02'))

View(analysis_1_2016_quarter1)

analysis_1_2016_quarter1 <- 
  analysis_1_2016_quarter1 %>% 
    dplyr::select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2016_quarter1, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter1, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(data = analysis_1_2016_quarter1,formula = 발생률 ~ .)

cor(analysis_1_2016_quarter1)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2016년 2분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter2 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('03','04','05'))

View(analysis_1_2016_quarter2)

analysis_1_2016_quarter2 <- 
  analysis_1_2016_quarter2 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2016_quarter2, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter2, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(data = analysis_1_2016_quarter2,formula = 발생률 ~ .)

cor(analysis_1_2016_quarter2)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2016년 3분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter3 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('06','07','08'))

View(analysis_1_2016_quarter3)

analysis_1_2016_quarter3 <- 
  analysis_1_2016_quarter3 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2016_quarter3, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter3, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(data = analysis_1_2016_quarter3,formula = 발생률 ~ .)

cor(analysis_1_2016_quarter3)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2016년 4분기

# 필요변수 생성 및 추출

analysis_1_2016_quarter4 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('09','10','11'))

View(analysis_1_2016_quarter4)

analysis_1_2016_quarter4 <- 
  analysis_1_2016_quarter4 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2016_quarter4)

fit1 <- lm(data = analysis_1_2016_quarter4, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2016_quarter4, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(formula = 발생률 ~ `평균기온(°C)` + NO2 + SO2 + `평균 풍속(m/s)`, 
             data = analysis_1_2016_quarter4)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2017년 1분기

analysis_1_2017_quarter1 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter((substr(일시,1,4) == '2016' & substr(일시,6,7) %in% c('12')) |
           (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('01','02'))
         )

View(analysis_1_2017_quarter1)

analysis_1_2017_quarter1 <- 
  analysis_1_2017_quarter1 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2017_quarter1, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter1, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2017_quarter1)

lm.fit <- lm(formula = 발생률 ~ `최저기온(°C)` + O3 + `평균기온(°C)` + 
               `평균 현지기압(hPa)` + `평균 풍속(m/s)` + NO2, 
             data = analysis_1_2017_quarter1)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2017년 2분기

analysis_1_2017_quarter2 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('03','04','05'))
  )

View(analysis_1_2017_quarter2)

analysis_1_2017_quarter2 <- 
  analysis_1_2017_quarter2 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2017_quarter2, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter2, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2017_quarter2)

lm.fit <- lm(formula = 발생률 ~ `최저기온(°C)` + O3 + `평균기온(°C)` + 
             `평균 현지기압(hPa)` + `평균 풍속(m/s)` + NO2, 
             data = analysis_1_2017_quarter2)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)



## 2017년 3분기

analysis_1_2017_quarter3 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('06','07','08'))
  )

View(analysis_1_2017_quarter3)

analysis_1_2017_quarter3 <- 
  analysis_1_2017_quarter3 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2017_quarter3)

fit1 <- lm(data = analysis_1_2017_quarter3, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter3, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(data = analysis_1_2017_quarter3,formula = 발생률 ~ .)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2017년 4분기

analysis_1_2017_quarter4 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('09','10','11'))
  )

View(analysis_1_2017_quarter4)

analysis_1_2017_quarter4 <- 
  analysis_1_2017_quarter4 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2017_quarter4)

fit1 <- lm(data = analysis_1_2017_quarter4, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2017_quarter4, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(data = analysis_1_2017_quarter4,formula = 발생률 ~ .)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2018년 1분기

analysis_1_2018_quarter1 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter((substr(일시,1,4) == '2017' & substr(일시,6,7) %in% c('12')) |
           (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('01','02'))
  )

View(analysis_1_2018_quarter1)

analysis_1_2018_quarter1 <- 
  analysis_1_2018_quarter1 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2018_quarter1, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter1, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2018_quarter1)

lm.fit <- lm(formula = 발생률 ~ `최저기온(°C)` + O3 + `평균기온(°C)` + 
               `평균 현지기압(hPa)` + `평균 풍속(m/s)` + NO2, 
             data = analysis_1_2018_quarter1)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2018년 2분기

analysis_1_2018_quarter2 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('03','04','05'))
  )

View(analysis_1_2018_quarter2)

analysis_1_2018_quarter2 <- 
  analysis_1_2018_quarter2 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

fit1 <- lm(data = analysis_1_2018_quarter2, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter2, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

cor(analysis_1_2018_quarter2)

lm.fit <- lm(formula = 발생률 ~ `최저기온(°C)` + O3 + `평균기온(°C)` + 
               `평균 현지기압(hPa)` + `평균 풍속(m/s)` + NO2, 
             data = analysis_1_2018_quarter2)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)



## 2018년 3분기

analysis_1_2018_quarter3 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('06','07','08'))
  )

View(analysis_1_2018_quarter3)

analysis_1_2018_quarter3 <- 
  analysis_1_2018_quarter3 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2018_quarter3)

fit1 <- lm(data = analysis_1_2018_quarter3, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter3, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(data = analysis_1_2018_quarter3,formula = 발생률 ~ .)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)


## 2018년 4분기

analysis_1_2018_quarter4 <- analysis_1 %>% 
  mutate(발생률 = 발생건수/인구수 * 10000) %>% 
  filter(
    (substr(일시,1,4) == '2018' & substr(일시,6,7) %in% c('09','10','11'))
  )

View(analysis_1_2018_quarter4)

analysis_1_2018_quarter4 <- 
  analysis_1_2018_quarter4 %>% 
  select(-시도코드,-발생건수,-PM10등급,-PM25등급,-인구수,-년도,-`최다풍향(16방위)`,-시도,-일시)

# 회귀분석

cor(analysis_1_2018_quarter4)

fit1 <- lm(data = analysis_1_2018_quarter4, formula = 발생률 ~ .)
fit2 <- lm(data = analysis_1_2018_quarter4, formula = 발생률 ~ 1)

stepAIC(fit1, direction = 'backward')
stepAIC(fit2, direction = 'forward', scope = list(upper = fit1,lower = fit2))
stepAIC(fit2, direction = 'both', scope = list(upper = fit1,lower = fit2))

lm.fit <- lm(data = analysis_1_2018_quarter4,formula = 발생률 ~ .)

summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)

vif(lm.fit)