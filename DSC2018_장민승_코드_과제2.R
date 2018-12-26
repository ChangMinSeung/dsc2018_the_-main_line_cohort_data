####필요한 library 실행#####
library(readr) #csv 파일 읽기
library(dplyr) #pipe 연사자
library(reshape2) #melt 함수
library(PerformanceAnalytics)#상관분석 그래프
library(ggplot2) #그래프
library(rpart) #회귀 트리
library(rpart.plot) #회귀 트리 그래프
library(partykit) #회귀 트리 그래프

#####


#####데이터 가져오기#####
bioage <- read.csv(file.choose())

#####


#####EDA#####
#요약 확인
summary(bioage)
#전체 피처의 결측치 개수 확인
colSums(is.na(bioage))
  #분석에 사용할 피처의 결측치 
sum(is.na(bioage$BMI)) #5(0.00017)
sum(is.na(bioage$BAID)) #0(0)
sum(is.na(bioage$SEX)) #0(0)
sum(is.na(bioage$CRAGE)) #0(0)
sum(is.na(bioage$BFP)) #1558(0.052)
sum(is.na(bioage$MONOCYTE)) #2(6.6e-05)

#데이터 타입 확인 
str(bioage$BMI) 
str(bioage$BAID) 
str(bioage$SEX) 
str(bioage$CRAGE) 
str(bioage$BFP) 
str(bioage$MONOCYTE) 

##박스 플롯 확인
par(mfrow=c(1,3)) 
#10대_남성
box_M_10 <- bioage %>%
            filter(SEX == 1, CRAGE < 20) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_10$BMI, na.rm=T, main="10대_남성_BMI")
boxplot(box_M_10$BFP, na.rm=T, main="10대_남성_BFP")
boxplot(box_M_10$MONOCYTE, na.rm=T, main="10대_남성_MONOCYTE")
#10대_여성
box_W_10 <- bioage %>%
            filter(SEX == 2, CRAGE < 20) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_10$BMI, na.rm=T, main="10대_여성_BMI")
boxplot(box_W_10$BFP, na.rm=T, main="10대_여성_BFP")
boxplot(box_W_10$MONOCYTE, na.rm=T, main="10대_여성_MONOCYTE")
#20대_남성
box_M_20 <- bioage %>%
            filter(SEX == 1, 20 <= CRAGE & CRAGE < 30) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_20$BMI, na.rm=T, main="20대_남성_BMI")
boxplot(box_M_20$BFP, na.rm=T, main="20대_남성_BFP")
boxplot(box_M_20$MONOCYTE, na.rm=T, main="20대_남성_MONOCYTE")
#20대_여성
box_W_20 <- bioage %>%
            filter(SEX == 2, 20 <= CRAGE & CRAGE < 30) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_20$BMI, na.rm=T, main="20대_여성_BMI")
boxplot(box_W_20$BFP, na.rm=T, main="20대_여성_BFP")
boxplot(box_W_20$MONOCYTE, na.rm=T, main="20대_여성_MONOCYTE")
#30대_남성
box_M_30 <- bioage %>%
            filter(SEX == 1, 30 <= CRAGE & CRAGE < 40) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_30$BMI, na.rm=T, main="30대_남성_BMI")
boxplot(box_M_30$BFP, na.rm=T, main="30대_남성_BFP")
boxplot(box_M_30$MONOCYTE, na.rm=T, main="30대_남성_MONOCYTE")
#30대_여성
box_W_30 <- bioage %>%
            filter(SEX == 2, 30 <= CRAGE & CRAGE < 40) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_30$BMI, na.rm=T, main="30대_여성_BMI")
boxplot(box_W_30$BFP, na.rm=T, main="30대_여성_BFP")
boxplot(box_W_30$MONOCYTE, na.rm=T, main="30대_여성_MONOCYTE")
#40대_남성
box_M_40 <- bioage %>%
            filter(SEX == 1, 40 <= CRAGE & CRAGE < 50) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_40$BMI, na.rm=T, main="40대_남성_BMI")
boxplot(box_M_40$BFP, na.rm=T, main="40대_남성_BFP")
boxplot(box_M_40$MONOCYTE, na.rm=T, main="40대_남성_MONOCYTE")
#40대_여성
box_W_40 <- bioage %>%
            filter(SEX == 2, 40 <= CRAGE & CRAGE < 50) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_40$BMI, na.rm=T, main="40대_여성_BMI")
boxplot(box_W_40$BFP, na.rm=T, main="40대_여성_BFP")
boxplot(box_W_40$MONOCYTE, na.rm=T, main="40대_여성_MONOCYTE")
#50대_남성
box_M_50 <- bioage %>%
            filter(SEX == 1, 50 <= CRAGE & CRAGE < 60) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_50$BMI, na.rm=T, main="50대_남성_BMI")
boxplot(box_M_50$BFP, na.rm=T, main="50대_남성_BFP")
boxplot(box_M_50$MONOCYTE, na.rm=T, main="50대_남성_MONOCYTE")
#50대_여성
box_W_50 <- bioage %>%
            filter(SEX == 2, 50 <= CRAGE & CRAGE < 60) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_50$BMI, na.rm=T, main="50대_여성_BMI")
boxplot(box_W_50$BFP, na.rm=T, main="50대_여성_BFP")
boxplot(box_W_50$MONOCYTE, na.rm=T, main="50대_여성_MONOCYTE")
#60대_남성
box_M_60 <- bioage %>%
            filter(SEX == 1, 60 <= CRAGE & CRAGE < 70) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_60$BMI, na.rm=T, main="60대_남성_BMI")
boxplot(box_M_60$BFP, na.rm=T, main="60대_남성_BFP")
boxplot(box_M_60$MONOCYTE, na.rm=T, main="60대_남성_MONOCYTE")
#60대_여성
box_W_60 <- bioage %>%
            filter(SEX == 2, 60 <= CRAGE & CRAGE < 70) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_60$BMI, na.rm=T, main="60대_여성_BMI")
boxplot(box_W_60$BFP, na.rm=T, main="60대_여성_BFP")
boxplot(box_W_60$MONOCYTE, na.rm=T, main="60대_여성_MONOCYTE")
#70대_남성
box_M_70 <- bioage %>%
            filter(SEX == 1, 70 <= CRAGE & CRAGE < 80) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_70$BMI, na.rm=T, main="70대_남성_BMI")
boxplot(box_M_70$BFP, na.rm=T, main="70대_남성_BFP")
boxplot(box_M_70$MONOCYTE, na.rm=T, main="70대_남성_MONOCYTE")
#70대_여성
box_W_70 <- bioage %>%
            filter(SEX == 2, 70 <= CRAGE & CRAGE < 80) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_70$BMI, na.rm=T, main="70대_여성_BMI")
boxplot(box_W_70$BFP, na.rm=T, main="70대_여성_BFP")
boxplot(box_W_70$MONOCYTE, na.rm=T, main="70대_여성_MONOCYTE")
#80대_남성
box_M_80 <- bioage %>%
            filter(SEX == 1, 80 <= CRAGE) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_M_80$BMI, na.rm=T, main="80대_남성_BMI")
boxplot(box_M_80$BFP, na.rm=T, main="80대_남성_BFP")
boxplot(box_M_80$MONOCYTE, na.rm=T, main="80대_남성_MONOCYTE")
#80대_여성
box_W_80 <- bioage %>%
            filter(SEX == 2, 80 <= CRAGE) %>%
            select(BMI, BFP, MONOCYTE)
boxplot(box_W_80$BMI, na.rm=T, main="80대_여성_BMI")
boxplot(box_W_80$BFP, na.rm=T, main="80대_여성_BFP")
boxplot(box_W_80$MONOCYTE, na.rm=T, main="80대_여성_MONOCYTE")

##### 


#####데이터 전처리#####
  #나이, 성별 기준 BMI_식별번호_체지방률_단핵구백분율 데이터 분리 
  #후 나눈 데이터 세트 별 평균으로 결측치 대체및 BMI 분류 판단 컬럼 생성
##10대_남성
bio_M_10 <- bioage%>%
  filter(SEX == 1 & CRAGE < 20) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_10)) #BFP(1)(3.4%)
#10대_남성_BFP의 평균으로 결측치 대체 
bio_M_10_BFP_mean <- round(mean.default(bio_M_10$BFP, na.rm = T), 0) 
bio_M_10$BFP <- ifelse(is.na(bio_M_10$BFP), bio_M_10_BFP_mean, bio_M_10$BFP)
#결측치 확인
colSums(is.na(bio_M_10))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_10$CLINIC <- "NA"
bio_M_10$CLINIC <- ifelse(bio_M_10$BMI >= 18.5 & bio_M_10$BMI < 25, "정상", bio_M_10$CLINIC)
bio_M_10$CLINIC <- ifelse(bio_M_10$BMI >= 0 & bio_M_10$BMI < 18.5 |
                            bio_M_10$BMI >= 25 & bio_M_10$BMI < 30, "경계", bio_M_10$CLINIC)
bio_M_10$CLINIC <- ifelse(bio_M_10$BMI >= 30, "이상", bio_M_10$CLINIC)

##10대_여성
bio_W_10 <- bioage%>%
  filter(SEX == 2 & CRAGE < 20) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_10)) #BFP(2)(10.5%)
#10대_여성_BFP의 평균으로 결측치 대체 
bio_W_10_BFP_mean <- round(mean.default(bio_W_10$BFP, na.rm = T), 0) 
bio_W_10$BFP <- ifelse(is.na(bio_W_10$BFP), bio_W_10_BFP_mean, bio_W_10$BFP)
#결측치 확인
colSums(is.na(bio_W_10))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_10$CLINIC <- "NA"
bio_W_10$CLINIC <- ifelse(bio_W_10$BMI >= 18.5 & bio_W_10$BMI < 25, "정상", bio_W_10$CLINIC)
bio_W_10$CLINIC <- ifelse(bio_W_10$BMI >= 0 & bio_W_10$BMI < 18.5 |
                          bio_W_10$BMI >= 25 & bio_W_10$BMI < 30, "경계", bio_W_10$CLINIC)
bio_W_10$CLINIC <- ifelse(bio_W_10$BMI >= 30, "이상", bio_W_10$CLINIC)

##20대_남성
bio_M_20 <- bioage%>%
  filter(SEX == 1 & 20 <= CRAGE & CRAGE < 30) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_20)) #BFP(49)(7.1%) 
#20대_남성_BFP의 평균으로 결측치 대체 
bio_M_20_BFP_mean <- round(mean.default(bio_M_20$BFP, na.rm = T), 0) 
bio_M_20$BFP <- ifelse(is.na(bio_M_20$BFP), bio_M_20_BFP_mean, bio_M_20$BFP)
#결측치 확인
colSums(is.na(bio_M_20))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_20$CLINIC <- "NA"
bio_M_20$CLINIC <- ifelse(bio_M_20$BMI >= 18.5 & bio_M_20$BMI < 25, "정상", bio_M_20$CLINIC)
bio_M_20$CLINIC <- ifelse(bio_M_20$BMI >= 0 & bio_M_20$BMI < 18.5 |
                          bio_M_20$BMI >= 25 & bio_M_20$BMI < 30, "경계", bio_M_20$CLINIC)
bio_M_20$CLINIC <- ifelse(bio_M_20$BMI >= 30, "이상", bio_M_20$CLINIC)

##20대_여성
bio_W_20 <- bioage%>%
  filter(SEX == 2 & 20 <= CRAGE & CRAGE < 30) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_20)) #BFP(36)(4.4%) 
#20대_여성_BFP의 평균으로 결측치 대체 
bio_W_20_BFP_mean <- round(mean.default(bio_W_20$BFP, na.rm = T), 0) 
bio_W_20$BFP <- ifelse(is.na(bio_W_20$BFP), bio_W_20_BFP_mean, bio_W_20$BFP)
#결측치 확인
colSums(is.na(bio_W_20))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_20$CLINIC <- "NA"
bio_W_20$CLINIC <- ifelse(bio_W_20$BMI >= 18.5 & bio_W_20$BMI < 25, "정상", bio_W_20$CLINIC)
bio_W_20$CLINIC <- ifelse(bio_W_20$BMI >= 0 & bio_W_20$BMI < 18.5 |
                            bio_W_20$BMI >= 25 & bio_W_20$BMI < 30, "경계", bio_W_20$CLINIC)
bio_W_20$CLINIC <- ifelse(bio_W_20$BMI >= 30, "이상", bio_W_20$CLINIC)

##30대_남성
bio_M_30 <- bioage%>%
  filter(SEX == 1 & 30 <= CRAGE & CRAGE < 40) %>%  
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_30)) #BMI(1)(0.03%) #BFP(310)(9.4%) 
#30대_남성_BMI의 평균으로 결측치 대체 
bio_M_30_BMI_mean <- round(mean.default(bio_M_30$BMI, na.rm = T), 0) 
bio_M_30$BMI <- ifelse(is.na(bio_M_30$BMI), bio_M_30_BMI_mean, bio_M_30$BMI)
#30대_남성_BFP의 평균으로 결측치 대체 
bio_M_30_BFP_mean <- round(mean.default(bio_M_30$BFP, na.rm = T), 0) 
bio_M_30$BFP <- ifelse(is.na(bio_M_30$BFP), bio_M_30_BFP_mean, bio_M_30$BFP)
#결측치 확인
colSums(is.na(bio_M_30))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_30$CLINIC <- "NA"
bio_M_30$CLINIC <- ifelse(bio_M_30$BMI >= 18.5 & bio_M_30$BMI < 25, "정상", bio_M_30$CLINIC)
bio_M_30$CLINIC <- ifelse(bio_M_30$BMI >= 0 & bio_M_30$BMI < 18.5 |
                            bio_M_30$BMI >= 25 & bio_M_30$BMI < 30, "경계", bio_M_30$CLINIC)
bio_M_30$CLINIC <- ifelse(bio_M_30$BMI >= 30, "이상", bio_M_30$CLINIC)

##30대_여성
bio_W_30 <- bioage%>%
  filter(SEX == 2 & 30 <= CRAGE & CRAGE < 40) %>%  
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_30)) #BFP(146)(0.05%) 
#30대_여성_BFP의 평균으로 결측치 대체 
bio_W_30_BFP_mean <- round(mean.default(bio_W_30$BFP, na.rm = T), 0) 
bio_W_30$BFP <- ifelse(is.na(bio_W_30$BFP), bio_W_30_BFP_mean, bio_W_30$BFP)
#결측치 확인
colSums(is.na(bio_W_30))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_30$CLINIC <- "NA"
bio_W_30$CLINIC <- ifelse(bio_W_30$BMI >= 18.5 & bio_W_30$BMI < 25, "정상", bio_W_30$CLINIC)
bio_W_30$CLINIC <- ifelse(bio_W_30$BMI >= 0 & bio_W_30$BMI < 18.5 |
                            bio_W_30$BMI >= 25 & bio_W_30$BMI < 30, "경계", bio_W_30$CLINIC)
bio_W_30$CLINIC <- ifelse(bio_W_30$BMI >= 30, "이상", bio_W_30$CLINIC)

##40대_남성
bio_M_40 <- bioage%>%
  filter(SEX == 1 & 40 <= CRAGE & CRAGE < 50) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_40)) #BMI(1)(0.02%) #BFP(425)(7.2%) #MONOCYTE(1)(0.02%) 
#40대_남성_BMI의 평균으로 결측치 대체 
bio_M_40_BMI_mean <- round(mean.default(bio_M_40$BMI, na.rm = T), 0) 
bio_M_40$BMI <- ifelse(is.na(bio_M_40$BMI), bio_M_40_BMI_mean, bio_M_40$BMI)
#40대_남성_BFP의 평균으로 결측치 대체 
bio_M_40_BFP_mean <- round(mean.default(bio_M_40$BFP, na.rm = T), 0) 
bio_M_40$BFP <- ifelse(is.na(bio_M_40$BFP), bio_M_40_BFP_mean, bio_M_40$BFP)
#40대_남성_MONOCYTE의 평균으로 결측치 대체 
bio_M_40_MONOCYTE_mean <- round(mean.default(bio_M_40$MONOCYTE, na.rm = T), 0) 
bio_M_40$MONOCYTE <- ifelse(is.na(bio_M_40$MONOCYTE), bio_M_40_MONOCYTE_mean, bio_M_40$MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_40))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_40$CLINIC <- "NA"
bio_M_40$CLINIC <- ifelse(bio_M_40$BMI >= 18.5 & bio_M_40$BMI < 25, "정상", bio_M_40$CLINIC)
bio_M_40$CLINIC <- ifelse(bio_M_40$BMI >= 0 & bio_M_40$BMI < 18.5 |
                          bio_M_40$BMI >= 25 & bio_M_40$BMI < 30, "경계", bio_M_40$CLINIC)
bio_M_40$CLINIC <- ifelse(bio_M_40$BMI >= 30, "이상", bio_M_40$CLINIC)

##40대_여성
bio_W_40 <- bioage%>%
  filter(SEX == 2 & 40 <= CRAGE & CRAGE < 50) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_40)) #BMI(2)(0.05%) #BFP(152)(3.7%) #MONOCYTE(1)(0.02%)
#40대_여성_BMI의 평균으로 결측치 대체 
bio_W_40_BMI_mean <- round(mean.default(bio_W_40$BMI, na.rm = T), 0) 
bio_W_40$BMI <- ifelse(is.na(bio_W_40$BMI), bio_W_40_BMI_mean, bio_W_40$BMI)
#40대_여성_BFP의 평균으로 결측치 대체 
bio_W_40_BFP_mean <- round(mean.default(bio_W_40$BFP, na.rm = T), 0) 
bio_W_40$BFP <- ifelse(is.na(bio_W_40$BFP), bio_W_40_BFP_mean, bio_W_40$BFP)
#40대_여성_MONOCYTE의 평균으로 결측치 대체 
bio_W_40_MONOCYTE_mean <- round(mean.default(bio_W_40$MONOCYTE, na.rm = T), 0) 
bio_W_40$MONOCYTE <- ifelse(is.na(bio_W_40$MONOCYTE), bio_W_40_MONOCYTE_mean, bio_W_40$MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_40))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_40$CLINIC <- "NA"
bio_W_40$CLINIC <- ifelse(bio_W_40$BMI >= 18.5 & bio_W_40$BMI < 25, "정상", bio_W_40$CLINIC)
bio_W_40$CLINIC <- ifelse(bio_W_40$BMI >= 0 & bio_W_40$BMI < 18.5 |
                          bio_W_40$BMI >= 25 & bio_W_40$BMI < 30, "경계", bio_W_40$CLINIC)
bio_W_40$CLINIC <- ifelse(bio_W_40$BMI >= 30, "이상", bio_W_40$CLINIC)  

##50대_남성
bio_M_50 <- bioage%>%
  filter(SEX == 1 & 50 <= CRAGE & CRAGE < 60) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_50)) #BMI(1)(0.02%) #BFP(240)(4.8%)
#50대_남성_BMI의 평균으로 결측치 대체 
bio_M_50_BMI_mean <- round(mean.default(bio_M_50$BMI, na.rm = T), 0) 
bio_M_50$BMI <- ifelse(is.na(bio_M_50$BMI), bio_M_50_BMI_mean, bio_M_50$BMI)
#50대_남성_BFP의 평균으로 결측치 대체 
bio_M_50_BFP_mean <- round(mean.default(bio_M_50$BFP, na.rm = T), 0) 
bio_M_50$BFP <- ifelse(is.na(bio_M_50$BFP), bio_M_50_BFP_mean, bio_M_50$BFP)
#결측치 확인
colSums(is.na(bio_M_50))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_50$CLINIC <- "NA"
bio_M_50$CLINIC <- ifelse(bio_M_50$BMI >= 18.5 & bio_M_50$BMI < 25, "정상", bio_M_50$CLINIC)
bio_M_50$CLINIC <- ifelse(bio_M_50$BMI >= 0 & bio_M_50$BMI < 18.5 |
                            bio_M_50$BMI >= 25 & bio_M_50$BMI < 30, "경계", bio_M_50$CLINIC)
bio_M_50$CLINIC <- ifelse(bio_M_50$BMI >= 30, "이상", bio_M_50$CLINIC)

##50대_여성
bio_W_50 <- bioage%>%
  filter(SEX == 2 & 50 <= CRAGE & CRAGE < 60) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_50)) #BFP(76)(2.1%)
#50대_여성_BFP의 평균으로 결측치 대체 
bio_W_50_BFP_mean <- round(mean.default(bio_W_50$BFP, na.rm = T), 0) 
bio_W_50$BFP <- ifelse(is.na(bio_W_50$BFP), bio_W_50_BFP_mean, bio_W_50$BFP)
#결측치 확인
colSums(is.na(bio_W_50))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_50$CLINIC <- "NA"
bio_W_50$CLINIC <- ifelse(bio_W_50$BMI >= 18.5 & bio_W_50$BMI < 25, "정상", bio_W_50$CLINIC)
bio_W_50$CLINIC <- ifelse(bio_W_50$BMI >= 0 & bio_W_50$BMI < 18.5 |
                          bio_W_50$BMI >= 25 & bio_W_50$BMI < 30, "경계", bio_W_50$CLINIC)
bio_W_50$CLINIC <- ifelse(bio_W_50$BMI >= 30, "이상", bio_W_50$CLINIC)  

##60대_남성
bio_M_60 <- bioage%>%
  filter(SEX == 1 & 60 <= CRAGE & CRAGE < 70) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_60)) #BFP(68)(3.5%) 
#60대_남성_BFP의 평균으로 결측치 대체 
bio_M_60_BFP_mean <- round(mean.default(bio_M_60$BFP, na.rm = T), 0) 
bio_M_60$BFP <- ifelse(is.na(bio_M_60$BFP), bio_M_60_BFP_mean, bio_M_60$BFP)
#결측치 확인
colSums(is.na(bio_M_60))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_60$CLINIC <- "NA"
bio_M_60$CLINIC <- ifelse(bio_M_60$BMI >= 18.5 & bio_M_60$BMI < 25, "정상", bio_M_60$CLINIC)
bio_M_60$CLINIC <- ifelse(bio_M_60$BMI >= 0 & bio_M_60$BMI < 18.5 |
                            bio_M_60$BMI >= 25 & bio_M_60$BMI < 30, "경계", bio_M_60$CLINIC)
bio_M_60$CLINIC <- ifelse(bio_M_60$BMI >= 30, "이상", bio_M_60$CLINIC)

##60대_여성
bio_W_60 <- bioage%>%
  filter(SEX == 2 & 60 <= CRAGE & CRAGE < 70) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_60)) #BFP(28)(1.9%) 
#60대_여성_BFP의 평균으로 결측치 대체 
bio_W_60_BFP_mean <- round(mean.default(bio_W_60$BFP, na.rm = T), 0) 
bio_W_60$BFP <- ifelse(is.na(bio_W_60$BFP), bio_W_60_BFP_mean, bio_W_60$BFP)
#결측치 확인
colSums(is.na(bio_W_60))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_60$CLINIC <- "NA"
bio_W_60$CLINIC <- ifelse(bio_W_60$BMI >= 18.5 & bio_W_60$BMI < 25, "정상", bio_W_60$CLINIC)
bio_W_60$CLINIC <- ifelse(bio_W_60$BMI >= 0 & bio_W_60$BMI < 18.5 |
                            bio_W_60$BMI >= 25 & bio_W_60$BMI < 30, "경계", bio_W_60$CLINIC)
bio_W_60$CLINIC <- ifelse(bio_W_60$BMI >= 30, "이상", bio_W_60$CLINIC)  

##70대_남성
bio_M_70 <- bioage%>%
  filter(SEX == 1 & 70 <= CRAGE & CRAGE < 80) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_70)) #BFP(21)(4.5%) 
#70대_남성_BFP의 평균으로 결측치 대체 
bio_M_70_BFP_mean <- round(mean.default(bio_M_70$BFP, na.rm = T), 0) 
bio_M_70$BFP <- ifelse(is.na(bio_M_70$BFP), bio_M_70_BFP_mean, bio_M_70$BFP)
#결측치 확인
colSums(is.na(bio_M_70))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_70$CLINIC <- "NA"
bio_M_70$CLINIC <- ifelse(bio_M_70$BMI >= 18.5 & bio_M_70$BMI < 25, "정상", bio_M_70$CLINIC)
bio_M_70$CLINIC <- ifelse(bio_M_70$BMI >= 0 & bio_M_70$BMI < 18.5 |
                          bio_M_70$BMI >= 25 & bio_M_70$BMI < 30, "경계", bio_M_70$CLINIC)
bio_M_70$CLINIC <- ifelse(bio_M_70$BMI >= 30, "이상", bio_M_70$CLINIC)  

##70대_여성
bio_W_70 <- bioage%>%
  filter(SEX == 2 & 70 <= CRAGE & CRAGE < 80) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_70)) #BFP(3)(0.8%) 
#70대_여성_BFP의 평균으로 결측치 대체 
bio_W_70_BFP_mean <- round(mean.default(bio_W_70$BFP, na.rm = T), 0) 
bio_W_70$BFP <- ifelse(is.na(bio_W_70$BFP), bio_W_70_BFP_mean, bio_W_70$BFP)
#결측치 확인
colSums(is.na(bio_W_70))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_70$CLINIC <- "NA"
bio_W_70$CLINIC <- ifelse(bio_W_70$BMI >= 18.5 & bio_W_70$BMI < 25, "정상", bio_W_70$CLINIC)
bio_W_70$CLINIC <- ifelse(bio_W_70$BMI >= 0 & bio_W_70$BMI < 18.5 |
                          bio_W_70$BMI >= 25 & bio_W_70$BMI < 30, "경계", bio_W_70$CLINIC)
bio_W_70$CLINIC <- ifelse(bio_W_70$BMI >= 30, "이상", bio_W_70$CLINIC)  

##80대_남성
bio_M_80 <- bioage%>%
  filter(SEX == 1 & 80 <= CRAGE) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_M_80)) #BFP(1)(0.1%) 
#80대_남성_BFP의 평균으로 결측치 대체 
bio_M_80_BFP_mean <- round(mean.default(bio_M_80$BFP, na.rm = T), 0) 
bio_M_80$BFP <- ifelse(is.na(bio_M_80$BFP), bio_M_80_BFP_mean, bio_M_80$BFP)
#결측치 확인
colSums(is.na(bio_M_80))
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_M_80$CLINIC <- "NA"
bio_M_80$CLINIC <- ifelse(bio_M_80$BMI >= 18.5 & bio_M_80$BMI < 25, "정상", bio_M_80$CLINIC)
bio_M_80$CLINIC <- ifelse(bio_M_80$BMI >= 0 & bio_M_80$BMI < 18.5 |
                          bio_M_80$BMI >= 25 & bio_M_80$BMI < 30, "경계", bio_M_80$CLINIC)
bio_M_80$CLINIC <- ifelse(bio_M_80$BMI >= 30, "이상", bio_M_80$CLINIC)   

##80대_여성
bio_W_80 <- bioage%>%
  filter(SEX == 2 & 80 <= CRAGE) %>%
  select(BMI, BAID, BFP, MONOCYTE)
#결측치 확인
colSums(is.na(bio_W_80))  
#BMI에 대한 임상판단 컬럼 생성(정상_18.5,25)(경계_0,18.5|25,30)(이상_30-)
bio_W_80$CLINIC <- "NA"
bio_W_80$CLINIC <- ifelse(bio_W_80$BMI >= 18.5 & bio_W_80$BMI < 25, "정상", bio_W_80$CLINIC)
bio_W_80$CLINIC <- ifelse(bio_W_80$BMI >= 0 & bio_W_80$BMI < 18.5 |
                          bio_W_80$BMI >= 25 & bio_W_80$BMI < 30, "경계", bio_W_80$CLINIC)
bio_W_80$CLINIC <- ifelse(bio_W_80$BMI >= 30, "이상", bio_W_80$CLINIC)     


  #MONOCYYE(단핵구백분율)데이터 편향 조정
#10대_남성 *최종사용 
chart.Correlation(bio_M_10[,c(1,3,4)], histogram = TRUE, pch = 19)

#10대_여성 *최종사용
chart.Correlation(bio_W_10[,c(1,3,4)], histogram = TRUE, pch = 19)

#20대_남성 *최종사용
chart.Correlation(bio_M_20[,c(1,3,4)], histogram = TRUE, pch = 19)

#20대_여성 *최종사용
chart.Correlation(bio_W_20[,c(1,3,4)], histogram = TRUE, pch = 19)

#30대_남성 
chart.Correlation(bio_M_30[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_M_30_log <- transform(bio_M_30[,4], MONOCYTE_log = log(bio_M_30[,4] + 1))
hist(bio_M_30_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_M_30_2 <- cbind(bio_M_30, MONOCYTE_log = bio_M_30_log$MONOCYTE_log)
chart.Correlation(bio_M_30_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환 
bio_M_30_sqrt <- transform(bio_M_30[,4], MONOCYTE_sqrt = sqrt(bio_M_30[,4] + 1))
hist(bio_M_30_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_M_30_2 <- cbind(bio_M_30, MONOCYTE_sqrt = bio_M_30_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_M_30_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#30대_여성 
chart.Correlation(bio_W_30[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_W_30_log <- transform(bio_W_30[,4], MONOCYTE_log = log(bio_W_30[,4] + 1))
hist(bio_W_30_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_W_30_2 <- cbind(bio_W_30, MONOCYTE_log = bio_W_30_log$MONOCYTE_log)
chart.Correlation(bio_W_30_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환 
bio_W_30_sqrt <- transform(bio_W_30[,4], MONOCYTE_sqrt = sqrt(bio_W_30[,4] + 1))
hist(bio_W_30_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_W_30_2 <- cbind(bio_W_30, MONOCYTE_sqrt = bio_W_30_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_W_30_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#40대_남성 
chart.Correlation(bio_M_40[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_M_40_log <- transform(bio_M_40[,4], MONOCYTE_log = log(bio_M_40[,4] + 1))
hist(bio_M_40_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_M_40_2 <- cbind(bio_M_40, MONOCYTE_log = bio_M_40_log$MONOCYTE_log)
chart.Correlation(bio_M_40_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환 
bio_M_40_sqrt <- transform(bio_M_40[,4], MONOCYTE_sqrt = sqrt(bio_M_40[,4] + 1))
hist(bio_M_40_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_M_40_2 <- cbind(bio_M_40, MONOCYTE_sqrt = bio_M_40_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_M_40_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#40대_여성 
chart.Correlation(bio_W_40[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_W_40_log <- transform(bio_W_40[,4], MONOCYTE_log = log(bio_W_40[,4] + 1))
hist(bio_W_40_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_W_40_2 <- cbind(bio_W_40, MONOCYTE_log = bio_W_40_log$MONOCYTE_log)
chart.Correlation(bio_W_40_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환 
bio_W_40_sqrt <- transform(bio_W_40[,4], MONOCYTE_sqrt = sqrt(bio_W_40[,4] + 1))
hist(bio_W_40_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_W_40_2 <- cbind(bio_W_40, MONOCYTE_sqrt = bio_W_40_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_W_40_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#50대_남성 
chart.Correlation(bio_M_50[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_M_50_log <- transform(bio_M_50[,4], MONOCYTE_log = log(bio_M_50[,4] + 1))
hist(bio_M_50_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_M_50_2 <- cbind(bio_M_50, MONOCYTE_log = bio_M_50_log$MONOCYTE_log)
chart.Correlation(bio_M_50_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환 
bio_M_50_sqrt <- transform(bio_M_50[,4], MONOCYTE_sqrt = sqrt(bio_M_50[,4] + 1))
hist(bio_M_50_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_M_50_2 <- cbind(bio_M_50, MONOCYTE_sqrt = bio_M_50_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_M_50_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#50대_여성
chart.Correlation(bio_W_50[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_W_50_log <- transform(bio_W_50[,4], MONOCYTE_log = log(bio_W_50[,4] + 1))
hist(bio_W_50_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_W_50_2 <- cbind(bio_W_50, MONOCYTE_log = bio_W_50_log$MONOCYTE_log)
chart.Correlation(bio_W_50_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환 
bio_W_50_sqrt <- transform(bio_W_50[,4], MONOCYTE_sqrt = sqrt(bio_W_50[,4] + 1))
hist(bio_W_50_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_W_50_2 <- cbind(bio_W_50, MONOCYTE_sqrt = bio_W_50_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_W_50_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#60대_남성 
chart.Correlation(bio_M_60[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_M_60_log <- transform(bio_M_60[,4], MONOCYTE_log = log(bio_M_60[,4] + 1))
hist(bio_M_60_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_M_60_2 <- cbind(bio_M_60, MONOCYTE_log = bio_M_60_log$MONOCYTE_log)
chart.Correlation(bio_M_60_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환
bio_M_60_sqrt <- transform(bio_M_60[,4], MONOCYTE_sqrt = sqrt(bio_M_60[,4] + 1))
hist(bio_M_60_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_M_60_2 <- cbind(bio_M_60, MONOCYTE_sqrt = bio_M_60_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_M_60_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#60대_여성 
chart.Correlation(bio_W_60[,c(1,3,4)], histogram = TRUE, pch = 19)
## 로그 변환 *최종사용
bio_W_60_log <- transform(bio_W_60[,4], MONOCYTE_log = log(bio_W_60[,4] + 1))
hist(bio_W_60_log$MONOCYTE_log, breaks=500, freq=TRUE)
#
bio_W_60_2 <- cbind(bio_W_60, MONOCYTE_log = bio_W_60_log$MONOCYTE_log)
chart.Correlation(bio_W_60_2[,c(1,3,6)], histogram = TRUE, pch = 19)
## 제곱근 변환 
bio_W_60_sqrt <- transform(bio_W_60[,4], MONOCYTE_sqrt = sqrt(bio_W_60[,4] + 1))
hist(bio_W_60_sqrt$MONOCYTE_sqrt, breaks=500, freq=TRUE)
#
bio_W_60_2 <- cbind(bio_W_60, MONOCYTE_sqrt = bio_W_60_sqrt$MONOCYTE_sqrt)
chart.Correlation(bio_W_60_2[,c(1,3,6)], histogram = TRUE, pch = 19)

#70대_남성 *최종사용
chart.Correlation(bio_M_70[,c(1,3,4)], histogram = TRUE, pch = 19)

#70대_여성 *최종사용
chart.Correlation(bio_W_70[,c(1,3,4)], histogram = TRUE, pch = 19)

#80대_남성 *최종사용
chart.Correlation(bio_M_80[,c(1,3,4)], histogram = TRUE, pch = 19)

#80대_여성 *최종사용
chart.Correlation(bio_W_80[,c(1,3,4)], histogram = TRUE, pch = 19)

#####


#####박스 플롯으로 나이와 성별 그리고 BMI 분류 판단 기준 BMI 통계량 확인#####
#10대_남성_클리닉(정상)
bio_M_10_nomal <- bio_M_10 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_10_nomal$BMI)
#10대_남성_클리닉(경계)
bio_M_10_alert <- bio_M_10 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_10_alert$BMI)
#10대_남성_클리닉(이상) *無
bio_M_10_odd <- bio_M_10 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_10_odd$BMI)
##박스 플롯 확인
bio_M_10_melt <- melt(bio_M_10[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_10_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#10대_여성_클리닉(정상)
bio_W_10_nomal <- bio_W_10 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_10_nomal$BMI)
#10대_여성_클리닉(경계)
bio_W_10_alert <- bio_W_10 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_10_alert$BMI)
#10대_여성_클리닉(이상) *無
bio_W_10_odd <- bio_W_10 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_10_odd$BMI)
##박스 플롯 확인
bio_W_10_melt <- melt(bio_W_10[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_10_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#20대_남성_클리닉(정상)
bio_M_20_nomal <- bio_M_20 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_20_nomal$BMI)
#20대_남성_클리닉(경계)
bio_M_20_alert <- bio_M_20 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_20_alert$BMI)
#20대_남성_클리닉(이상) 
bio_M_20_odd <- bio_M_20 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_20_odd$BMI)
##박스 플롯 확인
bio_M_20_melt <- melt(bio_M_20[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_20_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#20대_여성_클리닉(정상)
bio_W_20_nomal <- bio_W_20 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_20_nomal$BMI)
#20대_여성_클리닉(경계)
bio_W_20_alert <- bio_W_20 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_20_alert$BMI)
#20대_여성_클리닉(이상) 
bio_W_20_odd <- bio_W_20 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_20_odd$BMI)
##박스 플롯 확인
bio_W_20_melt <- melt(bio_W_20[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_20_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#30대_남성_클리닉(정상)
bio_M_30_nomal <- bio_M_30 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_30_nomal$BMI)
#30대_남성_클리닉(경계)
bio_M_30_alert <- bio_M_30 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_30_alert$BMI)
#30대_남성_클리닉(이상)
bio_M_30_odd <- bio_M_30 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_30_odd$BMI)
##박스 플롯 확인
bio_M_30_melt <- melt(bio_M_30[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_30_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#30대_여성_클리닉(정상)
bio_W_30_nomal <- bio_W_30 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_30_nomal$BMI)
#30대_여성_클리닉(경계)
bio_W_30_alert <- bio_W_30 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_30_alert$BMI)
#30대_여성_클리닉(이상) 
bio_W_30_odd <- bio_W_30 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_30_odd$BMI)
##박스 플롯 확인
bio_W_30_melt <- melt(bio_W_30[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_30_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#40대_남성_클리닉(정상)
bio_M_40_nomal <- bio_M_40 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_40_nomal$BMI)
#40대_남성_클리닉(경계)
bio_M_40_alert <- bio_M_40 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_40_alert$BMI)
#40대_남성_클리닉(이상)
bio_M_40_odd <- bio_M_40 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_40_odd$BMI)
##박스 플롯 확인
bio_M_40_melt <- melt(bio_M_40[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_40_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#40대_여성_클리닉(정상)
bio_W_40_nomal <- bio_W_40 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_40_nomal$BMI)
#40대_여성_클리닉(경계)
bio_W_40_alert <- bio_W_40 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_40_alert$BMI)
#40대_여성_클리닉(이상) 
bio_W_40_odd <- bio_W_40 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_40_odd$BMI)
##박스 플롯 확인
bio_W_40_melt <- melt(bio_W_40[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_40_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#50대_남성_클리닉(정상)
bio_M_50_nomal <- bio_M_50 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_50_nomal$BMI)
#50대_남성_클리닉(경계)
bio_M_50_alert <- bio_M_50 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_50_alert$BMI)
#50대_남성_클리닉(이상)
bio_M_50_odd <- bio_M_50 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_50_odd$BMI)
##박스 플롯 확인
bio_M_50_melt <- melt(bio_M_50[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_50_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#50대_여성_클리닉(정상)
bio_W_50_nomal <- bio_W_50 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_50_nomal$BMI)
#50대_여성_클리닉(경계)
bio_W_50_alert <- bio_W_50 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_50_alert$BMI)
#50대_여성_클리닉(이상) 
bio_W_50_odd <- bio_W_50 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_50_odd$BMI)
##박스 플롯 확인
bio_W_50_melt <- melt(bio_W_50[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_50_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#60대_남성_클리닉(정상)
bio_M_60_nomal <- bio_M_60 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_60_nomal$BMI)
#60대_남성_클리닉(경계)
bio_M_60_alert <- bio_M_60 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_60_alert$BMI)
#60대_남성_클리닉(이상)
bio_M_60_odd <- bio_M_60 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_60_odd$BMI)
##박스 플롯 확인
bio_M_60_melt <- melt(bio_M_60[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_60_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#60대_여성_클리닉(정상)
bio_W_60_nomal <- bio_W_60 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_60_nomal$BMI)
#60대_여성_클리닉(경계)
bio_W_60_alert <- bio_W_60 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_60_alert$BMI)
#60대_여성_클리닉(이상) 
bio_W_60_odd <- bio_W_60 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_60_odd$BMI)
##박스 플롯 확인
bio_W_60_melt <- melt(bio_W_60[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_60_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#70대_남성_클리닉(정상)
bio_M_70_nomal <- bio_M_70 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_70_nomal$BMI)
#70대_남성_클리닉(경계)
bio_M_70_alert <- bio_M_70 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_70_alert$BMI)
#70대_남성_클리닉(이상)
bio_M_70_odd <- bio_M_70 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_70_odd$BMI)
##박스 플롯 확인
bio_M_70_melt <- melt(bio_M_70[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_70_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#70대_여성_클리닉(정상)
bio_W_70_nomal <- bio_W_70 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_70_nomal$BMI)
#70대_여성_클리닉(경계)
bio_W_70_alert <- bio_W_70 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_70_alert$BMI)
#70대_여성_클리닉(이상) 
bio_W_70_odd <- bio_W_70 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_70_odd$BMI)
##박스 플롯 확인
bio_W_70_melt <- melt(bio_W_70[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_70_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#80대_남성_클리닉(정상)
bio_M_80_nomal <- bio_M_80 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_80_nomal$BMI)
#80대_남성_클리닉(경계)
bio_M_80_alert <- bio_M_80 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_80_alert$BMI)
#80대_남성_클리닉(이상) *無
bio_M_80_odd <- bio_M_80 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_M_80_odd$BMI)
##박스 플롯 확인
bio_M_80_melt <- melt(bio_M_80[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_M_80_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#80대_여성_클리닉(정상)
bio_W_80_nomal <- bio_W_80 %>%
  filter(CLINIC  == "정상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_80_nomal$BMI)
#80대_여성_클리닉(경계)
bio_W_80_alert <- bio_W_80 %>%
  filter(CLINIC  == "경계") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_80_alert$BMI)
#80대_여성_클리닉(이상) 
bio_W_80_odd <- bio_W_80 %>%
  filter(CLINIC  == "이상") %>%
  select(BMI, BAID, BFP, MONOCYTE, CLINIC)
summary(bio_W_80_odd$BMI)
##박스 플롯 확인
bio_W_80_melt <- melt(bio_W_80[,c(1,5)], id.vars = "CLINIC")
ggplot(data = bio_W_80_melt, aes(x = CLINIC, y = value)) + geom_boxplot(fill='slategrey',aes(color=CLINIC),width=0.3) + facet_wrap(~ variable)

#####


#####나이와 성별 기준 BFP(체지방률)와 MONOCYTE(단핵구 백분율)로 BMI 예측#####
##훈련, 테스트 데이터 분리##
set.seed(1234)
  #10대_남성 
bio_M_10_index <- sample(1:nrow(bio_M_10), size = nrow(bio_M_10) * 0.7)
bio_M_10_train <- bio_M_10[bio_M_10_index, ]    # 훈련데이터 (70%)
bio_M_10_test <- bio_M_10[-bio_M_10_index, ]    # 검증데이터 (30%)
nrow(bio_M_10_train); nrow(bio_M_10_test)
  #10대_여성
bio_W_10_index <- sample(1:nrow(bio_W_10), size = nrow(bio_W_10) * 0.7)
bio_W_10_train <- bio_W_10[bio_W_10_index, ]    # 훈련데이터 (70%)
bio_W_10_test <- bio_W_10[-bio_W_10_index, ]    # 검증데이터 (30%)
nrow(bio_W_10_train); nrow(bio_W_10_test)
  #20대_남성
bio_M_20_index <- sample(1:nrow(bio_M_20), size = nrow(bio_M_20) * 0.7)
bio_M_20_train <- bio_M_20[bio_M_20_index, ]    # 훈련데이터 (70%)
bio_M_20_test <- bio_M_20[-bio_M_20_index, ]    # 검증데이터 (30%)
nrow(bio_M_20_train); nrow(bio_M_20_test)
  #20대_여성
bio_W_20_index <- sample(1:nrow(bio_W_20), size = nrow(bio_W_20) * 0.7)
bio_W_20_train <- bio_W_20[bio_W_20_index, ]    # 훈련데이터 (70%)
bio_W_20_test <- bio_W_20[-bio_W_20_index, ]    # 검증데이터 (30%)
nrow(bio_W_20_train); nrow(bio_W_20_test)
  #30대_남성_로그 변환
bio_M_30_2_index <- sample(1:nrow(bio_M_30_2), size = nrow(bio_M_30_2) * 0.7)
bio_M_30_2_train <- bio_M_30_2[bio_M_30_2_index, ]    # 훈련데이터 (70%)
bio_M_30_2_test <- bio_M_30_2[-bio_M_30_2_index, ]    # 검증데이터 (30%)
nrow(bio_M_30_2_train); nrow(bio_M_30_2_test)
  #30대_여성_로그 변환 
bio_W_30_2_index <- sample(1:nrow(bio_W_30_2), size = nrow(bio_W_30_2) * 0.7)
bio_W_30_2_train <- bio_W_30_2[bio_W_30_2_index, ]    # 훈련데이터 (70%)
bio_W_30_2_test <- bio_W_30_2[-bio_W_30_2_index, ]    # 검증데이터 (30%)
nrow(bio_W_30_2_train); nrow(bio_W_30_2_test)
  #40대_남성_로그 변환
bio_M_40_2_index <- sample(1:nrow(bio_M_40_2), size = nrow(bio_M_40_2) * 0.7)
bio_M_40_2_train <- bio_M_40_2[bio_M_40_2_index, ]    # 훈련데이터 (70%)
bio_M_40_2_test <- bio_M_40_2[-bio_M_40_2_index, ]    # 검증데이터 (30%)
nrow(bio_M_40_2_train); nrow(bio_M_40_2_test)
  #40대_여성 로그 변환 
bio_W_40_2_index <- sample(1:nrow(bio_W_40_2), size = nrow(bio_W_40_2) * 0.7)
bio_W_40_2_train <- bio_W_40_2[bio_W_40_2_index, ]    # 훈련데이터 (70%)
bio_W_40_2_test <- bio_W_40_2[-bio_W_40_2_index, ]    # 검증데이터 (30%)
nrow(bio_W_40_2_train); nrow(bio_W_40_2_test)
  #50대_남성_로그 변환
bio_M_50_2_index <- sample(1:nrow(bio_M_50_2), size = nrow(bio_M_50_2) * 0.7)
bio_M_50_2_train <- bio_M_50_2[bio_M_50_2_index, ]    # 훈련데이터 (70%)
bio_M_50_2_test <- bio_M_50_2[-bio_M_50_2_index, ]    # 검증데이터 (30%)
nrow(bio_M_50_2_train); nrow(bio_M_50_2_test)
  #50대_여성 로그 변환 
bio_W_50_2_index <- sample(1:nrow(bio_W_50_2), size = nrow(bio_W_50_2) * 0.7)
bio_W_50_2_train <- bio_W_50_2[bio_W_50_2_index, ]    # 훈련데이터 (70%)
bio_W_50_2_test <- bio_W_50_2[-bio_W_50_2_index, ]    # 검증데이터 (30%)
nrow(bio_W_50_2_train); nrow(bio_W_50_2_test)
  #60대_남성_로그 변환
bio_M_60_2_index <- sample(1:nrow(bio_M_60_2), size = nrow(bio_M_60_2) * 0.7)
bio_M_60_2_train <- bio_M_60_2[bio_M_60_2_index, ]    # 훈련데이터 (70%)
bio_M_60_2_test <- bio_M_60_2[-bio_M_60_2_index, ]    # 검증데이터 (30%)
nrow(bio_M_60_2_train); nrow(bio_M_60_2_test)
  #60대_여성 로그 변환 
bio_W_60_2_index <- sample(1:nrow(bio_W_60_2), size = nrow(bio_W_60_2) * 0.7)
bio_W_60_2_train <- bio_W_60_2[bio_W_60_2_index, ]    # 훈련데이터 (70%)
bio_W_60_2_test <- bio_W_60_2[-bio_W_60_2_index, ]    # 검증데이터 (30%)
nrow(bio_W_60_2_train); nrow(bio_W_60_2_test)
  #70대_남성
bio_M_70_index <- sample(1:nrow(bio_M_70), size = nrow(bio_M_70) * 0.7)
bio_M_70_train <- bio_M_70[bio_M_70_index, ]    # 훈련데이터 (70%)
bio_M_70_test <- bio_M_70[-bio_M_70_index, ]    # 검증데이터 (30%)
nrow(bio_M_70_train); nrow(bio_M_70_test)
  #70대_여성
bio_W_70_index <- sample(1:nrow(bio_W_70), size = nrow(bio_W_70) * 0.7)
bio_W_70_train <- bio_W_70[bio_W_70_index, ]    # 훈련데이터 (70%)
bio_W_70_test <- bio_W_70[-bio_W_70_index, ]    # 검증데이터 (30%)
nrow(bio_W_70_train); nrow(bio_W_70_test)
  #80대_남성
bio_M_80_index <- sample(1:nrow(bio_M_80), size = nrow(bio_M_80) * 0.7)
bio_M_80_train <- bio_M_80[bio_M_80_index, ]    # 훈련데이터 (70%)
bio_M_80_test <- bio_M_80[-bio_M_80_index, ]    # 검증데이터 (30%)
nrow(bio_M_80_train); nrow(bio_M_80_test)
  #80대_여성
bio_W_80_index <- sample(1:nrow(bio_W_80), size = nrow(bio_W_80) * 0.7)
bio_W_80_train <- bio_W_80[bio_W_80_index, ]    # 훈련데이터 (70%)
bio_W_80_test <- bio_W_80[-bio_W_80_index, ]    # 검증데이터 (30%)
nrow(bio_W_80_train); nrow(bio_W_80_test)


  #10대_남성
##회귀 분석##
lm.bio_M_10 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_10_train); lm.bio_M_10
#모델 성능 평가
summary(lm.bio_M_10)
  

#10대_여성
##회귀 분석##
lm.bio_W_10 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_10_train); lm.bio_W_10
#모델 성능 평가
summary(lm.bio_W_10)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_W_10 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_W_10_train); m.bio_W_10 #BFP< 20 _17 #BFP>=20 _19 #MONOCYTE>=5.2 _20 #BMONOCYTE< 5.2 _21 #BFP< 36 _22 #BFP>=36 _24 #MONOCYTE< 3.5 _25 #BFP>=41 _29 
#회귀 트리 시각화
plot(as.party(m.bio_W_10))


#20대_남성
##회귀 분석##
lm.bio_M_20 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_20_train); lm.bio_M_20
#모델 성능 평가
summary(lm.bio_M_20)


#20대_여성 *유효함
##회귀 분석##
lm.bio_W_20 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_20_train); lm.bio_W_20
#모델 성능 평가
summary(lm.bio_W_20)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_W_20 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_W_20_train); m.bio_W_20 #BFP< 20 _17 #BFP>=20 _19 #MONOCYTE>=5.2 _20 #BMONOCYTE< 5.2 _21 #BFP< 36 _22 #BFP>=36 _24 #MONOCYTE< 3.5 _25 #BFP>=41 _29 
#회귀 트리 시각화
plot(as.party(m.bio_W_20))
#모델 성능 평가
p.bio_W_20 <- predict(m.bio_W_20, bio_W_20_test)
summary(p.bio_W_20)
summary(bio_W_20_test$BMI)
##상관 관계
cor(p.bio_W_20,bio_W_20_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_W_20,bio_W_20_test$BMI)
#모델 성능 개선_M5'알고리즘
m.m5p.bio_W_20 <- 
  M5P(BMI ~ BFP + MONOCYTE, data = bio_W_20_train); m.m5p.bio_W_20  
#M5' 알고리즘 시각화
plot(as.party(m.m5p.bio_W_20))
# 통계 확인
summary(m.m5p.bio_W_20)
#모델 성능 평가
p.m5p.bio_W_20 <- predict(m.m5p.bio_W_20, bio_W_20_test)
summary(p.m5p.bio_W_20)
##상관 관계
cor(p.m5p.bio_W_20,bio_W_20_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.m5p.bio_W_20,bio_W_20_test$BMI)


#30대_남성 *유효함 
##회귀 분석##
lm.bio_M_30 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_30_2_train); lm.bio_M_30
#모델 성능 평가
summary(lm.bio_M_30)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_M_30 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_M_30_2_train); m.bio_M_30 #BFP< 14 _20 BFP>=14 _20 #BFP< 22 _24 #BFP< 30 _26 #BFP>=30 _29  
#회귀 트리 시각화
plot(as.party(m.bio_M_30))
#모델 성능 평가
p.bio_M_30 <- predict(m.bio_M_30, bio_M_30_2_test)
summary(p.bio_M_30)
summary(bio_M_30_2_test$BMI)
##상관 관계
cor(p.bio_M_30,bio_M_30_2_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_M_30,bio_M_30_2_test$BMI)
#모델 성능 개선_M5'알고리즘
m.m5p.bio_M_30 <- 
  M5P(BMI ~ BFP + MONOCYTE, data = bio_M_30_2_train); m.m5p.bio_M_30


#30대_여성 *유효함
##회귀 분석##
lm.bio_W_30 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_30_2_train); lm.bio_W_30
#모델 성능 평가
summary(lm.bio_W_30)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_W_30 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_W_30_2_train); m.bio_W_30 #BFP< 23 _19 BFP>=23 _20 #BFP< 29 _21 #BFP>=29 _22 #BFP< 37 _23 #MONOCYTE>=4.6 _25  #MONOCYTE< 4.6 _29  #BFP>=42 _30  
#회귀 트리 시각화
plot(as.party(m.bio_W_30))
#모델 성능 평가
p.bio_W_30 <- predict(m.bio_W_30, bio_W_30_2_test)
summary(p.bio_W_30)
summary(bio_W_30_2_test$BMI)
##상관 관계
cor(p.bio_W_30,bio_W_30_2_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_W_30,bio_W_30_2_test$BMI)


#40대_남성 *유효함 
##회귀 분석##
lm.bio_M_40 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_40_2_train); lm.bio_M_40
#모델 성능 평가
summary(lm.bio_M_40)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_M_40 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_M_40_2_train); m.bio_M_40 #BFP< 18 _21 BFP>=18 _23 #BFP< 26 _25 #BFP< 30 _26 #BFP>=30 _28  
#회귀 트리 시각화
plot(as.party(m.bio_M_40))
#모델 성능 평가
p.bio_M_40 <- predict(m.bio_M_40, bio_M_40_2_test)
summary(p.bio_M_40)
summary(bio_M_40_2_test$BMI)
##상관 관계
cor(p.bio_M_40,bio_M_40_2_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_M_40,bio_M_40_2_test$BMI)


#40대_여성 *유효함
##회귀 분석##
lm.bio_W_40 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_40_2_train); lm.bio_W_40
#모델 성능 평가
summary(lm.bio_W_40)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_W_40 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_W_40_2_train); m.bio_W_40 #BFP< 25 _20 BFP>=25 _21 #BFP< 35 _23 #BFP>=35 _25 #BFP< 42 #BFP< 42 _26 #BFP>=42 _30    
#회귀 트리 시각화
plot(as.party(m.bio_W_40))
#모델 성능 평가
p.bio_W_40 <- predict(m.bio_W_40, bio_W_40_2_test)
summary(p.bio_W_40)
summary(bio_W_40_2_test$BMI)
##상관 관계
cor(p.bio_W_40,bio_W_40_2_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_W_40,bio_W_40_2_test$BMI)


#50대_남성 *유효함 
##회귀 분석##
lm.bio_M_50 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_50_2_train); lm.bio_M_50
#모델 성능 평가
summary(lm.bio_M_50)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_M_50 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_M_50_2_train); m.bio_M_50 #BFP< 18 _21 #BFP< 20 _23 #BFP>=20 _24 #BFP< 28 _25 #BFP>=28 _27  
#회귀 트리 시각화
plot(as.party(m.bio_M_50))
#모델 성능 평가
p.bio_M_50 <- predict(m.bio_M_50, bio_M_50_2_test)
summary(p.bio_M_50)
summary(bio_M_50_2_test$BMI)
##상관 관계
cor(p.bio_M_50,bio_M_50_2_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_M_50,bio_M_50_2_test$BMI)


#50대_여성 *유효함
##회귀 분석##
lm.bio_W_50 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_50_2_train); lm.bio_W_50
#모델 성능 평가
summary(lm.bio_W_50)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_W_50 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_W_50_2_train); m.bio_W_50 #BFP< 23 _19 BFP>=23 _21 #BFP< 36 _24 #BFP>=36 _25 #BFP< 43 _27 #BFP>=43 _30    
#회귀 트리 시각화
plot(as.party(m.bio_W_50))
#모델 성능 평가
p.bio_W_50 <- predict(m.bio_W_50, bio_W_50_2_test)
summary(p.bio_W_50)
summary(bio_W_50_2_test$BMI)
##상관 관계
cor(p.bio_W_50,bio_W_50_2_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_W_50,bio_W_50_2_test$BMI)


#60대_남성 
##회귀 분석##
lm.bio_M_60 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_60_2_train); lm.bio_M_60
#모델 성능 평가
summary(lm.bio_M_60)


#60대_여성 
##회귀 분석##
lm.bio_W_60 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_60_2_train); lm.bio_W_60
#모델 성능 평가
summary(lm.bio_W_60)


#70대_남성 
##회귀 분석##
lm.bio_M_70 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_70_train); lm.bio_M_70
#모델 성능 평가
summary(lm.bio_M_70)


#70대_여성 *유효함
##회귀 분석##
lm.bio_W_70 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_70_train); lm.bio_W_70
#모델 성능 평가
summary(lm.bio_W_70)


#80대_남성
##회귀 분석##
lm.bio_M_80 <- lm(BMI ~ BFP + MONOCYTE, data = bio_M_80_train); lm.bio_M_80
#모델 성능 평가
summary(lm.bio_M_80)


#80대_여성
##회귀 분석##
lm.bio_W_80 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_80_train); lm.bio_W_80
#모델 성능 평가
summary(lm.bio_W_80)

#####


#####MONOCYTE 피처가 유효한 그룹 중 MONOCYTE 피처에서 예측이 이뤄진 노드만 있는 집단 분류#####
#20대_여성 *유효함
##회귀 분석##
lm.bio_W_20 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_20_train); lm.bio_W_20
#모델 성능 평가
summary(lm.bio_W_20)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_W_20 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_W_20_train); m.bio_W_20 #BFP< 20 _17 #BFP>=20 _19 #MONOCYTE>=5.2 _20 #BMONOCYTE< 5.2 _21 #BFP< 36 _22 #BFP>=36 _24 #MONOCYTE< 3.5 _25 #BFP>=41 _29 
#회귀 트리 시각화
plot(as.party(m.bio_W_20))
#모델 성능 평가
p.bio_W_20 <- predict(m.bio_W_20, bio_W_20_test)
summary(p.bio_W_20)
summary(bio_W_20_test$BMI)
##상관 관계
cor(p.bio_W_20,bio_W_20_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_W_20,bio_W_20_test$BMI)


#30대_여성 *유효함
##회귀 분석##
lm.bio_W_30 <- lm(BMI ~ BFP + MONOCYTE, data = bio_W_30_2_train); lm.bio_W_30
#모델 성능 평가
summary(lm.bio_W_30)

##회귀 트리##
par(mfrow=c(1,1)) 
#분류기 구축 
m.bio_W_30 <- 
  rpart(BMI ~ BFP + MONOCYTE, data = bio_W_30_2_train); m.bio_W_30 #BFP< 23 _19 BFP>=23 _20 #BFP< 29 _21 #BFP>=29 _22 #BFP< 37 _23 #MONOCYTE>=4.6 _25  #MONOCYTE< 4.6 _29  #BFP>=42 _30  
#회귀 트리 시각화
plot(as.party(m.bio_W_30))
#모델 성능 평가
p.bio_W_30 <- predict(m.bio_W_30, bio_W_30_2_test)
summary(p.bio_W_30)
summary(bio_W_30_2_test$BMI)
##상관 관계
cor(p.bio_W_30,bio_W_30_2_test$BMI)
##평균 절대 오차
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.bio_W_30,bio_W_30_2_test$BMI)

#####