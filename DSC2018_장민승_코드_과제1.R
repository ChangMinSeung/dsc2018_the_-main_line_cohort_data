#####필요한 library 실행#####
library(readr) #csv 파일 읽기
library(dplyr) #데이터 분리
library(NbClust) #군집 개수 확인
library(arules) #연관성 분석
library(arulesViz) #연관성 분석 시각화
library(igraph)#연관성 분석 시각화

#####

#####데이터 가져오기#####
PDCD <- read.csv(file.choose())

#####

#####EDA#####
#요약 확인
summary(PDCD)
#요약 확인을 통해 의심스러운 변수 확인
PDCD$V1_PHYACTM #결측치 확인 
PDCD$V1_PHYACTH #결측치 확인
PDCD$V2_UPH #결측치 확인
PDCD$V2_USG #결측치 확인
PDCD$V2_UNITR #결측치 확인
#전체 피처에 대한 결측치 확인 및 추적조사 별 모집단 확인
colSums(is.na(PDCD))
  #조절할 수 없는 위험인자 중 결측치를 확인한 변수
    #V1_EDUA(1627), V1_INCOME(188), V1_MARRYA(11), V1_JOBB(32)
  #추적조사 별로 동일한 개수의 결측치 확인 
    #V1(1609), V2(2445), V3(3344), V4(4034), V5(4285), V6(5207)
    #V2-V1(836), V3-V2(899), V4-V3(690), V5-V4(251), V6-V5(922)
    #결측치 증감 수 / +63, -209, -439, +671 
    #결측치 없는 튜플 / V1(7859), V2(7023), V3(6124), V4(5434), V5(5183), V6(4261) 
#결측치가 있는 튜플의 고유번호 중복성 확인(V1_V4_V6)(검사 항목 별 피처 하나씩)(조사 시점 별/조사 시점 간)
  #V1
#이환정보
PDCD[which(!is.na(PDCD$V1_HEALTH)),]
#건강행태정보
PDCD[which(!is.na(PDCD$V1_DRINK)),]
#혈액
PDCD[which(!is.na(PDCD$V1_WBC)),]
#소변
PDCD[which(!is.na(PDCD$V1_UPH)),]
#신체계측
PDCD[which(!is.na(PDCD$V1_SBP)),]
  #V4
#이환정보
PDCD[which(!is.na(PDCD$V4_HEALTH)),]
#건강행태정보
PDCD[which(!is.na(PDCD$V4_DRINK)),]
#혈액
PDCD[which(!is.na(PDCD$V4_WBC)),]
#소변
PDCD[which(!is.na(PDCD$V4_UPH)),]
#신체계측
PDCD[which(!is.na(PDCD$V4_SBP)),]
  #V6
#이환정보
PDCD[which(!is.na(PDCD$V6_HEALTH)),]
#건강행태정보
PDCD[which(!is.na(PDCD$V6_DRINK)),]
#혈액
PDCD[which(!is.na(PDCD$V6_WBC)),]
#소변
PDCD[which(!is.na(PDCD$V6_UPH)),]
#신체계측
PDCD[which(!is.na(PDCD$V6_SBP)),]

#####


#####데이터 전처리#####
#조사 시점 별 검사종류 기준 최소 피처(4개)인 신체계측검사로 
#결측치 없는 튜플 기반 데이터셋 생성
#V1 - 9468개 튜플 중 9468개 사용(100%) = (0개)0%손실##
PDCD_V1 <- PDCD %>%
  filter(!is.na(PDCD$V1_SBP) & !is.na(PDCD$V1_DBP) & !is.na(PDCD$V1_WAIST) &
           !is.na(PDCD$V1_BMI))

colSums(is.na(PDCD_V1))

#평균으로 대체해 데이터 분리
#성별/나이/조사횟수/(조절할 수 있는 위험인자의 검사종류)이환정보, 
#건강행태정보(SMDU제외_SMOKEACA에서 과거와 현재 구분 없기 때문)(PHYACT_L_M_H 비교대상 없어 제외), 
#혈액, 소변, 신체계측(폐기능검사제외_기반조사만 존재해 제외) 별 결측치(INCOME)를 성별/나이 별 

#V1
##남성_40대_이환정보 (2127명)
pdcd_interview_V1_1_M_40 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD,  
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST, V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_M_40)) #V1_INCOME(49)(2.7%)
#남성 40대 'V1_INCOME(188)'의 평균으로 결측치 대체 
pdcd_V1_M_40_IMCOME <- PDCD_V1 %>% filter(V1_SEX == 1 & V1_AGE < 50) %>% select(V1_INCOME)
pdcd_V1_M_40_IMCOME_mean <- round(mean.default(pdcd_V1_M_40_IMCOME$V1_INCOME, na.rm = T), 0) 
pdcd_interview_V1_1_M_40$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_1_M_40$V1_INCOME), pdcd_V1_M_40_IMCOME_mean, pdcd_interview_V1_1_M_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_1_M_40))

##여성_40대_이환정보 (2208명)
pdcd_interview_V1_1_W_40 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & V1_AGE < 50) %>%  
  select(BAID, V1_INCOME, fuPDCD,
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST, V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_W_40)) #V1_INCOME(58)(2.8%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_V1_W_40_IMCOME <- PDCD_V1 %>% filter(V1_SEX == 2 & V1_AGE < 50) %>% select(V1_INCOME)
pdcd_V1_W_40_IMCOME_mean <- round(mean.default(pdcd_V1_W_40_IMCOME$V1_INCOME, na.rm = T))
pdcd_interview_V1_1_W_40$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_1_W_40$V1_INCOME), pdcd_V1_W_40_IMCOME_mean, pdcd_interview_V1_1_W_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_1_W_40))

##남성_50대_이환정보 (1184명)
pdcd_interview_V1_1_M_50 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST, V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_M_50)) #V1_INCOME(26)(2.8%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_V1_M_50_IMCOME <- PDCD_V1 %>% filter(V1_SEX == 1 & 50 <= V1_AGE & V1_AGE < 60) %>% select(V1_INCOME)
pdcd_V1_M_50_IMCOME_mean <- round(mean.default(pdcd_V1_M_50_IMCOME$V1_INCOME, na.rm = T))
pdcd_interview_V1_1_M_50$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_1_M_50$V1_INCOME), pdcd_V1_M_50_IMCOME_mean, pdcd_interview_V1_1_M_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_1_M_50))

##여성_50대_이환정보 (1322명)
pdcd_interview_V1_1_W_50 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST, V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_W_50)) #V1_INCOME(36)(3.3%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_V1_W_50_IMCOME <- PDCD_V1 %>% filter(V1_SEX == 2 & 50 <= V1_AGE & V1_AGE < 60) %>% select(V1_INCOME)
pdcd_V1_W_50_IMCOME_mean <- round(mean.default(pdcd_V1_W_50_IMCOME$V1_INCOME, na.rm = T))
pdcd_interview_V1_1_W_50$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_1_W_50$V1_INCOME), pdcd_V1_W_50_IMCOME_mean, pdcd_interview_V1_1_W_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_1_W_50))

##남성_60대_이환정보 (1055명)
pdcd_interview_V1_1_M_60 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST,  V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_M_60)) #V1_INCOME(10)(1%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_V1_M_60_IMCOME <- PDCD_V1 %>% filter(V1_SEX == 1 & 60 <= V1_AGE & V1_AGE < 70) %>% select(V1_INCOME)
pdcd_V1_M_60_IMCOME_mean <- round(mean.default(pdcd_V1_M_60_IMCOME$V1_INCOME, na.rm = T))
pdcd_interview_V1_1_M_60$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_1_M_60$V1_INCOME), pdcd_V1_M_60_IMCOME_mean, pdcd_interview_V1_1_M_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_1_M_60))

##여성_60대_이환정보 (1441명)
pdcd_interview_V1_1_W_60 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST, V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_W_60)) #V1_INCOME(9)(1%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_V1_W_60_IMCOME <- PDCD_V1 %>% filter(V1_SEX == 2 & 60 <= V1_AGE & V1_AGE < 70) %>% select(V1_INCOME)
pdcd_V1_W_60_IMCOME_mean <- round(mean.default(pdcd_V1_W_60_IMCOME$V1_INCOME, na.rm = T))
pdcd_interview_V1_1_W_60$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_1_W_60$V1_INCOME), pdcd_V1_W_60_IMCOME_mean, pdcd_interview_V1_1_W_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_1_W_60))

##남성_70대_이환정보 (68명)
pdcd_interview_V1_1_M_70 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 70 <= V1_AGE & V1_AGE < 80) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST, V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_M_70)) #V1_INCOME(0)(0%)

##여성_70대_이환정보 (63명)
pdcd_interview_V1_1_W_70 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 70 <= V1_AGE & V1_AGE < 80) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_HEALTH, V1_PDHT_HIST, V1_PDDM_HIST, V1_PDTH_HIST, V1_PDLP_HIST,
         V1_PDAS_HIST, V1_PDKD_HIST, V1_PDHP_HIST, V1_PDTB_HIST, V1_PDCV_HIST,
         V1_PDAR_HIST, V1_FMHTREL3, V1_FMHTREL2, V1_FMHTREL1, V1_FMDMREL3, 
         V1_FMDMREL2, V1_FMDMREL1, V1_FMLPREL3, V1_FMLPREL2, V1_FMLPREL1,
         V1_FMCVAREL3, V1_FMCVAREL2, V1_FMCVAREL1, V1_FMCDREL3, V1_FMCDREL2, 
         V1_FMCDREL1, V1_TRTHT, V1_TRTDM, V1_TRTTH, V1_TRTLP,
         V1_TRTAS, V1_TRTKD, V1_TRTHP, V1_TRTTB, V1_TRTCV,
         V1_TRTAR, V1_DRUGINSCU, V1_DRUGHTCU, V1_DRUGDMCU, V1_DRUGASCU,  
         V1_DRUGLPCU)
#결측치 확인
colSums(is.na(pdcd_interview_V1_1_W_70)) #V1_INCOME(0)(0%)

##남성_40대_건강행태정보 (2127명)
pdcd_interview_V1_2_M_40 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_M_40)) #V1_INCOME(49)(2.3%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_interview_V1_2_M_40$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_2_M_40$V1_INCOME), pdcd_V1_M_40_IMCOME_mean, pdcd_interview_V1_2_M_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_M_40))

##여성_40대_건강행태정보 (2208명)
pdcd_interview_V1_2_W_40 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_W_40)) #V1_INCOME(58)(2.7%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_interview_V1_2_W_40$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_2_W_40$V1_INCOME), pdcd_V1_W_40_IMCOME_mean, pdcd_interview_V1_2_W_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_W_40))

##남성_50대_건강행태정보 (1184명)
pdcd_interview_V1_2_M_50 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_M_50)) #V1_INCOME(26)(2.2%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_interview_V1_2_M_50$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_2_M_50$V1_INCOME), pdcd_V1_M_50_IMCOME_mean, pdcd_interview_V1_2_M_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_M_50))

##여성_50대_건강행태정보 (1322명)
pdcd_interview_V1_2_W_50 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_W_50)) #V1_INCOME(36)(2.7%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_interview_V1_2_W_50$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_2_W_50$V1_INCOME), pdcd_V1_W_50_IMCOME_mean, pdcd_interview_V1_2_W_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_W_50))

##남성_60대_건강행태정보 (1055명)
pdcd_interview_V1_2_M_60 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_M_60)) #V1_INCOME(10)(0.9%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_interview_V1_2_M_60$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_2_M_60$V1_INCOME), pdcd_V1_M_60_IMCOME_mean, pdcd_interview_V1_2_M_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_M_60))

##여성_60대_건강행태정보 (1441명)
pdcd_interview_V1_2_W_60 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_W_60)) #V1_INCOME(9)(0.6%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_interview_V1_2_W_60$V1_INCOME <- ifelse(is.na(pdcd_interview_V1_2_W_60$V1_INCOME), pdcd_V1_W_60_IMCOME_mean, pdcd_interview_V1_2_W_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_W_60))

##남성_70대_건강행태정보 (68명)
pdcd_interview_V1_2_M_70 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 70 <= V1_AGE & V1_AGE < 80) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_M_70)) #V1_INCOME(0)(0%)

##여성_70대_건강행태정보 (63명)
pdcd_interview_V1_2_W_70 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 70 <= V1_AGE & V1_AGE < 80) %>%
  select(BAID, V1_INCOME, fuPDCD, V1_DRINK, V1_PSM, V1_SMOKEACA)
#결측치 확인
colSums(is.na(pdcd_interview_V1_2_W_70)) #V1_INCOME(0)(0%)

##남성_40대_혈액검사 (2127명)
pdcd_blood_V1_M_40 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_M_40)) #V1_INCOME(49)(2.3%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_blood_V1_M_40$V1_INCOME <- ifelse(is.na(pdcd_blood_V1_M_40$V1_INCOME), pdcd_V1_M_40_IMCOME_mean, pdcd_blood_V1_M_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_M_60))

##여성_40대_혈액검사 (2208명)
pdcd_blood_V1_W_40 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_W_40)) #V1_INCOME(58)(2.6%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_blood_V1_W_40$V1_INCOME <- ifelse(is.na(pdcd_blood_V1_W_40$V1_INCOME), pdcd_V1_W_40_IMCOME_mean, pdcd_blood_V1_W_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_interview_V1_2_W_60))

##남성_50대_혈액검사 (1184명)
pdcd_blood_V1_M_50 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_M_50)) #V1_INCOME(26)(2.2%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_blood_V1_M_50$V1_INCOME <- ifelse(is.na(pdcd_blood_V1_M_50$V1_INCOME), pdcd_V1_M_50_IMCOME_mean, pdcd_blood_V1_M_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_blood_V1_M_50))

##여성_50대_혈액검사 (1322명) 
pdcd_blood_V1_W_50 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_W_50)) #V1_INCOME(36)(2.7%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_blood_V1_W_50$V1_INCOME <- ifelse(is.na(pdcd_blood_V1_W_50$V1_INCOME), pdcd_V1_W_50_IMCOME_mean, pdcd_blood_V1_W_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_blood_V1_M_50))

##남성_60대_혈액검사 (1055명)
pdcd_blood_V1_M_60 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_M_60)) #V1_INCOME(10)(1%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_blood_V1_M_60$V1_INCOME <- ifelse(is.na(pdcd_blood_V1_M_60$V1_INCOME), pdcd_V1_M_60_IMCOME_mean, pdcd_blood_V1_M_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_blood_V1_M_60))

##여성_60대_혈액검사 (1441명)
pdcd_blood_V1_W_60 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_W_60)) #V1_INCOME(9)(1%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_blood_V1_W_60$V1_INCOME <- ifelse(is.na(pdcd_blood_V1_W_60$V1_INCOME), pdcd_V1_W_60_IMCOME_mean, pdcd_blood_V1_W_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_blood_V1_W_60))

##남성_70대_혈액검사 (68명)
pdcd_blood_V1_M_70 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 70 <= V1_AGE) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_M_70)) #V1_INCOME(0)(0%)

##여성_70대_혈액검사 (63명)
pdcd_blood_V1_W_70 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 70 <= V1_AGE) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_WBC, V1_RBC, V1_HB, V1_HCT, V1_HBA1C, 
         V1_PLAT, V1_ALT, V1_AST, V1_BUN, V1_CR, 
         V1_GLU, V1_CHOL, V1_HDL, V1_TG, V1_LDL)
#결측치 확인
colSums(is.na(pdcd_blood_V1_W_70)) #V1_INCOME(0)(0%)

##남성_40대_소변검사 (2127명)
pdcd_urine_V1_M_40 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_M_40)) #V1_INCOME(49)(2.3%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_urine_V1_M_40$V1_INCOME <- ifelse(is.na(pdcd_urine_V1_M_40$V1_INCOME), pdcd_V1_M_40_IMCOME_mean, pdcd_urine_V1_M_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_urine_V1_M_40)) 

##여성_40대_소변검사 (2208명)
pdcd_urine_V1_W_40 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_W_40)) #V1_INCOME(58)(2.6%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_urine_V1_W_40$V1_INCOME <- ifelse(is.na(pdcd_urine_V1_W_40$V1_INCOME), pdcd_V1_W_40_IMCOME_mean, pdcd_urine_V1_W_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_urine_V1_W_40)) 

##남성_50대_소변검사 (1184명)
pdcd_urine_V1_M_50 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_M_50)) #V1_INCOME(26)(2.2%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_urine_V1_M_50$V1_INCOME <- ifelse(is.na(pdcd_urine_V1_M_50$V1_INCOME), pdcd_V1_M_50_IMCOME_mean, pdcd_urine_V1_M_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_urine_V1_M_50)) 

##여성_50대_소변검사 (1441명)
pdcd_urine_V1_W_50 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_W_50)) #V1_INCOME(36)(2.5%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_urine_V1_W_50$V1_INCOME <- ifelse(is.na(pdcd_urine_V1_W_50$V1_INCOME), pdcd_V1_W_50_IMCOME_mean, pdcd_urine_V1_W_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_urine_V1_W_50))

##남성_60대_소변검사 (1055명)
pdcd_urine_V1_M_60 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_M_60)) #V1_INCOME(10)(0.9%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_urine_V1_M_60$V1_INCOME <- ifelse(is.na(pdcd_urine_V1_M_60$V1_INCOME), pdcd_V1_M_60_IMCOME_mean, pdcd_urine_V1_M_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_urine_V1_M_60))

##여성_60대_소변검사 (1441명)
pdcd_urine_V1_W_60 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_W_60)) #V1_INCOME(9)(0.6%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_urine_V1_W_60$V1_INCOME <- ifelse(is.na(pdcd_urine_V1_W_60$V1_INCOME), pdcd_V1_W_60_IMCOME_mean, pdcd_urine_V1_W_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_urine_V1_W_60))

##남성_70대_소변검사 (68명)
pdcd_urine_V1_M_70 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 70 <= V1_AGE) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_M_70)) #V1_INCOME(0)(0%)

##여성_70대_소변검사 (63명)
pdcd_urine_V1_W_70 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 70 <= V1_AGE) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_UPH, V1_UNITR, V1_USG, V1_UPRO, V1_UGLU, 
         V1_UKET, V1_UBIL, V1_UBLD, V1_URO)
#결측치 확인
colSums(is.na(pdcd_urine_V1_W_70)) #V1_INCOME(0)(0%)

##남성_40대_신체계측검사 (2127명)
pdcd_body_V1_M_40 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_M_40)) #V1_INCOME(49)(2.3%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_body_V1_M_40$V1_INCOME <- ifelse(is.na(pdcd_body_V1_M_40$V1_INCOME), pdcd_V1_M_40_IMCOME_mean, pdcd_body_V1_M_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_body_V1_M_40))

##여성_40대_신체계측검사 (2208명)
pdcd_body_V1_W_40 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & V1_AGE < 50) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_W_40)) #V1_INCOME(58)(2.6%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_body_V1_W_40$V1_INCOME <- ifelse(is.na(pdcd_body_V1_W_40$V1_INCOME), pdcd_V1_W_40_IMCOME_mean, pdcd_body_V1_W_40$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_body_V1_W_40))

##남성_50대_신체계측검사 (1184명)
pdcd_body_V1_M_50 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_M_50)) #V1_INCOME(26)(2.2%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_body_V1_M_50$V1_INCOME <- ifelse(is.na(pdcd_body_V1_M_50$V1_INCOME), pdcd_V1_M_50_IMCOME_mean, pdcd_body_V1_M_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_body_V1_M_50))

##여성_50대_신체계측검사 (1322명)
pdcd_body_V1_W_50 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 50 <= V1_AGE & V1_AGE < 60) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_W_50)) #V1_INCOME(36)(2.7%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_body_V1_W_50$V1_INCOME <- ifelse(is.na(pdcd_body_V1_W_50$V1_INCOME), pdcd_V1_W_50_IMCOME_mean, pdcd_body_V1_W_50$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_body_V1_W_50))

##남성_60대_신체계측검사 (1055명)
pdcd_body_V1_M_60 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_M_60)) #V1_INCOME(10)(0.9%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_body_V1_M_60$V1_INCOME <- ifelse(is.na(pdcd_body_V1_M_60$V1_INCOME), pdcd_V1_M_60_IMCOME_mean, pdcd_body_V1_M_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_body_V1_M_60))

##여성_60대_신체계측검사 (1441명)
pdcd_body_V1_W_60 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 60 <= V1_AGE & V1_AGE < 70) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_W_60)) #V1_INCOME(9)(0.6%)
#조절할 수 없는 위험인자에서 결측치를 확인한 변수 중 사용할 'V1_INCOME(188)' 평균으로 대체 
pdcd_body_V1_W_60$V1_INCOME <- ifelse(is.na(pdcd_body_V1_W_60$V1_INCOME), pdcd_V1_W_60_IMCOME_mean, pdcd_body_V1_W_60$V1_INCOME)
#결측치 재확인
colSums(is.na(pdcd_body_V1_W_60))

##남성_70대_신체계측검사 (68개)
pdcd_body_V1_M_70 <- PDCD_V1 %>%
  filter(V1_SEX == 1 & 70 <= V1_AGE) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_M_70)) #V1_INCOME(0)(0%)

##남성_70대_신체계측검사 (63개)
pdcd_body_V1_W_70 <- PDCD_V1 %>%
  filter(V1_SEX == 2 & 70 <= V1_AGE) %>%
  select(BAID, V1_INCOME, fuPDCD,
         V1_SBP, V1_DBP, V1_WAIST, V1_BMI)
#결측치 확인
colSums(is.na(pdcd_body_V1_W_70)) #V1_INCOME(0)(0%)

#####


#####군집 타당성 측도로 군집 개수 확인#####

#혈액검사 데이터 구조 확인 - 수치형
str(pdcd_blood_V1_M_40)

#V1_남성_혈액검사
#식별값 제외
pdcd_M_blood_clust<- rbind(pdcd_blood_V1_M_40[,4:18], pdcd_blood_V1_M_50[,4:18],
                            pdcd_blood_V1_M_60[,4:18], pdcd_blood_V1_M_70[,4:18] )
#와드 측정법
pdcd_M_blood_clust_3 <- NbClust(pdcd_M_blood_clust, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2", index = "all" )
pdcd_M_blood_clust_2_dis <- dist(pdcd_M_blood_clust, method = "euclidean")
pdcd_M_blood_clust_2_ward <- hclust(pdcd_M_blood_clust_2_dis, method = "ward.D2")
plot(pdcd_M_blood_clust_2_ward, hang = -1, labels = FALSE)
pdcd_M_blood_clust_2_ward_2 <- cutree(pdcd_M_blood_clust_2_ward, 2)
#K-평균 군집화
pdcd_M_blood_clust_4 <- NbClust(pdcd_M_blood_clust, min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
set.seed(1234)
pdcd_M_blood_clust_Nb_km <- kmeans(pdcd_M_blood_clust, 2, nstart = 25) 
#확인 
par(mfrow =c(1, 2))
boxplot(pdcd_M_blood_clust$V1_WBC ~ pdcd_M_blood_clust_2_ward_2, data = pdcd_M_blood_clust, main = "남성_혈액_Ward's" )
boxplot(pdcd_M_blood_clust$V1_WBC ~ pdcd_M_blood_clust_Nb_km$cluster, data = pdcd_M_blood_clust, main = "남성_혈액_K-Means")

#V1_여성_혈액검사
pdcd_W_blood_clust<- rbind(pdcd_blood_V1_W_40[,4:18], pdcd_blood_V1_W_50[,4:18],
                           pdcd_blood_V1_W_60[,4:18], pdcd_blood_V1_W_70[,4:18] )
#와드 측정법
pdcd_W_blood_clust_3 <- NbClust(pdcd_W_blood_clust, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2", index = "all" )
pdcd_W_blood_clust_2_dis <- dist(pdcd_W_blood_clust, method = "euclidean")
pdcd_W_blood_clust_2_ward <- hclust(pdcd_W_blood_clust_2_dis, method = "ward.D2")
plot(pdcd_W_blood_clust_2_ward, hang = -1, labels = FALSE)
pdcd_W_blood_clust_2_ward_2 <- cutree(pdcd_W_blood_clust_2_ward, 2)
#K-평균 군집화
pdcd_W_blood_clust_4 <- NbClust(pdcd_W_blood_clust, min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
set.seed(1234)
pdcd_W_blood_clust_Nb_km <- kmeans(pdcd_W_blood_clust, 2, nstart = 25) 
#확인 
par(mfrow =c(1, 2))
boxplot(pdcd_W_blood_clust$V1_WBC ~ pdcd_W_blood_clust_2_ward_2, data = pdcd_W_blood_clust, main = "여성_혈액_Ward's" )
boxplot(pdcd_W_blood_clust$V1_WBC ~ pdcd_W_blood_clust_Nb_km$cluster, data = pdcd_W_blood_clust, main = "여성_혈액_K-Means")

#소변검사 데이터 구조 확인 - 수치형
str(pdcd_urine_V1_M_40)

#V1_남성_소변검사
#식별값 제외
pdcd_M_urine_clust <- rbind(pdcd_urine_V1_M_40[,4], pdcd_urine_V1_M_50[,4], 
                            pdcd_urine_V1_M_60[,4], pdcd_urine_V1_M_70[,4])
#와드 측정법
pdcd_M_urine_clust_3 <- NbClust(pdcd_M_urine_clust, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2", index = "all" )
pdcd_M_urine_clust_2_dis <- dist(pdcd_M_urine_clust, method = "euclidean")
pdcd_M_urine_clust_2_ward <- hclust(pdcd_M_urine_clust_2_dis, method = "ward.D2")
plot(pdcd_M_urine_clust_2_ward, hang = -1, labels = FALSE)
pdcd_M_urine_clust_2_ward_2 <- cutree(pdcd_M_urine_clust_2_ward, 5)
#K-평균 군집화
pdcd_M_urine_clust_4 <- NbClust(pdcd_M_urine_clust, min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
set.seed(1234)
pdcd_M_urine_clust_Nb_km <- kmeans(pdcd_M_urine_clust, 5, nstart = 25) 
#확인 
par(mfrow =c(1, 2))
boxplot(pdcd_M_urine_clust$V1_UPH ~ pdcd_M_urine_clust_2_ward_2, data = pdcd_M_urine_clust, main = "남성_소변_Ward's(6개)" )
boxplot(pdcd_M_urine_clust$V1_UPH ~ pdcd_M_urine_clust_Nb_km$cluster, data = pdcd_M_urine_clust, main = "남성_소변_K-Means(6개)")

#V1_여성_소변검사
pdcd_W_urine_clust <- rbind(pdcd_urine_V1_W_40[,4], pdcd_urine_V1_W_50[,4], 
                            pdcd_urine_V1_W_60[,4], pdcd_urine_V1_W_70[,4])
#와드 측정법
pdcd_W_urine_clust_3 <- NbClust(pdcd_W_urine_clust, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2", index = "all" )
pdcd_W_urine_clust_2_dis <- dist(pdcd_W_urine_clust, method = "euclidean")
pdcd_W_urine_clust_2_ward <- hclust(pdcd_W_urine_clust_2_dis, method = "ward.D2")
plot(pdcd_W_urine_clust_2_ward, hang = -1, labels = FALSE)
pdcd_W_urine_clust_2_ward_2 <- cutree(pdcd_W_urine_clust_2_ward, 2)
#K-평균 군집화
pdcd_W_urine_clust_4 <- NbClust(pdcd_W_urine_clust, min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
set.seed(1234)
pdcd_W_urine_clust_Nb_km <- kmeans(pdcd_W_urine_clust, 2, nstart = 25) 
#확인 
par(mfrow =c(1, 2))
boxplot(pdcd_W_urine_clust$V1_UPH ~ pdcd_W_urine_clust_2_ward_2, data = pdcd_W_urine_clust, main = "여성_소변_Ward's(2개)" )
boxplot(pdcd_W_urine_clust$V1_UPH ~ pdcd_W_urine_clust_Nb_km$cluster, data = pdcd_W_urine_clust, main = "여성_소변_K-Means(2개)")

#소변검사 데이터 구조 확인 - 수치형
str(pdcd_body_V1_M_40)

#V1_남성_신체계측
#식별값 제외
pdcd_M_body_clust <- rbind(pdcd_body_V1_M_40[,4:7], pdcd_body_V1_M_50[,4:7], 
                            pdcd_body_V1_M_60[,4:7], pdcd_body_V1_M_70[,4:7])
#와드 측정법
pdcd_M_body_clust_3 <- NbClust(pdcd_M_body_clust, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2", index = "all" )
pdcd_M_body_clust_2_dis <- dist(pdcd_M_body_clust, method = "euclidean")
pdcd_M_body_clust_2_ward <- hclust(pdcd_M_body_clust_2_dis, method = "ward.D2")
plot(pdcd_M_body_clust_2_ward, hang = -1, labels = FALSE)
pdcd_M_body_clust_2_ward_2 <- cutree(pdcd_M_body_clust_2_ward, 2)
#K-평균 군집화
pdcd_M_body_clust_4 <- NbClust(pdcd_M_body_clust, min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
set.seed(1234)
pdcd_M_body_clust_Nb_km <- kmeans(pdcd_M_body_clust, 2, nstart = 25) 
#확인 
par(mfrow =c(1, 2))
boxplot(pdcd_M_body_clust$V1_SBP ~ pdcd_M_body_clust_2_ward_2, data = pdcd_M_body_clust, main = "남성_신체_Ward's(2개)" )
boxplot(pdcd_M_body_clust$V1_SBP ~ pdcd_M_body_clust_Nb_km$cluster, data = pdcd_M_body_clust, main = "남성_신체_K-Means(2개)")

#V1_여성_소변계측
#식별값 제외
pdcd_W_body_clust <- rbind(pdcd_body_V1_W_40[,4:7], pdcd_body_V1_W_50[,4:7], 
                           pdcd_body_V1_W_60[,4:7], pdcd_body_V1_W_70[,4:7])
#와드 측정법
pdcd_W_body_clust_3 <- NbClust(pdcd_W_body_clust, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2", index = "all" )
pdcd_W_body_clust_2_dis <- dist(pdcd_W_body_clust, method = "euclidean")
pdcd_W_body_clust_2_ward <- hclust(pdcd_W_body_clust_2_dis, method = "ward.D2")
plot(pdcd_W_body_clust_2_ward, hang = -1, labels = FALSE)
pdcd_W_body_clust_2_ward_2 <- cutree(pdcd_W_body_clust_2_ward, 2)
#K-평균 군집화
pdcd_W_body_clust_4 <- NbClust(pdcd_W_body_clust, min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
set.seed(1234)
pdcd_W_body_clust_Nb_km <- kmeans(pdcd_W_body_clust, 2, nstart = 25) 
#확인 
par(mfrow =c(1, 2))
boxplot(pdcd_W_body_clust$V1_SBP ~ pdcd_W_body_clust_2_ward_2, data = pdcd_W_body_clust, main = "여성_신체_Ward's(2개)" )
boxplot(pdcd_W_body_clust$V1_SBP ~ pdcd_W_body_clust_Nb_km$cluster, data = pdcd_W_body_clust, main = "여성_신체_K-Means(2개)")

#####


#####혈액검사/소변검사/신체계측검사 수치형 자료 -> 범주형 자료 변환#####
  #V1_남성_40대_혈액검사(2개 군집)
max(pdcd_blood_V1_M_40$V1_WBC)/4
pdcd_blood_V1_M_40$V1_WBC_DEX <- "NA"
pdcd_blood_V1_M_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_WBC <= 4, "2", pdcd_blood_V1_M_40$V1_WBC_DEX)
pdcd_blood_V1_M_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_WBC > 4 & pdcd_blood_V1_M_40$V1_WBC <= 8, "1", pdcd_blood_V1_M_40$V1_WBC_DEX)
pdcd_blood_V1_M_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_WBC > 8 & pdcd_blood_V1_M_40$V1_WBC <= 16, "1", pdcd_blood_V1_M_40$V1_WBC_DEX)
pdcd_blood_V1_M_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_WBC > 16,  "2", pdcd_blood_V1_M_40$V1_WBC_DEX)

max(pdcd_blood_V1_M_40$V1_RBC)/4
pdcd_blood_V1_M_40$V1_RBC_DEX <- "NA"
pdcd_blood_V1_M_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_RBC <= 1.6, "2", pdcd_blood_V1_M_40$V1_RBC_DEX)
pdcd_blood_V1_M_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_RBC > 1.6 & pdcd_blood_V1_M_40$V1_RBC <= 3.2, "1", pdcd_blood_V1_M_40$V1_RBC_DEX)
pdcd_blood_V1_M_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_RBC > 3.2 & pdcd_blood_V1_M_40$V1_RBC <= 4.8, "1", pdcd_blood_V1_M_40$V1_RBC_DEX)
pdcd_blood_V1_M_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_40$V1_RBC > 4.8,  "2", pdcd_blood_V1_M_40$V1_RBC_DEX)

max(pdcd_blood_V1_M_40$V1_HB)/4
pdcd_blood_V1_M_40$V1_HB_DEX <- "NA"
pdcd_blood_V1_M_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HB <= 4.8, "2", pdcd_blood_V1_M_40$V1_HB_DEX)
pdcd_blood_V1_M_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HB > 4.8 & pdcd_blood_V1_M_40$V1_HB <= 9.6, "1", pdcd_blood_V1_M_40$V1_HB_DEX)
pdcd_blood_V1_M_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HB > 9.6 & pdcd_blood_V1_M_40$V1_HB <= 14.4, "1", pdcd_blood_V1_M_40$V1_HB_DEX)
pdcd_blood_V1_M_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HB > 14.4,  "2", pdcd_blood_V1_M_40$V1_HB_DEX)

max(pdcd_blood_V1_M_40$V1_HCT)/4
pdcd_blood_V1_M_40$V1_HCT_DEX <- "NA"
pdcd_blood_V1_M_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HCT <= 14, "2", pdcd_blood_V1_M_40$V1_HCT_DEX)
pdcd_blood_V1_M_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HCT > 14 & pdcd_blood_V1_M_40$V1_HCT <= 28, "1", pdcd_blood_V1_M_40$V1_HCT_DEX)
pdcd_blood_V1_M_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HCT > 28 & pdcd_blood_V1_M_40$V1_HCT <= 42, "1", pdcd_blood_V1_M_40$V1_HCT_DEX)
pdcd_blood_V1_M_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HCT > 42,  "2", pdcd_blood_V1_M_40$V1_HCT_DEX)

max(pdcd_blood_V1_M_40$V1_HBA1C)/4
pdcd_blood_V1_M_40$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_M_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HBA1C <= 3.7, "2", pdcd_blood_V1_M_40$V1_HBA1C_DEX)
pdcd_blood_V1_M_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HBA1C > 3.7 & pdcd_blood_V1_M_40$V1_HBA1C <= 7.4, "1", pdcd_blood_V1_M_40$V1_HBA1C_DEX)
pdcd_blood_V1_M_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HBA1C > 7.4 & pdcd_blood_V1_M_40$V1_HBA1C <= 11.1, "1", pdcd_blood_V1_M_40$V1_HBA1C_DEX)
pdcd_blood_V1_M_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HBA1C > 11.1,  "2", pdcd_blood_V1_M_40$V1_HBA1C_DEX)

max(pdcd_blood_V1_M_40$V1_PLAT)/4
pdcd_blood_V1_M_40$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_M_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_PLAT <= 139, "2", pdcd_blood_V1_M_40$V1_PLAT_DEX)
pdcd_blood_V1_M_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_PLAT > 139 & pdcd_blood_V1_M_40$V1_PLAT <= 278, "1", pdcd_blood_V1_M_40$V1_PLAT_DEX)
pdcd_blood_V1_M_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_PLAT > 278 & pdcd_blood_V1_M_40$V1_PLAT <= 417, "1", pdcd_blood_V1_M_40$V1_PLAT_DEX)
pdcd_blood_V1_M_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_PLAT > 417,  "2", pdcd_blood_V1_M_40$V1_PLAT_DEX)

max(pdcd_blood_V1_M_40$V1_ALT)/4
pdcd_blood_V1_M_40$V1_ALT_DEX <- "NA"
pdcd_blood_V1_M_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_ALT <= 40, "2", pdcd_blood_V1_M_40$V1_ALT_DEX)
pdcd_blood_V1_M_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_ALT > 40 & pdcd_blood_V1_M_40$V1_ALT <= 80, "1", pdcd_blood_V1_M_40$V1_ALT_DEX)
pdcd_blood_V1_M_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_ALT > 80 & pdcd_blood_V1_M_40$V1_ALT <= 120, "1", pdcd_blood_V1_M_40$V1_ALT_DEX)
pdcd_blood_V1_M_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_40$V1_ALT > 160,  "2", pdcd_blood_V1_M_40$V1_ALT_DEX)

max(pdcd_blood_V1_M_40$V1_AST)/4
pdcd_blood_V1_M_40$V1_AST_DEX <- "NA"
pdcd_blood_V1_M_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_40$V1_AST <= 45, "2", pdcd_blood_V1_M_40$V1_AST_DEX)
pdcd_blood_V1_M_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_40$V1_AST > 45 & pdcd_blood_V1_M_40$V1_AST <= 90, "1", pdcd_blood_V1_M_40$V1_AST_DEX)
pdcd_blood_V1_M_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_40$V1_AST > 90 & pdcd_blood_V1_M_40$V1_AST <= 135, "1", pdcd_blood_V1_M_40$V1_AST_DEX)
pdcd_blood_V1_M_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_40$V1_AST > 135,  "2", pdcd_blood_V1_M_40$V1_AST_DEX)

max(pdcd_blood_V1_M_40$V1_BUN)/4
pdcd_blood_V1_M_40$V1_BUN_DEX <- "NA"
pdcd_blood_V1_M_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_40$V1_BUN <= 16, "2", pdcd_blood_V1_M_40$V1_BUN_DEX)
pdcd_blood_V1_M_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_40$V1_BUN > 16 & pdcd_blood_V1_M_40$V1_BUN <= 32, "1", pdcd_blood_V1_M_40$V1_BUN_DEX)
pdcd_blood_V1_M_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_40$V1_BUN > 32 & pdcd_blood_V1_M_40$V1_BUN <= 48, "1", pdcd_blood_V1_M_40$V1_BUN_DEX)
pdcd_blood_V1_M_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_40$V1_BUN > 48,  "2", pdcd_blood_V1_M_40$V1_BUN_DEX)

max(pdcd_blood_V1_M_40$V1_CR)/4
pdcd_blood_V1_M_40$V1_CR_DEX <- "NA"
pdcd_blood_V1_M_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CR <= 1.5, "2", pdcd_blood_V1_M_40$V1_CR_DEX)
pdcd_blood_V1_M_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CR > 1.5 & pdcd_blood_V1_M_40$V1_CR <= 3, "1", pdcd_blood_V1_M_40$V1_CR_DEX)
pdcd_blood_V1_M_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CR > 3 & pdcd_blood_V1_M_40$V1_CR <= 4.5, "1", pdcd_blood_V1_M_40$V1_CR_DEX)
pdcd_blood_V1_M_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CR > 4.5,  "2", pdcd_blood_V1_M_40$V1_CR_DEX)

max(pdcd_blood_V1_M_40$V1_GLU)/4
pdcd_blood_V1_M_40$V1_GLU_DEX <- "NA"
pdcd_blood_V1_M_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_40$V1_GLU <= 81, "2", pdcd_blood_V1_M_40$V1_GLU_DEX)
pdcd_blood_V1_M_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_40$V1_GLU > 81 & pdcd_blood_V1_M_40$V1_GLU <= 162, "1", pdcd_blood_V1_M_40$V1_GLU_DEX)
pdcd_blood_V1_M_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_40$V1_GLU > 162 & pdcd_blood_V1_M_40$V1_GLU <= 243, "1", pdcd_blood_V1_M_40$V1_GLU_DEX)
pdcd_blood_V1_M_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_40$V1_GLU > 243,  "2", pdcd_blood_V1_M_40$V1_GLU_DEX)

max(pdcd_blood_V1_M_40$V1_CHOL)/4
pdcd_blood_V1_M_40$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_M_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CHOL <= 105, "2", pdcd_blood_V1_M_40$V1_CHOL_DEX)
pdcd_blood_V1_M_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CHOL > 105 & pdcd_blood_V1_M_40$V1_CHOL <= 210, "1", pdcd_blood_V1_M_40$V1_CHOL_DEX)
pdcd_blood_V1_M_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CHOL > 210 & pdcd_blood_V1_M_40$V1_CHOL <= 315, "1", pdcd_blood_V1_M_40$V1_CHOL_DEX)
pdcd_blood_V1_M_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_CHOL > 315,  "2", pdcd_blood_V1_M_40$V1_CHOL_DEX)

max(pdcd_blood_V1_M_40$V1_HDL)/4
pdcd_blood_V1_M_40$V1_HDL_DEX <- "NA"
pdcd_blood_V1_M_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HDL <= 22, "2", pdcd_blood_V1_M_40$V1_HDL_DEX)
pdcd_blood_V1_M_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HDL > 22 & pdcd_blood_V1_M_40$V1_HDL <= 44, "1", pdcd_blood_V1_M_40$V1_HDL_DEX)
pdcd_blood_V1_M_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HDL > 44 & pdcd_blood_V1_M_40$V1_HDL <= 66, "1", pdcd_blood_V1_M_40$V1_HDL_DEX)
pdcd_blood_V1_M_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_HDL > 66,  "2", pdcd_blood_V1_M_40$V1_HDL_DEX)

max(pdcd_blood_V1_M_40$V1_TG)/4
pdcd_blood_V1_M_40$V1_TG_DEX <- "NA"
pdcd_blood_V1_M_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_40$V1_TG <= 668, "2", pdcd_blood_V1_M_40$V1_TG_DEX)
pdcd_blood_V1_M_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_40$V1_TG > 668 & pdcd_blood_V1_M_40$V1_TG <= 1336, "1", pdcd_blood_V1_M_40$V1_TG_DEX)
pdcd_blood_V1_M_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_40$V1_TG > 1336 & pdcd_blood_V1_M_40$V1_TG <= 2004, "1", pdcd_blood_V1_M_40$V1_TG_DEX)
pdcd_blood_V1_M_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_40$V1_TG > 2004,  "2", pdcd_blood_V1_M_40$V1_TG_DEX)

max(pdcd_blood_V1_M_40$V1_LDL)/4
pdcd_blood_V1_M_40$V1_LDL_DEX <- "NA"
pdcd_blood_V1_M_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_LDL <= 63, "2", pdcd_blood_V1_M_40$V1_LDL_DEX)
pdcd_blood_V1_M_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_LDL > 63 & pdcd_blood_V1_M_40$V1_LDL <= 126, "1", pdcd_blood_V1_M_40$V1_LDL_DEX)
pdcd_blood_V1_M_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_LDL > 126 & pdcd_blood_V1_M_40$V1_LDL <= 189, "1", pdcd_blood_V1_M_40$V1_LDL_DEX)
pdcd_blood_V1_M_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_40$V1_LDL > 189,  "2", pdcd_blood_V1_M_40$V1_LDL_DEX)

  #V1_여성_40대_혈액검사(2개 군집) 
max(pdcd_blood_V1_W_40$V1_WBC)/4
pdcd_blood_V1_W_40$V1_WBC_DEX <- "NA"
pdcd_blood_V1_W_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_WBC <= 3.4, "2", pdcd_blood_V1_W_40$V1_WBC_DEX)
pdcd_blood_V1_W_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_WBC > 3.4 & pdcd_blood_V1_W_40$V1_WBC <= 6.8, "1", pdcd_blood_V1_W_40$V1_WBC_DEX)
pdcd_blood_V1_W_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_WBC > 6.8 & pdcd_blood_V1_W_40$V1_WBC <= 13.6, "1", pdcd_blood_V1_W_40$V1_WBC_DEX)
pdcd_blood_V1_W_40$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_WBC > 13.6,  "2", pdcd_blood_V1_W_40$V1_WBC_DEX)

max(pdcd_blood_V1_W_40$V1_RBC)/4
pdcd_blood_V1_W_40$V1_RBC_DEX <- "NA"
pdcd_blood_V1_W_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_RBC <= 1.5, "2", pdcd_blood_V1_W_40$V1_RBC_DEX)
pdcd_blood_V1_W_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_RBC > 1.5 & pdcd_blood_V1_W_40$V1_RBC <= 3, "1", pdcd_blood_V1_W_40$V1_RBC_DEX)
pdcd_blood_V1_W_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_RBC > 3 & pdcd_blood_V1_W_40$V1_RBC <= 4.5, "1", pdcd_blood_V1_W_40$V1_RBC_DEX)
pdcd_blood_V1_W_40$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_40$V1_RBC > 4.5,  "2", pdcd_blood_V1_W_40$V1_RBC_DEX)

max(pdcd_blood_V1_W_40$V1_HB)/4
pdcd_blood_V1_W_40$V1_HB_DEX <- "NA"
pdcd_blood_V1_W_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HB <= 4, "2", pdcd_blood_V1_W_40$V1_HB_DEX)
pdcd_blood_V1_W_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HB > 4 & pdcd_blood_V1_W_40$V1_HB <= 8, "1", pdcd_blood_V1_W_40$V1_HB_DEX)
pdcd_blood_V1_W_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HB > 12 & pdcd_blood_V1_W_40$V1_HB <= 16, "1", pdcd_blood_V1_W_40$V1_HB_DEX)
pdcd_blood_V1_W_40$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HB > 16,  "2", pdcd_blood_V1_W_40$V1_HB_DEX)

max(pdcd_blood_V1_W_40$V1_HCT)/4
pdcd_blood_V1_W_40$V1_HCT_DEX <- "NA"
pdcd_blood_V1_W_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HCT <= 12, "2", pdcd_blood_V1_W_40$V1_HCT_DEX)
pdcd_blood_V1_W_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HCT > 12 & pdcd_blood_V1_W_40$V1_HCT <= 24, "1", pdcd_blood_V1_W_40$V1_HCT_DEX)
pdcd_blood_V1_W_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HCT > 24 & pdcd_blood_V1_W_40$V1_HCT <= 36, "1", pdcd_blood_V1_W_40$V1_HCT_DEX)
pdcd_blood_V1_W_40$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HCT > 36,  "2", pdcd_blood_V1_W_40$V1_HCT_DEX)

max(pdcd_blood_V1_W_40$V1_HBA1C)/4
pdcd_blood_V1_W_40$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_W_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HBA1C <= 3.2, "2", pdcd_blood_V1_W_40$V1_HBA1C_DEX)
pdcd_blood_V1_W_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HBA1C > 3.2 & pdcd_blood_V1_W_40$V1_HBA1C <= 6.4, "1", pdcd_blood_V1_W_40$V1_HBA1C_DEX)
pdcd_blood_V1_W_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HBA1C > 6.4 & pdcd_blood_V1_W_40$V1_HBA1C <= 9.6, "1", pdcd_blood_V1_W_40$V1_HBA1C_DEX)
pdcd_blood_V1_W_40$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HBA1C > 9.6,  "2", pdcd_blood_V1_W_40$V1_HBA1C_DEX)

max(pdcd_blood_V1_W_40$V1_PLAT)/4
pdcd_blood_V1_W_40$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_W_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_PLAT <= 134, "2", pdcd_blood_V1_W_40$V1_PLAT_DEX)
pdcd_blood_V1_W_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_PLAT > 134 & pdcd_blood_V1_W_40$V1_PLAT <= 268, "1", pdcd_blood_V1_W_40$V1_PLAT_DEX)
pdcd_blood_V1_W_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_PLAT > 268 & pdcd_blood_V1_W_40$V1_PLAT <= 402, "1", pdcd_blood_V1_W_40$V1_PLAT_DEX)
pdcd_blood_V1_W_40$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_PLAT > 402,  "2", pdcd_blood_V1_W_40$V1_PLAT_DEX)

max(pdcd_blood_V1_W_40$V1_ALT)/4
pdcd_blood_V1_W_40$V1_ALT_DEX <- "NA"
pdcd_blood_V1_W_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_ALT <= 37, "2", pdcd_blood_V1_W_40$V1_ALT_DEX)
pdcd_blood_V1_W_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_ALT > 74 & pdcd_blood_V1_W_40$V1_ALT <= 74, "1", pdcd_blood_V1_W_40$V1_ALT_DEX)
pdcd_blood_V1_W_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_ALT > 74 & pdcd_blood_V1_W_40$V1_ALT <= 111, "1", pdcd_blood_V1_W_40$V1_ALT_DEX)
pdcd_blood_V1_W_40$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_40$V1_ALT > 111,  "2", pdcd_blood_V1_W_40$V1_ALT_DEX)

max(pdcd_blood_V1_W_40$V1_AST)/4
pdcd_blood_V1_W_40$V1_AST_DEX <- "NA"
pdcd_blood_V1_W_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_40$V1_AST <= 28, "2", pdcd_blood_V1_W_40$V1_AST_DEX)
pdcd_blood_V1_W_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_40$V1_AST > 28 & pdcd_blood_V1_W_40$V1_AST <= 56, "1", pdcd_blood_V1_W_40$V1_AST_DEX)
pdcd_blood_V1_W_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_40$V1_AST > 56 & pdcd_blood_V1_W_40$V1_AST <= 84, "1", pdcd_blood_V1_W_40$V1_AST_DEX)
pdcd_blood_V1_W_40$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_40$V1_AST > 84,  "2", pdcd_blood_V1_W_40$V1_AST_DEX)

max(pdcd_blood_V1_W_40$V1_BUN)/4
pdcd_blood_V1_W_40$V1_BUN_DEX <- "NA"
pdcd_blood_V1_W_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_40$V1_BUN <= 8.5, "2", pdcd_blood_V1_W_40$V1_BUN_DEX)
pdcd_blood_V1_W_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_40$V1_BUN > 8.5 & pdcd_blood_V1_W_40$V1_BUN <= 17, "1", pdcd_blood_V1_W_40$V1_BUN_DEX)
pdcd_blood_V1_W_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_40$V1_BUN > 17 & pdcd_blood_V1_W_40$V1_BUN <= 25.5, "1", pdcd_blood_V1_W_40$V1_BUN_DEX)
pdcd_blood_V1_W_40$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_40$V1_BUN > 25.5,  "2", pdcd_blood_V1_W_40$V1_BUN_DEX)

max(pdcd_blood_V1_W_40$V1_CR)/4
pdcd_blood_V1_W_40$V1_CR_DEX <- "NA"
pdcd_blood_V1_W_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CR <= 0.66, "2", pdcd_blood_V1_W_40$V1_CR_DEX)
pdcd_blood_V1_W_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CR > 0.66 & pdcd_blood_V1_W_40$V1_CR <= 1.32, "1", pdcd_blood_V1_W_40$V1_CR_DEX)
pdcd_blood_V1_W_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CR > 1.32 & pdcd_blood_V1_W_40$V1_CR <= 1.98, "1", pdcd_blood_V1_W_40$V1_CR_DEX)
pdcd_blood_V1_W_40$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CR > 1.98,  "2", pdcd_blood_V1_W_40$V1_CR_DEX)

max(pdcd_blood_V1_W_40$V1_GLU)/4
pdcd_blood_V1_W_40$V1_GLU_DEX <- "NA"
pdcd_blood_V1_W_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_40$V1_GLU <= 74, "2", pdcd_blood_V1_W_40$V1_GLU_DEX)
pdcd_blood_V1_W_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_40$V1_GLU > 74 & pdcd_blood_V1_W_40$V1_GLU <= 148, "1", pdcd_blood_V1_W_40$V1_GLU_DEX)
pdcd_blood_V1_W_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_40$V1_GLU > 148 & pdcd_blood_V1_W_40$V1_GLU <= 222, "1", pdcd_blood_V1_W_40$V1_GLU_DEX)
pdcd_blood_V1_W_40$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_40$V1_GLU > 222,  "2", pdcd_blood_V1_W_40$V1_GLU_DEX)

max(pdcd_blood_V1_W_40$V1_CHOL)/4
pdcd_blood_V1_W_40$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_W_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CHOL <= 87, "2", pdcd_blood_V1_W_40$V1_CHOL_DEX)
pdcd_blood_V1_W_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CHOL > 87 & pdcd_blood_V1_W_40$V1_CHOL <= 174, "1", pdcd_blood_V1_W_40$V1_CHOL_DEX)
pdcd_blood_V1_W_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CHOL > 174 & pdcd_blood_V1_W_40$V1_CHOL <= 261, "1", pdcd_blood_V1_W_40$V1_CHOL_DEX)
pdcd_blood_V1_W_40$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_CHOL > 261,  "2", pdcd_blood_V1_W_40$V1_CHOL_DEX)

max(pdcd_blood_V1_W_40$V1_HDL)/4
pdcd_blood_V1_W_40$V1_HDL_DEX <- "NA"
pdcd_blood_V1_W_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HDL <= 23, "2", pdcd_blood_V1_W_40$V1_HDL_DEX)
pdcd_blood_V1_W_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HDL > 23 & pdcd_blood_V1_W_40$V1_HDL <= 46, "1", pdcd_blood_V1_W_40$V1_HDL_DEX)
pdcd_blood_V1_W_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HDL > 46 & pdcd_blood_V1_W_40$V1_HDL <= 69, "1", pdcd_blood_V1_W_40$V1_HDL_DEX)
pdcd_blood_V1_W_40$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_HDL > 69,  "2", pdcd_blood_V1_W_40$V1_HDL_DEX)

max(pdcd_blood_V1_W_40$V1_TG)/4
pdcd_blood_V1_W_40$V1_TG_DEX <- "NA"
pdcd_blood_V1_W_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_40$V1_TG <= 120, "2", pdcd_blood_V1_W_40$V1_TG_DEX)
pdcd_blood_V1_W_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_40$V1_TG > 120 & pdcd_blood_V1_W_40$V1_TG <= 240, "1", pdcd_blood_V1_W_40$V1_TG_DEX)
pdcd_blood_V1_W_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_40$V1_TG > 240 & pdcd_blood_V1_W_40$V1_TG <= 360, "1", pdcd_blood_V1_W_40$V1_TG_DEX)
pdcd_blood_V1_W_40$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_40$V1_TG > 360,  "2", pdcd_blood_V1_W_40$V1_TG_DEX)

max(pdcd_blood_V1_W_40$V1_LDL)/4
pdcd_blood_V1_W_40$V1_LDL_DEX <- "NA"
pdcd_blood_V1_W_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_LDL <= 61, "2", pdcd_blood_V1_W_40$V1_LDL_DEX)
pdcd_blood_V1_W_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_LDL > 61 & pdcd_blood_V1_W_40$V1_LDL <= 122, "1", pdcd_blood_V1_W_40$V1_LDL_DEX)
pdcd_blood_V1_W_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_LDL > 122 & pdcd_blood_V1_W_40$V1_LDL <= 183, "1", pdcd_blood_V1_W_40$V1_LDL_DEX)
pdcd_blood_V1_W_40$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_40$V1_LDL > 183,  "2", pdcd_blood_V1_W_40$V1_LDL_DEX)

#V1_남성_50대_혈액검사(2개 군집)
max(pdcd_blood_V1_M_50$V1_WBC)/4
pdcd_blood_V1_M_50$V1_WBC_DEX <- "NA"
pdcd_blood_V1_M_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_WBC <= 3.8, "2", pdcd_blood_V1_M_50$V1_WBC_DEX)
pdcd_blood_V1_M_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_WBC > 3.8 & pdcd_blood_V1_M_50$V1_WBC <= 7.6, "1", pdcd_blood_V1_M_50$V1_WBC_DEX)
pdcd_blood_V1_M_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_WBC > 7.6 & pdcd_blood_V1_M_50$V1_WBC <= 11.4, "1", pdcd_blood_V1_M_50$V1_WBC_DEX)
pdcd_blood_V1_M_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_WBC > 11.4,  "2", pdcd_blood_V1_M_50$V1_WBC_DEX)

max(pdcd_blood_V1_M_50$V1_RBC)/4
pdcd_blood_V1_M_50$V1_RBC_DEX <- "NA"
pdcd_blood_V1_M_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_RBC <= 1.6, "2", pdcd_blood_V1_M_50$V1_RBC_DEX)
pdcd_blood_V1_M_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_RBC > 1.6 & pdcd_blood_V1_M_50$V1_RBC <= 3.2, "1", pdcd_blood_V1_M_50$V1_RBC_DEX)
pdcd_blood_V1_M_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_RBC > 3.2 & pdcd_blood_V1_M_50$V1_RBC <= 4.8, "1", pdcd_blood_V1_M_50$V1_RBC_DEX)
pdcd_blood_V1_M_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_50$V1_RBC > 4.8,  "2", pdcd_blood_V1_M_50$V1_RBC_DEX)

max(pdcd_blood_V1_M_50$V1_HB)/4
pdcd_blood_V1_M_50$V1_HB_DEX <- "NA"
pdcd_blood_V1_M_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HB <= 4.9, "2", pdcd_blood_V1_M_50$V1_HB_DEX)
pdcd_blood_V1_M_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HB > 4.9 & pdcd_blood_V1_M_50$V1_HB <= 9.8, "1", pdcd_blood_V1_M_50$V1_HB_DEX)
pdcd_blood_V1_M_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HB > 9.8 & pdcd_blood_V1_M_50$V1_HB <= 14.7, "1", pdcd_blood_V1_M_50$V1_HB_DEX)
pdcd_blood_V1_M_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HB > 14.7,  "2", pdcd_blood_V1_M_50$V1_HB_DEX)

max(pdcd_blood_V1_M_50$V1_HCT)/4
pdcd_blood_V1_M_50$V1_HCT_DEX <- "NA"
pdcd_blood_V1_M_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HCT <= 14, "2", pdcd_blood_V1_M_50$V1_HCT_DEX)
pdcd_blood_V1_M_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HCT > 14 & pdcd_blood_V1_M_50$V1_HCT <= 28, "1", pdcd_blood_V1_M_50$V1_HCT_DEX)
pdcd_blood_V1_M_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HCT > 28 & pdcd_blood_V1_M_50$V1_HCT <= 42, "1", pdcd_blood_V1_M_50$V1_HCT_DEX)
pdcd_blood_V1_M_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HCT > 42,  "2", pdcd_blood_V1_M_50$V1_HCT_DEX)

max(pdcd_blood_V1_M_50$V1_HBA1C)/4
pdcd_blood_V1_M_50$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_M_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HBA1C <= 2.7, "2", pdcd_blood_V1_M_50$V1_HBA1C_DEX)
pdcd_blood_V1_M_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HBA1C > 2.7 & pdcd_blood_V1_M_50$V1_HBA1C <= 5.4, "1", pdcd_blood_V1_M_50$V1_HBA1C_DEX)
pdcd_blood_V1_M_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HBA1C > 5.4 & pdcd_blood_V1_M_50$V1_HBA1C <= 8.1, "1", pdcd_blood_V1_M_50$V1_HBA1C_DEX)
pdcd_blood_V1_M_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HBA1C > 8.1,  "2", pdcd_blood_V1_M_50$V1_HBA1C_DEX)

max(pdcd_blood_V1_M_50$V1_PLAT)/4
pdcd_blood_V1_M_50$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_M_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_PLAT <= 120, "2", pdcd_blood_V1_M_50$V1_PLAT_DEX)
pdcd_blood_V1_M_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_PLAT > 120 & pdcd_blood_V1_M_50$V1_PLAT <= 240, "1", pdcd_blood_V1_M_50$V1_PLAT_DEX)
pdcd_blood_V1_M_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_PLAT > 240 & pdcd_blood_V1_M_50$V1_PLAT <= 360, "1", pdcd_blood_V1_M_50$V1_PLAT_DEX)
pdcd_blood_V1_M_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_PLAT > 360,  "2", pdcd_blood_V1_M_50$V1_PLAT_DEX)

max(pdcd_blood_V1_M_50$V1_ALT)/4
pdcd_blood_V1_M_50$V1_ALT_DEX <- "NA"
pdcd_blood_V1_M_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_ALT <= 120, "2", pdcd_blood_V1_M_50$V1_ALT_DEX)
pdcd_blood_V1_M_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_ALT > 120 & pdcd_blood_V1_M_50$V1_ALT <= 240, "1", pdcd_blood_V1_M_50$V1_ALT_DEX)
pdcd_blood_V1_M_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_ALT > 240 & pdcd_blood_V1_M_50$V1_ALT <= 360, "1", pdcd_blood_V1_M_50$V1_ALT_DEX)
pdcd_blood_V1_M_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_50$V1_ALT > 360,  "2", pdcd_blood_V1_M_50$V1_ALT_DEX)

max(pdcd_blood_V1_M_50$V1_AST)/4
pdcd_blood_V1_M_50$V1_AST_DEX <- "NA"
pdcd_blood_V1_M_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_50$V1_AST <= 41, "2", pdcd_blood_V1_M_50$V1_AST_DEX)
pdcd_blood_V1_M_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_50$V1_AST > 41 & pdcd_blood_V1_M_50$V1_AST <= 82, "1", pdcd_blood_V1_M_50$V1_AST_DEX)
pdcd_blood_V1_M_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_50$V1_AST > 82 & pdcd_blood_V1_M_50$V1_AST <= 123, "1", pdcd_blood_V1_M_50$V1_AST_DEX)
pdcd_blood_V1_M_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_50$V1_AST > 123,  "2", pdcd_blood_V1_M_50$V1_AST_DEX)

max(pdcd_blood_V1_M_50$V1_BUN)/4
pdcd_blood_V1_M_50$V1_BUN_DEX <- "NA"
pdcd_blood_V1_M_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_50$V1_BUN <= 21, "2", pdcd_blood_V1_M_50$V1_BUN_DEX)
pdcd_blood_V1_M_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_50$V1_BUN > 21 & pdcd_blood_V1_M_50$V1_BUN <= 42, "1", pdcd_blood_V1_M_50$V1_BUN_DEX)
pdcd_blood_V1_M_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_50$V1_BUN > 42 & pdcd_blood_V1_M_50$V1_BUN <= 63, "1", pdcd_blood_V1_M_50$V1_BUN_DEX)
pdcd_blood_V1_M_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_50$V1_BUN > 63,  "2", pdcd_blood_V1_M_50$V1_BUN_DEX)

max(pdcd_blood_V1_M_50$V1_CR)/4
pdcd_blood_V1_M_50$V1_CR_DEX <- "NA"
pdcd_blood_V1_M_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CR <= 4.4, "2", pdcd_blood_V1_M_50$V1_CR_DEX)
pdcd_blood_V1_M_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CR > 4.4 & pdcd_blood_V1_M_50$V1_CR <= 8.8, "1", pdcd_blood_V1_M_50$V1_CR_DEX)
pdcd_blood_V1_M_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CR > 8.8 & pdcd_blood_V1_M_50$V1_CR <= 13.2, "1", pdcd_blood_V1_M_50$V1_CR_DEX)
pdcd_blood_V1_M_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CR > 13.2,  "2", pdcd_blood_V1_M_50$V1_CR_DEX)

max(pdcd_blood_V1_M_50$V1_GLU)/4
pdcd_blood_V1_M_50$V1_GLU_DEX <- "NA"
pdcd_blood_V1_M_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_50$V1_GLU <= 72, "2", pdcd_blood_V1_M_50$V1_GLU_DEX)
pdcd_blood_V1_M_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_50$V1_GLU > 72 & pdcd_blood_V1_M_50$V1_GLU <= 144, "1", pdcd_blood_V1_M_50$V1_GLU_DEX)
pdcd_blood_V1_M_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_50$V1_GLU > 144 & pdcd_blood_V1_M_50$V1_GLU <= 216, "1", pdcd_blood_V1_M_50$V1_GLU_DEX)
pdcd_blood_V1_M_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_50$V1_GLU > 216,  "2", pdcd_blood_V1_M_50$V1_GLU_DEX)

max(pdcd_blood_V1_M_50$V1_CHOL)/4
pdcd_blood_V1_M_50$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_M_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CHOL <= 96, "2", pdcd_blood_V1_M_50$V1_CHOL_DEX)
pdcd_blood_V1_M_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CHOL > 96 & pdcd_blood_V1_M_50$V1_CHOL <= 192, "1", pdcd_blood_V1_M_50$V1_CHOL_DEX)
pdcd_blood_V1_M_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CHOL > 192 & pdcd_blood_V1_M_50$V1_CHOL <= 288, "1", pdcd_blood_V1_M_50$V1_CHOL_DEX)
pdcd_blood_V1_M_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_CHOL > 288,  "2", pdcd_blood_V1_M_50$V1_CHOL_DEX)

max(pdcd_blood_V1_M_50$V1_HDL)/4
pdcd_blood_V1_M_50$V1_HDL_DEX <- "NA"
pdcd_blood_V1_M_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HDL <= 22, "2", pdcd_blood_V1_M_50$V1_HDL_DEX)
pdcd_blood_V1_M_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HDL > 22 & pdcd_blood_V1_M_50$V1_HDL <= 44, "1", pdcd_blood_V1_M_50$V1_HDL_DEX)
pdcd_blood_V1_M_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HDL > 44 & pdcd_blood_V1_M_50$V1_HDL <= 66, "1", pdcd_blood_V1_M_50$V1_HDL_DEX)
pdcd_blood_V1_M_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_HDL > 66,  "2", pdcd_blood_V1_M_50$V1_HDL_DEX)

max(pdcd_blood_V1_M_50$V1_TG)/4
pdcd_blood_V1_M_50$V1_TG_DEX <- "NA"
pdcd_blood_V1_M_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_50$V1_TG <= 386, "2", pdcd_blood_V1_M_50$V1_TG_DEX)
pdcd_blood_V1_M_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_50$V1_TG > 386 & pdcd_blood_V1_M_50$V1_TG <= 772, "1", pdcd_blood_V1_M_50$V1_TG_DEX)
pdcd_blood_V1_M_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_50$V1_TG > 772 & pdcd_blood_V1_M_50$V1_TG <= 1158, "1", pdcd_blood_V1_M_50$V1_TG_DEX)
pdcd_blood_V1_M_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_50$V1_TG > 1158,  "2", pdcd_blood_V1_M_50$V1_TG_DEX)

max(pdcd_blood_V1_M_50$V1_LDL)/4
pdcd_blood_V1_M_50$V1_LDL_DEX <- "NA"
pdcd_blood_V1_M_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_LDL <= 58, "2", pdcd_blood_V1_M_50$V1_LDL_DEX)
pdcd_blood_V1_M_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_LDL > 58 & pdcd_blood_V1_M_50$V1_LDL <= 116, "1", pdcd_blood_V1_M_50$V1_LDL_DEX)
pdcd_blood_V1_M_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_LDL > 116 & pdcd_blood_V1_M_50$V1_LDL <= 174, "1", pdcd_blood_V1_M_50$V1_LDL_DEX)
pdcd_blood_V1_M_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_50$V1_LDL > 174,  "2", pdcd_blood_V1_M_50$V1_LDL_DEX)

#V1_여성_50대_혈액검사(2개 군집)
max(pdcd_blood_V1_W_50$V1_WBC)/4
pdcd_blood_V1_W_50$V1_WBC_DEX <- "NA"
pdcd_blood_V1_W_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_WBC <= 4, "2", pdcd_blood_V1_W_50$V1_WBC_DEX)
pdcd_blood_V1_W_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_WBC > 4 & pdcd_blood_V1_W_50$V1_WBC <= 8, "1", pdcd_blood_V1_W_50$V1_WBC_DEX)
pdcd_blood_V1_W_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_WBC > 8 & pdcd_blood_V1_W_50$V1_WBC <= 12, "1", pdcd_blood_V1_W_50$V1_WBC_DEX)
pdcd_blood_V1_W_50$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_WBC > 12,  "2", pdcd_blood_V1_W_50$V1_WBC_DEX)

max(pdcd_blood_V1_W_50$V1_RBC)/4
pdcd_blood_V1_W_50$V1_RBC_DEX <- "NA"
pdcd_blood_V1_W_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_RBC <= 1.5, "2", pdcd_blood_V1_W_50$V1_RBC_DEX)
pdcd_blood_V1_W_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_RBC > 1.5 & pdcd_blood_V1_W_50$V1_RBC <= 3, "1", pdcd_blood_V1_W_50$V1_RBC_DEX)
pdcd_blood_V1_W_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_RBC > 3 & pdcd_blood_V1_W_50$V1_RBC <= 4.5, "1", pdcd_blood_V1_W_50$V1_RBC_DEX)
pdcd_blood_V1_W_50$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_50$V1_RBC > 4.5,  "2", pdcd_blood_V1_W_50$V1_RBC_DEX)

max(pdcd_blood_V1_W_50$V1_HB)/4
pdcd_blood_V1_W_50$V1_HB_DEX <- "NA"
pdcd_blood_V1_W_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HB <= 4.3, "2", pdcd_blood_V1_W_50$V1_HB_DEX)
pdcd_blood_V1_W_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HB > 4.3 & pdcd_blood_V1_W_50$V1_HB <= 8.6, "1", pdcd_blood_V1_W_50$V1_HB_DEX)
pdcd_blood_V1_W_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HB > 8.6 & pdcd_blood_V1_W_50$V1_HB <= 12.9, "1", pdcd_blood_V1_W_50$V1_HB_DEX)
pdcd_blood_V1_W_50$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HB > 12.9,  "2", pdcd_blood_V1_W_50$V1_HB_DEX)

max(pdcd_blood_V1_W_50$V1_HCT)/4
pdcd_blood_V1_W_50$V1_HCT_DEX <- "NA"
pdcd_blood_V1_W_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HCT <= 13, "2", pdcd_blood_V1_W_50$V1_HCT_DEX)
pdcd_blood_V1_W_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HCT > 13 & pdcd_blood_V1_W_50$V1_HCT <= 26, "1", pdcd_blood_V1_W_50$V1_HCT_DEX)
pdcd_blood_V1_W_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HCT > 26 & pdcd_blood_V1_W_50$V1_HCT <= 39, "1", pdcd_blood_V1_W_50$V1_HCT_DEX)
pdcd_blood_V1_W_50$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HCT > 39,  "2", pdcd_blood_V1_W_50$V1_HCT_DEX)

max(pdcd_blood_V1_W_50$V1_HBA1C)/4
pdcd_blood_V1_W_50$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_W_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HBA1C <= 3.1, "2", pdcd_blood_V1_W_50$V1_HBA1C_DEX)
pdcd_blood_V1_W_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HBA1C > 3.1 & pdcd_blood_V1_W_50$V1_HBA1C <= 6.2, "1", pdcd_blood_V1_W_50$V1_HBA1C_DEX)
pdcd_blood_V1_W_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HBA1C > 6.2 & pdcd_blood_V1_W_50$V1_HBA1C <= 9.3, "1", pdcd_blood_V1_W_50$V1_HBA1C_DEX)
pdcd_blood_V1_W_50$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HBA1C > 9.3,  "2", pdcd_blood_V1_W_50$V1_HBA1C_DEX)

max(pdcd_blood_V1_W_50$V1_PLAT)/4
pdcd_blood_V1_W_50$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_W_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_PLAT <= 115, "2", pdcd_blood_V1_W_50$V1_PLAT_DEX)
pdcd_blood_V1_W_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_PLAT > 115 & pdcd_blood_V1_W_50$V1_PLAT <= 230, "1", pdcd_blood_V1_W_50$V1_PLAT_DEX)
pdcd_blood_V1_W_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_PLAT > 230 & pdcd_blood_V1_W_50$V1_PLAT <= 345, "1", pdcd_blood_V1_W_50$V1_PLAT_DEX)
pdcd_blood_V1_W_50$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_PLAT > 345,  "2", pdcd_blood_V1_W_50$V1_PLAT_DEX)

max(pdcd_blood_V1_W_50$V1_ALT)/4
pdcd_blood_V1_W_50$V1_ALT_DEX <- "NA"
pdcd_blood_V1_W_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_ALT <= 32, "2", pdcd_blood_V1_W_50$V1_ALT_DEX)
pdcd_blood_V1_W_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_ALT > 32 & pdcd_blood_V1_W_50$V1_ALT <= 64, "1", pdcd_blood_V1_W_50$V1_ALT_DEX)
pdcd_blood_V1_W_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_ALT > 64 & pdcd_blood_V1_W_50$V1_ALT <= 96, "1", pdcd_blood_V1_W_50$V1_ALT_DEX)
pdcd_blood_V1_W_50$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_50$V1_ALT > 96,  "2", pdcd_blood_V1_W_50$V1_ALT_DEX)

max(pdcd_blood_V1_W_50$V1_AST)/4
pdcd_blood_V1_W_50$V1_AST_DEX <- "NA"
pdcd_blood_V1_W_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_50$V1_AST <= 21, "2", pdcd_blood_V1_W_50$V1_AST_DEX)
pdcd_blood_V1_W_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_50$V1_AST > 21 & pdcd_blood_V1_W_50$V1_AST <= 42, "1", pdcd_blood_V1_W_50$V1_AST_DEX)
pdcd_blood_V1_W_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_50$V1_AST > 42 & pdcd_blood_V1_W_50$V1_AST <= 63, "1", pdcd_blood_V1_W_50$V1_AST_DEX)
pdcd_blood_V1_W_50$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_50$V1_AST > 63,  "2", pdcd_blood_V1_W_50$V1_AST_DEX)

max(pdcd_blood_V1_W_50$V1_BUN)/4
pdcd_blood_V1_W_50$V1_BUN_DEX <- "NA"
pdcd_blood_V1_W_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_50$V1_BUN <= 12, "2", pdcd_blood_V1_W_50$V1_BUN_DEX)
pdcd_blood_V1_W_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_50$V1_BUN > 12 & pdcd_blood_V1_W_50$V1_BUN <= 24, "1", pdcd_blood_V1_W_50$V1_BUN_DEX)
pdcd_blood_V1_W_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_50$V1_BUN > 24 & pdcd_blood_V1_W_50$V1_BUN <= 36, "1", pdcd_blood_V1_W_50$V1_BUN_DEX)
pdcd_blood_V1_W_50$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_50$V1_BUN > 36,  "2", pdcd_blood_V1_W_50$V1_BUN_DEX)

max(pdcd_blood_V1_W_50$V1_CR)/4
pdcd_blood_V1_W_50$V1_CR_DEX <- "NA"
pdcd_blood_V1_W_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CR <= 2.5, "2", pdcd_blood_V1_W_50$V1_CR_DEX)
pdcd_blood_V1_W_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CR > 2.5 & pdcd_blood_V1_W_50$V1_CR <= 5, "1", pdcd_blood_V1_W_50$V1_CR_DEX)
pdcd_blood_V1_W_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CR > 5 & pdcd_blood_V1_W_50$V1_CR <= 7.5, "1", pdcd_blood_V1_W_50$V1_CR_DEX)
pdcd_blood_V1_W_50$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CR > 10,  "2", pdcd_blood_V1_W_50$V1_CR_DEX)

max(pdcd_blood_V1_W_50$V1_GLU)/4
pdcd_blood_V1_W_50$V1_GLU_DEX <- "NA"
pdcd_blood_V1_W_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_50$V1_GLU <= 71, "2", pdcd_blood_V1_W_50$V1_GLU_DEX)
pdcd_blood_V1_W_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_50$V1_GLU > 71 & pdcd_blood_V1_W_50$V1_GLU <= 142, "1", pdcd_blood_V1_W_50$V1_GLU_DEX)
pdcd_blood_V1_W_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_50$V1_GLU > 142 & pdcd_blood_V1_W_50$V1_GLU <= 213, "1", pdcd_blood_V1_W_50$V1_GLU_DEX)
pdcd_blood_V1_W_50$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_50$V1_GLU > 213,  "2", pdcd_blood_V1_W_50$V1_GLU_DEX)

max(pdcd_blood_V1_W_50$V1_CHOL)/4
pdcd_blood_V1_W_50$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_W_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CHOL <= 86, "2", pdcd_blood_V1_W_50$V1_CHOL_DEX)
pdcd_blood_V1_W_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CHOL > 86 & pdcd_blood_V1_W_50$V1_CHOL <= 172, "1", pdcd_blood_V1_W_50$V1_CHOL_DEX)
pdcd_blood_V1_W_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CHOL > 172 & pdcd_blood_V1_W_50$V1_CHOL <= 258, "1", pdcd_blood_V1_W_50$V1_CHOL_DEX)
pdcd_blood_V1_W_50$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_CHOL > 258,  "2", pdcd_blood_V1_W_50$V1_CHOL_DEX)

max(pdcd_blood_V1_W_50$V1_HDL)/4
pdcd_blood_V1_W_50$V1_HDL_DEX <- "NA"
pdcd_blood_V1_W_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HDL <= 22, "2", pdcd_blood_V1_W_50$V1_HDL_DEX)
pdcd_blood_V1_W_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HDL > 22 & pdcd_blood_V1_W_50$V1_HDL <= 44, "1", pdcd_blood_V1_W_50$V1_HDL_DEX)
pdcd_blood_V1_W_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HDL > 44 & pdcd_blood_V1_W_50$V1_HDL <= 66, "1", pdcd_blood_V1_W_50$V1_HDL_DEX)
pdcd_blood_V1_W_50$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_HDL > 66,  "2", pdcd_blood_V1_W_50$V1_HDL_DEX)

max(pdcd_blood_V1_W_50$V1_TG)/4
pdcd_blood_V1_W_50$V1_TG_DEX <- "NA"
pdcd_blood_V1_W_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_50$V1_TG <= 244, "2", pdcd_blood_V1_W_50$V1_TG_DEX)
pdcd_blood_V1_W_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_50$V1_TG > 244 & pdcd_blood_V1_W_50$V1_TG <= 488, "1", pdcd_blood_V1_W_50$V1_TG_DEX)
pdcd_blood_V1_W_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_50$V1_TG > 488 & pdcd_blood_V1_W_50$V1_TG <= 732, "1", pdcd_blood_V1_W_50$V1_TG_DEX)
pdcd_blood_V1_W_50$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_50$V1_TG > 732,  "2", pdcd_blood_V1_W_50$V1_TG_DEX)

max(pdcd_blood_V1_W_50$V1_LDL)/4
pdcd_blood_V1_W_50$V1_LDL_DEX <- "NA"
pdcd_blood_V1_W_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_LDL <= 67, "2", pdcd_blood_V1_W_50$V1_LDL_DEX)
pdcd_blood_V1_W_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_LDL > 67 & pdcd_blood_V1_W_50$V1_LDL <= 134, "1", pdcd_blood_V1_W_50$V1_LDL_DEX)
pdcd_blood_V1_W_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_LDL > 134 & pdcd_blood_V1_W_50$V1_LDL <= 201, "1", pdcd_blood_V1_W_50$V1_LDL_DEX)
pdcd_blood_V1_W_50$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_50$V1_LDL > 201,  "2", pdcd_blood_V1_W_50$V1_LDL_DEX)

#V1_남성_60대_혈액검사 
max(pdcd_blood_V1_M_60$V1_WBC)/4
pdcd_blood_V1_M_60$V1_WBC_DEX <- "NA"
pdcd_blood_V1_M_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_WBC <= 3.4, "2", pdcd_blood_V1_M_60$V1_WBC_DEX)
pdcd_blood_V1_M_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_WBC > 3.4 & pdcd_blood_V1_M_60$V1_WBC <= 6.8, "1", pdcd_blood_V1_M_60$V1_WBC_DEX)
pdcd_blood_V1_M_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_WBC > 6.8 & pdcd_blood_V1_M_60$V1_WBC <= 10.2, "1", pdcd_blood_V1_M_60$V1_WBC_DEX)
pdcd_blood_V1_M_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_WBC > 10.2,  "2", pdcd_blood_V1_M_60$V1_WBC_DEX)

max(pdcd_blood_V1_M_60$V1_RBC)/4
pdcd_blood_V1_M_60$V1_RBC_DEX <- "NA"
pdcd_blood_V1_M_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_RBC <= 1.5, "2", pdcd_blood_V1_M_60$V1_RBC_DEX)
pdcd_blood_V1_M_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_RBC > 1.5 & pdcd_blood_V1_M_60$V1_RBC <= 3, "1", pdcd_blood_V1_M_60$V1_RBC_DEX)
pdcd_blood_V1_M_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_RBC > 3 & pdcd_blood_V1_M_60$V1_RBC <= 4.5, "1", pdcd_blood_V1_M_60$V1_RBC_DEX)
pdcd_blood_V1_M_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_60$V1_RBC > 4.5,  "2", pdcd_blood_V1_M_60$V1_RBC_DEX)

max(pdcd_blood_V1_M_60$V1_HB)/4
pdcd_blood_V1_M_60$V1_HB_DEX <- "NA"
pdcd_blood_V1_M_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HB <= 4.5, "2", pdcd_blood_V1_M_60$V1_HB_DEX)
pdcd_blood_V1_M_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HB > 4.5 & pdcd_blood_V1_M_60$V1_HB <= 9, "1", pdcd_blood_V1_M_60$V1_HB_DEX)
pdcd_blood_V1_M_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HB > 9 & pdcd_blood_V1_M_60$V1_HB <= 13.5, "1", pdcd_blood_V1_M_60$V1_HB_DEX)
pdcd_blood_V1_M_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HB > 13.5,  "2", pdcd_blood_V1_M_60$V1_HB_DEX)

max(pdcd_blood_V1_M_60$V1_HCT)/4
pdcd_blood_V1_M_60$V1_HCT_DEX <- "NA"
pdcd_blood_V1_M_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HCT <= 14, "2", pdcd_blood_V1_M_60$V1_HCT_DEX)
pdcd_blood_V1_M_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HCT > 14 & pdcd_blood_V1_M_60$V1_HCT <= 28, "1", pdcd_blood_V1_M_60$V1_HCT_DEX)
pdcd_blood_V1_M_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HCT > 28 & pdcd_blood_V1_M_60$V1_HCT <= 42, "1", pdcd_blood_V1_M_60$V1_HCT_DEX)
pdcd_blood_V1_M_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HCT > 42,  "2", pdcd_blood_V1_M_60$V1_HCT_DEX)

max(pdcd_blood_V1_M_60$V1_HBA1C)/4
pdcd_blood_V1_M_60$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_M_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HBA1C <= 2.6, "2", pdcd_blood_V1_M_60$V1_HBA1C_DEX)
pdcd_blood_V1_M_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HBA1C > 2.6 & pdcd_blood_V1_M_60$V1_HBA1C <= 5.2, "1", pdcd_blood_V1_M_60$V1_HBA1C_DEX)
pdcd_blood_V1_M_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HBA1C > 5.2 & pdcd_blood_V1_M_60$V1_HBA1C <= 7.8, "1", pdcd_blood_V1_M_60$V1_HBA1C_DEX)
pdcd_blood_V1_M_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HBA1C > 7.8,  "2", pdcd_blood_V1_M_60$V1_HBA1C_DEX)

max(pdcd_blood_V1_M_60$V1_PLAT)/4
pdcd_blood_V1_M_60$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_M_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_PLAT <= 150, "2", pdcd_blood_V1_M_60$V1_PLAT_DEX)
pdcd_blood_V1_M_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_PLAT > 150 & pdcd_blood_V1_M_60$V1_PLAT <= 300, "1", pdcd_blood_V1_M_60$V1_PLAT_DEX)
pdcd_blood_V1_M_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_PLAT > 300 & pdcd_blood_V1_M_60$V1_PLAT <= 450, "1", pdcd_blood_V1_M_60$V1_PLAT_DEX)
pdcd_blood_V1_M_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_PLAT > 450,  "2", pdcd_blood_V1_M_60$V1_PLAT_DEX)

max(pdcd_blood_V1_M_60$V1_ALT)/4
pdcd_blood_V1_M_60$V1_ALT_DEX <- "NA"
pdcd_blood_V1_M_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_ALT <= 34, "2", pdcd_blood_V1_M_60$V1_ALT_DEX)
pdcd_blood_V1_M_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_ALT > 34 & pdcd_blood_V1_M_60$V1_ALT <= 68, "1", pdcd_blood_V1_M_60$V1_ALT_DEX)
pdcd_blood_V1_M_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_ALT > 68 & pdcd_blood_V1_M_60$V1_ALT <= 102, "1", pdcd_blood_V1_M_60$V1_ALT_DEX)
pdcd_blood_V1_M_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_60$V1_ALT > 102,  "2", pdcd_blood_V1_M_60$V1_ALT_DEX)

max(pdcd_blood_V1_M_60$V1_AST)/4
pdcd_blood_V1_M_60$V1_AST_DEX <- "NA"
pdcd_blood_V1_M_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_60$V1_AST <= 40, "2", pdcd_blood_V1_M_60$V1_AST_DEX)
pdcd_blood_V1_M_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_60$V1_AST > 40 & pdcd_blood_V1_M_60$V1_AST <= 80, "1", pdcd_blood_V1_M_60$V1_AST_DEX)
pdcd_blood_V1_M_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_60$V1_AST > 80 & pdcd_blood_V1_M_60$V1_AST <= 120, "1", pdcd_blood_V1_M_60$V1_AST_DEX)
pdcd_blood_V1_M_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_60$V1_AST > 120,  "2", pdcd_blood_V1_M_60$V1_AST_DEX)

max(pdcd_blood_V1_M_60$V1_BUN)/4
pdcd_blood_V1_M_60$V1_BUN_DEX <- "NA"
pdcd_blood_V1_M_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_60$V1_BUN <= 9.8, "2", pdcd_blood_V1_M_60$V1_BUN_DEX)
pdcd_blood_V1_M_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_60$V1_BUN > 9.8 & pdcd_blood_V1_M_60$V1_BUN <= 19.6, "1", pdcd_blood_V1_M_60$V1_BUN_DEX)
pdcd_blood_V1_M_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_60$V1_BUN > 19.6 & pdcd_blood_V1_M_60$V1_BUN <= 29.4, "1", pdcd_blood_V1_M_60$V1_BUN_DEX)
pdcd_blood_V1_M_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_60$V1_BUN > 29.4,  "2", pdcd_blood_V1_M_60$V1_BUN_DEX)

max(pdcd_blood_V1_M_60$V1_CR)/4
pdcd_blood_V1_M_60$V1_CR_DEX <- "NA"
pdcd_blood_V1_M_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CR <= 0.5, "2", pdcd_blood_V1_M_60$V1_CR_DEX)
pdcd_blood_V1_M_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CR > 0.5 & pdcd_blood_V1_M_60$V1_CR <= 1, "1", pdcd_blood_V1_M_60$V1_CR_DEX)
pdcd_blood_V1_M_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CR > 1 & pdcd_blood_V1_M_60$V1_CR <= 1.5, "1", pdcd_blood_V1_M_60$V1_CR_DEX)
pdcd_blood_V1_M_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CR > 1.5,  "2", pdcd_blood_V1_M_60$V1_CR_DEX)

max(pdcd_blood_V1_M_60$V1_GLU)/4
pdcd_blood_V1_M_60$V1_GLU_DEX <- "NA"
pdcd_blood_V1_M_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_60$V1_GLU <= 67, "2", pdcd_blood_V1_M_60$V1_GLU_DEX)
pdcd_blood_V1_M_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_60$V1_GLU > 67 & pdcd_blood_V1_M_60$V1_GLU <= 134, "1", pdcd_blood_V1_M_60$V1_GLU_DEX)
pdcd_blood_V1_M_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_60$V1_GLU > 134 & pdcd_blood_V1_M_60$V1_GLU <= 201, "1", pdcd_blood_V1_M_60$V1_GLU_DEX)
pdcd_blood_V1_M_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_60$V1_GLU > 201,  "2", pdcd_blood_V1_M_60$V1_GLU_DEX)

max(pdcd_blood_V1_M_60$V1_CHOL)/4
pdcd_blood_V1_M_60$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_M_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CHOL <= 80, "2", pdcd_blood_V1_M_60$V1_CHOL_DEX)
pdcd_blood_V1_M_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CHOL > 80 & pdcd_blood_V1_M_60$V1_CHOL <= 160, "1", pdcd_blood_V1_M_60$V1_CHOL_DEX)
pdcd_blood_V1_M_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CHOL > 160 & pdcd_blood_V1_M_60$V1_CHOL <= 240, "1", pdcd_blood_V1_M_60$V1_CHOL_DEX)
pdcd_blood_V1_M_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_CHOL > 240,  "2", pdcd_blood_V1_M_60$V1_CHOL_DEX)

max(pdcd_blood_V1_M_60$V1_HDL)/4
pdcd_blood_V1_M_60$V1_HDL_DEX <- "NA"
pdcd_blood_V1_M_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HDL <= 21, "2", pdcd_blood_V1_M_60$V1_HDL_DEX)
pdcd_blood_V1_M_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HDL > 21 & pdcd_blood_V1_M_60$V1_HDL <= 42, "1", pdcd_blood_V1_M_60$V1_HDL_DEX)
pdcd_blood_V1_M_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HDL > 42 & pdcd_blood_V1_M_60$V1_HDL <= 63, "1", pdcd_blood_V1_M_60$V1_HDL_DEX)
pdcd_blood_V1_M_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_HDL > 63,  "2", pdcd_blood_V1_M_60$V1_HDL_DEX)

max(pdcd_blood_V1_M_60$V1_TG)/4
pdcd_blood_V1_M_60$V1_TG_DEX <- "NA"
pdcd_blood_V1_M_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_60$V1_TG <= 313, "2", pdcd_blood_V1_M_60$V1_TG_DEX)
pdcd_blood_V1_M_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_60$V1_TG > 313 & pdcd_blood_V1_M_60$V1_TG <= 626, "1", pdcd_blood_V1_M_60$V1_TG_DEX)
pdcd_blood_V1_M_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_60$V1_TG > 626 & pdcd_blood_V1_M_60$V1_TG <= 939, "1", pdcd_blood_V1_M_60$V1_TG_DEX)
pdcd_blood_V1_M_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_60$V1_TG > 939,  "2", pdcd_blood_V1_M_60$V1_TG_DEX)

max(pdcd_blood_V1_M_60$V1_LDL)/4
pdcd_blood_V1_M_60$V1_LDL_DEX <- "NA"
pdcd_blood_V1_M_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_LDL <= 58, "2", pdcd_blood_V1_M_60$V1_LDL_DEX)
pdcd_blood_V1_M_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_LDL > 58 & pdcd_blood_V1_M_60$V1_LDL <= 116, "1", pdcd_blood_V1_M_60$V1_LDL_DEX)
pdcd_blood_V1_M_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_LDL > 116 & pdcd_blood_V1_M_60$V1_LDL <= 174, "1", pdcd_blood_V1_M_60$V1_LDL_DEX)
pdcd_blood_V1_M_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_60$V1_LDL > 174,  "2", pdcd_blood_V1_M_60$V1_LDL_DEX)

#V1_여성_60대_혈액검사
max(pdcd_blood_V1_W_60$V1_WBC)/4
pdcd_blood_V1_W_60$V1_WBC_DEX <- "NA"
pdcd_blood_V1_W_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_WBC <= 3.5, "2", pdcd_blood_V1_W_60$V1_WBC_DEX)
pdcd_blood_V1_W_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_WBC > 3.5 & pdcd_blood_V1_W_60$V1_WBC <= 7, "1", pdcd_blood_V1_W_60$V1_WBC_DEX)
pdcd_blood_V1_W_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_WBC > 7 & pdcd_blood_V1_W_60$V1_WBC <= 10.5, "1", pdcd_blood_V1_W_60$V1_WBC_DEX)
pdcd_blood_V1_W_60$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_WBC > 10.5,  "2", pdcd_blood_V1_W_60$V1_WBC_DEX)

max(pdcd_blood_V1_W_60$V1_RBC)/4
pdcd_blood_V1_W_60$V1_RBC_DEX <- "NA"
pdcd_blood_V1_W_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_RBC <= 1.4, "2", pdcd_blood_V1_W_60$V1_RBC_DEX)
pdcd_blood_V1_W_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_RBC > 1.4 & pdcd_blood_V1_W_60$V1_RBC <= 2.8, "1", pdcd_blood_V1_W_60$V1_RBC_DEX)
pdcd_blood_V1_W_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_RBC > 2.8 & pdcd_blood_V1_W_60$V1_RBC <= 4.2, "1", pdcd_blood_V1_W_60$V1_RBC_DEX)
pdcd_blood_V1_W_60$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_60$V1_RBC > 4.2,  "2", pdcd_blood_V1_W_60$V1_RBC_DEX)

max(pdcd_blood_V1_W_60$V1_HB)/4
pdcd_blood_V1_W_60$V1_HB_DEX <- "NA"
pdcd_blood_V1_W_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HB <= 4, "2", pdcd_blood_V1_W_60$V1_HB_DEX)
pdcd_blood_V1_W_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HB > 4 & pdcd_blood_V1_W_60$V1_HB <= 8, "1", pdcd_blood_V1_W_60$V1_HB_DEX)
pdcd_blood_V1_W_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HB > 8 & pdcd_blood_V1_W_60$V1_HB <= 12, "1", pdcd_blood_V1_W_60$V1_HB_DEX)
pdcd_blood_V1_W_60$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HB > 12,  "2", pdcd_blood_V1_W_60$V1_HB_DEX)

max(pdcd_blood_V1_W_60$V1_HCT)/4
pdcd_blood_V1_W_60$V1_HCT_DEX <- "NA"
pdcd_blood_V1_W_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HCT <= 12, "2", pdcd_blood_V1_W_60$V1_HCT_DEX)
pdcd_blood_V1_W_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HCT > 12 & pdcd_blood_V1_W_60$V1_HCT <= 24, "1", pdcd_blood_V1_W_60$V1_HCT_DEX)
pdcd_blood_V1_W_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HCT > 24 & pdcd_blood_V1_W_60$V1_HCT <= 36, "1", pdcd_blood_V1_W_60$V1_HCT_DEX)
pdcd_blood_V1_W_60$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HCT > 36,  "2", pdcd_blood_V1_W_60$V1_HCT_DEX)

max(pdcd_blood_V1_W_60$V1_HBA1C)/4
pdcd_blood_V1_W_60$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_W_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HBA1C <= 2.9, "2", pdcd_blood_V1_W_60$V1_HBA1C_DEX)
pdcd_blood_V1_W_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HBA1C > 2.9 & pdcd_blood_V1_W_60$V1_HBA1C <= 5.8, "1", pdcd_blood_V1_W_60$V1_HBA1C_DEX)
pdcd_blood_V1_W_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HBA1C > 5.8 & pdcd_blood_V1_W_60$V1_HBA1C <= 8.7, "1", pdcd_blood_V1_W_60$V1_HBA1C_DEX)
pdcd_blood_V1_W_60$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HBA1C > 8.7,  "2", pdcd_blood_V1_W_60$V1_HBA1C_DEX)

max(pdcd_blood_V1_W_60$V1_PLAT)/4
pdcd_blood_V1_W_60$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_W_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_PLAT <= 138, "2", pdcd_blood_V1_W_60$V1_PLAT_DEX)
pdcd_blood_V1_W_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_PLAT > 138 & pdcd_blood_V1_W_60$V1_PLAT <= 276, "1", pdcd_blood_V1_W_60$V1_PLAT_DEX)
pdcd_blood_V1_W_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_PLAT > 276 & pdcd_blood_V1_W_60$V1_PLAT <= 414, "1", pdcd_blood_V1_W_60$V1_PLAT_DEX)
pdcd_blood_V1_W_60$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_PLAT > 414,  "2", pdcd_blood_V1_W_60$V1_PLAT_DEX)

max(pdcd_blood_V1_W_60$V1_ALT)/4
pdcd_blood_V1_W_60$V1_ALT_DEX <- "NA"
pdcd_blood_V1_W_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_ALT <= 43, "2", pdcd_blood_V1_W_60$V1_ALT_DEX)
pdcd_blood_V1_W_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_ALT > 43 & pdcd_blood_V1_W_60$V1_ALT <= 86, "1", pdcd_blood_V1_W_60$V1_ALT_DEX)
pdcd_blood_V1_W_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_ALT > 86 & pdcd_blood_V1_W_60$V1_ALT <= 129, "1", pdcd_blood_V1_W_60$V1_ALT_DEX)
pdcd_blood_V1_W_60$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_60$V1_ALT > 129,  "2", pdcd_blood_V1_W_60$V1_ALT_DEX)

max(pdcd_blood_V1_W_60$V1_AST)/4
pdcd_blood_V1_W_60$V1_AST_DEX <- "NA"
pdcd_blood_V1_W_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_60$V1_AST <= 44, "2", pdcd_blood_V1_W_60$V1_AST_DEX)
pdcd_blood_V1_W_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_60$V1_AST > 44 & pdcd_blood_V1_W_60$V1_AST <= 88, "1", pdcd_blood_V1_W_60$V1_AST_DEX)
pdcd_blood_V1_W_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_60$V1_AST > 88 & pdcd_blood_V1_W_60$V1_AST <= 132, "1", pdcd_blood_V1_W_60$V1_AST_DEX)
pdcd_blood_V1_W_60$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_60$V1_AST > 132,  "2", pdcd_blood_V1_W_60$V1_AST_DEX)

max(pdcd_blood_V1_W_60$V1_BUN)/4
pdcd_blood_V1_W_60$V1_BUN_DEX <- "NA"
pdcd_blood_V1_W_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_60$V1_BUN <= 8.7, "2", pdcd_blood_V1_W_60$V1_BUN_DEX)
pdcd_blood_V1_W_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_60$V1_BUN > 8.7 & pdcd_blood_V1_W_60$V1_BUN <= 17.4, "1", pdcd_blood_V1_W_60$V1_BUN_DEX)
pdcd_blood_V1_W_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_60$V1_BUN > 17.4 & pdcd_blood_V1_W_60$V1_BUN <= 26.1, "1", pdcd_blood_V1_W_60$V1_BUN_DEX)
pdcd_blood_V1_W_60$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_60$V1_BUN > 26.1,  "2", pdcd_blood_V1_W_60$V1_BUN_DEX)

max(pdcd_blood_V1_W_60$V1_CR)/4
pdcd_blood_V1_W_60$V1_CR_DEX <- "NA"
pdcd_blood_V1_W_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CR <= 0.9, "2", pdcd_blood_V1_W_60$V1_CR_DEX)
pdcd_blood_V1_W_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CR > 0.9 & pdcd_blood_V1_W_60$V1_CR <= 1.8, "1", pdcd_blood_V1_W_60$V1_CR_DEX)
pdcd_blood_V1_W_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CR > 1.8 & pdcd_blood_V1_W_60$V1_CR <= 2.7, "1", pdcd_blood_V1_W_60$V1_CR_DEX)
pdcd_blood_V1_W_60$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CR > 2.7,  "2", pdcd_blood_V1_W_60$V1_CR_DEX)

max(pdcd_blood_V1_W_60$V1_GLU)/4
pdcd_blood_V1_W_60$V1_GLU_DEX <- "NA"
pdcd_blood_V1_W_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_60$V1_GLU <= 62, "2", pdcd_blood_V1_W_60$V1_GLU_DEX)
pdcd_blood_V1_W_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_60$V1_GLU > 62 & pdcd_blood_V1_W_60$V1_GLU <= 124, "1", pdcd_blood_V1_W_60$V1_GLU_DEX)
pdcd_blood_V1_W_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_60$V1_GLU > 124 & pdcd_blood_V1_W_60$V1_GLU <= 186, "1", pdcd_blood_V1_W_60$V1_GLU_DEX)
pdcd_blood_V1_W_60$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_60$V1_GLU > 186,  "2", pdcd_blood_V1_W_60$V1_GLU_DEX)

max(pdcd_blood_V1_W_60$V1_CHOL)/4
pdcd_blood_V1_W_60$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_W_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CHOL <= 95, "2", pdcd_blood_V1_W_60$V1_CHOL_DEX)
pdcd_blood_V1_W_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CHOL > 95 & pdcd_blood_V1_W_60$V1_CHOL <= 190, "1", pdcd_blood_V1_W_60$V1_CHOL_DEX)
pdcd_blood_V1_W_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CHOL > 190 & pdcd_blood_V1_W_60$V1_CHOL <= 285, "1", pdcd_blood_V1_W_60$V1_CHOL_DEX)
pdcd_blood_V1_W_60$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_CHOL > 285,  "2", pdcd_blood_V1_W_60$V1_CHOL_DEX)

max(pdcd_blood_V1_W_60$V1_HDL)/4
pdcd_blood_V1_W_60$V1_HDL_DEX <- "NA"
pdcd_blood_V1_W_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HDL <= 23, "2", pdcd_blood_V1_W_60$V1_HDL_DEX)
pdcd_blood_V1_W_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HDL > 23 & pdcd_blood_V1_W_60$V1_HDL <= 46, "1", pdcd_blood_V1_W_60$V1_HDL_DEX)
pdcd_blood_V1_W_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HDL > 46 & pdcd_blood_V1_W_60$V1_HDL <= 69, "1", pdcd_blood_V1_W_60$V1_HDL_DEX)
pdcd_blood_V1_W_60$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_HDL > 69,  "2", pdcd_blood_V1_W_60$V1_HDL_DEX)

max(pdcd_blood_V1_W_60$V1_TG)/4
pdcd_blood_V1_W_60$V1_TG_DEX <- "NA"
pdcd_blood_V1_W_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_60$V1_TG <= 188, "2", pdcd_blood_V1_W_60$V1_TG_DEX)
pdcd_blood_V1_W_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_60$V1_TG > 188 & pdcd_blood_V1_W_60$V1_TG <= 376, "1", pdcd_blood_V1_W_60$V1_TG_DEX)
pdcd_blood_V1_W_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_60$V1_TG > 376 & pdcd_blood_V1_W_60$V1_TG <= 564, "1", pdcd_blood_V1_W_60$V1_TG_DEX)
pdcd_blood_V1_W_60$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_60$V1_TG > 564,  "2", pdcd_blood_V1_W_60$V1_TG_DEX)

max(pdcd_blood_V1_W_60$V1_LDL)/4
pdcd_blood_V1_W_60$V1_LDL_DEX <- "NA"
pdcd_blood_V1_W_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_LDL <= 64, "2", pdcd_blood_V1_W_60$V1_LDL_DEX)
pdcd_blood_V1_W_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_LDL > 64 & pdcd_blood_V1_W_60$V1_LDL <= 128, "1", pdcd_blood_V1_W_60$V1_LDL_DEX)
pdcd_blood_V1_W_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_LDL > 128 & pdcd_blood_V1_W_60$V1_LDL <= 192, "1", pdcd_blood_V1_W_60$V1_LDL_DEX)
pdcd_blood_V1_W_60$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_60$V1_LDL > 192,  "2", pdcd_blood_V1_W_60$V1_LDL_DEX)

#V1_남성_70대_혈액검사
max(pdcd_blood_V1_M_70$V1_WBC)/4
pdcd_blood_V1_M_70$V1_WBC_DEX <- "NA"
pdcd_blood_V1_M_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_WBC <= 3.5, "2", pdcd_blood_V1_M_70$V1_WBC_DEX)
pdcd_blood_V1_M_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_WBC > 3.5 & pdcd_blood_V1_M_70$V1_WBC <= 7, "1", pdcd_blood_V1_M_70$V1_WBC_DEX)
pdcd_blood_V1_M_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_WBC > 7 & pdcd_blood_V1_M_70$V1_WBC <= 10.5, "1", pdcd_blood_V1_M_70$V1_WBC_DEX)
pdcd_blood_V1_M_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_WBC > 10.5,  "2", pdcd_blood_V1_M_70$V1_WBC_DEX)

max(pdcd_blood_V1_M_70$V1_RBC)/4
pdcd_blood_V1_M_70$V1_RBC_DEX <- "NA"
pdcd_blood_V1_M_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_RBC <= 1.4, "2", pdcd_blood_V1_M_70$V1_RBC_DEX)
pdcd_blood_V1_M_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_RBC > 1.4 & pdcd_blood_V1_M_70$V1_RBC <= 2.8, "1", pdcd_blood_V1_M_70$V1_RBC_DEX)
pdcd_blood_V1_M_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_RBC > 2.8 & pdcd_blood_V1_M_70$V1_RBC <= 4.2, "1", pdcd_blood_V1_M_70$V1_RBC_DEX)
pdcd_blood_V1_M_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_M_70$V1_RBC > 4.2,  "2", pdcd_blood_V1_M_70$V1_RBC_DEX)

max(pdcd_blood_V1_M_70$V1_HB)/4
pdcd_blood_V1_M_70$V1_HB_DEX <- "NA"
pdcd_blood_V1_M_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HB <= 4.2, "2", pdcd_blood_V1_M_70$V1_HB_DEX)
pdcd_blood_V1_M_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HB > 4.2 & pdcd_blood_V1_M_70$V1_HB <= 8.4, "1", pdcd_blood_V1_M_70$V1_HB_DEX)
pdcd_blood_V1_M_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HB > 8.4 & pdcd_blood_V1_M_70$V1_HB <= 12.6, "1", pdcd_blood_V1_M_70$V1_HB_DEX)
pdcd_blood_V1_M_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HB > 12.6,  "2", pdcd_blood_V1_M_70$V1_HB_DEX)

max(pdcd_blood_V1_M_70$V1_HCT)/4
pdcd_blood_V1_M_70$V1_HCT_DEX <- "NA"
pdcd_blood_V1_M_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HCT <= 12, "2", pdcd_blood_V1_M_70$V1_HCT_DEX)
pdcd_blood_V1_M_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HCT > 12 & pdcd_blood_V1_M_70$V1_HCT <= 24, "1", pdcd_blood_V1_M_70$V1_HCT_DEX)
pdcd_blood_V1_M_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HCT > 24 & pdcd_blood_V1_M_70$V1_HCT <= 36, "1", pdcd_blood_V1_M_70$V1_HCT_DEX)
pdcd_blood_V1_M_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HCT > 36,  "2", pdcd_blood_V1_M_70$V1_HCT_DEX)

max(pdcd_blood_V1_M_70$V1_HBA1C)/4
pdcd_blood_V1_M_70$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_M_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HBA1C <= 2.6, "2", pdcd_blood_V1_M_70$V1_HBA1C_DEX)
pdcd_blood_V1_M_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HBA1C > 2.6 & pdcd_blood_V1_M_70$V1_HBA1C <= 5.2, "1", pdcd_blood_V1_M_70$V1_HBA1C_DEX)
pdcd_blood_V1_M_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HBA1C > 5.2 & pdcd_blood_V1_M_70$V1_HBA1C <= 7.8, "1", pdcd_blood_V1_M_70$V1_HBA1C_DEX)
pdcd_blood_V1_M_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HBA1C > 7.8,  "2", pdcd_blood_V1_M_70$V1_HBA1C_DEX)

max(pdcd_blood_V1_M_70$V1_PLAT)/4
pdcd_blood_V1_M_70$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_M_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_PLAT <= 84, "2", pdcd_blood_V1_M_70$V1_PLAT_DEX)
pdcd_blood_V1_M_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_PLAT > 84 & pdcd_blood_V1_M_70$V1_PLAT <= 168, "1", pdcd_blood_V1_M_70$V1_PLAT_DEX)
pdcd_blood_V1_M_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_PLAT > 168 & pdcd_blood_V1_M_70$V1_PLAT <= 252, "1", pdcd_blood_V1_M_70$V1_PLAT_DEX)
pdcd_blood_V1_M_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_PLAT > 252,  "2", pdcd_blood_V1_M_70$V1_PLAT_DEX)

max(pdcd_blood_V1_M_70$V1_ALT)/4
pdcd_blood_V1_M_70$V1_ALT_DEX <- "NA"
pdcd_blood_V1_M_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_ALT <= 19, "2", pdcd_blood_V1_M_70$V1_ALT_DEX)
pdcd_blood_V1_M_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_ALT > 19 & pdcd_blood_V1_M_70$V1_ALT <= 38, "1", pdcd_blood_V1_M_70$V1_ALT_DEX)
pdcd_blood_V1_M_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_ALT > 38 & pdcd_blood_V1_M_70$V1_ALT <= 57, "1", pdcd_blood_V1_M_70$V1_ALT_DEX)
pdcd_blood_V1_M_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_M_70$V1_ALT > 57,  "2", pdcd_blood_V1_M_70$V1_ALT_DEX)

max(pdcd_blood_V1_M_70$V1_AST)/4
pdcd_blood_V1_M_70$V1_AST_DEX <- "NA"
pdcd_blood_V1_M_70$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_70$V1_AST <= 42, "2", pdcd_blood_V1_M_70$V1_AST_DEX)
pdcd_blood_V1_M_70$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_70$V1_AST > 42 & pdcd_blood_V1_M_70$V1_AST <= 84, "1", pdcd_blood_V1_M_70$V1_AST_DEX)
pdcd_blood_V1_M_70$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_70$V1_AST > 84 & pdcd_blood_V1_M_70$V1_AST <= 126, "1", pdcd_blood_V1_M_70$V1_AST_DEX)
pdcd_blood_V1_M_70$V1_AST_DEX <- ifelse(pdcd_blood_V1_M_70$V1_AST > 126,  "2", pdcd_blood_V1_M_70$V1_AST_DEX)

max(pdcd_blood_V1_M_70$V1_BUN)/4
pdcd_blood_V1_M_70$V1_BUN_DEX <- "NA"
pdcd_blood_V1_M_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_70$V1_BUN <= 7.8, "2", pdcd_blood_V1_M_70$V1_BUN_DEX)
pdcd_blood_V1_M_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_70$V1_BUN > 7.8 & pdcd_blood_V1_M_70$V1_BUN <= 15.6, "1", pdcd_blood_V1_M_70$V1_BUN_DEX)
pdcd_blood_V1_M_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_70$V1_BUN > 15.6 & pdcd_blood_V1_M_70$V1_BUN <= 23.4, "1", pdcd_blood_V1_M_70$V1_BUN_DEX)
pdcd_blood_V1_M_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_M_70$V1_BUN > 23.4,  "2", pdcd_blood_V1_M_70$V1_BUN_DEX)

max(pdcd_blood_V1_M_70$V1_CR)/4
pdcd_blood_V1_M_70$V1_CR_DEX <- "NA"
pdcd_blood_V1_M_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CR <= 0.44, "2", pdcd_blood_V1_M_70$V1_CR_DEX)
pdcd_blood_V1_M_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CR > 0.44 & pdcd_blood_V1_M_70$V1_CR <= 0.88, "1", pdcd_blood_V1_M_70$V1_CR_DEX)
pdcd_blood_V1_M_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CR > 0.77 & pdcd_blood_V1_M_70$V1_CR <= 1.32, "1", pdcd_blood_V1_M_70$V1_CR_DEX)
pdcd_blood_V1_M_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CR > 1.32,  "2", pdcd_blood_V1_M_70$V1_CR_DEX)

max(pdcd_blood_V1_M_70$V1_GLU)/4
pdcd_blood_V1_M_70$V1_GLU_DEX <- "NA"
pdcd_blood_V1_M_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_70$V1_GLU <= 60, "2", pdcd_blood_V1_M_70$V1_GLU_DEX)
pdcd_blood_V1_M_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_70$V1_GLU > 60 & pdcd_blood_V1_M_70$V1_GLU <= 120, "1", pdcd_blood_V1_M_70$V1_GLU_DEX)
pdcd_blood_V1_M_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_70$V1_GLU > 120 & pdcd_blood_V1_M_70$V1_GLU <= 180, "1", pdcd_blood_V1_M_70$V1_GLU_DEX)
pdcd_blood_V1_M_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_M_70$V1_GLU > 180,  "2", pdcd_blood_V1_M_70$V1_GLU_DEX)

max(pdcd_blood_V1_M_70$V1_CHOL)/4
pdcd_blood_V1_M_70$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_M_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CHOL <= 97, "2", pdcd_blood_V1_M_70$V1_CHOL_DEX)
pdcd_blood_V1_M_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CHOL > 97 & pdcd_blood_V1_M_70$V1_CHOL <= 194, "1", pdcd_blood_V1_M_70$V1_CHOL_DEX)
pdcd_blood_V1_M_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CHOL > 194 & pdcd_blood_V1_M_70$V1_CHOL <= 291, "1", pdcd_blood_V1_M_70$V1_CHOL_DEX)
pdcd_blood_V1_M_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_CHOL > 291,  "2", pdcd_blood_V1_M_70$V1_CHOL_DEX)

max(pdcd_blood_V1_M_70$V1_HDL)/4
pdcd_blood_V1_M_70$V1_HDL_DEX <- "NA"
pdcd_blood_V1_M_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HDL <= 20, "2", pdcd_blood_V1_M_70$V1_HDL_DEX)
pdcd_blood_V1_M_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HDL > 20 & pdcd_blood_V1_M_70$V1_HDL <= 40, "1", pdcd_blood_V1_M_70$V1_HDL_DEX)
pdcd_blood_V1_M_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HDL > 40 & pdcd_blood_V1_M_70$V1_HDL <= 60, "1", pdcd_blood_V1_M_70$V1_HDL_DEX)
pdcd_blood_V1_M_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_HDL > 60,  "2", pdcd_blood_V1_M_70$V1_HDL_DEX)

max(pdcd_blood_V1_M_70$V1_TG)/4
pdcd_blood_V1_M_70$V1_TG_DEX <- "NA"
pdcd_blood_V1_M_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_70$V1_TG <= 184, "2", pdcd_blood_V1_M_70$V1_TG_DEX)
pdcd_blood_V1_M_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_70$V1_TG > 184 & pdcd_blood_V1_M_70$V1_TG <= 368, "1", pdcd_blood_V1_M_70$V1_TG_DEX)
pdcd_blood_V1_M_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_70$V1_TG > 368 & pdcd_blood_V1_M_70$V1_TG <= 552, "1", pdcd_blood_V1_M_70$V1_TG_DEX)
pdcd_blood_V1_M_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_M_70$V1_TG > 552,  "2", pdcd_blood_V1_M_70$V1_TG_DEX)

max(pdcd_blood_V1_M_70$V1_LDL)/4
pdcd_blood_V1_M_70$V1_LDL_DEX <- "NA"
pdcd_blood_V1_M_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_LDL <= 68, "2", pdcd_blood_V1_M_70$V1_LDL_DEX)
pdcd_blood_V1_M_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_LDL > 68 & pdcd_blood_V1_M_70$V1_LDL <= 136, "1", pdcd_blood_V1_M_70$V1_LDL_DEX)
pdcd_blood_V1_M_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_LDL > 136 & pdcd_blood_V1_M_70$V1_LDL <= 204, "1", pdcd_blood_V1_M_70$V1_LDL_DEX)
pdcd_blood_V1_M_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_M_70$V1_LDL > 204,  "2", pdcd_blood_V1_M_70$V1_LDL_DEX)

#V1_여성_70대_혈액검사
max(pdcd_blood_V1_W_70$V1_WBC)/4
pdcd_blood_V1_W_70$V1_WBC_DEX <- "NA"
pdcd_blood_V1_W_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_WBC <= 3.6, "2", pdcd_blood_V1_W_70$V1_WBC_DEX)
pdcd_blood_V1_W_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_WBC > 3.6 & pdcd_blood_V1_W_70$V1_WBC <= 7.2, "1", pdcd_blood_V1_W_70$V1_WBC_DEX)
pdcd_blood_V1_W_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_WBC > 7.2 & pdcd_blood_V1_W_70$V1_WBC <= 10.8, "1", pdcd_blood_V1_W_70$V1_WBC_DEX)
pdcd_blood_V1_W_70$V1_WBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_WBC > 10.8,  "2", pdcd_blood_V1_W_70$V1_WBC_DEX)

max(pdcd_blood_V1_W_70$V1_RBC)/4
pdcd_blood_V1_W_70$V1_RBC_DEX <- "NA"
pdcd_blood_V1_W_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_RBC <= 1.3, "2", pdcd_blood_V1_W_70$V1_RBC_DEX)
pdcd_blood_V1_W_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_RBC > 1.3 & pdcd_blood_V1_W_70$V1_RBC <= 2.6, "1", pdcd_blood_V1_W_70$V1_RBC_DEX)
pdcd_blood_V1_W_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_RBC > 2.6 & pdcd_blood_V1_W_70$V1_RBC <= 3.9, "1", pdcd_blood_V1_W_70$V1_RBC_DEX)
pdcd_blood_V1_W_70$V1_RBC_DEX <- ifelse(pdcd_blood_V1_W_70$V1_RBC > 3.9,  "2", pdcd_blood_V1_W_70$V1_RBC_DEX)

max(pdcd_blood_V1_W_70$V1_HB)/4
pdcd_blood_V1_W_70$V1_HB_DEX <- "NA"
pdcd_blood_V1_W_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HB <= 3.6, "2", pdcd_blood_V1_W_70$V1_HB_DEX)
pdcd_blood_V1_W_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HB > 3.6 & pdcd_blood_V1_W_70$V1_HB <= 7.2, "1", pdcd_blood_V1_W_70$V1_HB_DEX)
pdcd_blood_V1_W_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HB > 7.2 & pdcd_blood_V1_W_70$V1_HB <= 10.8, "1", pdcd_blood_V1_W_70$V1_HB_DEX)
pdcd_blood_V1_W_70$V1_HB_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HB > 10.8,  "2", pdcd_blood_V1_W_70$V1_HB_DEX)

max(pdcd_blood_V1_W_70$V1_HCT)/4
pdcd_blood_V1_W_70$V1_HCT_DEX <- "NA"
pdcd_blood_V1_W_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HCT <= 11, "2", pdcd_blood_V1_W_70$V1_HCT_DEX)
pdcd_blood_V1_W_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HCT > 11 & pdcd_blood_V1_W_70$V1_HCT <= 22, "1", pdcd_blood_V1_W_70$V1_HCT_DEX)
pdcd_blood_V1_W_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HCT > 22 & pdcd_blood_V1_W_70$V1_HCT <= 33, "1", pdcd_blood_V1_W_70$V1_HCT_DEX)
pdcd_blood_V1_W_70$V1_HCT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HCT > 33,  "2", pdcd_blood_V1_W_70$V1_HCT_DEX)

max(pdcd_blood_V1_W_70$V1_HBA1C)/4
pdcd_blood_V1_W_70$V1_HBA1C_DEX <- "NA"
pdcd_blood_V1_W_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HBA1C <= 2.3, "2", pdcd_blood_V1_W_70$V1_HBA1C_DEX)
pdcd_blood_V1_W_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HBA1C > 2.3 & pdcd_blood_V1_W_70$V1_HBA1C <= 4.6, "1", pdcd_blood_V1_W_70$V1_HBA1C_DEX)
pdcd_blood_V1_W_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HBA1C > 4.6 & pdcd_blood_V1_W_70$V1_HBA1C <= 6.9, "1", pdcd_blood_V1_W_70$V1_HBA1C_DEX)
pdcd_blood_V1_W_70$V1_HBA1C_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HBA1C > 6.9,  "2", pdcd_blood_V1_W_70$V1_HBA1C_DEX)

max(pdcd_blood_V1_W_70$V1_PLAT)/4
pdcd_blood_V1_W_70$V1_PLAT_DEX <- "NA"
pdcd_blood_V1_W_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_PLAT <= 109, "2", pdcd_blood_V1_W_70$V1_PLAT_DEX)
pdcd_blood_V1_W_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_PLAT > 109 & pdcd_blood_V1_W_70$V1_PLAT <= 218, "1", pdcd_blood_V1_W_70$V1_PLAT_DEX)
pdcd_blood_V1_W_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_PLAT > 218 & pdcd_blood_V1_W_70$V1_PLAT <= 327, "1", pdcd_blood_V1_W_70$V1_PLAT_DEX)
pdcd_blood_V1_W_70$V1_PLAT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_PLAT > 327,  "2", pdcd_blood_V1_W_70$V1_PLAT_DEX)

max(pdcd_blood_V1_W_70$V1_ALT)/4
pdcd_blood_V1_W_70$V1_ALT_DEX <- "NA"
pdcd_blood_V1_W_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_ALT <= 8.2, "2", pdcd_blood_V1_W_70$V1_ALT_DEX)
pdcd_blood_V1_W_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_ALT > 8.2 & pdcd_blood_V1_W_70$V1_ALT <= 16.4, "1", pdcd_blood_V1_W_70$V1_ALT_DEX)
pdcd_blood_V1_W_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_ALT > 16.4 & pdcd_blood_V1_W_70$V1_ALT <= 24.6, "1", pdcd_blood_V1_W_70$V1_ALT_DEX)
pdcd_blood_V1_W_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_ALT > 24.6,  "2", pdcd_blood_V1_W_70$V1_ALT_DEX)

max(pdcd_blood_V1_W_70$V1_AST)/4
pdcd_blood_V1_W_70$V1_AST_DEX <- "NA"
pdcd_blood_V1_W_70$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_70$V1_AST <= 11, "2", pdcd_blood_V1_W_70$V1_AST_DEX)
pdcd_blood_V1_W_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_ALT > 11 & pdcd_blood_V1_W_70$V1_ALT <= 22, "1", pdcd_blood_V1_W_70$V1_ALT_DEX)
pdcd_blood_V1_W_70$V1_ALT_DEX <- ifelse(pdcd_blood_V1_W_70$V1_ALT > 33 & pdcd_blood_V1_W_70$V1_ALT <= 33, "1", pdcd_blood_V1_W_70$V1_ALT_DEX)
pdcd_blood_V1_W_70$V1_AST_DEX <- ifelse(pdcd_blood_V1_W_70$V1_AST > 33,  "2", pdcd_blood_V1_W_70$V1_AST_DEX)

max(pdcd_blood_V1_W_70$V1_BUN)/4
pdcd_blood_V1_W_70$V1_BUN_DEX <- "NA"
pdcd_blood_V1_W_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_70$V1_BUN <= 7.2, "2", pdcd_blood_V1_W_70$V1_BUN_DEX)
pdcd_blood_V1_W_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_70$V1_BUN > 7.2 & pdcd_blood_V1_W_70$V1_BUN <= 14.4, "1", pdcd_blood_V1_W_70$V1_BUN_DEX)
pdcd_blood_V1_W_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_70$V1_BUN > 14.4 & pdcd_blood_V1_W_70$V1_BUN <= 21.6, "1", pdcd_blood_V1_W_70$V1_BUN_DEX)
pdcd_blood_V1_W_70$V1_BUN_DEX <- ifelse(pdcd_blood_V1_W_70$V1_BUN > 21.6,  "2", pdcd_blood_V1_W_70$V1_BUN_DEX)

max(pdcd_blood_V1_W_70$V1_CR)/4
pdcd_blood_V1_W_70$V1_CR_DEX <- "NA"
pdcd_blood_V1_W_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CR <= 0.29, "2", pdcd_blood_V1_W_70$V1_CR_DEX)
pdcd_blood_V1_W_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CR > 0.29 & pdcd_blood_V1_W_70$V1_CR <= 0.58, "1", pdcd_blood_V1_W_70$V1_CR_DEX)
pdcd_blood_V1_W_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CR > 0.58 & pdcd_blood_V1_W_70$V1_CR <= 0.87, "1", pdcd_blood_V1_W_70$V1_CR_DEX)
pdcd_blood_V1_W_70$V1_CR_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CR > 0.87,  "2", pdcd_blood_V1_W_70$V1_CR_DEX)

max(pdcd_blood_V1_W_70$V1_GLU)/4
pdcd_blood_V1_W_70$V1_GLU_DEX <- "NA"
pdcd_blood_V1_W_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_70$V1_GLU <= 42, "2", pdcd_blood_V1_W_70$V1_GLU_DEX)
pdcd_blood_V1_W_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_70$V1_GLU > 42 & pdcd_blood_V1_W_70$V1_GLU <= 84, "1", pdcd_blood_V1_W_70$V1_GLU_DEX)
pdcd_blood_V1_W_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_70$V1_GLU > 84 & pdcd_blood_V1_W_70$V1_GLU <= 126, "1", pdcd_blood_V1_W_70$V1_GLU_DEX)
pdcd_blood_V1_W_70$V1_GLU_DEX <- ifelse(pdcd_blood_V1_W_70$V1_GLU > 126,  "2", pdcd_blood_V1_W_70$V1_GLU_DEX)

max(pdcd_blood_V1_W_70$V1_CHOL)/4
pdcd_blood_V1_W_70$V1_CHOL_DEX <- "NA"
pdcd_blood_V1_W_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CHOL <= 75, "2", pdcd_blood_V1_W_70$V1_CHOL_DEX)
pdcd_blood_V1_W_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CHOL > 75 & pdcd_blood_V1_W_70$V1_CHOL <= 150, "1", pdcd_blood_V1_W_70$V1_CHOL_DEX)
pdcd_blood_V1_W_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CHOL > 150 & pdcd_blood_V1_W_70$V1_CHOL <= 225, "1", pdcd_blood_V1_W_70$V1_CHOL_DEX)
pdcd_blood_V1_W_70$V1_CHOL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_CHOL > 225,  "2", pdcd_blood_V1_W_70$V1_CHOL_DEX)

max(pdcd_blood_V1_W_70$V1_HDL)/4
pdcd_blood_V1_W_70$V1_HDL_DEX <- "NA"
pdcd_blood_V1_W_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HDL <= 19, "2", pdcd_blood_V1_W_70$V1_HDL_DEX)
pdcd_blood_V1_W_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HDL > 19 & pdcd_blood_V1_W_70$V1_HDL <= 38, "1", pdcd_blood_V1_W_70$V1_HDL_DEX)
pdcd_blood_V1_W_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HDL > 38 & pdcd_blood_V1_W_70$V1_HDL <= 57, "1", pdcd_blood_V1_W_70$V1_HDL_DEX)
pdcd_blood_V1_W_70$V1_HDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_HDL > 57,  "2", pdcd_blood_V1_W_70$V1_HDL_DEX)

max(pdcd_blood_V1_W_70$V1_TG)/4
pdcd_blood_V1_W_70$V1_TG_DEX <- "NA"
pdcd_blood_V1_W_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_70$V1_TG <= 99, "2", pdcd_blood_V1_W_70$V1_TG_DEX)
pdcd_blood_V1_W_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_70$V1_TG > 99 & pdcd_blood_V1_W_70$V1_TG <= 198, "1", pdcd_blood_V1_W_70$V1_TG_DEX)
pdcd_blood_V1_W_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_70$V1_TG > 198 & pdcd_blood_V1_W_70$V1_TG <= 297, "1", pdcd_blood_V1_W_70$V1_TG_DEX)
pdcd_blood_V1_W_70$V1_TG_DEX <- ifelse(pdcd_blood_V1_W_70$V1_TG > 297,  "2", pdcd_blood_V1_W_70$V1_TG_DEX)

max(pdcd_blood_V1_W_70$V1_LDL)/4
pdcd_blood_V1_W_70$V1_LDL_DEX <- "NA"
pdcd_blood_V1_W_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_LDL <= 56, "2", pdcd_blood_V1_W_70$V1_LDL_DEX)
pdcd_blood_V1_W_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_LDL > 56 & pdcd_blood_V1_W_70$V1_LDL <= 112, "1", pdcd_blood_V1_W_70$V1_LDL_DEX)
pdcd_blood_V1_W_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_LDL > 112 & pdcd_blood_V1_W_70$V1_LDL <= 168, "1", pdcd_blood_V1_W_70$V1_LDL_DEX)
pdcd_blood_V1_W_70$V1_LDL_DEX <- ifelse(pdcd_blood_V1_W_70$V1_LDL > 168,  "2", pdcd_blood_V1_W_70$V1_LDL_DEX)

#V1_남성_40대_소변검사(6개 군집)
max(pdcd_urine_V1_M_40$V1_UPH)/12
pdcd_urine_V1_M_40$V1_UPH_DEX <- "NA"
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH <= 0.67, "6", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 0.67 & pdcd_urine_V1_M_40$V1_UPH <= 1.34, "5", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 1.34 & pdcd_urine_V1_M_40$V1_UPH <= 2.01, "4", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 2.01 & pdcd_urine_V1_M_40$V1_UPH <= 2.68, "3", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 2.68 & pdcd_urine_V1_M_40$V1_UPH <= 3.35, "2", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 3.35 & pdcd_urine_V1_M_40$V1_UPH <= 4.02, "1", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 4.02 & pdcd_urine_V1_M_40$V1_UPH <= 4.69, "1", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 4.69 & pdcd_urine_V1_M_40$V1_UPH <= 5.36, "2", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 5.36 & pdcd_urine_V1_M_40$V1_UPH <= 6.03, "3", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 6.03 & pdcd_urine_V1_M_40$V1_UPH <= 6.7, "4", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 6.7 & pdcd_urine_V1_M_40$V1_UPH <= 7.37, "5", pdcd_urine_V1_M_40$V1_UPH_DEX)
pdcd_urine_V1_M_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_40$V1_UPH > 7.37,  "6", pdcd_urine_V1_M_40$V1_UPH_DEX)

#V1_여성_40대_소변검사(6개 군집)
max(pdcd_urine_V1_W_40$V1_UPH)/12
pdcd_urine_V1_W_40$V1_UPH_DEX <- "NA"
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH <= 0.67, "6", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 0.67 & pdcd_urine_V1_W_40$V1_UPH <= 1.34, "5", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 1.34 & pdcd_urine_V1_W_40$V1_UPH <= 2.01, "4", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 2.01 & pdcd_urine_V1_W_40$V1_UPH <= 2.68, "3", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 2.68 & pdcd_urine_V1_W_40$V1_UPH <= 3.35, "2", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 3.35 & pdcd_urine_V1_W_40$V1_UPH <= 4.02, "1", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 4.02 & pdcd_urine_V1_W_40$V1_UPH <= 4.69, "1", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 4.69 & pdcd_urine_V1_W_40$V1_UPH <= 5.36, "2", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 5.36 & pdcd_urine_V1_W_40$V1_UPH <= 6.03, "3", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 6.03 & pdcd_urine_V1_W_40$V1_UPH <= 6.7, "4", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 6.7 & pdcd_urine_V1_W_40$V1_UPH <= 7.37, "5", pdcd_urine_V1_W_40$V1_UPH_DEX)
pdcd_urine_V1_W_40$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_40$V1_UPH > 7.37,  "6", pdcd_urine_V1_W_40$V1_UPH_DEX)

#V1_남성_50대_소변검사(6개 군집)
max(pdcd_urine_V1_M_50$V1_UPH)/12
pdcd_urine_V1_M_50$V1_UPH_DEX <- "NA"
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH <= 0.67, "6", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 0.67 & pdcd_urine_V1_M_50$V1_UPH <= 1.34, "5", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 1.34 & pdcd_urine_V1_M_50$V1_UPH <= 2.01, "4", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 2.01 & pdcd_urine_V1_M_50$V1_UPH <= 2.68, "3", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 2.68 & pdcd_urine_V1_M_50$V1_UPH <= 3.35, "2", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 3.35 & pdcd_urine_V1_M_50$V1_UPH <= 4.02, "1", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 4.02 & pdcd_urine_V1_M_50$V1_UPH <= 4.69, "1", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 4.69 & pdcd_urine_V1_M_50$V1_UPH <= 5.36, "2", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 5.36 & pdcd_urine_V1_M_50$V1_UPH <= 6.03, "3", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 6.03 & pdcd_urine_V1_M_50$V1_UPH <= 6.7, "4", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 6.7 & pdcd_urine_V1_M_50$V1_UPH <= 7.37, "5", pdcd_urine_V1_M_50$V1_UPH_DEX)
pdcd_urine_V1_M_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_50$V1_UPH > 7.37,  "6", pdcd_urine_V1_M_50$V1_UPH_DEX)

#V1_여성_50대_소변검사(6개 군집)
max(pdcd_urine_V1_W_50$V1_UPH)/12
pdcd_urine_V1_W_50$V1_UPH_DEX <- "NA"
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH <= 0.67, "6", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 0.67 & pdcd_urine_V1_W_50$V1_UPH <= 1.34, "5", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 1.34 & pdcd_urine_V1_W_50$V1_UPH <= 2.01, "4", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 2.01 & pdcd_urine_V1_W_50$V1_UPH <= 2.68, "3", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 2.68 & pdcd_urine_V1_W_50$V1_UPH <= 3.35, "2", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 3.35 & pdcd_urine_V1_W_50$V1_UPH <= 4.02, "1", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 4.02 & pdcd_urine_V1_W_50$V1_UPH <= 4.69, "1", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 4.69 & pdcd_urine_V1_W_50$V1_UPH <= 5.36, "2", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 5.36 & pdcd_urine_V1_W_50$V1_UPH <= 6.03, "3", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 6.03 & pdcd_urine_V1_W_50$V1_UPH <= 6.7, "4", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 6.7 & pdcd_urine_V1_W_50$V1_UPH <= 7.37, "5", pdcd_urine_V1_W_50$V1_UPH_DEX)
pdcd_urine_V1_W_50$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_50$V1_UPH > 7.37,  "6", pdcd_urine_V1_W_50$V1_UPH_DEX)

#V1_남성_60대_소변검사(6개 군집)
max(pdcd_urine_V1_M_60$V1_UPH)/12
pdcd_urine_V1_M_60$V1_UPH_DEX <- "NA"
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH <= 0.67, "6", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 0.67 & pdcd_urine_V1_M_60$V1_UPH <= 1.34, "5", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 1.34 & pdcd_urine_V1_M_60$V1_UPH <= 2.01, "4", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 2.01 & pdcd_urine_V1_M_60$V1_UPH <= 2.68, "3", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 2.68 & pdcd_urine_V1_M_60$V1_UPH <= 3.35, "2", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 3.35 & pdcd_urine_V1_M_60$V1_UPH <= 4.02, "1", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 4.02 & pdcd_urine_V1_M_60$V1_UPH <= 4.69, "1", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 4.69 & pdcd_urine_V1_M_60$V1_UPH <= 5.36, "2", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 5.36 & pdcd_urine_V1_M_60$V1_UPH <= 6.03, "3", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 6.03 & pdcd_urine_V1_M_60$V1_UPH <= 6.7, "4", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 6.7 & pdcd_urine_V1_M_60$V1_UPH <= 7.37, "5", pdcd_urine_V1_M_60$V1_UPH_DEX)
pdcd_urine_V1_M_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_60$V1_UPH > 7.37,  "6", pdcd_urine_V1_M_60$V1_UPH_DEX)

#V1_여성_60대_소변검사(6개 군집)
max(pdcd_urine_V1_W_60$V1_UPH)/12
pdcd_urine_V1_W_60$V1_UPH_DEX <- "NA"
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH <= 0.75, "6", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 0.75 & pdcd_urine_V1_W_60$V1_UPH <= 1.5, "5", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 1.5 & pdcd_urine_V1_W_60$V1_UPH <= 2.25, "4", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 2.25 & pdcd_urine_V1_W_60$V1_UPH <= 3, "3", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 3 & pdcd_urine_V1_W_60$V1_UPH <= 3.75, "2", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 3.75 & pdcd_urine_V1_W_60$V1_UPH <= 4.5, "1", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 4.5 & pdcd_urine_V1_W_60$V1_UPH <= 5.25, "1", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 5.25 & pdcd_urine_V1_W_60$V1_UPH <= 6, "2", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 6 & pdcd_urine_V1_W_60$V1_UPH <= 6.75, "3", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 6.75 & pdcd_urine_V1_W_60$V1_UPH <= 7.5, "4", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 7.5 & pdcd_urine_V1_W_60$V1_UPH <= 8.25, "5", pdcd_urine_V1_W_60$V1_UPH_DEX)
pdcd_urine_V1_W_60$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_60$V1_UPH > 8.25,  "6", pdcd_urine_V1_W_60$V1_UPH_DEX)

#V1_남성_70대_소변검사(6개 군집)
max(pdcd_urine_V1_M_70$V1_UPH)/12
pdcd_urine_V1_M_70$V1_UPH_DEX <- "NA"
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH <= 0.67, "6", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 0.67 & pdcd_urine_V1_M_70$V1_UPH <= 1.34, "5", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 1.34 & pdcd_urine_V1_M_70$V1_UPH <= 2.01, "4", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 2.01 & pdcd_urine_V1_M_70$V1_UPH <= 2.68, "3", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 2.68 & pdcd_urine_V1_M_70$V1_UPH <= 3.35, "2", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 3.35 & pdcd_urine_V1_M_70$V1_UPH <= 4.02, "1", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 4.02 & pdcd_urine_V1_M_70$V1_UPH <= 4.69, "1", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 4.69 & pdcd_urine_V1_M_70$V1_UPH <= 5.36, "2", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 5.36 & pdcd_urine_V1_M_70$V1_UPH <= 6.03, "3", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 6.03 & pdcd_urine_V1_M_70$V1_UPH <= 6.7, "4", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 6.7 & pdcd_urine_V1_M_70$V1_UPH <= 7.37, "5", pdcd_urine_V1_M_70$V1_UPH_DEX)
pdcd_urine_V1_M_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_M_70$V1_UPH > 7.37,  "6", pdcd_urine_V1_M_70$V1_UPH_DEX)

#V1_여성_70대_소변검사(6개 군집)
max(pdcd_urine_V1_W_70$V1_UPH)/12
pdcd_urine_V1_W_70$V1_UPH_DEX <- "NA"
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH <= 0.67, "6", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 0.67 & pdcd_urine_V1_W_70$V1_UPH <= 1.34, "5", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 1.34 & pdcd_urine_V1_W_70$V1_UPH <= 2.01, "4", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 2.01 & pdcd_urine_V1_W_70$V1_UPH <= 2.68, "3", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 2.68 & pdcd_urine_V1_W_70$V1_UPH <= 3.35, "2", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 3.35 & pdcd_urine_V1_W_70$V1_UPH <= 4.02, "1", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 4.02 & pdcd_urine_V1_W_70$V1_UPH <= 4.69, "1", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 4.69 & pdcd_urine_V1_W_70$V1_UPH <= 5.36, "2", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 5.36 & pdcd_urine_V1_W_70$V1_UPH <= 6.03, "3", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 6.03 & pdcd_urine_V1_W_70$V1_UPH <= 6.7, "4", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 6.7 & pdcd_urine_V1_W_70$V1_UPH <= 7.37, "5", pdcd_urine_V1_W_70$V1_UPH_DEX)
pdcd_urine_V1_W_70$V1_UPH_DEX <- ifelse(pdcd_urine_V1_W_70$V1_UPH > 7.37,  "6", pdcd_urine_V1_W_70$V1_UPH_DEX)

#V1_남성_40대_신체계측검사(2개 군집)
max(pdcd_body_V1_M_40$V1_SBP)/4
pdcd_body_V1_M_40$V1_SBP_DEX <- "NA"
pdcd_body_V1_M_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_SBP <= 46, "2", pdcd_body_V1_M_40$V1_SBP_DEX)
pdcd_body_V1_M_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_SBP > 46 & pdcd_body_V1_M_40$V1_SBP <= 92, "1",pdcd_body_V1_M_40$V1_SBP_DEX)
pdcd_body_V1_M_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_SBP > 92 & pdcd_body_V1_M_40$V1_SBP <= 138, "1",pdcd_body_V1_M_40$V1_SBP_DEX)
pdcd_body_V1_M_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_SBP > 138,  "2", pdcd_body_V1_M_40$V1_SBP_DEX)

max(pdcd_body_V1_M_40$V1_DBP)/4
pdcd_body_V1_M_40$V1_DBP_DEX <- "NA"
pdcd_body_V1_M_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_DBP <= 35, "2", pdcd_body_V1_M_40$V1_DBP_DEX)
pdcd_body_V1_M_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_DBP > 35 & pdcd_body_V1_M_40$V1_DBP <= 70, "1",pdcd_body_V1_M_40$V1_DBP_DEX)
pdcd_body_V1_M_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_DBP > 70 & pdcd_body_V1_M_40$V1_DBP <= 105, "1",pdcd_body_V1_M_40$V1_DBP_DEX)
pdcd_body_V1_M_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_40$V1_DBP > 105,  "2", pdcd_body_V1_M_40$V1_DBP_DEX)

max(pdcd_body_V1_M_40$V1_WAIST)/4
pdcd_body_V1_M_40$V1_WAIST_DEX <- "NA"
pdcd_body_V1_M_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_40$V1_WAIST <= 30, "2", pdcd_body_V1_M_40$V1_WAIST_DEX)
pdcd_body_V1_M_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_40$V1_WAIST > 30 & pdcd_body_V1_M_40$V1_WAIST <= 60, "1",pdcd_body_V1_M_40$V1_WAIST_DEX)
pdcd_body_V1_M_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_40$V1_WAIST > 60 & pdcd_body_V1_M_40$V1_WAIST <= 90, "1",pdcd_body_V1_M_40$V1_WAIST_DEX)
pdcd_body_V1_M_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_40$V1_WAIST > 90,  "2", pdcd_body_V1_M_40$V1_WAIST_DEX)

max(pdcd_body_V1_M_40$V1_BMI)/4
pdcd_body_V1_M_40$V1_BMI_DEX <- "NA"
pdcd_body_V1_M_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_40$V1_BMI <= 10, "2", pdcd_body_V1_M_40$V1_BMI_DEX)
pdcd_body_V1_M_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_40$V1_BMI > 10 & pdcd_body_V1_M_40$V1_BMI <= 20, "1",pdcd_body_V1_M_40$V1_BMI_DEX)
pdcd_body_V1_M_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_40$V1_BMI > 20 & pdcd_body_V1_M_40$V1_BMI <= 30, "1",pdcd_body_V1_M_40$V1_BMI_DEX)
pdcd_body_V1_M_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_40$V1_BMI > 30,  "2",pdcd_body_V1_M_40$V1_BMI_DEX)

#V1_여성_40대_신체계측검사(2개 군집)
max(pdcd_body_V1_W_40$V1_SBP)/4
pdcd_body_V1_W_40$V1_SBP_DEX <- "NA"
pdcd_body_V1_W_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_SBP <= 51, "2", pdcd_body_V1_W_40$V1_SBP_DEX)
pdcd_body_V1_W_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_SBP > 51 & pdcd_body_V1_W_40$V1_SBP <= 102, "1",pdcd_body_V1_W_40$V1_SBP_DEX)
pdcd_body_V1_W_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_SBP > 102 & pdcd_body_V1_W_40$V1_SBP <= 153, "1",pdcd_body_V1_W_40$V1_SBP_DEX)
pdcd_body_V1_W_40$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_SBP > 153,  "2", pdcd_body_V1_W_40$V1_SBP_DEX)

max(pdcd_body_V1_W_40$V1_DBP)/4
pdcd_body_V1_W_40$V1_DBP_DEX <- "NA"
pdcd_body_V1_W_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_DBP <= 34, "2", pdcd_body_V1_W_40$V1_DBP_DEX)
pdcd_body_V1_W_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_DBP > 34 & pdcd_body_V1_W_40$V1_DBP <= 68, "1",pdcd_body_V1_W_40$V1_DBP_DEX)
pdcd_body_V1_W_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_DBP > 68 & pdcd_body_V1_W_40$V1_DBP <= 102, "1",pdcd_body_V1_W_40$V1_DBP_DEX)
pdcd_body_V1_W_40$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_40$V1_DBP > 102,  "2", pdcd_body_V1_W_40$V1_DBP_DEX)

max(pdcd_body_V1_W_40$V1_WAIST)/4
pdcd_body_V1_W_40$V1_WAIST_DEX <- "NA"
pdcd_body_V1_W_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_40$V1_WAIST <= 28, "2", pdcd_body_V1_W_40$V1_WAIST_DEX)
pdcd_body_V1_W_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_40$V1_WAIST > 28 & pdcd_body_V1_W_40$V1_WAIST <= 56, "1",pdcd_body_V1_W_40$V1_WAIST_DEX)
pdcd_body_V1_W_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_40$V1_WAIST > 56 & pdcd_body_V1_W_40$V1_WAIST <= 84, "1",pdcd_body_V1_W_40$V1_WAIST_DEX)
pdcd_body_V1_W_40$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_40$V1_WAIST > 84,  "2", pdcd_body_V1_W_40$V1_WAIST_DEX)

max(pdcd_body_V1_W_40$V1_BMI)/4
pdcd_body_V1_W_40$V1_BMI_DEX <- "NA"
pdcd_body_V1_W_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_40$V1_BMI <= 9.6, "2", pdcd_body_V1_W_40$V1_BMI_DEX)
pdcd_body_V1_W_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_40$V1_BMI > 9.6 & pdcd_body_V1_W_40$V1_BMI <= 19.2, "1",pdcd_body_V1_W_40$V1_BMI_DEX)
pdcd_body_V1_W_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_40$V1_BMI > 19.2 & pdcd_body_V1_W_40$V1_BMI <= 28.8, "1",pdcd_body_V1_W_40$V1_BMI_DEX)
pdcd_body_V1_W_40$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_40$V1_BMI > 28.8,  "2",pdcd_body_V1_W_40$V1_BMI_DEX)

#V1_남성_50대_신체계측검사(2개 군집)
max(pdcd_body_V1_M_50$V1_SBP)/4
pdcd_body_V1_M_50$V1_SBP_DEX <- "NA"
pdcd_body_V1_M_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_SBP <= 48, "2", pdcd_body_V1_M_50$V1_SBP_DEX)
pdcd_body_V1_M_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_SBP > 48 & pdcd_body_V1_M_50$V1_SBP <= 96, "1",pdcd_body_V1_M_50$V1_SBP_DEX)
pdcd_body_V1_M_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_SBP > 96 & pdcd_body_V1_M_50$V1_SBP <= 144, "1",pdcd_body_V1_M_50$V1_SBP_DEX)
pdcd_body_V1_M_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_SBP > 144,  "2", pdcd_body_V1_M_50$V1_SBP_DEX)

max(pdcd_body_V1_M_50$V1_DBP)/4
pdcd_body_V1_M_50$V1_DBP_DEX <- "NA"
pdcd_body_V1_M_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_DBP <= 34, "2", pdcd_body_V1_M_50$V1_DBP_DEX)
pdcd_body_V1_M_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_DBP > 34 & pdcd_body_V1_M_50$V1_DBP <= 68, "1",pdcd_body_V1_M_50$V1_DBP_DEX)
pdcd_body_V1_M_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_DBP > 68 & pdcd_body_V1_M_50$V1_DBP <= 102, "1",pdcd_body_V1_M_50$V1_DBP_DEX)
pdcd_body_V1_M_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_50$V1_DBP > 102,  "2", pdcd_body_V1_M_50$V1_DBP_DEX)

max(pdcd_body_V1_M_50$V1_WAIST)/4
pdcd_body_V1_M_50$V1_WAIST_DEX <- "NA"
pdcd_body_V1_M_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_50$V1_WAIST <= 29, "2", pdcd_body_V1_M_50$V1_WAIST_DEX)
pdcd_body_V1_M_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_50$V1_WAIST > 29 & pdcd_body_V1_M_50$V1_WAIST <= 58, "1",pdcd_body_V1_M_50$V1_WAIST_DEX)
pdcd_body_V1_M_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_50$V1_WAIST > 58 & pdcd_body_V1_M_50$V1_WAIST <= 87, "1",pdcd_body_V1_M_50$V1_WAIST_DEX)
pdcd_body_V1_M_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_50$V1_WAIST > 87,  "2", pdcd_body_V1_M_50$V1_WAIST_DEX)

max(pdcd_body_V1_M_50$V1_BMI)/4
pdcd_body_V1_M_50$V1_BMI_DEX <- "NA"
pdcd_body_V1_M_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_50$V1_BMI <= 8.8, "2", pdcd_body_V1_M_50$V1_BMI_DEX)
pdcd_body_V1_M_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_50$V1_BMI > 8.8 & pdcd_body_V1_M_50$V1_BMI <= 17.6, "1",pdcd_body_V1_M_50$V1_BMI_DEX)
pdcd_body_V1_M_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_50$V1_BMI > 17.6 & pdcd_body_V1_M_50$V1_BMI <= 26.4, "1",pdcd_body_V1_M_50$V1_BMI_DEX)
pdcd_body_V1_M_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_50$V1_BMI > 26.4,  "2",pdcd_body_V1_M_50$V1_BMI_DEX)

#V1_여성_50대_신체계측검사(2개 군집)
max(pdcd_body_V1_W_50$V1_SBP)/4
pdcd_body_V1_W_50$V1_SBP_DEX <- "NA"
pdcd_body_V1_W_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_SBP <= 51, "2", pdcd_body_V1_W_50$V1_SBP_DEX)
pdcd_body_V1_W_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_SBP > 51 & pdcd_body_V1_W_50$V1_SBP <= 102, "1",pdcd_body_V1_W_50$V1_SBP_DEX)
pdcd_body_V1_W_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_SBP > 102 & pdcd_body_V1_W_50$V1_SBP <= 153, "1",pdcd_body_V1_W_50$V1_SBP_DEX)
pdcd_body_V1_W_50$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_SBP > 153,  "2", pdcd_body_V1_W_50$V1_SBP_DEX)

max(pdcd_body_V1_W_50$V1_DBP)/4
pdcd_body_V1_W_50$V1_DBP_DEX <- "NA"
pdcd_body_V1_W_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_DBP <= 34, "2", pdcd_body_V1_W_50$V1_DBP_DEX)
pdcd_body_V1_W_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_DBP > 34 & pdcd_body_V1_W_50$V1_DBP <= 68, "1",pdcd_body_V1_W_50$V1_DBP_DEX)
pdcd_body_V1_W_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_DBP > 68 & pdcd_body_V1_W_50$V1_DBP <= 102, "1",pdcd_body_V1_W_50$V1_DBP_DEX)
pdcd_body_V1_W_50$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_50$V1_DBP > 102,  "2", pdcd_body_V1_W_50$V1_DBP_DEX)

max(pdcd_body_V1_W_50$V1_WAIST)/4
pdcd_body_V1_W_50$V1_WAIST_DEX <- "NA"
pdcd_body_V1_W_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_50$V1_WAIST <= 30, "2", pdcd_body_V1_W_50$V1_WAIST_DEX)
pdcd_body_V1_W_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_50$V1_WAIST > 30 & pdcd_body_V1_W_50$V1_WAIST <= 60, "1",pdcd_body_V1_W_50$V1_WAIST_DEX)
pdcd_body_V1_W_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_50$V1_WAIST > 60 & pdcd_body_V1_W_50$V1_WAIST <= 90, "1",pdcd_body_V1_W_50$V1_WAIST_DEX)
pdcd_body_V1_W_50$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_50$V1_WAIST > 90,  "2", pdcd_body_V1_W_50$V1_WAIST_DEX)

max(pdcd_body_V1_W_50$V1_BMI)/4
pdcd_body_V1_W_50$V1_BMI_DEX <- "NA"
pdcd_body_V1_W_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_50$V1_BMI <= 11, "2", pdcd_body_V1_W_50$V1_BMI_DEX)
pdcd_body_V1_W_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_50$V1_BMI > 11 & pdcd_body_V1_W_50$V1_BMI <= 22, "1",pdcd_body_V1_W_50$V1_BMI_DEX)
pdcd_body_V1_W_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_50$V1_BMI > 22 & pdcd_body_V1_W_50$V1_BMI <= 33, "1",pdcd_body_V1_W_50$V1_BMI_DEX)
pdcd_body_V1_W_50$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_50$V1_BMI > 33,  "2",pdcd_body_V1_W_50$V1_BMI_DEX)

#V1_남성_60대_신체계측검사(2개 군집)
max(pdcd_body_V1_M_60$V1_SBP)/4
pdcd_body_V1_M_60$V1_SBP_DEX <- "NA"
pdcd_body_V1_M_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_SBP <= 50, "2", pdcd_body_V1_M_60$V1_SBP_DEX)
pdcd_body_V1_M_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_SBP > 50 & pdcd_body_V1_M_60$V1_SBP <= 100, "1",pdcd_body_V1_M_60$V1_SBP_DEX)
pdcd_body_V1_M_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_SBP > 100 & pdcd_body_V1_M_60$V1_SBP <= 150, "1",pdcd_body_V1_M_60$V1_SBP_DEX)
pdcd_body_V1_M_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_SBP > 150,  "2", pdcd_body_V1_M_60$V1_SBP_DEX)

max(pdcd_body_V1_M_60$V1_DBP)/4
pdcd_body_V1_M_60$V1_DBP_DEX <- "NA"
pdcd_body_V1_M_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_DBP <= 29, "2", pdcd_body_V1_M_60$V1_DBP_DEX)
pdcd_body_V1_M_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_DBP > 29 & pdcd_body_V1_M_60$V1_DBP <= 58, "1",pdcd_body_V1_M_60$V1_DBP_DEX)
pdcd_body_V1_M_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_DBP > 58 & pdcd_body_V1_M_60$V1_DBP <= 87, "1",pdcd_body_V1_M_60$V1_DBP_DEX)
pdcd_body_V1_M_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_60$V1_DBP > 87,  "2", pdcd_body_V1_M_60$V1_DBP_DEX)

max(pdcd_body_V1_M_60$V1_WAIST)/4
pdcd_body_V1_M_60$V1_WAIST_DEX <- "NA"
pdcd_body_V1_M_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_60$V1_WAIST <= 29, "2", pdcd_body_V1_M_60$V1_WAIST_DEX)
pdcd_body_V1_M_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_60$V1_WAIST > 29 & pdcd_body_V1_M_60$V1_WAIST <= 58, "1",pdcd_body_V1_M_60$V1_WAIST_DEX)
pdcd_body_V1_M_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_60$V1_WAIST > 58 & pdcd_body_V1_M_60$V1_WAIST <= 87, "1",pdcd_body_V1_M_60$V1_WAIST_DEX)
pdcd_body_V1_M_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_60$V1_WAIST > 87,  "2", pdcd_body_V1_M_60$V1_WAIST_DEX)

max(pdcd_body_V1_M_60$V1_BMI)/4
pdcd_body_V1_M_60$V1_BMI_DEX <- "NA"
pdcd_body_V1_M_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_60$V1_BMI <= 8.6, "2", pdcd_body_V1_M_60$V1_BMI_DEX)
pdcd_body_V1_M_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_60$V1_BMI > 8.6 & pdcd_body_V1_M_60$V1_BMI <= 17.2, "1",pdcd_body_V1_M_60$V1_BMI_DEX)
pdcd_body_V1_M_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_60$V1_BMI > 17.2 & pdcd_body_V1_M_60$V1_BMI <= 25.8, "1",pdcd_body_V1_M_60$V1_BMI_DEX)
pdcd_body_V1_M_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_60$V1_BMI > 25.8,  "2",pdcd_body_V1_M_60$V1_BMI_DEX)

#V1_여성_60대_신체계측검사(2개 군집)
max(pdcd_body_V1_W_60$V1_SBP)/4
pdcd_body_V1_W_60$V1_SBP_DEX <- "NA"
pdcd_body_V1_W_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_SBP <= 55, "2", pdcd_body_V1_W_60$V1_SBP_DEX)
pdcd_body_V1_W_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_SBP > 55 & pdcd_body_V1_W_60$V1_SBP <= 110, "1",pdcd_body_V1_W_60$V1_SBP_DEX)
pdcd_body_V1_W_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_SBP > 110 & pdcd_body_V1_W_60$V1_SBP <= 165, "1",pdcd_body_V1_W_60$V1_SBP_DEX)
pdcd_body_V1_W_60$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_SBP > 165,  "2", pdcd_body_V1_W_60$V1_SBP_DEX)

max(pdcd_body_V1_W_60$V1_DBP)/4
pdcd_body_V1_W_60$V1_DBP_DEX <- "NA"
pdcd_body_V1_W_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_DBP <= 28, "2", pdcd_body_V1_W_60$V1_DBP_DEX)
pdcd_body_V1_W_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_DBP > 28 & pdcd_body_V1_W_60$V1_DBP <= 56, "1",pdcd_body_V1_W_60$V1_DBP_DEX)
pdcd_body_V1_W_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_DBP > 56 & pdcd_body_V1_W_60$V1_DBP <= 84, "1",pdcd_body_V1_W_60$V1_DBP_DEX)
pdcd_body_V1_W_60$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_60$V1_DBP > 84,  "2", pdcd_body_V1_W_60$V1_DBP_DEX)

max(pdcd_body_V1_W_60$V1_WAIST)/4
pdcd_body_V1_W_60$V1_WAIST_DEX <- "NA"
pdcd_body_V1_W_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_60$V1_WAIST <= 31, "2", pdcd_body_V1_W_60$V1_WAIST_DEX)
pdcd_body_V1_W_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_60$V1_WAIST > 31 & pdcd_body_V1_W_60$V1_WAIST <= 62, "1",pdcd_body_V1_W_60$V1_WAIST_DEX)
pdcd_body_V1_W_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_60$V1_WAIST > 62 & pdcd_body_V1_W_60$V1_WAIST <= 93, "1",pdcd_body_V1_W_60$V1_WAIST_DEX)
pdcd_body_V1_W_60$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_60$V1_WAIST > 93,  "2", pdcd_body_V1_W_60$V1_WAIST_DEX)

max(pdcd_body_V1_W_60$V1_BMI)/4
pdcd_body_V1_W_60$V1_BMI_DEX <- "NA"
pdcd_body_V1_W_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_60$V1_BMI <= 10, "2", pdcd_body_V1_W_60$V1_BMI_DEX)
pdcd_body_V1_W_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_60$V1_BMI > 10 & pdcd_body_V1_W_60$V1_BMI <= 20, "1",pdcd_body_V1_W_60$V1_BMI_DEX)
pdcd_body_V1_W_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_60$V1_BMI > 20 & pdcd_body_V1_W_60$V1_BMI <= 30, "1",pdcd_body_V1_W_60$V1_BMI_DEX)
pdcd_body_V1_W_60$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_60$V1_BMI > 30,  "2",pdcd_body_V1_W_60$V1_BMI_DEX)

#V1_남성_70대_신체계측검사(2개 군집)
max(pdcd_body_V1_M_70$V1_SBP)/4
pdcd_body_V1_M_70$V1_SBP_DEX <- "NA"
pdcd_body_V1_M_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_SBP <= 48, "2", pdcd_body_V1_M_70$V1_SBP_DEX)
pdcd_body_V1_M_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_SBP > 48 & pdcd_body_V1_M_70$V1_SBP <= 96, "1",pdcd_body_V1_M_70$V1_SBP_DEX)
pdcd_body_V1_M_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_SBP > 96 & pdcd_body_V1_M_70$V1_SBP <= 144, "1",pdcd_body_V1_M_70$V1_SBP_DEX)
pdcd_body_V1_M_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_SBP > 144,  "2", pdcd_body_V1_M_70$V1_SBP_DEX)

max(pdcd_body_V1_M_70$V1_DBP)/4
pdcd_body_V1_M_70$V1_DBP_DEX <- "NA"
pdcd_body_V1_M_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_DBP <= 24, "2", pdcd_body_V1_M_70$V1_DBP_DEX)
pdcd_body_V1_M_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_DBP > 24 & pdcd_body_V1_M_70$V1_DBP <= 48, "1",pdcd_body_V1_M_70$V1_DBP_DEX)
pdcd_body_V1_M_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_DBP > 48 & pdcd_body_V1_M_70$V1_DBP <= 72, "1",pdcd_body_V1_M_70$V1_DBP_DEX)
pdcd_body_V1_M_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_M_70$V1_DBP > 72,  "2", pdcd_body_V1_M_70$V1_DBP_DEX)

max(pdcd_body_V1_M_70$V1_WAIST)/4
pdcd_body_V1_M_70$V1_WAIST_DEX <- "NA"
pdcd_body_V1_M_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_70$V1_WAIST <= 27, "2", pdcd_body_V1_M_70$V1_WAIST_DEX)
pdcd_body_V1_M_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_70$V1_WAIST > 27 & pdcd_body_V1_M_70$V1_WAIST <= 54, "1",pdcd_body_V1_M_70$V1_WAIST_DEX)
pdcd_body_V1_M_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_70$V1_WAIST > 54 & pdcd_body_V1_M_70$V1_WAIST <= 81, "1",pdcd_body_V1_M_70$V1_WAIST_DEX)
pdcd_body_V1_M_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_M_70$V1_WAIST > 81,  "2", pdcd_body_V1_M_70$V1_WAIST_DEX)

max(pdcd_body_V1_M_70$V1_BMI)/4
pdcd_body_V1_M_70$V1_BMI_DEX <- "NA"
pdcd_body_V1_M_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_70$V1_BMI <= 7.5, "2", pdcd_body_V1_M_70$V1_BMI_DEX)
pdcd_body_V1_M_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_70$V1_BMI > 7.5 & pdcd_body_V1_M_70$V1_BMI <= 15, "1",pdcd_body_V1_M_70$V1_BMI_DEX)
pdcd_body_V1_M_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_70$V1_BMI > 15 & pdcd_body_V1_M_70$V1_BMI <= 22.5, "1",pdcd_body_V1_M_70$V1_BMI_DEX)
pdcd_body_V1_M_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_M_70$V1_BMI > 22.5,  "2",pdcd_body_V1_M_70$V1_BMI_DEX)

#V1_여성_70대_신체계측검사(2개 군집)
max(pdcd_body_V1_W_70$V1_SBP)/4
pdcd_body_V1_W_70$V1_SBP_DEX <- "NA"
pdcd_body_V1_W_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_SBP <= 45, "2", pdcd_body_V1_W_70$V1_SBP_DEX)
pdcd_body_V1_W_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_SBP > 45 & pdcd_body_V1_W_70$V1_SBP <= 90, "1",pdcd_body_V1_W_70$V1_SBP_DEX)
pdcd_body_V1_W_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_SBP > 90 & pdcd_body_V1_W_70$V1_SBP <= 135, "1",pdcd_body_V1_W_70$V1_SBP_DEX)
pdcd_body_V1_W_70$V1_SBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_SBP > 135,  "2", pdcd_body_V1_W_70$V1_SBP_DEX)

max(pdcd_body_V1_W_70$V1_DBP)/4
pdcd_body_V1_W_70$V1_DBP_DEX <- "NA"
pdcd_body_V1_W_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_DBP <= 24, "2", pdcd_body_V1_W_70$V1_DBP_DEX)
pdcd_body_V1_W_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_DBP > 24 & pdcd_body_V1_W_70$V1_DBP <= 48, "1",pdcd_body_V1_W_70$V1_DBP_DEX)
pdcd_body_V1_W_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_DBP > 48 & pdcd_body_V1_W_70$V1_DBP <= 72, "1",pdcd_body_V1_W_70$V1_DBP_DEX)
pdcd_body_V1_W_70$V1_DBP_DEX <- ifelse(pdcd_body_V1_W_70$V1_DBP > 72,  "2", pdcd_body_V1_W_70$V1_DBP_DEX)

max(pdcd_body_V1_W_70$V1_WAIST)/4
pdcd_body_V1_W_70$V1_WAIST_DEX <- "NA"
pdcd_body_V1_W_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_70$V1_WAIST <= 26, "2", pdcd_body_V1_W_70$V1_WAIST_DEX)
pdcd_body_V1_W_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_70$V1_WAIST > 26 & pdcd_body_V1_W_70$V1_WAIST <= 52, "1",pdcd_body_V1_W_70$V1_WAIST_DEX)
pdcd_body_V1_W_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_70$V1_WAIST > 52 & pdcd_body_V1_W_70$V1_WAIST <= 78, "1",pdcd_body_V1_W_70$V1_WAIST_DEX)
pdcd_body_V1_W_70$V1_WAIST_DEX <- ifelse(pdcd_body_V1_W_70$V1_WAIST > 78,  "2", pdcd_body_V1_W_70$V1_WAIST_DEX)

max(pdcd_body_V1_W_70$V1_BMI)/4
pdcd_body_V1_W_70$V1_BMI_DEX <- "NA"
pdcd_body_V1_W_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_70$V1_BMI <= 7.7, "2", pdcd_body_V1_W_70$V1_BMI_DEX)
pdcd_body_V1_W_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_70$V1_BMI > 7.7 & pdcd_body_V1_W_70$V1_BMI <= 15.4, "1",pdcd_body_V1_W_70$V1_BMI_DEX)
pdcd_body_V1_W_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_70$V1_BMI > 15.4 & pdcd_body_V1_W_70$V1_BMI <= 23.1, "1",pdcd_body_V1_W_70$V1_BMI_DEX)
pdcd_body_V1_W_70$V1_BMI_DEX <- ifelse(pdcd_body_V1_W_70$V1_BMI > 23.1,  "2",pdcd_body_V1_W_70$V1_BMI_DEX)

#####


#####관상동맥 발병 여부 별 조사종류 기준 가장 늪은 확률의 규칙 피처 확인 및 위험도 확인#####
#####  
#####V1_남성_40대_이환정보_관상동맥#####
#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_40_2 <- pdcd_interview_V1_1_M_40 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_M_40_2_x <- pdcd_interview_V1_1_M_40_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_40_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_HEALTH)
pdcd_interview_V1_1_M_40_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_40_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_40_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_40_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_40_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_40_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_40_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_40_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_40_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_40_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_40_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_40_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_40_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_40_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_40_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_40_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_40_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTHT)
pdcd_interview_V1_1_M_40_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTDM)
pdcd_interview_V1_1_M_40_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTTH)
pdcd_interview_V1_1_M_40_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTLP)
pdcd_interview_V1_1_M_40_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTAS)
pdcd_interview_V1_1_M_40_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTKD)
pdcd_interview_V1_1_M_40_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTHP)
pdcd_interview_V1_1_M_40_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTTB)
pdcd_interview_V1_1_M_40_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTCV)
pdcd_interview_V1_1_M_40_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_TRTAR)
pdcd_interview_V1_1_M_40_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_40_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_40_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_40_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_40_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_40_2_x$V1_DRUGLPCU)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_1 <- apriori(pdcd_interview_V1_1_M_40_2_x, parameter = list(supp=0.75, conf=0.95, maxlen=3)); rules_1
rules_1_lift <- sort(rules_1, by='lift', decreasing = TRUE)
inspect(rules_1_lift[1:10])
summary(rules_1)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_1_lift[3:6], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_이환정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_40_1 <- pdcd_interview_V1_1_M_40 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_M_40_1_x <- pdcd_interview_V1_1_M_40_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_40_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_HEALTH)
pdcd_interview_V1_1_M_40_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_40_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_40_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_40_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_40_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_40_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_40_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_40_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_40_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_40_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_40_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_40_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_40_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_40_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_40_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_40_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_40_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTHT)
pdcd_interview_V1_1_M_40_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTDM)
pdcd_interview_V1_1_M_40_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTTH)
pdcd_interview_V1_1_M_40_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTLP)
pdcd_interview_V1_1_M_40_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTAS)
pdcd_interview_V1_1_M_40_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTKD)
pdcd_interview_V1_1_M_40_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTHP)
pdcd_interview_V1_1_M_40_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTTB)
pdcd_interview_V1_1_M_40_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTCV)
pdcd_interview_V1_1_M_40_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_TRTAR)
pdcd_interview_V1_1_M_40_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_40_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_40_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_40_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_40_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_40_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_2 <- apriori(pdcd_interview_V1_1_M_40_1_x, parameter = list(supp=0.75, conf=0.95, maxlen=3)); rules_2
rules_2_lift <- sort(rules_2, by='lift', decreasing = TRUE)
inspect(rules_2_lift[1:10])
summary(rules_2)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_2_lift[c(5,7),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_이환정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_40대_이환정보
abs(log(4/25982) - log(2/25676))
pdcd_interview_V1_1_M_40$RISK <- abs(log(4/25982) - log(2/25676))

#####

#####V1_여성_40대_이환정보_관상동맥#####
##관상동맥 발병_2(예)##
##여성_40대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_40_2 <- pdcd_interview_V1_1_W_40 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_W_40_2_x <- pdcd_interview_V1_1_W_40_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_40_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_HEALTH)
pdcd_interview_V1_1_W_40_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_40_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_40_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_40_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_40_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_40_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_40_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_40_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_40_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_40_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_40_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_40_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_40_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_40_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_40_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_40_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_40_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTHT)
pdcd_interview_V1_1_W_40_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTDM)
pdcd_interview_V1_1_W_40_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTTH)
pdcd_interview_V1_1_W_40_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTLP)
pdcd_interview_V1_1_W_40_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTAS)
pdcd_interview_V1_1_W_40_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTKD)
pdcd_interview_V1_1_W_40_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTHP)
pdcd_interview_V1_1_W_40_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTTB)
pdcd_interview_V1_1_W_40_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTCV)
pdcd_interview_V1_1_W_40_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_TRTAR)
pdcd_interview_V1_1_W_40_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_40_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_40_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_40_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_40_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_40_2_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_3 <- apriori(pdcd_interview_V1_1_W_40_2_x, parameter = list(supp=0.70, conf=0.95, maxlen=3)); rules_3
rules_3_lift <- sort(rules_3, by='lift', decreasing = TRUE)
inspect(rules_3_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_3_lift[3:4], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_이환정보_관상동맥 발병_2(예)")

#####

##V1
##관상동맥 발병_1(아니오)##
##여성_40대_이환정보
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_40_1 <- pdcd_interview_V1_1_W_40 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_W_40_1_x <- pdcd_interview_V1_1_W_40_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_40_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_HEALTH)
pdcd_interview_V1_1_W_40_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_40_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_40_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_40_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_40_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_40_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_40_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_40_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_40_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_40_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_40_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_40_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_40_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_40_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_40_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_40_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_40_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTHT)
pdcd_interview_V1_1_W_40_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTDM)
pdcd_interview_V1_1_W_40_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTTH)
pdcd_interview_V1_1_W_40_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTLP)
pdcd_interview_V1_1_W_40_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTAS)
pdcd_interview_V1_1_W_40_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTKD)
pdcd_interview_V1_1_W_40_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTHP)
pdcd_interview_V1_1_W_40_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTTB)
pdcd_interview_V1_1_W_40_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTCV)
pdcd_interview_V1_1_W_40_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_TRTAR)
pdcd_interview_V1_1_W_40_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_40_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_40_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_40_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_40_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_40_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_4 <- apriori(pdcd_interview_V1_1_W_40_1_x, parameter = list(supp=0.70, conf=0.95, maxlen=3)); rules_4
rules_4_lift <- sort(rules_4, by='lift', decreasing = TRUE)
inspect(rules_4_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_4_lift[c(1),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_이환정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_40대_이환정보
#36/19195 = 0.2%
#1966/26413 = 7.4%
abs(log(2/19195) - log(1/26413))
pdcd_interview_V1_1_W_40$RISK <- abs(log(2/19195) - log(1/26413))

#####

#####V1_남성_50대_이환정보_관상동맥#####
##관상동맥 발병_2(예)##
##남성_50대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_50_2 <- pdcd_interview_V1_1_M_50 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_M_50_2_x <- pdcd_interview_V1_1_M_50_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_50_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_HEALTH)
pdcd_interview_V1_1_M_50_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_50_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_50_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_50_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_50_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_50_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_50_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_50_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_50_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_50_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_50_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_50_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_50_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_50_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_50_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_50_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_50_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTHT)
pdcd_interview_V1_1_M_50_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTDM)
pdcd_interview_V1_1_M_50_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTTH)
pdcd_interview_V1_1_M_50_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTLP)
pdcd_interview_V1_1_M_50_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTAS)
pdcd_interview_V1_1_M_50_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTKD)
pdcd_interview_V1_1_M_50_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTHP)
pdcd_interview_V1_1_M_50_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTTB)
pdcd_interview_V1_1_M_50_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTCV)
pdcd_interview_V1_1_M_50_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_TRTAR)
pdcd_interview_V1_1_M_50_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_50_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_50_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_50_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_50_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_50_2_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_5 <- apriori(pdcd_interview_V1_1_M_50_2_x, parameter = list(supp=0.75, conf=0.95, maxlen=3)); rules_5
rules_5_lift <- sort(rules_5, by='lift', decreasing = TRUE)
inspect(rules_5_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_5_lift[5:6], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_이환정보_관상동맥 발병_2(예)")
#####

##V1
##관상동맥 발병_1(아니오)##
##남성_50대_이환정보 (1184명)
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_50_1 <- pdcd_interview_V1_1_M_50 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_M_50_1_x <- pdcd_interview_V1_1_M_50_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_50_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_HEALTH)
pdcd_interview_V1_1_M_50_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_50_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_50_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_50_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_50_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_50_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_50_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_50_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_50_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_50_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_50_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_50_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_50_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_50_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_50_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_50_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_50_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTHT)
pdcd_interview_V1_1_M_50_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTDM)
pdcd_interview_V1_1_M_50_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTTH)
pdcd_interview_V1_1_M_50_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTLP)
pdcd_interview_V1_1_M_50_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTAS)
pdcd_interview_V1_1_M_50_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTKD)
pdcd_interview_V1_1_M_50_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTHP)
pdcd_interview_V1_1_M_50_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTTB)
pdcd_interview_V1_1_M_50_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTCV)
pdcd_interview_V1_1_M_50_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_TRTAR)
pdcd_interview_V1_1_M_50_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_50_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_50_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_50_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_50_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_50_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_6 <- apriori(pdcd_interview_V1_1_M_50_1_x, parameter = list(supp=0.75, conf=0.95, maxlen=3)); rules_6
rules_6_lift <- sort(rules_6, by='lift', decreasing = TRUE)
inspect(rules_6_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_6_lift[c(9),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_이환정보_관상동맥 발병_1(아니오)")
#####

##위험도 계산 및 추가
##남성_50대_이환정보
abs(log(2/20958) - log(1/21921))
pdcd_interview_V1_1_M_50$RISK <- abs(log(2/20958) - log(1/21921))

#####


#####V1_여성_50대_이환정보_관상동맥#####
##관상동맥 발병_2(예)##
##여성_50대_이환정보
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_50_2 <- pdcd_interview_V1_1_W_50 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_W_50_2_x <- pdcd_interview_V1_1_W_50_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_50_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_HEALTH)
pdcd_interview_V1_1_W_50_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_50_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_50_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_50_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_50_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_50_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_50_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_50_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_50_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_50_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_50_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_50_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_50_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_50_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_50_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_50_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_50_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTHT)
pdcd_interview_V1_1_W_50_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTDM)
pdcd_interview_V1_1_W_50_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTTH)
pdcd_interview_V1_1_W_50_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTLP)
pdcd_interview_V1_1_W_50_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTAS)
pdcd_interview_V1_1_W_50_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTKD)
pdcd_interview_V1_1_W_50_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTHP)
pdcd_interview_V1_1_W_50_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTTB)
pdcd_interview_V1_1_W_50_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTCV)
pdcd_interview_V1_1_W_50_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_TRTAR)
pdcd_interview_V1_1_W_50_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_50_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_50_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_50_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_50_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_50_2_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_7 <- apriori(pdcd_interview_V1_1_W_50_2_x, parameter = list(supp=0.8, conf=0.95, maxlen=3)); rules_7
rules_7_lift <- sort(rules_7, by='lift', decreasing = TRUE)
inspect(rules_7_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_7_lift[5:6], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_이환정보_관상동맥 발병_2(예)")

#####

##V1
##관상동맥 발병_1(아니오)##
##여성_50대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_50_1 <- pdcd_interview_V1_1_W_50 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_W_50_1_x <- pdcd_interview_V1_1_W_50_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_50_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_HEALTH)
pdcd_interview_V1_1_W_50_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_50_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_50_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_50_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_50_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_50_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_50_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_50_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_50_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_50_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_50_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_50_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_50_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_50_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_50_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_50_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_50_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTHT)
pdcd_interview_V1_1_W_50_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTDM)
pdcd_interview_V1_1_W_50_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTTH)
pdcd_interview_V1_1_W_50_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTLP)
pdcd_interview_V1_1_W_50_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTAS)
pdcd_interview_V1_1_W_50_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTKD)
pdcd_interview_V1_1_W_50_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTHP)
pdcd_interview_V1_1_W_50_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTTB)
pdcd_interview_V1_1_W_50_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTCV)
pdcd_interview_V1_1_W_50_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_TRTAR)
pdcd_interview_V1_1_W_50_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_50_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_50_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_50_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_50_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_50_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_8 <- apriori(pdcd_interview_V1_1_W_50_1_x, parameter = list(supp=0.8, conf=0.95, maxlen=3)); rules_8
rules_8_lift <- sort(rules_8, by='lift', decreasing = TRUE)
inspect(rules_8_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_8_lift[c(4,5),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_이환정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_50대_이환정보 
abs(log(2/18746) - log(2/18349))
pdcd_interview_V1_1_W_50$RISK <- abs(log(2/18746) - log(2/18349))

#####


#####V1_남성_60대_이환정보_관상동맥#####
##관상동맥 발병_2(예)##
##남성_60대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_60_2 <- pdcd_interview_V1_1_M_60 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_M_60_2_x <- pdcd_interview_V1_1_M_60_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_60_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_HEALTH)
pdcd_interview_V1_1_M_60_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_60_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_60_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_60_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_60_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_60_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_60_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_60_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_60_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_60_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_60_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_60_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_60_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_60_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_60_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_60_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_60_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTHT)
pdcd_interview_V1_1_M_60_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTDM)
pdcd_interview_V1_1_M_60_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTTH)
pdcd_interview_V1_1_M_60_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTLP)
pdcd_interview_V1_1_M_60_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTAS)
pdcd_interview_V1_1_M_60_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTKD)
pdcd_interview_V1_1_M_60_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTHP)
pdcd_interview_V1_1_M_60_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTTB)
pdcd_interview_V1_1_M_60_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTCV)
pdcd_interview_V1_1_M_60_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_TRTAR)
pdcd_interview_V1_1_M_60_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_60_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_60_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_60_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_60_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_60_2_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_9 <- apriori(pdcd_interview_V1_1_M_60_2_x, parameter = list(supp=0.85, conf=0.95, maxlen=3)); rules_9
rules_9_lift <- sort(rules_9, by='lift', decreasing = TRUE)
inspect(rules_9_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_9_lift[10], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_이환정보_관상동맥 발병_2(예)")

#####

##V1
##관상동맥 발병_1(아니오)##
##남성_60대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_60_1 <- pdcd_interview_V1_1_M_60 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_M_60_1_x <- pdcd_interview_V1_1_M_60_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_60_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_HEALTH)
pdcd_interview_V1_1_M_60_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_60_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_60_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_60_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_60_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_60_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_60_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_60_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_60_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_60_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_60_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_60_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_60_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_60_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_60_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_60_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_60_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTHT)
pdcd_interview_V1_1_M_60_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTDM)
pdcd_interview_V1_1_M_60_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTTH)
pdcd_interview_V1_1_M_60_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTLP)
pdcd_interview_V1_1_M_60_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTAS)
pdcd_interview_V1_1_M_60_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTKD)
pdcd_interview_V1_1_M_60_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTHP)
pdcd_interview_V1_1_M_60_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTTB)
pdcd_interview_V1_1_M_60_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTCV)
pdcd_interview_V1_1_M_60_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_TRTAR)
pdcd_interview_V1_1_M_60_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_60_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_60_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_60_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_60_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_60_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_10 <- apriori(pdcd_interview_V1_1_M_60_1_x, parameter = list(supp=0.85, conf=0.95, maxlen=3)); rules_10
rules_10_lift <- sort(rules_10, by='lift', decreasing = TRUE)
inspect(rules_10_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_10_lift[c(7,8),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_이환정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_60대_이환정보 
abs(log(1/13717) - log(2/20376))
pdcd_interview_V1_1_M_60$RISK <- abs(log(1/13717) - log(2/20376))

#####


#####V1_여성_60대_이환정보_관상동맥#####
##관상동맥 발병_2(예)##
##여성_60대_이환정보
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_60_2 <- pdcd_interview_V1_1_W_60 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_W_60_2_x <- pdcd_interview_V1_1_W_60_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_60_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_HEALTH)
pdcd_interview_V1_1_W_60_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_60_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_60_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_60_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_60_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_60_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_60_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_60_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_60_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_60_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_60_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_60_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_60_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_60_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_60_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_60_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_60_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTHT)
pdcd_interview_V1_1_W_60_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTDM)
pdcd_interview_V1_1_W_60_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTTH)
pdcd_interview_V1_1_W_60_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTLP)
pdcd_interview_V1_1_W_60_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTAS)
pdcd_interview_V1_1_W_60_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTKD)
pdcd_interview_V1_1_W_60_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTHP)
pdcd_interview_V1_1_W_60_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTTB)
pdcd_interview_V1_1_W_60_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTCV)
pdcd_interview_V1_1_W_60_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_TRTAR)
pdcd_interview_V1_1_W_60_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_60_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_60_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_60_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_60_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_60_2_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_11 <- apriori(pdcd_interview_V1_1_W_60_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_11
rules_11_lift <- sort(rules_11, by='lift', decreasing = TRUE)
inspect(rules_11_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_11_lift[1:2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_이환정보_관상동맥 발병_2(예)")

#####

##V1
##관상동맥 발병_1(아니오)##
##여성_60대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_60_1 <- pdcd_interview_V1_1_W_60 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_W_60_1_x <- pdcd_interview_V1_1_W_60_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_60_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_HEALTH)
pdcd_interview_V1_1_W_60_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_60_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_60_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_60_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_60_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_60_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_60_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_60_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_60_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_60_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_60_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_60_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_60_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_60_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_60_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_60_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_60_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTHT)
pdcd_interview_V1_1_W_60_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTDM)
pdcd_interview_V1_1_W_60_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTTH)
pdcd_interview_V1_1_W_60_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTLP)
pdcd_interview_V1_1_W_60_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTAS)
pdcd_interview_V1_1_W_60_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTKD)
pdcd_interview_V1_1_W_60_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTHP)
pdcd_interview_V1_1_W_60_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTTB)
pdcd_interview_V1_1_W_60_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTCV)
pdcd_interview_V1_1_W_60_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_TRTAR)
pdcd_interview_V1_1_W_60_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_60_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_60_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_60_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_60_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_60_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_12 <- apriori(pdcd_interview_V1_1_W_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_12
rules_12_lift <- sort(rules_12, by='lift', decreasing = TRUE)
inspect(rules_12_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_12_lift[c(10),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_이환정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_60대_이환정보 
abs(log(2/16599) - log(1/17853))
pdcd_interview_V1_1_W_60$RISK <- abs(log(2/16599) - log(1/17853))

#####


#####V1_남성_70대_이환정보_관상동맥#####
##관상동맥 발병_2(예)##
##남성_70대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_70_2 <- pdcd_interview_V1_1_M_70 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_M_70_2_x <- pdcd_interview_V1_1_M_70_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_70_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_HEALTH)
pdcd_interview_V1_1_M_70_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_70_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_70_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_70_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_70_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_70_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_70_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_70_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_70_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_70_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_70_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_70_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_70_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_70_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_70_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_70_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_70_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTHT)
pdcd_interview_V1_1_M_70_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTDM)
pdcd_interview_V1_1_M_70_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTTH)
pdcd_interview_V1_1_M_70_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTLP)
pdcd_interview_V1_1_M_70_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTAS)
pdcd_interview_V1_1_M_70_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTKD)
pdcd_interview_V1_1_M_70_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTHP)
pdcd_interview_V1_1_M_70_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTTB)
pdcd_interview_V1_1_M_70_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTCV)
pdcd_interview_V1_1_M_70_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_TRTAR)
pdcd_interview_V1_1_M_70_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_70_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_70_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_70_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_70_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_70_2_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_13 <- apriori(pdcd_interview_V1_1_M_60_2_x, parameter = list(supp=0.90, conf=0.95, maxlen=3)); rules_13
rules_13_lift <- sort(rules_13, by='lift', decreasing = TRUE)
inspect(rules_13_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_13_lift[3], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_남성_이환정보_관상동맥 발병_2(예)")

#####

##V1
##관상동맥 발병_1(아니오)##
##남성_70대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_M_70_1 <- pdcd_interview_V1_1_M_70 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_M_70_1_x <- pdcd_interview_V1_1_M_70_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_M_70_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_HEALTH)
pdcd_interview_V1_1_M_70_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_M_70_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_M_70_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_M_70_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_M_70_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_M_70_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_M_70_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_M_70_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_M_70_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_M_70_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_M_70_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_M_70_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_M_70_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_M_70_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_M_70_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_M_70_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_M_70_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTHT)
pdcd_interview_V1_1_M_70_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTDM)
pdcd_interview_V1_1_M_70_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTTH)
pdcd_interview_V1_1_M_70_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTLP)
pdcd_interview_V1_1_M_70_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTAS)
pdcd_interview_V1_1_M_70_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTKD)
pdcd_interview_V1_1_M_70_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTHP)
pdcd_interview_V1_1_M_70_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTTB)
pdcd_interview_V1_1_M_70_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTCV)
pdcd_interview_V1_1_M_70_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_TRTAR)
pdcd_interview_V1_1_M_70_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_M_70_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_M_70_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_M_70_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_M_70_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_M_70_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_14 <- apriori(pdcd_interview_V1_1_M_70_1_x, parameter = list(supp=0.90, conf=0.95, maxlen=3)); rules_14
rules_14_lift <- sort(rules_14, by='lift', decreasing = TRUE)
inspect(rules_14_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_14_lift[c(1:8),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_남성_이환정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_70대_이환정보 
abs(log(1/9864) - log(8/14321))
pdcd_interview_V1_1_M_70$RISK <- abs(log(1/9864) - log(8/14321))

#####


#####V1_여성_70대_이환정보_관상동맥#####
##관상동맥 발병_2(예)##
##여성_70대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_70_2 <- pdcd_interview_V1_1_W_70 %>% filter(fuPDCD == 2)
pdcd_interview_V1_1_W_70_2_x <- pdcd_interview_V1_1_W_70_2[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_70_2_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_HEALTH)
pdcd_interview_V1_1_W_70_2_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_70_2_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_70_2_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_70_2_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_70_2_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_70_2_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_70_2_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_70_2_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_70_2_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_70_2_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_70_2_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_70_2_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_70_2_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_70_2_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_70_2_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_70_2_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_70_2_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTHT)
pdcd_interview_V1_1_W_70_2_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTDM)
pdcd_interview_V1_1_W_70_2_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTTH)
pdcd_interview_V1_1_W_70_2_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTLP)
pdcd_interview_V1_1_W_70_2_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTAS)
pdcd_interview_V1_1_W_70_2_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTKD)
pdcd_interview_V1_1_W_70_2_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTHP)
pdcd_interview_V1_1_W_70_2_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTTB)
pdcd_interview_V1_1_W_70_2_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTCV)
pdcd_interview_V1_1_W_70_2_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_TRTAR)
pdcd_interview_V1_1_W_70_2_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_70_2_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_70_2_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_70_2_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_70_2_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_70_2_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_15 <- apriori(pdcd_interview_V1_1_W_70_2_x, parameter = list(supp=0.50, conf=0.95, maxlen=3)); rules_15
rules_15_lift <- sort(rules_15, by='lift', decreasing = TRUE)
inspect(rules_15_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_15_lift, method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_이환정보_관상동맥 발병_2(예)")

#####

##V1
##관상동맥 발병_1(아니오)##
##여성_70대_이환정보 
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_1_W_70_1 <- pdcd_interview_V1_1_W_70 %>% filter(fuPDCD == 1)
pdcd_interview_V1_1_W_70_1_x <- pdcd_interview_V1_1_W_70_1[,4:44]
#####팩터형으로 변환##### 
pdcd_interview_V1_1_W_70_1_x$V1_HEALTH <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_HEALTH)
pdcd_interview_V1_1_W_70_1_x$V1_PDHT_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDHT_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDDM_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDDM_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDTH_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDTH_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDLP_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDLP_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDAS_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDAS_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDKD_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDKD_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDHP_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDHP_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDTB_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDTB_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDCV_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDCV_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_PDAR_HIST <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_PDAR_HIST)
pdcd_interview_V1_1_W_70_1_x$V1_FMHTREL3 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMHTREL3)
pdcd_interview_V1_1_W_70_1_x$V1_FMHTREL2 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMHTREL2)
pdcd_interview_V1_1_W_70_1_x$V1_FMHTREL1 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMHTREL1)
pdcd_interview_V1_1_W_70_1_x$V1_FMDMREL3 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMDMREL3)
pdcd_interview_V1_1_W_70_1_x$V1_FMDMREL2 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMDMREL2)
pdcd_interview_V1_1_W_70_1_x$V1_FMDMREL1 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMDMREL1)
pdcd_interview_V1_1_W_70_1_x$V1_FMLPREL3 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMLPREL3)
pdcd_interview_V1_1_W_70_1_x$V1_FMLPREL2 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMLPREL2)
pdcd_interview_V1_1_W_70_1_x$V1_FMLPREL1 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMLPREL1)
pdcd_interview_V1_1_W_70_1_x$V1_FMCVAREL3 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMCVAREL3)
pdcd_interview_V1_1_W_70_1_x$V1_FMCVAREL2 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMCVAREL2)
pdcd_interview_V1_1_W_70_1_x$V1_FMCVAREL1 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMCVAREL1)
pdcd_interview_V1_1_W_70_1_x$V1_FMCDREL3 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMCDREL3)
pdcd_interview_V1_1_W_70_1_x$V1_FMCDREL2 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMCDREL2)
pdcd_interview_V1_1_W_70_1_x$V1_FMCDREL1 <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_FMCDREL1)
pdcd_interview_V1_1_W_70_1_x$V1_TRTHT <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTHT)
pdcd_interview_V1_1_W_70_1_x$V1_TRTDM <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTDM)
pdcd_interview_V1_1_W_70_1_x$V1_TRTTH <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTTH)
pdcd_interview_V1_1_W_70_1_x$V1_TRTLP <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTLP)
pdcd_interview_V1_1_W_70_1_x$V1_TRTAS <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTAS)
pdcd_interview_V1_1_W_70_1_x$V1_TRTKD <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTKD)
pdcd_interview_V1_1_W_70_1_x$V1_TRTHP <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTHP)
pdcd_interview_V1_1_W_70_1_x$V1_TRTTB <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTTB)
pdcd_interview_V1_1_W_70_1_x$V1_TRTCV <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTCV)
pdcd_interview_V1_1_W_70_1_x$V1_TRTAR <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_TRTAR)
pdcd_interview_V1_1_W_70_1_x$V1_DRUGINSCU <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_DRUGINSCU)
pdcd_interview_V1_1_W_70_1_x$V1_DRUGHTCU <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_DRUGHTCU)
pdcd_interview_V1_1_W_70_1_x$V1_DRUGDMCU <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_DRUGDMCU)
pdcd_interview_V1_1_W_70_1_x$V1_DRUGASCU <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_DRUGASCU)
pdcd_interview_V1_1_W_70_1_x$V1_DRUGLPCU <- as.factor(pdcd_interview_V1_1_W_70_1_x$V1_DRUGLPCU)
#####장바구니 분석#####
options(digits = 2)
#향상도 기준
rules_16 <- apriori(pdcd_interview_V1_1_W_70_1_x, parameter = list(supp=0.50, conf=0.95, maxlen=3)); rules_16
rules_16_lift <- sort(rules_16, by='lift', decreasing = TRUE)
inspect(rules_16_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_16_lift[21:23], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_이환정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_70대_이환정보 
abs(log(42936/42936) - log(3/19600))
pdcd_interview_V1_1_W_70$RISK <- abs(log(42936/42936) - log(3/19600))

#####


#####남성_40대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_40_2 <- pdcd_interview_V1_2_M_40 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_M_40_2_x <- pdcd_interview_V1_2_M_40_2[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_M_40_2_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_M_40_2_x$V1_DRINK)
pdcd_interview_V1_2_M_40_2_x$V1_PSM <- as.factor(pdcd_interview_V1_2_M_40_2_x$V1_PSM)
pdcd_interview_V1_2_M_40_2_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_M_40_2_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_17 <- apriori(pdcd_interview_V1_2_M_40_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_17
rules_17_lift <- sort(rules_17, by='lift', decreasing = TRUE)
inspect(rules_17_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_17_lift, method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_건강행태정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_40_1 <- pdcd_interview_V1_2_M_40 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_M_40_1_x <- pdcd_interview_V1_2_M_40_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_M_40_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_M_40_1_x$V1_DRINK)
pdcd_interview_V1_2_M_40_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_M_40_1_x$V1_PSM)
pdcd_interview_V1_2_M_40_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_M_40_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_18 <- apriori(pdcd_interview_V1_1_M_40_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_18
rules_18_lift <- sort(rules_18, by='lift', decreasing = TRUE)
inspect(rules_18_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_18_lift[c(5,7),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_40대_건강행태정보
abs(log(2/2) - log(2/25628))
pdcd_interview_V1_2_M_40$RISK <- abs(log(2/2) - log(2/25628))

#####


#####여성_40대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_40_2 <- pdcd_interview_V1_2_W_40 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_W_40_2_x <- pdcd_interview_V1_2_W_40_2[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_40_2_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_40_2_x$V1_DRINK)
pdcd_interview_V1_2_W_40_2_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_40_2_x$V1_PSM)
pdcd_interview_V1_2_W_40_2_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_40_2_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_19 <- apriori(pdcd_interview_V1_2_W_40_2_x, parameter = list(supp=0.60, conf=0.95, maxlen=3)); rules_19
rules_19_lift <- sort(rules_19, by='lift', decreasing = TRUE)
inspect(rules_19_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_19_lift[2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_건강행태정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_40_1 <- pdcd_interview_V1_2_W_40 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_W_40_1_x <- pdcd_interview_V1_2_W_40_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_40_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_40_1_x$V1_DRINK)
pdcd_interview_V1_2_W_40_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_40_1_x$V1_PSM)
pdcd_interview_V1_2_W_40_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_40_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_20 <- apriori(pdcd_interview_V1_1_W_40_1_x, parameter = list(supp=0.60, conf=0.95, maxlen=3)); rules_20
rules_20_lift <- sort(rules_20, by='lift', decreasing = TRUE)
inspect(rules_20_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_20_lift[c(1),], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_40대_건강행태정보
abs(log(1/2) - log(1/26413))
pdcd_interview_V1_2_W_40$RISK <- abs(log(1/2) - log(1/26413))

#####


#####남성_50대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_50_2 <- pdcd_interview_V1_2_M_50 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_M_50_2_x <- pdcd_interview_V1_2_M_50_2[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_M_50_2_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_M_50_2_x$V1_DRINK)
pdcd_interview_V1_2_M_50_2_x$V1_PSM <- as.factor(pdcd_interview_V1_2_M_50_2_x$V1_PSM)
pdcd_interview_V1_2_M_50_2_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_M_50_2_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_21 <- apriori(pdcd_interview_V1_2_M_50_2_x, parameter = list(supp=0.14, conf=0.95, maxlen=3)); rules_21
rules_21_lift <- sort(rules_21, by='lift', decreasing = TRUE)
inspect(rules_21_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_21_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_건강행태정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_50_1 <- pdcd_interview_V1_2_M_50 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_M_50_1_x <- pdcd_interview_V1_2_M_50_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_M_50_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_M_50_1_x$V1_DRINK)
pdcd_interview_V1_2_M_50_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_M_50_1_x$V1_PSM)
pdcd_interview_V1_2_M_50_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_M_50_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_22 <- apriori(pdcd_interview_V1_1_M_50_1_x, parameter = list(supp=0.14, conf=0.95, maxlen=3)); rules_22
rules_22_lift <- sort(rules_22, by='lift', decreasing = TRUE)
inspect(rules_22_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_22_lift[9], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_50대_건강행태정보
abs(log(1/2) - log(1/25188))
pdcd_interview_V1_2_M_50$RISK <- abs(log(1/2) - log(1/25188))

#####

#####여성_50대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_50_2 <- pdcd_interview_V1_2_W_50 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_W_50_2_x <- pdcd_interview_V1_2_W_50_2[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_50_2_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_50_2_x$V1_DRINK)
pdcd_interview_V1_2_W_50_2_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_50_2_x$V1_PSM)
pdcd_interview_V1_2_W_50_2_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_50_2_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_23 <- apriori(pdcd_interview_V1_2_W_50_2_x, parameter = list(supp=0.60, conf=0.95, maxlen=3)); rules_23
rules_23_lift <- sort(rules_23, by='lift', decreasing = TRUE)
inspect(rules_23_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_23_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_건강행태정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_50_1 <- pdcd_interview_V1_2_W_50 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_W_50_1_x <- pdcd_interview_V1_2_W_50_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_50_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_50_1_x$V1_DRINK)
pdcd_interview_V1_2_W_50_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_50_1_x$V1_PSM)
pdcd_interview_V1_2_W_50_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_50_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_24 <- apriori(pdcd_interview_V1_1_W_50_1_x, parameter = list(supp=0.60, conf=0.95, maxlen=3)); rules_24
rules_24_lift <- sort(rules_24, by='lift', decreasing = TRUE)
inspect(rules_24_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_24_lift[9], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_50대_건강행태정보
abs(log(1/2) - log(1/22575))
pdcd_interview_V1_2_W_50$RISK <-abs(log(1/2) - log(1/22575))

#####


#####남성_60대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_60_2 <- pdcd_interview_V1_2_M_60 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_M_60_2_x <- pdcd_interview_V1_2_M_60_2[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_M_60_2_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_M_60_2_x$V1_DRINK)
pdcd_interview_V1_2_M_60_2_x$V1_PSM <- as.factor(pdcd_interview_V1_2_M_60_2_x$V1_PSM)
pdcd_interview_V1_2_M_60_2_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_M_60_2_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_25 <- apriori(pdcd_interview_V1_2_M_60_2_x, parameter = list(supp=0.10, conf=0.95, maxlen=3)); rules_25
rules_25_lift <- sort(rules_25, by='lift', decreasing = TRUE)
inspect(rules_25_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_25_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_건강행태정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_60_1 <- pdcd_interview_V1_2_M_60 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_M_60_1_x <- pdcd_interview_V1_2_M_60_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_M_60_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_M_60_1_x$V1_DRINK)
pdcd_interview_V1_2_M_60_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_M_60_1_x$V1_PSM)
pdcd_interview_V1_2_M_60_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_M_60_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_26 <- apriori(pdcd_interview_V1_1_M_60_1_x, parameter = list(supp=0.10, conf=0.95, maxlen=3)); rules_26
rules_26_lift <- sort(rules_26, by='lift', decreasing = TRUE)
inspect(rules_26_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_26_lift[c(4,6)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_60대_건강행태정보
abs(log(1/2) - log(2/31346))
pdcd_interview_V1_2_M_60$RISK <- abs(log(1/2) - log(2/31346))

#####


#####여성_60대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_60_2 <- pdcd_interview_V1_2_W_60 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_W_60_2_x <- pdcd_interview_V1_2_W_60_2[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_60_2_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_60_2_x$V1_DRINK)
pdcd_interview_V1_2_W_60_2_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_60_2_x$V1_PSM)
pdcd_interview_V1_2_W_60_2_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_60_2_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_27 <- apriori(pdcd_interview_V1_2_W_60_2_x, parameter = list(supp=0.10, conf=0.95, maxlen=3)); rules_27
rules_27_lift <- sort(rules_27, by='lift', decreasing = TRUE)
inspect(rules_27_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_27_lift[1:2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_건강행태정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_60_1 <- pdcd_interview_V1_2_W_60 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_W_60_1_x <- pdcd_interview_V1_2_W_60_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_60_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_60_1_x$V1_DRINK)
pdcd_interview_V1_2_W_60_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_60_1_x$V1_PSM)
pdcd_interview_V1_2_W_60_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_60_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_28 <- apriori(pdcd_interview_V1_1_W_60_1_x, parameter = list(supp=0.10, conf=0.95, maxlen=3)); rules_28
rules_28_lift <- sort(rules_28, by='lift', decreasing = TRUE)
inspect(rules_28_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_28_lift[9], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_60대_건강행태정보
abs(log(2/5) - log(1/33429))
pdcd_interview_V1_2_W_60$RISK <-abs(log(2/5) - log(1/33429))

#####


#####남성_70대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_70_2 <- pdcd_interview_V1_2_M_70 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_M_70_2_x <- pdcd_interview_V1_2_M_70_2[,4:6]

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_M_70_1 <- pdcd_interview_V1_2_M_70 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_M_70_1_x <- pdcd_interview_V1_2_M_70_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_M_70_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_M_70_1_x$V1_DRINK)
pdcd_interview_V1_2_M_70_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_M_70_1_x$V1_PSM)
pdcd_interview_V1_2_M_70_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_M_70_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_30 <- apriori(pdcd_interview_V1_1_M_70_1_x, parameter = list(supp=0.95, conf=0.95, maxlen=3)); rules_30
rules_30_lift <- sort(rules_30, by='lift', decreasing = TRUE)
inspect(rules_30_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_30_lift[1:8], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_남성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_70대_건강행태정보
abs(log(8/6790))
pdcd_interview_V1_2_M_70$RISK <- abs(log(8/6790))

#####


#####여성_70대_건강행태정보_문진_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_70_2 <- pdcd_interview_V1_2_W_70 %>% filter(fuPDCD == 2)
pdcd_interview_V1_2_W_70_2_x <- pdcd_interview_V1_2_W_70_2[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_70_2_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_70_2_x$V1_DRINK)
pdcd_interview_V1_2_W_70_2_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_70_2_x$V1_PSM)
pdcd_interview_V1_2_W_70_2_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_70_2_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_31 <- apriori(pdcd_interview_V1_2_W_70_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_31
rules_31_lift <- sort(rules_31, by='lift', decreasing = TRUE)
inspect(rules_31_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_31_lift, method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_건강행태정보_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_interview_V1_2_W_70_1 <- pdcd_interview_V1_2_W_70 %>% filter(fuPDCD == 1)
pdcd_interview_V1_2_W_70_1_x <- pdcd_interview_V1_2_W_70_1[,4:6]
#####팩터형으로 변환##### 
pdcd_interview_V1_2_W_70_1_x$V1_DRINK <- as.factor(pdcd_interview_V1_2_W_70_1_x$V1_DRINK)
pdcd_interview_V1_2_W_70_1_x$V1_PSM <- as.factor(pdcd_interview_V1_2_W_70_1_x$V1_PSM)
pdcd_interview_V1_2_W_70_1_x$V1_SMOKEACA <- as.factor(pdcd_interview_V1_2_W_70_1_x$V1_SMOKEACA)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_32 <- apriori(pdcd_interview_V1_1_W_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_32
rules_32_lift <- sort(rules_32, by='lift', decreasing = TRUE)
inspect(rules_32_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_32_lift[10], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_건강행태정정보_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_70대_건강행태정보
abs(log(12/12) - log(1/17853))
pdcd_interview_V1_2_W_70$RISK <- abs(log(12/12) - log(1/17853))

#####


#####남성_40대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_40_2 <- pdcd_blood_V1_M_40 %>% filter(fuPDCD == 2)
pdcd_blood_V1_M_40_2_x <- pdcd_blood_V1_M_40_2[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_M_40_2_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_WBC_DEX)
pdcd_blood_V1_M_40_2_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_RBC_DEX)
pdcd_blood_V1_M_40_2_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_HB_DEX)
pdcd_blood_V1_M_40_2_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_HCT_DEX)
pdcd_blood_V1_M_40_2_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_HBA1C_DEX)
pdcd_blood_V1_M_40_2_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_PLAT_DEX)
pdcd_blood_V1_M_40_2_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_ALT_DEX)
pdcd_blood_V1_M_40_2_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_AST_DEX)
pdcd_blood_V1_M_40_2_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_BUN_DEX)
pdcd_blood_V1_M_40_2_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_CR_DEX)
pdcd_blood_V1_M_40_2_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_GLU_DEX)
pdcd_blood_V1_M_40_2_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_CHOL_DEX)
pdcd_blood_V1_M_40_2_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_HDL_DEX)
pdcd_blood_V1_M_40_2_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_TG_DEX)
pdcd_blood_V1_M_40_2_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_M_40_2_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_33 <- apriori(pdcd_blood_V1_M_40_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_33
rules_33_lift <- sort(rules_33, by='lift', decreasing = TRUE)
inspect(rules_33_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_33_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_혈액검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_40_1 <- pdcd_blood_V1_M_40 %>% filter(fuPDCD == 1)
pdcd_blood_V1_M_40_1_x <- pdcd_blood_V1_M_40_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_M_40_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_WBC_DEX)
pdcd_blood_V1_M_40_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_RBC_DEX)
pdcd_blood_V1_M_40_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_HB_DEX)
pdcd_blood_V1_M_40_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_HCT_DEX)
pdcd_blood_V1_M_40_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_M_40_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_PLAT_DEX)
pdcd_blood_V1_M_40_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_ALT_DEX)
pdcd_blood_V1_M_40_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_AST_DEX)
pdcd_blood_V1_M_40_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_BUN_DEX)
pdcd_blood_V1_M_40_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_CR_DEX)
pdcd_blood_V1_M_40_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_GLU_DEX)
pdcd_blood_V1_M_40_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_CHOL_DEX)
pdcd_blood_V1_M_40_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_HDL_DEX)
pdcd_blood_V1_M_40_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_TG_DEX)
pdcd_blood_V1_M_40_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_M_40_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_34 <- apriori(pdcd_blood_V1_M_40_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_34
rules_34_lift <- sort(rules_34, by='lift', decreasing = TRUE)
inspect(rules_34_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_34_lift[3:4], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_40대_혈액검사
abs(log(1/740) - log(2/705))
pdcd_blood_V1_M_40$RISK <- abs(log(1/740) - log(2/705))

#####


#####여성_40대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_40_2 <- pdcd_blood_V1_W_40 %>% filter(fuPDCD == 2)
pdcd_blood_V1_W_40_2_x <- pdcd_blood_V1_W_40_2[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_40_2_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_WBC_DEX)
pdcd_blood_V1_W_40_2_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_RBC_DEX)
pdcd_blood_V1_W_40_2_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_HB_DEX)
pdcd_blood_V1_W_40_2_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_HCT_DEX)
pdcd_blood_V1_W_40_2_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_40_2_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_PLAT_DEX)
pdcd_blood_V1_W_40_2_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_ALT_DEX)
pdcd_blood_V1_W_40_2_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_AST_DEX)
pdcd_blood_V1_W_40_2_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_BUN_DEX)
pdcd_blood_V1_W_40_2_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_CR_DEX)
pdcd_blood_V1_W_40_2_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_GLU_DEX)
pdcd_blood_V1_W_40_2_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_CHOL_DEX)
pdcd_blood_V1_W_40_2_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_HDL_DEX)
pdcd_blood_V1_W_40_2_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_TG_DEX)
pdcd_blood_V1_W_40_2_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_40_2_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_35 <- apriori(pdcd_blood_V1_W_40_2_x, parameter = list(supp=0.70, conf=0.95, maxlen=3)); rules_35
rules_35_lift <- sort(rules_35, by='lift', decreasing = TRUE)
inspect(rules_35_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_35_lift[c(1:2, 5:10)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_혈액검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_40_1 <- pdcd_blood_V1_W_40 %>% filter(fuPDCD == 1)
pdcd_blood_V1_W_40_1_x <- pdcd_blood_V1_W_40_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_40_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_WBC_DEX)
pdcd_blood_V1_W_40_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_RBC_DEX)
pdcd_blood_V1_W_40_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_HB_DEX)
pdcd_blood_V1_W_40_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_HCT_DEX)
pdcd_blood_V1_W_40_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_40_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_PLAT_DEX)
pdcd_blood_V1_W_40_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_ALT_DEX)
pdcd_blood_V1_W_40_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_AST_DEX)
pdcd_blood_V1_W_40_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_BUN_DEX)
pdcd_blood_V1_W_40_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_CR_DEX)
pdcd_blood_V1_W_40_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_GLU_DEX)
pdcd_blood_V1_W_40_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_CHOL_DEX)
pdcd_blood_V1_W_40_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_HDL_DEX)
pdcd_blood_V1_W_40_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_TG_DEX)
pdcd_blood_V1_W_40_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_40_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_36 <- apriori(pdcd_blood_V1_W_40_1_x, parameter = list(supp=0.70, conf=0.95, maxlen=3)); rules_36
rules_36_lift <- sort(rules_36, by='lift', decreasing = TRUE)
inspect(rules_36_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_36_lift[2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_40대_혈액검사
abs(log(8/787) - log(1/718))
pdcd_blood_V1_W_40$RISK <- abs(log(8/787) - log(1/718))

#####


#####남성_50대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_50_2 <- pdcd_blood_V1_M_50 %>% filter(fuPDCD == 2)
pdcd_blood_V1_M_50_2_x <- pdcd_blood_V1_M_50_2[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_M_50_2_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_WBC_DEX)
pdcd_blood_V1_M_50_2_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_RBC_DEX)
pdcd_blood_V1_M_50_2_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_HB_DEX)
pdcd_blood_V1_M_50_2_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_HCT_DEX)
pdcd_blood_V1_M_50_2_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_HBA1C_DEX)
pdcd_blood_V1_M_50_2_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_PLAT_DEX)
pdcd_blood_V1_M_50_2_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_ALT_DEX)
pdcd_blood_V1_M_50_2_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_AST_DEX)
pdcd_blood_V1_M_50_2_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_BUN_DEX)
pdcd_blood_V1_M_50_2_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_CR_DEX)
pdcd_blood_V1_M_50_2_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_GLU_DEX)
pdcd_blood_V1_M_50_2_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_CHOL_DEX)
pdcd_blood_V1_M_50_2_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_HDL_DEX)
pdcd_blood_V1_M_50_2_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_TG_DEX)
pdcd_blood_V1_M_50_2_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_M_50_2_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_37 <- apriori(pdcd_blood_V1_M_50_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_37
rules_37_lift <- sort(rules_37, by='lift', decreasing = TRUE)
inspect(rules_37_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_37_lift[1:8], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_혈액검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_50_1 <- pdcd_blood_V1_M_50 %>% filter(fuPDCD == 1)
pdcd_blood_V1_M_50_1_x <- pdcd_blood_V1_M_50_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_M_50_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_WBC_DEX)
pdcd_blood_V1_M_50_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_RBC_DEX)
pdcd_blood_V1_M_50_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_HB_DEX)
pdcd_blood_V1_M_50_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_HCT_DEX)
pdcd_blood_V1_M_50_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_M_50_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_PLAT_DEX)
pdcd_blood_V1_M_50_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_ALT_DEX)
pdcd_blood_V1_M_50_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_AST_DEX)
pdcd_blood_V1_M_50_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_BUN_DEX)
pdcd_blood_V1_M_50_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_CR_DEX)
pdcd_blood_V1_M_50_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_GLU_DEX)
pdcd_blood_V1_M_50_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_CHOL_DEX)
pdcd_blood_V1_M_50_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_HDL_DEX)
pdcd_blood_V1_M_50_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_TG_DEX)
pdcd_blood_V1_M_50_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_M_50_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_38 <- apriori(pdcd_blood_V1_M_50_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_38
rules_38_lift <- sort(rules_38, by='lift', decreasing = TRUE)
inspect(rules_38_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_38_lift[4], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_50대_혈액검사
abs(log(8/773) - log(1/724))
pdcd_blood_V1_M_50$RISK <- abs(log(8/773) - log(1/724))


#####

#####여성_50대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_50_2 <- pdcd_blood_V1_W_50 %>% filter(fuPDCD == 2)
pdcd_blood_V1_W_50_2_x <- pdcd_blood_V1_W_50_2[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_50_2_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_WBC_DEX)
pdcd_blood_V1_W_50_2_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_RBC_DEX)
pdcd_blood_V1_W_50_2_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_HB_DEX)
pdcd_blood_V1_W_50_2_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_HCT_DEX)
pdcd_blood_V1_W_50_2_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_50_2_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_PLAT_DEX)
pdcd_blood_V1_W_50_2_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_ALT_DEX)
pdcd_blood_V1_W_50_2_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_AST_DEX)
pdcd_blood_V1_W_50_2_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_BUN_DEX)
pdcd_blood_V1_W_50_2_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_CR_DEX)
pdcd_blood_V1_W_50_2_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_GLU_DEX)
pdcd_blood_V1_W_50_2_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_CHOL_DEX)
pdcd_blood_V1_W_50_2_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_HDL_DEX)
pdcd_blood_V1_W_50_2_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_TG_DEX)
pdcd_blood_V1_W_50_2_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_50_2_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_39 <- apriori(pdcd_blood_V1_W_50_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_39
rules_39_lift <- sort(rules_39, by='lift', decreasing = TRUE)
inspect(rules_39_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_39_lift[c(1, 9)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_혈액검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_50_1 <- pdcd_blood_V1_W_50 %>% filter(fuPDCD == 1)
pdcd_blood_V1_W_50_1_x <- pdcd_blood_V1_W_50_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_50_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_WBC_DEX)
pdcd_blood_V1_W_50_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_RBC_DEX)
pdcd_blood_V1_W_50_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_HB_DEX)
pdcd_blood_V1_W_50_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_HCT_DEX)
pdcd_blood_V1_W_50_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_50_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_PLAT_DEX)
pdcd_blood_V1_W_50_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_ALT_DEX)
pdcd_blood_V1_W_50_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_AST_DEX)
pdcd_blood_V1_W_50_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_BUN_DEX)
pdcd_blood_V1_W_50_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_CR_DEX)
pdcd_blood_V1_W_50_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_GLU_DEX)
pdcd_blood_V1_W_50_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_CHOL_DEX)
pdcd_blood_V1_W_50_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_HDL_DEX)
pdcd_blood_V1_W_50_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_TG_DEX)
pdcd_blood_V1_W_50_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_50_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_40 <- apriori(pdcd_blood_V1_W_50_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_40
rules_40_lift <- sort(rules_40, by='lift', decreasing = TRUE)
inspect(rules_40_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_40_lift[9], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_50대_혈액검사
abs(log(2/126) - log(1/181))
pdcd_blood_V1_W_50$RISK <- abs(log(2/126) - log(1/181))

#####


#####남성_60대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_60_2 <- pdcd_blood_V1_M_60 %>% filter(fuPDCD == 2)
pdcd_blood_V1_M_60_2_x <- pdcd_blood_V1_M_60_2[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_M_60_2_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_WBC_DEX)
pdcd_blood_V1_M_60_2_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_RBC_DEX)
pdcd_blood_V1_M_60_2_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_HB_DEX)
pdcd_blood_V1_M_60_2_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_HCT_DEX)
pdcd_blood_V1_M_60_2_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_HBA1C_DEX)
pdcd_blood_V1_M_60_2_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_PLAT_DEX)
pdcd_blood_V1_M_60_2_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_ALT_DEX)
pdcd_blood_V1_M_60_2_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_AST_DEX)
pdcd_blood_V1_M_60_2_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_BUN_DEX)
pdcd_blood_V1_M_60_2_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_CR_DEX)
pdcd_blood_V1_M_60_2_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_GLU_DEX)
pdcd_blood_V1_M_60_2_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_CHOL_DEX)
pdcd_blood_V1_M_60_2_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_HDL_DEX)
pdcd_blood_V1_M_60_2_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_TG_DEX)
pdcd_blood_V1_M_60_2_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_M_60_2_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_41 <- apriori(pdcd_blood_V1_M_60_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_41
rules_41_lift <- sort(rules_41, by='lift', decreasing = TRUE)
inspect(rules_41_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_41_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_혈액검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_60_1 <- pdcd_blood_V1_M_60 %>% filter(fuPDCD == 1)
pdcd_blood_V1_M_60_1_x <- pdcd_blood_V1_M_60_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_M_60_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_WBC_DEX)
pdcd_blood_V1_M_60_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_RBC_DEX)
pdcd_blood_V1_M_60_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_HB_DEX)
pdcd_blood_V1_M_60_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_HCT_DEX)
pdcd_blood_V1_M_60_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_M_60_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_PLAT_DEX)
pdcd_blood_V1_M_60_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_ALT_DEX)
pdcd_blood_V1_M_60_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_AST_DEX)
pdcd_blood_V1_M_60_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_BUN_DEX)
pdcd_blood_V1_M_60_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_CR_DEX)
pdcd_blood_V1_M_60_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_GLU_DEX)
pdcd_blood_V1_M_60_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_CHOL_DEX)
pdcd_blood_V1_M_60_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_HDL_DEX)
pdcd_blood_V1_M_60_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_TG_DEX)
pdcd_blood_V1_M_60_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_M_60_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_42 <- apriori(pdcd_blood_V1_M_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_42
rules_42_lift <- sort(rules_42, by='lift', decreasing = TRUE)
inspect(rules_42_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_42_lift[3], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_60대_혈액검사
abs(log(1/483) - log(1/683))
pdcd_blood_V1_M_60$RISK <- abs(log(1/483) - log(1/683))

#####


#####여성_60대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_60_2 <- pdcd_blood_V1_W_60 %>% filter(fuPDCD == 2)
pdcd_blood_V1_W_60_2_x <- pdcd_blood_V1_W_60_2[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_60_2_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_WBC_DEX)
pdcd_blood_V1_W_60_2_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_RBC_DEX)
pdcd_blood_V1_W_60_2_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_HB_DEX)
pdcd_blood_V1_W_60_2_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_HCT_DEX)
pdcd_blood_V1_W_60_2_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_60_2_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_PLAT_DEX)
pdcd_blood_V1_W_60_2_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_ALT_DEX)
pdcd_blood_V1_W_60_2_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_AST_DEX)
pdcd_blood_V1_W_60_2_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_BUN_DEX)
pdcd_blood_V1_W_60_2_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_CR_DEX)
pdcd_blood_V1_W_60_2_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_GLU_DEX)
pdcd_blood_V1_W_60_2_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_CHOL_DEX)
pdcd_blood_V1_W_60_2_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_HDL_DEX)
pdcd_blood_V1_W_60_2_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_TG_DEX)
pdcd_blood_V1_W_60_2_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_60_2_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_43 <- apriori(pdcd_blood_V1_W_60_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_43
rules_43_lift <- sort(rules_43, by='lift', decreasing = TRUE)
inspect(rules_43_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_39_lift[c(1:4)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_혈액검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_60_1 <- pdcd_blood_V1_W_60 %>% filter(fuPDCD == 1)
pdcd_blood_V1_W_60_1_x <- pdcd_blood_V1_W_60_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_60_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_WBC_DEX)
pdcd_blood_V1_W_60_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_RBC_DEX)
pdcd_blood_V1_W_60_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_HB_DEX)
pdcd_blood_V1_W_60_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_HCT_DEX)
pdcd_blood_V1_W_60_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_60_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_PLAT_DEX)
pdcd_blood_V1_W_60_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_ALT_DEX)
pdcd_blood_V1_W_60_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_AST_DEX)
pdcd_blood_V1_W_60_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_BUN_DEX)
pdcd_blood_V1_W_60_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_CR_DEX)
pdcd_blood_V1_W_60_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_GLU_DEX)
pdcd_blood_V1_W_60_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_CHOL_DEX)
pdcd_blood_V1_W_60_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_HDL_DEX)
pdcd_blood_V1_W_60_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_TG_DEX)
pdcd_blood_V1_W_60_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_60_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_44 <- apriori(pdcd_blood_V1_W_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_44
rules_44_lift <- sort(rules_44, by='lift', decreasing = TRUE)
inspect(rules_44_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_40_lift[6:7], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_60대_혈액검사
abs(log(4/798) - log(2/811))
pdcd_blood_V1_W_60$RISK <- abs(log(4/798) - log(2/811))

#####


#####남성_70대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_70_2 <- pdcd_blood_V1_M_70 %>% filter(fuPDCD == 2)
pdcd_blood_V1_M_70_2_x <- pdcd_blood_V1_M_70_2[,19:33]

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_M_70_1 <- pdcd_blood_V1_M_70 %>% filter(fuPDCD == 1)
pdcd_blood_V1_M_70_1_x <- pdcd_blood_V1_M_70_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_M_70_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_WBC_DEX)
pdcd_blood_V1_M_70_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_RBC_DEX)
pdcd_blood_V1_M_70_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_HB_DEX)
pdcd_blood_V1_M_70_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_HCT_DEX)
pdcd_blood_V1_M_70_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_M_70_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_PLAT_DEX)
pdcd_blood_V1_M_70_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_ALT_DEX)
pdcd_blood_V1_M_70_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_AST_DEX)
pdcd_blood_V1_M_70_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_BUN_DEX)
pdcd_blood_V1_M_70_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_CR_DEX)
pdcd_blood_V1_M_70_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_GLU_DEX)
pdcd_blood_V1_M_70_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_CHOL_DEX)
pdcd_blood_V1_M_70_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_HDL_DEX)
pdcd_blood_V1_M_70_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_TG_DEX)
pdcd_blood_V1_M_70_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_M_70_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_46 <- apriori(pdcd_blood_V1_M_70_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_46
rules_46_lift <- sort(rules_46, by='lift', decreasing = TRUE)
inspect(rules_46_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_46_lift[3], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_남성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_70대_혈액검사
abs(log(1/295))
pdcd_blood_V1_M_70$RISK <- abs(log(1/295))

#####


#####여성_70대_혈액검사_장바구니분석#####
##관상동맥 발병_2(예)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_70_2 <- pdcd_blood_V1_W_70 %>% filter(fuPDCD == 2)
pdcd_blood_V1_W_70_2_x <- pdcd_blood_V1_W_70_2[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_70_2_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_WBC_DEX)
pdcd_blood_V1_W_70_2_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_RBC_DEX)
pdcd_blood_V1_W_70_2_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_HB_DEX)
pdcd_blood_V1_W_70_2_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_HCT_DEX)
pdcd_blood_V1_W_70_2_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_70_2_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_PLAT_DEX)
pdcd_blood_V1_W_70_2_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_ALT_DEX)
pdcd_blood_V1_W_70_2_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_AST_DEX)
pdcd_blood_V1_W_70_2_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_BUN_DEX)
pdcd_blood_V1_W_70_2_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_CR_DEX)
pdcd_blood_V1_W_70_2_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_GLU_DEX)
pdcd_blood_V1_W_70_2_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_CHOL_DEX)
pdcd_blood_V1_W_70_2_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_HDL_DEX)
pdcd_blood_V1_W_70_2_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_TG_DEX)
pdcd_blood_V1_W_70_2_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_70_2_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_47 <- apriori(pdcd_blood_V1_W_70_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_47
rules_47_lift <- sort(rules_47, by='lift', decreasing = TRUE)
inspect(rules_43_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_47_lift[c(1:4)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_혈액검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
#fuPDCD 기준으로 데이터 나누기
pdcd_blood_V1_W_70_1 <- pdcd_blood_V1_W_70 %>% filter(fuPDCD == 1)
pdcd_blood_V1_W_70_1_x <- pdcd_blood_V1_W_70_1[,19:33]
#####팩터형으로 변환##### 
pdcd_blood_V1_W_70_1_x$V1_WBC_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_WBC_DEX)
pdcd_blood_V1_W_70_1_x$V1_RBC_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_RBC_DEX)
pdcd_blood_V1_W_70_1_x$V1_HB_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_HB_DEX)
pdcd_blood_V1_W_70_1_x$V1_HCT_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_HCT_DEX)
pdcd_blood_V1_W_70_1_x$V1_HBA1C_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_HBA1C_DEX)
pdcd_blood_V1_W_70_1_x$V1_PLAT_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_PLAT_DEX)
pdcd_blood_V1_W_70_1_x$V1_ALT_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_ALT_DEX)
pdcd_blood_V1_W_70_1_x$V1_AST_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_AST_DEX)
pdcd_blood_V1_W_70_1_x$V1_BUN_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_BUN_DEX)
pdcd_blood_V1_W_70_1_x$V1_CR_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_CR_DEX)
pdcd_blood_V1_W_70_1_x$V1_GLU_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_GLU_DEX)
pdcd_blood_V1_W_70_1_x$V1_CHOL_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_CHOL_DEX)
pdcd_blood_V1_W_70_1_x$V1_HDL_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_HDL_DEX)
pdcd_blood_V1_W_70_1_x$V1_TG_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_TG_DEX)
pdcd_blood_V1_W_70_1_x$V1_LDL_DEX <- as.factor(pdcd_blood_V1_W_70_1_x$V1_LDL_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_48 <- apriori(pdcd_blood_V1_W_70_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_48
rules_48_lift <- sort(rules_48, by='lift', decreasing = TRUE)
inspect(rules_48_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_48_lift[3:4], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_혈액검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_70대_혈액검사
abs(log(4/1027) - log(2/199))
pdcd_blood_V1_W_70$RISK <- abs(log(4/1027) - log(2/199))

#####


#####남성_40대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_M_40_2 <- pdcd_urine_V1_M_40 %>% filter(fuPDCD == 2)
pdcd_urine_V1_M_40_2_x <- pdcd_urine_V1_M_40_2[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_M_40_2_x$V1_UNITR <- as.factor(pdcd_urine_V1_M_40_2_x$V1_UNITR)
pdcd_urine_V1_M_40_2_x$V1_USG <- as.factor(pdcd_urine_V1_M_40_2_x$V1_USG)
pdcd_urine_V1_M_40_2_x$V1_UPRO <- as.factor(pdcd_urine_V1_M_40_2_x$V1_UPRO)
pdcd_urine_V1_M_40_2_x$V1_UGLU <- as.factor(pdcd_urine_V1_M_40_2_x$V1_UGLU)
pdcd_urine_V1_M_40_2_x$V1_UKET <- as.factor(pdcd_urine_V1_M_40_2_x$V1_UKET)
pdcd_urine_V1_M_40_2_x$V1_UBIL <- as.factor(pdcd_urine_V1_M_40_2_x$V1_UBIL)
pdcd_urine_V1_M_40_2_x$V1_UBLD <- as.factor(pdcd_urine_V1_M_40_2_x$V1_UBLD)
pdcd_urine_V1_M_40_2_x$V1_URO <- as.factor(pdcd_urine_V1_M_40_2_x$V1_URO)
pdcd_urine_V1_M_40_2_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_M_40_2_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_49 <- apriori(pdcd_urine_V1_M_40_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_49
rules_49_lift <- sort(rules_49, by='lift', decreasing = TRUE)
inspect(rules_49_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_49_lift[3], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_소변검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_M_40_1 <- pdcd_urine_V1_M_40 %>% filter(fuPDCD == 1)
pdcd_urine_V1_M_40_1_x <- pdcd_urine_V1_M_40_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_M_40_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_M_40_1_x$V1_UNITR)
pdcd_urine_V1_M_40_1_x$V1_USG <- as.factor(pdcd_urine_V1_M_40_1_x$V1_USG)
pdcd_urine_V1_M_40_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_M_40_1_x$V1_UPRO)
pdcd_urine_V1_M_40_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_M_40_1_x$V1_UGLU)
pdcd_urine_V1_M_40_1_x$V1_UKET <- as.factor(pdcd_urine_V1_M_40_1_x$V1_UKET)
pdcd_urine_V1_M_40_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_M_40_1_x$V1_UBIL)
pdcd_urine_V1_M_40_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_M_40_1_x$V1_UBLD)
pdcd_urine_V1_M_40_1_x$V1_URO <- as.factor(pdcd_urine_V1_M_40_1_x$V1_URO)
pdcd_urine_V1_M_40_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_M_40_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_50 <- apriori(pdcd_urine_V1_M_40_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_50
rules_50_lift <- sort(rules_50, by='lift', decreasing = TRUE)
inspect(rules_50_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_50_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_40대_소변검사
abs(log(1/12) - log(1/36))
pdcd_urine_V1_M_40$RISK <- abs(log(1/12) - log(1/36))

#####


#####여성_40대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_W_40_2 <- pdcd_urine_V1_W_40 %>% filter(fuPDCD == 2)
pdcd_urine_V1_W_40_2_x <- pdcd_urine_V1_W_40_2[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_40_2_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_40_2_x$V1_UNITR)
pdcd_urine_V1_W_40_2_x$V1_USG <- as.factor(pdcd_urine_V1_W_40_2_x$V1_USG)
pdcd_urine_V1_W_40_2_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_40_2_x$V1_UPRO)
pdcd_urine_V1_W_40_2_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_40_2_x$V1_UGLU)
pdcd_urine_V1_W_40_2_x$V1_UKET <- as.factor(pdcd_urine_V1_W_40_2_x$V1_UKET)
pdcd_urine_V1_W_40_2_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_40_2_x$V1_UBIL)
pdcd_urine_V1_W_40_2_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_40_2_x$V1_UBLD)
pdcd_urine_V1_W_40_2_x$V1_URO <- as.factor(pdcd_urine_V1_W_40_2_x$V1_URO)
pdcd_urine_V1_W_40_2_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_40_2_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_51 <- apriori(pdcd_urine_V1_W_40_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_51
rules_51_lift <- sort(rules_51, by='lift', decreasing = TRUE)
inspect(rules_51_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_51_lift[1:6], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_소변검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_W_40_1 <- pdcd_urine_V1_W_40 %>% filter(fuPDCD == 1)
pdcd_urine_V1_W_40_1_x <- pdcd_urine_V1_W_40_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_40_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_40_1_x$V1_UNITR)
pdcd_urine_V1_W_40_1_x$V1_USG <- as.factor(pdcd_urine_V1_W_40_1_x$V1_USG)
pdcd_urine_V1_W_40_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_40_1_x$V1_UPRO)
pdcd_urine_V1_W_40_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_40_1_x$V1_UGLU)
pdcd_urine_V1_W_40_1_x$V1_UKET <- as.factor(pdcd_urine_V1_W_40_1_x$V1_UKET)
pdcd_urine_V1_W_40_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_40_1_x$V1_UBIL)
pdcd_urine_V1_W_40_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_40_1_x$V1_UBLD)
pdcd_urine_V1_W_40_1_x$V1_URO <- as.factor(pdcd_urine_V1_W_40_1_x$V1_URO)
pdcd_urine_V1_W_40_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_40_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_52 <- apriori(pdcd_urine_V1_W_40_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_52
rules_52_lift <- sort(rules_52, by='lift', decreasing = TRUE)
inspect(rules_52_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_52_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_40대_소변검사
abs(log(6/32) - log(1/42))
pdcd_urine_V1_W_40$RISK <- abs(log(6/32) - log(1/42))

#####


#####남성_50대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_M_50_2 <- pdcd_urine_V1_M_50 %>% filter(fuPDCD == 2)
pdcd_urine_V1_M_50_2_x <- pdcd_urine_V1_M_50_2[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_M_50_2_x$V1_UNITR <- as.factor(pdcd_urine_V1_M_50_2_x$V1_UNITR)
pdcd_urine_V1_M_50_2_x$V1_USG <- as.factor(pdcd_urine_V1_M_50_2_x$V1_USG)
pdcd_urine_V1_M_50_2_x$V1_UPRO <- as.factor(pdcd_urine_V1_M_50_2_x$V1_UPRO)
pdcd_urine_V1_M_50_2_x$V1_UGLU <- as.factor(pdcd_urine_V1_M_50_2_x$V1_UGLU)
pdcd_urine_V1_M_50_2_x$V1_UKET <- as.factor(pdcd_urine_V1_M_50_2_x$V1_UKET)
pdcd_urine_V1_M_50_2_x$V1_UBIL <- as.factor(pdcd_urine_V1_M_50_2_x$V1_UBIL)
pdcd_urine_V1_M_50_2_x$V1_UBLD <- as.factor(pdcd_urine_V1_M_50_2_x$V1_UBLD)
pdcd_urine_V1_M_50_2_x$V1_URO <- as.factor(pdcd_urine_V1_M_50_2_x$V1_URO)
pdcd_urine_V1_M_50_2_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_M_50_2_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_53 <- apriori(pdcd_urine_V1_M_50_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_53
rules_53_lift <- sort(rules_53, by='lift', decreasing = TRUE)
inspect(rules_53_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_53_lift[c(1,2,7,8)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_소변검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_M_50_1 <- pdcd_urine_V1_M_50 %>% filter(fuPDCD == 1)
pdcd_urine_V1_M_50_1_x <- pdcd_urine_V1_M_50_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_M_50_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_M_50_1_x$V1_UNITR)
pdcd_urine_V1_M_50_1_x$V1_USG <- as.factor(pdcd_urine_V1_M_50_1_x$V1_USG)
pdcd_urine_V1_M_50_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_M_50_1_x$V1_UPRO)
pdcd_urine_V1_M_50_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_M_50_1_x$V1_UGLU)
pdcd_urine_V1_M_50_1_x$V1_UKET <- as.factor(pdcd_urine_V1_M_50_1_x$V1_UKET)
pdcd_urine_V1_M_50_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_M_50_1_x$V1_UBIL)
pdcd_urine_V1_M_50_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_M_50_1_x$V1_UBLD)
pdcd_urine_V1_M_50_1_x$V1_URO <- as.factor(pdcd_urine_V1_M_50_1_x$V1_URO)
pdcd_urine_V1_M_50_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_M_50_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_54 <- apriori(pdcd_urine_V1_M_50_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_54
rules_54_lift <- sort(rules_54, by='lift', decreasing = TRUE)
inspect(rules_54_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_54_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_50대_소변검사
abs(log(4/45) - log(1/32))
pdcd_urine_V1_M_50$RISK <- abs(log(4/45) - log(1/32))

#####


#####여성_50대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_W_50_2 <- pdcd_urine_V1_W_50 %>% filter(fuPDCD == 2)
pdcd_urine_V1_W_50_2_x <- pdcd_urine_V1_W_50_2[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_50_2_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_50_2_x$V1_UNITR)
pdcd_urine_V1_W_50_2_x$V1_USG <- as.factor(pdcd_urine_V1_W_50_2_x$V1_USG)
pdcd_urine_V1_W_50_2_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_50_2_x$V1_UPRO)
pdcd_urine_V1_W_50_2_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_50_2_x$V1_UGLU)
pdcd_urine_V1_W_50_2_x$V1_UKET <- as.factor(pdcd_urine_V1_W_50_2_x$V1_UKET)
pdcd_urine_V1_W_50_2_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_50_2_x$V1_UBIL)
pdcd_urine_V1_W_50_2_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_50_2_x$V1_UBLD)
pdcd_urine_V1_W_50_2_x$V1_URO <- as.factor(pdcd_urine_V1_W_50_2_x$V1_URO)
pdcd_urine_V1_W_50_2_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_50_2_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_55 <- apriori(pdcd_urine_V1_W_50_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_55
rules_55_lift <- sort(rules_55, by='lift', decreasing = TRUE)
inspect(rules_55_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_55_lift[1:6], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_소변검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_W_50_1 <- pdcd_urine_V1_W_50 %>% filter(fuPDCD == 1)
pdcd_urine_V1_W_50_1_x <- pdcd_urine_V1_W_50_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_50_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_50_1_x$V1_UNITR)
pdcd_urine_V1_W_50_1_x$V1_USG <- as.factor(pdcd_urine_V1_W_50_1_x$V1_USG)
pdcd_urine_V1_W_50_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_50_1_x$V1_UPRO)
pdcd_urine_V1_W_50_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_50_1_x$V1_UGLU)
pdcd_urine_V1_W_50_1_x$V1_UKET <- as.factor(pdcd_urine_V1_W_50_1_x$V1_UKET)
pdcd_urine_V1_W_50_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_50_1_x$V1_UBIL)
pdcd_urine_V1_W_50_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_50_1_x$V1_UBLD)
pdcd_urine_V1_W_50_1_x$V1_URO <- as.factor(pdcd_urine_V1_W_50_1_x$V1_URO)
pdcd_urine_V1_W_50_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_50_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_56 <- apriori(pdcd_urine_V1_W_50_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_56
rules_56_lift <- sort(rules_56, by='lift', decreasing = TRUE)
inspect(rules_56_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_56_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_50대_소변검사
abs(log(6/36) - log(1/42))
pdcd_urine_V1_W_50$RISK <- abs(log(6/36) - log(1/42))

#####


#####남성_60대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_M_60_2 <- pdcd_urine_V1_M_60 %>% filter(fuPDCD == 2)
pdcd_urine_V1_M_60_2_x <- pdcd_urine_V1_M_60_2[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_M_60_2_x$V1_UNITR <- as.factor(pdcd_urine_V1_M_60_2_x$V1_UNITR)
pdcd_urine_V1_M_60_2_x$V1_USG <- as.factor(pdcd_urine_V1_M_60_2_x$V1_USG)
pdcd_urine_V1_M_60_2_x$V1_UPRO <- as.factor(pdcd_urine_V1_M_60_2_x$V1_UPRO)
pdcd_urine_V1_M_60_2_x$V1_UGLU <- as.factor(pdcd_urine_V1_M_60_2_x$V1_UGLU)
pdcd_urine_V1_M_60_2_x$V1_UKET <- as.factor(pdcd_urine_V1_M_60_2_x$V1_UKET)
pdcd_urine_V1_M_60_2_x$V1_UBIL <- as.factor(pdcd_urine_V1_M_60_2_x$V1_UBIL)
pdcd_urine_V1_M_60_2_x$V1_UBLD <- as.factor(pdcd_urine_V1_M_60_2_x$V1_UBLD)
pdcd_urine_V1_M_60_2_x$V1_URO <- as.factor(pdcd_urine_V1_M_60_2_x$V1_URO)
pdcd_urine_V1_M_60_2_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_M_60_2_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_57 <- apriori(pdcd_urine_V1_M_60_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_57
rules_57_lift <- sort(rules_57, by='lift', decreasing = TRUE)
inspect(rules_57_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_57_lift[c(1,2,3)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_소변검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_M_60_1 <- pdcd_urine_V1_M_60 %>% filter(fuPDCD == 1)
pdcd_urine_V1_M_60_1_x <- pdcd_urine_V1_M_60_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_M_60_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_M_60_1_x$V1_UNITR)
pdcd_urine_V1_M_60_1_x$V1_USG <- as.factor(pdcd_urine_V1_M_60_1_x$V1_USG)
pdcd_urine_V1_M_60_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_M_60_1_x$V1_UPRO)
pdcd_urine_V1_M_60_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_M_60_1_x$V1_UGLU)
pdcd_urine_V1_M_60_1_x$V1_UKET <- as.factor(pdcd_urine_V1_M_60_1_x$V1_UKET)
pdcd_urine_V1_M_60_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_M_60_1_x$V1_UBIL)
pdcd_urine_V1_M_60_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_M_60_1_x$V1_UBLD)
pdcd_urine_V1_M_60_1_x$V1_URO <- as.factor(pdcd_urine_V1_M_60_1_x$V1_URO)
pdcd_urine_V1_M_60_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_M_60_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_58 <- apriori(pdcd_urine_V1_M_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_58
rules_58_lift <- sort(rules_58, by='lift', decreasing = TRUE)
inspect(rules_58_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_58_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_60대_소변검사
abs(log(3/11) - log(1/32))
pdcd_urine_V1_M_60$RISK <- abs(log(3/11) - log(1/32))

#####


#####여성_60대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_W_60_2 <- pdcd_urine_V1_W_60 %>% filter(fuPDCD == 2)
pdcd_urine_V1_W_60_2_x <- pdcd_urine_V1_W_60_2[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_60_2_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_60_2_x$V1_UNITR)
pdcd_urine_V1_W_60_2_x$V1_USG <- as.factor(pdcd_urine_V1_W_60_2_x$V1_USG)
pdcd_urine_V1_W_60_2_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_60_2_x$V1_UPRO)
pdcd_urine_V1_W_60_2_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_60_2_x$V1_UGLU)
pdcd_urine_V1_W_60_2_x$V1_UKET <- as.factor(pdcd_urine_V1_W_60_2_x$V1_UKET)
pdcd_urine_V1_W_60_2_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_60_2_x$V1_UBIL)
pdcd_urine_V1_W_60_2_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_60_2_x$V1_UBLD)
pdcd_urine_V1_W_60_2_x$V1_URO <- as.factor(pdcd_urine_V1_W_60_2_x$V1_URO)
pdcd_urine_V1_W_60_2_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_60_2_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_59 <- apriori(pdcd_urine_V1_W_60_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_59
rules_59_lift <- sort(rules_59, by='lift', decreasing = TRUE)
inspect(rules_59_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_59_lift[2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_소변검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_W_60_1 <- pdcd_urine_V1_W_60 %>% filter(fuPDCD == 1)
pdcd_urine_V1_W_60_1_x <- pdcd_urine_V1_W_60_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_60_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_60_1_x$V1_UNITR)
pdcd_urine_V1_W_60_1_x$V1_USG <- as.factor(pdcd_urine_V1_W_60_1_x$V1_USG)
pdcd_urine_V1_W_60_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_60_1_x$V1_UPRO)
pdcd_urine_V1_W_60_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_60_1_x$V1_UGLU)
pdcd_urine_V1_W_60_1_x$V1_UKET <- as.factor(pdcd_urine_V1_W_60_1_x$V1_UKET)
pdcd_urine_V1_W_60_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_60_1_x$V1_UBIL)
pdcd_urine_V1_W_60_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_60_1_x$V1_UBLD)
pdcd_urine_V1_W_60_1_x$V1_URO <- as.factor(pdcd_urine_V1_W_60_1_x$V1_URO)
pdcd_urine_V1_W_60_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_60_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_60 <- apriori(pdcd_urine_V1_W_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_60
rules_60_lift <- sort(rules_60, by='lift', decreasing = TRUE)
inspect(rules_60_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_60_lift[c(6,10)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_60대_소변검사
abs(log(1/21) - log(2/60))
pdcd_urine_V1_W_60$RISK <- abs(log(1/21) - log(2/60))

#####


#####남성_70대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_M_70_2 <- pdcd_urine_V1_M_70 %>% filter(fuPDCD == 2)
pdcd_urine_V1_M_70_2_x <- pdcd_urine_V1_M_70_2[,5:13]

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_M_70_1 <- pdcd_urine_V1_M_70 %>% filter(fuPDCD == 1)
pdcd_urine_V1_M_70_1_x <- pdcd_urine_V1_M_70_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_M_70_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_M_70_1_x$V1_UNITR)
pdcd_urine_V1_M_70_1_x$V1_USG <- as.factor(pdcd_urine_V1_M_70_1_x$V1_USG)
pdcd_urine_V1_M_70_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_M_70_1_x$V1_UPRO)
pdcd_urine_V1_M_70_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_M_70_1_x$V1_UGLU)
pdcd_urine_V1_M_70_1_x$V1_UKET <- as.factor(pdcd_urine_V1_M_70_1_x$V1_UKET)
pdcd_urine_V1_M_70_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_M_70_1_x$V1_UBIL)
pdcd_urine_V1_M_70_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_M_70_1_x$V1_UBLD)
pdcd_urine_V1_M_70_1_x$V1_URO <- as.factor(pdcd_urine_V1_M_70_1_x$V1_URO)
pdcd_urine_V1_M_70_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_M_70_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_62 <- apriori(pdcd_urine_V1_M_70_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_62
rules_62_lift <- sort(rules_62, by='lift', decreasing = TRUE)
inspect(rules_62_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_62_lift[10], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_남성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_70대_소변검사
abs(log(1/36))
pdcd_urine_V1_M_70$RISK <- abs(log(1/36))

#####

#####여성_70대_소변검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_urine_V1_W_70_2 <- pdcd_urine_V1_W_70 %>% filter(fuPDCD == 2)
pdcd_urine_V1_W_70_2_x <- pdcd_urine_V1_W_70_2[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_70_2_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_70_2_x$V1_UNITR)
pdcd_urine_V1_W_70_2_x$V1_USG <- as.factor(pdcd_urine_V1_W_70_2_x$V1_USG)
pdcd_urine_V1_W_70_2_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_70_2_x$V1_UPRO)
pdcd_urine_V1_W_70_2_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_70_2_x$V1_UGLU)
pdcd_urine_V1_W_70_2_x$V1_UKET <- as.factor(pdcd_urine_V1_W_70_2_x$V1_UKET)
pdcd_urine_V1_W_70_2_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_70_2_x$V1_UBIL)
pdcd_urine_V1_W_70_2_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_70_2_x$V1_UBLD)
pdcd_urine_V1_W_70_2_x$V1_URO <- as.factor(pdcd_urine_V1_W_70_2_x$V1_URO)
pdcd_urine_V1_W_70_2_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_70_2_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_63 <- apriori(pdcd_urine_V1_W_70_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_63
rules_63_lift <- sort(rules_63, by='lift', decreasing = TRUE)
inspect(rules_63_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_63_lift, method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_소변검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_urine_V1_W_70_1 <- pdcd_urine_V1_W_70 %>% filter(fuPDCD == 1)
pdcd_urine_V1_W_70_1_x <- pdcd_urine_V1_W_70_1[,5:13]
#####팩터형으로 변환##### 
pdcd_urine_V1_W_70_1_x$V1_UNITR <- as.factor(pdcd_urine_V1_W_70_1_x$V1_UNITR)
pdcd_urine_V1_W_70_1_x$V1_USG <- as.factor(pdcd_urine_V1_W_70_1_x$V1_USG)
pdcd_urine_V1_W_70_1_x$V1_UPRO <- as.factor(pdcd_urine_V1_W_70_1_x$V1_UPRO)
pdcd_urine_V1_W_70_1_x$V1_UGLU <- as.factor(pdcd_urine_V1_W_70_1_x$V1_UGLU)
pdcd_urine_V1_W_70_1_x$V1_UKET <- as.factor(pdcd_urine_V1_W_70_1_x$V1_UKET)
pdcd_urine_V1_W_70_1_x$V1_UBIL <- as.factor(pdcd_urine_V1_W_70_1_x$V1_UBIL)
pdcd_urine_V1_W_70_1_x$V1_UBLD <- as.factor(pdcd_urine_V1_W_70_1_x$V1_UBLD)
pdcd_urine_V1_W_70_1_x$V1_URO <- as.factor(pdcd_urine_V1_W_70_1_x$V1_URO)
pdcd_urine_V1_W_70_1_x$V1_UPH_DEX <- as.factor(pdcd_urine_V1_W_70_1_x$V1_UPH_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_64 <- apriori(pdcd_urine_V1_W_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_64
rules_64_lift <- sort(rules_64, by='lift', decreasing = TRUE)
inspect(rules_64_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_64_lift[c(6,10)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_소변검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_70대_소변검사
abs(log(55/55) - log(2/60))
pdcd_urine_V1_W_70$RISK <- abs(log(55/55) - log(2/60))

#####


#####남성_40대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_M_40_2 <- pdcd_body_V1_M_40 %>% filter(fuPDCD == 2)
pdcd_body_V1_M_40_2_x <- pdcd_body_V1_M_40_2[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_M_40_2_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_M_40_2_x$V1_SBP_DEX)
pdcd_body_V1_M_40_2_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_M_40_2_x$V1_DBP_DEX)
pdcd_body_V1_M_40_2_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_M_40_2_x$V1_WAIST_DEX)
pdcd_body_V1_M_40_2_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_M_40_2_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_65 <- apriori(pdcd_body_V1_M_40_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_65
rules_65_lift <- sort(rules_65, by='lift', decreasing = TRUE)
inspect(rules_65_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_65_lift[8], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_신체계측검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_M_40_1 <- pdcd_body_V1_M_40 %>% filter(fuPDCD == 1)
pdcd_body_V1_M_40_1_x <- pdcd_body_V1_M_40_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_M_40_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_M_40_1_x$V1_SBP_DEX)
pdcd_body_V1_M_40_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_M_40_1_x$V1_DBP_DEX)
pdcd_body_V1_M_40_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_M_40_1_x$V1_WAIST_DEX)
pdcd_body_V1_M_40_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_M_40_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_66 <- apriori(pdcd_body_V1_M_40_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_66
rules_66_lift <- sort(rules_66, by='lift', decreasing = TRUE)
inspect(rules_66_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_66_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_남성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_40대_신체계측검사
abs(log(1/10) - log(1/8))
pdcd_body_V1_M_40$RISK <- abs(log(1/10) - log(1/8))

#####


#####여성_40대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_W_40_2 <- pdcd_body_V1_W_40 %>% filter(fuPDCD == 2)
pdcd_body_V1_W_40_2_x <- pdcd_body_V1_W_40_2[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_40_2_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_40_2_x$V1_SBP_DEX)
pdcd_body_V1_W_40_2_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_40_2_x$V1_DBP_DEX)
pdcd_body_V1_W_40_2_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_40_2_x$V1_WAIST_DEX)
pdcd_body_V1_W_40_2_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_40_2_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_67 <- apriori(pdcd_body_V1_W_40_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_67
rules_67_lift <- sort(rules_67, by='lift', decreasing = TRUE)
inspect(rules_67_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_67_lift[2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_신체계측검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_W_40_1 <- pdcd_body_V1_W_40 %>% filter(fuPDCD == 1)
pdcd_body_V1_W_40_1_x <- pdcd_body_V1_W_40_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_40_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_40_1_x$V1_SBP_DEX)
pdcd_body_V1_W_40_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_40_1_x$V1_DBP_DEX)
pdcd_body_V1_W_40_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_40_1_x$V1_WAIST_DEX)
pdcd_body_V1_W_40_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_40_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_68 <- apriori(pdcd_body_V1_W_40_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_68
rules_68_lift <- sort(rules_68, by='lift', decreasing = TRUE)
inspect(rules_68_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_68_lift[4], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_40대_여성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_40대_신체계측검사
abs(log(1/6) - log(1/8))
pdcd_body_V1_W_40$RISK <- abs(log(1/6) - log(1/8))

#####


#####남성_50대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_M_50_2 <- pdcd_body_V1_M_50 %>% filter(fuPDCD == 2)
pdcd_body_V1_M_50_2_x <- pdcd_body_V1_M_50_2[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_M_50_2_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_M_50_2_x$V1_SBP_DEX)
pdcd_body_V1_M_50_2_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_M_50_2_x$V1_DBP_DEX)
pdcd_body_V1_M_50_2_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_M_50_2_x$V1_WAIST_DEX)
pdcd_body_V1_M_50_2_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_M_50_2_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_69 <- apriori(pdcd_body_V1_M_50_2_x, parameter = list(supp=0.60, conf=0.95, maxlen=3)); rules_69
rules_69_lift <- sort(rules_69, by='lift', decreasing = TRUE)
inspect(rules_69_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_69_lift[2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_신체계측검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_M_50_1 <- pdcd_body_V1_M_50 %>% filter(fuPDCD == 1)
pdcd_body_V1_M_50_1_x <- pdcd_body_V1_M_50_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_M_50_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_M_50_1_x$V1_SBP_DEX)
pdcd_body_V1_M_50_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_M_50_1_x$V1_DBP_DEX)
pdcd_body_V1_M_50_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_M_50_1_x$V1_WAIST_DEX)
pdcd_body_V1_M_50_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_M_50_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_70 <- apriori(pdcd_body_V1_M_50_1_x, parameter = list(supp=0.60, conf=0.95, maxlen=3)); rules_70
rules_70_lift <- sort(rules_70, by='lift', decreasing = TRUE)
inspect(rules_70_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_70_lift[2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_남성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_50대_신체계측검사
abs(log(1/3) - log(1/4))
pdcd_body_V1_M_50$RISK <- abs(log(1/3) - log(1/4))

#####


#####여성_50대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_W_50_2 <- pdcd_body_V1_W_50 %>% filter(fuPDCD == 2)
pdcd_body_V1_W_50_2_x <- pdcd_body_V1_W_50_2[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_50_2_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_50_2_x$V1_SBP_DEX)
pdcd_body_V1_W_50_2_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_50_2_x$V1_DBP_DEX)
pdcd_body_V1_W_50_2_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_50_2_x$V1_WAIST_DEX)
pdcd_body_V1_W_50_2_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_50_2_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_71 <- apriori(pdcd_body_V1_W_50_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_71
rules_71_lift <- sort(rules_71, by='lift', decreasing = TRUE)
inspect(rules_71_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_71_lift[c(8,9)], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_신체계측검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_W_50_1 <- pdcd_body_V1_W_50 %>% filter(fuPDCD == 1)
pdcd_body_V1_W_50_1_x <- pdcd_body_V1_W_50_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_50_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_50_1_x$V1_SBP_DEX)
pdcd_body_V1_W_50_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_50_1_x$V1_DBP_DEX)
pdcd_body_V1_W_50_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_50_1_x$V1_WAIST_DEX)
pdcd_body_V1_W_50_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_50_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_72 <- apriori(pdcd_body_V1_W_50_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_72
rules_72_lift <- sort(rules_72, by='lift', decreasing = TRUE)
inspect(rules_72_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_72_lift[3], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_50대_여성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_50대_신체계측검사
abs(log(2/12) - log(1/12))
pdcd_body_V1_W_50$RISK <- abs(log(2/12) - log(1/12))

#####


#####남성_60대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_M_60_2 <- pdcd_body_V1_M_60 %>% filter(fuPDCD == 2)
pdcd_body_V1_M_60_2_x <- pdcd_body_V1_M_60_2[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_M_60_2_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_M_60_2_x$V1_SBP_DEX)
pdcd_body_V1_M_60_2_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_M_60_2_x$V1_DBP_DEX)
pdcd_body_V1_M_60_2_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_M_60_2_x$V1_WAIST_DEX)
pdcd_body_V1_M_60_2_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_M_60_2_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_73 <- apriori(pdcd_body_V1_M_60_2_x, parameter = list(supp=0.40, conf=0.95, maxlen=3)); rules_73
rules_73_lift <- sort(rules_73, by='lift', decreasing = TRUE)
inspect(rules_73_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_73_lift[1:4], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_신체계측검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_M_60_1 <- pdcd_body_V1_M_60 %>% filter(fuPDCD == 1)
pdcd_body_V1_M_60_1_x <- pdcd_body_V1_M_60_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_M_60_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_M_60_1_x$V1_SBP_DEX)
pdcd_body_V1_M_60_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_M_60_1_x$V1_DBP_DEX)
pdcd_body_V1_M_60_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_M_60_1_x$V1_WAIST_DEX)
pdcd_body_V1_M_60_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_M_60_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_74 <- apriori(pdcd_body_V1_M_60_1_x, parameter = list(supp=0.40, conf=0.95, maxlen=3)); rules_74
rules_74_lift <- sort(rules_74, by='lift', decreasing = TRUE)
inspect(rules_74_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_74_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_남성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_50대_신체계측검사
abs(log(4/14) - log(1/4))
pdcd_body_V1_M_60$RISK <- abs(log(4/14) - log(1/4))

#####


#####여성_60대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_W_60_2 <- pdcd_body_V1_W_60 %>% filter(fuPDCD == 2)
pdcd_body_V1_W_60_2_x <- pdcd_body_V1_W_60_2[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_60_2_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_60_2_x$V1_SBP_DEX)
pdcd_body_V1_W_60_2_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_60_2_x$V1_DBP_DEX)
pdcd_body_V1_W_60_2_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_60_2_x$V1_WAIST_DEX)
pdcd_body_V1_W_60_2_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_60_2_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_75 <- apriori(pdcd_body_V1_W_60_2_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_75
rules_75_lift <- sort(rules_75, by='lift', decreasing = TRUE)
inspect(rules_75_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_75_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_신체계측검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_W_60_1 <- pdcd_body_V1_W_60 %>% filter(fuPDCD == 1)
pdcd_body_V1_W_60_1_x <- pdcd_body_V1_W_60_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_60_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_60_1_x$V1_SBP_DEX)
pdcd_body_V1_W_60_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_60_1_x$V1_DBP_DEX)
pdcd_body_V1_W_60_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_60_1_x$V1_WAIST_DEX)
pdcd_body_V1_W_60_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_60_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_76 <- apriori(pdcd_body_V1_W_60_1_x, parameter = list(supp=0.80, conf=0.95, maxlen=3)); rules_76
rules_76_lift <- sort(rules_76, by='lift', decreasing = TRUE)
inspect(rules_76_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_76_lift[2], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_60대_여성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_60대_신체계측검사
abs(log(1/6) - log(1/8))
pdcd_body_V1_W_60$RISK <- abs(log(1/6) - log(1/8))

#####


#####남성_70대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_M_70_2 <- pdcd_body_V1_M_70 %>% filter(fuPDCD == 2)
pdcd_body_V1_M_70_2_x <- pdcd_body_V1_M_70_2[,8:11]

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_M_70_1 <- pdcd_body_V1_M_70 %>% filter(fuPDCD == 1)
pdcd_body_V1_M_70_1_x <- pdcd_body_V1_M_70_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_M_70_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_M_70_1_x$V1_SBP_DEX)
pdcd_body_V1_M_70_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_M_70_1_x$V1_DBP_DEX)
pdcd_body_V1_M_70_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_M_70_1_x$V1_WAIST_DEX)
pdcd_body_V1_M_70_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_M_70_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_78 <- apriori(pdcd_body_V1_M_70_1_x, parameter = list(supp=0.38, conf=0.95, maxlen=3)); rules_78
rules_78_lift <- sort(rules_78, by='lift', decreasing = TRUE)
inspect(rules_78_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_78_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_남성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##남성_70대_신체계측검사
abs(log(1/1))
pdcd_body_V1_M_70$RISK <- abs(log(1/1))

#####


#####여성_70대_신체계측검사_장바구니분석#####
##관상동맥 발병_2(예)## 
pdcd_body_V1_W_70_2 <- pdcd_body_V1_W_70 %>% filter(fuPDCD == 2)
pdcd_body_V1_W_70_2_x <- pdcd_body_V1_W_70_2[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_70_2_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_70_2_x$V1_SBP_DEX)
pdcd_body_V1_W_70_2_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_70_2_x$V1_DBP_DEX)
pdcd_body_V1_W_70_2_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_70_2_x$V1_WAIST_DEX)
pdcd_body_V1_W_70_2_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_70_2_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_79 <- apriori(pdcd_body_V1_W_70_2_x, parameter = list(supp=0.24, conf=0.95, maxlen=3)); rules_79
rules_79_lift <- sort(rules_79, by='lift', decreasing = TRUE)
inspect(rules_79_lift[1:10])
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_79_lift, method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_신체계측검사_관상동맥 발병_2(예)")

#####

##관상동맥 발병_1(아니오)##
pdcd_body_V1_W_70_1 <- pdcd_body_V1_W_70 %>% filter(fuPDCD == 1)
pdcd_body_V1_W_70_1_x <- pdcd_body_V1_W_70_1[,8:11]
#####팩터형으로 변환##### 
pdcd_body_V1_W_70_1_x$V1_SBP_DEX <- as.factor(pdcd_body_V1_W_70_1_x$V1_SBP_DEX)
pdcd_body_V1_W_70_1_x$V1_DBP_DEX <- as.factor(pdcd_body_V1_W_70_1_x$V1_DBP_DEX)
pdcd_body_V1_W_70_1_x$V1_WAIST_DEX <- as.factor(pdcd_body_V1_W_70_1_x$V1_WAIST_DEX)
pdcd_body_V1_W_70_1_x$V1_BMI_DEX <- as.factor(pdcd_body_V1_W_70_1_x$V1_BMI_DEX)
#####연관규칙 분석#####
options(digits = 2)
#향상도 기준
rules_80 <- apriori(pdcd_body_V1_W_70_1_x, parameter = list(supp=0.24, conf=0.95, maxlen=3)); rules_80
rules_80_lift <- sort(rules_80, by='lift', decreasing = TRUE)
inspect(rules_80_lift)
#연관 분석 그래프 
par(mfrow=c(1,1))
plot(rules_80_lift[1], method = "graph", measure = "lift", control = list(type="items"),
     vertex.label.cex = 0.7, edge.arrow.size = 0.3, edge.arrow.width = 2,
     main = "V1_70대_여성_신체계측검사_관상동맥 발병_1(아니오)")

#####

##위험도 계산 및 추가
##여성_70대_신체계측검사
abs(log(43/43) - log(1/2))
pdcd_body_V1_W_70$RISK <- abs(log(43/43) - log(1/2))

#####
