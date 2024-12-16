#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#라이브러리
# install.packages("pacman")
pacman::p_load(dplyr,lubridate,ggplot2,caret,glmnet,psych)

#작업 디렉토리
setwd("D:\\")

#데이터 불러오기
dat<-read.csv("MI_21m_cohort_death_pulse.csv")

str(dat) 
dim(dat) #n=219,562, p=89 
names(dat)

#분석시 이용할 수정 변수명 불러오기
varname<-read.csv("ML_project_SNUPH_variables.csv")

#변수명 수정
names(dat)=varname$Variables

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#변수 순서 정렬
#결과변수, 인구사회학적 특징
var_g1<-dat %>% select(eid,MI,age,sex,BMI,race,race_rev,
                       education,
                       college_uni_degree,
                       Job_involves_shift_work,job_night_shift_work,
                       current_employment,
                       employment,
                       employment2,TDI)

#라이프스타일: 흡연, 음주, 식이, 운동, 수면
var_g2<-dat %>% select(eid,alcohol,smoking,PA_moderate:dried_fruit_intake,
                       leisure_time_screen_use:processed_meat_intake2,
                       METs:dozing)

#혈액 검사 지표
var_g3<-dat %>% select(eid,visit_date,SBP,DBP,pulse_rate,RBC_count,WBC_count,
                       ALT:VitaminD)

#기저 병력, 사망 등
var_g4<-dat %>% select(eid,cancer,Diabetes:days_until_death)

#자료 연계
dat_rev<-var_g1 %>% left_join(var_g2,by="eid") %>% left_join(var_g3,by="eid") %>% left_join(var_g4,by="eid")

#위 정리 자료 저장시
# write.csv(dat_rev,file="dat_rev.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#데이터 탐색
head(dat_rev)

names(dat_rev)

#개별 변수별 빈도 테이블 함수
func_cat<-function(x){
rbind(table(x),
      round(prop.table(table(x))*100,2))}

#개별 변수*결과변수(MI)별 빈도 테이블 함수
func_cat2<-function(x){
  rbind(table(dat$MI,x),
        round(prop.table(table(dat$MI,x))*100,2))}

#employment 변수 검토 필요
func_cat(dat_rev$employment)
func_cat(dat_rev$employment2)
ifelse(dat_rev$employment==dat_rev$employment2,T,F) %>% table
dat_rev[!ifelse(dat_rev$employment==dat_rev$employment2,T,F),] %>% select(eid,employment,employment2) %>% View
dat_rev[!ifelse(dat_rev$employment==dat_rev$employment2,T,F),] %>% select(eid,employment,employment2) %>% nrow
#범주형 자료 정리
dat_cat<-dat_rev %>% select(MI,sex,race:employment2,
                            alcohol:dried_fruit_intake,
                            vegetable_intake:processed_meat_intake2,METs_g,
                            computer_time,tv_time,sleep,wakeup:dozing,cancer:Hypertension)


freq1=NULL      #범주자료 빈도 저장할 객체
length(dat_cat) #43

#반복문 이용하여 테이블 빈도 저장 
for(i in 1:length(dat_cat)){
freq1[[i]]<-data.frame(category=names(dat_cat)[i],label=names(table(dat_cat[,i])),t(func_cat(dat_cat[,i])))
print(i)
}

#결과 리스트 저장
freq1<-do.call(rbind,freq1)

View(freq1)
#결과 저장
write.csv(freq1,file="freq1.csv",row.names=F,na="")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#연속형 자료 정리
dat_con<-dat_rev %>% select(age,BMI,TDI,METs,leisure_time_screen_use,healthy_sleep_score,
                            healthy_diet_score,SBP:VitaminD,days_until_death)


#개별 변수별 연속형 자료 기초 통계량 함수
func_con<-function(x){
cbind(as.data.frame(describe(x)),missing=nrow(dat_rev)-length(x),
      P1=quantile(x ,c(0.01)),
      P10=quantile(x,c(0.1)),
      P25=quantile(x,c(0.25)), 
      P50=quantile(x,c(0.50)),
      P75=quantile(x,c(0.75)),
      P90=quantile(x,c(0.90)),
      P99=quantile(x,c(0.99))) %>% select(n,missing,mean,sd,min,P1:P99,max,range,skew,kurtosis)
}

con1=NULL      #결과 자료 저장할 객체
length(dat_con) #43

#반복문 적용
for(i in 1:length(dat_con)){
  x<-na.omit(dat_con[,i]) #결측 존재시 삭제
  con1[[i]]<-func_con(x) %>% mutate(category=names(dat_con)[i]) %>% select(category,n:kurtosis)
  print(i)
}

#결과 리스트 저장
con1_res<-as.data.frame(do.call(rbind,con1))

#결과 저장
write.csv(con1_res,file="con1.csv",row.names=F,na="")
