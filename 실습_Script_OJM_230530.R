#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#errorbar, percentage change , 95% CI
setwd("C:\\Users\\kard1\\OneDrive\\바탕 화면\\대학원\\R")
dat<-read.csv("errorbar_example.csv")
head(dat)
str(dat)

x11();ggplot(dat,aes(x=Disease,y=est))+geom_point()+
  geom_errorbar(aes(ymin=lci,ymax=uci))

x11();ggplot(dat,aes(x=Disease,y=est))+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2)

dat$Trimester=factor(1:3)
x11();ggplot(dat,aes(x=Disease,y=est,group=Trimester))+
  geom_point(size=4,aes(shape=Trimester))+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2)+
  scale_shape_manual(values=c(16,17,15))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
x11();ggplot(dat,aes(x=Disease,y=est,group=Trimester))+
  geom_point(size=4,aes(shape=Trimester),
             position = position_dodge(0.5))+
  geom_errorbar(aes(ymin=lci,ymax=uci),
                width=0.2,
                position = position_dodge(0.5))+
  scale_shape_manual(values=c(16,17,15))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
ifelse(dat$uci<0,1,0)
signi_col=ifelse(dat$uci<0,"blue","black")
dat$Disease=factor(dat$Disease,levels=unique(dat$Disease))

x11();ggplot(dat,aes(x=Disease,y=est,group=Trimester))+
  geom_point(size=4,aes(shape=Trimester),
             position = position_dodge(0.5),
             col=signi_col)+
  geom_errorbar(aes(ymin=lci,ymax=uci),
                width=0.2,position = position_dodge(0.5),
                col=signi_col)+
  scale_shape_manual(values=c(16,17,15))+
  geom_hline(yintercept = 0,col="red")

x11();ggplot(dat,aes(x=Disease,y=est,group=Trimester))+
  geom_point(size=4,aes(shape=Trimester),position = position_dodge(0.5),
             col=signi_col)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,position = position_dodge(0.5),
                col=signi_col)+
  scale_shape_manual(values=c(16,17,15))+geom_hline(yintercept = 0,col="red")+
  labs(x="",y="Percentage change (95% CI)")+
  theme_gray(base_size=15)+
  theme(legend.position = "top",
        legend.title = element_blank())+coord_flip()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
dat2<-dat %>% arrange(Disease,desc(Trimester))
dat2$Disease=factor(dat2$Disease,levels=rev(unique(dat$Disease)))
signi_col=ifelse(dat2$uci<0,"blue","black")

x11();ggplot(dat2,aes(x=Disease,y=est,group=Trimester))+
  geom_point(size=4,aes(shape=Trimester),position = position_dodge(0.5),
             col=signi_col)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,position = position_dodge(0.5),
                col=signi_col)+
  scale_shape_manual(values=c(16,17,15))+geom_hline(yintercept = 0,col="red")+
  labs(x="",y="Percentage change (95% CI)",
       shape="Type of Trimester")+
  theme_gray(base_size=25)+
  theme(legend.position = "top",
        # legend.title=element_blank(),
  )+coord_flip()
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
pacman::p_load(sf,tidyverse,mapproj,ggplot2,sp,readxl,dplyr)

setwd("C:\\Users\\kard1\\OneDrive\\바탕 화면\\대학원\\R\\EMD_202005")
list.files()

korea_emd<- st_read('EMD.shp')
korea_emd

korea_emd<- st_read('EMD.shp',options = "ENCODING=euc-kr")
korea_emd
korea_emd_shp <-  as(korea_emd, 'Spatial')
korea_emd_shp

map_korea_emd <- fortify(korea_emd_shp)
#읍면동 지도 
x11();ggplot()+geom_polygon(data=map_korea_emd,
                            aes(x=long,y=lat,group=group),
                            fill="white",
                            col="black")+labs(x="",y="")+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

korea_emd
seoul_emd<-subset(korea_emd,substr(EMD_CD,1,2)=="11")
seoul_emd_shp <-  as(seoul_emd, 'Spatial')
seoul_emd_shp$EMD_CD
map_seoul_emd <- fortify(seoul_emd_shp)

x11();ggplot()+geom_polygon(data=map_seoul_emd,aes(x=long,y=lat,group=group),
                            fill="white",
                            col="black")+labs(x="",y="")+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

length(seoul_emd$EMD_CD)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
setwd("C:\\Users\\kard1\\OneDrive\\바탕 화면\\대학원\\R")
exp<-read_excel("request_seoul_merged_수정(220519).xlsx",sheet=2)
table(exp$year)

exp_16_20<-subset(exp,year>=2016)
table(exp_16_20$month)
#구별현황 - 행정동 426개동,법정동 467개
unique(exp_16_20$ADM_DR_CD) %>% length
head(exp_16_20)
str(exp_16_20)
table(exp_16_20$ADM_DR_NM)

#읍면동별 년월별 노출자료를 그룹별 평균 산출
head(exp_16_20)
agg1<-aggregate(pm25~ADM_DR_CD,data=exp_16_20,mean)
agg2<-aggregate(pm10~ADM_DR_CD,data=exp_16_20,mean)
agg3<-aggregate(no2 ~ADM_DR_CD,data=exp_16_20,mean)
agg4<-aggregate(co  ~ADM_DR_CD,data=exp_16_20,mean)
agg5<-aggregate(o3  ~ADM_DR_CD,data=exp_16_20,mean)

#연구기간(2016~2020년간) 읍면동 노출 자료 산출 
avg_exp<-cbind(agg1,pm10=agg2$pm10,no2=agg3$no2,co=agg4$co,o3=agg5$o3)

#다른 방법
avg_exp<-exp_16_20 %>% group_by(ADM_DR_CD) %>% summarise(pm25=mean(pm25),
                                                         pm10=mean(pm10),
                                                         no2 =mean(no2),
                                                         co  =mean(co),
                                                         o3  =mean(o3))

#지도 자료와 노출 자료 연계
length(unique(map_seoul_emd2$EMD_CD))

#서울시 읍면동 정리한 파일 불러오기
seoul_emd_code<-read_excel("seoul_emd_230527.xlsx",sheet=3)

seoul_emd_code2<-seoul_emd_code[,c(3,5)]
names(avg_exp)[1]="EMD_CD_Haengjeong"

#data merge
head(seoul_emd_code2,1)
head(avg_exp,1)
avg_exp2<-merge(seoul_emd_code2,avg_exp,by="EMD_CD_Haengjeong",all.x=T)

#읍면동(법정동동별 평균 산출
avg_exp3<-avg_exp2 %>% group_by(EMD_CD) %>% summarise(pm25=mean(pm25),
                                                      pm10=mean(pm10),
                                                      no2 =mean(no2),
                                                      co  =mean(co),
                                                      o3  =mean(o3)) 

seoul_emd
seoul_id<-data.frame(id=as.character(1:nrow(seoul_emd)),
                     EMD_ENG_NM=seoul_emd$EMD_ENG_NM,
                     EMD_KOR_NM=seoul_emd$EMD_KOR_NM,
                     EMD_CD=seoul_emd$EMD_CD)

map_seoul_emd
map_seoul_emd2<-map_seoul_emd %>% left_join(seoul_id,by="id")
map_seoul_emd2

final_dat<-merge(map_seoul_emd2,avg_exp3,by="EMD_CD",all.x=T)
View(final_dat)

x11();ggplot()+geom_polygon(data=final_dat,
                            aes(x=long,y=lat,group=group,fill=pm25),
                            col="black")+labs(x="",y="")+
  scale_fill_gradient(low="green",high="red",na.value="gray")+
  labs(fill=expression(paste(PM[2.5]," (",mu,g/m^3,")")))+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

quantile(avg_exp3$pm25,c(0.01,0.1,0.25,0.50,0.75,0.90,0.99))

#PM2.5 노출 농도 상위 90백분위수, 하위 10백분위수 
final_dat$PM25_high=ifelse(final_dat$pm25>=24.21342,1,0)
final_dat$PM25_low=ifelse(final_dat$pm25<=22.44190,1,0)

uniq_high_emd<-unique(subset(final_dat,PM25_high==1)$EMD_KOR_NM)
uniq_low_emd <-unique(subset(final_dat,PM25_low==1)$EMD_KOR_NM)
uniq_high_emd
uniq_low_emd

seoul_emd_high<-subset(seoul_emd_code,EMD_KOR_NM %in% uniq_high_emd)
seoul_emd_low <-subset(seoul_emd_code,EMD_KOR_NM %in% uniq_low_emd)

table(seoul_emd_high$SGG)
table(seoul_emd_low$SGG)

View(seoul_emd_high)

x11();ggplot()+geom_polygon(data=final_dat,aes(x=long,y=lat,group=group,fill=PM25_high),
                            col="black")+labs(x="",y="")+
  scale_fill_gradient(low="white",high="red",na.value="gray")+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position ="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

head(final_dat)
table(final_dat$PM25_high+final_dat$PM25_low)
x11();ggplot()+geom_polygon(data=final_dat,aes(x=long,y=lat,group=group,fill=PM25_low),
                            col="black")+labs(x="",y="")+
  scale_fill_gradient(low="white",high="darkgreen",na.value="gray")+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position ="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
#-----------------------------------------------------------------------------#