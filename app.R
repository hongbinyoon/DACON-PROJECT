library(ggplot2)
library(extrafont)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(glue)
library(ggthemes)
library(lubridate)
library(anytime)
library(stringi)
library(gganimate)
library(gifski)
library(data.table)
library(newsanchor)
library(readxl)
library(geojsonio)
library(leaflet)
library(plotly)
library(shinydashboard)
library(shiny)
library(leaflet)
library(DT)
library(echarts4r.assets)
library(dashboardthemes)
library(wordcloud2)
library(waffle)
library(bit64)


options(shiny.maxRequestSize = 30*1024^2)

#####환경불러오기######
load('cardnanum1.RData')
load('cardnanum2.RData')
load('namuge.RData')
load('patient.RData')

card<-rbind(cardnanum1,cardnanum2)

card <- filter(card,!str_detect(selng_cascnt, regex("[ㄱ-ㅎ가-힣]")))#텍스트 데이터 없애기
card <- filter(card,!str_detect(salamt, regex("[ㄱ-ㅎ가-힣]")))#텍스트 데이터 없애기
card$selng_cascnt<-as.numeric(card$selng_cascnt)#숫자 데이터로 바꾸기
card$salamt<-as.numeric(card$salamt)#숫자 데이터로 바꾸기
card$selng_cascnt<-abs(card$selng_cascnt)#음수값 처리하기 위해 절대값 처리해줌
card$salamt<-abs(card$salamt)#음수값 처리하기 위해 절대값 처리해줌
#card2<-filter(card,selng_cascnt<0)#음수값 처리됐는지 확인
#card2<-filter(card,salamt<0)#음수값 처리됐는지 확인


V2 <- substr(card$mrhst_induty_cl_code,1,1)
card<-cbind(card, V2)#industry코드의 앞부분은 카테고리를 뜻하므로 나눔
attach(card)
V2[V2=="1"]='여행&교통수단'
V2[V2=="2"]='스포츠&문화&여가'
V2[V2=="3"]='생활용품&주유'
V2[V2=="4"]='패션&쇼핑'
V2[V2=="5"]='교육&사무'
V2[V2=="6"]='차량&보험'
V2[V2=="7"]='의료&미용'
V2[V2=="8"]='식품&외식'
V2[V2=="9"]='기타'
detach(card)#attach해준것 다시 detach해줌, 위에는 카테고리된것 이름형으로 바꾼것


card<-cbind(card,V2)
            card<-cbind(card, substr(card$receipt_dttm,5,6))#데이터를 월별로 보고자, 20200225라면 02(월)만 빼서 변수화 시켜주고, 마찬가지로 열에 추가
            names(card) <- c('date', 'ad_code', 'ad_nm', 'ind_code', 'ind_nm', 'sal_cnt', 'sal_amt', 'cate_code','cate_name','month') #변수 이름 쉽게 바꿈
            
            
            
            
            #카테고리1,2,3,4, 각각 월별 sal_cnt(결제건수)의 합을 보기위함
            
            for (i in 1:9){
              assign(paste0("card",i),filter(card, cate_code == i))
            }
            #str(card)
            
            card$sal_cnt <- as.numeric(card$sal_cnt)
            card$sal_amt <- as.numeric(card$sal_amt)
            ############################바로 카테고리 나누고, 월별로 결제건수 합 구하기 ########################
            for (x in 1:9){
              A<-filter(card, cate_code == x)
              assign(paste0("category1",x),aggregate(sal_cnt~month,A,sum))
            }
            ################################완료#############################################################
            
            #카테고리1,2,3,4, 각각 월별 sal_amt(결제금액)의 합을 보기위함
            ###################################################################################################
            for (x in 1:9){
              A<-filter(card, cate_code == x)
              assign(paste0("category2",x),aggregate(sal_amt~month,A,sum))
            }
            ######################################################################################################
            #category1~로 시작되는것은 위에 의료,문화등으로 카테고리 해준것을 각각 데이터 셋으로 나누고, 월별 결제건수를 나타내준것
            #category2~로 시작되는것은 위에 의료,문화등으로 카테고리 해준것을 각각 데이터 셋으로 나누고, 월별 결제금약를 나타내준것
            ##########################################################################################
            
            
            card$date <- ymd(card$date)
            
            x <- card[,c("date","cate_code","sal_cnt","sal_amt")]
            
            x$category_name <- ifelse(x$cate_code=="1", '여행&교통수단',
                                      ifelse(x$cate_code=="2",'스포츠&문화&여가',
                                             ifelse(x$cate_code=="3",'생활용품&주유',
                                                    ifelse(x$cate_code=="4",'패션&쇼핑',
                                                           ifelse(x$cate_code=="5",'교육&사무',
                                                                  ifelse(x$cate_code=="6",'차량&보험',
                                                                         ifelse(x$cate_code=="7",'의료&미용',
                                                                                ifelse(x$cate_code=="8",'식품&외식','기타')
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
            )
            te <- card[,c("date","month","sal_amt","cate_code","cate_name")]
            
            sub1 <- subset(card, card$cate_code==1)
            sum_sub1 <- aggregate(sal_amt ~ date,sub1, sum)
            sum_sub1$month <- month(sum_sub1$date)
            sum_month1 <- aggregate(sal_amt ~ month, sum_sub1, sum)
            sum_month1[,3] <- as.character(1)
            colnames(sum_month1) <- c("month", "sum", "category")
            
            sub2 <- subset(card, card$cate_code==2)
            sum_sub2 <- aggregate(sal_amt ~ date,sub2, sum)
            sum_sub2$month <- month(sum_sub2$date)
            sum_month2 <- aggregate(sal_amt ~ month, sum_sub2, sum)
            sum_month2[,3] <- as.character(2)
            colnames(sum_month2) <- c("month", "sum", "category")
            
            sub3 <- subset(card, card$cate_code==3)
            sum_sub3 <- aggregate(sal_amt ~ date,sub3, sum)
            sum_sub3$month <- month(sum_sub3$date)
            sum_month3 <- aggregate(sal_amt ~ month, sum_sub3, sum)
            sum_month3[,3] <- as.character(3)
            colnames(sum_month3) <- c("month", "sum", "category")
            
            sub4 <- subset(card, card$cate_code==4)
            sum_sub4 <- aggregate(sal_amt ~ date,sub4, sum)
            sum_sub4$month <- month(sum_sub4$date)
            sum_month4 <- aggregate(sal_amt ~ month, sum_sub4, sum)
            sum_month4[,3] <- as.character(4)
            colnames(sum_month4) <- c("month", "sum", "category")
            
            sub5 <- subset(card, card$cate_code==5)
            sum_sub5 <- aggregate(sal_amt ~ date,sub5, sum)
            sum_sub5$month <- month(sum_sub5$date)
            sum_month5 <- aggregate(sal_amt ~ month, sum_sub5, sum)
            sum_month5[,3] <- as.character(5)
            colnames(sum_month5) <- c("month", "sum", "category")
            
            sub6 <- subset(card, card$cate_code==6)
            sum_sub6 <- aggregate(sal_amt ~ date,sub6, sum)
            sum_sub6$month <- month(sum_sub6$date)
            sum_month6 <- aggregate(sal_amt ~ month, sum_sub6, sum)
            sum_month6[,3] <- as.character(6)
            colnames(sum_month6) <- c("month", "sum", "category")
            
            sub7 <- subset(card, card$cate_code==7)
            sum_sub7 <- aggregate(sal_amt ~ date,sub7, sum)
            sum_sub7$month <- month(sum_sub7$date)
            sum_month7 <- aggregate(sal_amt ~ month, sum_sub7, sum)
            sum_month7[,3] <- as.character(7)
            colnames(sum_month7) <- c("month", "sum", "category")
            
            sub8 <- subset(card, card$cate_code==8)
            sum_sub8 <- aggregate(sal_amt ~ date,sub8, sum)
            sum_sub8$month <- month(sum_sub8$date)
            sum_month8 <- aggregate(sal_amt ~ month, sum_sub8, sum)
            sum_month8[,3] <- as.character(8)
            colnames(sum_month8) <- c("month", "sum", "category")
            
            sub9 <- subset(card, card$cate_code==9)
            sum_sub9 <- aggregate(sal_amt ~ date,sub9, sum)
            sum_sub9$month <- month(sum_sub9$date)
            sum_month9 <- aggregate(sal_amt ~ month, sum_sub9, sum)
            sum_month9[,3] <- as.character(9)
            colnames(sum_month9) <- c("month", "sum", "category")
            
            view_card <- card[,-10]
            colnames(view_card) <- c("날짜", "행정동코드","행정동명","가맹점업종코드","가맹점업종명","매출발생건수","매출발생금액","가맹점 카테고리","카테고리이름")
            
            
            
            #data loading#
            
            raw_seoul <- subset(raw_seoul, raw_seoul$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            seoul_t <- subset(raw_seoul, raw_seoul$`내/외국인`=="합계")
            seoul_t <- seoul_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            seoul_t <- as.data.frame(t(seoul_t))
            
            #column이름 변경
            colnames(seoul_t) <- seoul_t[1,]
            seoul_t <- seoul_t[-1,]
            
            ################################################################################################################
            #숫자 데이터에 천단위 구분기호에 의해 숫자로 인식하지 못함 해당 내역 제거##
            #각 컬럼 이름이 깨지는 현상 발생 naming 다시 진행#
            Alltime <- c("전체", "2018년", "2018-01-01", "2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01","2019년","2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020년","2020-01-01","2020-02-01","2020-03-01")
            year_month <- c("2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12","2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12","2020-01","2020-02","2020-03")
            yeartime <- c("전체", "2018년", "2019년", "2020년")
            monthtime <- c("2018-01-01", "2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01","2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01","2020-02-01","2020-03-01")
            
            numeric <- function(x){
              return(as.numeric(str_replace_all(x,',','')))
            }
            
            seoul_t <- as.data.frame(sapply(seoul_t,numeric))
            seoul_t[is.na(seoul_t)] <- 0
            
            #행이름 각 변경
            seoul_t <- cbind(Alltime, seoul_t)
            coln_seoul <- c("date",raw_seoul$관광지)
            colnames(seoul_t) <- coln_seoul
            
            #연간 방문객#
            seoul_year <- subset(seoul_t,seoul_t$date=="전체"|seoul_t$date=="2018년"|seoul_t$date=="2019년"|seoul_t$date=="2020년")
            seoul_month <- subset(seoul_t,seoul_t$date!="전체"&seoul_t$date!="2018년"&seoul_t$date!="2019년"&seoul_t$date!="2020년")
            
            #월간 관광지 방문객#
            seoul_visiter <- as.data.frame(cbind(year_month,rowSums(seoul_month[,-1]),c("seoul")))
            colnames(seoul_visiter) <- c("date", "visiter", "region")
            seoul_visiter$visiter <- as.numeric(seoul_visiter$visiter)
            
            
            
            
            ###########################################################################################################
            #타 데이터 동일시 진행#
            #부산#
            
            raw_Busan <- subset(raw_Busan, raw_Busan$`내/외국인`=="합계")
            
            
            #필요없는 행 제거#
            Busan_t <- subset(raw_Busan, raw_Busan$`내/외국인`=="합계")
            Busan_t <- Busan_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Busan_t <- as.data.frame(t(Busan_t))
            
            #column이름 변경
            colnames(Busan_t) <- Busan_t[1,]
            Busan_t <- Busan_t[-1,]
           
            Busan_t <- as.data.frame(sapply(Busan_t,numeric))
            Busan_t[is.na(Busan_t)] <- 0
            
            #행이름 각 변경
            Busan_t <- cbind(Alltime, Busan_t)
            coln_Busan <- c("date",raw_Busan$관광지)
            colnames(Busan_t) <- coln_Busan
            
            #연간 방문객#
            Busan_year <- subset(Busan_t,Busan_t$date=="전체"|Busan_t$date=="2018년"|Busan_t$date=="2019년"|Busan_t$date=="2020년")
            Busan_month <- subset(Busan_t,Busan_t$date!="전체"&Busan_t$date!="2018년"&Busan_t$date!="2019년"&Busan_t$date!="2020년")
            
            #월간 관광지 방문객#
            Busan_visiter <- as.data.frame(cbind(year_month,rowSums(Busan_month[,-1]),c("Busan")))
            colnames(Busan_visiter) <- c("date", "visiter", "region")
            Busan_visiter$visiter <- as.numeric(Busan_visiter$visiter)
            
            
            
            ###########################################################################################################
            #충북#
            
            raw_ChungBuk <- subset(raw_ChungBuk, raw_ChungBuk$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            ChungBuk_t <- subset(raw_ChungBuk, raw_ChungBuk$`내/외국인`=="합계")
            ChungBuk_t <- ChungBuk_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            ChungBuk_t <- as.data.frame(t(ChungBuk_t))
            
            #column이름 변경
            colnames(ChungBuk_t) <- ChungBuk_t[1,]
            ChungBuk_t <- ChungBuk_t[-1,]
            
            ChungBuk_t <- as.data.frame(sapply(ChungBuk_t,numeric))
            ChungBuk_t[is.na(ChungBuk_t)] <- 0
            
            #행이름 각 변경
            ChungBuk_t <- cbind(Alltime, ChungBuk_t)
            coln_ChungBuk <- c("date",raw_ChungBuk$관광지)
            colnames(ChungBuk_t) <- coln_ChungBuk
            
            #연간 방문객#
            ChungBuk_year <- subset(ChungBuk_t,ChungBuk_t$date=="전체"|ChungBuk_t$date=="2018년"|ChungBuk_t$date=="2019년"|ChungBuk_t$date=="2020년")
            ChungBuk_month <- subset(ChungBuk_t,ChungBuk_t$date!="전체"&ChungBuk_t$date!="2018년"&ChungBuk_t$date!="2019년"&ChungBuk_t$date!="2020년")
            
            #월간 서울 관광지 방문객#
            ChungBuk_visiter <- as.data.frame(cbind(year_month,rowSums(ChungBuk_month[,-1]),c("ChungBuk")))
            colnames(ChungBuk_visiter) <- c("date", "visiter", "region")
            ChungBuk_visiter$visiter <- as.numeric(ChungBuk_visiter$visiter)
            
            
            
            ###########################################################################################################
            #충남#
            raw_ChungNam <- subset(raw_ChungNam, raw_ChungNam$`내/외국인`=="합계")
            
            
            #필요없는 행 제거#
            ChungNam_t <- subset(raw_ChungNam, raw_ChungNam$`내/외국인`=="합계")
            ChungNam_t <- ChungNam_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            ChungNam_t <- as.data.frame(t(ChungNam_t))
            
            #column이름 변경
            colnames(ChungNam_t) <- ChungNam_t[1,]
            ChungNam_t <- ChungNam_t[-1,]
            
            ChungNam_t <- as.data.frame(sapply(ChungNam_t,numeric))
            ChungNam_t[is.na(ChungNam_t)] <- 0
            
            #행이름 각 변경
            ChungNam_t <- cbind(Alltime, ChungNam_t)
            coln_ChungNam <- c("date",raw_ChungNam$관광지)
            colnames(ChungNam_t) <- coln_ChungNam
            
            #연간 방문객#
            ChungNam_year <- subset(ChungNam_t,ChungNam_t$date=="전체"|ChungNam_t$date=="2018년"|ChungNam_t$date=="2019년"|ChungNam_t$date=="2020년")
            ChungNam_month <- subset(ChungNam_t,ChungNam_t$date!="전체"&ChungNam_t$date!="2018년"&ChungNam_t$date!="2019년"&ChungNam_t$date!="2020년")
            
            #월간 관광지 방문객#
            ChungNam_visiter <- as.data.frame(cbind(year_month,rowSums(ChungNam_month[,-1]),c("ChungNam")))
            colnames(ChungNam_visiter) <- c("date", "visiter", "region")
            ChungNam_visiter$visiter <- as.numeric(ChungNam_visiter$visiter)
            
            
            
            ###########################################################################################################
            #대구#
            
            raw_Daegu <- subset(raw_Daegu, raw_Daegu$`내/외국인`=="합계")
            
            
            
            #필요없는 행 제거#
            Daegu_t <- subset(raw_Daegu, raw_Daegu$`내/외국인`=="합계")
            Daegu_t <- Daegu_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Daegu_t <- as.data.frame(t(Daegu_t))
            
            #column이름 변경
            colnames(Daegu_t) <- Daegu_t[1,]
            Daegu_t <- Daegu_t[-1,]
            
            Daegu_t <- as.data.frame(sapply(Daegu_t,numeric))
            Daegu_t[is.na(Daegu_t)] <- 0
            
            #행이름 각 변경
            Daegu_t <- cbind(Alltime, Daegu_t)
            coln_Daegu <- c("date",raw_Daegu$관광지)
            colnames(Daegu_t) <- coln_Daegu
            
            #연간 방문객#
            Daegu_year <- subset(Daegu_t,Daegu_t$date=="전체"|Daegu_t$date=="2018년"|Daegu_t$date=="2019년"|Daegu_t$date=="2020년")
            Daegu_month <- subset(Daegu_t,Daegu_t$date!="전체"&Daegu_t$date!="2018년"&Daegu_t$date!="2019년"&Daegu_t$date!="2020년")
            
            #월간 관광지 방문객#
            Daegu_visiter <- as.data.frame(cbind(year_month,rowSums(Daegu_month[,-1]),c("Daegu")))
            colnames(Daegu_visiter) <- c("date", "visiter", "region")
            Daegu_visiter$visiter <- as.numeric(Daegu_visiter$visiter)
            
            
            
            ###########################################################################################################
            #대전#
            
            raw_Daejeon <- subset(raw_Daejeon, raw_Daejeon$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Daejeon_t <- subset(raw_Daejeon, raw_Daejeon$`내/외국인`=="합계")
            Daejeon_t <- Daejeon_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Daejeon_t <- as.data.frame(t(Daejeon_t))
            
            #column이름 변경
            colnames(Daejeon_t) <- Daejeon_t[1,]
            Daejeon_t <- Daejeon_t[-1,]
            
            Daejeon_t<- as.data.frame(sapply(Daejeon_t,numeric))
            Daejeon_t[is.na(Daejeon_t)] <- 0
            
            #행이름 각 변경
            Daejeon_t <- cbind(Alltime, Daejeon_t)
            coln_Daejeon <- c("date",raw_Daejeon$관광지)
            colnames(Daejeon_t) <- coln_Daejeon
            
            #연간 방문객#
            Daejeon_year <- subset(Daejeon_t,Daejeon_t$date=="전체"|Daejeon_t$date=="2018년"|Daejeon_t$date=="2019년"|Daejeon_t$date=="2020년")
            Daejeon_month <- subset(Daejeon_t,Daejeon_t$date!="전체"&Daejeon_t$date!="2018년"&Daejeon_t$date!="2019년"&Daejeon_t$date!="2020년")
            
            #월간 관광지 방문객#
            Daejeon_visiter <- as.data.frame(cbind(year_month,rowSums(Daejeon_month[,-1]),c("Daejeon")))
            colnames(Daejeon_visiter) <- c("date", "visiter", "region")
            Daejeon_visiter$visiter <- as.numeric(Daejeon_visiter$visiter)
            
            
            
            ###########################################################################################################
            #강원#
            
            raw_Gangwon <- subset(raw_Gangwon, raw_Gangwon$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Gangwon_t<- subset(raw_Gangwon, raw_Gangwon$`내/외국인`=="합계")
            Gangwon_t <- Gangwon_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Gangwon_t <- as.data.frame(t(Gangwon_t))
            
            #column이름 변경
            colnames(Gangwon_t) <- Gangwon_t[1,]
            Gangwon_t <- Gangwon_t[-1,]
            
            Gangwon_t <- as.data.frame(sapply(Gangwon_t,numeric))
            Gangwon_t[is.na(Gangwon_t)] <- 0
            
            #행이름 각 변경
            Gangwon_t <- cbind(Alltime, Gangwon_t)
            coln_Gangwon <- c("date",raw_Gangwon$관광지)
            colnames(Gangwon_t) <- coln_Gangwon
            
            #연간 방문객#
            Gangwon_year <- subset(Gangwon_t,Gangwon_t$date=="전체"|Gangwon_t$date=="2018년"|Gangwon_t$date=="2019년"|Gangwon_t$date=="2020년")
            Gangwon_month <- subset(Gangwon_t,Gangwon_t$date!="전체"&Gangwon_t$date!="2018년"&Gangwon_t$date!="2019년"&Gangwon_t$date!="2020년")
            
            #월간 관광지 방문객#
            Gangwon_visiter <- as.data.frame(cbind(year_month,rowSums(Gangwon_month[,-1]),c("Gangwon")))
            colnames(Gangwon_visiter) <- c("date", "visiter", "region")
            Gangwon_visiter$visiter <- as.numeric(Gangwon_visiter$visiter)
            
            
            
            ###########################################################################################################
            #광주#
            
            raw_Gwangju <- subset(raw_Gwangju, raw_Gwangju$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Gwangju_t <- subset(raw_Gwangju, raw_Gwangju$`내/외국인`=="합계")
            Gwangju_t <- Gwangju_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Gwangju_t <- as.data.frame(t(Gwangju_t))
            
            #column이름 변경
            colnames(Gwangju_t) <- Gwangju_t[1,]
            Gwangju_t <- Gwangju_t[-1,]
            
            Gwangju_t <- as.data.frame(sapply(Gwangju_t,numeric))
            Gwangju_t[is.na(Gwangju_t)] <- 0
            
            #행이름 각 변경
            Gwangju_t <- cbind(Alltime, Gwangju_t)
            coln_Gwangju <- c("date",raw_Gwangju$관광지)
            colnames(Gwangju_t) <- coln_Gwangju
            
            #연간 방문객#
            Gwangju_year <- subset(Gwangju_t,Gwangju_t$date=="전체"|Gwangju_t$date=="2018년"|Gwangju_t$date=="2019년"|Gwangju_t$date=="2020년")
            Gwangju_month <- subset(Gwangju_t,Gwangju_t$date!="전체"&Gwangju_t$date!="2018년"&Gwangju_t$date!="2019년"&Gwangju_t$date!="2020년")
            
            #월간 관광지 방문객#
            Gwangju_visiter <- as.data.frame(cbind(year_month,rowSums(Gwangju_month[,-1]),c("Gwangju")))
            colnames(Gwangju_visiter) <- c("date", "visiter", "region")
            Gwangju_visiter$visiter <- as.numeric(Gwangju_visiter$visiter)
            
            
            ###########################################################################################################
            #경북#
            
            raw_GyeongBuk <- subset(raw_GyeongBuk, raw_GyeongBuk$`내/외국인`=="합계")
          
            #필요없는 행 제거#
            GyeongBuk_t <- subset(raw_GyeongBuk, raw_GyeongBuk$`내/외국인`=="합계")
            GyeongBuk_t <- GyeongBuk_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            GyeongBuk_t <- as.data.frame(t(GyeongBuk_t))
            
            #column이름 변경
            colnames(GyeongBuk_t) <- GyeongBuk_t[1,]
            GyeongBuk_t <- GyeongBuk_t[-1,]
            
            GyeongBuk_t <- as.data.frame(sapply(GyeongBuk_t,numeric))
            GyeongBuk_t[is.na(GyeongBuk_t)] <- 0
            
            #행이름 각 변경
            GyeongBuk_t <- cbind(Alltime, GyeongBuk_t)
            coln_GyeongBuk <- c("date",raw_GyeongBuk$관광지)
            colnames(GyeongBuk_t) <- coln_GyeongBuk
            
            #연간 방문객#
            GyeongBuk_year <- subset(GyeongBuk_t,GyeongBuk_t$date=="전체"|GyeongBuk_t$date=="2018년"|GyeongBuk_t$date=="2019년"|GyeongBuk_t$date=="2020년")
            GyeongBuk_month <- subset(GyeongBuk_t,GyeongBuk_t$date!="전체"&GyeongBuk_t$date!="2018년"&GyeongBuk_t$date!="2019년"&GyeongBuk_t$date!="2020년")
            
            #월간 관광지 방문객#
            GyeongBuk_visiter <- as.data.frame(cbind(year_month,rowSums(GyeongBuk_month[,-1]),c("GyeongBuk")))
            colnames(GyeongBuk_visiter) <- c("date", "visiter", "region")
            GyeongBuk_visiter$visiter <- as.numeric(GyeongBuk_visiter$visiter)
            
            
            ###########################################################################################################
            #경기도#
            
            raw_Gyeonggido <- subset(raw_Gyeonggido, raw_Gyeonggido$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Gyeonggido_t <- subset(raw_Gyeonggido, raw_Gyeonggido$`내/외국인`=="합계")
            Gyeonggido_t <- Gyeonggido_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Gyeonggido_t <- as.data.frame(t(Gyeonggido_t))
            
            #column이름 변경
            colnames(Gyeonggido_t) <- Gyeonggido_t[1,]
            Gyeonggido_t <- Gyeonggido_t[-1,]
            
            Gyeonggido_t <- as.data.frame(sapply(Gyeonggido_t,numeric))
            Gyeonggido_t[is.na(Gyeonggido_t)] <- 0
            
            #행이름 각 변경
            Gyeonggido_t <- cbind(Alltime, Gyeonggido_t)
            coln_Gyeonggido <- c("date",raw_Gyeonggido$관광지)
            colnames(Gyeonggido_t) <- coln_Gyeonggido
            
            #연간 방문객#
            Gyeonggido_year <- subset(Gyeonggido_t,Gyeonggido_t$date=="전체"|Gyeonggido_t$date=="2018년"|Gyeonggido_t$date=="2019년"|Gyeonggido_t$date=="2020년")
            Gyeonggido_month <- subset(Gyeonggido_t,Gyeonggido_t$date!="전체"&Gyeonggido_t$date!="2018년"&Gyeonggido_t$date!="2019년"&Gyeonggido_t$date!="2020년")
            
            #월간 관광지 방문객#
            Gyeonggido_visiter <- as.data.frame(cbind(year_month,rowSums(Gyeonggido_month[,-1]),c("Gyeonggido")))
            colnames(Gyeonggido_visiter) <- c("date", "visiter", "region")
            Gyeonggido_visiter$visiter <- as.numeric(Gyeonggido_visiter$visiter)
            
            
            ###########################################################################################################
            #경남#
            
            raw_GyeongNam <- subset(raw_GyeongNam, raw_GyeongNam$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            GyeongNam_t <- subset(raw_GyeongNam, raw_GyeongNam$`내/외국인`=="합계")
            GyeongNam_t <- GyeongNam_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            GyeongNam_t <- as.data.frame(t(GyeongNam_t))
            
            #column이름 변경
            colnames(GyeongNam_t) <- GyeongNam_t[1,]
            GyeongNam_t <- GyeongNam_t[-1,]
            
            GyeongNam_t <- as.data.frame(sapply(GyeongNam_t,numeric))
            GyeongNam_t[is.na(GyeongNam_t)] <- 0
            
            #행이름 각 변경
            GyeongNam_t <- cbind(Alltime, GyeongNam_t)
            coln_GyeongNam <- c("date",raw_GyeongNam$관광지)
            colnames(GyeongNam_t) <- coln_GyeongNam
            
            #연간 방문객#
            GyeongNam_year <- subset(GyeongNam_t,GyeongNam_t$date=="전체"|GyeongNam_t$date=="2018년"|GyeongNam_t$date=="2019년"|GyeongNam_t$date=="2020년")
            GyeongNam_month <- subset(GyeongNam_t,GyeongNam_t$date!="전체"&GyeongNam_t$date!="2018년"&GyeongNam_t$date!="2019년"&GyeongNam_t$date!="2020년")
            
            #월간 관광지 방문객#
            GyeongNam_visiter <- as.data.frame(cbind(year_month,rowSums(GyeongNam_month[,-1]),c("GyeongNam")))
            colnames(GyeongNam_visiter) <- c("date", "visiter", "region")
            GyeongNam_visiter$visiter <- as.numeric(GyeongNam_visiter$visiter)
            
            
            
            ###########################################################################################################
            #인천#
            
            raw_Incheon <- subset(raw_Incheon, raw_Incheon$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Incheon_t <- subset(raw_Incheon, raw_Incheon$`내/외국인`=="합계")
            Incheon_t <- Incheon_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Incheon_t <- as.data.frame(t(Incheon_t))
            
            #column이름 변경
            colnames(Incheon_t) <- Incheon_t[1,]
            Incheon_t <- Incheon_t[-1,]
            
            Incheon_t <- as.data.frame(sapply(Incheon_t,numeric))
            Incheon_t[is.na(Incheon_t)] <- 0
            
            #행이름 각 변경
            Incheon_t <- cbind(Alltime, Incheon_t)
            coln_Incheon <- c("date",raw_Incheon$관광지)
            colnames(Incheon_t) <- coln_Incheon
            
            #연간 방문객#
            Incheon_year <- subset(Incheon_t,Incheon_t$date=="전체"|Incheon_t$date=="2018년"|Incheon_t$date=="2019년"|Incheon_t$date=="2020년")
            Incheon_month <- subset(Incheon_t,Incheon_t$date!="전체"&Incheon_t$date!="2018년"&Incheon_t$date!="2019년"&Incheon_t$date!="2020년")
            
            #월간 관광지 방문객#
            Incheon_visiter <- as.data.frame(cbind(year_month,rowSums(Incheon_month[,-1]),c("Incheon")))
            colnames(Incheon_visiter) <- c("date", "visiter", "region")
            Incheon_visiter$visiter <- as.numeric(Incheon_visiter$visiter)
            
            
            
            ###########################################################################################################
            #제주#
            
            raw_Jeju <- subset(raw_Jeju, raw_Jeju$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Jeju_t <- subset(raw_Jeju, raw_Jeju$`내/외국인`=="합계")
            Jeju_t <- Jeju_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Jeju_t <- as.data.frame(t(Jeju_t))
            
            #column이름 변경
            colnames(Jeju_t) <- Jeju_t[1,]
            Jeju_t <- Jeju_t[-1,]
            
            Jeju_t <- as.data.frame(sapply(Jeju_t,numeric))
            Jeju_t[is.na(Jeju_t)] <- 0
            
            #행이름 각 변경
            Jeju_t <- cbind(Alltime, Jeju_t)
            coln_Jeju <- c("date",raw_Jeju$관광지)
            colnames(Jeju_t) <- coln_Jeju
            
            #연간 방문객#
            Jeju_year <- subset(Jeju_t,Jeju_t$date=="전체"|Jeju_t$date=="2018년"|Jeju_t$date=="2019년"|Jeju_t$date=="2020년")
            Jeju_month <- subset(Jeju_t,Jeju_t$date!="전체"&Jeju_t$date!="2018년"&Jeju_t$date!="2019년"&Jeju_t$date!="2020년")
            
            #월간 관광지 방문객#
            Jeju_visiter <- as.data.frame(cbind(year_month,rowSums(Jeju_month[,-1]),c("Jeju")))
            colnames(Jeju_visiter) <- c("date", "visiter", "region")
            Jeju_visiter$visiter <- as.numeric(Jeju_visiter$visiter)
            
            
            
            ###########################################################################################################
            #전북#
            
            raw_JeonBuk <- subset(raw_JeonBuk, raw_JeonBuk$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            JeonBuk_t <- subset(raw_JeonBuk, raw_JeonBuk$`내/외국인`=="합계")
            JeonBuk_t <- JeonBuk_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            JeonBuk_t <- as.data.frame(t(JeonBuk_t))
            
            #column이름 변경
            colnames(JeonBuk_t) <- JeonBuk_t[1,]
            JeonBuk_t <- JeonBuk_t[-1,]
            
            JeonBuk_t <- as.data.frame(sapply(JeonBuk_t,numeric))
            JeonBuk_t[is.na(JeonBuk_t)] <- 0
            
            #행이름 각 변경
            JeonBuk_t <- cbind(Alltime, JeonBuk_t)
            coln_JeonBuk <- c("date",raw_JeonBuk$관광지)
            colnames(JeonBuk_t) <- coln_JeonBuk
            
            #연간 방문객#
            JeonBuk_year <- subset(JeonBuk_t,JeonBuk_t$date=="전체"|JeonBuk_t$date=="2018년"|JeonBuk_t$date=="2019년"|JeonBuk_t$date=="2020년")
            JeonBuk_month <- subset(JeonBuk_t,JeonBuk_t$date!="전체"&JeonBuk_t$date!="2018년"&JeonBuk_t$date!="2019년"&JeonBuk_t$date!="2020년")
            
            #월간 관광지 방문객#
            JeonBuk_visiter <- as.data.frame(cbind(year_month,rowSums(JeonBuk_month[,-1]),c("JeonBuk")))
            colnames(JeonBuk_visiter) <- c("date", "visiter", "region")
            JeonBuk_visiter$visiter <- as.numeric(JeonBuk_visiter$visiter)
            
            
            
            ###########################################################################################################
            #전남#
            
            raw_JeonNam <- subset(raw_JeonNam, raw_JeonNam$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            JeonNam_t <- subset(raw_JeonNam, raw_JeonNam$`내/외국인`=="합계")
            JeonNam_t <- JeonNam_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            JeonNam_t <- as.data.frame(t(JeonNam_t))
            
            #column이름 변경
            colnames(JeonNam_t) <- JeonNam_t[1,]
            JeonNam_t <- JeonNam_t[-1,]
            
            JeonNam_t <- as.data.frame(sapply(JeonNam_t,numeric))
            JeonNam_t[is.na(JeonNam_t)] <- 0
            
            #행이름 각 변경
            JeonNam_t <- cbind(Alltime, JeonNam_t)
            coln_JeonNam <- c("date",raw_JeonNam$관광지)
            colnames(JeonNam_t) <- coln_JeonNam
            
            #연간 방문객#
            JeonNam_year <- subset(JeonNam_t,JeonNam_t$date=="전체"|JeonNam_t$date=="2018년"|JeonNam_t$date=="2019년"|JeonNam_t$date=="2020년")
            JeonNam_month <- subset(JeonNam_t,JeonNam_t$date!="전체"&JeonNam_t$date!="2018년"&JeonNam_t$date!="2019년"&JeonNam_t$date!="2020년")
            
            #월간 관광지 방문객#
            JeonNam_visiter <- as.data.frame(cbind(year_month,rowSums(JeonNam_month[,-1]),c("JeonNam")))
            colnames(JeonNam_visiter) <- c("date", "visiter", "region")
            JeonNam_visiter$visiter <- as.numeric(JeonNam_visiter$visiter)
            
            
            ###########################################################################################################
            #세종#
            
            raw_Saejong <- subset(raw_Saejong, raw_Saejong$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Saejong_t <- subset(raw_Saejong, raw_Saejong$`내/외국인`=="합계")
            Saejong_t <- Saejong_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Saejong_t <- as.data.frame(t(Saejong_t))
            
            #column이름 변경
            colnames(Saejong_t) <- Saejong_t[1,]
            Saejong_t <- Saejong_t[-1,]
            
            Saejong_t <- as.data.frame(sapply(Saejong_t,numeric))
            Saejong_t[is.na(Saejong_t)] <- 0
            
            #행이름 각 변경
            Saejong_t <- cbind(Alltime, Saejong_t)
            coln_Saejong <- c("date",raw_Saejong$관광지)
            colnames(Saejong_t) <- coln_Saejong
            
            #연간 방문객#
            Saejong_year <- subset(Saejong_t,Saejong_t$date=="전체"|Saejong_t$date=="2018년"|Saejong_t$date=="2019년"|Saejong_t$date=="2020년")
            Saejong_month <- subset(Saejong_t,Saejong_t$date!="전체"&Saejong_t$date!="2018년"&Saejong_t$date!="2019년"&Saejong_t$date!="2020년")
            
            #월간 관광지 방문객#
            Saejong_visiter <- as.data.frame(cbind(year_month,rowSums(Saejong_month[,-1]),c("Saejong")))
            colnames(Saejong_visiter) <- c("date", "visiter", "region")
            Saejong_visiter$visiter <- as.numeric(Saejong_visiter$visiter)
            
            
            
            ###########################################################################################################
            #울산#
            
            raw_Ulsan <- subset(raw_Ulsan, raw_Ulsan$`내/외국인`=="합계")
            
            #필요없는 행 제거#
            Ulsan_t <- subset(raw_Ulsan, raw_Ulsan$`내/외국인`=="합계")
            Ulsan_t <- Ulsan_t[,c(-1,-2,-4)]
            
            #년월방문객/관광지형태로 변환#
            Ulsan_t <- as.data.frame(t(Ulsan_t))
            
            #column이름 변경
            colnames(Ulsan_t) <- Ulsan_t[1,]
            Ulsan_t <- Ulsan_t[-1,]
            
            Ulsan_t <- as.data.frame(sapply(Ulsan_t,numeric))
            Ulsan_t[is.na(Ulsan_t)] <- 0
            
            #행이름 각 변경
            Ulsan_t <- cbind(Alltime, Ulsan_t)
            coln_Ulsan <- c("date",raw_Ulsan$관광지)
            colnames(Ulsan_t) <- coln_Ulsan
            
            #연간 방문객#
            Ulsan_year <- subset(Ulsan_t,Ulsan_t$date=="전체"|Ulsan_t$date=="2018년"|Ulsan_t$date=="2019년"|Ulsan_t$date=="2020년")
            Ulsan_month <- subset(Ulsan_t,Ulsan_t$date!="전체"&Ulsan_t$date!="2018년"&Ulsan_t$date!="2019년"&Ulsan_t$date!="2020년")
            
            #월간 관광지 방문객#
            Ulsan_visiter <- as.data.frame(cbind(year_month,rowSums(Ulsan_month[,-1]),c("Ulsan")))
            colnames(Ulsan_visiter) <- c("date", "visiter", "region")
            Ulsan_visiter$visiter <- as.numeric(Ulsan_visiter$visiter)
            
            
            
            #관광지별 월별데이터 합치기#
            monthly_visiter <- merge(seoul_month, Busan_month, by="date")
            monthly_visiter <- merge(monthly_visiter, ChungBuk_month, by="date")
            monthly_visiter <- merge(monthly_visiter, ChungNam_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Daegu_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Daejeon_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Gangwon_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Gwangju_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Gyeonggido_month, by="date")
            monthly_visiter <- merge(monthly_visiter, GyeongNam_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Incheon_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Jeju_month, by="date")
            monthly_visiter <- merge(monthly_visiter, JeonBuk_month, by="date")
            monthly_visiter <- merge(monthly_visiter, JeonNam_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Saejong_month, by="date")
            monthly_visiter <- merge(monthly_visiter, Ulsan_month, by="date")
            
            #관광지별 연별데이터 합치기#
            yearly_visiter <- merge(seoul_year, Busan_year, by="date")
            yearly_visiter <- merge(yearly_visiter, ChungBuk_year, by="date")
            yearly_visiter <- merge(yearly_visiter, ChungNam_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Daegu_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Daejeon_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Gangwon_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Gwangju_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Gyeonggido_year, by="date")
            yearly_visiter <- merge(yearly_visiter, GyeongNam_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Incheon_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Jeju_year, by="date")
            yearly_visiter <- merge(yearly_visiter, JeonBuk_year, by="date")
            yearly_visiter <- merge(yearly_visiter, JeonNam_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Saejong_year, by="date")
            yearly_visiter <- merge(yearly_visiter, Ulsan_year, by="date")
            
            total_visiter <- yearly_visiter[4,]
            yearly_visiter <- yearly_visiter[-4,]
            
            #월별 지역 방문객#
            monthly_region <- rbind(seoul_visiter,
                                    Busan_visiter,
                                    ChungBuk_visiter,
                                    ChungNam_visiter,
                                    Daegu_visiter,
                                    Daejeon_visiter,
                                    Gangwon_visiter,
                                    Gwangju_visiter,
                                    GyeongBuk_visiter,
                                    Gyeonggido_visiter,
                                    GyeongNam_visiter,
                                    Incheon_visiter,
                                    Jeju_visiter,
                                    JeonBuk_visiter,
                                    JeonNam_visiter,
                                    Saejong_visiter,
                                    Ulsan_visiter)
            
            monthly_region$date <- anydate(monthly_region$date)
            summary(monthly_region)
            #selection_data#
            se1 <- monthly_region %>% filter(monthly_region$date=="2019-01-01"|monthly_region$date=="2019-02-01"|monthly_region$date=="2019-03-01"|monthly_region$date=="2020-01-01"|monthly_region$date=="2020-02-01"|monthly_region$date=="2020-03-01")
            
            se2 <- monthly_region %>%  filter(monthly_region$date > "2019-07-01")
            
            visit_place <- cbind(seoul_t[,-1], Busan_t[,-1], ChungBuk_t[,-1], ChungNam_t[,-1], Daegu_t[,-1], Daejeon_t[,-1], Gangwon_t[,-1], Gwangju_t[,-1], GyeongBuk_t[,-1], Gyeonggido_t[,-1], GyeongNam_t[,-1], Incheon_t[,-1], Jeju_t[,-1], JeonBuk_t[,-1], JeonNam_t[,-1], Saejong_t[,-1], Ulsan_t[,-1])
            rownames(visit_place) <- c("2018~2020년 합계", "2018년 합계","2018년1월", "2018년 2월", "2018년 3월", "2018년 4월", "2018년 5월", "2018년 6월", "2018년 7월", "2018년 8월", "2018년 9월", "2018년 10월", "2018년 11월", "2018년 12월",
                                       "2019년 합계", "2019년 1월", "2019년 2월", "2019년 3월", "2019년 4월", "2019년 5월", "2019년 6월", "2019년 7월", "2019년 8월","2019년 9월", "2019년 10월", "2019년 11월", "2019년 12월",
                                       "2020년 합계", "2020년 1월", "2020년 2월", "2020년 3월")
            
            view_visit_place <- t(visit_place)
            
            
            monthly_region<-cbind(monthly_region, month=substr(monthly_region$date,6,7))
            monthly_region<-cbind(monthly_region, year=substr(monthly_region$date,1,4))
            monthly_region1 <- monthly_region %>% filter(monthly_region$date=="2019-01-01"|monthly_region$date=="2019-02-01"|monthly_region$date=="2019-03-01")
            monthly_region2 <- monthly_region %>% filter(monthly_region$date=="2020-01-01"|monthly_region$date=="2020-02-01"|monthly_region$date=="2020-03-01")
            monthly_region3 <- monthly_region %>%filter(monthly_region$date >= "2019-01-01")
            monthly_region3<-unite(data=monthly_region3,col=ym, year,month, sep="")
            
            regiondat<-aggregate(monthly_region3$visiter ,by=list(monthly_region3$ym,monthly_region3$region), sum)
            monthly_region1<-unite(data=monthly_region1,col=ym, year,month, sep="")
            
            regiondat1<-aggregate(monthly_region1$visiter ,by=list(monthly_region1$ym,monthly_region1$region), sum)
            monthly_region2<-unite(data=monthly_region2,col=ym, year,month, sep="")
            
            regiondat2<-aggregate(monthly_region2$visiter ,by=list(monthly_region2$ym,monthly_region2$region), sum)
            
            regiondat=rename(regiondat, "year"="Group.1")
            regiondat=rename(regiondat, "region"="Group.2")
            regiondat=rename(regiondat, "visiter"="x")
            regiondat$year=as.integer(regiondat$year)
            regiondat1=rename(regiondat1, "year"="Group.1")
            regiondat1=rename(regiondat1, "region"="Group.2")
            regiondat1=rename(regiondat1, "visiter"="x")
            regiondat1$year=as.integer(regiondat1$year)
            regiondat2=rename(regiondat2, "year"="Group.1")
            regiondat2=rename(regiondat2, "region"="Group.2")
            regiondat2=rename(regiondat2, "visiter"="x")
            regiondat2$year=as.integer(regiondat2$year)
            
            
            results <- get_headlines(query = "코로나", country="kr",page = 1, page_size = 20,api_key= "87f922af12404088b7c52926a28855cd")
            test <- results$results_df
            
            results2 <- get_headlines(query = "COVID-19" ,country="us",page = 1, page_size = 20,api_key= "87f922af12404088b7c52926a28855cd")
            test2 <- results2$results_df
            
            Case$latitude <- as.numeric(Case$latitude)
            Case$longitude <- as.numeric(Case$longitude)
            
            Case <- subset(Case,Case$latitude!="")
            Case <- subset(Case,Case$longitude!="")
            
            Case_pal <- colorFactor("viridis", Case$province)
            
            Time_Age <- aggregate(confirmed ~ age,TimeAge, sum)
            
            ggDonut=function(data=acs,donuts="Dx",count=NULL,
                             addPieLabel=TRUE,addDonutLabel=TRUE,showRatio=TRUE,
                             polar=TRUE,labelposition=1){
              if(is.null(count)){
                dat1=ddply(data,donuts,nrow)
                colnames(dat1)[2]="n"
              } else{
                dat1=data
                colnames(dat1)[colnames(dat1)==count]="n"
              }        
              dat1$ymax=cumsum(dat1$n)
              dat1$ymin=cumsum(dat1$n)-dat1$n
              dat1$ypos=dat1$ymin+dat1$n/2
              dat1$ratio=dat1$n*100/sum(dat1$n)
              dat1$cumratio=dat1$ypos*100/sum(dat1$n)
              dat1$hjust=ifelse((dat1$cumratio>25 & dat1$cumratio<75),0,1)
              
              
              mainCol=rainbow(nrow(dat1))
              p<-ggplot(dat1) + 
                geom_rect(aes( ymax=ymax, ymin=ymin, xmax=4,xmin=3),fill=mainCol,
                          colour="white",alpha=0.7)+ 
                coord_polar(theta="y",start=3*pi/2)+
                xlim(0,4+labelposition)+
                theme_clean()
              
              label=dat1[[donuts]]
              if(showRatio) 
                label=paste0(label,"\n(",round(dat1$ratio,2),"%)")
              
              if(labelposition==1) {
                p<- p+ geom_text(aes(label=label,x=4.3,y=ypos,hjust=hjust),size=4)+
                  geom_segment(aes(x=4,xend=4.2,y=ypos,yend=ypos))      
              }  else{
                p<- p+ geom_text(aes(label=label,x=3.5,y=ypos),size=5)
              }      
              
              p
              
            }
            
            theme_clean=function(base_size=12){
              theme_grey(base_size) %+replace%
                theme(
                  axis.title=element_blank(),
                  axis.text=element_blank(),
                  panel.background=element_blank(),
                  panel.grid=element_blank(),
                  axis.ticks.length=unit(0,"cm"),
                  axis.ticks.margin=unit(0,"cm"),
                  panel.margin=unit(0,"lines"),
                  plot.margin=unit(c(0,0,0,0),"lines"),
                  complete=TRUE
                )
            }
            
            theme_axis_blank=function(){
              theme(axis.ticks=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank())
            }
            
            TimeGender <- TimeGender[,c(1,3,4)]
            TimeGender$date <- ymd(TimeGender$date)
            TimeGender$month <- month(TimeGender$date)
            
            TimeGender_count <- aggregate(confirmed ~ sex, TimeGender,sum)
            TimeGender_count_male <- subset(TimeGender_count, TimeGender_count$sex=="male")$confirmed
            TimeGender_count_female <- subset(TimeGender_count, TimeGender_count$sex=="female")$confirmed
            
            Gender_count <- c(TimeGender_count_male, TimeGender_count_female)
            Gender_count <- as.data.frame(c(round(TimeGender_count_male*100/(TimeGender_count_male + TimeGender_count_female)), round(TimeGender_count_female*100/(TimeGender_count_male+TimeGender_count_female))))
            Gender_count <- as.data.frame(t(Gender_count))
            colnames(Gender_count) <- c("Male","Female")
            rownames(Gender_count) <- c("counting")
            
            Gender_sex_plot <- waffle(c("Female"=Gender_count$Female, "male"=Gender_count$Male),size=20, rows=10,glyph_size = 20,use_glyph = c("female", "male")) + coord_equal(ratio=1, clip="on")
            
            TimeAge <- TimeAge[,c(1,3,4)]
            TimeAge$date <- ymd(TimeAge$date)
            TimeAge$month <- month(TimeAge$date)
            
            Time$date <- ymd(Time$date)
            Time <- aggregate(confirmed ~date,Time, sum)
            Time$month <- month(Time$date)
            
            monthpatient<-aggregate(confirmed~month,Time,sum)
            monthpatient=cbind(monthpatient, la = lag(monthpatient$confirmed))
            monthpatient$la[is.na(monthpatient$la)] <- 0
            monthpatient=cbind(monthpatient, diff = monthpatient$confirmed-monthpatient$la)
            Time=cbind(Time, la = lag(Time$confirmed))
            Time$la[is.na(Time$la)] <- 0
            Time=cbind(Time, diff = Time$confirmed-Time$la)
            
            
            #########jhu깃허브에서 글로벌 코로나 19 확진,사망,회복자수 불러들이고, map그려줌###########################
            #전처리 하기위한 함수 설정
            # function to update jhu input data according to mapping base format
            update_jhu = function(input_df, tag) {
              names(input_df)[1:2] = c("Province", "Country")
              input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
              input_df$Country[input_df$Province=="Macau"] = "Macao"
              input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
              input_df$Country[input_df$Country=="Korea, South"] = "RepublicofKorea"
              input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
              input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
              input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
              input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
              input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
              input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
              input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
              input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
              input_df$Country = input_df$Country %>% str_replace_all(., " ", "") 
              dates = names(input_df)[which(names(input_df)=="1/22/20"):ncol(input_df)]
              input_df = input_df %>% 
                select(-c(Province, Lat, Long)) %>% 
                group_by(Country) %>% 
                summarise_each(funs(sum)) %>%
                data.frame()
              rownames(input_df) = input_df$Country
              rownames(input_df) = paste0(input_df$Country,"_",tag)
              input_df = input_df %>% select(-c(Country)) %>% t()
              input_df = data.frame(input_df)
              input_df$Date = dates
              rownames(input_df) = 1:nrow(input_df)
              input_df$Date = format(as.Date(input_df$Date,"%m/%d/%y"))
              input_df
            }
            
            # load latest Covid-2019 data: confirmed cases
            update_jhu = function(input_df, tag) {
              names(input_df)[1:2] = c("Province", "Country")
              input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
              input_df$Country[input_df$Province=="Macau"] = "Macao"
              input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
              input_df$Country[input_df$Country=="Korea, South"] = "RepublicofKorea"
              input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
              input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
              input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
              input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
              input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
              input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
              input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
              input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
              input_df$Country = input_df$Country %>% str_replace_all(., " ", "") 
              dates = names(input_df)[which(names(input_df)=="1/22/20"):ncol(input_df)]
              input_df = input_df %>% 
                select(-c(Province, Lat, Long)) %>% 
                group_by(Country) %>% 
                summarise_each(funs(sum)) %>%
                data.frame()
              rownames(input_df) = input_df$Country
              rownames(input_df) = paste0(input_df$Country,"_",tag)
              input_df = input_df %>% select(-c(Country)) %>% t()
              input_df = data.frame(input_df)
              input_df$Date = dates
              rownames(input_df) = 1:nrow(input_df)
              input_df$Date = format(as.Date(input_df$Date,"%m/%d/%y"))
              input_df
            }
            
            # load latest Covid-2019 data: confirmed cases
            jhu_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
            jhu_cases[is.na(jhu_cases)]=0
            total_cases <- sum(jhu_cases[,ncol(jhu_cases)])
            jhu_cases = update_jhu(jhu_cases, "cases")
            if (total_cases!=sum(jhu_cases[nrow(jhu_cases),1:(ncol(jhu_cases)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }
            jhu_cases1<-jhu_cases[,-length(colnames(jhu_cases))]
            jhu_cases<-jhu_cases[-(1:(length(jhu_cases$Afghanistan_cases)-1)),]
            jhu_cases<-jhu_cases[,-length(colnames(jhu_cases))]
            jhu_cases1<-jhu_cases1[-(1:(length(jhu_cases1$Afghanistan_cases)-2)),]
            jhu_cases1<-jhu_cases1[-2,]
            country<-colnames(jhu_cases)<-gsub("_cases","",colnames(jhu_cases))
            jhu_cases <- as.data.frame(t(jhu_cases))
            jhu_cases1 <- as.data.frame(t(jhu_cases1))
            jhu_cases <-cbind(jhu_cases,country)
            names(jhu_cases) [names(jhu_cases) == colnames(jhu_cases)[1]] <- c("confirmed")
            names(jhu_cases1) [names(jhu_cases1) == colnames(jhu_cases1)[1]] <- c("confirmed")
            jhu_cases <-cbind(jhu_cases,dff=jhu_cases$confirmed-jhu_cases1$confirmed)
            
            
            # load latest Covid-2019 data: deaths
            jhu_deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
            jhu_deaths[is.na(jhu_deaths)]=0
            total_deaths <- sum(jhu_deaths[,ncol(jhu_deaths)])
            jhu_deaths = update_jhu(jhu_deaths, "deaths")
            if (total_deaths!=sum(jhu_deaths[nrow(jhu_deaths),1:(ncol(jhu_deaths)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }
            jhu_deaths1<-jhu_deaths[,-length(colnames(jhu_deaths))]
            jhu_deaths<-jhu_deaths[-(1:(length(jhu_deaths$Afghanistan_deaths)-1)),]
            jhu_deaths<-jhu_deaths[,-length(colnames(jhu_deaths))]
            jhu_deaths1<-jhu_deaths1[-(1:(length(jhu_deaths1$Afghanistan_death)-2)),]
            jhu_deaths1<-jhu_deaths1[-2,]
            colnames(jhu_deaths)<-gsub("_deaths","",colnames(jhu_deaths))
            jhu_deaths <- as.data.frame(t(jhu_deaths))
            jhu_deaths1 <- as.data.frame(t(jhu_deaths1))
            jhu_deaths <-cbind(jhu_deaths,country)
            names(jhu_deaths) [names(jhu_deaths) == colnames(jhu_deaths)[1]] <- c("deaths")
            names(jhu_deaths1) [names(jhu_deaths1) == colnames(jhu_deaths1)[1]] <- c("deaths")
            jhu_deaths <-cbind(jhu_deaths,dff=jhu_deaths$deaths-jhu_deaths1$deaths)
            
            
            # load latest Covid-2019 data: recovered
            jhu_rec <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
            jhu_rec[is.na(jhu_rec)]=0
            total_rec <- sum(jhu_rec[,ncol(jhu_rec)])
            jhu_rec = update_jhu(jhu_rec, "recovered")
            if (total_rec!=sum(jhu_rec[nrow(jhu_rec),1:(ncol(jhu_rec)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }
            jhu_rec1<-jhu_rec[,-length(colnames(jhu_rec))]
            jhu_rec<-jhu_rec[-(1:(length(jhu_rec$Afghanistan_recovered)-1)),]
            jhu_rec<-jhu_rec[,-length(colnames(jhu_rec))]
            jhu_rec1<-jhu_rec1[-(1:(length(jhu_rec1$Afghanistan_rec)-2)),]
            jhu_rec1<-jhu_rec1[-2,]
            colnames(jhu_rec)<-gsub("_recovered","",colnames(jhu_rec))
            jhu_rec <- as.data.frame(t(jhu_rec))
            jhu_rec1 <- as.data.frame(t(jhu_rec1))
            jhu_rec <-cbind(jhu_rec,country)
            names(jhu_rec) [names(jhu_rec) == colnames(jhu_rec)[1]] <- c("rec")
            names(jhu_rec1) [names(jhu_rec1) == colnames(jhu_rec1)[1]] <- c("rec")
            jhu_rec <-cbind(jhu_rec,dff=jhu_rec$rec-jhu_rec1$rec)
            
            # 색지정 확진:빨강 회복:파랑 사망:보라
            confirmed_col = "#cc4c02"
            recover_col = "#045a8d"
            death_col = "#4d004b"
            ####country는 나라의 위도경도를 나타내는 데이터이다, country에써있는 국가의 이름과 jhu데이터의 국가이름이 다르므로 일치시켜주는 작업
            jhu_cases$country =gsub("RepublicofKorea", "Republic of Korea", jhu_cases$country)
            jhu_deaths$country = gsub("RepublicofKorea", "Republic of Korea", jhu_deaths$country)
            jhu_rec$country=gsub("RepublicofKorea", "Republic of Korea", jhu_rec$country)
            jhu_cases$country=gsub("US", "USA", jhu_cases$country)
            jhu_deaths$country =gsub("US", "USA", jhu_deaths$country)
            jhu_rec$country=gsub("US", "USA", jhu_rec$country)
            jhu_cases$country =gsub("China", "Mainland China", jhu_cases$country)
            jhu_deaths$country =gsub("China", "Mainland China", jhu_deaths$country)
            jhu_rec$country =gsub("China", "Mainland China", jhu_rec$country)
            jhu_cases$country =gsub("SouthAfrica", "South Africa", jhu_cases$country)
            jhu_deaths$country =gsub("SouthAfrica", "South Africa", jhu_deaths$country)
            jhu_rec$country =gsub("SouthAfrica", "South Africa", jhu_rec$country)
            jhu_cases$country =gsub("UnitedKingdom", "UK", jhu_cases$country)
            jhu_deaths$country =gsub("UnitedKingdom", "UK", jhu_deaths$country)
            jhu_rec$country =gsub("UnitedKingdom", "UK", jhu_rec$country)
            cv_cases = merge(jhu_cases, countries %>% select(-c(jhu_ID, global_level, continent_level)), by = "country")
            cv_death = merge(jhu_deaths, countries %>% select(-c(jhu_ID, global_level, continent_level)), by = "country")
            cv_rec = merge(jhu_rec, countries %>% select(-c(jhu_ID, global_level, continent_level)), by = "country")
            cv_cases =cbind(cv_cases,death=cv_death$deaths)
            cv_cases =cbind(cv_cases,rec=cv_rec$rec)
            cv_large_countries = cv_cases %>% filter(alpha3 %in% worldcountry$id)
            plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]  
            
            #######지도 그려주는 과정#############(코드공유에는 나타나지 않는 현상 발생 ㅠㅠ)#####
            basemap = leaflet(plot_map) %>% 
              addTiles() %>% 
              addLayersControl(
                position = "bottomright",
                overlayGroups = c("2019-COVID (confirmed)", "2019-COVID (Death)", "2019-COVID (Recovery)"),
                options = layersControlOptions(collapsed = FALSE)) %>% 
              #hideGroup(c("2019-COVID (Death)", "2019-COVID (Recovery)"))  %>%
              addProviderTiles(providers$CartoDB.Positron) %>%
              fitBounds(~-100,-50,~80,80) %>%
              addCircles(data = cv_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(confirmed)^(1/4)*3.5e4*penalty,
                         fillOpacity = 0.2, color = confirmed_col, group = "2019-COVID (confirmed)",
                         label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed : %g<br/>Deaths: %d<br/>Recovery : %g", cv_cases$country, cv_cases$confirmed, cv_cases$death, cv_cases$rec) %>% 
                           lapply(htmltools::HTML),labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "color" = confirmed_col),textsize = "15px", direction = "auto"))%>%
              addCircles(data = cv_death, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(deaths)^(1/4)*3.5e4*penalty,
                         fillOpacity = 0.2, color = death_col, group = "2019-COVID (Death)",
                         label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed : %g<br/>Deaths: %d<br/>Recovery : %g", cv_cases$country, cv_cases$confirmed, cv_cases$death, cv_cases$rec) %>% 
                           lapply(htmltools::HTML),labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px", "color" = death_col),
                             textsize = "15px", direction = "auto"))%>%
              addCircles(data = cv_rec, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(rec)^(1/4)*3.5e4*penalty,
                         fillOpacity = 0.2, color = recover_col, group = "2019-COVID (Recovery)",
                         label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed : %g<br/>Deaths: %d<br/>Recovery : %g", cv_cases$country, cv_cases$confirmed, cv_cases$death, cv_cases$rec) %>% 
                           lapply(htmltools::HTML),labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "color" = recover_col),textsize = "15px", direction = "auto"))
            
            
            vars <- jhu_cases$country #웹사이트에 사용자가 나라를 선택할 수 있도록 선택박스를 만들기위해 vars에 저장해줌
            
            
            
            
            #####################웹사이트 구현과정!!!######################################

            
            #UI
            ui <- dashboardPage( 
              skin = "yellow",
              dashboardHeader(title = "Covid-19 Analysis"),
              dashboardSidebar(sidebarMenu(
                menuItem("Background", tabName = "Background", icon = icon("fas fa-lungs-virus")),
                menuItem("Data_summary", tabName = "widgets", icon = icon("database"),
                         menuSubItem("Patient data", tabName="patient_data", icon=icon("bed")),
                         menuSubItem("Card data", tabName="card_data",icon=icon("credit-card")),
                         menuSubItem("Tour data", tabName="tour_data", icon=icon("luggage-cart"))
                ),
                menuItem("Visualization", tabName = "visual", icon = icon("fas fa-chart-bar"),
                         menuSubItem("Patient data", tabName="patient_visual",icon=icon("bed")),
                         menuSubItem("Card data", tabName="card_visual",icon=icon("credit-card")),
                         menuSubItem("Tour data", tabName="tour_visual",icon=icon("luggage-cart")),
                         menuSubItem("Search data", tabName = "Search_visual",icon=icon("internet-explorer"))
                )
              )),##### 웹의 제목, 좌측 탭바 설정끝, 메인창 시작#########
              dashboardBody(tags$head(tags$style(HTML('
    .main-header .logo {
    font-family: "Georgia", Times, "Times New Roman", serif;
    font-weight: bold;
    font-size: 24px;
    },
                                   '))),#### 백그라운드 탭 메인화면 구성######
                            tabItems( 
                              tabItem(tabName = "Background",
                                      ####나라을 설정하면 코로나19관련 확진자수와 사망자수, 회복자수를 실시간으로 보여주는 BOX생성
                                      fluidRow(
                                        box(selectInput('xcol', 'COUNRTY', vars), width = 12 ,icon=icon("luggage-cart"))), 
                                      fluidRow(
                                        valueBoxOutput("Confirmed"),valueBoxOutput("Death"),valueBoxOutput("Rec")
                                      ),
                                      #####전세계 코로나19현황 Circle로 보여주는 map생성
                                      fluidRow( leafletOutput("mymap",height=800)),
                                      hr(),
                                      fluidRow(
                                        box(h1("Latest news on coronavirus disease(KOREA)"),
                                            h3(textOutput("krnews1")),
                                            h6(textOutput("krnews11")),
                                            uiOutput("tab1"),
                                            hr(),
                                            h3(textOutput("krnews2")),
                                            h6(textOutput("krnews21")),
                                            uiOutput("tab2"),
                                            hr(),
                                            h3(textOutput("krnews3")),
                                            h6(textOutput("krnews31")),
                                            uiOutput("tab3"),
                                            status = "primary"),
                                        box(h1("Latest news on coronavirus disease(USA)"),
                                            h3(textOutput("krnews4")),
                                            h6(textOutput("krnews41")),
                                            uiOutput("tab4"),
                                            hr(),
                                            h3(textOutput("krnews5")),
                                            h6(textOutput("krnews51")),
                                            uiOutput("tab5"),
                                            hr(),
                                            h3(textOutput("krnews6")),
                                            h6(textOutput("krnews61")),
                                            uiOutput("tab6"),status = "primary")
                                      )
                              ),
                                      ##########한국의 코로나19 현황파악하기 위한 시각화 UI생성 (데이터,집단감염발생지 시각화,확진자테이블시각화)################################################################################################################################3
                                      tabItem(tabName = "patient_data",
                                              fluidRow(
                                                box(leafletOutput("Patient_map", height=1130),title="Patient_map",height=1200,  status = "primary",solidHeader = TRUE),
                                                box(status = "primary",
                                                    fluidRow(
                                                      box(
                                                        dataTableOutput("confirmed_table"),style = "width:6;height:1070px;overflow-y: scroll;",title="Patient_table",width=12, height=1150),
                                                    )
                                                )
                                              )),
                                      ##########카드결제 데이터에 대한 시각화 UI생성(결제건수에 대한 월별 시각화, 결제금액에 대한 월별 시각화)################################################################################################################################3
                                      
                                      tabItem(tabName = "card_data",
                                              fluidRow(
                                                box(
                                                  p("카드 데이터는 카드 회사가 접수한 카드사용내역을 바탕으로 다음 데이터를 보여준다."),
                                                  div("1. 카드사용지역", style = "color:blue; font-size:12pt;font-weight:bold"),br(),
                                                  div("2. 업종명", style = "color:blue; font-size:12pt;font-weight:bold"),br(),
                                                  div("3. 발생건수", style = "color:blue; font-size:12pt;font-weight:bold"),br(),
                                                  div("4. 발생금액", style = "color:blue; font-size:12pt;font-weight:bold"),
                                                  title="data_description",width=12,height=250,status = "primary",solidHeader=TRUE),
                                              ),
                                              fluidRow(
                                                box(dataTableOutput("card_table"),style = "width:12;height:700px;overflow-y: scroll;",title="card_Table",width=12,height=800,status = "primary",solidHeader = TRUE)
                                              )
                                      ),
                                      ##########카드결제 데이터에 대한 시각화ui(결제건수에 대한 월별 시각화, 결제금액에 대한 월별 시각화)################################################################################################################################3
                                      
                                      tabItem(tabName = "tour_data",
                                              fluidRow(
                                                box(leafletOutput("visiter_place", height=930),
                                                    title=tagList(shiny::icon("fas fa-map-marked-alt"), "Region"),width=6,height=1000, status = "info",solidHeader = TRUE),
                                                box(dataTableOutput("tour_table"), style="width:12;height:950px;overflow-x:scroll;overflow-y: scroll;font-size:70%;",title="Tour_Table",width=6,height=1000,status = "primary",solidHeader = TRUE)
                                              )
                                      ),
                                      #####################visual#################################################
                                      tabItem(tabName = "patient_visual",
                                              fluidRow(
                                                box(plotlyOutput("Patient_total_plot"),title="Patient_total_plot", height=460,width=12,status = "danger",solidHeader = TRUE)
                                              ),
                                              fluidRow(
                                                box(plotlyOutput("Patient_diff_plot"),title="Patient_diff_plot", height=460,width=12,status = "danger",solidHeader = TRUE)
                                              ),
                                              fluidRow(
                                                box(plotOutput("Patient_age_plot", height=700),title="age",width=6, height=760),
                                                box(plotOutput("Patient_sex_plot", width="100%", height=700),title="sex",width=6, height=760)
                                              )
                                      ),
                                      tabItem(tabName = "card_visual",
                                              fluidRow(
                                                box(dateRangeInput("input_month", label="month",start = min(card$date),end=max(card$date), format="yyyy/mm/dd"),width=12)
                                              ),
                                              fluidRow(
                                                box(plotOutput("category_percentage", height=345),title="category_percentage",width=6,height=400,status="danger",solidHeader = TRUE),
                                                box(plotOutput("category_amount_Plot", height=340),title="category_consumption_amount", width=6, height=400,status = "danger",solidHeader = TRUE)
                                              ),
                                              fluidRow(
                                                box(plotlyOutput("categorical_consumption_plot", height=380),title="categorical_consumption_plot",width=12,height=450,status = "danger",solidHeader = TRUE)
                                              )
                                      ),
                                      tabItem(tabName="tour_visual",
                                              fluidRow(
                                                box(plotOutput("all_period_plot", height=800),title=tagList(shiny::icon("fas fa-plane-departure"), "monthly_visiter"), width=12, height=870,status = "primary",solidHeader = TRUE)
                                              ),
                                              fluidRow(
                                                box(
                                                  imageOutput("plot1"),
                                                  title=tagList(shiny::icon("chart-bar"), "Tour Pattern:2019"),width=6,height=1000, status = "warning",solidHeader = TRUE),
                                                box(
                                                  imageOutput("plot2"),
                                                  title=tagList(shiny::icon("chart-bar"), "Tour Pattern:2020"),width=6,height=1000, status = "warning",solidHeader = TRUE)
                                              )
                                      ),
                              tabItem(tabName="Search_visual",
                                      fluidRow(
                                        box(width=6,
                                            fluidRow(
                                              box(width=12,
                                                  img(src="https://fscluster.org/sites/default/files/styles/core-group-featured-image/public/banner-696x321.png?itok=l7uFday9",title="COVID_image", width="100%"),height=400)
                                            ),
                                            fluidRow(
                                              box(title="word_cloud",height=580,width=12, status = "info",solidHeader = TRUE,collapsible = TRUE,
                                                  fluidRow(
                                                    box(width=12,
                                                        column(textInput("text", label = NULL, value = "COVID"),width=9),
                                                        column(actionButton("update", "change"),width=3)
                                                    )
                                                  ),
                                                  fluidRow(width=12,height=300,
                                                           box(wordcloud2Output("word_cloud",height="400px"),width=12)  
                                                  )
                                              )
                                            )
                                        ),
                                        box(title="word Cloud Frequency", height=1060, status="info", solidHeader = TRUE, collapsible = TRUE,
                                            plotOutput("word_cloud_plot", height=1000)
                                        )
                                      )
                              )
                              )
                            )
              )
              
              # Define server logic required to draw a histogram
              server <-function(input, output) {
                url1 <- a("headline", href= test$url[1])
                url2 <- a("headline", href= test$url[2])
                url3<- a("headline", href= test$url[3])
                url4 <- a("headline", href= test2$url[1])
                url5 <- a("headline", href= test2$url[2])
                url6<- a("headline", href= test2$url[3])
                output$mymap <- renderLeaflet({
                  basemap
                })
                output$Confirmed <- renderValueBox({
                  valueBox(paste0("Total",":  ",jhu_cases$confirmed[jhu_cases$country== input$xcol],"  ","(+",jhu_cases$dff[jhu_cases$country== input$xcol],")"), 
                           "Confirmed", icon = icon("fas fa-lungs-virus"), color = "yellow")
                })
                output$Death <- renderValueBox({
                  valueBox(paste0("Total",":  ",jhu_deaths$deaths[jhu_deaths$country== input$xcol],"  ","(+",jhu_deaths$dff[jhu_deaths$country== input$xcol],")"), 
                           "Death", icon = icon("fas fa-skull-crossbones"), color = "red")
                })
                output$Rec <- renderValueBox({
                  valueBox(paste0("Total",":  ",jhu_rec$rec[jhu_rec$country== input$xcol],"  ","(+",jhu_rec$dff[jhu_rec$country== input$xcol],")"), 
                           "Recovery", icon = icon("far fa-smile"), color = "blue")
                })
                output$tab1 <- renderUI({tagList("URL link:", url1)})
                output$krnews1 <- renderText({test$title[1]})
                output$krnews11 <- renderText({test$description[1]})
                output$tab2 <- renderUI({tagList("URL link:", url2)})
                output$krnews2<- renderText({test$title[2]})
                output$krnews21 <- renderText({test$description[2]})
                output$tab3 <- renderUI({tagList("URL link:", url3)})
                output$krnews3 <- renderText({test$title[3]})
                output$krnews31 <- renderText({test$description[3]})
                output$tab4 <- renderUI({tagList("URL link:", url4)})
                output$krnews4 <- renderText({test2$title[1]})
                output$krnews41 <- renderText({test2$description[1]})
                output$tab5 <- renderUI({tagList("URL link:", url5)})
                output$krnews5 <- renderText({test2$title[2]})
                output$krnews51 <- renderText({test2$description[2]})
                output$tab6 <- renderUI({tagList("URL link:", url6)})
                output$krnews6 <- renderText({test2$title[3]})
                output$krnews61 <- renderText({test2$description[3]})
                
                output$Patient_map <- renderLeaflet({
                  leaflet(Case) %>%
                    setView(lng=126.9784, lat=36.01, zoom=7) %>%
                    addProviderTiles('CartoDB.Positron') %>%
                    addCircles(lng=~longitude, lat=~latitude, color=~Case_pal(province))
                })
                
                output$confirmed_table <- renderDataTable({
                  datatable(Case_info, options = list(paging = FALSE))
                })
                
                
                output$card_table <- renderDataTable({
                  datatable(head(view_card,100),options = list(pageLength=20, lengthMenu = c(20,50)))
                })
                
                output$tour_table <- renderDataTable({
                  datatable(view_visit_place, options = list(pageLength=10, lengthMenu = c(20,40)))
                })
                
                output$Patient_total_plot <- renderPlotly({
                  patient_total <- ggplot(Time)+geom_area(aes(x=date, y=confirmed), fill ="red", alpha=0.2)+ggtitle(label = 'COVID-19 Cumulative Cases by Day for SOUTHKOREA') +
                    theme_wsj() +
                    theme(title = element_text(size = 20,
                                               face = 'bold',
                                               family = 'NanumSquare'))
                  ggplotly(patient_total) %>% plotly::config(displayModeBar=F)
                })
                
                output$Patient_diff_plot <- renderPlotly({
                  patient_diff <- ggplot(Time)+geom_area(aes(x=date, y=diff), fill ="red", alpha=0.4)+ ggtitle(label = 'Daily confirmed cases of COVID-19 in SOUTH KOREA') +
                    theme_wsj() +
                    theme(title = element_text(size = 20,
                                               face = 'bold',
                                               family = 'NanumSquare'))+scale_y_continuous(limits = c(0, 813))
                  ggplotly(patient_diff) %>% plotly::config(displayModeBar=F)
                })
                
                output$Patient_age_plot <- renderPlot({
                  ggDonut(Time_Age,"age","confirmed")+ theme_clean()
                })
                
                output$Patient_sex_plot <- renderPlot({
                  Gender_sex_plot
                })
                
                output$category_percentage <- renderPlot({
                  select_date <- filter(card,date >= min(ymd(as.list(input$input_month)[[1]])))
                  select_date <- filter(select_date,date <= max(ymd(as.list(input$input_month)[[2]])))[,c("month","sal_amt","cate_code","cate_name")]
                  data <- aggregate(sal_amt ~ cate_name, select_date,sum)
                  plot <- ggplot(data, aes(x=cate_name, y=sal_amt, fill=factor(cate_name)))+geom_col(width = 1, color="white")
                  plot <- plot + coord_polar()
                  plot <- plot + labs(
                    x="",
                    y=""
                  )
                  plot <- plot + theme_minimal()+
                    theme(
                      legend.text=element_text(size=12),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_text(face="bold", size=12),
                      plot.title = element_text(size=24, face="bold"),
                    )
                  plot  
                })
                
                output$category_amount_Plot <- renderPlot({
                  select_date <- filter(card,date >= min(ymd(as.list(input$input_month)[[1]])))
                  select_date <- filter(select_date,date <= max(ymd(as.list(input$input_month)[[2]])))[,c("month","sal_cnt","cate_code","cate_name")]
                  data <- aggregate(sal_cnt ~ cate_name, select_date,sum)
                  ggplot(data, aes(reorder(cate_name, sal_cnt), y=sal_cnt, fill=factor(cate_name)))+geom_col() + coord_flip()+
                    theme(axis.title.x = element_blank(),
                          axis.text.x = element_text(size=12),
                          axis.text.y = element_text(size=12),
                          axis.title.y = element_blank(),
                    )
                })
                
                output$categorical_consumption_plot <- renderPlotly({
                  select_date <- filter(card,date >= min(ymd(as.list(input$input_month)[[1]])))
                  select_date <- filter(select_date,date <= max(ymd(as.list(input$input_month)[[2]])))[,c("month","sal_amt","cate_code","cate_name")]
                  categorical_consumption_data <- aggregate(sal_amt ~ month+cate_code+cate_name, select_date, sum)
                  p <- ggplot(categorical_consumption_data, aes(x=month, y=sal_amt, color=cate_name)) + geom_point()+geom_path() + theme(legend.direction="horizontal", legend.position="bottom",legend.text=element_text(size=12))
                  ggplotly(p) %>% config(displayModeBar=F)
                })
                
                output$all_period_plot <- renderPlot({
                  ggplot(monthly_region, aes(x=date, y=visiter,color=region))+geom_bar(stat="identity", aes(fill=region))
                })
                
                output$same_period_visiter <- renderPlot({
                  se1 <- monthly_region %>% filter(monthly_region$date=="2019-01-01"|monthly_region$date=="2019-02-01"|monthly_region$date=="2019-03-01"|monthly_region$date=="2020-01-01"|monthly_region$date=="2020-02-01"|monthly_region$date=="2020-03-01")
                  ggplot(se1, aes(x=date, y=visiter,color=region))+geom_bar(stat="identity", aes(fill=region))
                })
                
                output$continuous_period_plot <- renderPlot({
                  se2 <- monthly_region %>%  filter(monthly_region$date > "2019-07-01")
                  ggplot(se2, aes(x=date, y=visiter,color=region))+geom_bar(stat="identity", aes(fill=region))
                })
                
                output$plot1 <- renderImage({
                  outfile <- tempfile(fileext='.gif')
                  staticplot1 = ggplot(regiondat1, aes(visiter, region, fill = as.factor(region), color = as.factor(region)))+geom_bar(stat="identity", aes(fill=region))
                  anim1 = staticplot1 + transition_states(year, transition_length = 4, state_length = 1) +ease_aes ( 'cubic-in-out') + labs(title="Tourism State : {closest_state}",caption = "DateSource: https://know.tour.go.kr/")
                  animate(anim1, 200, fps = 20,  width = 1200, height = 1000, renderer = gifski_renderer("gganim1.gif"))
                  anim_save("outfile.gif", animate(anim1)) # New
                  
                  list(src = "outfile.gif",
                       contentType = 'image/gif',
                       width="100%"
                  )}, deleteFile = TRUE)
                
                output$plot2 <- renderImage({
                  outfile <- tempfile(fileext='.gif')
                  staticplot2 = ggplot(regiondat2, aes(visiter, region, fill = as.factor(region), color = as.factor(region)))+geom_bar(stat="identity", aes(fill=region)) + scale_x_continuous(limits = c(0,6000000))
                  anim2 = staticplot2 + transition_states(year, transition_length = 4, state_length = 1) +ease_aes ( 'cubic-in-out') + labs(title="Tourism State : {closest_state}",caption = "DateSource: https://know.tour.go.kr/")
                  animate(anim2, 200, fps = 20,  width = 1200, height = 1000, renderer = gifski_renderer("gganim2.gif"))
                  anim_save("outfile.gif", animate(anim2)) # New
                  
                  list(src = "outfile.gif",
                       contentType = 'image/gif',
                       width="100%"
                  )}, deleteFile = TRUE)
                
                output$visiter_place <- renderLeaflet({
                  leaflet(Case) %>%
                    setView(lng=126.9784, lat=36.01, zoom=7) %>%
                    addProviderTiles('CartoDB.Positron') %>%
                    addCircles(lng=~longitude, lat=~latitude, color=~Case_pal(province))
                })
                
                output$word_cloud <- renderWordcloud2({
                  input$update
                  t <- isolate(word_cloud_function(iconv(input$text, to="UTF-8")))
                  wordcloud2(t, size = 1,color = "random-light", backgroundColor = "white")
                })
                
                output$word_cloud_plot <- renderPlot({
                  input$update
                  t <- isolate(word_cloud_function(iconv(input$text, to="UTF-8")))
                  t_dataframe <- as.data.frame(t)
                  t_dataframe$Freq <- as.numeric(t_dataframe$Freq)
                  ggplot(t_dataframe[c(1:10),], aes(x=total, y=Freq)) + geom_bar(stat="identity")
                })
                
              }
              
              
              # Run the application 
              shinyApp(ui = ui, server = server)

              
