library(readxl)
library(readODS)
excel_sheets("BirthRate_81_107.xls")
ods_sheets("BirthRate_81_107.ods")
#####處理第一頁####
Birth_1<-read_ods(path = "D:/Users/aa225/Documents/FinalTerm/BirthRate_81_107.ods",col_names = T, 
         sheet = 1, na = "", skip = 5, formula_as_formula = FALSE, range = NULL)
getNrOfSheetsInODS("BirthRate_81_107.ods")
#去除不要的欄位
Birth_1<-Birth_1[-271:-276,1:19]
grep("[0-9]",Birth_1$`Year (Month)`)
BirFrom_1981<-Birth_1[grep("[0-9]",Birth_1$`Year (Month)`),]
strsplit (BirFrom_1981$`Year (Month)`[1:2],"19")[[1]][2]
#清理年分
for (n in 1:nrow(BirFrom_1981)) {
  if(grepl("19",BirFrom_1981$`Year (Month)`[n])){
    BirFrom_1981$`Year (Month)`[n]<-
      paste0("19",strsplit (BirFrom_1981$`Year (Month)`[n],"19")[[1]][2])
  }else if(grepl("20",BirFrom_1981$`Year (Month)`[n])){
    BirFrom_1981$`Year (Month)`[n]<-
      paste0("20",strsplit (BirFrom_1981$`Year (Month)`[n],"20")[[1]][2])
  }
}
library(ggplot2)
logmax<-log10(max(BirFrom_1981$`Crude Birth Rate (0/00)`))
BirFrom_1981$xxxxx<-NA
for (n in 1:nrow(BirFrom_1981)) {
  BirFrom_1981$xxxxx[n]<-log10(BirFrom_1981$`Crude Birth Rate (0/00)`[n])/logmax
}


ggplot(BirFrom_1981,
       aes(x = `Year (Month)`,
           y = `Crude Birth Rate (0/00)`),
       state = "identity")+
  geom_point()

#####處理第二頁之後####
#出生資料表
library(dplyr)
library(reshape2) #for melt()
BirthCounty<-NULL
BirthCounty<-data.frame(
  Locality=read_ods(path = "D:/Users/aa225/Documents/FinalTerm/BirthRate_81_107.ods",col_names = T, 
           sheet = 2, na = "", skip = 5, formula_as_formula = FALSE, range = NULL)[1:25,1])
for (n in 2:getNrOfSheetsInODS("BirthRate_81_107.ods")) {
  BirthCounty<-full_join(BirthCounty,
                         read_ods(path = "D:/Users/aa225/Documents/FinalTerm/BirthRate_81_107.ods",
                                  col_names = T,sheet = n, na = "", skip = 5, 
                                  formula_as_formula = FALSE, range = NULL)[,c(1,3)],
                         by =  "Locality"
  )
}
colnames(BirthCounty)[-1]<-ods_sheets("BirthRate_81_107.ods")[-1]
BirthCounty$Locality<-gsub(" ","",BirthCounty$Locality)
BirthCounty$Locality<-gsub("[a-zA-Z]","",BirthCounty$Locality)
BirthCounty<-BirthCounty[c(-26:-35,-59),]
#分得乾乾淨淨
dealBirth<-BirthCounty[!complete.cases(BirthCounty),]
BirthCounty_1<-BirthCounty[complete.cases(BirthCounty),]
#合併儲存格
dealBirthFinal<-NULL
dealBirth_1<-colSums( dealBirth[grepl("桃園",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("桃園市",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("北",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("新北市",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("宜蘭",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("宜蘭縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("苗栗",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("苗栗縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("彰化",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("彰化縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("南投",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("南投縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("雲林",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("雲林縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("屏東",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("屏東縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("臺東",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("臺東縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("花蓮",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("花蓮縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("澎湖",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("澎湖縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("基隆",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("基隆市",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("連江",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("連江縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("新竹市",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("新竹市",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("新竹縣",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("新竹縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("嘉義市",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("嘉義市",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("嘉義縣",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("嘉義縣",dealBirth_1))
dealBirth_1<-colSums( dealBirth[grepl("臺灣省",dealBirth$Locality),-1],na.rm =T)
dealBirthFinal<-rbind(dealBirthFinal,c("臺灣省",dealBirth_1))
colnames(dealBirthFinal)[1]<-"Locality"
dealBirthFinal<-data.frame(dealBirthFinal)
dealBirthFinal$Locality<-as.factor(dealBirthFinal$Locality)
colnames(dealBirthFinal)[-1]<-ods_sheets("BirthRate_81_107.ods")[-1]

BirthCounty.trans<-BirthCounty[,1:2]
for (i in 1:nrow(BirthCounty)) {
  transrow<-BirthCounty[i,-1]
  for (j in 2:ncol(transrow)-1) {
    transrow.col<-NULL
    transrow.col<-as.numeric(transrow[j]-sum(transrow[c(j-1,j,j+1)])/3)
    BirthCounty.trans[i,j+1]<-transrow.col
  }
  
}
BirthCounty.trans<-BirthCounty.trans[,-2]
colnames(BirthCounty.trans)[-1]<-ods_sheets("BirthRate_81_107.ods")[-1:-2]
BirthCounty

#寬表轉長表,以名字作依據
BirthCounty.m <- melt(dealBirthFinal,id.vars = "Locality")
BirthCounty.m$value<-as.numeric(BirthCounty.m$value)
head(BirthCounty.m,5)
#各縣市出生率熱度圖作法
ggplot(BirthCounty.m, aes(variable, Locality)) + 
  geom_tile(aes(fill = value),
            colour = "white")+ 
  scale_fill_gradient(
    low = "white",high = "steelblue")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5))

#-----------------------------------月份----------------------
library(dplyr)
library(ggplot2)

Bir_by_month<-Birth_1[grep("月",Birth_1$`Year (Month)`),]
Bir_by_month$Year<-NA
for (n in 1:nrow(Bir_by_month)%/%12) {
  m<-12*n-11
  k<-12*n
  Bir_by_month$Year[m:k]<-(n+1999)
}
Bir_by_month<-Bir_by_month[,c(20,1:19)]
Bir_by_month$Year[217:221]<-2018

Bir_by_month_final<-group_by(Bir_by_month,`Year (Month)`)%>%
  summarise(
    "Crude Birth Rate (0/00)"=mean(`Crude Birth Rate (0/00)`)
  )
Bir_by_month_final$number<-c(1,7,9,2,8,10,11,12,3,5,6,4)
Bir_by_month_final<-arrange(Bir_by_month_final,number)
Bir_by_month_final$`Year (Month)`<-factor(Bir_by_month_final$`Year (Month)`,
                                          levels = Bir_by_month_final$`Year (Month)`)

qplot(`Year (Month)` , `Crude Birth Rate (0/00)`, 
      data = Bir_by_month_final)+theme_bw()

###############各縣市特愛生效面輛####
library(maptools) #for readShapeSpatial()
library(rgdal)#for fortify()
library(rgeos) #for fortify()
tw_new <- readShapeSpatial("TOWN_MOI_1070330.shp") 
head(tw_new$COUNTYID)
tw_new.df <- 
  fortify(tw_new, region = "COUNTYID") 
tw_new.df

dealBirthFinal_1<-dealBirthFinal
dealBirthFinal_1<-NULL
dealBirthFinal_1$Locality<-dealBirthFinal$Locality
dealBirthFinal_1<-data.frame(dealBirthFinal_1,
      matrix(NA,nrow = 18,ncol = 27))
for (i in 2:ncol(dealBirthFinal)) {
  for (j in 1:17) {
    dealBirthFinal_1[j,i]<-as.numeric(as.character(dealBirthFinal[j,i]))/
      as.numeric(as.character(dealBirthFinal[18,i]))
  }
}
colnames(dealBirthFinal_1)[-1]<-ods_sheets("BirthRate_81_107.ods")[-1]


BirFrom_1994to2017<-melt(dealBirthFinal_1[,c(-2,-27:-28)],id.vars = "Locality"  )
BirFrom_1994to2017$variable<-
  as.character(BirFrom_1994to2017$variable)
BirFrom_1994to2017$variable<-
  as.numeric(BirFrom_1994to2017$variable)
BirFrom_1994to2017$value<-
  as.numeric(BirFrom_1994to2017$value)
BirFrom_1994to2017<-arrange(BirFrom_1994to2017,variable)
BirFrom_1994to2017$Animal<-0
for (n in 1:nrow(BirFrom_1994to2017)%/%18) {
  m<-18*n-17
  k<-18*n
  p<-(n+9)%%12+1
  BirFrom_1994to2017$Animal[m:k]<-p
}


BirFrom_1994to2017$Animal<-as.factor(BirFrom_1994to2017$Animal)
BirFrom_1994to2017<-BirFrom_1994to2017%>%
  group_by(Animal,Locality)%>%summarise(value = sum(value))
maxBirCounty<-BirFrom_1994to2017%>%
  arrange(desc(value)) %>%
  group_by(Locality) %>%arrange(Locality)
