#데이터 처리 #####################################
library(jjstat)
library(knitr)
source("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/synllabic_function.R")
getwd()
setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")

#데이터보기
printall <- function(data) print(data, n=Inf)
pall <- function(data) print(data, n=Inf)
dall <- function(data) data.frame(data)

load(file ="kge_bind0a.RData")
load(file ="kge_bind10a.RData")
load(file ="kge_bind1a.RData")
load(file ="kge_bind2a.RData")
load(file =" Kge_person.RData")
load(file = "Kge_arrangeRule.RData")
load(file = "Kge_word.RData")


###
library(readxl)
setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")
kge20240330 <- read_excel("kge20240330.xlsx")

Kge_person = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",  sheet = "제보자분류")
Kge_person
## 피실험자----
Kge_person
save(Kge_person, file =" Kge_person.RData")



Kge_word = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",  sheet = "단어리스트")
Kge_word%>% print(n=Inf)
Kge_accentRule = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx", sheet = "accentRule")
Kge_accentRule %>% printall()

Kge_weightRule = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx", sheet = "weightRule")
Kge_weightRule %>% pall()
Kge_arrangeRule = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",   sheet = "arrangeRule")
Kge_arrangeRule %>% pall()

# save(Kge_arrangeRule, file = "Kge_arrangeRule.RData")
# save(Kge_word, file = "Kge_word.RData")

Kge_arrangeRule%>% filter(type=="초성") %>% print(n=Inf)
Kge_arrangeRule%>% filter(type=="중성") %>% print(n=Inf)
Kge_arrangeRule%>% filter(type=="종성") %>% print(n=Inf)


# Kge_word %>% str()
Kge_word %>% print(n=Inf)
Kge_word %>% filter(동음이의 == 1) %>% print(n=Inf)



#개인 데이터 import
Kge_jbs = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",  sheet = "JBS")
Kge_khjo = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx", sheet = "KHJO")
Kge_lhb = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",   sheet = "LHB")
Kge_jmh = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",  sheet = "JMH")
Kge_kdc = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",  sheet = "KDC")
Kge_kjy = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240330.xlsx",  sheet = "KJY")

#데이터 체크
Kge_jbs %>% pall()
Kge_khjo %>% pall()
Kge_lhb %>% pall()
Kge_jmh %>% pall()
Kge_kdc %>% pall()
Kge_kjy %>% pall()
#데이터 입력이 일부만 되어 있어서 없는 데이터는 NA처리
Kge_kjy <- Kge_kjy %>% replace_df(pattern = "NULL")

##데이터 결합 측저데이터와 개인정보 데이터와 결합
Kge_jbs1 <- Kge_jbs %>% cbind(filter(Kge_person, speaker =="JBS") %>% select(3,4))
Kge_khjo1 <- Kge_khjo %>% cbind(filter(Kge_person, speaker =="KHJO")%>% select(3,4))
Kge_lhb1 <- Kge_lhb %>% cbind(filter(Kge_person, speaker =="LHB")%>% select(3,4))
Kge_jmh1 <- Kge_jmh %>% cbind(filter(Kge_person, speaker =="JMH")%>% select(3,4))
Kge_kdc1 <- Kge_kdc %>% cbind(filter(Kge_person, speaker =="KDC")%>% select(3,4))

Kge_jbs1
Kge_khjo1
Kge_lhb1
Kge_jmh1
Kge_kdc1

## 부산 5명 데이터 결합 ----
kge_bind0a = bind_rows(Kge_jbs1, Kge_khjo1, Kge_lhb1, Kge_jmh1, Kge_kdc1 )
#@ kge_bind0a데이터 확인 ---------
kge_bind0a
kge_bind0a %>% str()
kge_bind0a %>% dim()  # 6150   11


# save(kge_bind0a, file ="kge_bind0a.RData")
load(file ="kge_bind0a.RData")
load(file ="kge_bind10a.RData")
load(file ="kge_bind1a.RData")
load(file ="kge_bind2a.RData")
load(file =" Kge_person.RData")
load(file = "Kge_arrangeRule.RData")
load(file = "Kge_word.RData")
#데이터 분석 체크
kge_bind0a %>%
  filter(type=="외래어4" & word =="다이나믹") %>%
  split_kw_df_match_ko() %>% kge_weigth_ko() %>%
  data.frame()




kge_bind10a <- kge_bind0a %>% split_kw_df_match_ko() #%>% kge_weigth()
kge_bind10a$N %>% unique()

#데이터검증: 입력시 " "와 같은 빈칸들이 존재함.
kge_bind10a$A3 %>% unique()
kge_bind10a$B3 %>% unique()
kge_bind10a$C3 %>% unique()
kge_bind10a$D3 %>% unique()
# kge_bind10$d3 %>% unique()
#데이터 전처리: 빈칸 규칙 체크하여 정리
kge_bind10a$A3 <- kge_bind10a$A3 %>% replace_df(imp = "", pattern = " ")
kge_bind10a$B3 <- kge_bind10a$B3 %>% replace_df(imp = "", pattern = " ")
kge_bind10a$C3 <- kge_bind10a$C3 %>% replace_df(imp = "", pattern = " ")
kge_bind10a$D3 <- kge_bind10a$D3 %>% replace_df(imp = "", pattern = " ")
kge_bind10a$A3 %>% unique();kge_bind10$B3 %>% unique();kge_bind10$C3 %>% unique();kge_bind10$D3 %>% unique()

kge_bind10a  %>% dall()
kge_bind10a%>% kge_weigth_ko() %>%  data.frame()

# save(kge_bind10a, file ="kge_bind10a.RData")
# write_excel_csv(kge_bind10a, file ="kge_bind10a.csv")
load(file ="kge_bind10a.RData")


# @ kge_bind1a -------
kge_bind1a <- kge_bind10a%>% kge_weigth_ko()
kge_bind1a %>% dall()

# # number of 외래어4 weight type
# kge_bind1a$Wz %>% unique()
#
# 성조에 따른 분류 개수:  6150
kge_bind1a %>% nrow()
#성조에 아무것도 없는 것들의 개수: 5643
kge_bind1a %>% filter(성조 != "") %>% nrow()
#차이: 507
kge_bind1a %>% nrow() - kge_bind1a %>% filter(성조 != "") %>% nrow()

# save(kge_bind1a, file ="kge_bind1a.RData")
# write_excel_csv(kge_bind1a, file ="kge_bind1a.csv")
load(file ="kge_bind1a.RData")



# @ kge_bind2a 성조가 NA인 것을 제거하고 다시 생성-------
kge_bind2a <- kge_bind1a %>% filter(성조 != "")

# 5643
kge_bind2a%>% nrow()

# save(kge_bind2a, file ="kge_bind2a.RData")
# write_excel_csv(kge_bind2a, file ="kge_bind2a.csv")
load(file ="kge_bind2a.RData")


## 분석시 결합할 패턴 ----
accent_pattern = rbind(
  고유어1  = c(pattern = "H(H)_1", imp="H(H)"),
  고유어2  = c(pattern = "LH(H)", imp ="LH"),
  고유어3  = c(pattern = "LHH", imp ="LLH"),
  외래어2  = c(pattern = "LH_f", imp ="LH"),
  외래어3  = c(pattern = "LLH_f", imp ="LLH"),
  외래어4  = c(pattern = "LLLH_f", imp ="LLLH"),
  가상어2  = c(pattern = "", imp =""),
  가상어3  = c(pattern = "", imp ="")
) %>% data.frame() %>% rownames_to_column("word")

accent_pattern

#패턴 적용

# kge_bind2a %>% filter(type =="고유어1") %>% auto_pattern()
kge_bind2a %>% filter(type =="고유어1") %>% auto_pattern("고유어1")
kge_bind2a %>% filter(type =="고유어2") %>% auto_pattern("고유어1")
kge_bind2a %>% filter(type =="고유어2") %>% auto_pattern("고유어1")


#분석자료를 데이터 프레임으로-----
accent_table = function(data,
                        cols = "a1",
                        rows = "성조",
                        title = "",
                        trans = TRUE,
                        cex = 1.3,
                        color = TRUE,
                        ylab = "onset", xlab="accent",
                        sub = NULL,
                        plot= FALSE,
                        type = "res"){

  res = data  %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = rows, values_from = "Freq") %>%
    rename(accent = cols) %>% tibble::column_to_rownames("accent")

  res_df = data  %>% as.matrix() %>%data.frame()

  if(plot){
    res_mosaicplot = res %>%
      mosaicplot(color = color, ylab = ylab, xlab=xlab,
                 cex.axis = cex,
                 main = paste("Contigency Table of var(", title,")"),
                 sub= sub)
  }else{
    res_mosaicplot=NULL
  }
  # sub="Friendly, M. (1994). Mosaic displays for multi-way contingency tables"

  res_all = list(res, res_df, res_mosaicplot)

  if(trans){res= res %>% t()

  }else{res}


  switch(type,
         res = res,
         df = res_df,
         g = res_mosaicplot,
         all = res_all )
}



#*1 accent_table관측치에 대한 발화자별 패턴 분석 ---------
# 패턴 변경이 없는 경우
kge_bind2a %>% filter(type=="고유어1") %>%
  # replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", color = 2:5)

## 함수적용하여 변경
kge_bind2a %>% filter(N==1) %>%
  auto_pattern("고유어1")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", "고유어1", plot = T)

#고유어1
kge_bind2a %>% filter(type=="고유어1") %>%
  auto_pattern("고유어1")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", "고유어1")

#고유어2
kge_bind2a %>% filter(type=="고유어2") %>%
  auto_pattern("고유어2")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", "고유어2")

#*1> 패천적용 accent_table 만들기 -----
# observed table result -
obs_table = function(data, typesel = NULL, cex=1.3,plot=TRUE){
  data %>% filter(type==typesel) %>%
    auto_pattern(typesel)%>%
    select(성조, speaker) %>% table() %>%
    accent_table("speaker", "성조", typesel, cex=cex,  plot = plot)
}

kge_bind2a %>% obs_table("고유어1")
kge_bind2a %>% obs_table("고유어2")
kge_bind2a %>% obs_table("고유어3", cex=1)
kge_bind2a %>% obs_table("외래어2")
kge_bind2a %>% obs_table("외래어3")
kge_bind2a %>% obs_table("외래어4", cex =1)
kge_bind2a %>% obs_table("가상어2")
kge_bind2a %>% obs_table("가상어3")


# #longdata transfomation
# long_df = function(data, names_to = "speaker",
#                    values_to = "freq",
#                    cols = 2:ncol(data1),
#                    rowname ="accent"){
#
#   # colnames0 = colnames(data)
#   data1 = data %>% data.frame %>%
#     rownames_to_column("accent")
#
#   data2 <- data1%>%
#     pivot_longer(names_to = "speaker",
#                  values_to = "freq",
#                  cols=cols)
#   data2
#
# }
##> 피실험자 정보 결합 -----
kge_bind2a %>% obs_table("고유어1") %>% long_df() %>% merge(Kge_person, by="speaker" )
kge_bind2a %>% obs_table("고유어2") %>% long_df() %>% merge(Kge_person, by="speaker" )

kge_bind2a %>% obs_table("고유어1") %>% long_df() %>%
  inner_join(Kge_person, by="speaker" ) %>%
  tidyr::unite(speaker , c(speaker, age, area)) %>% arrange(speaker)


#피험자 정보 데이터와결합
# combind_person= function(data){
#   load(file =" Kge_person.RData")
#
#   data1=  data %>%long_df() %>%
#     inner_join(Kge_person, by="speaker" ) %>%
#     tidyr::unite(speaker , c(speaker, age, area))%>%
#     arrange(speaker) %>%
#     dplyr::select( name, gender,speaker,accent, freq)
#
#   data1
#
# }

#데이터와 사용자 정보를 결합하는 함수 코드
### s 피실험자 정보결합+발화 패턴을 모자이크 플롯을 이용하여 확인-----
kge_bind2a %>% obs_table("고유어1") %>% combind_person()
kge_bind2a %>% obs_table("고유어2") %>% combind_person()
kge_bind2a %>% obs_table("고유어3", cex=1) %>% combind_person()
kge_bind2a %>% obs_table("외래어2") %>% combind_person()
kge_bind2a %>% obs_table("외래어3") %>% combind_person()
kge_bind2a %>% obs_table("외래어4", cex =1) %>% combind_person()
kge_bind2a %>% obs_table("가상어2") %>% combind_person()
kge_bind2a %>% obs_table("가상어3") %>% combind_person()

## >s 발화 패턴 체크 : 개별로 분리 -----
kge_bind2a %>% obs_table("고유어1") %>% combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("고유어2") %>% combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("고유어3", cex=1) %>% combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("외래어2") %>% combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("외래어3") %>% combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("외래어4", cex =1) %>% combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("가상어2") %>% combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("가상어3") %>% combind_person()%>%  patternGraph(tolong=FALSE)




#두개의 샘플을 비교하여 agree를 체크
# 성조 일치 여부 :  고유어 1에 대하여 20대와 60대의 일치 여부
kge_bind2a %>% replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  filter(age == 60 & 지역=="부산" &type=="고유어1") %>%
  select(speaker, word, 성조, age, 지역) %>%
  inner_join(
    kge_bind2a%>% replace_df(pattern = "H(H)_1", imp="H(H)")  %>%
      filter(age == 20 & 지역=="부산"  &type=="고유어1")%>%
      select(speaker, word, 성조, age, 지역),
    by ="word")

#*연구문제 2 각 성조형에 대한 제보자들의 응답 일치 정도-----
kge_bind2a %>% replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  filter(age == 60 & 지역=="부산" &type=="고유어1") %>%
  select(speaker, word, 성조, age, 지역) %>%
  inner_join(
    kge_bind2a%>% replace_df(pattern = "H(H)_1", imp="H(H)")  %>%
      filter(age == 20 & 지역=="부산"  &type=="고유어1")%>%
      select(speaker, word, 성조, age, 지역),
    by ="word", relationship = "many-to-many") %>%
  mutate(same  = ifelse(성조.x == 성조.y, "Agree","Disagree")) %>%
  rename(age60 = age.x, age20 = age.y) %>%
  select(word, age60, age20, same) %>%
  pivot_longer(names_to = "Age", values_to = "S", cols = age60:age20) %>%
  select(Age, same) %>% table() %>% accent_table("Age","same") %>%
  data.frame() %>%
  select(age60, age20)



#Agree_table  ###############
Agree_table = function(data,
                       wordkey="",
                       age1=60,
                       age2=60,
                       area1="부산",
                       area2="부산",
                       # pattern = "H(H)_1", imp="H(H)",
                       n=15,type="Res"
){
  res = data %>%
    # replace_df(pattern = pattern, imp = imp) %>%
    auto_pattern(type = wordkey) %>%  #패턴 자동입력
    dplyr::filter(age == age1 & 지역== area1 & type == wordkey) %>%
    dplyr::select(speaker,word, 성조, age, 지역) %>%
    dplyr::arrange(speaker) %>%
    dplyr::inner_join(
      data %>%
        auto_pattern(type = wordkey) %>%
        # replace_df(pattern = pattern, imp = imp)  %>%
        dplyr::filter(age == age2 & 지역== area2  & type == wordkey)%>%
        dplyr::select(speaker,  word, 성조, age, 지역) %>%
        arrange(speaker),
      by ="word", relationship = "many-to-many") %>%
    mutate(same  = ifelse(성조.x == 성조.y, "Agree","Disagree")) #%>%
  # rename(age60 = age.x, age20 = age.y, 성조60=성조.x, 성조20=성조.y)

  res10 = res%>%
    dplyr::select(word, age.x, age.y, same)

    # colnames(res10)=c(
    #   "word",
    #   paste0("age",age1),
    #   paste0("age",age2),
    #   "same")

  res1 = res10 %>%
  # dplyr::select(word, age.x, same) %>%
    pivot_longer(names_to = "Age", values_to = "S",
                 # cols =  all_of(paste0("age",age1)):all_of(paste0("age",age2))) %>%
                 cols =  age.x:age.y) %>%
    dplyr::select(Age, same) %>% table() %>%
    accent_table("Age","same", plot = FALSE) %>%
    `colnames<-`(c(paste0(area1, age1,"대"),
                   paste0(area2, age2,"대")))

  Res = list(
             r2 = res1 %>%
               markdown_table(caption = paste0(wordkey,"에 관한",
                                               area1, age1,"대와",
                                               area2, age2, "대" )),
             head_top15 = res %>%  head(n=n),
             r1 = res1
  )

  switch(type,
         data = res1 %>% data.frame() %>%
           dplyr::select(1),
         dplyr::select(paste0(area1, age1,"대")),

         Res = Res)
}




# Kge_accentRule%>% filter(type =="고유어1")
# kge_bind2a%>% Agree_table("고유어1", 60,20, pattern = "H(H)_1", imp="H(H)")
kge_bind2a%>% Agree_table("고유어1", 60, 20)
kge_bind2a%>% Agree_table("고유어2", 60, 20)
kge_bind2a%>% Agree_table("고유어3", 60, 20)




#* 2개의 연령을 묶어서 분석------------
bind_agree_tabe = function(data, wordkey,
                           Age1,
                           Age2,
                           Area="부산",
                           plot=TRUE,
                           color=TRUE,
                           cex=1.3
                           # pattern = "H(H)_1", imp="H(H)"

){


  res = bind_cols(Agree_table(data, wordkey,
                              age1 = Age1, age2 = Age1,
                              area1 = Area, area2 = Area ,
                              # pattern = pattern, imp = imp, # remove
                              type = "data") #%>%
                  # `colnames<-`(c(paste0("",Area, Age1,"대")))
                  ,

                  Agree_table(data, wordkey,
                              age1 = Age2, age2 = Age2,
                              area1 = Area, area2 = Area ,
                              # pattern = pattern, imp = imp, # remove
                              type = "data") #%>%
                  # `colnames<-`(c(paste0("",Area, Age2,"대")))
  )



  if(plot){
    res_mosaicplot = res %>% t() %>%
      mosaicplot(color = color, ylab = "Agree/Disagree", xlab = "age",
                 cex.axis = cex,
                 main = paste("Contigency Table of var(", wordkey,")") )
  }else{
    res_mosaicplot=NULL
  }




  res = res %>% rownames_to_column("Agreement_rate")

  res1 = res
  colnames(res1)= c("Agreement_rate" ,"a1","a2")
  res1 = res1 %>% mutate(ratio1 = paste0(round(a1/sum(a1),2)*100,"%"),
                         ratio2 = paste0(round(a2/sum(a2),2)*100,"%")
  ) %>%
    mutate(
      A1 = paste0(a1,"(",ratio1,")"),
      A2 = paste0(a2,"(",ratio2,")")
    ) %>%
    dplyr::select(Agreement_rate, A1 ,A2) %>%
    `colnames<-`(c("Agreement_rate",
                   paste0("",Area, Age1,"대"),
                   paste0("",Area, Age2,"대"))
    )



  RES = list(console = res ,
             res1,
             markdown = res1 %>%
               markdown_table(font_size = 20,
                 caption = paste0(wordkey,": ",Area, Age1,"대, ",
                                  "",Area, Age2,"대")),
             plot= res_mosaicplot)
  RES
}
#*> 연구결과2-----
#*# kge_bind2a %>% bind_agree_tabe("고유어1",20, 60, pattern = "H(H)_1", imp="H(H)")
kge_bind2a %>% bind_agree_tabe("고유어1",60, 20)
kge_bind2a %>% bind_agree_tabe("고유어2",60, 20)
kge_bind2a %>% bind_agree_tabe("고유어3",60, 20)
kge_bind2a %>% bind_agree_tabe("외래어2",60, 20)
kge_bind2a %>% bind_agree_tabe("외래어3",60, 20)
kge_bind2a %>% bind_agree_tabe("외래어4",60, 20)
kge_bind2a %>% bind_agree_tabe("가상어2",60, 20)
kge_bind2a %>% bind_agree_tabe("가상어3",60, 20)




##한글변형 코드 ------------
patternGraph1 = function(data,
                            type="data",
                            raw=TRUE,
                            ncol=NULL,
                            yadd =0.09,
                            strip_size = 16,
                            axis_size = 15,
                            text_size = 13
){

  if(raw){
    data1 <- data
    data_long <- data1$chi_table %>%
      rownames_to_column("syllabic") %>%
      pivot_longer(names_to = "성조형",
                   values_to = '관측기대비율',
                   cols = 2: (ncol(data1$chi_table)+1) )
  }else{
    data1 <- data

    data_long <- data1 %>%
      rownames_to_column("syllabic") %>%
      pivot_longer(names_to = "성조형", values_to = '관측기대비율',
                   cols=2: (ncol(data1)+1) )

  }

  g = data_long%>% ggplot(aes(x = 성조형, y = 관측기대비율))+
    geom_bar(stat = "identity", aes( fill = 성조형),
             position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    geom_text(aes(label = round(관측기대비율,2)), hjust = -0.1, size = 4)+
    ylim(0,max(data_long$관측기대비율)+ yadd)+
    coord_flip()+
    theme_bw()+
    theme(axis.text = element_text(size= text_size),
          axis.title = element_text(size= axis_size),
          strip.text = element_text(size= strip_size)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ syllabic , ncol = ncol)


  res = list(data, data_long, g)
  res1 = list(g)

  switch(type, all= res, data=res1)
}


# chisq table observed/Expected table
kge_chisq_table = function(data,
                           v1="a1",
                           v2="성조",
                           title ="Table",
                           type = "res2",
                           digits = 3,yadd=0.1,ncol=NULL,
                           trans = FALSE,
                           ko = TRUE)
{

  data =  data %>%
    dplyr::select(all_of(v1), all_of(v2)) %>%
    table()
  # 최종결과 에 포함
  data_margin = data %>% addmargins() %>%
    accent_table( v1, v2, trans = trans)
  #
  #chisq.test
  Onset_or_Coda_Accent_Contingency_table <- data
  res = chisq.test(Onset_or_Coda_Accent_Contingency_table)
  res_df = chisq.test(data)%>% broom::tidy()
  res_report = chisq.test(data)%>% report::report()

  chi_mag = paste0(" [chisq = ",round(res$statistic, digits),
                   ", df = ",res$parameter,
                   ", p = ", format_number(res$p.value, digits),"]" )
  # res$statistic
  # res$parameter
  # res$p.value

  if(nrow(data) != 1){
    chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
      tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
      tibble::column_to_rownames(v1) %>%
      Round(digits)


    # g = chi_table %>%  patternGraph()



  }else{

    # chi_table = (res$observed / res$expected)
    chi_table =
      rbind(
        observed = data %>%
          accent_table( v1, v2, trans = trans),
        expected = res$expected,
        obs_expected_ratio = (res$observed / res$expected)
      ) %>% Round(digits)

    # g= NULL
  }
  #


  chi_table_md = chi_table %>%
    markdown_table(caption = paste0(title,chi_mag),
                   digits = digits,
                   general = NULL)


  # 결과를 정리하여 나타내는 값들
  result = list(chisq_test = res,
                margin = data_margin %>%
                  markdown_table(caption = paste0(title,"Contingency table"),
                                 general = NULL),
                chi_table_md,
                chi_table = chi_table)


  #패턴 그래프 ko적용
  if(ko){
    graph = patternGraph1_ko(chi_table, raw = FALSE,
                             yadd = yadd, ncol = ncol)
  }else{
    graph = patternGraph1(chi_table, raw = FALSE,
                          yadd = yadd, ncol = ncol)
  }

  result1 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chisq_report = res_report,
    chi_table = chi_table ,
    g = graph,
    chi_table_md)



  result2 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chisq_report = res_report,
    chi_table = chi_table ,
    # g = patternGraph1(chi_table,raw = FALSE),
    chi_table_md)


  switch(type,
         ct = data,
         df = data.frame(data),
         margin = data_margin,
         chisq_test = res,
         chisq_df = res_df,
         chisq_report = res_report,
         chitable = chi_table,
         res1 = result,
         res2 = result1,
         res3 = result2)
}



kge_bind2a %>% filter(지역 =="부산" & type=="고유어1") %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a1","성조", "고유어1 부산")

#
# kge_bind2 %>% filter(지역 =="부산" & type=="고유어1") %>%
#   auto_pattern("고유어1") %>%
#   kge_chisq_table("a1","성조", "고유어1 부산")


# kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
#   auto_pattern("고유어1") %>% select(w1f,성조) %>% table() %>%
#   accent_table("성조", "w1f") %>%
#   patternGraph2( raw = F, yadd= 20)




# > 고유어1 부산  --------------------------------------------------------------

#고유어1 초성
kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 onset")

#고유어1 초성 그래프 전환
kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 onset") %>%
  patternGraph2()



#* 고유어1 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1")%>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("w1f","성조", "고유어1 부산 첫음절 종성 weight ")

#* 고유어1 부산 종성 2개 weight (light, heavy) 그래프 전환
kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("w1f","성조", "고유어1 부산 첫음절 종성 weight ") %>%
  patternGraph2()

#고유어1 CA
kge_bind2a %>% filter(지역 =="부산" & type=="고유어1") %>%
  select(a1, 성조) %>% table() %>%
  accent_table("a1","성조") %>% FactoMineR::CA()



# > 고유어2  부산 -----------------------------------------------------------------
kge_bind2a %>% data.frame()

#고유어2 초성
kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern("고유어2") %>%
  kge_chisq_table("a1","성조", "고유어2 부산 onset")

#고유어2 초성 그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern("고유어2") %>%
  kge_chisq_table("a1","성조", "고유어2 부산 onset")%>%
  patternGraph2()

#* 고유어2 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern("고유어2") %>%
    kge_chisq_table("weigth_comb2","성조", "고유어2 부산 첫음절 종성 weight ")


#* 고유어2 부산 종성 2개 weight (light, heavy)
#* # X-Heavy
kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern("고유어2") %>%
  kge_chisq_table("w2f","성조", "고유어2 부산 두번째 음절 -> 종성 weight ")



kge_bind2a$w1f
kge_bind2a %>% data.frame()
#* 고유어2 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern("고유어2") %>%
  kge_chisq_table("w1f","성조", "고유어2 부산 첫음절 종성 weight ") %>%
  patternGraph2()




kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern("고유어2") %>%
  unite(onsetcoda, c(a1, w1f), sep = " / ") %>%
kge_chisq_table("onsetcoda","성조", "고유어2 부산 첫음절 종성 weight ")



# > 고유어3 부산 ---------------------------------------------------------------
kge_bind2a %>% str()
# 고유어3초성
kge_bind2a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("a1","성조", "고유어3 부산 onset")

# 고유어3초성 그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("a1","성조", "고유어3 부산 onset")%>%
  patternGraph1()

#* 고유어3 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("weigth_comb","성조", "고유어3 부산 첫음절 종성 weight ")
#* 고유어3 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("weigth_comb3","성조", "고유어3 부산 첫음절 종성 weight ")


#* 고유어3 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("w1f","성조", "고유어3 부산 첫음절 종성 weight ") %>%
  patternGraph2()


# > 외래어2 부산 ---------------------------------------------------------------

#외래어2 초성
kge_bind2a %>% filter(지역 =="부산" & type=="외래어2") %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("a1","성조", "외래어2 부산 onset")

# 외래어2 그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="외래어2") %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("a1","성조", "외래어2 부산 onset")%>%
  patternGraph2()

#* 외래어2 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("w1f","성조", "외래어2 부산 첫음절 종성 weight ")


#* 외래어2 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("w1f","성조", "외래어2 부산 첫음절 종성 weight ") %>%
  patternGraph2()



# > 외래어3 부산 ---------------------------------------------------------------

# 외래어3 초성
kge_bind2a %>% filter(지역 =="부산" & type=="외래어3") %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("a1","성조", "외래어3 부산 onset")

# 외래어3 그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="외래어3") %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("a1","성조", "외래어3 부산 onset")%>%
  patternGraph2()


#* 외래어3 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("w1f","성조", "외래어3 부산 첫음절 종성 weight ")


#* 외래어3 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("w1f","성조", "외래어3 부산 첫음절 종성 weight ") %>%
  patternGraph2()


# > 외래어4 부산 ---------------------------------------------------------------

# 외래어4 초성
kge_bind2a %>% filter(지역 =="부산" & type=="외래어4") %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("a1","성조", "외래어4 부산 onset")

# 외래어4 그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="외래어4") %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("a1","성조", "외래어4 부산 onset")%>%
  patternGraph2()


#* 외래어4 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("w1f","성조", "외래어4 부산 첫음절 종성 weight ")


#* 외래어4 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("w1f","성조", "외래어4 부산 첫음절 종성 weight ") %>%
  patternGraph2()



# > 가상어2 부산 ---------------------------------------------------------------

# 가상어2 초성
kge_bind2a %>% filter(지역 =="부산" & type=="가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("a1","성조", "가상어2 부산 onset")

# 가상어2 그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("a1","성조", "가상어2 부산 onset")%>%
  patternGraph2(yadd=0.4)


#* 가상어2 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("w1f","성조", "가상어2 부산 첫음절 종성 weight ")


#* 가상어2 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("w1f","성조", "가상어2 부산 첫음절 종성 weight ") %>%
  patternGraph2()


# > 가상어3 부산 ---------------------------------------------------------------

# 가상어3 초성
kge_bind2a %>% filter(지역 =="부산" & type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a1","성조", "가상어3 부산 onset", yadd= 0.3)

# 가상어3 그래프 전환
kge_bind2a %>% filter(지역 =="부산" & type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a1","성조", "가상어3 부산 onset")%>%
  patternGraph2()


#* 가상어3 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("w1f","성조", "가상어3 부산 첫음절 종성 weight ")


#* 가상어3 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("w1f","성조", "가상어3 부산 첫음절 종성 weight ") %>%
  patternGraph2()

#plot recheck
kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  select(성조,a1, a3) %>% unite(onset_coda, a1:a3, sep = " / ") %>%
  table() %>% t() %>% FactoMineR::CA() %>% plot()



## 대응분석 --------------

library(FactoMineR) #대응분석 CA함수
library(factoextra) #대응분석 plot함수
###
library(RColorBrewer)
# Greens
brewer.pal.info


kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  select(성조,a1, a3) %>% unite(onse, a1:a3, sep = " / ") %>%
  table() %>% t() %>% ca::mjca()


kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  select(성조,a1, a3) %>% unite(onse, a1:a3, sep = " / ") %>%
  table() %>% t() %>% ca::mjca()%>%
  plot( arrows=c(F, F), col= c("black","red","blue"))

kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  select(성조,a1, a3) %>% unite(on_co, a1:a3,sep = "/") %>%
  table() %>% t() %>% ca::mjca()%>%
  plot( arrows=c(F, F), col= c("black","red","blue"))


#FactorMineR
aaaa <- kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  select(성조,a1, a3) %>% unite(onset_coda, a1:a3, sep = " / ") %>%
  table() %>% t() %>% FactoMineR::CA() #%>% summary()

aaaa
aaaa$eig[1,2];aaaa$eig[2,2]

aaaa %>% str()
aaaa %>% summary()

kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  xtabs(formula = ~ 성조+ a1 + W2) %>% ca::mjca() %>% plot()

kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  select(성조,a1, a3) %>% unite(onset_coda, a1:a3, sep = " / ") %>%
  table() %>% t() %>% FactoMineR::CA() %>%
  # table() %>% t() %>% ca::Ca() %>%
  factoextra::fviz_ca_biplot(
    repel = T,
    col.row ="black",
    col.col = "contrib",
    gradient.cols = c("red","tomato"),
    # gradient.cols = c("#0066CC",
    #                   "#009966",
    #                   "#996600",
    #                   "#660000",
    #                   "#9933CC"),
    labelsize = 4,
    pointsize = 3,
    arrow = c(F, T),
    # map ="rowprincipal",
    map ="symbiplot",
    title = "고유어2 onset + coda ")+
  theme_bw()+
  theme(legend.position = "none")


kge_bind2a %>% data.frame()

# 대응분석결과 #####
# 변수를 새롭고 구성

kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  xtabs(formula = ~ 성조+ a1 + W2) %>% ca::mjca() %>% plot()

#
# ca_analysis = function(data, typekey= "",
#                        colsel= "weigth_comb2",
#                        area="부산",
#                        col = c("darkgreen","red","blue"),
#                        arrows = c(F, T),
#                        col.row = "gray30",
#                        pointsize = 3,
#                        labelsize = 4
#                        ){
#
#   res0 <- data %>% filter(지역 == area & type == typekey) %>%
#     select(성조, a1, all_of(colsel)  ) %>%
#     # unite(onset_coda, c(a1, all_of(colsel) ), sep = " / ") %>%
#     # table() %>% t() %>%
#     xtabs(formula = ~ 성조+ a1 + all_of(colsel)) %>%
#     # ca::mjca()
#     FactoMineR::MCA()
#
#   res1 = res0 %>% summary()
#   # res3 = res0 %>% fviz_screeplot(addlabels = TRUE)
#
#   explain =   round(res0$eig[1,2] + res0$eig[2,2],2)
#   # res2 <- factoextra::fviz_ca_biplot(res1, repel = T, col.row ="black")
#   # res2 <- plot(res0, arrows = arrows, col = c("black","red","blue"))
#   res2 <- res0 %>%  factoextra::fviz_ca_biplot(
#                     repel = T,
#                     col.row = col.row ,
#                 col.col = "contrib",
#                 gradient.cols = c("red","tomato"),
#                 labelsize = labelsize,
#                 pointsize = pointsize,
#                  arrows = arrows,
#                 # map ="rowprincipal",
#                 map ="symbiplot",
#                  title = paste0(typekey," onset + coda (총설명력: ",explain,"%)"))+
#                theme_bw() + theme(legend.position = "none")
#
#   res3 = res0 %>% factoextra::fviz_screeplot(addlabels = TRUE)
#
#   res = list(res1, res3,  res2)
#   res
# }

ca_analysis= function(data, typekey, selcol, arrows =c(F,F), title=""){

  df = data %>% filter(지역 =="부산" & type == typekey) %>%
  xtabs(formula = formula( paste("~ 성조 + a1 +", selcol) ))%>% ca::mjca()

 explain =   round((df$inertia.e[1] +   df$inertia.e[2])*100 , 2)

  g = df %>% plot(arrows =arrows,
                  main= paste(title, typekey,"(",explain , "%)"))

 res =  list(df, g)
 res
}

kge_bind2a %>% ca_analysis("고유어2", "W2")

kge_bind2a %>% ca_analysis("고유어3", "W3")
kge_bind2a %>% ca_analysis("고유어3", "weigth_comb")
kge_bind2a %>% ca_analysis("외래어3", "W3")


formula( paste("~ 성조 + a1 +", "고유어2") )

bbb = kge_bind2a %>% filter(지역 =="부산" & type == "고유어3") %>%
  xtabs(formula = ~ 성조+ a1 + W3) %>% ca::mjca()
bbb
bbb %>% str()
round(bbb$inertia.e[1],4) *100
round(bbb$inertia.e[2],4) *100

?ca::mjca
kge_bind2a %>% filter(지역 =="부산" & type == "고유어3") %>%
  xtabs(formula = ~ 성조+ a1 + W3) %>% ca::mjca() %>% plot()



kge_bind2a %>% ca_analysis("고유어3", "W3")
kge_bind2a %>% ca_analysis("고유어4", "W3")
kge_bind2a %>% ca_analysis("고유어4", "weigth_comb4")


# *>대응분석 결과보기 --------------------------------------------------------------
kge_bind2a %>% data.frame()
kge_bind2a %>% str()
# kge_bind2a %>% colnames()

kge_bind2a %>% ca_analysis("고유어1")
kge_bind2a %>% ca_analysis("고유어2")
kge_bind2a %>% ca_analysis("고유어3")
kge_bind2a %>% ca_analysis("외래어2")
kge_bind2a %>% ca_analysis("외래어3")
kge_bind2a %>% ca_analysis("외래어4")
kge_bind2a %>% ca_analysis("가상어2")
kge_bind2a %>% ca_analysis("가상어3")

kge_bind2a %>% ca_analysis("고유어1", arrows = c(F, F))
kge_bind2a %>% ca_analysis("고유어2", arrows = c(F, F))
kge_bind2a %>% ca_analysis("고유어3", arrows = c(F, F))
kge_bind2a %>% ca_analysis("외래어2", arrows = c(F, F))
kge_bind2a %>% ca_analysis("외래어3", arrows = c(F, F))
kge_bind2a %>% ca_analysis("외래어4", arrows = c(F, F))
kge_bind2a %>% ca_analysis("가상어2", arrows = c(F, F))
kge_bind2a %>% ca_analysis("가상어3", arrows = c(F, F))



# kge_bind2a %>% data.frame()
kge_bind2a %>% str()

kge_bind2a$weight %>% unique()
kge_bind2a$Wz %>% unique()
kge_bind2a$W3 %>% unique()
kge_bind2a$a1 %>% unique()
kge_bind2a$a3 %>% unique()
kge_bind2a$w1f %>% unique()
kge_bind2a$w3f %>% unique()

kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
  select(성조,a1, a3, weight) %>% unite(onset_coda, a1:a3, sep = " / ") %>%
  lm(formula = weight ~ onset_coda) %>% broom::tidy() %>%
  jjstat::p_mark_sig()



kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
  select(성조,a1, a3, weight) %>%
  lm(formula = weight ~ a1) %>% broom::tidy() %>%
  jjstat::p_mark_sig()

kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
  select(성조,a1, a3, weight) %>%
  lm(formula = weight ~ a1 +a3) %>% broom::tidy() %>%
  jjstat::p_mark_sig()

kge_bind2a %>% filter(지역 =="부산" & type == "고유어3") %>%
  select(성조,a1, a3, weight) %>%
  lm(formula = weight ~ a1 +a3) %>% broom::tidy() %>%
  jjstat::p_mark_sig()

#
# bb= lmerTest::lmer(weight ~ a1 + a3  +(1|a3),
#            data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어3") %>%select(성조,a1, a3, weight) ) %>% summary()
# bb$coefficients %>% colnames()
# bb$coefficients %>% data.frame()%>% rownames_to_column("vars")
# %>% lme_report()

#
# lme4::lmer(mpg ~ hp +(1|am) , data = mtcars)


##
lmerTest::lmer(weight ~ a1 + a3  +(1|a1),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme()

lmerTest::lmer(weight ~ a1 + a3  +(1|a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme()

lmerTest::lmer(weight ~ a1 + a3 +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme()


lmerTest::lmer(weight ~ a1 + a3 + (1|a1)  +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme()



lmerTest::lmer(weight ~ a1 + a3 + (1|a3) +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme()



lmerTest::lmer(weight ~ a1 + a3 + (1|a1) + (1|a3) +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme()


#주요 모델 선정 -----
# onset과 coda는 무작휘 효과로 취급하였다. onset과 coda의 상호작용도 무작위 효과로 취급하였다.
#<<모델 비교 함수 ------
bind_lme_model(
  # 모형1
lmerTest::lmer(weight ~ a1# + a3
               + (1|a1),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                select(성조,a1, a3, weight) ) ,
# 모형2
# lmerTest::lmer(weight ~ a1 + a3  + (1|a3),
#                data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
#                 select(성조,a1, a3, weight) ) ,
# 모형3*
lmerTest::lmer(weight ~ a1 + a3  + (1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                 select(성조,a1, a3, weight) ),
# 모형4
lmerTest::lmer(weight ~ a1 + a3 + (1|a1)  +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                 select(성조,a1, a3, weight) ),
# 모형5*
lmerTest::lmer(weight ~ a1 + a3 + (1|a3) +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                 select(성조,a1, a3, weight) ),
# 모형6
lmerTest::lmer(weight ~ a1 + a3 + (1|a1) + (1|a3) +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                 select(성조,a1, a3, weight) )
   )







# 다층모형 비교 결과 --------------------------------------------------------------


model_comparions = function(data, typekey, title="모형 비교"){

  dataset = data %>% filter(지역 =="부산" & type == typekey) %>%
    select(성조,a1, a3, weight) %>%
    dplyr::rename(onset = a1, coda = a3) %>%
    dplyr::select(성조, onset, coda, weight)

  bind_lme_model(title = paste0(typekey, "에 관한 ", title),
  # 모형1
  lmerTest::lmer(weight ~ onset + coda  + (1|onset),
                 data =  dataset) ,
  # 모형2
  lmerTest::lmer(weight ~ onset + coda  + (1|coda),
                 data = dataset) ,
  # 모형3*
  lmerTest::lmer(weight ~ onset + coda   + (1|onset:coda),
                 data = dataset ),
  # 모형4
  lmerTest::lmer(weight ~ onset + coda  + (1|onset)  +(1|onset:coda),
  # lmerTest::lmer(weight ~ a1 + a3 + (a1|a1:a3),
                 data = dataset),
  # 모형5*
  lmerTest::lmer(weight ~ onset + coda  + (1|coda) +(1|onset:coda),
  # lmerTest::lmer(weight ~ a1 + a3 + (a3|a1:a3),
                 data = dataset ),
  # 모형6
  lmerTest::lmer(weight ~ onset + coda + (1|onset) + (1|coda) +(1|onset:coda),
                 data = dataset )
  )
}


kge_bind2a %>% model_comparions("고유어2") #3
kge_bind2a %>% model_comparions("고유어3") #3
kge_bind2a %>% model_comparions("외래어2") #3
kge_bind2a %>% model_comparions("외래어3") #4
kge_bind2a %>% model_comparions("외래어4")
kge_bind2a %>% model_comparions("가상어2")
kge_bind2a %>% model_comparions("가상어3")


# kge_bind2a %>% model_comparions("고유어2")

#고유어1
bind_lme_model(

  lmerTest::lmer(weight ~ 1 + (1|a1),
                 data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                   select(성조,a1, a3, weight) ) ,

  lmerTest::lmer(weight ~ a1 + (1|a1),
                 data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                   select(성조,a1, a3, weight) )
)

# kge_bind2a %>% model_comparions("고유어1") #고유어1은 a3이 없음.

lmerTest::lmer(weight ~ 1 + (1|a1),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어1") %>%
                 select(성조,a1, a3, weight) )


kge_bind2a %>% model_comparions("고유어2")
#모형5
lmerTest::lmer(weight ~ a1 + a3 + (1|a3) +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme(apa=T,type="APA")# %>%
#  markdown_table()
# 모형3
lmerTest::lmer(weight ~ a1 + a3  +(1|a1:a3),
               data = kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
                 select(성조,a1, a3, weight) ) %>% report_lme()


kge_bind2a %>% model_comparions("고유어2") #3
kge_bind2a %>% model_comparions("고유어3") #3
kge_bind2a %>% model_comparions("외래어2") #3
kge_bind2a %>% model_comparions("외래어3") #4
kge_bind2a %>% model_comparions("외래어4")
kge_bind2a %>% model_comparions("가상어2")
kge_bind2a %>% model_comparions("가상어3")



# > 전체 다층모형 결과 -------------

# > 전체 다층모형 결과 -------------
model_summary7 = function(data){

  bind_lme_model(title = "전체 언어유형에 따른 onset : coda 다층모형 분석 ",
                 model_name = c(
                   "M1:고유어2",
                   "M2:고유어3",
                   "M3:외래어2",
                   "M4:외래어3",
                   "M5:외래어4",
                   "M6:가상어2",
                   "M7:가상어3" ),

                 lmerTest::lmer(weight ~ a1 + a3 + (1|a1:a3),
                                data = data %>%
                                  filter(지역 =="부산" & type == "고유어2") %>%
                                  select(성조, a1 ,a3, weight)),

                 lmerTest::lmer(weight ~ a1 + a3 + (1|a1:a3),
                                data = data %>%
                                  filter(지역 =="부산" & type == "고유어3") %>%
                                  select(성조, a1 ,a3, weight)),

                 lmerTest::lmer(weight ~ a1 + a3 + (1|a1:a3),
                                data = data %>%
                                  filter(지역 =="부산" & type == "외래어2") %>%
                                  select(성조, a1 ,a3, weight)),

                 lmerTest::lmer(weight ~ a1 + a3 + (1|a1:a3),
                                data = data %>%
                                  filter(지역 =="부산" & type == "외래어3") %>%
                                  select(성조, a1 ,a3, weight)),

                 lmerTest::lmer(weight ~ a1 + a3 + (1|a1:a3),
                                data = data %>%
                                  filter(지역 =="부산" & type == "외래어4") %>%
                                  select(성조, a1 ,a3, weight)),

                 lmerTest::lmer(weight ~ a1 + a3 + (1|a1:a3),
                                data = data %>%
                                  filter(지역 =="부산" & type == "가상어2") %>%
                                  select(성조, a1 ,a3, weight)),

                 lmerTest::lmer(weight ~ a1 + a3 + (1|a1:a3),
                                data = data %>%
                                  filter(지역 =="부산" & type == "가상어3") %>%
                                  select(성조, a1 ,a3, weight))
  )
}

#모델결과 데이터로보기
kge_bind2a %>% model_summary7()


#모델 결과 정리
model_summary8 = function(data){

  bind_lme_model(title = "전체 언어유형에 따른 onset : coda 다층모형 분석 ",
                 model_name = c(
                   "M1:고유어2",
                   "M2:고유어3",
                   "M3:외래어2",
                   "M4:외래어3",
                   "M5:외래어4",
                   "M6:가상어2",
                   "M7:가상어3" ),

    merTest::lmer(weight ~ onset + coda + (1|onset:coda),
                                data = data %>%
                                  filter(지역 =="부산" & type == "고유어2") %>%
                                  rename(onset=a1, coda=a3) %>%
                                  select(성조,onset, coda, weight)),

     lmerTest::lmer(weight ~ onset + coda + (1|onset:coda),
                                data = data %>%
                                  filter(지역 =="부산" & type == "고유어3") %>%
                                  rename(onset=a1, coda=a3) %>%
                                  select(성조,onset, coda, weight)),

    lmerTest::lmer(weight ~ onset + coda + (1|onset:coda),
                                data = data %>%
                                  filter(지역 =="부산" & type == "외래어2") %>%
                                  rename(onset=a1, coda=a3) %>%
                                  select(성조,onset, coda, weight)),

      lmerTest::lmer(weight ~ onset + coda + (1|onset:coda),
                                data = data %>%
                                  filter(지역 =="부산" & type == "외래어3") %>%
                                  rename(onset=a1, coda=a3) %>%
                                  select(성조,onset, coda, weight)),

      lmerTest::lmer(weight ~ onset + coda + (1|onset:coda),
                                data = data %>%
                                  filter(지역 =="부산" & type == "외래어4") %>%
                                  rename(onset=a1, coda=a3) %>%
                                  select(성조,onset, coda, weight)),

      lmerTest::lmer(weight ~ onset + coda + (1|onset:coda),
                                data = data %>%
                                  filter(지역 =="부산" & type == "가상어2") %>%
                                  rename(onset=a1, coda=a3) %>%
                                  select(성조,onset, coda, weight)),

      lmerTest::lmer(weight ~ onset + coda + (1|onset:coda),
                                data = data %>%
                                  filter(지역 =="부산" & type == "가상어3") %>%
                                  rename(onset=a1, coda=a3) %>%
                                  select(성조,onset, coda, weight))
  )
}


#모뎔결과 전체 보기
kge_bind2a %>% model_summary8()





#                  # 모형5*
# lmerTest::lmer(weight ~ a1 + a3 + (1|a3) +(1|a1:a3),
#                 data = data %>% filter(지역 =="부산" & type == typekey[5]) %>%
#                select(성조,a1, a3, weight) )






# report_lme(apa = T)
