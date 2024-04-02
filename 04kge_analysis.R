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
load(file ="kge_bind3a.RData")


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
Kge_word %>% select(word, type) #%>% print(n=Inf)
Kge_word$word   #%>% print(n=Inf)

Kge_word %>% filter(분석1 == 1) %>% nrow()



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

kge_bind1a %>% filter(성조 != "") %>% nrow()  # 5643
kge_bind1a %>% filter(성조 != ""  & 분석1 == "1") %>% nrow() #5422

# @ kge_bind2a 성조가 NA인 것을 제거하고 다시 생성-------
kge_bind2a <- kge_bind1a %>% filter(성조 != "")

kge_bind3a <- kge_bind1a %>% filter(성조 != ""  & 분석1 == "1")  #5422
# 5643
kge_bind2a%>% nrow()
kge_bind3a%>% nrow()

# save(kge_bind2a, file ="kge_bind2a.RData")
save(kge_bind3a, file ="kge_bind3a.RData")
# write_excel_csv(kge_bind2a, file ="kge_bind2a.csv")
load(file ="kge_bind2a.RData")
load(file ="kge_bind3a.RData")


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


accent_pattern2 = rbind(
  고유어11  = c(pattern = "H(H)_1", imp="H(H)+H(H)_1"),
  고유어12  = c(pattern = "H(H)", imp="H(H)+H(H)_1"),
  고유어21  = c(pattern = "LH(H)", imp ="LH+LH(H)"),
  고유어22 = c(pattern = "LH", imp ="LH+LH(H)"),
  고유어31  = c(pattern = "LHH", imp ="LLH+LHH"),
  고유어32  = c(pattern = "LLH", imp ="LLH+LHH"),
  외래어21  = c(pattern = "LH_f", imp ="LH+LH_f"),
  외래어22  = c(pattern = "LH", imp ="LH+LH_f"),
  외래어31  = c(pattern = "LLH_f", imp ="LLH+LLH_f"),
  외래어32  = c(pattern = "LLH", imp ="LLH+LLH_f"),
  외래어41  = c(pattern = "LLLH_f", imp ="LLLH+LLLH_f"),
  외래어42  = c(pattern = "LLLH", imp ="LLLH+LLLH_f"),
  가상어2  = c(pattern = "", imp =""),
  가상어3  = c(pattern = "", imp ="")
) %>% data.frame() %>% rownames_to_column("word") %>% tibble()

accent_pattern2 %>% markdown_table("전체 성조분류 패턴 결정")

#패턴 적용

# kge_bind2a %>% filter(type =="고유어1") %>% auto_pattern()
kge_bind2a %>% filter(type =="고유어1") %>% auto_pattern("고유어1")
kge_bind2a %>% filter(type =="고유어2") %>% auto_pattern("고유어1")
kge_bind2a %>% filter(type =="고유어2") %>% auto_pattern("고유어1")


#f분석자료를 데이터 프레임으로-----
accent_table = function(data, #table data
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



  # 데이터를 데이터 프레임으로 반들기
  res_df = data  %>% as.matrix() %>%data.frame()


  #비율을 생성하여 행렬화
  res_ratio = add_ratio(res)

  #퍼센트 붙이기
  res_df_RES =  combine_data(res, add_ratio(res_ratio),"(", "%)")

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

  # res_all = list(res, res_df_RES, res_mosaicplot)

  if(trans){res= res %>% t()

  }else{res}


  switch(type,
         res = res,
         res_df = res_df,
         ratio = res_df_RES,
         g = res_mosaicplot,
         all = res_all )
}

# 분석 테스트

##* 전체 contigency table----

kge_bind3a %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t() %>%
  markdown_table("스피커별 전체 contigency table")


#*성조패턴 변경 데이터 : 전체에서 패턴 변경은 의미가 없음
kge_bind3a %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t() %>%
  markdown_table("스피커별 전체 contigency table")


##성조패턴 변화전 데이터
speaker_acc1 = rbind(
#* 고유어1 contigency table 4
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
,



#* 고유어2 contigency table
kge_bind3a %>% filter(type=="고유어2") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
,


#* 고유어3 contigency table
kge_bind2a %>% filter(type=="고유어3") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
,


#* 외래어2 contigency table
kge_bind3a %>% filter(type=="외래어2") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
,

#* 외래어3 contigency table
kge_bind3a %>% filter(type=="외래어3") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

,

#* 외래어4 contigency table
kge_bind3a %>% filter(type=="외래어4") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

,

#* 가상어2 contigency table
kge_bind3a %>% filter(type=="가상어2") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

,

#* 가상어3 contigency table
kge_bind3a %>% filter(type=="가상어3") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
)

roww.name.speaker = rownames(speaker_acc1)

speaker_acc1 = speaker_acc1 %>% data.frame()%>%
          mutate(accent= roww.name.speaker) %>%
  select(6,1:5)

speaker_acc1
type_label_1 = c(
  rep("고유어1", 4),
  rep("고유어2", 4),
  rep("고유어3", 4),
  rep("외래어2", 4),
  rep("외래어3", 5),
  rep("외래어4", 6),
  rep("가상어2", 3),
  rep("가상어3", 4)
)
type_label_1

#
speaker_accent_total_data =cbind.data.frame(type_label_1, speaker_acc1) %>% tibble()
speaker_accent_total_data %>% dall()
speaker_accent_total_data %>% markdown_table()


## 패턴변경후 스피커별 데이터  ---------------------------------------------------------



speaker_acc2 = rbind(
  #* 고유어1 contigency table 3
  kge_bind3a %>% filter(type=="고유어1") %>%
    auto_pattern2("고유어1a") %>%
    auto_pattern2("고유어1b") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
  ,



  #* 고유어2 contigency table 3
  kge_bind3a %>% filter(type=="고유어2") %>%
    auto_pattern2("고유어2a") %>%
    auto_pattern2("고유어2b") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
  ,


  #* 고유어3 contigency table 4
  kge_bind3a %>% filter(type=="고유어3") %>%
    auto_pattern2("고유어3a") %>%
    auto_pattern2("고유어3b") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

  ,


  #* 외래어2 contigency table 3
  kge_bind3a %>% filter(type=="외래어2") %>%
    auto_pattern2("외래어2a") %>%
    auto_pattern2("외래어2b") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
  ,

  #* 외래어3 contigency table 4
  kge_bind3a %>% filter(type=="외래어3") %>%
    auto_pattern2("외래어3a") %>%
    auto_pattern2("외래어3b") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

  ,

  #* 외래어4 contigency table 5
  kge_bind3a %>% filter(type=="외래어4") %>%
    auto_pattern2("외래어4a") %>%
    auto_pattern2("외래어4b") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

  ,

  #* 가상어2 contigency table 3
  kge_bind3a %>% filter(type=="가상어2") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

  ,

  #* 가상어3 contigency table 4
  kge_bind3a %>% filter(type=="가상어3") %>%
    select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = "성조", values_from = "Freq") %>%
    rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()
)

roww.name.speaker2 = rownames(speaker_acc2)

speaker_acc2 = speaker_acc2 %>% data.frame()%>%
  mutate(accent= roww.name.speaker2) %>%
  select(6,1:5)

speaker_acc2
type_label_2 = c(
  rep("고유어1", 3),
  rep("고유어2", 3),
  rep("고유어3", 4),
  rep("외래어2", 3),
  rep("외래어3", 4),
  rep("외래어4", 5),
  rep("가상어2", 3),
  rep("가상어3", 4))
type_label_2

#
speaker_accent_total_data2 =cbind.data.frame(type_label_2, speaker_acc2) %>% tibble()
speaker_accent_total_data2 %>% dall()
speaker_accent_total_data2 %>% markdown_table()



## *고유어1에 대한 스피커별 빈도수 ------------------------------------------------------


#contigency table
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t() %>%
  addmargins() %>%
  markdown_table()


kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>%
  rbind(SUM=apply(., MARGIN=2, FUN=sum)) %>%
  cbind(SUM=apply(., MARGIN=1, FUN=sum))




kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()%>%
  addmargins() %>%
  markdown_table()


kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", color = 2:5,type="ratio") %>% t()%>%
  markdown_table()

kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", color = 2:5)

#check 분석체크용 데이터 ---------------------------------------------------------------

kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>% t()

#촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>%
  obs_exp_table()

#관측/기대비율 분석
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>%
  calculate_chi_sig()


#관측기대 통계적 유의성
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>%
  p_value_cal()

# 관측기대 유의성 표시
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>%
  p_sig_cal()

# 관측기대 유의성 표시
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, speaker) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "speaker") %>% tibble::column_to_rownames("accent") %>%
  p_sig_cal() %>% long_df(values_to="star")


kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1")







#*>결과1 accent_table관측치에 대한 발화자별 패턴 분석 ---------
# 패턴 변경이 없는 경우

##* 성조패턴 변화후에 화자별 분포 -----------

## 함수적용하여 변경
kge_bind3a %>% filter(N==1) %>%
  auto_pattern("고유어1")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", "고유어1", plot = T)


#*
kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a")%>%
  auto_pattern2("고유어1b")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조") %>%
  # add_ratio_df() %>%
  add_sum(add_ratio = T)

#성조패턴 변화후에 화자별 분포 보고서에 투입
kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a")%>%
  auto_pattern2("고유어1b")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조") %>%
  # add_ratio_df() %>%
  add_sum(add_ratio = T, plot = T, color = TRUE)%>%
  markdown_table()

#*각자의 그래프 - 각자 분리
kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a")%>%
  auto_pattern2("고유어1b")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조") %>%
  patternGraph(tolong = T, type= "all", show=F)


#고유어1의 빈도 참고용 transpose
kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a")%>%
  auto_pattern2("고유어1b")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", color = 2:5, type="ratio",plot = T)


# #고유어1
# kge_bind3a %>% filter(type=="고유어1") %>%
#   auto_pattern2("고유어1a")%>%
#   auto_pattern2("고유어1b")%>%
#   select(성조, speaker) %>% table() %>%
#   accent_table("speaker", "성조", "고유어1")
#
# kge_bind3a %>% filter(type=="고유어1") %>%
#   auto_pattern2("고유어1a")%>%
#   auto_pattern2("고유어1b")%>%
#   select(성조, speaker) %>% table() %>%
#   accent_table("speaker", "성조", "고유어1") %>%
#   patternGraph(tolong = T, type= "all", show=FALSE)


#고유어2는 필요하지 않음.
kge_bind3a %>% filter(type=="고유어2") %>%
  auto_pattern("고유어2")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조", "고유어2") %>%
  add_sum()








##*1> 패천적용 accent_table 만들기 -----
# observed table result -
obs_table = function(data, typesel = NULL, cex=1.3, plot=TRUE, type="res"){

  res= data %>% filter(type==typesel) %>%
    auto_pattern(typesel)%>%
    select(성조, speaker) %>% table() %>%
    accent_table("speaker", "성조", typesel, cex=cex,  plot = plot, type = type)

  res
}

kge_bind2a %>% obs_table("고유어1")
kge_bind2a %>% obs_table("고유어2")
kge_bind2a %>% obs_table("고유어3", cex=1)
kge_bind2a %>% obs_table("외래어2")
kge_bind2a %>% obs_table("외래어3")
kge_bind2a %>% obs_table("외래어4", cex =1)
kge_bind2a %>% obs_table("가상어2")
kge_bind2a %>% obs_table("가상어3")


#longdata transfomation
long_df = function(data,
                   names_to = "speaker",
                   values_to = "freq",
                   cols = 2:ncol(data1),
                   rowname ="accent"){

  colName = colnames(data)
  rowName = rownames(data) #accent
  # colnames0 = colnames(data)
  data1 = data %>% data.frame() %>%
    rownames_to_column(rowname)

  colnames(data1)= c(rowname,colName)

  data2 <- data1%>%
    pivot_longer(names_to = names_to,
                 values_to = values_to,
                 cols=cols)
  data2

}

#피실험자
Kge_person %>% markdown_table()

##> 피실험자 정보 결합 -----
kge_bind2a %>% obs_table_ratio("고유어1") %>%
  long_df(rowname="speaker", names_to="accent") %>%
  merge(Kge_person[,-6], by="speaker" )


kge_bind2a %>% obs_table_ratio("고유어1") %>%
  long_df(rowname="speaker", names_to="accent") %>%
  merge(Kge_person[,-6], by="speaker" ) %>%
  markdown_table()


#같은 자료
kge_bind2a %>% obs_table_ratio("고유어1") %>%
  long_df(rowname="speaker", names_to="accent") %>%
  inner_join(Kge_person, by="speaker" ) %>%
  tidyr::unite(speaker , c(speaker, age, area)) %>% arrange(speaker)



# 고유어2 필요하지 않음.
kge_bind2a %>% obs_table_ratio("고유어2") %>%
  long_df(rowname="speaker", names_to="accent") %>%
  merge(Kge_person, by="speaker" )



# 피험자 정보 데이터와결합
combind_person = function(data){
  load(file =" Kge_person.RData")

  data1=  data %>%long_df() %>%
    inner_join(Kge_person, by="speaker" ) %>%
    tidyr::unite(speaker , c(speaker, age, area))%>%
    arrange(speaker) %>%
    dplyr::select( name, gender,speaker,accent, freq)

  data1

}


#피험자 정보 데이터와결합
combind_person2 = function(data,rowname="speaker", names_to="accent" ){
  load(file =" Kge_person.RData")

  data1=  data %>%
    long_df(rowname=rowname, names_to=names_to) %>%
    inner_join(Kge_person, by="speaker" ) %>%
    tidyr::unite(speaker , c(speaker, age, area))%>%
    arrange(speaker) %>%
    dplyr::select( name, gender,speaker,accent, freq)

  data1

}





kge_bind3a %>% filter(type=="고유어1") %>%
  auto_pattern2("고유어1a")%>%
  auto_pattern2("고유어1b")%>%
  select(성조, speaker) %>% table() %>%
  accent_table("speaker", "성조") %>%
  patternGraph(tolong = T, type= "all", show=F)


#데이터와 사용자 정보를 결합하는 함수 코드
kge_bind3a %>% obs_table("고유어1") %>% combind_person()
kge_bind3a %>% obs_table("고유어2") %>% combind_person()
kge_bind3a %>% obs_table("고유어3", cex=1) %>% combind_person()
kge_bind3a %>% obs_table("외래어2") %>% combind_person()
kge_bind3a %>% obs_table("외래어3") %>% combind_person()
kge_bind3a %>% obs_table("외래어4", cex =1) %>% combind_person()
kge_bind3a %>% obs_table("가상어2") %>% combind_person()
kge_bind3a %>% obs_table("가상어3") %>% combind_person()

### s 피실험자 정보결합+발화 패턴을 모자이크 플롯을 이용하여 확인-----
kge_bind3a %>% obs_table_ratio("고유어1") %>% combind_person2()
kge_bind3a %>% obs_table_ratio("고유어2") %>% combind_person2()
kge_bind3a %>% obs_table_ratio("고유어3", cex=1) %>% combind_person2()
kge_bind3a %>% obs_table_ratio("외래어2") %>% combind_person2()
kge_bind3a %>% obs_table_ratio("외래어3") %>% combind_person2()
kge_bind3a %>% obs_table_ratio("외래어4", cex =1) %>% combind_person2()
kge_bind3a %>% obs_table_ratio("가상어2") %>% combind_person2()
kge_bind3a %>% obs_table_ratio("가상어3") %>% combind_person2()


## >s 발화 패턴 체크 : 개별로 분리 -----
kge_bind2a %>% obs_table("고유어1") %>%
  combind_person()%>%
  patternGraph(tolong=FALSE, show=F)

kge_bind2a %>% obs_table("고유어2") %>%
  combind_person()%>%  patternGraph(tolong=FALSE)

kge_bind2a %>% obs_table("고유어3", cex=1) %>%
  combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("외래어2") %>%
  combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("외래어3") %>%
  combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("외래어4", cex =1) %>%
  combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("가상어2") %>%
  combind_person()%>%  patternGraph(tolong=FALSE)
kge_bind2a %>% obs_table("가상어3") %>%
  combind_person()%>%  patternGraph(tolong=FALSE)




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
kge_bind2a%>% Agree_table("고유어1", 60, 60)
kge_bind2a%>% Agree_table("고유어1", 20, 20)
kge_bind2a%>% Agree_table("고유어2", 60, 20)
kge_bind2a%>% Agree_table("고유어3", 60, 20)
kge_bind2a%>% Agree_table("가상어2", 60, 60)
kge_bind2a%>% Agree_table("가상어2", 20, 20)




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







#** kge_chisq_table chisq table observed/Expected table--------
kge_chisq_table = function(dataset,
                           v1="a1",
                           v2="성조",
                           title ="Table",
                           type = "res2",
                           digits = 3,
                           yadd=0.3,
                           Ncol=NULL,
                           trans = FALSE,
                           simple= FALSE, #유의성 종류
                           ko = FALSE,
                           simulate.p.value=FALSE,
                           correct= FALSE,
                           cramer="adjust"
)  #패턴그래프
{

  data =  dataset %>%
    dplyr::select(all_of(v1), all_of(v2)) %>%
    table()

  # 최종결과 에 포함을 margn sum
  data_margin0 = data %>% addmargins()

  data_rowsum0 = data %>%  apply(., MARGIN = 2 , FUN = sum)
  data_rowsum_df = data %>%  rbind(SUM=apply(., MARGIN = 2 , FUN = sum) )
  data_colsum = data_rowsum_df %>% apply(., MARGIN = 1 , FUN = sum)

  #데이터 게산
  data_margin = data  %>%
    accent_table( v1, v2, trans = trans, type = "ratio")

  #비율계산과 데이터 margin sum
  data_margin = cbind(
    rbind(data_margin, SUM=data_rowsum0 ),
    SUM=data_colsum )
  #크래머 상관
  cramer_cor = cramers_v(data, type = cramer)
  cramer_cor_v = cramers_v(data, type = "cramer")

  # data_margin = cbind(data_margin, data_margin0[, ncol(data_margin0)])

  #chisq.test
  Onset_or_Coda_Accent_Contingency_table <- data
  #카이제곱 데이터프리엠 변형
  res_df = chisq.test(data)%>% broom::tidy()

  #카이제곱 테스트
  res = chisq.test(Onset_or_Coda_Accent_Contingency_table,
                   correct = correct,
                   simulate.p.value = simulate.p.value)
  # 판단용 통계치
  chi2 = res$statistic
  p_vlaue_chi = res$p.value
  # df_chi = res$parameter

  msg_sig_chi = ifelse(p_vlaue_chi < 0.05,"significant", "not significant")
  # msg_p_chi = ifelse( p_vlaue_chi < 0.01, "< .001",
  #                     paste0("= ",
  #                            format(p_vlaue_chi, digits, scientific=TRUE) ))
  # res_report = chisq.test(data)%>% report::report()


  chi_mag = paste0(" [chisq = ",round(res$statistic, digits),
                   ", df = ",res$parameter,
                   ", p = ", format_number(res$p.value, digits),"]" )
  # res$statistic
  # res$parameter
  # res$p.value


  res_report = paste0("The Pearson's Chi-squared test of independence between ",
                      v1," and ",v2,
                      " suggests that the effect is statistically ", msg_sig_chi,
                      chi_mag,
                      ".; ",
                      cramer_cor_v, ".")



  if(nrow(data) != 1){
    chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
      tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
      tibble::column_to_rownames(v1) %>%
      Round(digits)
  }else{
    chi_table =
      rbind(
        observed = data %>%
          accent_table( v1, v2, trans = trans),
        expected = res$expected,
        obs_expected_ratio = (res$observed / res$expected)
      ) %>% Round(digits)

    # g= NULL
  }
  #observer/Expected 에 유의성 표시
  chi_table_sig = format(calculate_chi_sig(data, simple = simple), 3)
  chi_table_sig2 = perform_chi_square_test(data,
                                           simple = simple,
                                           type = "data2")

  #유의성표시된 것으로 변경
  chi_table_md = chi_table_sig %>%
    markdown_table(caption = paste0(title, chi_mag,"; ", cramer_cor_v,"."),
                   digits = digits,
                   general = NULL)


  # 결과를 정리하여 나타내는 값들
  result = list(chisq_test = res,
                margin = data_margin %>%
                  markdown_table(caption = paste0(title,"Contingency table"),
                                 general = NULL),
                chi_table_md,
                chi_table = chi_table)


  #패턴 그래프 ko적용--raq =TRUE, type="g"
  data_graph = data  %>% accent_table( v2, v1, type = "res")


  if(ko){
    graph = patternGraph_obs_exp_ko(data_graph,
                                    raw = TRUE,
                                    yadd = yadd,
                                    Ncol = Ncol)
  }else{
    graph = patternGraph_obs_exp(data_graph,
                                 raw = TRUE,
                                 yadd = yadd,
                                 Ncol = Ncol)
  }


  #최종울력
  result1 = list(
    # data_rowsum0,data_colsum,
    # msg=msg,
    contigency_table_margin = data_margin0,
    contigency_table = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test_overall = res,

    # chi_df = res_df,
    chisq_report_overall = res_report,
    CRAMER_V.adusted = cramer_cor,
    CRAMER_V = cramer_cor_v,

    chi_table = chi_table ,
    chi_table_sig_each_variable = chi_table_sig,
    chi_table_sig_each_variable2 = chi_table_sig2,
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
         chi_table = chi_table,
         res1 = result,
         res2 = result1,
         res3 = result2)
}


kge_bind3a %>% filter(지역 =="부산" & type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 초성 ")

#
# kge_bind2 %>% filter(지역 =="부산" & type=="고유어1") %>%
#   auto_pattern("고유어1") %>%
#   kge_chisq_table("a1","성조", "고유어1 부산")


# kge_bind2a %>% filter(지역 =="부산" & N ==1) %>%
#   auto_pattern("고유어1") %>% select(w1f,성조) %>% table() %>%
#   accent_table("성조", "w1f") %>%
#   patternGraph2( raw = F, yadd= 20)


#contigency table 1
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, a1) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "a1") %>% tibble::column_to_rownames("accent")


#촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, a1) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "a1") %>% tibble::column_to_rownames("accent") %>%
  obs_exp_table()

kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, a1) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "a1") %>% tibble::column_to_rownames("accent") %>%
  obs_exp_table()%>%
  patternGraph_obs_exp()


#관측기대 통계적 유의성
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, a1) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "a1") %>% tibble::column_to_rownames("accent") %>%
  p_value_cal()

# 관측기대 유의성 표시
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, a1) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "a1") %>% tibble::column_to_rownames("accent") %>%
  p_sig_cal()


# 관측기대 유의성 표시
kge_bind3a %>% filter(type=="고유어1") %>%
  select(성조, a1) %>% table() %>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = "성조", values_from = "Freq") %>%
  rename(accent = "a1") %>% tibble::column_to_rownames("accent") %>%
  p_sig_cal() %>% long_df(values_to="star")


#contigency table 2---
kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1")

kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("a1","성조")


# 촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1")%>%
  obs_exp_table()

# 촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1")%>%
  p_value_cal()

kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1")%>%
  p_value_cal()%>% long_df(values_to="pvalue")


# 촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1")%>%
  p_sig_cal()

# 촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1")%>%
  p_sig_cal()%>% long_df(values_to="star")


# 촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1") %>%
  # obs_exp_table() %>%
  patternGraph_obs_exp_ko(raw=T, type = "res")

kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1") %>%
  # obs_exp_table() %>%
  patternGraph_obs_exp_ko(raw=T, type = "g")

# 촥측 기대 테이블 생성
kge_bind3a %>% filter(type=="고유어1") %>%
  select( 성조,a1)%>% table() %>%
  accent_table("성조","a1") %>%
  obs_exp_table() %>%
  patternGraph_obs_exp_ko(raw=F, type = "res")






#*> 고유어1 부산  --------------------------------------------------------------

## >>고유어1 초성 분석 진행 변경  ----
kge_bind3a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 onset")

#고유어1 초성 그래프 전환해서 보기
kge_bind3a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 onset") %>%
  patternGraph2()


## 고유어1 pattern 그래프만 보기
kge_bind3a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 onset") %>%
  patternGraph3()







#참고용 - 다른 방법
kge_bind3a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 onset", type = "data_graph") %>%
  patternGraph3(data_graph=T)


## 고유어1 pattern 그래프만 보기
kge_bind3a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 onset",type = "chi_table") %>%
  patternGraph_obs_exp_ko()








##>> 고유어1 부산 종성 2개 weight (light, heavy)--------
kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1")%>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a3","성조", "고유어1 부산 첫음절 종성(coda) ")


##>> 고유어1 부산 종성 그래프 전환
kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1")%>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("a3","성조", "고유어1 부산 첫음절 종성(coda) ")%>%
  patternGraph2()



#* 고유어1 부산 종성 2개 weight (light, heavy) 그래프 전환
kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern2("고유어1a") %>%
  auto_pattern2("고유어1b") %>%
  kge_chisq_table("w1f","성조", "고유어1 부산 첫음절 종성 weight ")



kge_bind2a %>% filter(지역 =="부산"  & type=="고유어1") %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("w1f","성조", "고유어1 부산 첫음절 종성 weight ")%>%
  patternGraph2()

#고유어1 CA
kge_bind2a %>% filter(지역 =="부산" & type=="고유어1") %>%
  select(a1, 성조) %>% table() %>%
  accent_table("a1","성조") %>% FactoMineR::CA()











kge_bind4a <- kge_bind3a %>% kge_weigth_add_ca()





kge_bind2a %>% ca_analysis("고유어1")

# 대응분석 시각화 함수 ----------
ca_analysis= function(dataset,
                      typekey,
                      selcol_3="w1f",
                      type="all",
                      arrows =c(F,T),
                      selcol_1="성조",
                      selcol_2="onset",
                      area="부산",
                      addtext="",
                      xlim = NULL, #c(0, 0.3),
                      ylim = NULL, #c(-0.3, 0.4)
                      opt=1

){



  if(selcol_3=="a3"){
    data = dataset %>%
      rename(onset = a1,
             # coda = a3,
             Coda = selcol_3
      )

    selcol_3 ="Coda"

  }else{
    data = dataset %>%
      rename(onset = a1,
             # coda = a3,
             Weight = selcol_3
      )
    selcol_3="Weight"
  }


  #1,2,4와 3,5,6이 같음
  if(opt==1){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_1,"+", selcol_2, "+", selcol_3) ))
  }else if(opt==2){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_1,"+", selcol_3, "+", selcol_2) ))
  }else if(opt==3){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_2,"+", selcol_1, "+", selcol_3) ))

  }else if(opt==4){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_2,"+", selcol_3, "+", selcol_1) ))
  }else if(opt==5){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_3,"+", selcol_1, "+", selcol_2) ))
  }else if(opt==6){
    df0 = data %>% filter(지역 == area & type == typekey) %>%
      xtabs(formula = formula(
        paste("~", selcol_3,"+", selcol_2, "+", selcol_1) ))
  }

  df = df0%>% ca::mjca()
  # df_graph = df$cols
  explain =   round((df$inertia.e[1] +df$inertia.e[2])*100 , 2)

  g = df %>% plot(arrows = arrows, col = 1:ncol(df0),
                  main = paste( typekey,
                 "에 관한 성조형과 음절 다중대응분석의 총 설명력(",
                 explain , "%)"),
                  xlim = xlim,
                  ylim = ylim,
                  cex.lab = 1.2,
                  cex.sub = 1.2,cex=2,
                  pch = 17, bg=1:3)

  # graph_df= g$cols

  graph_df = g$cols %>% data.frame() %>%
    rownames_to_column("var") %>%
    separate(var, c("factor", "level"), remove = FALSE, sep=":") %>%
    select(-1)


  graph_df_md = graph_df %>%
    markdown_table(caption = paste0(typekey,
                                    "(",area,")-",addtext,
                                    "-다중대응분석 좌표점(",explain,"%)" ),
                   digits = 4)

  gplot = g$cols %>% data.frame() %>%
    rownames_to_column("var") %>%
    separate(var, c("factor", "level"), remove = FALSE, sep=":") %>%
    ggplot(aes(x= Dim1, y=Dim2))+
    geom_point(aes(color= factor), size=4, show.legend = FALSE)+
    ggrepel::geom_text_repel(aes(label= var),
                             # geom_text(aes(label= var),
                             size = 5,
                             vjust = -0.7,
                             hjust = -0.1)+
    theme_bw()+
    geom_vline(xintercept = 0, linetype=2)+
    geom_hline(yintercept = 0, linetype=2)+
    labs(
      x = paste0("Dim1(", round(df$inertia.e[1]*100, 2),"%)"),
      y = paste0("Dim2(", round(df$inertia.e[2]*100, 2),"%)"),
      title = paste0("다중 대응분석(multiple Correspondence Analysis): ",
                     selcol_1,", ",
                     selcol_2,", ",
                     selcol_3,"에 대한 ",
                     "총 설명력(",explain,"%)")
    )+
    theme(axis.text = element_text(size=12))



  # res =  list( df, g, df0)
  res =  list(ca_res=df,
              graph_df= g$cols,
              graph_df_md = graph_df_md,
              g=g,
              gplot= gplot)

  switch(type,
         res = res,
         all = res,
         ca_res=df,
         graph_df_md = graph_df_md,
         graph_df= g$cols,
         g=g,
         gplot= gplot
         )
}


kge_bind3a %>% data.frame()
# kge_bind3a %>% colnames()


kge_bind3a %>% ca_analysis("고유어1", "a3")#,3

# kge_bind3a %>% ca_analysis("고유어1", "a3","ca_res")#,3

kge_bind3a %>% ca_analysis("고유어1", "w1f", addtext="Weight")#,3
# kge_bind3a %>% ca_analysis("고유어1", "Weight", opt=1)#,3
# kge_bind3a %>% ca_analysis("고유어1", "w1f", opt=2)#,2
# kge_bind3a %>% ca_analysis("고유어1", "w1f", opt=4)#,
#
# kge_bind3a %>% ca_analysis("고유어1", "w1f", opt=3)#,
# kge_bind3a %>% ca_analysis("고유어1", "w1f", opt=5)#, 1
# kge_bind3a %>% ca_analysis("고유어1", "w1f", opt=6)#, 1

kge_bind3a %>% ca_analysis("고유어1", "a3")

kge_bind3a %>% ca_analysis("고유어2", "weight")#, xlim=c(-0.3, 0.5))

kge_bind3a %>% ca_analysis("외래어2", "weigth_comb2")#, xlim=c(-0.3, 0.5))

kge_bind3a %>% ca_analysis("고유어3", "weigth_comb")#, xlim=c(-0.3, 0.5))
kge_bind3a %>% ca_analysis("외래어3", "weigth_comb")#, xlim=c(-0.3, 0.5))
kge_bind3a %>% ca_analysis("외래어4", "weigth_comb4")#, xlim=c(-0.3, 0.5))
kge_bind3a %>% kge_weigth_add_ca() %>% data.frame()


# kge_bind3a %>% select(type, a1, a3, w1f) %>%
#   rename(onset = "a1", coda = a3)



# *> 고유어2  부산 -----------------------------------------------------------------
kge_bind3a %>% data.frame()

#고유어2 초성
# kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
#   auto_pattern2("고유어2a") %>%
#   auto_pattern2("고유어2b") %>%
#   accent_table("성조","a1", raw = T)

##>> 고유어2 초성 ----
kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("a1","성조", "고유어2 부산 onset")

# #고유어2 초성
# kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
#   auto_pattern2("고유어2a") %>%
#   auto_pattern2("고유어2b") %>%
#   kge_chisq_table("a1","성조", "고유어2 부산 onset")

kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("a1","성조", "고유어2 부산 onset")%>%
  patternGraph2()




#고유어2 첫전째 종성 Heagy-X 음절구조
kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("w1f","성조", "고유어2 부산 첫음절종성 음절구조(weight)")


#* 고유어2 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("w1f","성조", "고유어2 부산 첫음절종성 음절구조(weight) ") %>%
  patternGraph2()



#* 고유어2 마지막 종성 음절구조 X-Heavy
kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("w2f","성조", "고유어2 부산 성조형 마지막 음절구조(weight) ")


kge_bind2a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("w2f","성조", "고유어2 부산 성조형 마지막 음절구조(weight) ") %>%
  patternGraph2()



#* 고유어2 부산 종성 2개 weight (light, heavy)
kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("weigth_comb2","성조", "고유어2 부산 성조형과 음절구조(weight) ")

kge_bind3a %>% filter(지역 =="부산" & type=="고유어2") %>%
  auto_pattern2("고유어2a") %>%
  auto_pattern2("고유어2b") %>%
  kge_chisq_table("weigth_comb2","성조", "고유어2 부산 성조형과 음절구조(weight) ") %>%
  patternGraph2()


#* 고유어2 부산 종성 2개 weight (light, heavy)




kge_bind2a$w1f
kge_bind2a$w2f
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






# 고유어2 대응분석  --------------------------------------------------------------


kge_bind3a %>% kge_weigth_add_ca() %>% dall()

ca_analysis("고유어2", "weight_comb2")#, xlim=c(-0.3, 0.5))

kge_bind3a %>% kge_weigth_add_ca() %>%
  ca_analysis("고유어2", "weigth_c2")#, xlim=c(-0.3, 0.5))



# > 고유어3 부산 ---------------------------------------------------------------
kge_bind2a %>% str()
# 고유어3초성
kge_bind3a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern2("고유어3a") %>%
  auto_pattern2("고유어3b") %>%
  kge_chisq_table("a1","성조", "고유어3 부산 onset")

kge_bind3a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern2("고유어3a") %>%
  auto_pattern2("고유어3b") %>%
  kge_chisq_table("a1","성조", "고유어3 부산 onset") %>%
  patternGraph2()


# 고유어3초성 그래프 전환
# kge_bind3a %>% filter(지역 =="부산" & type=="고유어3") %>%
#   auto_pattern2("고유어3a") %>%
#   auto_pattern2("고유어3b") %>%
#   kge_chisq_table("a1","성조", "고유어3 부산 onset")%>%
#

#* 고유어3 부산 종성 2개 weight (light, heavy)
kge_bind3a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern2("고유어3a") %>%
  auto_pattern2("고유어3b") %>%
  kge_chisq_table("weigth_comb","성조", "고유어3 부산 종성 음절구조(weight)")


kge_weigth_add_ca
#* 고유어3 부산 종성 2개 weight (light, heavy)
kge_bind3a %>% filter(지역 =="부산" & type=="고유어3") %>%
  kge_weigth_add_ca() %>% # 개-ㅌ-개 형태의 데이터 생성
  auto_pattern2("고유어3a") %>%
  auto_pattern2("고유어3b") %>%
  kge_chisq_table("weigth_comb3X","성조", "고유어3 부산 종성 음절구조(weight-X-weight) ")

kge_bind3a %>% filter(지역 =="부산" & type=="고유어3") %>%
  kge_weigth_add_ca() %>% # 개-ㅌ-개 형태의 데이터 생성
  auto_pattern2("고유어3a") %>%
  auto_pattern2("고유어3b") %>%
  kge_chisq_table("weigth_comb3X","성조", "고유어3 부산 종성 음절구조(weight-X-weight) ")%>%
  patternGraph2()


#* 고유어3 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind3a %>% filter(지역 =="부산" & type=="고유어3") %>%
  auto_pattern2("고유어3a") %>%
  auto_pattern2("고유어3b") %>%
  kge_chisq_table("weigth_comb3","성조", "고유어3 부산 종성음절구조(X-weight) type2")







# 고유어3 대응분석  --------------------------------------------------------------


kge_bind3a %>% kge_weigth_add_ca() %>% dall()

ca_analysis("고유어3", "weight_comb2")#, xlim=c(-0.3, 0.5))

kge_bind3a %>% kge_weigth_add_ca() %>%
  ca_analysis("고유어3", "weigth_c3")#


kge_bind3a %>% kge_weigth_add_ca() %>%
  ca_analysis("고유어3", "weigth_cx3")#

kge_bind3a %>% kge_weigth_add_ca() %>%
  ca_analysis("고유어3", "weigth_cy3")#

















# > 외래어2 부산 ---------------------------------------------------------------
kge_bind3a
#외래어2 초성
kge_bind3a %>% filter(지역 =="부산" & type=="외래어2") %>%
  auto_pattern2("외래어2a") %>%
  auto_pattern2("외래어2b") %>%
  kge_chisq_table("a1","성조", "외래어2 부산 onset과 성조형관계")

# 외래어2 그래프 전환
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어2") %>%
    auto_pattern2("외래어2a") %>%
    auto_pattern2("외래어2b") %>%
  kge_chisq_table("a1","성조", "외래어2 부산 onset")%>%
  patternGraph2()


  #* 외래어2 부산 종성 음절구조
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어2") %>%
    auto_pattern2("외래어2a") %>%
    auto_pattern2("외래어2b") %>%
    kge_chisq_table("weigth_comb2","성조", "외래어2 부산 종성음절구조(weight) ")


#* 외래어2 부산 종성 2개 weight (light, heavy)
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어2") %>%
    auto_pattern2("외래어2a") %>%
    auto_pattern2("외래어2b") %>%
  kge_chisq_table("w1f","성조", "외래어2 부산 첫음절 종성 weight ")

  kge_bind3a %>% filter(지역 =="부산" & type=="외래어2") %>%
    auto_pattern2("외래어2a") %>%
    auto_pattern2("외래어2b") %>%
  kge_chisq_table("w2f","성조", "외래어2 부산 첫음절 종성 weight ")





  kge_bind3a %>% filter(지역 =="부산" & type=="외래어2") %>%
    auto_pattern2("외래어2a") %>%
    auto_pattern2("외래어2b") %>%
  kge_chisq_table("weigth_comb2","성조", "외래어2 부산 종성음절구조(weight) ") %>%
  patternGraph2()




  # 외래어2 대응분석  --------------------------------------------------------------


  kge_bind3a %>% kge_weigth_add_ca() %>% dall()

  ca_analysis("외래어2", "weight_comb2")#, xlim=c(-0.3, 0.5))

  kge_bind3a %>% kge_weigth_add_ca() %>%
     filter(지역 =="부산" & type=="외래어2") %>%
    auto_pattern2("외래어2a") %>%
    auto_pattern2("외래어2b") %>%
    ca_analysis("외래어2", "weigth_c2")#

  #
  # kge_bind3a %>% kge_weigth_add_ca() %>%
  #   ca_analysis("고유어3", "weigth_cx3")#
  #
  # kge_bind3a %>% kge_weigth_add_ca() %>%
  #   ca_analysis("고유어3", "weigth_cy3")#






# > 외래어3 부산 ---------------------------------------------------------------
  kge_bind3a %>%  kge_weigth_add_ca() %>%
    filter(지역 =="부산" & type=="외래어3") %>% dall()

# 외래어3 초성
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어3") %>%
    auto_pattern2("외래어3a") %>%
    auto_pattern2("외래어3b") %>%
  kge_chisq_table("a1","성조", "외래어3 부산 onset")

# 외래어3 그래프 전환
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어3") %>%
    auto_pattern2("외래어3a") %>%
    auto_pattern2("외래어3b") %>%
  kge_chisq_table("a1","성조", "외래어3 부산 onset")%>%
  patternGraph2()




  #* 고유어3 부산 종성 2개 weight (light, heavy)
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어3") %>%
    auto_pattern2("외래어3a") %>%
    auto_pattern2("외래어3b") %>%
    kge_chisq_table("weigth_comb","성조", "고유어3 부산 종성 음절구조(weight)")


  # kge_weigth_add_ca
  #* 고유어3 부산 종성 2개 weight (light, heavy)  W-X-W
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어3") %>%
    kge_weigth_add_ca() %>% # 개-ㅌ-개 형태의 데이터 생성
    auto_pattern2("외래어3a") %>%
    auto_pattern2("외래어3b") %>%
    kge_chisq_table("weigth_comb3X","성조", "고유어3 부산 종성 음절구조(weight-X-weight) ")


  #X-W-W
  kge_bind3a %>% filter(지역 =="부산" & type=="외래어3") %>%
    kge_weigth_add_ca() %>% # 개-ㅌ-개 형태의 데이터 생성
    auto_pattern2("외래어3a") %>%
    auto_pattern2("외래어3b") %>%
    kge_chisq_table("weigth_comb3","성조", "외래어3 부산 종성 음절구조(weight-X-weight) ")#%>%
    # patternGraph2()


  # #* 고유어3 부산 종성 2개 weight (light, heavy)그래프 전환
  # kge_bind3a %>% filter(지역 =="부산" & type=="외래어3") %>%
  #   auto_pattern2("외래어3a") %>%
  #   auto_pattern2("외래어3b") %>%
  #   kge_chisq_table("weigth_comb","성조", "외래어3 부산 종성음절구조(X-weight) type2")
  #



  # 외래어3 대응분석  --------------------------------------------------------------


  kge_bind3a %>% kge_weigth_add_ca() %>% dall()

  # ca_analysis("외래어3", "weight_comb3")#, xlim=c(-0.3, 0.5))

  kge_bind3a %>%
    filter(지역 =="부산" & type=="외래어3") %>%
    auto_pattern2("외래어3a") %>%
    auto_pattern2("외래어3b") %>%
    kge_weigth_add_ca() %>%
    ca_analysis("외래어3", "weigth_c3")

  #

# > 외래어4 부산 ---------------------------------------------------------------

# 외래어4 초성
kge_bind3a %>% filter(지역 =="부산" & type=="외래어4") %>%
    auto_pattern2("외래어4a") %>%
    auto_pattern2("외래어4b") %>%
  kge_chisq_table("a1","성조", "외래어4 부산 onset")

# 외래어4 그래프 전환
 #  kge_bind3a %>% filter(지역 =="부산" & type=="외래어4") %>%
 # kge_weigth_add_ca()
 #    auto_pattern2("외래어4a") %>%
 #    auto_pattern2("외래어4b") %>%
 #  kge_chisq_table("a1","성조", "외래어4 부산 onset")%>%
 #  patternGraph2()


#* 외래어4 부산 종성 2개 weight (light, heavy)
  kge_bind3a %>% kge_weigth_add_ca() %>%
    filter(지역 =="부산" & type=="외래어4") %>%
    auto_pattern2("외래어4a") %>%
    auto_pattern2("외래어4b") %>%
  kge_chisq_table("weigth_comb4","성조", "외래어4 부산 첫음절 종성 weight ",
                  yadd = 5,  size_bartext= 4,
                  strip_size = 14,
                  axis_size = 12,
                  text_size = 12)


#* 외개음절-X-X-개음절’, ‘개음절-X-X-폐음절’, ‘폐음절-X-X-개음절’, ‘폐음절-X-X-폐음절’,
  kge_bind3a %>% kge_weigth_add_ca() %>%
    filter(지역 =="부산" & type=="외래어4") %>%
    auto_pattern2("외래어4a") %>%
    auto_pattern2("외래어4b") %>%
  kge_chisq_table("weigth_comb4x","성조", "외래어4 부산 성조형 음절구조(weight) ")


  # ‘X-X-개음절-개음절’
  kge_bind3a %>% kge_weigth_add_ca() %>%
    filter(지역 =="부산" & type=="외래어4") %>%
    auto_pattern2("외래어4a") %>%
    auto_pattern2("외래어4b") %>%
  kge_chisq_table("weigth_comb4y","성조", "외래어4 부산 성조형 음절구조(weight)",
                  yadd = 0.5)




  # 외래어3 대응분석  --------------------------------------------------------------


  kge_bind3a %>% kge_weigth_add_ca() %>% dall()

  # ca_analysis("외래어3", "weight_comb3")#, xlim=c(-0.3, 0.5))

  kge_bind3a %>% filter(지역 =="부산" & type=="외래어4") %>%
    auto_pattern2("외래어4a") %>%
    auto_pattern2("외래어4b") %>%
    kge_weigth_add_ca() %>%
    ca_analysis("외래어4", "weigth_c4", size_text=6)

  #

# > 가상어2 부산 ---------------------------------------------------------------

# 가상어2 초성
  kge_bind3a %>% filter(지역 =="부산" & type=="가상어2") %>%
    # auto_pattern2("가상어4a") %>%
    # auto_pattern2("가상어4b") %>%
  kge_chisq_table("a1","성조", "가상어2 부산 성조형과 onset")

# # 가상어2 그래프 전환
# kge_bind2a %>% filter(지역 =="부산" & type=="가상어2") %>%
#   auto_pattern("가상어2") %>%
#   kge_chisq_table("a1","성조", "가상어2 부산 onset")%>%
#   patternGraph2(yadd=0.4)


#* 가상어2 부산 종성 2개 weight (light, heavy)
kge_bind2a %>% filter(지역 =="부산"  & type=="가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("w1f","성조", "가상어2 부산 첫음절 종성 weight ")


#* 가상어2 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind2a %>% filter(지역 =="부산"  & type=="가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("weigth_comb2","성조", "가상어2 부산 종성음절구조(weight)")

# 가상어2 대응분석  --------------------------------------------------------------


kge_bind3a %>% kge_weigth_add_ca() %>% dall()

# ca_analysis("외래어3", "weight_comb3")#, xlim=c(-0.3, 0.5))

kge_bind3a %>% filter(지역 =="부산" & type=="가상어2") %>%
  kge_weigth_add_ca() %>%
  ca_analysis("가상어2", "weigth_c2", size_text=6,
              xlim=c(-0.6, 1), ylim=c(-0.3, 0.4))




# > 가상어3 부산 ---------------------------------------------------------------
kge_bind3a %>%kge_weigth_add_ca()%>%
  filter(지역 =="부산" & type=="가상어3") %>% dall()
# 가상어3 초성
kge_bind3a %>%  kge_weigth_add_ca()%>%
  filter(지역 =="부산" & type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a1","성조", "가상어3 부산 onset", yadd= 0.3)

# # 가상어3 그래프 전환
# kge_bind3a %>% filter(지역 =="부산" & type=="가상어3") %>%
#   auto_pattern("가상어3") %>%
#   kge_chisq_table("a1","성조", "가상어3 부산 onset")%>%
#   patternGraph2()




#* 가상어3 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind3a %>% kge_weigth_add_ca() %>%
  filter(지역 =="부산" &  type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("weigth_comb","성조", "가상어3 부산 성조형과 음절구조(weight) ") %>%
  patternGraph2()

#* 가상어3 부산 종성 2개 weight (light, heavy)그래프 전환
kge_bind3a%>% kge_weigth_add_ca() %>%
  filter(지역 =="부산" &  type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("weigth_comb","성조", "가상어3 부산 성조형과 음절구조(weight) ")


#* 가상어3 부산 종성 2개W-X-W
kge_bind3a%>% kge_weigth_add_ca() %>%
  filter(지역 =="부산" &  type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("weigth_cx3","성조", "가상어3 부산 성조형과 음절구조(weight) ")


#X-W-W
kge_bind3a%>% kge_weigth_add_ca() %>%
  filter(지역 =="부산" &  type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("weigth_cy3","성조", "가상어3 부산 성조형과 음절구조(weight) ")


  # patternGraph2()

# #plot recheck
# kge_bind2a %>% filter(지역 =="부산" & type == "고유어2") %>%
#   select(성조,a1, a3) %>% unite(onset_coda, a1:a3, sep = " / ") %>%
#   table() %>% t() %>% FactoMineR::CA() %>% plot()

# 가상어3 대응분석  --------------------------------------------------------------


kge_bind3a %>% kge_weigth_add_ca() %>% dall()

# ca_analysis("외래어3", "weight_comb3")#, xlim=c(-0.3, 0.5))

kge_bind3a %>% filter(지역 =="부산" & type=="가상어3") %>%
  kge_weigth_add_ca() %>%
  ca_analysis("가상어3", "weigth_c3", size_text=6,
              xlim=c(-0.8, 1.2), ylim=c(-0.7, 0.5))

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
    xtabs(formula = formula( paste("~ 성조 + a1 +", selcol) ))%>%
    ca::mjca()

  explain =   round((df$inertia.e[1] +df$inertia.e[2])*100 , 2)

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
