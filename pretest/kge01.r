
u="02_OutAna/kimgoeun_seoul_phD2024/data/kge240307.xlsx"
library(tidyverse)
library(readxl)
library(jjstat)

kge0 = read_excel("02_OutAna/kimgoeun_seoul_phD2024/data/kge240307.xlsx")
kge0


kge0$BY1_N %>% unique()
kge0$BY1_N %>% Freq_table()

kge0$BY2_N %>% unique()
kge0$BY2_N %>% Freq_table()

kge0$A1 %>% unique()
kge0$A1 %>% Freq_table()

# kge0$A3 %>% unique()
kge0$A3 %>% Freq_table()

# kge0$B1 %>% unique()
kge0$B1 %>% Freq_table()

# kge0$B3 %>% unique()
kge0$B3 %>% Freq_table()

# kge0$C1 %>% unique()
kge0$C1 %>% Freq_table()

# kge0$C3 %>% unique()
kge0$C3 %>% Freq_table()


음절
split_word <- function(word) {
  # 입력된 단어를 음절 단위로 분리
  return(strsplit(word, split = "")[[1]])
}

split_word("호랑이")
split_word("HLL")
# 예시: "호랑이" 단어를 음절 단위로 나누기
input_word <- "호랑이"
result <- split_word(input_word)
print(result)

# strsplit("호랑이", split = "")



split_korean_word <- function(word, paste=TRUE) {
  # Hangul Unicode Range
  HANGUL_START <- 44032
  HANGUL_END <- 55203

  # Initialization list
  CHOSUNG_LIST <- c(
    'ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  )

  # Neutral list
  JUNGSUNG_LIST <- c(
    'ㅏ', 'ㅐ', 'ㅑ', 'ㅒ', 'ㅓ', 'ㅔ', 'ㅕ', 'ㅖ', 'ㅗ', 'ㅘ',
    'ㅙ', 'ㅚ', 'ㅛ', 'ㅜ', 'ㅝ', 'ㅞ', 'ㅟ', 'ㅠ', 'ㅡ', 'ㅢ', 'ㅣ'
  )

  # Species List
  JONGSUNG_LIST <- c(
    '', 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ',
    'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  )

  result <- character(0)
  for (char in strsplit(word, '')[[1]]) {
    if (char %in% letters) {
      result <- append(result, char)

    } else if (HANGUL_START <= utf8ToInt(char) && utf8ToInt(char) <= HANGUL_END) {
      ## Locate Korean consonants and vowels
      char_code <- utf8ToInt(char) - HANGUL_START
      chosung_index <- char_code %/% 21 %/% 28
      jungsung_index <- char_code %/% 28 %% 21
      jongsung_index <- char_code %% 28

      result <- append(result, CHOSUNG_LIST[chosung_index + 1])
      result <- append(result, JUNGSUNG_LIST[jungsung_index + 1])
      
      if (jongsung_index > 0) {
        result <- append(result, JONGSUNG_LIST[jongsung_index + 1])
      
      } else {
        result <- append(result, '')  # Insert an empty string
      }
    } else {
      result <- append(result, char)
    }
  }
if(paste){
  return(result %>% paste0(collapse=","))
}else{
   return(result) 
}
}



# 사용 예시
# input_word <- "책"
split_korean_word("책")
split_korean_word("채")
split_korean_word("고양이")
split_korean_word("곱창밥")

jjstat::split_korean_word("통계분석")
jjstat::split_korean_word("곱창밥")

# cat(paste(split_korean_word("책"), collapse = ' '),"\n\n")  # 출력: 'ㅊ ㅐ ㄱ'

# "함수명은 split_k_word_df로 하여 함수를 추가로 만들어주세요  

# 위에 R함수 split_korean_word를 이용하여 2음절 언어와 3음절 언어가 들어오면 초성과 중성, 종성을 분리하는 함수를 추가로 더 만들어 주세요. 
# 2음절은 "토끼",  3음절은 "호랑이" 와 같은 단어입니다. 
# 단 2음절 단어도 3음절을 기준하여 만들어주시구요. 
# 마지막 종성에서 받침이 없으면 ""이 들어가는 split_korean_word의 규칙이 유지 됩니다. 

# 예) 
# split_k_word_df("토끼")
# 결과:
#  ㅌ ㅗ ㄲ ㅣ 

# split_k_word_df("호랑이")
# 결과:
# ㅎ ㅗ ㄹ ㅏ ㅇ ㅇ ㅣ 
# "




"

utf8ToInt(char):
이 함수는 문자열 char의 UTF-8 인코딩 값을 정수로 변환합니다.
한글 문자의 경우, 해당 문자의 유니코드 값을 반환합니다.

%in%:
이 연산자는 벡터에 특정 값이 포함되어 있는지 여부를 확인합니다.
예를 들어, char %in% letters는 char가 알파벳 문자인지 확인합니다.

%/%와 %%:
%/%는 정수 나눗셈 연산자입니다. 예를 들어, 5 %/% 2는 2를 반환합니다.
%%는 나머지 연산자입니다. 예를 들어, 5 %% 2는 1을 반환합니다.

append(result, char):
이 함수는 벡터 result에 원소 char를 추가합니다.
result 벡터의 끝에 char를 붙입니다.

"
append(1:5, 0:1, after = 3)
append(1:5, 0:1)




# split_k_word_df 함수 (2음절 또는 3음절 단어를 받아서 1음절로 분리 후 초성, 중성, 종성 분리)
split_korean_word_check <- function(worddata, type = "all") {
    
if(nchar(worddata)==2){
   worddata =  paste0(worddata,"   ")
}else{
    worddata
}

  # Isolate initial, neutral, and final gender
  separated_korean <- split_korean_word(worddata)

res = separated_korean %>% paste0(collapse=",") 
res0 = list(worddata, separated_korean)
res2 = data.frame(res)%>%
            t() %>% as.data.frame() %>%
                tibble::rownames_to_column("word")


switch(type, 
    text = res,
    all = res0,
    df = res2
        )
}

# 사용 예시
# split_korean_word("책이")
# split_korean_word("호랑이")

# nchar("토끼")
# nchar("호랑이")

split_k_word_df("토끼") 

split_k_word_df("호랑이")  
split_k_word_df("호랑잉") 

split_k_word_df("토끼   ", type= "all") 
split_k_word_df("토끼   ", type= "text")  


split_k_word_df("호랑이") 
############################################################ 
# split_kw_df 함수 (초성, 중성, 종성 분리하여 syllable 열에 텍스트 데이터로 넣기)


split_kw_df <- function(df, sel_col="word", remove = TRUE) {
  # Separate each word in the word column into initial, neutral, and final.
 if(!is.data.frame(df)){
  stop("You Must Enter as a [data frame], you need check your data ")
 }

 df = df %>% dplyr::mutate(N = nchar(word))

spl = function(separated_word) {
    # separated_word <- paste(unlist(strsplit(word, '')), collapse = ' ')
    if(nchar(separated_word)==2){
       separated_word = paste0(separated_word,"   ")
       }else{
         separated_word
         }

    separated_korean <- split_korean_word(separated_word)
    paste(separated_korean, collapse = '')
  }

  df$syllabic <- sapply(df[, sel_col], spl)

df = separate(df, syllabic, c(
                    # paste0("init", 1:3),
                    # paste0("mid", 1:3),
                    # paste0("final", 1:3)
        paste0(rep(LETTERS[1:max(df$N)], each = 3), 
                            rep(1:3, times = 3))
                                        ), sep=",", 
                    remove = remove )  

### REMOVe <NA>                   
df[is.na(df)] <- ""

# return(list(df, df1))
return(df)

## suppres warning message 
options(warn = -1)

}

library(jjstat)

wd <- data.frame(
  id =  paste0("BY", 1:5),
  word = c("토끼", "휘파람", "뻐스", "우리냥","우리낙"),
  tone = c("HH","LHH","HL","LHH","LHL" ),
  gender =c("F","F","M","M","M"),
  age = c(20,60,20,60, 30),
  area= c("n","s","n","s","s"),
  mix= c(FALSE,FALSE,FALSE,TRUE,TRUE),
  stringsAsFactors = FALSE)
wd
wd%>% jjstat::split_kw_df("word")
wd%>%split_kw_df()



split_kw_df_match <- function(df, 
                                sel_col="word", 
                                remove = TRUE,
                                type="res" ){
  # Separate each word in the word column into initial, neutral, and final.
 if(!is.data.frame(df)){
  stop("You Must Enter as a [data frame], you need check your data ")
                   }

 df = df %>% dplyr::mutate(N = nchar(word))
 n_col =  ncol(df)  #select 


spl = function(separated_word) {

    if(nchar(separated_word)==2){
       separated_word = paste0(separated_word,"   ")
       }else{
         separated_word        }

    separated_korean <- split_korean_word(separated_word)
    paste(separated_korean, collapse = '')
       }




  df$syllabic <- sapply(df[, sel_col], spl)

  df = separate(df, syllabic, c(
        paste0(rep(LETTERS[1:max(df$N)], each = 3), 
                            rep(1:3, times = 3))
                                        ), sep=",", 
                    remove = remove )  


# tonecheck 
tonecheck <- data.frame(
  tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
  code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "null", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "que", "que", "que")
 )

# Find variables with a letter followed by a number
df1 <- df
target_columns <- grep("[A-Z]1$", names(df1), value = TRUE)


# Create a1, b1, c1 by matching their values with tonecheck data
for (col in target_columns) {
  df1[[col]] <- tonecheck$code[match(df1[[col]], tonecheck$tone)]

    }
   colnames(df1) = tolower(colnames(df1))


tonecheck2 <- data.frame(
  tone = c("ㅏ", "ㅐ", "ㅑ", "ㅒ", "ㅓ", "ㅔ", "ㅕ", "ㅖ", "ㅗ", 
           "ㅘ", "ㅙ", "ㅚ", "ㅛ", "ㅜ", "ㅝ", "ㅞ", "ㅟ", "ㅠ", "ㅡ", "ㅢ"),
  code = c("sg", "sg", "dp", "dp", "sg", "sg", "dp", "dp", "sg",
           "dp", "dp", "dp", "dp", "sg", "dp", "dp", "dp", "dp", "sg", "dp")
            )

df2 <- df

target_columns2 <- grep("[A-Z]2$", names(df2), value = TRUE)
  for (col2 in target_columns2) {
  df2[[col2]] <- tonecheck2$code[match(df2[[col2]], tonecheck2$tone)]
    }
 colnames(df2) = tolower(colnames(df2))

tonecheck3 <- data.frame(
  tone = c("ㄱ", "ㄲ", "ㄳ", "ㄴ", "ㄵ", "ㄶ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ", "ㄽ", "ㄾ", "ㄿ", "ㅀ", "ㅁ", "ㅂ", "ㅄ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ"),
  code = c("obs", "obs", "obs", "son", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs")
 )

df3 <- df
 target_columns3 <- grep("[A-Z]3$", names(df3), value = TRUE)
  for (col3 in target_columns3) {
  df3[[col3]] <- tonecheck3$code[match(df3[[col3]], tonecheck3$tone)]
   }
   colnames(df3) = tolower(colnames(df3))



if(max(df$N) < 4 ){
df = bind_cols(df, 
                df1[,-c(1: n_col)][c(1)], 
                df2[,-c(1: n_col)][c(2)], 
                df3[,-c(1: n_col)][c(3)], 
                df1[,-c(1: n_col)][c(4)], 
                df2[,-c(1: n_col)][c(5)],
                df3[,-c(1: n_col)][c(6)],
                df1[,-c(1: n_col)][c(7)], 
                df2[,-c(1: n_col)][c(8)],
                df3[,-c(1: n_col)][c(9)]
                 )
}else if(max(df$N) == 4){
df = bind_cols(df, 
                df1[,-c(1: n_col)][c(1)], 
                df2[,-c(1: n_col)][c(2)], 
                df3[,-c(1: n_col)][c(3)], 
                df1[,-c(1: n_col)][c(4)], 
                df2[,-c(1: n_col)][c(5)],
                df3[,-c(1: n_col)][c(6)],
                df1[,-c(1: n_col)][c(7)], 
                df2[,-c(1: n_col)][c(8)],
                df3[,-c(1: n_col)][c(9)],
                df1[,-c(1: n_col)][c(10)], 
                df2[,-c(1: n_col)][c(11)],
                df3[,-c(1: n_col)][c(12)]
                 )
 }else if(max(df$N) == 5){
   df = bind_cols(df, 
                df1[,-c(1: n_col)][c(1)], 
                df2[,-c(1: n_col)][c(2)], 
                df3[,-c(1: n_col)][c(3)], 
                df1[,-c(1: n_col)][c(4)], 
                df2[,-c(1: n_col)][c(5)],
                df3[,-c(1: n_col)][c(6)],
                df1[,-c(1: n_col)][c(7)], 
                df2[,-c(1: n_col)][c(8)],
                df3[,-c(1: n_col)][c(9)],
                df1[,-c(1: n_col)][c(10)], 
                df2[,-c(1: n_col)][c(11)],
                df3[,-c(1: n_col)][c(12)],
                df1[,-c(1: n_col)][c(13)], 
                df2[,-c(1: n_col)][c(14)],
                df3[,-c(1: n_col)][c(15)])
            }
### REMOVe <NA>                   
df[is.na(df)] <- ""
df1[is.na(df1)] <- ""
df2[is.na(df2)] <- ""
df3[is.na(df3)] <- ""

resall = list(raw = df, 
              초성 = df1, 
              중성 = df2, 
              종성 = df3)
res = df
options(warn = -1)
# return(df)
switch(type, res = res, all = resall)


}


library(tidyverse)
wd2 %>% jjstat::split_kw_df_match("word", type="res")
wd2%>%split_kw_df_match("word", type="all")
# library(dplyr)
# df <- df %>% select_if(~sum(!is.na(.)) > 0)
# # options(warn = 0)
wd
wd%>%split_kw_df("word")
wd%>%split_kw_df_match("word", type="res")

wd%>%split_kw_df_match("word", type="all")
wd%>%split_kw_df_match("word", type="all", analysis=T)

wd%>%split_kw_df_match("word", type="all", analysis=F)

wd <- data.frame(
  id =  paste0("BY", 1:5),
  word = c("토끼", "휘파람", "뻐스", "우리냥","우리낙"),
  tone = c("HH","LHH","HL","LHH","LHL" ),
  gender =c("F","F","M","M","M"),
  age = c(20,60,20,60, 30),
  area= c("n","s","n","s","s"),
  mix= c(FALSE,FALSE,FALSE,TRUE,TRUE),
  stringsAsFactors = FALSE
)
wd%>%split_kw_df_match("word", type="res")
                # df1[,-c(1: n_col)][c(13)], 
                # df2[,-c(1: n_col)][c(14)],
                # df3[,-c(1: n_col)][c(15)],
                # df1[,-c(1: n_col)][c(16)], 
                # df2[,-c(1: n_col)][c(17)],
                # df3[,-c(1: n_col)][c(18)]

wd2 <- data.frame(
  id =  paste0("BY", 1:5),
  word = c("토끼", "휘파람", "뻐스", "우리냥","우리낙"))

wd2%>%split_kw_df_match("word", type="res")
wd2%>%split_kw_df_match("word", type="all")




wd2%>%jjstat::split_kw_df_match("word", type="res")
wd2%>%split_kw_df_match("word", type="all")




# 주어진 벡터
vec <- 1:120

# 1, 4, 7과 같이 3칸씩 건너뛰어 값을 선택
result <- vec[seq(1, length(vec), 3)]

# 결과 출력
print(result)






# tonecheck 데이터프레임을 참고하여 sig 열 추가
df1$sig <- tonecheck$code[match(df1$A1, tonecheck$tone)]
df1 %>%rm()

# df <- df %>% select_if(~sum(!is.na(.)) > 0)
#  rep(paste0(LETTERS[1:4], 1:3), each = 3)
#  rep(paste0(LETTERS[1:4], 1:3),  3)
 
#  paste0(LETTERS[1:3], 1:3)

# A1부터 C3까지의 문자열 생성
# iv <- c()
# for (i in LETTERS[1:3]) {
#   for (j in 1:3) {
#     iv <- c(iv, paste0(i, j))
#   }
# }

# # 결과 출력
# cat(iv, sep = ", ")
 paste0(rep(LETTERS[1:3], each = 3), rep(1:3, times = 3))



#  paste0(1:12, c("st", "nd", "rd", rep("th", 9)))
 
 ## paste works the same, but separates each input with

# 사용 예시
wd <- data.frame(
  id =  paste0("BY", 1:5),
  word = c("토끼", "휘파람", "뻐스", "우리냥","우리낙"),
  tone = c("HH","LHH","HL","LHH","LHL" ),
  gender =c("F","F","M","M","M"),
  age = c(20,60,20,60, 30),
  area= c("n","s","n","s","s"),
  mix= c(FALSE,FALSE,FALSE,TRUE,TRUE),
  stringsAsFactors = FALSE
)
wd[1:3][c(1,2)]
wd%>%split_kw_df("word")%>% dplyr::select(1,2,3,8,9,10,11,12)
wd%>%split_kw_df("word")
# c("호랑이")%>%split_kw_df("word")

wd %>%split_kw_df(col= "word")

# tonecheck = jjstat::datapaste()
# tonecheck
tonecheck <- data.frame(
  tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
  code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "null", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "que", "que", "que")
)

syllabics= list(
  CHOSUNG_19 = c(
    'ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  ),

  # 중성 리스트
  JUNGSUNG_21 =c(
    'ㅏ', 'ㅐ', 'ㅑ', 'ㅒ', 'ㅓ', 'ㅔ', 'ㅕ', 'ㅖ', 'ㅗ', 'ㅘ',
    'ㅙ', 'ㅚ', 'ㅛ', 'ㅜ', 'ㅝ', 'ㅞ', 'ㅟ', 'ㅠ', 'ㅡ', 'ㅢ', 'ㅣ'
  ),

  # 종성 리스트
  JONGSUNG_28 = c(
    '', 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ',
    'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  ))
syllabics
syllabics[[1]][19]
syllabics[[2]][10]





data.frame(
    word = c("토끼","호랑이","엽전","마나리")
) %>% mutate(sl = split_k_word_df(word))



# 주어진 데이터프레임 df1
df1 <- data.frame(
  id = c("BY1", "BY2", "BY3", "BY4", "BY5"),
  word = c("토끼", "휘파람", "뻐스", "우리냥", "우리낭"),
  tone = c("HH", "LHH", "HL", "LHH", "LHL"),
  N = c(2, 3, 2, 3, 3),
  A1 = c("ㅌ", "ㅎ", "ㅃ", "ㅇ", "ㅇ")
)

# 주어진 데이터프레임 tonecheck


# tonecheck 데이터프레임을 참고하여 sig 열 추가
df1$sig <- tonecheck$code[match(df1$A1, tonecheck$초성)]

# 결과 출력
print(df1)





# tonecheck 데이터프레임
tonecheck <- data.frame(
  tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
  code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "null", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "que", "que", "que")
)

# 문자 뒤에 숫자가 1인 변수 찾기
target_columns <- grep("[A-Z]1$", names(df1), value = TRUE)

# 해당하는 값을 tonecheck 데이터와 매칭하여 a1, b1, c1 생성
for (col in target_columns) {
  df1[[col]] <- tonecheck$code[match(df1[[col]], tonecheck$tone)]
}

# 결과 출력
print(df1)






























**[Cameron, A. Colin, and Pravin K. Trivedi. Regression analysis of panel data. Cambridge University Press, 2010.]

# 패널 데이터 불러오기
# install.packages("plm")
library(plm)
data("Produc")
Produc %>%str()
data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)
# replicates some results from Baltagi (2013), table 3.1
data("Grunfeld", package = "plm")
p <- plm(inv ~ value + capital,
         data = Grunfeld, model = "pooling")
wi <- plm(inv ~ value + capital,
          data = Grunfeld, model = "within", effect = "twoways")
swar <- plm(inv ~ value + capital,
            data = Grunfeld, model = "random", effect = "twoways")
amemiya <- plm(inv ~ value + capital,
               data = Grunfeld, model = "random", random.method = "amemiya",
               effect = "twoways")
walhus <- plm(inv ~ value + capital,
              data = Grunfeld, model = "random", random.method = "walhus",
              effect = "twoways")
# summary and summary with a furnished vcov (passed as matrix, 
# as function, and as function with additional argument)
summary(wi)
summary(wi, vcov = vcovHC(wi))
summary(wi, vcov = vcovHC)
summary(wi, vcov = function(x) vcovHC(x, method = "white2"))
## nested random effect model
# replicate Baltagi/Song/Jung (2001), p. 378 (table 6), columns SA, WH
# == Baltagi (2013), pp. 204-205
data("Produc", package = "plm")
pProduc <- pdata.frame(Produc, index = c("state", "year", "region"))
pProduc %>%head(10)
   ,form <- log(gsp) ~ log(pc) + log(emp) + log(hwy) + log(water) + log(util) + unemp
summary(plm(form, data = pProduc, model = "random", effect = "nested"))
summary(plm(form, data = pProduc, model = "random", effect = "nested",
            random.method = "walhus"))
## Instrumental variable estimations
# replicate Baltagi (2013/2021), p. 133/162, table 7.1
data("Crime", package = "plm")
FE2SLS <- plm(lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen +
                ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
                lwsta + lwloc + lpctymle + lpctmin + region + smsa + factor(year)
              | . - lprbarr - lpolpc + ltaxpc + lmix,
              data = Crime, model = "within")
G2SLS <- update(FE2SLS, model = "random", inst.method = "bvk")
EC2SLS <- update(G2SLS, model = "random", inst.method = "baltagi")
## Hausman-Taylor estimator and Amemiya-MaCurdy estimator
# replicate Baltagi (2005, 2013), table 7.4; Baltagi (2021), table 7.5
data("Wages", package = "plm")
ht <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
              bluecol + ind + union + sex + black + ed |
              bluecol + south + smsa + ind + sex + black |
              wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595,
          random.method = "ht", model = "random", inst.method = "baltagi")
summary(ht)
am <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
              bluecol + ind + union + sex + black + ed |
              bluecol + south + smsa + ind + sex + black |
              wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595,
          random.method = "ht", model = "random", inst.method = "am")
summary(am)

# 패널 데이터 불러오기
library(plm)
data("Grunfeld")
Grunfeld%>%str()
# 모델 정의 (예: VAR 모델)
# install.packages("vars")
library(vars)
VARmodel <- VAR(Grunfeld, p = 2)
VARmodel
# 모델 추정
VARfit <- summary(VARmodel)
VARfit
# 평균 그룹 추정
mean_group <- apply(coef(VARfit), 1, mean)

# 결과 해석
print(mean_group)
