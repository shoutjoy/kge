
library(jjstat)
library(FactoMineR) #대응분석 CA함수
library(factoextra) #대응분석 plot함수

#데이터 출력함수
printall <- function(data) print(data, n=Inf)
pall <- function(data) print(data, n=Inf)
dall <- function(data) data.frame(data)



#한글의 초성과 중성, 종성을 분리하는 함수
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
#
# split_korean_word("고양이")
# split_korean_word("다이나믹")
# split_korean_word("곱창밥")



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






#######################################################################
split_kw_df_match <- function(df,
                              sel_col="word",
                              remove = TRUE,
                              type="res" ){
  # Separate each word in the word column into initial, neutral, and final.
  if(!is.data.frame(df)){
    # stop("You Must Enter as a [data frame], you need check your data ")
    df <- as.data.frame(df)
  }

  df = df %>% as.data.frame()%>% dplyr::mutate(N = nchar(word))
  n_col =  ncol(df)  #select


  spl = function(separated_word) {

    if(nchar(separated_word)==1){
      separated_word = paste0(separated_word,"      ")

    }else if(nchar(separated_word)==2){
      separated_word = paste0(separated_word,"   ")

    }else{
      separated_word  }



    separated_korean <- split_korean_word(separated_word)

    paste(separated_korean, collapse = '')
  }

  df$syllabic <- sapply(as.vector(df[, sel_col]), spl)

  df = separate(df, syllabic, c(
    paste0(rep(LETTERS[1:max(df$N)], each = 3),
           rep(1:3, times = 3))
  ), sep=",",
  remove = remove )


  # tonecheck
  # tonecheck <- data.frame(
  #   tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
  #   code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "null", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "que", "que", "que")
  # )

  tonecheck <- data.frame(
    tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
    code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "son", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "fri", "fri", "fri")
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
    tone = c("ㄱ", "ㄲ", "ㄳ", "ㄴ", "ㄵ", "ㄶ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ", "ㄽ", "ㄾ", "ㄿ", "ㅀ", "ㅁ", "ㅂ", "ㅄ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ", ""),
    code = c("obs", "obs", "obs", "son", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs","light")
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
  ## REMOVe <NA>
  df[is.na(df)] <- ""
  df1[is.na(df1)] <- ""
  df2[is.na(df2)] <- ""
  df3[is.na(df3)] <- ""

  resall = list(raw = df,
                초성 = df1,
                중성 = df2,
                종성 = df3)
  res = df %>% tibble::tibble()
  options(warn = -1)
  # return(df)
  switch(type, res = tibble::tibble(res), all = resall)
}


#한글로 변형하는 코드 -----------
split_kw_df_match_ko <- function(df,
                                 sel_col="word",
                                 remove = TRUE,
                                 type="res" ){
  # Separate each word in the word column into initial, neutral, and final.
  if(!is.data.frame(df)){
    # stop("You Must Enter as a [data frame], you need check your data ")
    df <- as.data.frame(df)
  }

  df = df %>% as.data.frame()%>% dplyr::mutate(N = nchar(word))
  n_col =  ncol(df)  #select


  spl = function(separated_word) {

    if(nchar(separated_word)==1){
      separated_word = paste0(separated_word,"      ")

    }else if(nchar(separated_word)==2){
      separated_word = paste0(separated_word,"   ")

    }else{
      separated_word  }



    separated_korean <- split_korean_word(separated_word)

    paste(separated_korean, collapse = '')
  }

  df$syllabic <- sapply(as.vector(df[, sel_col]), spl)

  df = separate(df, syllabic, c(
    paste0(rep(LETTERS[1:max(df$N)], each = 3),
           rep(1:3, times = 3))
  ), sep=",",
  remove = remove )


  # tonecheck
  # tonecheck <- data.frame(
  #   tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
  #   code = c("평파열음_평파찰음", "평파열음_평파찰음", "평파열음_평파찰음", "평파열음_평파찰음", "평파열음_평파찰음", "폐음절_공명자음", "폐음절_공명자음", "폐음절_공명자음", "null", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "que", "que", "que")
  # )

  tonecheck <- data.frame(
    tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
    code = c("평파열음_평파찰음", "평파열음_평파찰음", "평파열음_평파찰음", "평파열음_평파찰음", "평파열음_평파찰음", "공명음", "공명음", "공명음", "공명음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "유기음_경음", "마찰음", "마찰음", "마찰음")
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
    tone = c("ㄱ", "ㄲ", "ㄳ", "ㄴ", "ㄵ", "ㄶ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ", "ㄽ", "ㄾ", "ㄿ", "ㅀ", "ㅁ", "ㅂ", "ㅄ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ", ""),
    code = c("폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_공명자음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_공명자음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_공명자음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_공명자음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음", "폐음절_평파열음","개음절")
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
  ## REMOVe <NA>
  df[is.na(df)] <- ""
  df1[is.na(df1)] <- ""
  df2[is.na(df2)] <- ""
  df3[is.na(df3)] <- ""

  resall = list(raw = df,
                초성 = df1,
                중성 = df2,
                종성 = df3)
  res = df %>% tibble::tibble()
  options(warn = -1)
  # return(df)
  switch(type, res = tibble::tibble(res), all = resall)
}





kge_weigth = function(df, type= "res", pattern ="", remove= FALSE){

  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1),
      w4 = ifelse(D3 == pattern, 0, 1)
    ) %>% as.data.frame()

  df00 = df0 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
    tibble::tibble()

  # df001 = df00 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  #   tibble::tibble()
  df001= df00 %>% mutate(
              W3 = substr(Wz, 1, 3),
              W2 = substr(Wz, 1, 2)
                         )
  df002 = df001%>% tibble::tibble()%>%
    mutate(
      w1f = ifelse(w1 == 0, "light", "heavy"),
      w2f = ifelse(w2 == 0, "light", "heavy"),
      w3f = ifelse(w3 == 0, "light", "heavy"),
      w4f = ifelse(w4 == 0, "light", "heavy"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), factor)



  df1 = df002 %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        W3 == "000" ~ 1,
        W3 == "001" ~ 2,
        W3 == "010" ~ 3,
        W3 == "011" ~ 4,
        W3 == "100" ~ 5,
        W3 == "101" ~ 6,
        W3 == "110" ~ 7,
        W3 == "111" ~ 8)
    )%>% tibble::tibble()

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb = dplyr::case_when(
        W3 == "000" ~ "light-light-light",
        W3 == "001" ~ "light-light-heavy",
        W3 == "010" ~ "light-heavy-light",
        W3 == "011" ~ "light-heavy-heavy",
        W3 == "100" ~ "heavy-light-light",
        W3 == "101" ~ "heavy-light-heavy",
        W3 == "110" ~ "heavy-heavy-light",
        W3 == "111" ~ "heavy-heavy-heavy"),

      weigth_comb2 =  dplyr::case_when(
        W2 == "00" ~ "light-light",
        W2 == "01" ~ "light-heavy",
        W2 == "10" ~ "heavy-light",
        W2 == "11" ~ "heavy-heavy"),

      weigth_comb3 = dplyr::case_when(
        W3 == "000" ~ "X-light-light",
        W3 == "001" ~ "X-light-heavy",
        W3 == "010" ~ "X-heavy-light",
        W3 == "011" ~ "X-heavy-heavy",
        W3 == "100" ~ "X-light-light",
        W3 == "101" ~ "X-light-heavy",
        W3 == "110" ~ "X-heavy-light",
        W3 == "111" ~ "X-heavy-heavy"),


      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "light-light-light-light",
        Wz == "0001" ~ "light-light-light-heavy",
        Wz == "0010" ~ "light-light-heavy-light",
        Wz == "0011" ~ "light-light-heavy-heavy",
        Wz == "0100" ~ "light-heavy-light-light",
        Wz == "0101" ~ "light-heavy-light-heavy",
        Wz == "0110" ~ "light-heavy-heavy-light",
        Wz == "0111" ~ "light-heavy-heavy-heavy",
        Wz == "1000" ~ "heavy-light-light-light",
        Wz == "1001" ~ "heavy-light-light-heavy",
        Wz == "1010" ~ "heavy-light-heavy-light",
        Wz == "1011" ~ "heavy-light-heavy-heavy",
        Wz == "1100" ~ "heavy-heavy-light-light",
        Wz == "1101" ~ "heavy-heavy-light-heavy",
        Wz == "1110" ~ "heavy-heavy-heavy-light",
        Wz == "1111" ~ "heavy-heavy-heavy-heavy")


    )%>% tibble::tibble()

  df3 = df2 %>% dplyr::select(-weight) %>% tibble::tibble()


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}





#
# kge_bind0%>% kge_weigth() %>%
#   # kge_weigth_add()%>%
#   data.frame()
#
# kge_bind0 %>% split_kw_df_match() %>% data.frame()
# kge_bind0 %>% split_kw_df_match()%>% kge_weigth() %>% data.frame()
# # load(file = "KGEbind.RData")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res", remove = F)
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res1")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res2")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res3")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res4")





#한국어 표시 코드 변경 -----
kge_weigth_ko = function(df, type= "res", pattern ="", remove= FALSE){

  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1),
      w4 = ifelse(D3 == pattern, 0, 1)
    ) %>% as.data.frame()

  df00 = df0 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
    tibble::tibble()

  # df001 = df00 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  #   tibble::tibble()
  df001= df00 %>% mutate(
    W3 = substr(Wz, 1, 3),
    W2 = substr(Wz, 1, 2)
  )
  df002 = df001%>% tibble::tibble()%>%
    mutate(
      w1f = ifelse(w1 == 0, "개음절", "폐음절"),
      w2f = ifelse(w2 == 0, "개음절", "폐음절"),
      w3f = ifelse(w3 == 0, "개음절", "폐음절"),
      w4f = ifelse(w4 == 0, "개음절", "폐음절"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), factor)



  df1 = df002 %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        W3 == "000" ~ 1,
        W3 == "001" ~ 2,
        W3 == "010" ~ 3,
        W3 == "011" ~ 4,
        W3 == "100" ~ 5,
        W3 == "101" ~ 6,
        W3 == "110" ~ 7,
        W3 == "111" ~ 8)
    )%>% tibble::tibble()

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb = dplyr::case_when(
        W3 == "000" ~ "개음절-개음절-개음절",
        W3 == "001" ~ "개음절-개음절-폐음절",
        W3 == "010" ~ "개음절-폐음절-개음절",
        W3 == "011" ~ "개음절-폐음절-폐음절",
        W3 == "100" ~ "폐음절-개음절-개음절",
        W3 == "101" ~ "폐음절-개음절-폐음절",
        W3 == "110" ~ "폐음절-폐음절-개음절",
        W3 == "111" ~ "폐음절-폐음절-폐음절"),

      weigth_comb2 =  dplyr::case_when(
        W2 == "00" ~ "개음절-개음절",
        W2 == "01" ~ "개음절-폐음절",
        W2 == "10" ~ "폐음절-개음절",
        W2 == "11" ~ "폐음절-폐음절"),

      weigth_comb3 = dplyr::case_when(
        W3 == "000" ~ "X-개음절-개음절",
        W3 == "001" ~ "X-개음절-폐음절",
        W3 == "010" ~ "X-폐음절-개음절",
        W3 == "011" ~ "X-폐음절-폐음절",
        W3 == "100" ~ "X-개음절-개음절",
        W3 == "101" ~ "X-개음절-폐음절",
        W3 == "110" ~ "X-폐음절-개음절",
        W3 == "111" ~ "X-폐음절-폐음절"),


      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "개음절-개음절-개음절-개음절",
        Wz == "0001" ~ "개음절-개음절-개음절-폐음절",
        Wz == "0010" ~ "개음절-개음절-폐음절-개음절",
        Wz == "0011" ~ "개음절-개음절-폐음절-폐음절",
        Wz == "0100" ~ "개음절-폐음절-개음절-개음절",
        Wz == "0101" ~ "개음절-폐음절-개음절-폐음절",
        Wz == "0110" ~ "개음절-폐음절-폐음절-개음절",
        Wz == "0111" ~ "개음절-폐음절-폐음절-폐음절",
        Wz == "1000" ~ "폐음절-개음절-개음절-개음절",
        Wz == "1001" ~ "폐음절-개음절-개음절-폐음절",
        Wz == "1010" ~ "폐음절-개음절-폐음절-개음절",
        Wz == "1011" ~ "폐음절-개음절-폐음절-폐음절",
        Wz == "1100" ~ "폐음절-폐음절-개음절-개음절",
        Wz == "1101" ~ "폐음절-폐음절-개음절-폐음절",
        Wz == "1110" ~ "폐음절-폐음절-폐음절-개음절",
        Wz == "1111" ~ "폐음절-폐음절-폐음절-폐음절")


    )%>% tibble::tibble()

  df3 = df2 %>% dplyr::select(-weight) %>% tibble::tibble()


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}





















kge_weigth_add = function(df){
  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1f = ifelse(w1 == 0, "light", "heavy"),
      w2f = ifelse(w2 == 0, "light", "heavy"),
      w3f = ifelse(w3 == 0, "light", "heavy"),
      w4f = ifelse(w4 == 0, "light", "heavy"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), as.factor)


  df1 = df0%>%
    dplyr::mutate(
      weigth_comb2 = dplyr::case_when(
        W3 == "000" ~ "light-light",
        W3 == "001" ~ "light-light",
        W3 == "010" ~ "light-heavy",
        W3 == "011" ~ "light-heavy",
        W3 == "100" ~ "heavy-light",
        W3 == "101" ~ "heavy-light",
        W3 == "110" ~ "heavy-heavy",
        W3 == "111" ~ "heavy-heavy")
    )%>% tibble::tibble()

  # df1

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb3 = dplyr::case_when(
        W3 == "000" ~ "X-light-light",
        W3 == "001" ~ "X-light-heavy",
        W3 == "010" ~ "X-heavy-light",
        W3 == "011" ~ "X-heavy-heavy",
        W3 == "100" ~ "X-light-light",
        W3 == "101" ~ "X-light-heavy",
        W3 == "110" ~ "X-heavy-light",
        W3 == "111" ~ "X-heavy-heavy"),

      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "light-light-light-light",
        Wz == "0001" ~ "light-light-light-heavy",
        Wz == "0010" ~ "light-light-heavy-light",
        Wz == "0011" ~ "light-light-heavy-heavy",
        Wz == "0100" ~ "light-heavy-light-light",
        Wz == "0101" ~ "light-heavy-light-heavy",
        Wz == "0110" ~ "light-heavy-heavy-light",
        Wz == "0111" ~ "light-heavy-heavy-heavy",
        Wz == "1000" ~ "heavy-light-light-light",
        Wz == "1001" ~ "heavy-light-light-heavy",
        Wz == "1010" ~ "heavy-light-heavy-light",
        Wz == "1011" ~ "heavy-light-heavy-heavy",
        Wz == "1100" ~ "heavy-heavy-light-light",
        Wz == "1101" ~ "heavy-heavy-light-heavy",
        Wz == "1110" ~ "heavy-heavy-heavy-light",
        Wz == "1111" ~ "heavy-heavy-heavy-heavy")
    )%>% tibble::tibble()
  df2
}





kge_weigth_add16 = function(df, pattern=""){


  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1),
      w4 = ifelse(D3 == pattern, 0, 1)
          ) %>% as.data.frame()

  df00 = df0 %>%
    tidyr::unite(Wz , w1:w4, sep="",remove = FALSE )%>%
    tibble::tibble()



  df1 = df00 %>% tibble::tibble()%>%
    mutate(
      # w1f = ifelse(w1 == 0, "light", "heavy"),
      # w2f = ifelse(w2 == 0, "light", "heavy"),
      # w3f = ifelse(w3 == 0, "light", "heavy"),
      w4f = ifelse(w4 == 0, "light", "heavy")
    ) %>% as.data.frame() %>%
    mutate_at(c(
      # "w1f", "w2f","w3f",
      "w4f" ), as.factor)


  df2 = df1%>%
    dplyr::mutate(
      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "light-light-light-light",
        Wz == "0001" ~ "light-light-light-heavy",
        Wz == "0010" ~ "light-light-heavy-light",
        Wz == "0011" ~ "light-light-heavy-heavy",
        Wz == "0100" ~ "light-heavy-light-light",
        Wz == "0101" ~ "light-heavy-light-heavy",
        Wz == "0110" ~ "light-heavy-heavy-light",
        Wz == "0111" ~ "light-heavy-heavy-heavy",
        Wz == "1000" ~ "heavy-light-light-light",
        Wz == "1001" ~ "heavy-light-light-heavy",
        Wz == "1010" ~ "heavy-light-heavy-light",
        Wz == "1011" ~ "heavy-light-heavy-heavy",
        Wz == "1100" ~ "heavy-heavy-light-light",
        Wz == "1101" ~ "heavy-heavy-light-heavy",
        Wz == "1110" ~ "heavy-heavy-heavy-light",
        Wz == "1111" ~ "heavy-heavy-heavy-heavy")  )%>%
        tibble::tibble()
  df2


}











# # library(tidyverse)
# wd2 <- data.frame(
#   id =  paste0("BY", 1:5),
#   word = c("토끼", "휘파람", "뻥", "우리냥","우리낙징"))
# wd2 %>%class()
# wd2 %>% split_kw_df_match("word")

# wd2 %>% jjstat::split_kw_df_match("word", type="res")
# wd2%>%split_kw_df_match("word", type="all")

# kge0%>% as.data.frame()%>% split_kw_df_match("word", type="res") %>%head()

format_number <- function(number, n1=8, n2=3, n3=5) {
  # Use scientific notation when there are more than 18 decimal places
  if (nchar(sub("\\d+\\.", "", as.character(number))) >= n1) {
    return(format(number, scientific = TRUE, digits = n2))
  } else {
    return(format(number, scientific = FALSE, digits = n3))
  }
}


all_na_zero <- function(df) {
  # NA 값을 0으로 바꾸기
  df = as.data.frame(df)
  df[is.na(df)] <- 0
  return(df)
}


#데이터 대체
replace_df = function(df, pattern="", imp=NA ){

    df[df == pattern] <- imp

  df
}



# Create a function to replace NA with an empty string in an R vector data
Replace <- function(vector, pattern=NA, imp="") {
  # Use the replace function to replace NA with an empty string
  return(replace(vector, vector == pattern, imp))
}


# Replace(c("A", NA, "B", "", "C", "C"), "C", imp="T")







#필요할 때마다 성조패턴 적용 -------------
auto_pattern = function(data,
                        type = NULL
                        ){
  if(is.null(type)){
    stop("type= 성조변형 패턴을 위한 언어타입을 써주세요,
         고유어1, 고유어2, 고유어3, 외래어2, 외래어3,외래어4,
         가성어2, 가상어3")
  }

  # 고유어1  = c( "H(H)_1  --> H(H)")
  # 고유어2  = c(pattern = "LH(H)", imp ="LH")
  # 고유어3  = c(pattern = "LHH", imp ="LLH")
  # 외래어2  = c(pattern = "LH_f", imp ="LH")
  # 외래어3  = c(pattern = "LLH_f", imp ="LLH")
  # 외래어4  = c(pattern = "LLLH_f", imp ="LLLH")
  # 가상어2  = c(pattern = "", imp ="")
  # 가상어3  = c(pattern = "", imp ="")

  고유어1p  = replace_df(data, pattern = "H(H)_1", imp="H(H)")
  고유어2p  = replace_df(data, pattern = "LH(H)", imp ="LH")
  고유어3p  = replace_df(data, pattern = "LHH", imp ="LLH")
  외래어2p  = replace_df(data, pattern = "LH_f", imp ="LH")
  외래어3p  = replace_df(data, pattern = "LLH_f", imp ="LLH")
  외래어4p  = replace_df(data, pattern = "LLLH_f", imp ="LLLH")
  가상어2p  = replace_df(data, pattern = "", imp ="")
  가상어3p  = replace_df(data, pattern = "", imp ="")


  cat( paste("\n",type,"패턴 변경 완료 \n\n"))

  switch(type,
         고유어1 = 고유어1p,
         고유어2 = 고유어2p,
         고유어3 = 고유어3p,
         외래어2 = 외래어2p,
         외래어3 = 외래어3p,
         외래어4 = 외래어4p,
         가상어2 = 가상어2p,
         가상어3 = 가상어3p
  )
}

# kge_bind2 %>% filter(type =="고유어1") %>%
            # replace_df( pattern = 고유어1$pattern, 고유어1$imp )
# kge_bind2 %>% filter(type =="고유어1") %>% auto_pattern( "고유어1" )
# kge_bind2 %>% filter(type =="고유어2") %>% auto_pattern( "고유어2" )
#

#accent_pattern 성조치환에 대한 패턴 정리------
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
#보기
# accent_pattern %>% tibble()


# #분석자료를 데이터 프레임으로
kge_accent_table = function(data, Var1="a1", Var2="성조", trans = TRUE){


  res = data  %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    rename(accent = Var1) %>% tibble::column_to_rownames("accent")

  if(trans){res= res%>% t()
  }else{res}
  res
}







#두가지 행렬데이터를 결합해주는 함수 ------
combine_data <- function(observed_expected, p_value, left="", right="") {
  # Extract row and column names
  row_names <- rownames(observed_expected)
  col_names <- colnames(observed_expected)

  # Create a combined data frame
  combined <- data.frame(matrix("",
                                nrow = nrow(observed_expected),
                                ncol = ncol(observed_expected)))
  row.names(combined) <- row_names

  # Fill in combined data frame with observed/expected values and p-values
  for (i in 1:nrow(observed_expected)) {
    for (j in 1:ncol(observed_expected)) {
      value <- observed_expected[i, j]
      p_value_cell <- p_value[i, j]
      combined[i, j] <- paste0(value, left, format(p_value_cell, scientific = FALSE, digits = 5), right)
    }
  }

  # Assign column names
  colnames(combined) <- col_names

  return(combined)
}

# Example data
# observed_expected <- matrix(c(1.0587162, 1.1477347, 0.50877193,
#                               1.1269248, 0.5743712, 1.94957983,
#                               0.9163776, 1.4060606, 0.03515152,
#                               0.9530237, 0.8624314, 1.44531611),
#                             nrow = 4, byrow = TRUE,
#                             dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#                                             c("H", "H(H)", "L")))

# p_value <- matrix(c(0, 0.00001, 0.00000,
#                     0, 0.00000, 0.01497,
#                     0, 0.00013, 0.00000,
#                     0, 0.00000, 0.00102),
#                   nrow = 4, byrow = TRUE,
#                   dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#                                   c("H", "H(H)", "L")))

# # Combine the data
# combined_data <- combine_data(observed_expected, p_value)
# print(combined_data)


#행렬, 데이터프레임에 통계적 유의성을 출력해주는 함수 단순표현과 정확표현
add_significance_symbols <- function(p_value_table, simple=FALSE) {
  # Define significance levels
  significance_levels <- c(0.001, 0.01, 0.05)


  if(simple){
    # Convert p-values to significance symbols
    symbols <- matrix("", nrow = nrow(p_value_table), ncol = ncol(p_value_table))
    for (i in 1:nrow(p_value_table)) {
      for (j in 1:ncol(p_value_table)) {
        if (p_value_table[i, j] < significance_levels[1]) {
          symbols[i, j] <- "*"
        } else if (p_value_table[i, j] < significance_levels[2]) {
          symbols[i, j] <- "*"
        } else if (p_value_table[i, j] < significance_levels[3]) {
          symbols[i, j] <- "*"
        }
      }
    }
  }else{
    # Convert p-values to significance symbols
    symbols <- matrix("", nrow = nrow(p_value_table), ncol = ncol(p_value_table))
    for (i in 1:nrow(p_value_table)) {
      for (j in 1:ncol(p_value_table)) {
        if (p_value_table[i, j] < significance_levels[1]) {
          symbols[i, j] <- "***"
        } else if (p_value_table[i, j] < significance_levels[2]) {
          symbols[i, j] <- "**"
        } else if (p_value_table[i, j] < significance_levels[3]) {
          symbols[i, j] <- "*"
        }
      }
    }
  }


  # Combine symbols with p-values
  result <- p_value_table
  for (i in 1:nrow(p_value_table)) {
    for (j in 1:ncol(p_value_table)) {
      # result[i, j] <- paste0(format(p_value_table[i, j], scientific = FALSE, digits = 5), symbols[i, j])

      result[i, j] <- symbols[i, j]

    }
  }

  return(result)
}

# Add significance symbols
# p_value
# add_significance_symbols(p_value)

# # Example p-value table
# p_value <- matrix(c(0, 0.00001, 0.00000,
#                     0, 0.00000, 0.01497,
#                     0, 0.00013, 0.00000,
#                     0, 0.00000, 0.00102),
#                   nrow = 4, byrow = TRUE,
#                   dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#                                   c("H", "H(H)", "L")))


# Son, J. H., & Ito, C. (2016). The accent of Korean native nouns: North Gyeongsang compared to South Gyeongsang: North Gyeongsang compared to South Gyeongsang. 음성음운형태론연구, 22(3), 499-532.
#테이블의 계산법을 제샇ㅁ

calculate_chi_sig <- function(observed, type = "data", simple=FALSE) {
  # 기대값 계산
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  grand_total <- sum(observed)

  expected <- outer(row_totals, col_totals, "*") / grand_total

  # 카이제곱 통계량 계산
  chi_square_statistic <- sum((observed - expected)^2 / expected)

  # 자유도
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)

  # p-값 계산
  p_value <- pchisq(chi_square_statistic, df, lower.tail = FALSE)

  # 각 셀에 대한 p-값 반환
  cell_p_values <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
  for (i in 1:nrow(observed)) {
    for (j in 1:ncol(observed)) {
      cell_observed <- observed[i, j]
      if (cell_observed == 0) {
        cell_p_values[i, j] <- 1  # If observed is 0, set p-value to 1
      } else {

      cell_expected <- expected[i, j]
      cell_chi_square <- ((cell_observed - cell_expected)^2) / cell_expected
      cell_p_values[i, j] <- pchisq(cell_chi_square, df = 1, lower.tail = FALSE)
      }}

  }
  observed_over_expected_ratios = round(observed / expected, 3)



  colnames(cell_p_values) = colnames(observed)
  rownames(cell_p_values) = rownames(observed)

  p_sig = add_significance_symbols(cell_p_values, simple= simple)
  colnames(p_sig) = colnames(observed)
  rownames(p_sig) = rownames(observed)



  #처리한 값 :관측값과
  chi_combine  =  combine_data(observed, observed_over_expected_ratios, left = "(", right=")")
  # chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
  #처리한 값
  chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
  chi_sig2 =  format(combine_data(chi_combine, p_sig), 5)



  # cramers' v
  cramersv = cramers_v(observed)
  # chisq
  chisq = chisq.test(observed)

  Res = list(
    # chi_square_statistic = chi_square_statistic,
    #           degrees_of_freedom = df,
    #           p_value = p_value,
    chisq_test= chisq,
    cramersv= cramersv,
    observed_over_expected_ratios = observed_over_expected_ratios,
    cell_p_values = cell_p_values,
    p_sig = p_sig,
    chi_sig=chi_sig,
    chi_sig2=chi_sig2
    )

  switch(type,
         res= Res,
         all= Res,
         chisq_test= chisq,
         cramersv= cramersv,
         observed_over_expected_ratios = observed_over_expected_ratios,
         cell_p_values = cell_p_values,
         p_sig = p_sig,
         data = chi_sig,
         data2 = chi_sig2
         )

}

# # calculate_chi_sig(data)
# # Example data
# matrix(c(36, 67, 11,
#                  0, 35, 44,
#                  41, 108, 1,
#                  56, 87, 54), nrow = 4, byrow = TRUE,
#                dimnames = list(c("공명음", "마찰음",
#                                  "유기음_경음", "평파열음_평파찰음"),
#                                c("H", "H(H)", "L"))) %>%
# # calculate_chi_sig("data2")
# calculate_chi_sig("all")
#
# matrix(c(36, 67, 11,
#                  40, 35, 44,
#                  41, 108, 1,
#                  56, 87, 54), nrow = 4, byrow = TRUE,
#                dimnames = list(c("공명음", "마찰음",
#                                  "유기음_경음", "평파열음_평파찰음"),
#                                c("H", "H(H)", "L"))) %>%
# calculate_chi_sig(simple = TRUE)
# ##ref
# chisq.test(matrix(c(36, 67, 11,
#                       40, 35, 44,
#                       41, 108, 1,
#                       56, 87, 54), nrow = 4, byrow = TRUE,
#                     dimnames = list(c("공명음", "마찰음",
#                                       "유기음_경음", "평파열음_평파찰음"),
#                                     c("H", "H(H)", "L")))   )





#범주형 변수의 상관계수
cramers_v <- function(data) {
  # if (!requireNamespace("vcd", quietly = TRUE)) {
  #   install.packages("vcd")
  # }
  # library(vcd)

  # Calculate chi-square test for independence
  chi_square_test <- chisq.test(data)

  # Calculate Cramer's V
  n <- sum(data)
  num_rows <- nrow(data) - 1
  num_cols <- ncol(data) - 1
  phi <- sqrt(chi_square_test$statistic / n)
  v <- sqrt(phi / min(num_rows, num_cols))

  # Calculate p-value
  p_value <- pchisq(chi_square_test$statistic, df = chi_square_test$parameter, lower.tail = FALSE)

  res = cbind.data.frame(Cramer_V = v, p.value = p_value)

  res = res
  res
}

# matrix(c(36, 67, 11,
#          40, 35, 44,
#          41, 108, 1,
#          56, 87, 54), nrow = 4, byrow = TRUE,
#        dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#                        c("H", "H(H)", "L"))) %>%
#   cramers_v()

# # A tibble: 1 × 3
# Cramer_V  p.value sig
# <dbl>    <dbl> <chr>
# 0.441 1.01e-16 ***



perform_chi_square_test <-  function(observed, type = "data", simple=FALSE) {
  # 기대값 계산
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  grand_total <- sum(observed)

  expected <- outer(row_totals, col_totals, "*") / grand_total

  # 카이제곱 통계량 계산
  chi_square_statistic <- sum((observed - expected)^2 / expected)

  # 자유도
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)

  # p-값 계산
  p_value <- pchisq(chi_square_statistic, df, lower.tail = FALSE)

  # 각 셀에 대한 p-값 반환
  cell_p_values <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
  for (i in 1:nrow(observed)) {
    for (j in 1:ncol(observed)) {
      cell_observed <- observed[i, j]
      if (cell_observed == 0) {
        cell_p_values[i, j] <- 1  # If observed is 0, set p-value to 1
      } else {

        cell_expected <- expected[i, j]
        cell_chi_square <- ((cell_observed - cell_expected)^2) / cell_expected
        cell_p_values[i, j] <- pchisq(cell_chi_square, df = 1, lower.tail = FALSE)
      }}

  }
  observed_over_expected_ratios = round(observed / expected, 3)


  colnames(cell_p_values) = colnames(observed)
  rownames(cell_p_values) = rownames(observed)

  p_sig = add_significance_symbols(cell_p_values, simple= simple)
  colnames(p_sig) = colnames(observed)
  rownames(p_sig) = rownames(observed)



  #처리한 값 :관측값과
  chi_combine  =  combine_data(observed, observed_over_expected_ratios, left = "(", right=")")
  # chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
  #처리한 값
  chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
  chi_sig2 =  format(combine_data(chi_combine, p_sig), 5)



  # cramers' v
  cramersv = cramers_v(observed)
  # chisq
  chisq = chisq.test(observed)

  Res = list(
    # chi_square_statistic = chi_square_statistic,
    #           degrees_of_freedom = df,
    #           p_value = p_value,
    chisq_test= chisq,
    cramersv= cramersv,
    observed_over_expected_ratios = observed_over_expected_ratios,
    cell_p_values = cell_p_values,
    p_sig = p_sig,
    chi_sig=chi_sig,
    chi_sig2=chi_sig2
  )
  cat("\nSon, J. H., & Ito, C. (2016). The accent of Korean native nouns:
North Gyeongsang compared to South Gyeongsang: North Gyeongsang compared to South Gyeongsang. 음성음운형태론연구, 22(3), 499-532.
여기에서 observed/expected 값에 유의성을 chisq.test의 각 셀값으로 계산한 것으로 제시함 \n\n")


  switch(type,
         res= Res,
         all= Res,
         chisq_test= chisq,
         cramersv= cramersv,
         observed_over_expected_ratios = observed_over_expected_ratios,
         cell_p_values = cell_p_values,
         p_sig = p_sig,
         data = chi_sig,
         data2 = chi_sig2
  )

}




# #
# matrix(c(36, 67, 11,
#          40, 35, 44,
#          41, 108, 1,
#          56, 87, 54), nrow = 4, byrow = TRUE,
#        dimnames = list(c("공명음", "마찰음",
#                          "유기음_경음", "평파열음_평파찰음"),
#                        c("H", "H(H)", "L"))) %>%
#   perform_chi_square_test()
# matrix(c(36, 67, 11,
#          40, 35, 44,
#          41, 108, 1,
#          56, 87, 54), nrow = 4, byrow = TRUE,
#        dimnames = list(c("공명음", "마찰음",
#                          "유기음_경음", "평파열음_평파찰음"),
#                        c("H", "H(H)", "L"))) %>%
#   perform_chi_square_test("all")
# # #
# matrix(c(36, 67, 11,
#          40, 35, 44,
#          41, 108, 1,
#          56, 87, 54), nrow = 4, byrow = TRUE,
#        dimnames = list(c("공명음", "마찰음",
#                          "유기음_경음", "평파열음_평파찰음"),
#                        c("H", "H(H)", "L"))) %>%
#   perform_chi_square_test(type="res")


# Agresti, A. (2002). Categorical Data Analysis (2nd ed.). Wiley.
# Sokal, R. R., & Rohlf, F. J. (2012). Biometry: The Principles and Practice of Statistics in Biological Research (4th ed.). W. H. Freeman.








# https://search.r-project.org/CRAN/refmans/confintr/html/cramersv.html














#분석자료를 데이터 프레임으로-----
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


# observed table result ---------------
# observed table result
# observed table result -
obs_table = function(data, typesel = NULL, cex=1.3,plot=TRUE){
  data %>% filter(type==typesel) %>%
    auto_pattern(typesel)%>%
    select(성조, speaker) %>% table() %>%
    accent_table("speaker", "성조", typesel, cex=cex,  plot = plot)
}
# kge_bind2a %>% obs_table("고유어1")






# kge_bind2a %>% filter(N==1) %>%
# auto_pattern("고유어1")%>%
#   select(성조, speaker) %>% table() %>%
#   accent_table("speaker", "성조", "고유어1")
#
# kge_bind2a %>% filter(N==1) %>%
# auto_pattern("고유어1")%>%
#   select(성조, speaker) %>% table() %>%
#   accent_table("speaker", "성조", "고유어1", plot=FALSE)


# chisq table observed/Expected table
kge_chisq_table = function(data,
                           v1="a1",
                           v2="성조",
                           title ="Table",
                           type = "res2",
                           digits = 3,yadd=0.1,ncol=NULL,
                           trans = FALSE,
                           simple= FALSE, #유의성 종류
                           ko = TRUE)  #패턴그래프
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
  #observer/Expected 에 유의성 표시
  chi_table_sig = calculate_chi_sig(data, simple = simple)
  chi_table_sig2 = perform_chi_square_test(data, simple = simple, type = "data2")

  #유의성표시된 것으로 변경
  chi_table_md = chi_table_sig %>%
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
    graph = patternGraph1_ko(chi_table, raw = FALSE, yadd = yadd, ncol = ncol)
  }else{
    graph = patternGraph1(chi_table, raw = FALSE, yadd = yadd, ncol = ncol)
  }


  #최종울력
  result1 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test_overall = res,

    # chi_df = res_df,
    chisq_report_overall = res_report,

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
         chitable = chi_table,
         res1 = result,
         res2 = result1,
         res3 = result2)
}


 # chisq table observed/Expected table


# chisq table observed/Expected table
# kge_chisq_table = function(data,
#                            v1="a1",
#                            v2="성조",
#                            title ="Table",
#                            type = "res2",
#                            digits = 3,yadd=0.1,
#                            trans = FALSE)
# {
#
#   data =  data %>%
#     dplyr::select(all_of(v1), all_of(v2)) %>%
#     table()
#   # 최종결과 에 포함
#   data_margin = data %>% addmargins() %>%
#     accent_table( v1, v2, trans = trans)
#   #
#   #chisq.test
#   Onset_or_Coda_Accent_Contingency_table <- data
#   res = chisq.test(Onset_or_Coda_Accent_Contingency_table)
#   res_df = chisq.test(data)%>% broom::tidy()
#   res_report = chisq.test(data)%>% report::report()
#
#   chi_mag = paste0(" [chisq = ",round(res$statistic, digits),
#                    ", df = ",res$parameter,
#                    ", p = ", format_number(res$p.value, digits),"]" )
#   # res$statistic
#   # res$parameter
#   # res$p.value
#
#   if(nrow(data) != 1){
#     chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
#       tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
#       tibble::column_to_rownames(v1) %>%
#       Round(digits)
#
#
#     # g = chi_table %>%  patternGraph()
#
#
#
#   }else{
#
#     # chi_table = (res$observed / res$expected)
#     chi_table =
#       rbind(
#         observed = data %>%
#           accent_table( v1, v2, trans = trans),
#         expected = res$expected,
#         obs_expected_ratio = (res$observed / res$expected)
#       ) %>% Round(digits)
#
#     # g= NULL
#   }
#   #
#
#
#   chi_table_md = chi_table %>%
#     jjstat::markdown_table(caption = paste0(title,chi_mag),
#                    digits = digits,
#                    general = NULL)
#
#
#   # 결과를 정리하여 나타내는 값들
#   result = list(chisq_test = res,
#                 margin = data_margin %>%
#                   jjstat::markdown_table(caption = paste0(title,"Contingency table"),
#                                  general = NULL),
#                 chi_table_md,
#                 chi_table = chi_table)
#   result1 = list(
#     # msg=msg,
#     crosstable = data_margin,
#     data_margin %>%
#       jjstat::markdown_table(caption = paste0(title," Contingency table"),
#                      general = NULL),
#     chisq_test = res,
#     # chi_df = res_df,
#     chisq_report = res_report,
#     chi_table = chi_table ,
#     g = patternGraph1(chi_table,raw = FALSE, yadd = yadd),
#     chi_table_md)
#
#   result2 = list(
#     # msg=msg,
#     crosstable = data_margin,
#     data_margin %>%
#       jjstat::markdown_table(caption = paste0(title," Contingency table"),
#                      general = NULL),
#     chisq_test = res,
#     # chi_df = res_df,
#     chisq_report = res_report,
#     chi_table = chi_table ,
#     # g = patternGraph1(chi_table,raw = FALSE),
#     chi_table_md)
#
#
#   switch(type,
#          ct = data,
#          df = data.frame(data),
#          margin = data_margin,
#          chisq_test = res,
#          chisq_df = res_df,
#          chisq_report = res_report,
#          chitable = chi_table,
#          res1 = result,
#          res2 = result1,
#          res2 = result2)
# }
# kge_ko1
# kge_ko1 %>% chisq_table("a1","성조")

# #spearker pattern graph
# patternGraph = function(data){
#
#   data %>% data.frame %>%
#     rownames_to_column("accent") %>%
#     pivot_longer(names_to = "speaker", values_to = "freq", cols=2:5) %>%
#     ggplot(aes(x = accent, y = freq))+
#     geom_bar(stat = "identity", aes( fill = accent),
#              position = "dodge", show.legend = FALSE
#     )+
#     coord_flip()+
#     theme(axis.text = element_text(size=14),
#           axis.title = element_text(size=14),
#           strip.text = element_text(size=14)
#     )+
#     scale_fill_grey(start = 0, end = 0.7) +
#     # facet_wrap(~ accent)
#     facet_wrap(~ speaker)
# }



patternGraph = function(data,size=4,
                        strip=14,
                        text = 12,
                        axis= 14,
                        hjust=-0.4,
                        yadd = 10,
                        type="g",
                        show=FALSE,
                        tolong= FALSE
){
  if(tolong){
    data1 = data %>% data.frame %>%
      rownames_to_column("accent") %>%
      pivot_longer(names_to = "speaker", values_to = "freq", cols=2:5)
  }else{
    data1 = data
  }


  if(show){
    g =  data1%>%
      ggplot(aes(x = accent, y = freq))+
      geom_bar(stat = "identity", aes( fill = accent),
               position = "dodge", show.legend = FALSE)+
      geom_text(aes(label = freq), hjust =  hjust, size = size)+
      coord_flip()+
      ylim(0, max(data1$freq)+ yadd)+
      theme_bw()+
      theme(axis.text = element_text(size= text),
            axis.title = element_text(size= axis),
            strip.text = element_text(size= strip)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      # facet_wrap(~ accent)
      facet_wrap(~ speaker)

  }else{
    g =  data1%>%
      ggplot(aes(x = accent, y = freq))+
      geom_bar(stat = "identity", aes( fill = accent),
               position = "dodge", show.legend = FALSE)+
      # geom_text(aes(label = freq), hjust =  hjust, size = size)+
      coord_flip()+
      ylim(0, max(data1$freq)+ yadd)+
      theme_bw()+
      theme(axis.text = element_text(size= text),
            axis.title = element_text(size= axis),
            strip.text = element_text(size= strip)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      # facet_wrap(~ accent)
      facet_wrap(~ speaker)
  }
  switch(type,
         g = g,
         data = data1)
}



#longdata transfomation
long_df = function(data, names_to = "speaker",
                   values_to = "freq",
                   cols = 2:ncol(data1),
                   rowname ="accent"){

  # colnames0 = colnames(data)
  data1 = data %>% data.frame %>%
    rownames_to_column("accent")

  data2 <- data1%>%
    pivot_longer(names_to = "speaker",
                 values_to = "freq",
                 cols=cols)
  data2

}

#longdata transfomation
# long_df = function(data, cols=2:5){
#   data1 = data %>% data.frame %>%
#     rownames_to_column("accent") %>%
#     pivot_longer(names_to = "speaker",
#                  values_to = "freq",
#                  cols=cols)
#   data1
# }

#피험자 정보 데이터와결합
combind_person= function(data){
  load(file =" Kge_person.RData")

  data1=  data %>%long_df() %>%
    inner_join(Kge_person, by="speaker" ) %>%
    tidyr::unite(speaker , c(speaker, age, area))%>%
    arrange(speaker) %>%
    dplyr::select( name, gender,speaker,accent, freq)

  data1

}


### 음절별 성조의 패턴
### 음절별 성조의 패턴calculate_chi_sig

### 음절별 성조의 패턴 그래프 --------
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
      pivot_longer(names_to = "accent",
                   values_to = 'ratio',
                   cols = 2: (ncol(data1$chi_table)+1) )
  }else{
    data1 <- data

    data_long <- data1 %>% as.data.frame() %>%
      rownames_to_column("syllabic") %>%
      pivot_longer(names_to = "accent", values_to = 'ratio',
                   cols=2: (ncol(data1)+1) )

  }



  g = data_long%>% ggplot(aes(x = accent, y = ratio))+
    geom_bar(stat = "identity", aes( fill = accent),
             position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
    ylim(0,max(data_long$ratio)+ yadd)+
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







##한글변형 코드 ------------
patternGraph1_ko = function(data,
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

    data_long <- data1 %>% as.data.frame() %>%
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

#
#
# #저장
# aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#   auto_pattern("고유어1") %>%
#   kge_chisq_table("a1","성조", "고유어1 부산")
#
# #데이터저장
# data_long<- aaa$chi_table %>%
#   rownames_to_column("syllabic") %>%
#   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
# data_long
#
# data_long%>% ggplot(aes(x = accent, y = ratio))+
#   geom_bar(stat = "identity", aes( fill = accent),
#            position = "dodge", show.legend = FALSE)+
#   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#   ylim(0,max(data1$ratio)+0.07)+
#   coord_flip()+
#   theme_bw()+
#   theme(axis.text = element_text(size= 12),
#         axis.title = element_text(size= 12),
#         strip.text = element_text(size= 14)
#   )+
#   scale_fill_grey(start = 0, end = 0.7) +
#   facet_wrap(~ syllabic )
#

# aaa%>% patternGraph1()




### 음절별 성조의 패턴 그래프 --------
patternGraph2 = function(data,
                         type="data",
                         raw=TRUE,
                         ncol=NULL,
                         yadd = 0.2,
                         strip_size = 16,
                         axis_size = 15,
                         text_size = 13,
                         ko=TRUE
){

  if(ko){
    if(raw){
      data1 <- data
      data_long <- data1$chi_table %>%
        rownames_to_column("음절") %>%
        pivot_longer(names_to = "성조형",
                     values_to = '관측기대비율',
                     cols = 2: (ncol(data1$chi_table)+1) )
    }else{
      data1 <- data

      data_long <- data1 %>% as.data.frame() %>%
        rownames_to_column("음절") %>%
        pivot_longer(names_to = "성조형", values_to = '관측기대비율',
                     cols=2: (ncol(data1)+1) )

    }

    g = data_long%>% ggplot(aes(x = 음절, y = 관측기대비율))+
      geom_bar(stat = "identity", aes( fill = 음절),
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
      facet_wrap(~ 성조형 , ncol = ncol)




  }else{
  if(raw){
    data1 <- data
    data_long <- data1$chi_table %>%
      rownames_to_column("syllabic") %>%
       pivot_longer(names_to = "accent",
                   values_to = 'ratio',
                   cols = 2: (ncol(data1$chi_table)+1) )
  }else{
    data1 <- data

    data_long <- data1 %>% as.data.frame() %>%
      rownames_to_column("syllabic") %>%
      pivot_longer(names_to = "accent", values_to = 'ratio',
                   cols = 2: (ncol(data1)+1) )

  }

    g = data_long%>% ggplot(aes(x = syllabic, y = ratio))+
      geom_bar(stat = "identity", aes( fill = syllabic),
               position = "dodge", show.legend = FALSE)+
      geom_hline(yintercept = 1, linetype=2, color="gray80")+
      geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
      ylim(0,max(data_long$ratio)+ yadd)+
      coord_flip()+
      theme_bw()+
      theme(axis.text = element_text(size= text_size),
            axis.title = element_text(size= axis_size),
            strip.text = element_text(size= strip_size)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      facet_wrap(~ accent , ncol = ncol)

  }
  res = list(data, data_long, g)
  res1 = list(g)

  switch(type, all= res, data=res1)
}







# 대응분석 함수 -----------------------------------------------------------------


ca_analysis = function(data, typekey= "", area="부산",
                       col = c("black","red","blue"),
                       arrows = c(F, T),
                       col.row = "gray30",
                       pointsize = 3,
                       labelsize = 5
){

  res0 <- data %>% filter(지역 == area & type == typekey) %>%
    select(성조,a1, a3) %>%
    unite(onset_coda, a1:a3, sep = " / ") %>%
    table() %>% t() %>%
    # ca::mjca()
    FactoMineR::CA()

  res1 = res0 %>% summary()
  # res3 = res0 %>% fviz_screeplot(addlabels = TRUE)

  explain =   round(res0$eig[1,2] + res0$eig[2,2],2)
  # res2 <- factoextra::fviz_ca_biplot(res1, repel = T, col.row ="black")
  # res2 <- plot(res0, arrows = arrows, col = c("black","red","blue"))
  res2 <- res0 %>%  factoextra::fviz_ca_biplot(
    repel = T,
    col.row = col.row ,
    col.col = "contrib",
    gradient.cols = c("red","tomato"),
    labelsize = labelsize,
    pointsize = pointsize,
    arrows = arrows,
    # map ="rowprincipal",
    map ="symbiplot",
    title = paste0(typekey,"onset + coda (총설명력: ",explain,"%)"))+
    theme_bw() + theme(legend.position = "none")

  res3 = res0 %>% factoextra::fviz_screeplot(addlabels = TRUE)

  res = list(res1, res3,  res2)
  res
}

# kge_bind2a %>% ca_analysis("고유어1")
# kge_bind2a %>% ca_analysis("고유어2")
# kge_bind2a %>% ca_analysis("고유어3")
# kge_bind2a %>% ca_analysis("외래어2")
# kge_bind2a %>% ca_analysis("외래어3")
# kge_bind2a %>% ca_analysis("외래어4")
# kge_bind2a %>% ca_analysis("가상어2")
# kge_bind2a %>% ca_analysis("가상어3")






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


# kge_bind2a %>% model_comparions("고유어2") #3
# kge_bind2a %>% model_comparions("고유어3") #3
# kge_bind2a %>% model_comparions("외래어2") #3
# kge_bind2a %>% model_comparions("외래어3") #4
# kge_bind2a %>% model_comparions("외래어4")
# kge_bind2a %>% model_comparions("가상어2")
# kge_bind2a %>% model_comparions("가상어3")
# kge_bind2a %>% model_comparions("고유어2")


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
# kge_bind2a %>% model_summary7()


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

                 lmerTest::lmer(weight ~ onset + coda + (1|onset:coda),
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


# #모뎔결과 전체 보기
# kge_bind2a %>% model_summary8()
#





# #Agree_table  ###############
# Agree_table = function(data,
#                        wordkey="",
#                        age1=60,
#                        age2=20,
#                        area1="부산",
#                        area2="부산",
#                        pattern = "H(H)_1", imp="H(H)",
#                        n=15,type="Res"
# ){
#   res= data %>%
#     replace_df(pattern = pattern, imp = imp) %>%
#     dplyr::filter(age == age1 & 지역== area1 &type==wordkey) %>%
#     dplyr::select(speaker,word, 성조, age, 지역) %>%
#     dplyr::arrange(speaker) %>%
#     dplyr::inner_join(
#       data %>%
#         replace_df(pattern = pattern, imp = imp)  %>%
#         dplyr::filter(age == age2 & 지역== area2  &type == wordkey)%>%
#         dplyr::select(speaker,  word, 성조, age, 지역) %>%
#         arrange(speaker) ,
#       by ="word", relationship = "many-to-many") %>%
#     mutate(same  = ifelse(성조.x == 성조.y, "Agree","Disagree")) #%>%
#   # rename(age60 = age.x, age20 = age.y, 성조60=성조.x, 성조20=성조.y)
#
#   res1= res%>%
#     dplyr::select(word, age.x, age.y, same) %>%
#     # dplyr::select(word, age.x, same) %>%
#     pivot_longer(names_to = "Age", values_to = "S", cols = age.x:age.y) %>%
#     dplyr::select(Age, same) %>% table() %>%
#     accent_table("Age","same") %>%
#     `colnames<-`(c(paste0(area1, age1,"대"),
#                    paste0(area2, age2,"대")))
#
#   Res = list(r1=res1 ,
#              r2=res1 %>%
#                jjstat::markdown_table(caption = paste0(wordkey,"에 관한",
#                                                area1, age1,"대와",
#                                                area2, age2, "대" ))
#              ,
#              head_top15 = res %>% head(n=n)
#   )
#
#   switch(type,
#          data = res1 %>% data.frame() %>%
#            dplyr::select(1),
#          # dplyr::select(paste0(area1, age1,"대")),
#
#          Res = Res)
# }


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
# kge_bind2%>% Agree_table("고유어1", pattern = "H(H)_1", imp="H(H)")
#
# Kge_accentRule%>% filter(type =="고유어2")
# kge_bind2 %>% Agree_table("고유어2", pattern = "LH(H)", imp ="LH" )





# 2개의 연령을 묶어서 분석
# bind_agree_tabe= function(data, wordkey,
#                           Age1,
#                           Age2,
#                           Area="부산",
#                           pattern = "H(H)_1", imp="H(H)"
#
# ){
#
#
#   res = bind_cols(Agree_table(data, wordkey,
#                               age1 = Age1, age2 = Age1,
#                               area1 = Area, area2 = Area ,
#                               pattern = pattern, imp = imp,
#                               type = "data") #%>%
#                   # `colnames<-`(c(paste0("",Area, Age1,"대")))
#                   ,
#
#                   Agree_table(data, wordkey,
#                               age1 = Age2, age2 = Age2,
#                               area1 = Area, area2 = Area ,
#                               pattern = pattern, imp = imp,
#                               type = "data") #%>%
#                   # `colnames<-`(c(paste0("",Area, Age2,"대")))
#   )
#
#   res = res %>% rownames_to_column("Agreement_rate")
#
#   res1 = res
#   colnames(res1)= c("Agreement_rate" ,"a1","a2")
#   res1 = res1 %>% mutate(ratio1 = paste0(round(a1/sum(a1),2)*100,"%"),
#                          ratio2 = paste0(round(a2/sum(a2),2)*100,"%")
#   ) %>%
#     mutate(
#       A1 = paste0(a1,"(",ratio1,")"),
#       A2 = paste0(a2,"(",ratio2,")")
#     ) %>%
#     dplyr::select(Agreement_rate, A1 ,A2) %>%
#     `colnames<-`(c("Agreement_rate",
#                    paste0("",Area, Age1,"대"),
#                    paste0("",Area, Age2,"대"))
#     )
#
#
#
#   RES = list(console = res ,
#              res1,
#              markdown = res1 %>%
#                jjstat::markdown_table(
#                  caption = paste0(wordkey,": ",Area, Age1,"대, ",
#                                   "",Area, Age2,"대")
#                ))
#   RES
# }
#
# # kge_bind2 %>%
# #   bind_agree_tabe("고유어1",20, 60, pattern = "H(H)_1", imp="H(H)")
#
#




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













ICC = function(lmedata){
  #random effect
  random_effect <- data.frame(lme4::VarCorr(lmedata))


  icc =  random_effect |>
    dplyr::mutate(Sum = sum(vcov),
                  ICC = (vcov/Sum),
                  ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                  ICC_rank = rank(desc(ICC))
    ) |>
    dplyr::select(1:2,7,8,9)
  icc
}

#  PRE(오차감소비율; proportional reduction in error 함수 in 혼합모형(다층모형)
PRE <- function(model1, model2){
  #proportional reduction in error
  var_cov_model1 <- data.frame(VarCorr(model1))
  var_cov_model2 <- data.frame(VarCorr(model2))
  #random_intercep
  pre_random_intercept = (var_cov_model1$vcov[1]-var_cov_model2$vcov[1])/var_cov_model1$vcov[1]
  #random_slope
  pre_random_slope = (var_cov_model1$vcov[2]-var_cov_model2$vcov[2])/var_cov_model1$vcov[2]

  pre_level1 = (var_cov_model1$vcov[4]-var_cov_model2$vcov[4])/var_cov_model1$vcov[4]

  res = data.frame(PRE_Intercept = pre_random_intercept,
                   PRE_Slope = pre_random_slope,
                   PRE_level1 = pre_level1 )
  res = res|>t() |>`colnames<-`(c("value"))
  # is.na(res)
  # NA data existed row is eliminate
  res = na.omit(res)|>data.frame()
  res$ratio = paste(round(res$value * 100,2),"%")

  data0 = data.frame( model_1 = c(var_cov_model1$vcov[1],
                                  var_cov_model1$vcov[2]),
                      model_2 = c(var_cov_model2$vcov[1], var_cov_model2$vcov[2]),
                      diff =c(var_cov_model1$vcov[1]-var_cov_model2$vcov[1],
                              var_cov_model1$vcov[2]-var_cov_model2$vcov[2])
  )

  Res  = cbind(RPE=c("PRE_Intercept","PRE_Slope"),data0, res)

  Res|>tibble()
}




#mixed effect
lme_report <- function(lmedata,
                       type = "basic",
                       form = "lmer",
                       apa=FALSE,
                       fit_more=FALSE,
                       ranef_sig = FALSE,
                       show.effect=FALSE,
                       show.ci=FALSE){

  library(multilevelTools)
  #formula output
  formula = lmedata@call

  #generate summary data
  lmedata_summary <- summary(lmedata)

  #fixed effect
  fixed_effect <- lmedata_summary$coefficients %>% p_mark_sig("Pr(>|t|)")

  #random effect
  random_effect <- data.frame(lme4::VarCorr(lmedata))

  if(show.effect){
    ranef = ranef(lmedata)
    fixef = fixef(lmedata)
    # prediction for each categories
    # fixef(lmedata) + ranef(lmedata)$operator
    coef = coef(lmedata)
  }else{
    ranef ="If you want to see the effect, show.effect = TRUE results."
    fixef="If you want to see the effect, show.effect = TRUE results."
    coef="If you want to see the effect, show.effect = TRUE results."
  }


  if(form=="glmer"){
    #ICC
    # pisqaure/3
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)

  }else if(form =="lmer"){
    #ICC
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)

  }else if(form == "poisson"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + 1,
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "pois"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + 1,
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "logit"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "dich"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }



  # test the variance parameter
  # APA style


  #Significance of random effects
  # H0: Var(random effect) (i.e., σ2)= 0
  # Ha: Var(random effect) (i.e., σ2) > 0
  if(ranef_sig){
    ranef_sig = RLRsim::exactRLRT(lmedata)
  }else{
    ranef_sig = "ranef_sig = TRUE -> perform random effect test "
  }

  #confidence interval
  if(show.ci){
    CI = confint(lmedata, oldNames = FALSE)
  }else{
    CI = "If you want to see 95%CI, set show.ci = TRUE"
  }

  #apa output
  if(apa){
    apa = lmedata |>
      JWileymisc::modelTest() |>
      JWileymisc::APAStyler()

  }else{
    apa = "If you want to see APA style result, set apa = TRUE"
  }

  # bind_cols(AIC(lmedata), BIC(lmedata))
  # p-value based on lmerTest
  anova_test = anova(lmedata)




  #model fit
  if(fit_more){
    fit = lmedata |> JWileymisc::modelPerformance()
  }else{
    fit = dplyr::bind_cols(AIC = AIC(lmedata), BIC= BIC(lmedata))
  }

  # result
  basic = list(formula = formula,
               Fixed_effect = fixed_effect,
               Random_effect = random_effect,
               ICC = icc,
               FIT = fit,
               APA = apa
  )



  res = list(formula = formula,
             Fixed_effect = fixed_effect,
             Random_effect = random_effect,
             ICC = icc,
             ranef_sig  = ranef_sig,
             FIT = fit,
             fixef = fixef,
             ranef = ranef,
             coef = coef,
             ConfidenceInterval_95 = CI,
             Satterthwaite_method = anova_test,
             APA = apa  )

  #full data view
  full = list(
    summary = lmedata_summary,
    formula = formula,
    Fixed_effect = fixed_effect,
    Random_effect = random_effect,
    ICC = icc,
    # ICC_glmer = icc_glmer,
    ranef_sig  = ranef_sig,
    FIT = fit,
    fixef = fixef,
    ranef = ranef,
    coef = coef,
    ConfidenceInterval_95 = CI,
    Satterthwaite_method = anova_test,
    APA = apa  )
  #select result
  switch(type,
         basic = basic,
         all = res,
         full = full, #full data
         summary = lmedata_summary, #lmer summary
         Fixed_effect = fixed_effect,
         Random_effect = random_effect,
         ICC = icc,
         ICC_glmer = icc_glmer,
         ranef_sig = ranef_sig,
         CI = CI,
         FIT = fit,
         ranef = ranef,
         fixef = fixef,
         anova = anova_test,
         APA = apa,
         formula =   formula
  )
}
