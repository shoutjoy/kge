
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



##한글변형 코드 ------------
patternGraph1_ko = function(data,
                         type="data",
                         raw=TRUE,
                         ncol=NULL,
                         yadd =0.09){

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
    theme(axis.text = element_text(size= 12),
          axis.title = element_text(size= 12),
          strip.text = element_text(size= 14)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ syllabic , ncol = ncol)


  res = list(data, data_long, g)
  res1 = list(g)

  switch(type, all= res, data=res1)
}
