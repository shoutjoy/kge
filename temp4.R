


#한국어 대응분석용 데이터 생성  -----
kge_weigth_add_ca = function(df001, type= "res", pattern ="", remove= FALSE){

  # df0 = df %>% tibble::tibble()%>%
  #   mutate(
  #     w1 = ifelse(A3 == pattern, 0, 1),
  #     w2 = ifelse(B3 == pattern, 0, 1),
  #     w3 = ifelse(C3 == pattern, 0, 1),
  #     w4 = ifelse(D3 == pattern, 0, 1)
  #   ) %>% as.data.frame()
  #
  # df00 = df0 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  #   tibble::tibble()
  #
  # # df001 = df00 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  # #   tibble::tibble()
  # df001= df00 %>% mutate(
  #   W3 = substr(Wz, 1, 3),
  #   W2 = substr(Wz, 1, 2)
  # )
  #

  df002 = df001%>% tibble::tibble()%>%
    mutate(
      w1fc = ifelse(w1 == 0, "개", "폐"),
      w2fc = ifelse(w2 == 0, "개", "폐"),
      w3fc = ifelse(w3 == 0, "개", "폐"),
      w4fc = ifelse(w4 == 0, "개", "폐"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), factor)


  #
  # df1 = df002 %>%
  #   dplyr::mutate(
  #     weight = dplyr::case_when(
  #       W3 == "000" ~ 1,
  #       W3 == "001" ~ 2,
  #       W3 == "010" ~ 3,
  #       W3 == "011" ~ 4,
  #       W3 == "100" ~ 5,
  #       W3 == "101" ~ 6,
  #       W3 == "110" ~ 7,
  #       W3 == "111" ~ 8)
  #   )%>% tibble::tibble()

  df2 = df002 %>%
    dplyr::mutate(
      weigth_c2 =  dplyr::case_when(
        W2 == "00" ~ "개-개",
        W2 == "01" ~ "개-폐",
        W2 == "10" ~ "폐-개",
        W2 == "11" ~ "폐-폐"),

      weigth_cx2 =  dplyr::case_when(
        W2 == "00" ~ "",
        W2 == "01" ~ "X-폐",
        W2 == "10" ~ "폐-X",
        W2 == "11" ~ ""),

      weigth_c3 = dplyr::case_when(
        W3 == "000" ~ "개-개-개",
        W3 == "001" ~ "개-개-폐",
        W3 == "010" ~ "개-폐-개",
        W3 == "011" ~ "개-폐-폐",
        W3 == "100" ~ "폐-개-개",
        W3 == "101" ~ "폐-개-폐",
        W3 == "110" ~ "폐-폐-개",
        W3 == "111" ~ "폐-폐-폐"),

      weigth_cx3 = dplyr::case_when(
        W3 == "000" ~ "개-X-개",
        W3 == "001" ~ "개-X-폐",
        W3 == "010" ~ "개-X-개",
        W3 == "011" ~ "개-X-폐",
        W3 == "100" ~ "폐-X-개",
        W3 == "101" ~ "폐-X-폐",
        W3 == "110" ~ "폐-X-개",
        W3 == "111" ~ "폐-X-폐"),


      weigth_cy3 = dplyr::case_when(
        W3 == "000" ~ "X-개-개",
        W3 == "001" ~ "X-개-폐",
        W3 == "010" ~ "X-폐-개",
        W3 == "011" ~ "X-폐-폐",
        W3 == "100" ~ "X-개-개",
        W3 == "101" ~ "X-개-폐",
        W3 == "110" ~ "X-폐-개",
        W3 == "111" ~ "X-폐-폐"),



      weigth_c4 = dplyr::case_when(
        Wz == "0000" ~ "개-개-개-개",
        Wz == "0001" ~ "개-개-개-폐",
        Wz == "0010" ~ "개-개-폐-개",
        Wz == "0011" ~ "개-개-폐-폐",
        Wz == "0100" ~ "개-폐-개-개",
        Wz == "0101" ~ "개-폐-개-폐",
        Wz == "0110" ~ "개-폐-폐-개",
        Wz == "0111" ~ "개-폐-폐-폐",
        Wz == "1000" ~ "폐-개-개-개",
        Wz == "1001" ~ "폐-개-개-폐",
        Wz == "1010" ~ "폐-개-폐-개",
        Wz == "1011" ~ "폐-개-폐-폐",
        Wz == "1100" ~ "폐-폐-개-개",
        Wz == "1101" ~ "폐-폐-개-폐",
        Wz == "1110" ~ "폐-폐-폐-개",
        Wz == "1111" ~ "폐-폐-폐-폐"),

      weigth_cx4 = dplyr::case_when(
        Wz == "0000" ~ "개-X-X-개",
        Wz == "0001" ~ "개-X-X-폐",
        Wz == "0010" ~ "개-X-X-개",
        Wz == "0011" ~ "개-X-X-폐",
        Wz == "0100" ~ "개-X-X-개",
        Wz == "0101" ~ "개-X-X-폐",
        Wz == "0110" ~ "개-X-X-개",
        Wz == "0111" ~ "개-X-X-폐",
        Wz == "1000" ~ "폐-X-X-개",
        Wz == "1001" ~ "폐-X-X-폐",
        Wz == "1010" ~ "폐-X-X-개",
        Wz == "1011" ~ "폐-X-X-폐",
        Wz == "1100" ~ "폐-X-X-개",
        Wz == "1101" ~ "폐-X-X-폐",
        Wz == "1110" ~ "폐-X-X-개",
        Wz == "1111" ~ "폐-X-X-폐")


    )%>% tibble::tibble()


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}

