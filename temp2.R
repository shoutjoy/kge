


#* 가상어3 부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a1","성조", "가상어3 부산 초성")

# *가상어3 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a3","성조", "가상어3 부산 첫음절 종성 3(light,obs,son",  yadd =0.18)

# #가상어3 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("w1f","성조", "가상어3 부산 첫음절 종성 light/heavy")



#가상어3 weight
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("weigth_comb4","성조","가상어3 부산 종성 weight", ncol=4)

