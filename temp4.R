
# > 가상어3 부산 ---------------------------------------------------------------

# 가상어3 초성
kge_bind2a %>% filter(지역 =="부산" & type=="가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a1","성조", "가상어3 부산 onset")

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


