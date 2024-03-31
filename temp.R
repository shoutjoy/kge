
library(lme4)
library(lmerTest)
require(lattice)


#data check

# xyplot(Freq ~ a1 | 성조, chisq_table(kge_ko1, "a1","성조", type = "df"))

dotplot(Freq ~ a1 | 성조, chisq_table(kge_ko1, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_ko1, "a1","성조", type = "df"), xlab="accent")

dotplot(Freq ~ a1 | 성조, chisq_table(kge_ko2, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_ko2, "a1","성조", type = "df"), xlab="accent")

dotplot(Freq ~ a1 | 성조, chisq_table(kge_ko3, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_ko3, "a1","성조", type = "df"), xlab="accent")

dotplot(Freq ~ a1 | 성조, chisq_table(kge_fo2, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_fo2, "a1","성조", type = "df"), xlab="accent")

dotplot(Freq ~ a1 | 성조, chisq_table(kge_fo3, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_fo3, "a1","성조", type = "df"), xlab="accent")

dotplot(Freq ~ a1 | 성조, chisq_table(kge_fo4, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_fo4, "a1","성조", type = "df"), xlab="accent")

dotplot(Freq ~ a1 | 성조, chisq_table(kge_vi2, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_vi2, "a1","성조", type = "df"), xlab="accent")

dotplot(Freq ~ a1 | 성조, chisq_table(kge_vi3, "a1","성조", type = "df"), xlab="onset")
dotplot(Freq ~ 성조 | a1, chisq_table(kge_vi3, "a1","성조", type = "df"), xlab="accent")

# ?lattice::dotplot
chisq_table(KGEbind, "a1","성조", type = "df") %>%
  ggplot(aes(x=a1, y=Freq))+
  geom_point(aes(fill=성조, col=성조), size=3)+
  theme_bw()+
  facet_wrap(~ 성조)+
  theme(strip.text = element_text(size=14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size=14, angle=90))


chisq_table(kge_ko1, "a1","성조", type = "df") %>%
  ggplot(aes(x=a1, y=Freq))+
  geom_point(aes(fill=성조, col=성조), size=3)+
  theme_bw()+
  facet_wrap(~ 성조)+
  theme(strip.text = element_text(size=14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size=14))


chisq_table(kge_ko1, "a1","성조", type = "df")


#Mixed model
#성조의 절편효과 모형 기본 모형
ko1_m1 = lmer(Freq ~ 1 + (1|성조) ,
              data = chisq_table(kge_ko1, "a1","성조", type = "df") )

ko1_m1%>% summary()


#급내상관계수
ko1_m1 %>% ICC()   # 3.5%
ko1_m1 %>% lme_report()   # 3.5%


#onset의 절편효과 모형
ko1_m2 = lmerTest::lmer(Freq ~ 1 + (1|a1) ,
                        data = kge_ko1 %>%
                          chisq_table("a1","성조", type = "df") )
ko1_m2%>% summary()
#급내상관계수
ko1_m2 %>% ICC()

#onset과 accent의 절편효과모형
ko1_m3 = lmer(Freq ~ 1 + (1|성조) +(1|a1) ,
              data = kge_ko1 %>%
                chisq_table("a1","성조", type = "df") )
ko1_m3%>% summary()
ko1_m3%>% ICC()
#급내상관계수
# ?SEM212::lme_report
ko1_m3 %>% SEM212::lme_report(fit_more = T, show.ci = T)

bind_lme_model(ko1_m1, ko1_m2, ko1_m3)


##
ko1_m4 = lmer(Freq ~ a1 + (1|a1) ,
              data = kge_ko1 %>%
                chisq_table("a1","성조", type = "df") )
ko1_m4%>% summary()


# sig ***
ko1_m5 = lmer(Freq ~ a1 + (1|성조) ,
              data = kge_ko1 %>%
                chisq_table("a1","성조", type = "df") )
ko1_m5%>% summary()
ko1_m5%>% bind_lme_model()


ko1_m6 = lmer(Freq ~ a1 + (1|성조) + (1|a1) ,
              data = kge_ko1 %>%
                chisq_table("a1","성조", type = "df") )
ko1_m6%>% summary()
# ko1_m6%>% bind_lme_model()






ko1_m7 = lmer(Freq ~ 성조 + (1|성조) ,
              data = kge_ko1 %>%
                chisq_table("a1","성조", type = "df") )
ko1_m7%>% summary()


ko1_m8 = lmer(Freq ~ 성조 + (1|a1),
              data = kge_ko1 %>%
                chisq_table("a1","성조", type = "df") )
ko1_m8%>% summary()





##sig 적합 모형 ----
ko1_m9 = lmer(Freq ~ a1 + 성조 + (1|성조) ,
              data = kge_ko1 %>%
                chisq_table("a1","성조", type = "df") )
ko1_m9%>% summary()
ko1_m9%>% ICC()
ko1_m9%>% bind_lme_model()


ko1_m10 = lmer(Freq ~ a1 + 성조 + (1|a1),
               data = kge_ko1 %>%
                 chisq_table("a1","성조", type = "df") )
ko1_m10%>% summary()



#onset의 기울기 효과 효과없음.
ko1_m11 = lmer(Freq ~ a1 + 성조 + (1|a1) + (1|성조),
               data = kge_ko1 %>%
                 chisq_table("a1","성조", type = "df") )
ko1_m11%>% summary()
# ko1_m7%>% bind_lme_model()

#onset의 기울기 효과
ko1_m12 = lmer(Freq ~ a1 + 성조 + (1|a1) + (1|성조),
               data = kge_ko1 %>%
                 chisq_table("a1","성조", type = "df") )
ko1_m12%>% summary()






PRE(ko1_m1, ko1_m2)

bind_lme_model(ko1_m1,
               ko1_m2,
               ko1_m5,
               ko1_m9
)
#ko1_m9 적합 모형




lmer(Freq ~ a1 + 성조 + (1|성조) ,
     data = KGEbind %>% #rename(onset = a1, accent= 성조) %>%
       chisq_table("a1","성조", type = "df") ) %>% summary()

lmer(Freq ~ a1 + 성조 + (1|성조) ,
     data = KGEbind %>% #rename(onset = a1, accent= 성조) %>%
       chisq_table("a1","성조", type = "df") ) %>%
  bind_lme_model()


# setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/")


# kge_ko1%>% as.data.frame() %>%split_kw_df_match()


# table(kge_ko1$성조, kge_ko1$a1) %>% chisq.test() %>% broom::tidy()[[1]]
#
#
# ko1_chisq <- table(kge_ko1$성조, kge_ko1$a1) %>% chisq.test()
# ko1_chisq$expected
# ko1_chisq$observed
# ko1_chisq %>% str()
#
#
# kge_ko1 %>% select(a1,성조) %>% table() %>%
#   addmargins() %>%
# accent_table(trans = F)
#
# kge_ko1 %>% select(a1,성조) %>% table()



#
# ko1_chisq$observed/ ko1_chisq$expected %>% as.matrix() %>%data.frame() %>%markdown_table()
#   pivot_wider(names_from =성조, values_from = Freq) %>%
#   rename(accent = a1) %>% tibble::column_to_rownames("accent")
#
#
# table(kge_ko1$성조, kge_ko1$a1) %>% prop.table() %>% round(2)
# table(kge_ko1$성조, kge_ko1$a1) %>% addmargins()
#
# table(kge_ko1$a1, kge_ko1$성조) %>%t()%>% as.matrix() %>%data.frame() %>%
#   pivot_wider(names_from = Var2, values_from = Freq) %>%
#   rename(accent = Var1) %>% tibble::column_to_rownames("accent")
