# Read Library
pacman::p_load(tidyverse, 
               pwr, 
               hrbrthemes, 
               rlang, 
               gt, 
               patchwork, 
               tidymodels,
               lubridate)
# Set Working Directory
setwd("~/Documents/03_2022_Minacare/01_rawdata")

# 事前アンケート最終結果の読み込み
dat01 <-
  readxl::read_xlsx("survey_before_final.xlsx") 

dat01 %>% dim()
dat01 %>% skimr::skim()
dat01 %>% names()

# データ整形
## 先頭へ質問番号を付与
changecol <- function(x,y,z){
  dat01 %>%
    rename_at(c(x:y), ~str_c(z, ., sep = ""))
}
dat01 <- changecol(12, 18, "01_")
dat01 <- changecol(21, 28, "03_")
dat01 <- changecol(30, 37, "04_")
dat01 <- changecol(40, 49, "06_")
dat01 <- changecol(52, 60, "08_")
dat01 <- changecol(63, 67, "10_")
dat01 <- changecol(69, 73, "11_")
dat01 <- changecol(75, 78, "12_")
dat01 <- changecol(82, 87, "15_")
dat01 <- changecol(90, 102, "17_")
dat01 <- changecol(110, 115, "24_")
dat01 <- changecol(118, 130, "26_")
dat01 <- changecol(135, 139, "30_")
dat01 <- changecol(141, 145, "31_")
dat01 <- changecol(147, 150, "32_")
dat01 <- changecol(154, 163, "35_")
dat01 <- changecol(166, 177, "37_")
dat01 <- changecol(180, 191, "39_")
dat01 <- changecol(199, 213, "46_")
dat01 <- changecol(215, 229, "47_")
dat01 <- changecol(231, 233, "48_")

dat01 %>% names() %>% dput()


## 全角を半角へ
install.packages("stringi")
library(stringi)

dat02 <- 
  dat01 %>%
  setNames(stringi::stri_trans_nfkc(names(.)))
colnames(dat02) %>% dput()

## 先頭の"."を"_"へ変更
dat02 %>%
  rename_at(c(11,19,20,29,38,39,50,51,61), ~str_c("0",.,sep = "")) -> dat03
dat03 %>% 
  rename_all(~str_replace(.,"\\.", "_")) -> dat04
dat04 %>% names() %>% dput()
write_csv(dat04, "survey_before_arranged.csv")

## HLS-14部分の列名変更
colnamelist <-
  c(
    "h1_読めない漢字がある"   = "30_読めない漢字がある", 
    "h1_字が細かくて読みにくい"   = "30_字が細かくて読みにくい", 
    "h1_内容が難しくて分かりにくい"   = "30_内容が難しくて分かりにくい", 
    "h1_読むのに時間が掛かる"   = "30_読むのに時間が掛かる", 
    "h1_誰かに代わりに読んでもらうことがある"   = "30_誰かに代わりに読んでもらうことがある", 
    "h2_いろいろなところから情報を集めた"   = "31_いろいろなところから情報を集めた", 
    "h2_たくさんある情報から自分が求めるものを選び出した"   = "31_たくさんある情報から自分が求めるものを選び出した", 
    "h2_自分が見聞きした情報を理解できた"   = "31_自分が見聞きした情報を理解できた", 
    "h2_病気についての自分の意見や考えを医師や身近なひとに伝えた"   = "31_病気についての自分の意見や考えを医師や身近なひとに伝えた", 
    "h2_見聞きした情報をもとに実際に生活を変えてみた"   = "31_見聞きした情報をもとに実際に生活を変えてみた", 
    "h3_自分にもあてはまるかどうか考えた"   = "32_自分にもあてはまるかどうか考えた", 
    "h3_信頼性に疑問を持った"   = "32_信頼性に疑問を持った", 
    "h3_正しいかどうか聞いたり調べたりした"   = "32_正しいかどうか聞いたり調べたりした", 
    "h3_病院や治療法などを自分で決めるために調べた"  = "32_病院や治療法などを自分で決めるために調べた"
  )

## リテラシー列の数値化
h1_rep <-
 c("全くそう思わない" = "5",
"あまりそう思わない" = "4",
"どちらでもない" = "3",
"まあそう思う" = "2",
"強くそう思う" = "1")

h23_rep <-
  c("全くそう思わない" = "1",
    "あまりそう思わない" = "2",
    "どちらでもない" = "3",
    "まあそう思う" = "4",
    "強くそう思う" = "5")

dat04 %>% select(starts_with("h1_")) %>% names() %>% dput() -> h1_family
dat04 %>% select(starts_with(c("h2_","h3_"))) %>% names() %>% dput() -> h23_family

dat04 <- dat04 %>% rename(!!! colnamelist)　# Literacyに関わる列の列名の変更
dat04 %>% names() %>% dput()

## リテラシー列の数値化
dat04 %>% mutate_at(
  h1_family,
  funs(str_replace_all(.,h1_rep))) -> dat05

dat05 %>% mutate_at(
  h23_family,
  funs(str_replace_all(.,h23_rep))) -> dat05
dat05 %>% write_csv("survey_before_arranged_02.csv")

dat_01 <- read_csv("survey_before_arranged_02.csv")
dat_01 %>% select(starts_with(c("h1","h2","h3"))) %>% names() %>% dput() -> list_h
dat_01 %>% mutate(across(all_of(list_h), as.numeric))


## Add cols for total health literacy
sum_h <- function(n){
  rowSums(across(starts_with(n)), na.rm = TRUE) }
dat_02 <- dat_01 %>%
  mutate(total_h1 = sum_h("h1"), 
         total_h2 = sum_h("h2"), 
         total_h3 = sum_h("h3"), 
         total_h4 = sum_h("total")) 
dat_02 %>% rename("personal_id" = "{personal_id}") -> dat_02
dat_02 %>% write_csv("survey_before_arranged_02.csv")

dat_02 <- read_csv("survey_before_arranged_02.csv")


# DWHから出力した保険者ごとのminacareIDデータ の整形
## データの読み込み
datm <- read.csv("mcidmaster.csv", fileEncoding = "shift-jis")
datm %>% mutate(`生年月日` = as.Date(.$生年月日)) -> datm

## 年齢算出＆列追加
library(lubridate)
datm %>%
  mutate("today" = Sys.Date(),
         "age" = year(today) - year(`生年月日`)) %>%
  select(1,2,3,6) -> datm


# 性別年齢との結合、及びpersonal id重複行削除
dat_10 <- 
  datm %>% left_join(., dat_02, by = "personal_id") 
dat_10 %>% write_csv("survey_before_arranged_03.csv")

setwd("/Users/kawaiyuusei/Documents/03_2022_Minacare/01_rawdata")
dat_10 <- read_csv("survey_before_arranged_03.csv")
dat_10 %>%
  mutate(`Last updated on` = as_datetime(.$`Last updated on`)) -> dat_10

## personalid を lastupateが早いものだけ残して後削除
dat_10 %>%
  group_by(personal_id) %>%
  mutate(updatetime = max(`Last updated on`)) %>%
  mutate(tf = case_when(`Last updated on` == updatetime ~TRUE)) %>%
  drop_na(tf) -> dat_11
dat_11 %>% write_csv("survey_before_arranged_05.csv")


##########
dat_10 %>%
  distinct(personal_id, .keep_all = TRUE) -> dat_11
dat_11 %>% select(personal_id) %>% group_by(personal_id) %>% dim()
dat_11 %>% write_csv("survey_before_arranged_04.csv")
##########


########## 
# 重複行の確認
dat_10 %>% group_by(personal_id) %>% filter(n()>1) %>% ungroup() %>% 
  select(`Last updated on`) %>% pull() %>% dput()

%>% unique() %>% table() %>% data.frame()
##########

dat_11 <-
  read_csv("survey_before_arranged_03.csv")
dat_11 %>% names() %>% dput()
dat_11 %>% filter(`男女区分コード` %in% c("男性","女性")) -> dat_11
dat_11 %>% names() %>% dput()

## 性別不明を除いたもののCSV出力
dat_11 %>% write_csv("survey_before_arranged_06.csv")
dat_11 <- read_csv("survey_before_arranged_06.csv")

library(openxlsx)
dat_11 %>% names %>% tibble() %>%
write.xlsx("df_table_ccpj3_before.xlsx")
