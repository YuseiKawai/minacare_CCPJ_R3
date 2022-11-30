# 01_democratic data
## 性年齢別
dat_11 %>%
  group_nest(`保険者名`, `男女区分コード`) %>% 
  mutate("平均年齢" = map_dbl(data, ~mean(.$age, na.rm=TRUE)),
        n = map_dbl(data, nrow))%>%
  select(1,2,4,5) %>%
  pivot_wider(names_from = "男女区分コード", values_from = c( "平均年齢",n)) %>%
  rowwise() %>% 
  mutate(`平均年齢_全体` = mean(c_across(starts_with("平均年齢"))),
         `n_全体` = sum(c_across(starts_with("n")))) %>%
  relocate("平均年齢_全体", .before = "n_女性") -> table_dm

table_a <- map_df(table_dm%>%select(1:4), mean)
table_b <- map_df(table_dm%>%select(5:7), sum)
table_c <- bind_cols(table_a,table_b)

table_c %>%
  mutate("保険者名" = as_character("保険者名")) %>%
  mutate("保険者名" = str_replace(
           .$`保険者名`,
           pattern = "保険者名",
           replacement = "保険者全体")
         ) %>% 
  bind_rows(table_dm,.) %>% 
  relocate("平均年齢_全体", .before = "n_女性") %>%
  rename_at(c(5:7), 
            ~str_replace(., pattern = "n", replacement = "参加者数")) -> table_sn

table_sn %>%
  gt() %>%
  fmt_number(
    columns = c(2:7),
    decimals = 0,
    use_seps = FALSE
  ) %>%
  tab_spanner_delim(
    delim = "_"
  )%>%
  tab_options(table.width = 650,
              column_labels.font.weight = "bold") %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "gray",
      weight = px(2)
    ),
    locations = 
      cells_body(
        columns = everything(),
        rows = 11
      )  )%>%
  tab_options(table_body.hlines.color = "white",
              table_body.border.bottom.color = "white",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white") 

## 年齢分布
dat_11 %>% filter(age <100) %>% 
  ggplot() +
  aes(x=age, fill=`男女区分コード`) +
  geom_histogram(stat = "count", bins = 50, na.rm = TRUE)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(x="年齢", y="人数", fill="性別")+
  scale_fill_grey(start = 0.4, end = 0.7)+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_02.png",
         width = 8, height = 4, bg = "transparent")


## 年代別の人数
dat_11$age %>% summary()
dat_11 %>% 
  mutate(age_group = case_when(
    .$age <= 30 ~ "20代",
    .$age >30&.$age<=40 ~ "30代",
    .$age > 40 & .$age <=50 ~"40代",
    .$age >50 ~"50代以上",
  )) -> dat_13

dat_13 %>% write_csv("survey_before_arranged_07.csv")

list.files(pattern = ".csv$")
dat_13 <-
  read_csv("survey_before_arranged_07.csv")


dat_13 %>% drop_na(age) %>%
  group_nest(`男女区分コード`, age_group) %>%
  mutate(n = map_dbl(data, nrow)) %>% select(1,2,4) %>%
  pivot_wider(names_from = `男女区分コード`, values_from = n) %>%
  gt() %>%
  cols_label(age_group = "年代") %>%
  tab_spanner(
    label = md("**参加者数**"),
    columns = c("男性", "女性")) %>%
  tab_options(table.width = 300,
              column_labels.font.weight = "bold",
              table_body.hlines.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white") 

# 02_主観的健康状態_Q
## 男女別の違い
dat_13 %>% 
  group_nest(.[,22],`男女区分コード` ) %>%
  mutate(n = map_dbl(data, nrow)) %>%
  select(1,2,4) %>%
  pivot_wider(names_from = `男女区分コード`, values_from = n) %>%
  rename("主観的健康状態"=1) -> table_01
  
table_01 %>%
  mutate(
    "比率_女性"= round(`女性`/sum(`女性`),digits = 2),
    "比率_男性"= round(`男性`/sum(`男性`),digits = 2)
  ) %>%
  arrange(
    match(
      `主観的健康状態`,
      c("よくない",
        "あまりよくない", 
        "ふつう", 
        "まあよい", 
        "よい"
      )
    )
  ) %>%
  gt() %>%
  tab_spanner(
    label = md("**人数**"),
    columns = c("女性","男性")) %>%
  tab_spanner_delim(
    delim = "_"
  ) %>%
  tab_options(table.width = 400,
              column_labels.font.weight = "bold",
              table_body.hlines.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")


## 主観的健康状態 vs 年齢
dat_13 %>%
  filter(age <100) %>%
  rename("主観的健康状態"=22) %>%
  ggplot() +
  aes(x=`主観的健康状態`, y=age) +
  geom_boxplot() +
  labs(title = "主観的健康状態vs年齢")+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_04.png",
         width = 8, height = 5, bg = "transparent")

dat_13 %>% group_by(.[,22]) %>% 
  summarise(mean = mean(age, na.rm = TRUE))

## 主観的健康状態 vs 保険者
dat_13 %>% group_by(`保険者名`, .[,22]) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = 2, values_from = n) %>%
  ungroup() %>%  
  select(c("保険者名",
           "よくない",
           "あまりよくない", 
           "ふつう", 
           "まあよい", 
           "よい"
  )) %>%
gt() %>%
  tab_header(title = md("**主観的健康状態vs保険者**")) %>%
  tab_options(table.width = 700,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white") 

## 主観的健康状態　vs プレゼンティーズム
dat_13 %>%
  rename("presentism" = 41,
         "healthstate" = 22) %>%
  ggplot(aes(x=factor(healthstate, levels = c(
    "よくない",
    "あまりよくない", 
    "ふつう", 
    "まあよい", 
    "よい"
  )),y=presentism))+
  geom_boxplot()+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(x="主権的健康状態",title = "プレゼンティーズムvs主観的健康状態") +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_07.png",
         width = 8, height = 5, bg = "transparent")


dat_13 %>%
  rename("presentism" = 41) %>%
  ggplot()+
  aes(x=`保険者名`,y=presentism) %>%
  geom_boxplot() +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(title = "プレゼンティーズムvs保険者")+
  coord_flip() +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_06.png",
         width = 8, height = 5, bg = "transparent")
  


# プレゼンティーズムスコア
## 参加者全員のヒストグラム
dat_13 %>%
  rename("presentism" = 41) %>%
  ggplot()+
  aes(x=presentism)+
  geom_histogram(bins = 20, color = "white")+
    theme_ipsum(base_family = "HiraKakuPro-W3")+
    labs(x="プレゼンティーズムスコア",title = "プレゼンティーズムスコアのヒストグラム")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_03.png",
         width = 8, height = 5, bg = "transparent")

## 男女別のboxplot
dat_13 %>%
  rename("presentism" = 41) %>%
  ggplot()+
  aes(x=`男女区分コード`, y = `presentism` )+
  geom_boxplot()+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(x="性別", y="プレゼンティーズムスコア")


## 男女別かつ保険者別
dat_13 %>%
  rename("presentism" = 41) %>%
  ggplot()+
  aes(x=`男女区分コード`, y = `presentism`, fill = `男女区分コード` )+
  geom_boxplot()+
  scale_fill_manual(values = c("gray40", "white")) +
  labs(title = "プレゼンティーズムvs男女（保険者別）")+
  facet_wrap(~`保険者名`, ncol = 5)+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "none")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_05.png",
         width = 10, height = 6, bg = "transparent")

  
## プレゼンティーズム vs 年代別
dat_13 %>%
  drop_na(age_group) %>%
  rename("presentism" = 41) %>%
  ggplot(aes(x=age_group,y=`presentism`))+
  geom_boxplot()+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(title="年代別プレゼンティーズムスコア")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_08.png",
         width = 8, height = 5, bg = "transparent")


# 月経セルフケア
## 月経セルフケアサマライズ
dat_13 %>%
  filter(`男女区分コード`=="女性") %>%
  select(starts_with("10")&where(is.character))-> table_wself

table_wself %>% names() %>% 
  str_replace(., "10_", "") %>%
  dput() -> namelist

table_wself %>% map(.,table) %>% 
  bind_rows() %>% 
  mutate(item = namelist) %>%
  relocate(any_of(c(
    "item",
    "あてはまる",
    "ややあてはまる",
    "ややあてはまらない",
    "あてはまらない"
  ))) %>%
  mutate_at(2:5, as.double) -> table_wself_2

table_wself_2 %>%
  pivot_longer(cols = c(2:5), names_to = "state", values_to = "n")%>%
  group_by(item) %>% mutate("ratio" = n/sum(n)*100) %>% 
  ggplot()+
  aes(x=item,y=ratio, fill=factor(state, levels=c(
    "あてはまらない",
    "ややあてはまらない",
    "ややあてはまる",
    "あてはまる"
  )))+
  geom_bar(stat = "identity",width = .7, alpha=.7) +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(fill = NULL, x=NULL,y="(%)", title = "月経セルフケア(割合)")+
  guides(fill = guide_legend(reverse = TRUE))+
  geom_text(aes(label = round(ratio, digits = 0)), colour = "black", 
            position = "stack", hjust = 1.8)+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="white", 
                                         size=.5, 
                                         linetype = "solid",
                                         color = "white"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=8))+
  scale_fill_brewer(palette = "Oranges")+
  coord_flip() +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_09.png",
         width = 10, height = 5, bg = "transparent")
  

# 女性の体に関する知識のサマライズ
dat_13 %>%
  filter(`男女区分コード`=="女性") %>%
  select(starts_with("11")&where(is.character)) -> table_wknow01
table_wknow01 %>% names() %>% str_replace("11_","") %>% dput() -> namelist2

table_wknow01 %>% map(.,table)%>% 
  bind_rows() %>% 
  mutate(item = namelist2) %>%
  relocate(any_of(c(
    "item",
    "あてはまる",
    "ややあてはまる",
    "ややあてはまらない",
    "あてはまらない"
  ))) %>%
  mutate_at(2:5, as.double) -> table_wknow02

table_wknow02 %>%
  pivot_longer(cols = c(2:5), names_to = "state", values_to = "n")%>%
  group_by(item) %>% mutate("ratio" = n/sum(n)*100) %>% 
  ggplot()+
  aes(x=item,y=ratio, fill=factor(state, levels=c(
    "あてはまらない",
    "ややあてはまらない",
    "ややあてはまる",
    "あてはまる"
  )))+
  geom_bar(stat = "identity",width = .7, alpha=.7) +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(fill = NULL, x=NULL,y="(%)", title = "体に関する知識（割合）")+
  guides(fill = guide_legend(reverse = TRUE))+
  geom_text(aes(label = round(ratio, digits = 0)), colour = "black", 
            position = "stack", hjust = 1.2)+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="white", 
                                         size=.5, 
                                         linetype = "solid",
                                         color = "white"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=8))+
  scale_fill_brewer(palette = "Oranges")+
  coord_flip()+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_10.png",
         width = 10, height = 5, bg = "transparent")


# 痩せに関する知識
dat_13 %>%
  select(age_group,starts_with("12")&where(is.character)) %>%
  group_nest(age_group) %>% na.omit() %>% pull(age_group) %>% dput() -> namelist3
  
dat_13 %>%
  select(age_group,starts_with("12")&where(is.character)) %>%
  group_nest(age_group) %>% drop_na(age_group) %>% 
  mutate(count = map(data, ~map(., table)))%>% 
  .$count %>% bind_rows() %>% mutate("年代" = namelist3)%>%
  rename_with(., ~str_replace(., "12_",""), starts_with("12_")) %>%
  relocate(5, .before = 1) %>%mutate_at(2:5, as.double) -> table_yase

dat_13 %>% 
  filter(`男女区分コード`=="女性") %>%
  group_nest(age_group) %>% mutate(n=map_dbl(data,nrow)) %>%
  drop_na(age_group) %>%  .$n %>% dput() -> numlist

table_yase <- mutate(table_yase, "人数" = numlist)
table_yase

table_yase %>% 
  pivot_longer(c(2:5), names_to = "item", values_to = "n") %>%
  group_by(`年代`) %>%
  mutate(ratio = n/`人数`*100) %>%
  ggplot(aes(x=年代,y=ratio,fill=item))+
  geom_col(position = "dodge", width = .5, alpha=.5)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_fill_ipsum() +
  labs(y="(%)", title = "痩せに関する理解度合", fill= NULL)+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_11.png",
         width = 10, height = 5, bg = "transparent")
  

# 更年期不調を感じている女性
## 年代別の更年期の人数
dat_13 %>% 
  rename("konenki" = 83) %>%
  filter(`男女区分コード`=="女性") %>% 
  group_nest(age_group) %>%
  mutate(count = map(data, ~table(.$konenki)))  %>% drop_na(age_group)%>%
  .$count %>% bind_cols() %>%
  setNames(c("20代","30代","40代","50代以上")) %>%
  mutate_all(as.double) %>%
  mutate("症状"= c(
    "以前症状はあったが今は特にない",
    "特に症状はない",
    "現在症状がある"
  )) %>% relocate(5, .before = 1) %>%
  gt() %>%
  tab_spanner(
    label = md("**人数**"),
    columns = c(2:5)) %>%
  tab_header(title = md("**更年期症状の有無（年代別）**")) %>%
  tab_options(table.width = 500,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")


##上記普通にこれでできた。。。。（涙
table(dat_13$age_group, dat_13$`14_更年期(閉経前後5年間)に現れる以下の症状について当てはまるものはありますか?`)%>%
  tibble()
  
## 更年期の総数
table(
dat_11 %>% filter(.$`男女区分コード`== "女性") %>% 
  select(83) 
) %>% 
  bind_rows() %>% tibble() %>% mutate_all(as.double) %>%
  pivot_longer(cols = c(1:3), names_to = "症状", values_to = "人数") %>% 
  mutate(ratio = round(`人数`/sum(`人数`), digits = 2)) %>% gt()

# 更年期の対処法に関する現状
dat_13 %>%
  filter(`男女区分コード`=="女性") %>% 
  select(starts_with("15")&where(is.character)) %>%
  map(., table) %>% bind_cols() %>%
  pivot_longer(c(1:6), names_to = "対処法", values_to = "人数") -> table_wt

table_wt %>% .$`対処法` %>% str_replace("15_","") %>% 
  str_split("_", simplify = TRUE) %>% .[,1] %>% dput() -> namelist4
table_wt %>% mutate("対処法"=namelist4) %>% 
  gt() %>%
  tab_header(title = md("**更年期不調時に実施した対処法**")) %>%
  tab_options(table.width = 500,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")


# 更年期の対処に対する分類
dat_13 %>%
  filter(`男女区分コード`=="女性") %>%
  group_nest(.[91]) %>%
  rename("state" = 1)%>%
  mutate("人数"=map_dbl(data,nrow)) %>%
  arrange(match(state, c("半年以上継続して行っている", 
                         "相談または受診を開始して半年未満",
                         "していないが1か月以内に相談または受診するつもり", 
                         "していないが6か月以内に相談または受診するつもり", 
                         "相談または受診を開始するつもりはない"))) %>%
  select(1,3) %>% gt() %>% 
  tab_header(title = md("**更年期不調を抱える女性の行動フェーズ分類**")) %>%
  tab_options(table.width = 500,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")


# なぜ更年期不調に対しての受療行動をしないのか
dat_13 %>% 
  rename("16_consult" = 91) %>% 
  filter(`男女区分コード`=="女性",
         .[,83]=="現在例に挙げられている症状がある",
        ) %>% 
  filter(`16_consult` %in% c("していないが1か月以内に相談または受診するつもり", 
                             "していないが6か月以内に相談または受診するつもり", 
                             "相談または受診を開始するつもりはない")) %>% 
  select(starts_with("17_")) %>%
  select(-1) %>%
  map_if(is.character, ~str_count(., "x") %>% sum(na.rm = TRUE)) %>%
  bind_rows() %>%
  pivot_longer(c(1:13), names_to = "item", values_to = "人数") %>%
  arrange(-`人数`) %>% 
  mutate(item = namelist5) %>% gt() %>%
  tab_header(title = md("**なぜ更年期不調に対して受療行動をとらないか？**")) %>%
  tab_options(table.width = 500,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")

# 更年期不調の際にどこの診療科へ行けば良いかの知識
dat_13 %>% 
  filter(`男女区分コード` == "女性") %>%
  group_nest(.[,108]) %>%
  mutate(n = map_dbl(data, nrow)) %>%
  select(1,3) %>%
  rename("選択肢" = 1) %>%
  drop_na(1) %>% 
  gt() %>% 
  tab_header(title = md("**更年期不調の際かかるべき診療科に関する知識**")) %>%
  tab_options(table.width = 400,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")
  

## 現在症状があり、かつプレゼンティーズムも低い女性
dat_13 %>% filter(
  `男女区分コード`=="女性",
  .[,83]=="現在例に挙げられている症状がある",
  .[,106] <= 6
) 

dat_13%>% select(111)

# health literacy score
## ヒストグラム
dat_13 %>% ggplot() +
  aes(x = total_h4, fill = `男女区分コード`) +
  geom_histogram(stat = "count", bins = 100)+
  labs(x = "ヘルスリテラシースコア") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_fill_grey(start = .3, end = .8) +
  labs(fill="性別",title = "参加者のヘルスリテラシー分布") +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_20.png",
         width = 8, height = 5, bg = "transparent")

## SD
dat_13$total_h4  %>% summary()


## 年齢vsリテラシー
dat_11 %>% filter(age <70) %>%
  ggplot() +
  aes(x = age, y = total_h4) +
  geom_point(color = "royalblue", alpha = 0.5, size = 3)+
  labs(y = "ヘルスリテラシースコア")+
  theme_ipsum(base_family = "HiraKakuPro-W3")

## 性別vsリテラシー
dat_11 %>%
  ggplot() +
  aes(x=`男女区分コード`,　y=total_h4)+
  geom_boxplot()+
  labs(title = "年齢vsヘルスリテラシー", y="ヘルスリテラシースコア",x=NULL)+
  theme_ipsum(base_family = "HiraKakuPro-W3")
dat_11 %>% group_nest(`男女区分コード`)　%>%
  mutate(mean_hl = map_dbl(data, ~mean(.$total_h4)))



## リテラシーは高くても健康行動はできていない
### リテラシーを高い低いに分類
dat_11$total_h4 %>% summary()
dat_13 %>%
  mutate(
    LH_Level = case_when(
      total_h4 >= 52 ~"high",
      total_h4 < 52 ~"low"
    )
  ) -> dat_13

### 更年期女性の中でリテラシーは高いが行動していない人の理由
dat_13 %>% 
  filter(`男女区分コード`=="女性",
         .[,83]=="現在例に挙げられている症状がある") %>%
  group_nest(LH_Level,.[,91]) %>%
  mutate(n = map_dbl(data,nrow)) %>%
  select(1,2,4)  %>%
  mutate(percent = n/sum(n)*100) %>% select(1,4,2)


dat_13 %>% 
  rename("16_consult" = 91) %>% 
  filter(`男女区分コード`=="女性",
         .[,83]=="現在例に挙げられている症状がある",
         LH_Level=="high") %>% 
  filter(`16_consult` %in% c("していないが1か月以内に相談または受診するつもり", 
                             "していないが6か月以内に相談または受診するつもり", 
                             "相談または受診を開始するつもりはない")) %>% 
  select(starts_with("17_")) %>%
  select(-1) %>%
  map_if(is.character, ~str_count(., "x") %>% sum(na.rm = TRUE)) %>%
  bind_rows() %>%
  pivot_longer(c(1:13), names_to = "item", values_to = "人数") %>%
  arrange(-`人数`) %>% 
  mutate(item = namelist5) %>% gt() 

namelist5 %>% str_split("_", simplify = TRUE) %>% .[,2] -> namelist5


# 更年期による生産性低下
## プレゼンティーズムの低下
table(dat_13[,106]) %>% bind_rows() %>%
  mutate_all(as.numeric) %>%
  pivot_longer(c(1:11), 
               names_to = "仕事パフォーマンス",
               values_to = "人数") %>%
  ggplot(aes(x=factor(`仕事パフォーマンス`,
                      levels = c(0,1,2,3,4,5,6,7,8,9,10)),y=`人数`))+
  geom_bar(stat = "identity",  fill="#FFAF42", width=.8, alpha=.8)+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(title = "更年期不調時の仕事パフォーマンスの変化（変化なし =10）",
       x="仕事パフォーマンス") +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_13.png",
         width = 10, height = 5, bg = "transparent")
  

###
table(dat_11[,106]) %>% names() %>% dput()-> list_wp
table(dat_11[,106]) %>% tibble() %>%
  mutate("仕事パフォーマンス" = list_wp) %>% 
  rename(c("人数" = ".")) %>% mutate_all(as.numeric) %>%
  mutate(n = `人数`*`仕事パフォーマンス`) -> table_wp

table_wp %>%
  ggplot() +aes(x=`仕事パフォーマンス`, y=`人数`)+
  geom_bar(stat = "identity", fill="#FFAF42", alpha=.8)+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(title = "更年期不調時の仕事パフォーマンス（変化なし =10）") +
  coord_flip()

sum(table_wp$n)/sum(table_wp$人数) # 平均6.327 

tibble(
  "仕事パフォーマンス" = c("変化なし","低下する"),
  "人数"　= c(61, (633-61))
) %>%
  ggplot() +aes(x=`仕事パフォーマンス`, y=`人数`)+
  geom_bar(stat = "identity", fill="#FFAF42")+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  coord_flip()


# 更年期による生産性低下日数（平均）
table(dat_11[,107])%>% names() %>% dput() -> list_seisan

table(dat_11[,107]) %>% tibble() %>% mutate(day = list_seisan) %>%
  mutate_all(as.numeric) %>%
  mutate(dayx = . * day) -> dat_seisan
sum(dat_seisan$dayx)/sum(dat_seisan$.)


# 更年期による生産性低下日数（ヒストグラム）
dat_13 %>% filter(`男女区分コード` == "女性")　%>% 
  select(107) %>%
  rename("day" = 1) %>% 
  ggplot() +
  aes(x=day) +
  geom_histogram(binwidth = 10,color = "white", fill = "#FFAF42")+
  labs(title = "更年期による生産性低下日数（女性）")+
  geom_vline(xintercept = median(dat_13[,107] %>% pull() %>% na.omit()), 
             color = "royalblue", 
             size = .5)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_14.png",
         width = 10, height = 5, bg = "transparent")

###
median(dat_13 %>%
         filter(`男女区分コード` == "女性") %>%
         select(107) %>% pull() %>% na.omit())
###


# 更年期による生産性日数低下（中央値）
dat_11 %>% group_nest(`男女区分コード`)　%>%
  mutate(median = map_dbl(data, ~median(.$`19_前問で、更年期や更年期障害によって仕事パフォーマンス(職務遂行能力)が落ちていると答えた方に質問です。過去1年間で何日程度パフォーマンスが落ちている日がありましたか? (1日 = 1, 365日 = 365)`, na.rm = TRUE)))

# 更年期の際にかかる診療科を知っているか？
dat_13 %>%
  filter(`男女区分コード`=="女性") %>%
  group_nest(.[,136])


# 男性更年期
## 男性更年期認知
dat_11 %>% filter(`男女区分コード` == "男性") %>% select(109) %>%
  table() %>% data.frame() %>%
  mutate("item" = "男性認知") %>%
  ggplot() +
  aes(x=item, y=Freq, fill=.)+
  geom_bar(stat = "identity", position = "fill") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_y_continuous(labels = percent) +
  coord_flip()
  
dat_11 %>% filter(`男女区分コード` == "男性")%>%
  group_nest(.[,109]) %>%
  mutate(n = map_dbl(data, nrow)) %>% select(1,3) %>% drop_na() %>% gt() -> dat_dk
497/(155+497)*100

## 男性更年期への対処法
dat_11 %>% filter(`男女区分コード` == "男性") %>%
  group_nest(.[,110]) %>% drop_na() %>%
  mutate(n = map_dbl(data, nrow)) %>% select(1,3)  %>% gt()

-> dat_dk2
dat_dk2 %>% mutate(item = "対処法知識の有無") %>%
  rename("知識"=1) %>%
  ggplot()+
  aes(x=item, y=n, fill=factor(`知識`,
                         levels = c(
                           "あてはまらない",
                           "あまりあてはまらない",
                           "ややあてはまる",
                           "あてはまる"
                         )))+
  geom_bar(stat = "identity", position = "fill")+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_y_continuous(labels = percent) +
  labs(title =md("男性更年期の対処法知識"),
         fill = NULL)+
  scale_fill_ipsum()+
  coord_flip() 

(265+253)/sum(dat_dk2$n)*100

# 男性更年期の症状
dat_13 %>% filter(`男女区分コード`=="男性") %>%
  group_nest(.[,111]) %>%
  mutate(n = map_dbl(data, nrow)) %>% select(1,3) %>% drop_na(1) %>%
  gt()

# 男性更年期症状があった際の対処法
dat_13 %>%
  filter(`男女区分コード`=="男性") %>% 
  select(starts_with("24")&where(is.character))%>%
  map(., table) %>% bind_cols() %>%
  pivot_longer(c(1:6), names_to = "対処法", values_to = "人数") -> table_mt

table_mt %>% .$`対処法` %>% str_replace("24_","") %>% 
  str_split("_", simplify = TRUE) %>% .[,1] %>% dput() -> namelist5

table_mt %>% mutate("対処法"=namelist5) %>% 
  gt() %>%
  tab_header(title = md("**男性更年期不調時に実施した対処法**")) %>%
  tab_options(table.width = 500,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")


# 男性更年期への受療行動の状況
dat_13 %>%
  filter(`男女区分コード`=="男性") %>%
  group_nest(.[119]) %>%
  rename("state" = 1)%>%
  mutate("人数"=map_dbl(data,nrow)) %>%
  arrange(match(state, c("半年以上継続して行っている", 
                         "相談または受診を開始して半年未満",
                         "していないが1か月以内に相談または受診するつもり", 
                         "していないが6か月以内に相談または受診するつもり", 
                         "相談または受診を開始するつもりはない"))) %>%
  select(1,3) %>% gt() %>% 
  tab_header(title = md("**更年期不調を抱える男性の行動フェーズ分類**")) %>%
  tab_options(table.width = 500,
              column_labels.font.weight = "bold",
              heading.border.bottom.color = "white",
              table_body.hlines.color = "white",
              table.border.top.color = "white",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white")


# 男性更年期による仕事パフォーマンス低下
presentism <- function(x,y,z,Z){
  dat_13 %>%
    filter({{x}} == y) %>%
    select(z) %>%
    table() %>% bind_rows() %>%
    mutate_all(as.numeric) %>%
    pivot_longer(c(1:11), 
               names_to = "仕事パフォーマンス",
               values_to = "人数") %>%
    ggplot(aes(x=factor(`仕事パフォーマンス`,
                        levels = c(0,1,2,3,4,5,6,7,8,9,10)),y=`人数`))+
    geom_bar(stat = "identity",  fill="#FFAF42", width=.8, alpha=.8)+
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    labs(title = Z,
         x="仕事パフォーマンス") 
}

presentism(男女区分コード, 
           "男性",
           134,
           "男性の更年期不調時の仕事パフォーマンス(変化なし=10)")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_21.png",
         width = 10, height = 5, bg = "transparent")


# 男性更年期による生産性日数低下
seisan <- function(x,y,z,Z){
  dat_13 %>% 
    filter({{x}} == y)　%>% 
    select(z) %>%
    rename("day" = 1) %>% 
    ggplot() +
    aes(x=day) +
    geom_histogram(binwidth = 10,color = "white", fill = "#FFAF42")+
    labs(title = Z)+
    geom_vline(xintercept = median(dat_13[,z] %>% pull() %>% na.omit()), 
               color = "royalblue", 
               size = .5)+
    theme_ipsum(base_family = "HiraKakuPro-W3")
}

seisan(男女区分コード,
       "男性",
       135,
       "更年期による生産性低下日数（男性）") +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/b_plot_22.png",
         width = 10, height = 5, bg = "transparent")


# 男性更年期についてどの診療科へ行けば良いか
dat_13 %>%
  filter(`男女区分コード`=="男性") %>%
  select(136) %>% table %>% bind_rows %>%
  pivot_longer(c(1:4), names_to = "選択肢", values_to = "n") %>% 
  gt() %>%
  tab_header(title = md("**更年期不調の際にかかるべき診療科に関する知識（男性）**")) %>%
  tab_options(table.width = 300)

# オンライン診療の利用度合い
dat_13[,155] %>% table() 


# 35_(女性限定)女性の健康に関することで、健康管理のためにあなたが実践していることを全てお答えください
dat_13 %>% 
  select(starts_with("35_")&where(is.character)) %>% map(., table) %>% bind_cols %>%
  rename_with(., ~str_replace(., "35_",""))%>%
  pivot_longer(cols = everything(), names_to = "実践活動", values_to = "n") %>%
  gt()
  
# 36_(女性限定)定期的に婦人科・産婦人科を受診していますか?
dat_13[,167] %>% table

# 38_企業や健保から案内される健診を受診していますか?
dat_13[,181] %>% table
  
# Health Check
## 検診を受けていない人の理由
library(tidytext)
dat_11 %>% filter(
  .$`38_企業や健保から案内される健診を受診していますか?` == "いいえ"
) %>%
  select(starts_with("39_")) -> dat_12


# 女性のための取り組み
## 不要な取り組み
dat_13[,225] %>% pull() %>% str_count("x") %>% sum(na.rm = TRUE)

torikumi <- function(x,y){
  dat_13 %>% 
    select(starts_with(x)&where(is.character)) %>%
    map_if(is.character, ~str_count(., "x") %>% sum(na.rm = TRUE)) %>%
    bind_rows() %>%
    rename_with(., ~str_replace(., pattern = x, replacement = "")) %>% 
    rename_with(., ~str_split(.,"_", simplify = TRUE)%>% .[,1]) %>%
    pivot_longer(cols = everything(), names_to = "item", values_to = "n") %>%
    arrange(desc(n)) %>%
    filter(!item =="特になし") %>%
    ggplot() +
    aes(x=reorder(item,n), y=n) +
    geom_bar(stat = "identity") +
    labs(x=NULL,title = y) +
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    coord_flip()
}

torikumi("47_", "不要な取り組み")

## 必要な取り組み
torikumi("46_", "有用な取り組み")


# 健康活動の実施具合
## 男女別の実施度合い
isiki <- function(x){
dat_11 %>% 
  group_nest(.[,x],`男女区分コード`) %>%
  mutate(n = map_dbl(data, nrow)) %>% 
  select(1,2,4) %>% 
  pivot_wider(names_from = `男女区分コード`, values_from = n) %>%
  mutate("%_女性" = round(.$女性/sum(.$女性)*100, digits = 2),
         "%_男性" = round(.$男性/sum(.$男性)*100,digits = 2)) %>% 
    gt() %>%
    tab_spanner(label = md("**人数**"),
                columns = c("女性","男性")) %>%
    tab_spanner_delim(delim = "_") %>%
    tab_options(table.width = 500,
                column_labels.font.weight = "bold",
                heading.border.bottom.color = "white",
                table_body.hlines.color = "white",
                table.border.top.color = "white",
                table_body.border.bottom.color = "black",
                column_labels.border.bottom.color = "black",
                column_labels.border.top.color = "white")
}
isiki(196)%>%tab_header(title = md("**健康行動の実施状況_運動**")) ##運動 
isiki(197)%>%tab_header(title = md("**健康行動の実施状況_食事**"))##食事
isiki(198)%>%tab_header(title = md("**健康行動の実施状況_サプリ**"))##サプリ
isiki(199)##うがい
isiki(200)%>%tab_header(title = md("**健康行動の実施状況_バイタル**"))##バイタル

table(dat_11$`41_運動に関して`, dat_11$`男女区分コード`) 

## 実施ステータスベースでのリテラシー評価
dat_11 %>% 
  ggplot()+
  aes(x=`42_食事に関して`, y=total_h4) +
  geom_boxplot()+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  coord_flip()


# 婦人科検診を定期的に受けてない人の理由
dat_11 %>% 
  filter(`男女区分コード` == "女性",
         .[,167] == "いいえ") %>%
  select(starts_with("37_")) %>% select(-1) %>%
  map_if(is.character, ~str_count(., "x") %>% sum(na.rm = TRUE))%>% 
  bind_rows() %>%
  pivot_longer(cols = c(1:12), names_to = "status", values_to = "n") %>%
  arrange(-.$n)


# 女性年齢による違い
dat_11 %>% filter(`男女区分コード` == "女性",
                  age < 100) %>%
  ggplot() +
  aes(x=age, y=total_h4) %>%
  geom_point()
