# 
dat01 <-
  readxl::read_xlsx("data_20221014.xlsx") 
dat01 %>% select(10, 38) -> dat01
dat01 %>% rename("personal_id" = "{personal_id}") -> dat01

dat01 %>% names()

# 九州電力データの整形
library(lubridate)
dat_kyuden <-
  read_csv("dat_kyuden.csv")
dat_kyuden %>%
  mutate("today" = Sys.Date(),
         "age" = year(today) - year(`生年月日`)) %>%
  select(1,2,5) -> dat_kyuden01



# join 
dat_kyuden01 %>% left_join(., dat01, by = "personal_id") -> dat_kyuden01
dat_kyuden01 %>% names() %>% dput()

rename_list <- 
  c("personal_id", 
    "sex", 
    "age", 
    "presence"
  )
setNames(dat_kyuden01, rename_list) -> dat_kyuden01
skimr::skim(dat_kyuden01)

table(dat_kyuden01$personal_id)


# デモクラデータ
## 男女人数と平均年齢
dat_kyuden01 %>% 
  group_nest(sex) %>% 
  mutate("人数" = map_dbl(data, nrow),
         "平均年齢" = map_dbl(data, ~mean(.$age)),
         "sex" = str_replace(.$sex, 
                             pattern = c("1", "2"),
                             replacement = c("男","女"))) %>%
  select(-2) %>%
  mutate("人数" = `人数`　-1) %>%
  gt() %>%
  fmt_number(
    columns = c("平均年齢"),
    decimals = 1,
    use_seps = FALSE
  ) %>%
  tab_options(table.width = 250)  %>%
  cols_label(sex = "性別" ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "gray",
      weight = px(2)
    ),
    locations = 
      cells_body(
        columns = everything(),
        rows = 1
      ) ) %>%
  tab_options(table_body.hlines.color = "white",
              column_labels.border.top.color = "white") 


## 年齢ヒストグラム
dat_kyuden01 %>%
  ggplot() +
  aes(x= age) +
  geom_histogram(bins = 20, color = "white", fill = "#FFAF42") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_y_continuous(
    labels = label_number(accuracy = 1)
  ) +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/plot01.png",
         bg = "transparent", width = 5, height = 3)
  
# presentism
## table
dat_kyuden01 %>% 
  group_nest(sex) %>% 
  mutate("sex" = str_replace(.$sex, 
                             pattern = c("1", "2"),
                             replacement = c("男","女")),
         "平均スコア" = map_dbl(data, ~mean(.$presence)),
         "最小スコア" = map_dbl(data, ~min(.$presence)),
         "最大スコア" = map_dbl(data, ~max(.$presence))) %>%
  select(-2) %>%
  add_row(
    "sex" = "男女計", "平均スコア" = (89.8+76.9)/2,
    "最小スコア" = 20, "最大スコア" = 100
  ) %>%
  gt() %>%
  cols_label(sex = "性別" ) %>%
  tab_options(table.width = 400)  %>%
  fmt_number(
    columns = c("平均スコア"),
    decimals = 1,
    use_seps = FALSE
  )　%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_spanner(
    label = md("**プレゼンティーズム**"),
    columns = c(2:4)) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "gray",
      weight = px(2)
    ),
    locations = 
      cells_body(
        columns = everything(),
        rows = 1
      ) ) %>%
  tab_options(table_body.hlines.color = "white",
              column_labels.border.top.color = "white") 



## boxplot
p1 <- 
  dat_kyuden01 %>%
    ggplot() +
    aes(x=as.factor(sex), y=presence, fill=as.factor(sex)) +
    geom_boxplot(size = 1) +
    scale_fill_manual(values = c("gray40", "white")) +
    labs(x="性別", y="プレゼンティーズムスコア （max100）") +
    scale_x_discrete(labels=c(
      "1" = "男性",
      "2" = "女性")) +
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    theme(legend.position = "none") 

p1 + 
  geom_segment(x=1, y=yRoof*.95, xend=2, yend=yRoof*.95, size=1) + 
  annotate("text", x=1.5, y=yRoof*0.98, label=print(sig(tk$p_value)), size=13) +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/plot02.png",
         bg = "transparent",width = 4, height = 6)
#######
yRoof <- 
  round(max(dat_kyuden01$presence)*1.2,1)
#######
  
## t-test presentism
tk <- dat_kyuden01 %>% t_test(
  formula = presence ~ sex,
  order = c("1", "2"),
  alternative = "two-sided",
)
sig <- function(a) {
  if (a > 0.1) {
    return("")
  } else {
    if ((a <= 0.1)&&(a > 0.05)) {
      return(".")
    } else {
      if ((a <= 0.05)&&(a > 0.01)) {
        return("*")
      } else {
        if ((a <= 0.01)&&(a > 0.001)) {
          return("**")
        } else return("***")
      }
    }
  }
}

## dat_kyuden01 %>%
dat_kyuden01 %>%
  ggplot() +
    aes(x= presence) +
    geom_histogram(bins = 25, color = "white", fill = "#FFAF42")+
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    labs(x="プレゼンティーズムスコア")+
    ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/plot03.png",
           bg = "transparent",width = 5, height = 3)

## pointplot
dat_kyuden01 %>%
  ggplot() +
  aes(x=age, y=presence, color=as.factor(sex)) +
  geom_point() +
  scale_color_manual(values = c("royalblue", "salmon"),
                     labels = c("男性", "女性")) +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(x="年齢", y="プレゼンティーズムスコア", color="性別")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2022_Minacare/02_plot/plot04.png",
         bg = "transparent",width = 5, height = 3)
  