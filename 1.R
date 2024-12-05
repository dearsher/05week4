

library(sf)
library(dplyr)
library(countrycode)
library(readr)
library(tidyr)
library(tmap)

# 加载空间数据
world <- st_read("zuoye.geojson")

# 加载性别不平等数据
inequality_data <- read_csv("zuoye.csv")

# 选择相关年份并计算差异
inequality_diff <- inequality_data %>%
  filter(year %in% c(2010, 2019)) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  rename(GII_2010 = `2010`, GII_2019 = `2019`) %>%
  mutate(Difference = GII_2019 - GII_2010)

# 过滤无关区域
inequality_diff <- inequality_diff %>%
  filter(!country %in% c("Arab States", "East Asia And The Pacific", "Europe And Central Asia", 
                         "High Human Development", "Latin America And The Caribbean", 
                         "Low Human Development", "Medium Human Development", 
                         "South Asia", "Sub-Saharan Africa", 
                         "Very High Human Development", "World"))

# 匹配国家代码
inequality_diff <- inequality_diff %>%
  mutate(ISO_A3 = countrycode(country, "country.name", "iso3c"))

# 合并数据
world_with_gii <- world %>%
  left_join(inequality_diff, by = c("COUNTRY" = "country"))

# 切换到交互模式
tmap_mode("view")

# 绘制交互式地图
tm_shape(world_with_gii) +
  tm_polygons("Difference", 
              title = "GII Change (2010-2019)",
              palette = "-RdBu",
              midpoint = 0) +
  tm_layout(title = "Interactive Map: GII Change (2010-2019)",
            legend.outside = TRUE)

