install.packages("tidyverse")
install.packages("jsonlite")
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(rvest)


# 取得真正網址的內容（為了算出房屋總筆數＆頁數）
url <- "https://rent.591.com.tw/?kind=0&region=1&pattern=2"
rent_html <- read_html(url, encoding = 'utf-8')

#取出總房屋筆數
house_total <- html_nodes(rent_html, ".hasData i") %>% html_text()
house_total <- as.numeric(gsub(",", "", house_total))

#算出總頁數
page_total <- ceiling(house_total / 30)


# 先生成間隔30的數列，length.out是數列的長度
steps <- seq(from = 0, by = 30, length.out = page_total)

# firstRow 數字分為插入上面生成的數字
total_link <- paste0("https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=0&searchtype=1&region=1&pattern=2&firstRow=", 
                     steps,
                     "&totalRows=",house_total)

# 生成存放每一頁資訊的串列
total_rent <- vector("list", length(total_link))

# 把所有頁面資料放進個別串列
for(i in seq_along(total_link)) {
  json_content <- fromJSON(total_link[i])
  df <- json_content$data
  df <- df$data
  total_rent[[i]] <- df
}

#所有 list 資料併在一起
my_mad <- do.call(rbind, total_rent)

#去掉price內的逗號，也改資料型態成「數字」
my_mad$price <- as.numeric(gsub(",","", my_mad$price))

#畫出盒狀圖，說明台北市2房的價格
ggplot(my_mad,  aes( x = sectionname,
                      y = price,
                      colour = sectionname)) +
  geom_boxplot() +
  labs(title = '【台北市】－2房',
       x = '區域',
       y = '月租金(元)', colour = '區域') +
  scale_y_continuous(limits = c(6000, 30000), breaks=seq(6000, 30000, 500)) +
  theme_bw()+
  theme(text=element_text(family="DFHei Std W5", size=14))

###匯出圖檔
ggsave("2room_Taipei.png")

#匯出csv
write.csv(my_mad, file = "rent_df.csv")
