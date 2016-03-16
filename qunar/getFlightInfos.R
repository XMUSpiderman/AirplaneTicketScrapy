## Function to extract flight information from a string
##  例如： "06:3508:45首都机场T3虹桥机场T2吉祥航HO1252空客321(中)准点率94%有餐食¥5254.2折" 
getOneFlightInfo <- function(x){
  require(rvest)
  if(str_detect(x, "中转")==FALSE){
  dep_time <- str_sub(x, 1, 5)
  arr_time <- str_sub(x, 6, 10)
  dep_port <- str_extract(x, "[\u4e00-\u9fa5]{2,4}机场") # ...首都机场..虹桥机场..
  arr_port <- str_extract_all(x, "[\u4e00-\u9fa5]{2,4}机场")[[1]][2]# 第二个
  aircompany <- str_extract(x, "(([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))")# 吉祥航/南航/春秋
  flight <- str_extract(x, "(([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))[A-Z0-9]+")# 吉祥航HO1252/南航HO1252/春秋HO1252
  plane <- str_extract(x, "(((空客)|(波音))[:alpha:]?\\d{3,4}(\\([\u4e00-\u9fa5]\\))?|(其他机型))")# 波音770/空客330/空客A330/其他机型
  flight_actual <- str_extract(x, "实际乘坐：[\u4e00-\u9fa5]+[A-Z0-9]+")%>%
    str_extract("(([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))[A-Z0-9]+") #  实际乘坐：吉祥航HO1252
  stopby <-  str_extract(x, "经停") # 经停 
  acurate <- str_extract(x, "准点率\\d{1,3}\\%")%>% str_extract("\\d{1,3}\\%") # 准点率 93%
  food <- str_extract(x, "[\u4e00-\u9fa5]{1,2}餐食") # 有餐食或者“”
  Discount <- str_extract(x, "\\d\\.\\d折|全价") # “全价” /“3.4折”/""/
  price <- str_sub(x,str_locate(x, "¥")[1]+1, ifelse(str_detect(x, "全价"),str_locate(x,"全价")[1]-1, # ..有餐食¥525全价
                                                     ifelse(str_detect(x, "\\."),str_locate(x,"\\.")[1]-2, # ..有餐食¥5254.9折
                                                            ifelse(str_detect(x, "票少"),str_length(x)-2,     #..有餐食¥525票少
                                                                   str_length(x)) #..有餐食¥525
                                                            )
                                                     )
                   )%>%as.numeric()#装换成数值
  flightInfo = data.frame(dep_time, arr_time, dep_port,arr_port,
                          aircompany,flight,plane,flight_actual,stopby,acurate,
                          food, price, Discount, transfer ="无")
  }else{ # 下面是处理有中转的情况
  time <-  str_extract_all(x,"\\d{2}:\\d{2}")
  dep_time1 = time[[1]][1]
  arr_time1 = time[[1]][2]
  dep_time2 = time[[1]][3]
  arr_time2 = time[[1]][4]
  air_ports <- str_extract_all(x, "[\u4e00-\u9fa5]{2,4}机场") # ...首都机场..虹桥机场..
  dep_port1 <- air_ports[[1]][1] # 出发机场
  arr_port1 <- air_ports[[1]][2] # 到达中转机场
  dep_port2 <- air_ports[[1]][3] # 出发中转机场
  arr_port2 <- air_ports[[1]][4] # 到达机场
  aircompany1 <- str_extract(x,"(([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))")
  aircompany2 <- str_extract_all(x,"[^实际乘坐：](([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))")[[1]][2]%>%
    str_extract("(([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))")
  flight1 <- str_extract(x, "(([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))[A-Z0-9]+")
  flight2 <- str_extract_all(x,"[^实际乘坐：](([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))[A-Z0-9]+")[[1]][2]%>%
    str_extract("(([\u4e00-\u572A,\u573B-\u9fa5]{1,2}航)|(春秋))[A-Z0-9]+")
  flight_actual1 <-  str_extract(x, "实际乘坐：([\u4e00-\u9fa5]+[A-Z0-9]+){2}")%>%
    str_extract("实际乘坐：[\u4e00-\u9fa5]+[A-Z0-9]+")
  flight_actual2 <-  str_extract(x, "实际乘坐：([\u4e00-\u9fa5]+[A-Z0-9]+)准点率")%>%
    str_extract("实际乘坐：[\u4e00-\u9fa5]+[A-Z0-9]+")
  planes <- str_extract_all(x,"(((空客)|(波音))[:alpha:]?\\d{3,4}(\\([\u4e00-\u9fa5]\\))?|(其他机型))")
  plane1 <- planes[[1]][1]
  plane2<- planes[[1]][2]
  stopby <-  str_extract(x, "经停") # 经停 
  acurates <- str_extract_all(x, "准点率\\d{1,3}\\%")%>%
    str_extract_all("\\d{1,3}\\%")
  acurate1 <- acurates[[1]][1]
  acurate2 <- acurates[[1]][2]
  food <- str_extract_all(x, "[\u4e00-\u9fa5]{1,2}餐食")
  food1 <- food[[1]][1]
  food2 <- food[[1]][2]
  discount <- str_extract(x, "\\d\\.\\d折|全价") # “全价” /“3.4折”/""/
  ## Price这里可能以后使用的时候会遇到bug
  price <- str_sub(x,str_locate(x, "¥")[1]+1, ifelse(str_detect(x, "全价"),str_locate(x,"全价")[1]-1, # ..有餐食¥525全价
                                                     ifelse(str_detect(x, "\\."),str_locate(x,"\\.")[1]-2, # ..有餐食¥5254.9折
                                                            ifelse(str_detect(x, "票少"),str_locate(x, "中转")[1]-3,     #..有餐食¥525票少中转北京
                                                                   str_locate(x, "中转")[1]-1) #..有餐食¥525中转北京
                                                     )
  )
  )%>%as.numeric()#装换成数值

transfer <- str_extract(x, "中转.+")%>%
  str_extract("[^中转].+")

flightInfo1 <- data.frame(
    dep_time = dep_time1, arr_time = arr_time1,  dep_port=dep_port1,arr_port = arr_port1,
    aircompany = aircompany1,flight = flight1,plane=plane1,flight_actual=flight_actual1,stopby,acurate=acurate1,
    food = food1, price = price, Discount = discount, transfer
  ) 
flightInfo2 <- data.frame(
    dep_time = dep_time2, arr_time = arr_time2,  dep_port=dep_port2,arr_port = arr_port2,
    aircompany = aircompany2, flight = flight2,plane=plane2,flight_actual=flight_actual2,stopby,acurate=acurate2,
    food = food2, price = price, Discount=discount, transfer
  )
flightInfo = rbind(flightInfo1,  flightInfo2)
  }

  return(flightInfo)
}

 

##  Read->GetText-?ExtractInfo->CombineDataFrame
getFlightInfos <- function(page, queryDate){
  require(dplyr)
  require(rvest)
  ## 要求网页存储命名格式 "北京-广州2016-03-14.html"
   dep <- str_sub(page, str_locate(page, "[\u4e00-\u9fa5]+") ,str_locate(page,"-")-1)
   arr <- str_sub(page, str_locate(page,"-")[1]+1, str_locate(page, "2016")[1]-1)
   Date <- as.Date(str_extract(page, "\\d{4}-\\d{2}-\\d{2}"))
  flightInfos <- read_html(page)%>%   # Read a Pad-like page
    html_nodes(xpath  = '//*[@id="flight-list"]/div/div[6]/div[3]/div') %>% # nodes
    html_children()%>%
    html_text()%>%
    str_replace_all("[\\s,\\t,\\n]", "")%>% #remove whitespace \n and \t
    str_extract_all("\\d{2}:\\d{4}:\\d{2}.+")%>% # extract flight strings
    unlist()%>%
    lapply(getOneFlightInfo)%>%
    #unlist()%>%
    do.call("rbind",.)%>%
    mutate(Date = Date,
           queryDate = as.Date(queryDate),
           dep_time = as.POSIXct(str_c(Date,dep_time)),
           arr_time = as.POSIXct(str_c(Date,arr_time)),
           depCity = dep,
           arrCity = arr)%>%
    mutate( arr_time = ifelse(arr_time<dep_time,arr_time+lubridate::days(1),arr_time)%>%#处理跨日的情况, 起飞比到达大时到达时间+1d
             as.POSIXct(., origin = "1970-01-01"),
           dep_time = format(dep_time,"%H:%M")
           )%>%
    mutate(arrDate = format(arr_time,"%Y-%m-%d"),
           arr_time = format(arr_time,"%H:%M")
           )%>%
    select(depDate = Date, queryDate, depCity, arrCity, # 其实只是排序
           dep_time, arrDate,arr_time, dep_port, arr_port, 
           aircompany, flight, plane, flight_actual, 
           stopby, acurate, food, price, Discount,transfer
           )
  return(flightInfos)
}




#下面是测试

# temp <- read_html("./去哪儿网 - 去哪儿网 Qunar.com.html")%>%   # Read a Pad-like page
#   html_nodes(xpath  = '//*[@id="flight-list"]/div/div[6]/div[3]/div') %>% # nodes
#    html_children()%>%
#    html_text()%>%
#    str_replace_all("[\\s,\\t,\\n]", "")%>% #remove whitespace \n and \t
#    str_extract_all("\\d{2}:\\d{4}:\\d{2}.+")%>% # extract flight strings
#    unlist()
# temp <- bjsh[1]
# dep_time <- str_sub(temp, 1, 5)
# arr_time <- str_sub(temp, 6, 10)
# dep_port <- str_extract(temp, "[\u4e00-\u9fa5]{2,4}机场")
# arr_port <- str_extract_all(temp, "[\u4e00-\u9fa5]{2,4}机场")[[1]][2]
# aircompany <- str_extract(temp, "[\u4e00-\u9fa5]{1,2}航")
# flight <- str_extract(temp, "[\u4e00-\u9fa5]{1,2}航[A-Z0-9]+")
# plane <- str_extract(temp, "((空客)|(波音))\\d{3,4}(\\([\u4e00-\u9fa5]\\))?")
# flight_actual <- str_extract(temp, "实际乘坐：[\u4e00-\u9fa5]{1,2}航[A-Z0-9]+")
# acurate <- str_extract(temp, "准点率\\d{1,3}\\%")
# food <- str_extract(temp, "[\u4e00-\u9fa5]{1,2}餐食")
# Discount <- str_extract(temp, "\\d\\.\\d折")
# price <- str_sub(temp,str_locate(temp, "¥")[1]+1, str_locate(temp, "\\.")[1]-2)
# data.frame(dep_time, arr_time, dep_port,arr_port,
#            aircompany,flight,plane,flight_actual,acurate,
#            food, price, Discount)
# 
