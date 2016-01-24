#http://search.jd.com/search?keyword=手机

#### packages we need ####
## ----------------------------------------------------------------------- ##
require(stringr)
require(XML)
require(RCurl)
library(Rwebdriver)

setwd("JDDownload")

BaseUrl<-"http://search.jd.com"

quit_session()
start_session(root = "http://localhost:4444/wd/hub/",browser = "firefox")

# post Base Url
post.url(url = BaseUrl)

SearchField<-element_xpath_find(value = '//*[@id="keyword"]')
SearchButton<-element_xpath_find(value = '//*[@id="gwd_360buy"]/body/div[2]/form/input[3]')
#keyword for search
keywords<-'手机'

element_click(SearchField)
keys(keywords)
element_click(SearchButton)
Sys.sleep(1)
#test
get.url()

pageSource<-page_source()
parsedSourcePage<-htmlParse(pageSource, encoding = 'UTF-8')
## Download Search Results
fname <- paste0(keywords, " SearchPage 1.html")
writeLines(pageSource, fname)

#get all the brand url
Brand<-'//*[@id="J_selector"]/div[1]/div/div[2]/div[3]/ul/li/a/@href'
BrandLinks<-xpathSApply(doc = parsedSourcePage, path = Brand)

View(data.frame(BrandLinks))

BrandLinks<-sapply(BrandLinks,function(x){
  paste0(BaseUrl,"/",x)
  })

save(BrandLinks,file = 'BrandLinks.rda')


################################################
### 对各品牌的手机页面进行抓取       ########3#
BrandUrl<-BrandLinks[1]

#先搜索每一个品牌
curl  <- getCurlHandle()
Brand1Page<-post.url(url = BrandUrl)
Brand1_pageSource<-page_source()

#保存页面
Brand1_FileName<-paste0("Brand1/Brand1_pageSource.html")
writeLines(Brand1_pageSource,Brand1_FileName)

ParsedBrand1Page<-htmlParse(Brand1_pageSource, encoding = 'UTF-8')

#GET Brand name
BrandNamePath<-'//*[@id="J_crumbsBar"]/div[2]/div/a/em'
BrandName<-xpathSApply(doc = ParsedBrand1Page, path = BrandNamePath, fun = xmlValue)

#提取品牌页面的每个商品的网页链接
Bran1_AllProductPath<-'//*[@id="J_goodsList"]/ul/li/div/div[4]/a/@href'

#访问每个商品的页面
Brand1_AllProductLinks<-xpathSApply(doc = ParsedBrand1Page, path = Bran1_AllProductPath)
#Brand1_AllProductLinks<-data.frame(Brand1_AllProductLinks)

FalseLink<-grep(x = Brand1_AllProductLinks,pattern = 'https',fixed = TRUE)
Brand1_AllProductLinks<-Brand1_AllProductLinks[-FalseLink]
View(data.frame(Brand1_AllProductLinks))
Brand1_AllProductLinks<-str_c('http:',Brand1_AllProductLinks)

#保存每一品牌下的所有商品链接
save(Brand1_AllProductLinks,file = 'Brand1_AllProductLinks.rda')



#################################################
######## 访问每个商品页面，提取有用信息  ########

Bran1_Product1<-Brand1_AllProductLinks[1]

post.url(Bran1_Product1)
Sys.sleep(1)

#保存页面
Bran1_Product1_pageSource<-page_source()
Bran1_Product1_FileName<-paste0("Brand1/Bran1_Product1_pageSource.html")
writeLines(Bran1_Product1_pageSource,Bran1_Product1_FileName)

#parse 
ParsedBrand1_product1_Page<-htmlParse(Bran1_Product1_pageSource, encoding = 'UTF-8')

# get title,,key count,price,CommentCount

#PATH
TitlePath<-'//*[@id="name"]/h1'
KeyCountPath<-'//*[@id="p-ad"]'
PricePath<-'//*[@id="jd-price"]'
commentCountPath<-'//*[@id="comment-count"]/a'
SizePath<-'//*[@id="parameter1"]/li[1]/div/p[1]'
BackBitPath<-'//*[@id="parameter1"]/li[2]/div/p[1]'
ForwardBitPath<-'//*[@id="parameter1"]/li[2]/div/p[2]'
CorePath<-'//*[@id="parameter1"]/li[3]/div/p[1]'
NamePath<-'//*[@id="parameter2"]/li[1]'
CodePath<-'//*[@id="parameter2"]/li[2]'
BrandPath<-'//*[@id="parameter2"]/li[3]'
onSaleTimePath<-'//*[@id="parameter2"]/li[4]'
ResolutionPath<-'//*[@id="parameter1"]/li[1]/div/p[2]'

Title<-xpathSApply(doc = ParsedBrand1_product1_Page,path = TitlePath,xmlValue)
KeyCount<-xpathSApply(doc = ParsedBrand1_product1_Page,path = KeyCountPath,xmlValue)
Price<-xpathSApply(doc = ParsedBrand1_product1_Page,path = PricePath,xmlValue)
commentCount<-xpathSApply(doc = ParsedBrand1_product1_Page,path = commentCountPath,xmlValue)
Size<-xpathSApply(doc = ParsedBrand1_product1_Page,path = SizePath,xmlValue)
BackBit<-xpathSApply(doc = ParsedBrand1_product1_Page,path = BackBitPath,xmlValue)
ForwardBit<-xpathSApply(doc = ParsedBrand1_product1_Page,path = ForwardBitPath,xmlValue)
Core<-xpathSApply(doc = ParsedBrand1_product1_Page,path = CorePath,xmlValue)
Resolution<-xpathSApply(doc = ParsedBrand1_product1_Page,path = ResolutionPath,xmlValue)
Brand<-xpathSApply(doc = ParsedBrand1_product1_Page,path = BrandPath,xmlValue)
onSaleTime<-xpathSApply(doc = ParsedBrand1_product1_Page,path = onSaleTimePath,xmlValue)

# 整理成data frame
mydata<-data.frame(Title = Title,KeyCount = KeyCount, Price = Price,
                   commentCount = commentCount, Size = Size, BackBit = BackBit,
                   ForwardBit = ForwardBit, Core = Core, Resolution = Resolution,
                   Brand = Brand, onSaleTime = onSaleTime)

View(mydata)


##############################################################
####### Function  ############################################

##############Function 1 #################################3##

### 对各品牌的手机页面进行抓取       ########3#


getBrandPage<-function(BrandUrl,foreDownload = T){
  #获取某品牌搜索页面
  post.url(BrandUrl)
  Brand_pageSource<-page_source()
  #parse 
  parsedSourcePage<-htmlParse(Brand_pageSource, encoding = 'UTF-8')
  
  #get brand name
  BrandNamePath<-'//*[@id="J_crumbsBar"]/div[2]/div/a/em'
  BrandName<-xpathSApply(doc = parsedSourcePage, path = BrandNamePath, fun = xmlValue)
  
  #Save the page
  BrandPageName<-paste0(BrandName,'_PageSource.html')
  #Create a file
  if(!file.exists(BrandName)) dir.create(BrandName)
  # save
  writeLines(text = Brand_pageSource, con = paste0(BrandName,'/',BrandPageName))
  
  # get the product page url
    #path
    Brand_AllProductPath<-'//*[@id="J_goodsList"]/ul/li/div/div[4]/a/@href'
   #url
    Brand_AllProductLinks<-xpathSApply(doc = parsedSourcePage, path = Brand_AllProductPath)
  
#     #remove some false url
#     FalseLink<-grep(x = Brand_AllProductLinks,pattern = 'https',fixed = TRUE)
#     Brand_AllProductLinks<-Brand_AllProductLinks[-FalseLink]
    
    # add a head
    Brand_AllProductLinks<-str_c('http:',Brand_AllProductLinks)
  #save and return the url
    save(Brand_AllProductLinks,file = paste0(BrandName,'_AllProductLinks.rda'))
    return(Brand_AllProductLinks)
}

# test
BrandUrl<-BrandLinks[1]

getBrandPage(BrandUrl)

#get all the links
Brand_ProductLink<-list()
for(i in 1:length(BrandLinks)){
  Sys.sleep(10)
  Brand_ProductLink[[i]]<-getBrandPage(BrandUrl = BrandLinks[i])
}

#clean the links
All_ProductLink<-lapply(Brand_ProductLink,function(x){
   TrueLink<-grep(x = x,pattern = 'http://item.jd.com/',fixed = TRUE,value = FALSE)
   return(x[TrueLink])
})
# save the links
save(All_ProductLink,file = 'All_ProductLink.rda')


#################################################
######## Function2 :访问每个商品页面，提取有用信息  ########

Product<-function(ProductLink){
  post.url(ProductLink)
  Sys.sleep(4)
  
  # get the page
  Product_pageSource<-page_source()
  
  #parse 
  Parsed_product_Page<-htmlParse(Product_pageSource, encoding = 'UTF-8')
  
  # get title,,key count,price,CommentCount and so on
  
  #PATH
  TitlePath<-'//*[@id="name"]/h1'
  KeyCountPath<-'//*[@id="p-ad"]'
  PricePath<-'//*[@id="jd-price"]'
  commentCountPath<-'//*[@id="comment-count"]/a'
  SizePath<-'//*[@id="parameter1"]/li[1]/div/p[1]'
  BackBitPath<-'//*[@id="parameter1"]/li[2]/div/p[1]'
  ForwardBitPath<-'//*[@id="parameter1"]/li[2]/div/p[2]'
  CorePath<-'//*[@id="parameter1"]/li[3]/div/p[1]'
  NamePath<-'//*[@id="parameter2"]/li[1]'
  CodePath<-'//*[@id="parameter2"]/li[2]'
  BrandPath<-'//*[@id="parameter2"]/li[3]'
  onSaleTimePath<-'//*[@id="parameter2"]/li[4]'
  ResolutionPath<-'//*[@id="parameter1"]/li[1]/div/p[2]'
  
  Title<-xpathSApply(doc = Parsed_product_Page,path = TitlePath,xmlValue)
  KeyCount<-xpathSApply(doc = Parsed_product_Page,path = KeyCountPath,xmlValue)
  Price<-xpathSApply(doc = Parsed_product_Page,path = PricePath,xmlValue)
  commentCount<-xpathSApply(doc = Parsed_product_Page,path = commentCountPath,xmlValue)
  Size<-xpathSApply(doc = Parsed_product_Page,path = SizePath,xmlValue)
  BackBit<-xpathSApply(doc = Parsed_product_Page,path = BackBitPath,xmlValue)
  ForwardBit<-xpathSApply(doc = Parsed_product_Page,path = ForwardBitPath,xmlValue)
  Core<-xpathSApply(doc = Parsed_product_Page,path = CorePath,xmlValue)
  Name<-xpathSApply(doc = Parsed_product_Page,path = NamePath,xmlValue)
  Code<-xpathSApply(doc = Parsed_product_Page,path = CodePath,xmlValue)
  Resolution<-xpathSApply(doc = Parsed_product_Page,path = ResolutionPath,xmlValue)
  Brand<-xpathSApply(doc = Parsed_product_Page,path = BrandPath,xmlValue)
  onSaleTime<-xpathSApply(doc = Parsed_product_Page,path = onSaleTimePath,xmlValue)
  
  # 整理成data frame
  mydata<-data.frame(Title = Title,KeyCount = KeyCount, Price = Price,
                     commentCount = commentCount, Size = Size, BackBit = BackBit,
                     ForwardBit = ForwardBit, Core = Core, Name = Name,Code = Code,
                     Resolution = Resolution,
                     Brand = Brand, onSaleTime = onSaleTime)
  
  
  #save the page  
  FileName<-paste0('Product/',Brand,Code,'_pageSource.html')
  writeLines(text = Product_pageSource,con = FileName)
 #return the data
  return(mydata)
  
}





# test
quit_session()
start_session(root = "http://localhost:4444/wd/hub/",browser = "firefox")

load(file = 'All_ProductLink.rda')

ProductLink1<-All_ProductLink[[40]][1]

testData<-Product(ProductLink = ProductLink1)



#定义tryCatch

mySpider<-function(ProductLink){
  out<-tryCatch(
    {
      message('This is the try part:')
     Product(ProductLink = ProductLink)
    },
    error=function(e){
      message(e)
      return(NA)
    },
    finally = {
      message("The end!")
    }
  )
  return(out)
}

## loop

# get all data 
ProductInformation<-list()
k <-0

for(i in 1:length(All_ProductLink)){
  for(j in 1:length(All_ProductLink[[i]])){
    k<-k+1
    ProductInformation[[k]]<-mySpider(ProductLink = All_ProductLink[[i]][j])
  }
}

# save my data 
MobilePhoneInformation<-do.call(rbind,ProductInformation)
View(MobilePhoneInformation)
save(MobilePhoneInformation,file = 'MobilePhoneInformation.rda')

nrow(na.omit(MobilePhoneInformation))
