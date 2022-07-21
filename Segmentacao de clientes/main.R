# Recência (R)
# Há quantos dias foi a última compra desse cliente? Frequência (F)
# Quantas compras esse cliente já fez na sua empresa, desde que se cadastrou? Monetaridade (M)
# Quanto esse cliente já gastou em dinheiro na sua empresa?

library("dplyr")          ## load
library("ggplot2")
library ("cluster")
library("fpc")
library("ggfortify")
library("caret")
require("psych")
library("rpart")
library("rpart.plot")

online_retail <- readxl::read_excel("online_retail_II.xlsx")


nrow(online_retail)

head(online_retail)
colSums(is.na(online_retail))

online_retail$totalPrice <- online_retail %>% with(Price * Quantity)


online_retail$InvoiceDate <- substr(online_retail$InvoiceDate, 0, 10)
online_retail$InvoiceDate <- as.Date(online_retail$InvoiceDate, format="%Y-%m-%d")

online_retail <- online_retail %>% filter(!StockCode %in% c("TEST001", "TEST002"),
                                          Quantity >= 0,
                                          !is.na(`Customer ID`) )

colSums(is.na(online_retail))
nrow(online_retail)

online_retail %>% group_by(Country) %>% summarise(f = n()) %>%  arrange(desc(f))

online_retail <- online_retail %>% filter(Country == 'United Kingdom') 

nrow(online_retail)



online_retail %>% group_by(StockCode) %>% summarise(n())



all_products = online_retail %>% select(StockCode) %>% unique() %>% as.matrix() %>% as.vector()
all_clients = online_retail %>% select(`Customer ID`) %>% distinct() %>% as.matrix() 

df_sparse <- data.frame(matrix(ncol = length(all_products), nrow = length(all_clients)))
colnames(df_sparse) <- all_products
row.names(df_sparse) <- all_clients


online_retail %>% distinct(Invoice)




products_bought_by_customers = table(online_retail$Invoice, online_retail$StockCode)
as.numeric(products_bought_by_customers[0:1])

monetary <- function(data){
  data %>% group_by(`Customer ID`) %>% summarise(monetary = sum(totalPrice))
}

frequency <- function(data){
  data_frequency <- data %>% 
    group_by(`Customer ID`) %>% 
    summarise(frequency=n_distinct(Invoice))
  
  data_frequency$frequency <- as.numeric(data_frequency$frequency)
  data_frequency
}

recurrency <- function(data){
  last_date <- as.Date("2010-12-31", format="%Y-%m-%d")
  temp_data <- data %>% group_by(`Customer ID`) %>% 
    summarise(max_date = max(InvoiceDate)) %>% 
    mutate(recurrency = last_date-max_date)
  
  temp_data$recurrency <- temp_data$recurrency %>% as.numeric()
  temp_data
}

first_apper <- function(data){
  today_date <- as.Date("2010-12-31", format="%Y-%m-%d")
  temp_data <- online_retail %>% 
    group_by(`Customer ID`) %>% 
    summarise(first_time = as.Date("2010-12-31", format="%Y-%m-%d") - min(InvoiceDate))
  
  temp_data$first_time <- temp_data$first_time %>% as.numeric()
  temp_data
}


calculate_RFM <- function(data) {
  M <- monetary(data)
  F <- frequency(data)
  R <- recurrency(data)
  FD <- first_apper(data)
  
  rfm <- merge(M, F, by='Customer ID')
  rfm <- merge(rfm, R, by='Customer ID')
  rfm <- merge(rfm, FD, by='Customer ID')
  
  row.names(rfm) <- rfm$`Customer ID`
  rfm <- rfm
}

rfm <- calculate_RFM(online_retail)
rfm <- rfm %>% dplyr::select(-c(`Customer ID`, max_date))
#Ploting the distribution
rfm %>% ggplot(mapping=aes(frequency)) +
  geom_histogram() + 
  ggtitle('Distribuição da frequência')

rfm %>% ggplot(mapping=aes(recurrency)) +
  geom_histogram() + 
  ggtitle('Distribuição da recorrência')

rfm %>% ggplot(mapping=aes(monetary)) +
  geom_histogram() + 
  ggtitle('Distribuição do Total Gasto por cliente')

rfm %>% ggplot(mapping=aes(first_time)) +
  geom_histogram() + 
  ggtitle('Distribuição da primeira compra do cliente')


rfm %>% ggplot(mapping=aes(scale((first_time)))) +
  geom_histogram() + 
  ggtitle('Distribuição do Total Gasto por cliente')


rfm %>% ggplot(mapping = aes(scale(log1p(frequency)))) + 
  geom_histogram(bins=15) + 
  ggtitle('Distribuição do atributo frequency')

scale(log1p(rfm$frequency)) %>% hist()
scale(log1p(rfm$recurrency)) %>% hist()
scale(log1p(rfm$monetary)) %>% hist()
scale(log1p(rfm$first_time)) %>% hist()

rfm
rfm_scale <- data.frame(scale(log1p(rfm)))
rfm_scale
# Finding clusters 

library(factoextra)

fviz_nbclust(rfm_scale, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype = 2)

k2 <- kmeans(rfm_scale, centers = 4)

## PCA
pca_res <- prcomp(rfm_scale, scale. = TRUE, center = TRUE)
pca_res$sdev^2/sum(pca_res$sdev^2)

pca_res#
unique(k2$cluster)

rfm$cluster <- k2$cluster
rfm_scale$cluster <- k2$cluster

db <- fpc::dbscan(rfm_scale, eps = 0.15, MinPts = 5)

cortest.bartlett(rfm_scale %>% select(-cluster))

rfm_scale %>% select(-cluster)
pca_res <- prcomp(rfm_scale %>% select(-cluster))

ggplot(pca_res, aes(x=PC1, y=PC2, colour=as.factor(rfm_scale$cluster))) + 
  geom_point() + 
  labs(color="Clusters") + 
  ggtitle("Visualização do resultado de k-means com PCA") + 
  theme_bw()


tree<- rpart(cluster~., 
             data = rfm, 
             method = 'class',
             control=rpart.control(minsplit = 1, minbucket = 10, cp = 0.2, 
                                  maxcompete = 4, maxsurrogate = 5, xval = 10,
                                  surrogatestyle = 0))
rpart.plot(tree)


rfm %>% filter(cluster == 3)

predict(tree, type="class")

pred <- predict(tree, type="class")

confusionMatrix(data=factor(rfm$cluster), reference = pred)
