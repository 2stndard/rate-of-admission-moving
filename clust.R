library(tidyverse)
library(utils)

sc <- read.table('./synthetic_control.data', header=F, sep='')
View(sc)
typeof(sc)
glimpse(sc)
n <- 10

s <- sample(1:100, n)

idx <- c(s, 100+s, 200+s, 300+s, 400+s, 500+s)

sample2 <- sc[idx,]
View(sample2)
observedLabels <- c(rep(1,n), rep(2,n), rep(3,n), rep(4,n), rep(5,n), rep(6,n))
typeof(sample2)
glimpse(sample2)

library(dtw)
distMatrix <- dist(sc, method='DTW')

hc <- hclust(distMatrix, method='average')

plot(hc, labels=observedLabels, main='')


############################################

rbind(result_1학년 |> filter(시도 == '세종', 성별 == '전체') |> unique(),
      result_1학년 |> filter(시도 != '세종', 성별 == '전체')  ) |>
  mutate(지역 = paste0(시도, 행정구역)) |>
  select(연도, 지역, `4세대비1학년변동률`) |> 
  arrange(연도) |>  
  pivot_wider(names_from = 지역, values_from = `4세대비1학년변동률`) |> clipr::write_clip()


cl_data <- readxl::read_xlsx('./cluster_date.xlsx')

cl_data1 <- read.table('./cluster_date.csv', header=T, sep=',')

rownames(cl_data1) <- cl_data1[, 1]

cl_data1 <- cl_data1[, -1]

distMatrix <- dist(t(cl_data1))

hc <- hclust(distMatrix, method='average')

plot(hc, main='')

rect.hclust(hc, k = 10)

clust <- cutree(hc, k = 50)

clust_frmae <- data.frame(name = names(clust), num = clust)

level1 <- left_join(rbind(result_1학년 |> filter(시도 == '세종', 성별 == '전체') |> unique(),
      result_1학년 |> filter(시도 != '세종', 성별 == '전체')  ) |>
  mutate(지역 = paste0(시도, 행정구역)), clust_frmae, by = c('지역' = 'name'))


level1 |> 
  ggplot() + 
  geom_line(aes(x = 연도, y = `4세대비1학년변동률`, group = 지역)) + 
  facet_wrap(~num)


######################################################################

typeof(cl_data1)
library(dtwclust) 
install.packages('dtwclust')

t(cl_data1)

cluster = tsclust(t(cl_data1), type ='h', k=3:20, distance="dtw", 
                  centroid = 'sdtw_cent',seed=1234,trace=T,
args = tsclust_args(dist = list(window.size = 60)))


cluster_k_shape = tsclust(t(cl_data1), k=3:20, type = "partitional", preproc = zscore, distance = "sbd", 
                          centroid = "shape", seed=1234, trace=T)

eval_clust<-sapply(cluster_k_shape, cvi)

eval_clust_data.frame <- as.data.frame(eval_clust)

colnames(eval_clust_data.frame) <- 3:20

eval_clust_data.frame$method <- rownames(eval_clust_data.frame)

eval_clust_data.frame |>
  pivot_longer(1:18, names_to = 'times') |>
  mutate(times = as.numeric(times)) |>
  ggplot(aes(x = as.factor(times), y = value)) + 
  geom_line(aes(group = 1)) +
  geom_vline(aes(xintercept = 13), color = 'red') +
  facet_wrap(~method, scales = 'free_y', ncol = 2, label = as_labeller(c('Sil' =  'Silhouette index(MAx)', 'D' = 'Dunn index(MAX)', 'COP' = 'COP index(MIN)', 'DB' = 'Davies-Bouldin index(MIN)', 'DBstar' = 'Modified Davies-Bouldin index(MIN)', 'CH' = 'Calinski-Harabasz index(MAX)', 'SF' = 'Score Function(MAX)'))) + 
  labs(x = '성능 측정 변수', y = '클러스터수', title = '클러스터 개수별 성능 인덱스') +
  theme(strip.text = element_text(size=7))

cl = slot(cluster_k_shape[13], "cluster") 

dtwclust::plot(cluster_k_shape[[13]])

plot(cluster_k_shape[[13]]@cluster)
plot(cluster_k_shape[[13]], type='sc')
plot(cluster_k_shape[[13]], type='series', main = '클러스터별 지역 이동성 그래프', xlab)
plot(cluster_k_shape[[13]], type='centroids')

centroid <- as.data.frame(cluster_k_shape[[13]]@centroids, col.names = 1:15) |> 
  mutate(연도 = 2013:2021) |>
  pivot_longer(1:15, names_to = '클러스터') |>
  mutate(클러스터 = gsub('X', '', 클러스터))


as.data.frame(cluster_k_shape[[13]]@datalist) |>
  mutate(연도 = as.factor(2013:2021)) |>
  pivot_longer(-(last_col()), names_to = '지역') |>
  group_by(연도) |>
  mutate(클러스터 = cluster_k_shape[[13]]@cluster) |>
  ungroup() |>
  ggplot() +
  geom_line(aes(x = as.Date(paste0(연도, '-01-01')), y = value, group = 지역), alpha = 0.2, color = 'grey30') +
  geom_line(data = centroid, aes(x = as.Date(paste0(연도, '-01-01')), y = value, group = 클러스터)) + 
  geom_smooth(data = centroid, aes(x = as.Date(paste0(연도, '-01-01')), y = value, group = 클러스터), method = 'loess', color = 'blue') + 
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  facet_wrap(~as.factor(클러스터)) + 
  labs(x = '연도', y = '지역 이동성(z score)', title = '지역 이동성 클러스터의 주 패턴 모형') +
  theme(legend.position = 'none')


as.data.frame(cluster_k_shape[[13]]@datalist) |>
  mutate(연도 = as.factor(2013:2021)) |>
  pivot_longer(-(last_col()), names_to = '지역') |>
  group_by(연도) |>
  mutate(클러스터 = cluster_k_shape[[13]]@cluster) |>
  ungroup() |> clipr::write_clip()




as.data.frame(cluster_k_shape[[13]]@centroids, col.names = c(paste0('cluster', 1:15))) |> 
  mutate(연도 = 2013:2021) |>
  pivot_longer(1:15, names_to = '클러스터') |>
  mutate(클러스터 = fct_relevel(클러스터, c(paste0('cluster', 1:15)))) |>
  ggplot() +
  geom_line(aes(x = as.Date(paste0(연도, '-01-01')), y = value, group = 클러스터, color = 클러스터)) +
  scale_x_date(date_breaks = '2 years', date_labels = '%y') +
  facet_wrap(~as.factor(클러스터)) + 
  labs(x = '연도', y = '지역 이동성(z score)', title = '지역 이동성 클러스터의 주 패턴 모형') +
  theme(legend.position = 'none')

View(cluster_k_shape[[13]])

result_clust <- data.frame(t(cl_data1))
result_clust$clust <- cluster_k_shape[[13]]@cluster
View(cluster_k_shape[[11]])

glimpse(result_clust)

result_clust$지역 <- rownames(result_clust)

result_clust |>
  pivot_longer(1:9, names_to = '연도') |>
  mutate(연도 = as.factor(as.numeric(gsub('X', '', 연도)))) |>
  filter(clust == 12) |>
  ggplot() + 
  geom_line(aes(x = 연도, y = value, group = 지역))
  
  

openxlsx::write.xlsx(cluster_k_shape[[11]], "./tab1clustn.csv")


######################################################################

library(TSclust)

TSclust_distance_matrix <- diss(cl_data1[, -1], METHOD="AR.LPC.CEPS")

hc <- hclust(TSclust_distance_matrix)

plot(hc, main='')

rect.hclust(hc, k = 10)

TSclust_cutree <- cutree(hc, k = 50)



clust_frmae <- data.frame(name = names(TSclust_cutree), num = TSclust_cutree)

level1 <- left_join(rbind(result_1학년 |> filter(시도 == '세종', 성별 == '전체') |> unique(),
                          result_1학년 |> filter(시도 != '세종', 성별 == '전체')  ) |>
                      mutate(지역 = paste0(시도, 행정구역)), clust_frmae, by = c('지역' = 'name'))


level1 |> 
  ggplot() + 
  geom_line(aes(x = 연도, y = `4세대비1학년변동률`, group = 지역)) + 
  facet_wrap(~num)
