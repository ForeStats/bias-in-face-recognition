library(xml2)
library(readr)
library(data.table)
library(gridExtra)
library(matrixStats)
library(amap)

identity_meta <- fread("~/Documents/Turing/GPU/VGGData/VMER/identity_meta.csv")
colnames(identity_meta) <- c('id', 'name', 'count', 'flag', 'gender', 'age')

pg       <- read_xml("~/Documents/Turing/GPU/VGGData/VMER/finalTrain.xml")
recs     <- xml_find_all(pg, "//subject")

metadatatrain <- data.frame(do.call(rbind, lapply( recs,
                                                   function(x) {
                                                     cbind(id = xml_text(xml_find_all(x, xpath = 'id')), 
                                                           race = xml_text(xml_find_all(x, xpath = 'ethnicity')))
                                                   })))

pg       <- read_xml("~/Documents/Turing/GPU/VGGData/VMER/finalTest.xml")
recs     <- xml_find_all(pg, "//subject")

metadatatest <- data.frame(do.call(rbind, lapply( recs,
                                                  function(x) {
                                                    cbind(id = xml_text(xml_find_all(x, xpath = 'id')), 
                                                          race = xml_text(xml_find_all(x, xpath = 'ethnicity')))
                                                  })))
rm(list = c('pg', 'recs'))

metadata <- rbind(metadatatrain, metadatatest, c('n006003', 1))
setorder(metadata, id)
rm(list = c('metadatatrain', 'metadatatest'))
metadata <- identity_meta[metadata, on = 'id']
rm(identity_meta)
metadata$race <- factor(metadata$race, levels = 1:4, labels = c('black', 'asian', 'white', 'indian'))
metadata$gender <- factor(metadata$gender)
table(metadata$gender, metadata$race)

imagelist <- fread("~/Documents/Turing/GPU/VGGData/aligned/list.txt", sep = '/', header = FALSE)
colnames(imagelist) <- c('id', 'filename')
imagelist <- imagelist[metadata[, c('id', 'gender', 'race')], on = 'id']
setorder(imagelist, id, filename)
imagelist[, ':='(count = .N), by = 'id']
metadata[, ':='(grc = .N), by = c('gender', 'race')]

# Test Data for Viz
set.seed(1234)
filterdata <- imagelist[, .SD[sample.int(count, 10)], by =  'id']
idlist <- metadata[, .SD[sample.int(grc, 100)], by =  c('gender', 'race')]
filterdata <- filterdata[id %in% idlist$id]
filterdata$count <- NULL
imagelist$count <- NULL
metadata$grc <- NULL
table(filterdata$gender, filterdata$race)

save.image('workspace.RData')

# filterdata$dest_file <- paste0(filterdata$id, '_', filterdata$filename)
# filterdata$source <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/',
#                             filterdata$id, '/', filterdata$filename)
# filterdata$destination <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/testset')
# 
# subfolder_names <- unique(filterdata$destination)
# for (j in 1:length(subfolder_names)){
#   folder<-dir.create(subfolder_names[j])
# }
# 
# file.copy(from = filterdata$source, to = file.path(filterdata$destination, filterdata$dest_file))

setwd("~/Documents/Turing/GPU/BiasTests")
load("~/Documents/Turing/GPU/BiasTests/workspace.RData")
library(data.table)

# Triplet data generator
set.seed(1234)
metadata <- imagelist[, list(count = .N), by = c('id', 'race', 'gender')]
tripletmeta <- metadata[!id %in% idlist$id]
table(tripletmeta$gender, tripletmeta$race)

# GENDER + RACE
b_gender <- 'f'
gendids  <- tripletmeta[gender == b_gender & race != 'white']
setorder(gendids, race, -count)
table(gendids$race)

racedata <- imagelist[id %in% gendids$id]
racedata[, ':='(count = .N), by = 'id']
setorder(racedata, race, -count)
min(racedata$count)
racedata <- racedata[, .SD[c(sample.int(count, min(300, count)), 
                             sample.int(count, max(0, 300 - count), replace = TRUE))], by = 'id']
racedata <- racedata[, ':='(race_count = .N), by = 'race']
racedata <- racedata[, .SD[c(1:min(54000, race_count), 0:max(0, 54000 - race_count))], by = 'race']
table(racedata$id)
racedata <- racedata[, img_index := seq_len(.N), by = 'id']
racedata <- racedata[, id_bin := cut(img_index, breaks = seq(0, max(racedata$img_index), 300), 
                                     right = T, labels = F), by = 'id']
racedata <- racedata[, img_index := seq_len(.N), by = c('id', 'id_bin')]
racedata <- racedata[, batch_bin := cut(img_index, breaks = seq(0, max(racedata$img_index), 4), right = T, labels = F)]
racedata <- racedata[, id_index := as.integer(as.factor(id))]
table(racedata$batch_bin)
setorder(racedata, batch_bin, race, id_bin, id_index, img_index)

#### Paperspace write
racedata$source <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/',
                          racedata$id, '/', racedata$filename)

racedata$dest_file <- paste0(racedata$id, '_', racedata$filename)
racedata$destination <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/Ftrain')

copydata <- racedata[, c('source', 'destination', 'dest_file')]
copydata <- copydata[!duplicated(copydata),]

subfolder_names <- unique(copydata$destination)
for (j in 1:length(subfolder_names)){
  folder<-dir.create(subfolder_names[j])
}

file.copy(from = copydata$source, to = file.path(copydata$destination, copydata$dest_file))

writedf <- racedata[, c('id', 'dest_file')]
writedf$id <- as.integer(as.factor(writedf$id))
writedf$source <- paste0('/notebooks/Data/BFtrain/', writedf$dest_file)
write.table(writedf, 
            file = '/Users/santhoshnarayanan/Documents/Turing/GPU/BiasTests/OnlineTriplets/Data/F_images.csv', 
            sep = ',', row.names = F)

fnames <- list.files('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/BFtrain')
writedf <- data.table(file = fnames)
writedf[, c("id") := tstrsplit(file, "_", fixed=TRUE, keep=1L)][]
writedf$source <- paste0('/notebooks/Data/BFtrain/', writedf$file)
writedf <- writedf[, img_index := seq_len(.N), by = 'id']
writedf <- writedf[, batch_bin := cut(img_index, breaks = seq(0, 128, 8), right = T, labels = F)]
setorder(writedf, batch_bin, img_index)
write.table(writedf, 
            file = '/Users/santhoshnarayanan/Documents/Turing/GPU/BiasTests/OnlineTriplets/Data/BF_images.csv', 
            sep = ',', row.names = F)


####
library(data.table)
library(amap)
library(gridExtra)
path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/MOSIP/models/rep_NWF_FACENET.csv')
python_data    <- fread(path, header = FALSE, skip = 1)
#python_data$V2 <- substr(python_data$V2, 17, 40)
python_data$V2 <- substr(python_data$V2, 63, 90)
python_data    <- python_data[,-1]
python_data$id <- tstrsplit(python_data$V2, "_", keep = 1)
python_data$filename <- substr(python_data$V2, 9, 20)
python_data <- python_data[,-1]
ncol        <- ncol(python_data)
python_data <- python_data[ , .SD , .SDcols = c(ncol-1, ncol, 1:(ncol-2))]
alldata     <- filterdata[python_data, on = c('id', 'filename'), nomatch=NULL]
setkeyv(alldata, c('id', 'filename'))
rm(list = c('python_data', 'path'))

#Matrix <- as.matrix(alldata[,-c(1:4)])
#rowSums(Matrix)
#sim <- Matrix / sqrt(rowSums(Matrix * Matrix))
#sim <- sim %*% t(sim)
#dist_mat <- 1 - sim

play_data <- data.frame(alldata[,-c(1:4)])
dist_mat  <- as.matrix(Dist(play_data, nbproc = 15))
rm(play_data)

# Identification rates
results <- data.table()
for(i in 1:100){
  set.seed(i)
  reference_set <- sample.int(10, 800, replace = TRUE)
  reference_set <- reference_set + 10*seq(0, 799, 1)
  id_mat <- dist_mat[, reference_set]
  id_mat <- id_mat[-reference_set, ]
  id_set <- apply(id_mat, 1, FUN = which.min)
  res_dt <- data.table(alldata[-reference_set, c('race', 'gender')], 
                       success = alldata$id[-reference_set] == alldata$id[reference_set][id_set])
  res_dt <- res_dt[, list(rate = mean(success)), by = c('race', 'gender')]
  if(ncol(results) > 0){
    results <- cbind(results, res_dt$rate)
  }else{
    results <- cbind(results, res_dt)
  }
}
results <- results[, list(rate = rowMeans(.SD)), by = c('race', 'gender'), .SDcols = 3:102]
setorder(results, rate)
results$rate <- round(results$rate, 3)

pdf(paste0("FACENET_NWF.pdf"), height=3, width=2.2)
grid.table(results)
dev.off()

### SIMILARITY
setwd("~/Documents/Turing/GPU/BiasTests")
load('workspace.RData')

model <- 'RESNET'
imgset <- 'Female'
weights <- 'hard'
#path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/BiasTests/Baseline/Embeddings/rep_', model, '.csv')
#path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/BiasTests/OnlineTriplets/Results/',model,'/', imgset,'/2048_rep_', weights, '.csv')
python_data    <- fread(path, header = FALSE, skip = 1)
python_data$V2 <- substr(python_data$V2, 63, 90)
python_data    <- python_data[,-1]
python_data$id <- tstrsplit(python_data$V2, "_", keep = 1)
python_data$filename <- substr(python_data$V2, 9, 20)
python_data <- python_data[,-1]
ncol        <- ncol(python_data)
python_data <- python_data[ , .SD , .SDcols = c(ncol-1, ncol, 1:(ncol-2))]
alldata     <- filterdata[python_data, on = c('id', 'filename'), nomatch=NULL]
setkeyv(alldata, c('id', 'filename'))
rm(list = c('python_data', 'path'))

play_data <- data.frame(alldata[,-c(1:4)])
dist_mat  <- as.matrix(Dist(play_data, nbproc = 15))
rm(play_data)

# Identification rates
results <- data.table()
for(i in 1:100){
  set.seed(i)
  reference_set <- sample.int(10, 800, replace = TRUE)
  reference_set <- reference_set + 10*seq(0, 799, 1)
  id_mat <- dist_mat[, reference_set]
  id_mat <- id_mat[-reference_set, ]
  id_set <- apply(id_mat, 1, FUN = which.min)
  res_dt <- data.table(alldata[-reference_set, c('race', 'gender')], 
                       success = alldata$id[-reference_set] == alldata$id[reference_set][id_set])
  res_dt <- res_dt[, list(rate = mean(success)), by = c('race', 'gender')]
  if(ncol(results) > 0){
    results <- cbind(results, res_dt$rate)
  }else{
    results <- cbind(results, res_dt)
  }
}
results <- results[, list(rate = rowMeans(.SD)), by = c('race', 'gender'), .SDcols = 3:102]
setorder(results, rate)
results$rate <- round(results$rate, 3)

pdf(paste0(imgset, "_", model, "_", weights, ".pdf"), height=3, width=2.2)
grid.table(results)
dev.off()

# Calcs
melt_dist_mat     <- melt.data.table(cbind(alldata[,1:4], data.table(dist_mat)), 
                                     id.vars = 1:4, variable.name = 'col', value.name = 'score')
melt_dist_mat$yid <- alldata$id[as.integer(melt_dist_mat$col)]
head(melt_dist_mat, 10)
rm(dist_mat)
rm(alldata)

melt_dist_mat <- melt_dist_mat[score > 0]
median(melt_dist_mat$score[melt_dist_mat$id == melt_dist_mat$yid])
median(melt_dist_mat$score[melt_dist_mat$id != melt_dist_mat$yid])

melt_dist_mat$IDpair <- paste0(melt_dist_mat$id, '-', melt_dist_mat$yid)
agg_df <- melt_dist_mat[, c('IDpair','gender', 'race', 'score')]
setorder(agg_df, IDpair)
rm(melt_dist_mat)
agg_df <- agg_df[, list(sim_score = median(score)), by = c('IDpair', 'gender', 'race')]
agg_df$norm_score <- scale(-agg_df$sim_score)
agg_df[, c("X1", "X2") := tstrsplit(IDpair, "-", fixed=TRUE)]
setkeyv(agg_df, c('gender', 'race', 'X1'))
axis_order <- unique(agg_df$X1)
agg_df$X1 <- factor(agg_df$X1, levels = axis_order)
agg_df$X2 <- factor(agg_df$X2, levels = axis_order)

group_levels = c('female-black', 'female-asian', 'female-white', 'female-indian',
                 'male-black', 'male-asian', 'male-white', 'male-indian')

library(ggplot2)
plot <-   ggplot() +
  geom_raster(data = agg_df[agg_df$X1 != agg_df$X2,] , aes(x = X1, y = X2,
                                                           fill = norm_score))  +
  scale_fill_viridis_c(name = 'Similarity Score') +
  scale_x_discrete(name = "", breaks = axis_order[seq(50, 750, 100)], 
                   labels = group_levels) +
  scale_y_discrete(name = "", breaks = axis_order[seq(50, 750, 100)], 
                   labels = group_levels)

pdf(paste0('BlackRESNET.pdf'), height=8, width=10)
print(plot)
dev.off()

agg_df[, xgroup := paste0(gender, '-', race)]
agg_df[, ':='(gender = NULL, race = NULL)]
agg_df$id = agg_df$X2
agg_df <- idlist[, c('id', 'gender', 'race')][agg_df, on = 'id', nomatch = NULL]
agg_df[, ygroup := paste0(gender, '-', race)]
agg_df <- agg_df[, list(similarity = mean(norm_score)), by = c('xgroup', 'ygroup')]

group_levels = c('f-black', 'f-asian', 'f-white', 'f-indian',
                 'm-black', 'm-asian', 'm-white', 'm-indian')
agg_df$xgroup <- factor(agg_df$xgroup, levels = group_levels)
agg_df$ygroup <- factor(agg_df$ygroup, levels = group_levels)

cast_df <- dcast.data.table(agg_df, 'xgroup ~ ygroup')

library(formattable)
my.data <- as.data.frame(cbind(cast_df[,1], round(cast_df[,-1], 2)))
formattable(my.data, list( ~ color_tile('white', 'orange')))

rm(my.data)
rm(cast_df)
rm(agg_df)
rm(axis_order)
rm(group_levels)


# if sum of probability over timestamps is above any other token, sample timestamp 
logprobs = F.log_softmax(logits.float(), dim=-1) 

for k in range(tokens.shape[0]): 
  timestamp_logprob = logprobs[k, self.tokenizer.timestamp_begin :].logsumexp(dim=-1) 
  max_text_token_logprob = logprobs[k, : self.tokenizer.timestamp_begin].max() 
  #if timestamp_logprob > max_text_token_logprob:  
  print(timestamp_logprob,max_text_token_logprob) 
  if timestamp_logprob > -5 :  
    logits[k, : self.tokenizer.timestamp_begin] = -np.inf
