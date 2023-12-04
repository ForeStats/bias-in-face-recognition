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

# Triplet data generator
set.seed(1234)
tripletdata <- imagelist[!id %in% idlist$id]
tripletdata[, ':='(count = .N), by = 'id']
tripletmeta <- metadata[!id %in% idlist$id]
tripletmeta[, ':='(grc = .N), by = c('gender', 'race')]
table(tripletmeta$gender, tripletmeta$race)
tripletids <- tripletmeta[, .SD[sample.int(grc, 90)], by =  c('gender', 'race')]
tripletdata <- tripletdata[id %in% tripletids$id]
table(tripletids$gender, tripletids$race)
table(tripletdata$gender, tripletdata$race)

# BLACK/WHITE/BALANCED TRIPLETS
racedata <- tripletdata[race == 'white']
#racedata <- tripletdata
min(racedata$count)
racedata <- racedata[, .SD[sample.int(count, 80)], by =  c('id')]

racedata$dest_file <- paste0(racedata$id, '_', racedata$filename)
racedata$source <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/',
                          racedata$id, '/', racedata$filename)
racedata$destination <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/whiteset')

subfolder_names <- unique(racedata$destination)
for (j in 1:length(subfolder_names)){
  folder<-dir.create(subfolder_names[j])
}

file.copy(from = racedata$source, to = file.path(racedata$destination, racedata$dest_file))

# Hard triplets
model <- 'FACENET'
path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/BiasTests/Baseline/HardTriplets/black/rep_', model, '.csv')
python_data    <- fread(path, header = FALSE, skip = 1)
python_data$V2 <- substr(python_data$V2, 64, 100)
python_data    <- python_data[,-1]
python_data$id <- tstrsplit(python_data$V2, "_", keep = 1)
python_data$filename <- substr(python_data$V2, 9, 20)
python_data <- python_data[,-1]
ncol        <- ncol(python_data)
python_data <- python_data[ , .SD , .SDcols = c(ncol-1, ncol, 1:(ncol-2))]
alldata     <- imagelist[python_data, on = c('id', 'filename'), nomatch=NULL]
setkeyv(alldata, c('id', 'filename'))
rm(list = c('python_data', 'path'))

play_data <- data.frame(alldata[,-c(1:4)])
dist_mat  <- as.matrix(Dist(play_data, nbproc = 15))
rm(play_data)

colnames(dist_mat) <- paste0(alldata$id, ':', alldata$filename)
dist_mat <- cbind(alldata[, c('id', 'filename')], data.table(dist_mat))
melt_dist_mat <- melt.data.table(dist_mat, id.vars = 1:2)
melt_dist_mat <- melt_dist_mat[, c("ref_id", "ref_filename") := tstrsplit(variable, ":", fixed=TRUE), by = 'variable']
head(melt_dist_mat, 10)
setorder(melt_dist_mat, id, filename, ref_id, ref_filename)
raceids <- unique(alldata$id)

set.seed(123)
tripletdf <- data.frame()
for(i in raceids){
  tripdata <- melt_dist_mat[id==i]
  tripdata <- tripdata[value > 0]
  idimgs   <- alldata[id==i]$filename
  
  for(j in idimgs){
    
    ancdata  <- tripdata[filename == j]
    
    anchor  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/', 
                      i, '/', j)
    
    posfile  <- ancdata[ref_id == i]
    #posfile  <- posfile[, .SD[value < quantile(value, 0.2)]]
    posfile  <- posfile[, .SD[value > quantile(value, 0.8)]]
    setorder(posfile, value)
    
    positive <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/', 
                       i, '/', posfile$ref_filename)

    negfile  <- ancdata[ref_id != i]
    #negfile  <- negfile[, .SD[value < max(posfile$value)]]
    negfile  <- negfile[, .SD[value < min(posfile$value)]]
    setorder(negfile, value)
    
    if(nrow(negfile) > 0){
      
      if(nrow(negfile) > 15){
        negfile  <- negfile[, .SD[1:15]]
      }
      
      negative <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/', 
                         negfile$ref_id, '/', negfile$ref_filename)
      
      l <- nrow(negfile)
      
      tripletdf <- rbind(tripletdf, cbind(anchor, positive[1:l], negative))
    } 
  }
}
colnames(tripletdf) <- c('anchor', 'positive', 'negative')
writedf <- tripletdf[complete.cases(tripletdf), ]
write.table(writedf, file = '~/Documents/Turing/GPU/BiasTests/Triplets/Black/max_hard_triplets.csv', sep = ',', row.names = F)

subdf <- writedf[sample.int(nrow(writedf), 10000), ]
write.table(subdf, file = '~/Documents/Turing/GPU/BiasTests/Triplets/Black/sub_max_hard_triplets.csv', sep = ',', row.names = F)


### SIMILARITY
setwd("~/Documents/Turing/GPU/BiasTests")
load('workspace.RData')

model <- 'MOBILENET'
#path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/BiasTests/Baseline/Embeddings/rep_', model, '.csv')
path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/BiasTests/Triplets/Black/',model,'/rep_', model, '.csv')
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

# base_alldata   <- alldata
# base_dist_mat  <- dist_mat
# tuned_dist_mat <- dist_mat
# tuned_alldata   <- alldata
# 
# head(dist_mat[,1:10])
# head(tuned_dist_mat[,1:10])

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

pdf(paste0("Black_", model, ".pdf"), height=3, width=2.2)
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
