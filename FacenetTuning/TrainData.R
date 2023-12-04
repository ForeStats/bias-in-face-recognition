library(xml2)
library(readr)
library(data.table)
library(gridExtra)
library(matrixStats)
library(parallel)
library(stringr)
library(pdist)

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
setorder(imagelist, id, filename)
imagelist[, ':='(count = .N), by = 'id']

metadata$count <- NULL
idcount <- imagelist[, c('id', 'count')]
idcount <- idcount[!duplicated(idcount),]
metadata <- metadata[idcount, on = 'id']
setorder(metadata, race, gender, count)
metadata[, min(count), by = c('gender', 'race')]
metadata <- metadata[count>100]
metadata[, ':='(grc = .N), by = c('gender', 'race')]

imagelist <- imagelist[metadata[, c('id', 'gender', 'race')], on = 'id']

table(imagelist$gender, imagelist$race)
table(metadata$gender, metadata$race)

# Test Data for Viz
set.seed(1234)
filterdata <- imagelist[, .SD[sample.int(count, 100)], by =  'id']
idlist <- metadata[, .SD[1:40], by =  c('gender', 'race')]
#idlist <- metadata[, .SD[sample.int(90, 40)], by =  c('gender', 'race')]
filterdata <- filterdata[id %in% idlist$id]
filterdata$count <- NULL
imagelist$count <- NULL
metadata$grc <- NULL
table(filterdata$gender, filterdata$race)

rm(idcount)
save.image('workspace.RData')

dummy <- t(table(idlist$gender, idlist$race))
pdf("ValidationSet.pdf", height=1, width=3)
grid.table(t(dummy))
dev.off()
rm(dummy)

# testset
filterdata$dest_file <- paste0(filterdata$id, '_', filterdata$filename)
filterdata$source <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/',
                            filterdata$id, '/', filterdata$filename)
filterdata$destination <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/MOSIP/data/testset')

subfolder_names <- unique(filterdata$destination)
for (j in 1:length(subfolder_names)){
  folder<-dir.create(subfolder_names[j])
}

file.copy(from = filterdata$source, to = file.path(filterdata$destination, filterdata$dest_file))

# test
filterdata$source <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/',
                            filterdata$id, '/', filterdata$filename)
filterdata$destination <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/MOSIP/data/test/',
                                 filterdata$id)

subfolder_names <- unique(filterdata$destination)
for (j in 1:length(subfolder_names)){
  folder<-dir.create(subfolder_names[j])
}

filterdata[, ':='(filecount = seq_len(.N)), by = c('id')] 
filterdata$filestr <- paste0('000', filterdata$filecount)
filterdata$filestr <- str_sub(filterdata$filestr, -4)
filterdata$filestr <- paste0(filterdata$filestr, '.jpg')

file.copy(from = filterdata$source, to = file.path(filterdata$destination, paste0(filterdata$id, '_', filterdata$filestr)))

setwd("~/Documents/Turing/GPU/MOSIP/data")
load("~/Documents/Turing/GPU/MOSIP/data/workspace.RData")

# Triplet data generator
set.seed(1234)
metadata <- imagelist[, list(count = .N), by = c('id', 'race', 'gender')]
tripletmeta <- metadata[!id %in% idlist$id]
table(tripletmeta$gender, tripletmeta$race)

# GENDER + RACE
b_gender <- 'f'
b_race <- 'white'

#gendids  <- tripletmeta[race == b_race]
#gendids  <- tripletmeta[gender == b_gender]
gendids  <- tripletmeta

setorder(gendids, -count)

#idlist1 <- gendids[gender == 'f', .SD[1:150]]  # Black
#idlist2 <- gendids[gender == 'm', .SD[1:150]]  # Black

#idlist1 <- gendids[gender == 'f', .SD[11:160]]  # Asian
#idlist2 <- gendids[gender == 'm', .SD[1:150]]   # Asian

#idlist1 <- gendids[gender == 'f', .SD[61:210]]  # Indian
#idlist2 <- gendids[gender == 'm', .SD[31:180]]   # Indian

#idlist1 <- gendids[gender == 'f', .SD[821:970]]  # White
#idlist2 <- gendids[gender == 'm', .SD[951:1100]] # White

#idlist <- rbind(idlist1, idlist2)
#idlist[, sum(count), by = c('gender')]

#idlist1 <- gendids[race == 'black', .SD[1:150]]
#idlist2 <- gendids[race == 'asian', .SD[11:160]]
#idlist3 <- gendids[race == 'indian', .SD[61:210]]
#idlist4 <- gendids[race == 'white', .SD[821:970]]

#idlist <- rbind(idlist1, idlist2, idlist3, idlist4)
#idlist[, sum(count), by = c('race')]

idlist1 <- gendids[race == 'black' & gender == 'f', .SD[1:150]]  # Black
idlist2 <- gendids[race == 'black' & gender == 'm', .SD[1:150]]  # Black
idlist3 <- gendids[race == 'asian' & gender == 'f', .SD[11:160]]  # Asian
idlist4 <- gendids[race == 'asian' & gender == 'm', .SD[1:150]]   # Asian
idlist5 <- gendids[race == 'indian' & gender == 'f', .SD[61:210]]  # Indian
idlist6 <- gendids[race == 'indian' & gender == 'm', .SD[31:180]]   # Indian
idlist7 <- gendids[race == 'white' & gender == 'f', .SD[821:970]]  # White
idlist8 <- gendids[race == 'white' & gender == 'm', .SD[951:1100]] # White

idlist <- rbind(idlist1, idlist2, idlist3, idlist4, idlist5, idlist6, idlist7, idlist8)
idlist[, sum(count), by = c('race')]

racedata <- imagelist[id %in% idlist$id]
racedata[, ':='(count = .N), by = 'id']
setorder(racedata, id)
racedata$count <- NULL

#### Paperspace write
racedata$source <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/VGGData/aligned/images/',
                          racedata$id, '/', racedata$filename)

racedata$destination <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/MOSIP/data/train/Full/', 
                               racedata$id)

copydata <- racedata[, c('source', 'destination', 'filename')]
copydata <- copydata[!duplicated(copydata),]

subfolder_names <- unique(copydata$destination)
for (j in 1:length(subfolder_names)){
  folder<-dir.create(subfolder_names[j])
}

file.copy(from = copydata$source, to = file.path(copydata$destination, copydata$filename))

# Validation
setwd("~/Documents/Turing/GPU/MOSIP/data")
load("~/Documents/Turing/GPU/MOSIP/data/workspace.RData")
####
path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/MOSIP/models/rep_Full_FACENET_3.csv')
python_data    <- fread(path, header = FALSE, skip = 1)
python_data$V2 <- substr(python_data$V2, 66, 90)
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

cl <- makeCluster(getOption("cl.cores", 15))
clusterExport(cl, list("play_data", "alldata", "pdist", "data.table", "setorder"))

# Identification rates
id.results = parSapply(cl, 1:100, FUN = function(z){
  #set.seed(z)
  #reference_set <- sample.int(100, 320, replace = TRUE)
  #reference_set <- reference_set + 100*seq(0, 319, 1)
  reference_set <- z + 100*seq(0, 319, 1)
  presentation_set <- (1:nrow(play_data))[!(1:nrow(play_data)) %in% reference_set]
  dist_mat  <- as.matrix(pdist(X = play_data, indices.A = presentation_set, indices.B = reference_set))
  id_set <- apply(dist_mat, 1, FUN = which.min)
  res_dt <- data.table(alldata[-reference_set, c('race', 'gender')], 
                       success = alldata$id[-reference_set] == alldata$id[reference_set][id_set])
  res_dt <- res_dt[, list(rate = mean(success)), by = c('race', 'gender')]
  setorder(res_dt, race, gender)
  
  return(res_dt$rate)
})

stopCluster(cl)

id.meta <- data.table(expand.grid(race = unique(idlist$race), gender = unique(idlist$gender)))
setorder(id.meta, race, gender)
id.results <- cbind(id.meta, id.results)
id.results <- id.results[, list(rate = rowMeans(.SD)), by = c('race', 'gender'), .SDcols = -c(1,2)]
setorder(id.results, rate)
id.results$rate <- round(id.results$rate, 3)

setwd("~/Documents/Turing/GPU/MOSIP/output")
pdf(paste0("FACENET_Full_3.pdf"), height=3, width=2.2)
grid.table(id.results)
dev.off()

# Validation - 10 imgs
setwd("~/Documents/Turing/GPU/MOSIP/data")
load("~/Documents/Turing/GPU/MOSIP/data/workspace.RData")
####
path  <- paste0('/Users/santhoshnarayanan/Documents/Turing/GPU/MOSIP/models/rep_Baseline_FACENET_casia.csv')
python_data    <- fread(path, header = FALSE, skip = 1)
python_data$V2 <- substr(python_data$V2, 66, 90)
python_data    <- python_data[,-1]
python_data$id <- tstrsplit(python_data$V2, "_", keep = 1)
python_data$filename <- substr(python_data$V2, 9, 20)
python_data <- python_data[,-1]
ncol        <- ncol(python_data)
python_data <- python_data[ , .SD , .SDcols = c(ncol-1, ncol, 1:(ncol-2))]
alldata     <- filterdata[python_data, on = c('id', 'filename'), nomatch=NULL]
setkeyv(alldata, c('id', 'filename'))
rm(list = c('python_data', 'path'))

# Subset 10 images per id
set.seed(1234)
alldata <- alldata[, .SD[sample.int(100, 10)], by =  'id']

play_data <- data.frame(alldata[,-c(1:4)])

cl <- makeCluster(getOption("cl.cores", 15))
clusterExport(cl, list("play_data", "alldata", "pdist", "data.table", "setorder"))

# Identification rates
id.results = parSapply(cl, 1:1000, FUN = function(z){
  set.seed(z)
  reference_set <- sample.int(10, 320, replace = TRUE)
  reference_set <- reference_set + 10*seq(0, 319, 1)
  #reference_set <- z + 10*seq(0, 319, 1)
  presentation_set <- (1:nrow(play_data))[!(1:nrow(play_data)) %in% reference_set]
  dist_mat  <- as.matrix(pdist(X = play_data, indices.A = presentation_set, indices.B = reference_set))
  id_set <- apply(dist_mat, 1, FUN = which.min)
  res_dt <- data.table(alldata[-reference_set, c('race', 'gender')], 
                       success = alldata$id[-reference_set] == alldata$id[reference_set][id_set])
  res_dt <- res_dt[, list(rate = mean(success)), by = c('race', 'gender')]
  setorder(res_dt, race, gender)
  
  return(res_dt$rate)
})

stopCluster(cl)

id.meta <- data.table(expand.grid(race = unique(idlist$race), gender = unique(idlist$gender)))
setorder(id.meta, race, gender)
id.results <- cbind(id.meta, id.results)
id.results <- id.results[, list(rate = rowMeans(.SD)), by = c('race', 'gender'), .SDcols = -c(1,2)]
setorder(id.results, rate)
id.results$rate <- round(id.results$rate, 3)

setwd("~/Documents/Turing/GPU/MOSIP/output_10")
pdf(paste0("FACENET_Baseine_casia.pdf"), height=3, width=2.2)
grid.table(id.results)
dev.off()
