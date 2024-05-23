# Create Gene x p-Value Data
# Gabriel Odom
# 2024-05-23

data(golub, package = "multtest")
dim(golub)


pVals_num <- apply(golub, MARGIN = 1, FUN = function(xRow){
  t.test(x = xRow[1:19], y = xRow[20:38])$p.value
})
names(pVals_num) <- golub.gnames[, 3]
head(pVals_num)
saveRDS(pVals_num, file = "data/02_golub_pVals_20240523.rds")
