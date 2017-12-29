library(recosystem)

train_set = data_file(system.file("dat", "smalltrain.txt", package = "recosystem"))
test_set  = data_file(system.file("dat", "smalltest.txt",  package = "recosystem"))
r = Reco()
opts = r$tune(train_set, opts = list(dim = c(10, 20, 30, 40), lrate = c(0.1, 0.2,0.3), costp_l1 = c(0,0.1), costq_l1 = 0,nthread = 5, niter = 10))
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))
pred_file = tempfile()
r$predict(test_set, out_file(pred_file))
print(scan(pred_file, n = 10))
pred_rvec = r$predict(test_set, out_memory())

max(pred_rvec)
min(pred_rvec)


test_final <- read.table("~/resources/common/R/Library/recosystem/dat/smalltest.txt",sep=" ")
test_final$rating <- pred_rvec

plot(test_final)
