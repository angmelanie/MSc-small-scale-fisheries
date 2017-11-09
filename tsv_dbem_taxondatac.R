# export into .txt files
?write_csv
dbem_master <- read_csv("~/MSc-small-scale-fisheries/DBEM/TaxonDataC_master.csv", col_names = FALSE)
View(dbem_master)
dim(dbem_master)

for (i in 1:249){
  txtfile = data.frame(matrix(0, ncol = 2, nrow = 24))
  
  txtfile[,1] <- dbem_master[,1]
  txtfile[,2] <- dbem_master[,i+1]
  
  name <- paste("C:/cygwin64/home/angmel/Fortran/Data/TaxonDataC/", dbem_master[1,i+1], ".txt", sep = "")
  
  write_tsv(txtfile, name, append = FALSE, col_names = FALSE)
}

# CHECKS
# read_tsv(name)
# read_tsv("C:/cygwin64/home/angmel/Fortran/Data/TaxonDataC/601900.txt")
