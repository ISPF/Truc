library(data.table)

lst <- list.files("J:/Diffusion/2 - Groupe/metis/Publications", pattern="*.R", full.names=T, recursive = T)
lst <- grep("createGraphe", lst, value = T)

lst2 <- gsub("J:/Diffusion/2 - Groupe/metis/Publications/", "", lst)
lst2 <- gsub("2018/1", "1", lst2)
lst2 <- gsub("2019/1", "1", lst2)
lst2 <- gsub("2020/1", "1", lst2)
lst2 <- gsub("2021/1", "1", lst2)
lst2 <- gsub("2022/1", "1", lst2)
lst2 <- gsub("Points Référence/", "", lst2)
lst2 <- gsub("Points Etudes et Bilans/", "", lst2)
lst2 <- gsub("Points Conjoncture/", "", lst2)
lst2 <- gsub("/", "-", lst2)

lst2 <- paste0("S03E01/src/", lst2)

for(i in seq_along(lst))
  file.copy(lst[i], lst2[i])
