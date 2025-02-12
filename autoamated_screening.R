library(stringr)
library(dplyr)
library(revtools)

MEDLINE <- list()

for (i in 20:28) {
  MEDLINE[[i]] <- revtools::read_bibliography(paste0("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/REDDIE/WP2/diabetes_review/MEDLINE-03-10-23/ris (", i, ").ris"))
}

medline <- dplyr::bind_rows(MEDLINE) |>
  dplyr::mutate(
    abstract = stringr::str_remove(abstract, "Copyright <c2><a9> .*")
  )

numbers <- list()

for (i in 1:nrow(medline)) {
  numbers[[i]] <- unlist(regmatches(medline$abstract[i], gregexpr("-?[[:digit:]]+(\\.[[:digit:]]+)?", medline$abstract[i])))
}

words <- list()

for (i in 1:nrow(medline)) {
  words[[i]] <- if(is.na(medline$abstract[i]) == TRUE) {
    TRUE
  } else if (sum(stringr::str_detect(medline$abstract[i], c("thousand", "million"))) > 0) {
    TRUE
  } else {
    FALSE
  }
}

max <- list()

for (i in 1:length(numbers)) {
  max[[i]] <- max(as.numeric(numbers[[i]]))
}

for (i in 1:length(max)) {
  if (max[[i]] > 1000) {
    max[[i]] <- TRUE
  } else {
    max[[i]] <- FALSE
  }
}

for (i in 1:length(max)) {
  max[[i]] <- max[[i]] + words[[i]]
  
  if (max[[i]] == 2) {
    max[[i]] <- 1
  }
}

sum(unlist(max))

medline$over_1000 <- unlist(max)

# CINAHL

CINAHL <- revtools::read_bibliography("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/REDDIE/WP2/diabetes_review/CINAHL (3).ris")

numbers <- list()

for (i in 1:nrow(CINAHL)) {
  numbers[[i]] <- unlist(regmatches(CINAHL$abstract[i], gregexpr("-?[[:digit:]]+(\\.[[:digit:]]+)?", CINAHL$abstract[i])))
}

words <- list()

for (i in 1:nrow(CINAHL)) {
  words[[i]] <- if(is.na(CINAHL$abstract[i]) == TRUE) {
    TRUE
  } else if (sum(stringr::str_detect(CINAHL$abstract[i], c("thousand", "million"))) > 0) {
    TRUE
  } else {
    FALSE
  }
}

max <- list()

for (i in 1:length(numbers)) {
  max[[i]] <- max(as.numeric(numbers[[i]]))
}

for (i in 1:length(max)) {
  if (max[[i]] > 1000) {
    max[[i]] <- TRUE
  } else {
    max[[i]] <- FALSE
  }
}

for (i in 1:length(max)) {
  max[[i]] <- max[[i]] + words[[i]]
  
  if (max[[i]] == 2) {
    max[[i]] <- 1
  }
}

sum(unlist(max))

CINAHL$over_1000 <- unlist(max)

# SCOPUS

scopus <- revtools::read_bibliography("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/REDDIE/WP2/diabetes_review/scopus (6).csv") |>
  dplyr::mutate(
    abstract = stringr::str_remove(abstract, "© .*")
  )

scopus <- revtools::read_bibliography("C:/Users/jjct8/OneDrive - Loughborough University/Desktop/deduped_refs.txt")

numbers <- list()

for (i in 1:nrow(scopus)) {
  numbers[[i]] <- unlist(regmatches(scopus$abstract[i], gregexpr("-?[[:digit:]]+(\\.[[:digit:]]+)?", scopus$abstract[i])))
}

words <- list()

for (i in 1:nrow(scopus)) {
  words[[i]] <- if(is.na(scopus$abstract[i]) == TRUE) {
    TRUE
  } else if (sum(stringr::str_detect(scopus$abstract[i], c("thousand", "million"))) > 0) {
    TRUE
  } else {
    FALSE
  }
}

max <- list()

for (i in 1:length(numbers)) {
  max[[i]] <- max(as.numeric(numbers[[i]]))
}

for (i in 1:length(max)) {
  if (max[[i]] > 1000) {
    max[[i]] <- TRUE
  } else {
    max[[i]] <- FALSE
  }
}

for (i in 1:length(max)) {
  max[[i]] <- max[[i]] + words[[i]]
  
  if (max[[i]] == 2) {
    max[[i]] <- 1
  }
}

sum(unlist(max))

scopus$over_1000 <- unlist(max)

final_data <- plyr::rbind.fill(medline, CINAHL, scopus)

final_data <- final_data |>
  dplyr::filter(over_1000 == 1)

revtools::write_bibliography(final_data[, 1:14], "all_final_data.ris", format = "ris")

x <- revtools::read_bibliography("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/REDDIE/WP2/diabetes_review/final_data.ris")

# final data
# 
# final_data <- revtools::read_bibliography("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/WP2/diabetes_review/diabetes_review-02-10.txt") |>
#   dplyr::mutate(
#     abstract = stringr::str_remove(abstract, "© .*")
#   )
# 
# numbers <- list()
# 
# for (i in 1:nrow(scopus)) {
#   numbers[[i]] <- unlist(regmatches(scopus$abstract[i], gregexpr("-?[[:digit:]]+(\\.[[:digit:]]+)?", scopus$abstract[i])))
# }
# 
# words <- list()
# 
# for (i in 1:nrow(scopus)) {
#   words[[i]] <- if(is.na(scopus$abstract[i]) == TRUE) {
#     TRUE
#   } else if (sum(stringr::str_detect(scopus$abstract[i], c("thousand", "million"))) > 0) {
#     TRUE
#   } else {
#     FALSE
#   }
# }
# 
# max <- list()
# 
# for (i in 1:length(numbers)) {
#   max[[i]] <- max(as.numeric(numbers[[i]]))
# }
# 
# for (i in 1:length(max)) {
#   if (max[[i]] > 1000) {
#     max[[i]] <- TRUE
#   } else {
#     max[[i]] <- FALSE
#   }
# }
# 
# for (i in 1:length(max)) {
#   max[[i]] <- max[[i]] + words[[i]]
#   
#   if (max[[i]] == 2) {
#     max[[i]] <- 1
#   }
# }
# 
# sum(unlist(max))
# 
# scopus$over_1000 <- unlist(max)
# 
# final_data <- plyr::rbind.fill(medline, CINAHL, scopus)
# 
# final_data <- final_data |>
#   dplyr::filter(over_1000 == 1)
# 
# revtools::write_bibliography(final_data[, 1:14], "final_data.ris", format = "ris")

# Updated search

MEDLINE <- list()

for (i in 1:2) {
  MEDLINE[[i]] <- revtools::read_bibliography(paste0("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/REDDIE/WP2/diabetes_review/MEDLINE_UPDATED/ris-", i,".ris"))
}

MEDLINE[[1]] <- revtools::read_bibliography("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/REDDIE/WP2/diabetes_review/MEDLINE_UPDATED/ris-1.ris")
MEDLINE[[2]] <- revtools::read_bibliography("C:/Users/jjct8/OneDrive - University of Leicester/Leicester/REDDIE/WP2/diabetes_review/MEDLINE_UPDATED/ris-2.ris")

medline <- dplyr::bind_rows(MEDLINE) |>
  dplyr::mutate(
    abstract = stringr::str_remove(abstract, "Copyright <c2><a9> .*")
  )

numbers <- list()

for (i in 1:nrow(medline)) {
  numbers[[i]] <- unlist(regmatches(medline$abstract[i], gregexpr("-?[[:digit:]]+(\\.[[:digit:]]+)?", medline$abstract[i])))
}

words <- list()

for (i in 1:nrow(medline)) {
  words[[i]] <- if(is.na(medline$abstract[i]) == TRUE) {
    TRUE
  } else if (sum(stringr::str_detect(medline$abstract[i], c("thousand", "million"))) > 0) {
    TRUE
  } else {
    FALSE
  }
}

max <- list()

for (i in 1:length(numbers)) {
  max[[i]] <- max(as.numeric(numbers[[i]]))
}

for (i in 1:length(max)) {
  if (max[[i]] > 1000) {
    max[[i]] <- TRUE
  } else {
    max[[i]] <- FALSE
  }
}

for (i in 1:length(max)) {
  max[[i]] <- max[[i]] + words[[i]]
  
  if (max[[i]] == 2) {
    max[[i]] <- 1
  }
}

sum(unlist(max))

medline$over_1000 <- unlist(max)

medline_FINAL <- medline |>
  dplyr::filter(over_1000 == 1)

revtools::write_bibliography(medline_FINAL[, 1:14], "UPDATED_FINAL.ris", format = "ris")
