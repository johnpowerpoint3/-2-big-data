library(tidyverse)


data <- read_csv("communities_data.csv", col_names = FALSE, na= "?")


data_names <- read_csv("communities.csv")


data_names_clean <-  data_names  %>% 
  slice(57:184)
  

data_names_clean <- data_names_clean %>% 
  mutate(`Title: Communities and Crime`= 
           str_replace_all(`Title: Communities and Crime`,
                           "@attribute|numeric", ""))






new_colnames <- pull(data_names_clean, `Title: Communities and Crime`)

colnames(data) <- new_colnames

summary(data)


data_clean <- data %>% 
  select(-c(1:5))

summary(data_clean)


data_clean <- data_clean %>% 
  drop_na()







pca_cov <- prcomp(data_clean, center = TRUE, scale. = FALSE)


summary(pca_cov)



eigenvalues <- pca_cov$sdev^2


eigenvectors <- pca_cov$rotation

head(eigenvectors, 3)   
tail(eigenvectors, 3)   





explained_variance <- eigenvalues / sum(eigenvalues)
explained_variance_percent <- explained_variance * 100

explained_variance_percent


explained_df <- data.frame(
  PC = paste0("PC", seq_along(eigenvalues)),
  Eigenvalue = eigenvalues,
  ExplainedVariance = explained_variance_percent,
  CumulativeVariance = cumsum(explained_variance_percent)
)












