# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

# Load the data
survey_df <- read.csv('/Users/niteshreddy/Downloads/A4 DATA/Survey.csv', header = TRUE)
dim(survey_df)
names(survey_df)
head(survey_df)
str(survey_df)

# A) Do principal component analysis and factor analysis and identify the dimensions in the data.

# Check for NA values
sum(is.na(survey_df))

# Select the relevant columns for PCA and factor analysis
sur_int <- survey_df[, 20:46]
str(sur_int)
dim(sur_int)

# Perform PCA using psych package
pca_psych <- principal(sur_int, 5, n.obs = 162, rotate = "promax")
pca_psych

# Perform Omega hierarchical analysis
om.h <- omega(sur_int, n.obs = 162, sl = FALSE)
op <- par(mfrow = c(1, 1))
om <- omega(sur_int, n.obs = 162)

# Perform PCA using FactoMineR package
pca_fm <- PCA(sur_int, scale.unit = TRUE)
summary(pca_fm)

# Biplot using factoextra
fviz_pca_biplot(pca_fm, repel = TRUE)

# Show the structure and dimensions of the selected columns
str(sur_int)
dim(sur_int)
show(sur_int)
