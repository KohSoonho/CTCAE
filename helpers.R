library(tidyverse)

# CTCAE5 Table ------------------------------------------------------------

CTCAE5 <- read_csv("CTCAE_v5.0.csv")

CTCAEv5 <- CTCAE5 %>% 
  rename(MedDRA_Code = "MedDRA Code", 
         MedDRA_SOC = "MedDRA SOC", 
         CTCAE_Term = "CTCAE Term", 
         Grade_1 = "Grade 1 ??", 
         Grade_2 = "Grade 2 ??", 
         Grade_3 = "Grade 3 ??", 
         Grade_4 = "Grade 4 ??", 
         Grade_5 = "Grade 5 ??", 
         Definition = "Definition", 
         Navigational_Note = "Navigational Note", 
         CTCAEv5.0_Change = "CTCAE v5.0 Change")

# CTCAE4 Table ------------------------------------------------------------

CTCAE4 <- read_csv("CTCAE_4.03.csv")

CTCAEv4 <- CTCAE4 %>% 
  rename(MedDRA_v12.0 = "MedDRA v12.0 Code", 
         CTCAE_v4.0_SOC = "CTCAE v4.0 SOC", 
         CTCAE_v4.0_Term = "CTCAE v4.0 Term", 
         Grade_1 = "Grade 1 ??", 
         Grade_2 = "Grade 2 ??", 
         Grade_3 = "Grade 3 ??", 
         Grade_4 = "Grade 4 ??", 
         Grade_5 = "Grade 5 ??", 
         Definition = "CTCAE v4.0 AE Term Definition", 
         Errata = "Errata")


# Select Grade ------------------------------------------------------------

select_grade <- function(df, x, y) {
 n <- ncol(select(df, 1:Grade_1))
 all <- 1:5
 range <- seq(x, y, by = 1)
 delete <- n + all[-range] -1
 select(df, -delete)
}

