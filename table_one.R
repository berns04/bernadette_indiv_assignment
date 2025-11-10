#------------------------------------------
# Libraries
#------------------------------------------
library(tableone)
library(dplyr)
library(kableExtra)

# load cleaned dataset
bdhs_cl <- read.csv("Data/02-result.csv")

#------------------------------------------
# Variable lists
#------------------------------------------
exp_cont = c("m_age", "m_first_birth", "m_BMI", "c_height")

exp_fctr = c("c_birth_order",
             "c_diarrhea",
             "c_fever",
             "c_cough",
             "c_vit_a",
             "m_education",
             "m_job",
             "sd_f_education",
             "sd_residence",
             "sd_region",
             "sd_religion",
             "sd_household_size",
             "sd_wealth_index",
             "sd_toilet")

exp = c(exp_cont, exp_fctr)
dep_uw_binary = "o_binary_uw"

# ensure correct factor types
bdhs_cl[exp_fctr] <- lapply(bdhs_cl[exp_fctr], as.factor)
bdhs_cl[[dep_uw_binary]] <- as.factor(bdhs_cl[[dep_uw_binary]])

#------------------------------------------
# Table 1: stratified by underweight status
#------------------------------------------
tableOne_uw <- CreateTableOne(
  vars = exp,
  strata = dep_uw_binary,
  data = bdhs_cl,
  test = FALSE
)

# Convert to matrix for kable
table1_mat <- print(tableOne_uw,
                    nonnormal = exp_cont,
                    smd = TRUE,
                    quote = FALSE,
                    noSpaces = TRUE,
                    printToggle = FALSE)

#------------------------------------------
# Nice display using kableExtra
#------------------------------------------
kbl(table1_mat,
    caption = "Table 1. Maternal, Child, and Socioeconomic Characteristics by Underweight Status",
    align = "lccc",
    booktabs = TRUE) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  row_spec(0, bold = TRUE, background = "#d9edf7") %>%
  column_spec(1, bold = TRUE, width = "4cm") %>%
  add_header_above(c(" " = 1, "Not Underweight" = 1, "Underweight" = 1, "SMD" = 1)) %>%
  save_kable("Data/03-table1.html")  # saves as a clean HTML table
install.packages("webshot2")
library(webshot2)

# Then run your conversion again:
webshot("Data/03-table1.html", "Data/03-table1.png", zoom = 2)
