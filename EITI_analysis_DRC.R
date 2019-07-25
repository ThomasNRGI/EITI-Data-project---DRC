
  
#starting to use EITI summary table
library(tidyverse)
library(nrgiR)
library(openxlsx)
library(stringr)
library(forcats)
eitiDT <- data.table(get_eiti_company())

#Limit the database to one country: Democratic Republic of Congo
drcDT <- eitiDT %>%
  filter(country == "Democratic Republic of Congo") %>%
  mutate(year=as.numeric(year)) %>%
  mutate(commodities=as.factor(commodities)) %>%
  select(-c("created", "changed", "iso3", "start_date", "end_date" , "reporting_url")) %>%
  arrange(company_name)

setDT(drcDT)
summary(drcDT) 
str(drcDT)

#clean data: company names

#step 1: create a single-column data.frame from drcDT$company_name
drc_companies <- unique(drc_eiti$company_name) 
write.xlsx(drc_companies, "drc_companies.xlxs")

#Step 2: manually change drc_companies.xlxs to add clean names to second column - save as "drc_companies_clean.xlxs"
#note: all ivanhoe payments are attributed to Kapushi

#Step 3: merge "drc_companies_clean.xlxs" with drcDT and replace the original company_name column
drc_companies_clean <- read.xlsx("drc_companies_clean.xlsx")
drc_companies_clean$company_name_clean <- str_trim (drc_companies_clean$company_name_clean)
drc_companies_clean$company_name_clean <- str_squish (drc_companies_clean$company_name_clean)

drcDT <- inner_join(drcDT , drc_companies_clean) %>%
  select ( -company_name) %>%
  rename ("company_name" = "company_name_clean" ) %>%
  arrange(name_of_revenue_stream)

#clean data: name of revenue stream, gfs code and gfs description

#step 1: create a single-column data.frame from drcDT$name_of_revenue_stream
drc_revenue <- unique(drc_eiti$name_of_revenue_stream)
write.xlsx(drc_revenue, "drc_revenue.xlxs")

#Step 2: manually change drc_revenue.xlxs to add clean names to second column, gfs code to third column and gfs description to fourth column
#save as "drc_revenue_clean.xlxs"

#Step 3: merge "drc_revenue_clean.xlxs" with drcDT and replace the original gfs_code, gfs_description and name_of_revenue_stream columns
drc_revenue_clean <- read.xlsx("drc_revenue_clean.xlsx")
drc_revenue_clean$name_of_revenue_stream_clean <- str_trim (drc_revenue_clean$name_of_revenue_stream_clean )

drcDT <- inner_join(drcDT , drc_revenue_clean) %>%
 select ( -c(gfs_code, gfs_description ,  name_of_revenue_stream)) %>%
  rename ("gfs_code" = "gfs_code_clean" ) %>%
  rename ("gfs_description" = "gfs_description_clean" ) %>%
  rename ("name_of_revenue_stream" = "name_of_revenue_stream_clean" ) %>%
  mutate(gfs_description=as.factor(gfs_description)) %>%
  mutate(gfs_code=as.factor(gfs_code)) %>%
  mutate(name_of_revenue_stream=as.factor(name_of_revenue_stream)) %>%
  arrange(company_name)

# QUESTION FOR HARI: Identify the top N payers in the EITI database, and group all other companies together as "other companies"
fct_lump(drcDT$company_name, 10, other_level = "Other companies",
         ties.method = c("min", "average", "first", "last", "random", "max"))
drc_data %>% 
  group_by(company_name) %>% 
  mutate(total_pay = sum(value_reported_as_USD, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(company_group = if_else(total_pay > your_threshold, company_name, "Other"))

#Description 1: larger taxpayers overall
drcDT [sector == "Mining"] %>%
  group_by (company_name) %>%
  summarise (PYT = sum(value_reported_as_USD)) %>%
  filter(PYT > 150000000) %>%
  mutate(company_name = fct_reorder(company_name, PYT)) %>%
  ggplot ( aes (x = 1 , y = PYT , fill = company_name )) +
  geom_bar(position = "dodge" , stat = "identity" , colour="black") +
  scale_fill_manual(values = c(nrgi_colors(),"black", "red", "blue", "purple" , "navy" , "orange"), na.value = nrgi_colors()[5]) +
  nrgi_theme(base_family = "Lucida Sans Unicode") +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Top mining taxpayers according to EITI reports", x = "", y = "Cumulative payments")

#Description 2: larger revenue streams overall
drcDT [sector == "Mining"] %>%
 group_by (name_of_revenue_stream) %>%
 summarise (PYT = sum(value_reported_as_USD)) %>%
  setorder(PYT) %>%
  ggplot ( aes (x = name_of_revenue_stream , y = PYT )) +
  geom_bar(stat = "identity" , colour="black") +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 90))

drcDT [sector == "Mining"]  %>%
  group_by (gfs_description) %>%
  summarise (PYT = sum(value_reported_as_USD)) %>%
  filter(PYT > 50000000) %>%
  ggplot ( aes (x = 1 , y = PYT , fill = gfs_description )) +
  geom_bar(stat = "identity" , colour="black") +
  scale_fill_manual(values = c(nrgi_colors(),"#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), na.value = nrgi_colors()[5]) +
  nrgi_theme(base_family = "Lucida Sans Unicode") +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Most important payments from mining companies according to EITI reports", x = "", y = "Cumulative payments")

#Description 3: larger payments by year and by companies
drcDT[company_name == "Kamoto Copper Company (KCC)"
     |company_name == "Mutanda ya Mukonkota Mining (MUMI) (ex-Kansuki)" 
     |company_name == "Tenke Fungurume Mining (TFM)"
     |company_name == "BOSS MINING"
     |company_name == "MMG Kinsevere (ex Anvil Mining Concentrate Kinsevere) (MMG (ex-AMCK))"
     |company_name == "FRONTIER"
     |company_name == "KIBALI GOLDMINES SA"
     |company_name == "Compagnie de Traitement des Rejets de Kingamyambo (METALKOL)"
     |company_name == "KIPUSHI CORPORATION (KICO)"
     |company_name == "Kamoa Copper (ex African Minerals Barbados)"
     |company_name == "RUASHI MINING SAS"
     |company_name == "LA SINO-CONGOLAISE DES MINES (SICOMINES)" 
     |company_name == "Soci?t? d'exploitation de Kipoi (SEK)"
     ,           ] %>%
  group_by (year , company_name ) %>%
  summarise (PYT = sum(value_reported_as_USD)) %>%
  ggplot ( aes (x = year , y = PYT , fill = company_name)) +
   geom_bar(stat = "identity" , colour="black") +
   scale_fill_manual(values = c(nrgi_colors(),"black", "red", "blue", "purple" , "navy" , "orange"), na.value = nrgi_colors()[5]) +
   nrgi_theme(base_family = "Lucida Sans Unicode") +
   expand_limits(y = 0)

#Description 4: larger payments by year and by payment type
drcDT[company_name == "Kamoto Copper Company (KCC)"
         ,           ] %>%
  group_by (year , gfs_description , company_name) %>%
  summarise (PYT = sum(value_reported_as_USD)) %>%
   ggplot (  aes (x = year , y = PYT , fill = gfs_description)) +
  geom_bar(stat = "identity" , colour="black") +
  scale_fill_manual(values = c(nrgi_colors(),"black", "red", "blue", "purple" , "navy" , "orange" , "yellow"), na.value = nrgi_colors()[5]) +
  nrgi_theme(base_family = "Lucida Sans Unicode") +
  expand_limits(y = 0) +
  facet_wrap(~company_name)


#Add commodity prices and company production values through lookup tables



