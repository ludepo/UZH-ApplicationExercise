########################################################################################################################
# Title     : Exercise Research Assistantship Mathilde
# Created by: Luca Poll
# Created on: 07.10.2020
########################################################################################################################

#***********************************************************************************************************************
### 1) Load libraries **************************************************************************************************
#***********************************************************************************************************************
library("tidyverse")        # for general data modification
library("haven")            # read .dta data
library("psych")            # to describe data
library("xtable")           # to export tables to LaTeX
library("car")              # to plot data
library("treemap")          # to plot the treemap
library("ggforce")          # to use "facet_zoom"
library("gplots")           # to plot means with CI
library("plm")              # for econometric analysois
library("stargazer")        # export regression output to LaTeX

#***********************************************************************************************************************
### 2) Import files, create and modify/clean panel *********************************************************************
#***********************************************************************************************************************
# define path of files (note that inupt needs to end with /):
path <- "C:/Users/Luca Poll/Desktop/Dropbox/RA Zurich - Exercise/Test_Data/"

# get list of file names:
filenames <- list.files(path=path, pattern='.dta', all.files = FALSE)

# use file names to load individual data frames of .dta files:
for (i in filenames){
  name <- str_remove(i, ".dta")
  assign(name,value = read_dta(str_c(path, i)))
}

# combine baci92 dataframes to only one
baci92 <- bind_rows(baci92_2005, baci92_2006, baci92_2007, baci92_2008, baci92_2009, baci92_2010)

# Store country codes of relevant import and export partners:
imp_countr <- as.integer(c(100, 203, 348, 616, 642, 703, 705, 643, 112, 233, 428, 440, 498, 804, 31, 268, 398, 417,
                           762, 795, 860))
exp_countr <- as.integer(c(251, 842, 826, 528, 156, 40, 381, 757, 616, 58))
ger <- 276
china <- 156

# Clean data frame for only relevant observations and change format of some variables:
baci92 <- baci92 %>%
  mutate(withger = ifelse((.$i == ger | .$j == ger), 1, 0),  # create dummy if Germany is transaction partner
         relevantcountry = ifelse((.$i %in% c(imp_countr, exp_countr) |   # dummy if relevant country involved
                                   .$j %in% c(imp_countr, exp_countr)), 1, 0)) %>%
  filter(withger == 1, relevantcountry == 1) %>% # keep only observations that involve germany and relevant countries
  mutate(t = as.integer(t),
         hs6 = as.integer(hs6))

# Create panel dataset:
baci92 <- baci92 %>%
  mutate(import = ifelse(i == ger,as.integer(0), as.integer(1)), # indicator if trade is import or export (for germany)
         country = as.integer(ifelse(i != ger, .$i, .$j))) %>% # variable identifying Germanys trading partner
  mutate(impcountr = ifelse(.$country %in% imp_countr, as.integer(1), as.integer(0))) %>% #dummy type of partner country
  select(c(country, t, import, hs6, v, impcountr))

# Change values from USD to EUR:
t <- as.integer(c(2005, 2006, 2007, 2008, 2009, 2010))
eurinusd <- as.numeric(c(1.2441, 1.2556, 1.3705, 1.4708, 1.3948, 1.3257))
eurusd <- data.frame(t, eurinusd) # data frame containing yearly exchange rates
baci92 <- baci92 %>%
  left_join(., eurusd, by = "t") %>% # join exchange rates to baci91 data frame
  mutate(v = v/eurinusd) %>% # devide USD values by USD/EUR rate to obtain EUR value
  select(-eurinusd) # delete column with exchange rates again

# Load and clean file containing Consumer Price Index for Euro:
cpi <- read.csv(str_c(path, "ICPH_Eurostat-2.csv"), sep = ";", dec = ",") #values in spreadsheet are European format (,)
cpi <- cpi %>%
  select(c(TIME, Base.2005)) %>% # use only the year and the respective cpi relative to 2005
  rename(t = TIME, base2005 = Base.2005) %>%
  mutate(base2005 = as.numeric(base2005)/100,
         t = as.integer(.$t)) %>%
  filter(.$t < 2011 & .$t > 2004) # just keep the CPIs for the relevant years (2005-2010)

# Merge CPI data with baci92 and adjust values for inflation:
baci92 <- baci92 %>%
  left_join(., cpi, by = "t") %>% # join CPI of respective year to baci92 data
  mutate(v = v/base2005) %>% # compute CPI adjusted values
  select(-base2005) # delete CPIs after modification again

# Aggregate values of trade flows by year, country, product and import/export:
baci92 <- baci92 %>%
  group_by(t, country, hs6, import, impcountr) %>% # define groups
  summarize(v = sum(v)) # sum the values for the given groups (NOTE: no change since dataset is already aggregated)

# Prepare data frame on WZ93 industries and merge to baci92:
wz93 <- cw_HS_WZ93_3_final2 %>%
  mutate(hs6 = as.integer(hs6),
         wz93_3 = as.integer(wz93_3)) %>%
  rename(wz = wz93_3) %>%
  distinct(hs6, .keep_all = TRUE) # Dealing with duplicates: keep only one observation per hs6 code
baci92 <- baci92 %>%
  left_join(., wz93 , by = "hs6") %>% # merge wz classifications to data
  select(country, t, wz, hs6, import, impcountr, v, hs92_labels, temp_weight) %>% # change order of variables
  arrange(., country, t, wz, hs6) # arrange order of rows

### TROUBESHOOTING: there are 204015 additional observations being created when merging wz93 and baci92 ################
length(unique(baci92$hs6)) # : there are 4911 different products in baci92
length(wz93$hs6)           # : there are 6102 hs6 codes in WZ (note: number without "distinct" in line 95)
length(unique(wz93$hs6))   # : but only 5020 different ones
# ===> filtering out hs6 codes that have several entries shows that there are identical duplicates that could not
#      be detected by a simple distinct(.) or unique(.)
# ===> action taken: add "distinct(hs6, .keep_all = TRUE)" in wz93 modification
#-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -



#***********************************************************************************************************************
### 3) Exercise 1: Descriptive statistics ******************************************************************************
#***********************************************************************************************************************

### 3.1 Trade volume by year and trading partner type ##################################################################
# Descriptive statistics for imports from import partner countries per year
sumstatII <- baci92 %>% filter(impcountr == 1, import == 1) # select only relevant observations
sumstatII <- as.data.frame(psych::describeBy(sumstatII$v, group = sumstatII$t, mat = TRUE, digits = 2)) # summary stats
sumstatII <- sumstatII %>%
  select(group1, n, median, sd, range) %>% # select only most important measures
  rename(year = group1,
         nII = n,
         medianII = median,
         sdII = sd,
         rangeII = range)

# Descriptive statistics for exports to import partner countries per year
sumstatIE <- baci92 %>% filter(impcountr == 1, import == 0) # select only relevant observations
sumstatIE <- as.data.frame(psych::describeBy(sumstatIE$v, group = sumstatIE$t, mat = TRUE, digits = 2))
sumstatIE <- sumstatIE %>%
  select(group1, n, median, sd, range) %>%
  rename(year = group1,
         nIE = n,
         medianIE = median,
         sdIE = sd,
         rangeIE = range)

# Descriptive statistics for imports from export partner countries (excl. china) per year
sumstatEI <- baci92 %>% filter(impcountr == 0 & country != china, import == 1) # select only relevant observations
sumstatEI <- as.data.frame(psych::describeBy(sumstatEI$v, group = sumstatEI$t, mat = TRUE, digits = 2))
sumstatEI <- sumstatEI %>%
  select(group1, n, median, sd, range) %>%
  rename(year = group1,
         nEI = n,
         medianEI = median,
         sdEI = sd,
         rangeEI = range)

# Descriptive statistics for exports to export partner countries (excl. china) per year
sumstatEE <- baci92 %>% filter(impcountr == 0 & country != china, import == 0) # select only relevant observations
sumstatEE <- as.data.frame(psych::describeBy(sumstatEE$v, group = sumstatEE$t, mat = TRUE, digits = 2))
sumstatEE <- sumstatEE %>%
  select(group1, n, median, sd, range) %>%
  rename(year = group1,
         nEE = n,
         medianEE = median,
         sdEE = sd,
         rangeEE = range)

# Descriptive statistics for imports from China per year
sumstatCI <- baci92 %>% filter(country == china, import == 1) # select only relevant observations
sumstatCI <- as.data.frame(psych::describeBy(sumstatCI$v, group = sumstatCI$t, mat = TRUE, digits = 2))
sumstatCI <- sumstatCI %>%
  select(group1, n, median, sd, range) %>%
  rename(year = group1,
         nCI = n,
         medianCI = median,
         sdCI = sd,
         rangeCI = range)

# Descriptive statistics for exports to China per year
sumstatCE <- baci92 %>% filter(country == china, import == 0) # select only relevant observations
sumstatCE <- as.data.frame(psych::describeBy(sumstatCE$v, group = sumstatCE$t, mat = TRUE, digits = 2))
sumstatCE <- sumstatCE %>%
  select(group1, n, median, sd, range) %>%
  rename(year = group1,
         nCE = n,
         medianCE = median,
         sdCE = sd,
         rangeCE = range)

# Merge descriptive statistics for imports and exports from/to import partner countries
sumstatI <- sumstatII %>%
  left_join(sumstatIE, by = "year")

# Merge descriptive statistics for imports and exports from/to export partner countries
sumstatE <- sumstatEI %>%
  left_join(sumstatEE, by = "year")

# Merge descriptive statistics for imports and exports from/to China
sumstatC <- sumstatCI %>%
  left_join(sumstatCE, by = "year")

# Obtain Latex code of tables
xtable(sumstatI, caption = "Descriptive Statistics for main import partners per year", digits = 1)
xtable(sumstatE, caption = "Descriptive Statistics for main export partners per year", digits = 1)
xtable(sumstatC, caption = "Descriptive Statistics for China per year", digits = 1)

# Investigate graphically the distribution of the values of transactions for import partners:
ggplot(baci92 %>% ungroup() %>% filter(impcountr == 1 & v < 3000),
       aes(x = v, colour = factor(import), fill = factor(import))) +
  geom_density(alpha=0.4) +
  facet_zoom(x = v <= 200 & v >= 0, horizontal = TRUE) +
  scale_y_continuous(name = "density", limits = c(0,0.0125)) +
  scale_x_continuous(name = "value", labels = scales::comma) +
  scale_fill_discrete(name = NULL, labels = c("Exports", "Imports")) +
  guides(color = FALSE) +
  theme_bw()

# Compute the sum of the overall transactions for the given subgroups:
sum(baci92$v[baci92$impcountr == 1 & baci92$import == 1])
sum(baci92$v[baci92$impcountr == 1 & baci92$import == 0])
sum(baci92$v[baci92$impcountr == 0 & baci92$country != china & baci92$import == 1])
sum(baci92$v[baci92$impcountr == 0 & baci92$country != china & baci92$import == 0])


### 3.2 Trade volume by HS classification section and trading partner type #############################################
# Match hs6 classifications with respective section and aggregate values by type of country, import/export and section:
baci92 <- baci92 %>%
  mutate(hs2section = ifelse(hs6 < 60000, "Section I",
                       ifelse(hs6 > 5999 & hs6 < 150000, "Section II",
                        ifelse(hs6 > 149999 & hs6 < 160000, "Section III",
                         ifelse(hs6 > 159999 & hs6 < 250000, "Section IV",
                          ifelse(hs6 > 249999 & hs6 < 280000, "Section V",
                           ifelse(hs6 > 279999 & hs6 < 390000, "Section VI",
                            ifelse(hs6 > 389999 & hs6 < 410000, "Section VII",
                             ifelse(hs6 > 409999 & hs6 < 440000, "Section VIII",
                              ifelse(hs6 > 439999 & hs6 < 470000, "Section IX",
                               ifelse(hs6 > 469999 & hs6 < 500000, "Section X",
                                ifelse(hs6 > 499999 & hs6 < 640000, "Section XI",
                                 ifelse(hs6 > 639999 & hs6 < 680000, "Section XII",
                                  ifelse(hs6 > 679999 & hs6 < 710000, "Section XIII",
                                   ifelse(hs6 > 709999 & hs6 < 720000, "Section XIV",
                                    ifelse(hs6 > 719999 & hs6 < 840000, "Section XV",
                                     ifelse(hs6 > 839999 & hs6 < 860000, "Section XVI",
                                      ifelse(hs6 > 859999 & hs6 < 900000, "Section XVII",
                                       ifelse(hs6 > 899999 & hs6 < 930000, "Section XVIII",
                                        ifelse(hs6 > 929999 & hs6 < 940000, "Section XIX",
                                         ifelse(hs6 > 939999 & hs6 < 970000, "Section XX",
                                          ifelse(hs6 > 969999, "Section XXI", "ERROR")
                                         )))))))))))))))))))))

# Create table for descriptive statistics:
# Descriptive statistics for imports from import partner countries
descrII <- baci92 %>% filter(impcountr == 1, import == 1) # select only relevant observations
descrII <- as.data.frame(describeBy(descrII$v, group = descrII$hs2section, mat = TRUE, digits = 2))
descrII <- descrII %>%
  select(group1, n, median, sd, range) %>%
  rename(Section = group1,
         nII = n,
         medianII = median,
         sdII = sd,
         rangeII = range)

# Descriptive statistics for exports to import partner countries
descrIE <- baci92 %>% filter(impcountr == 1, import == 0) # select only relevant observations
descrIE <- as.data.frame(describeBy(descrIE$v, group = descrIE$hs2section, mat = TRUE, digits = 2))
descrIE <- descrIE %>%
  select(group1, n, median, sd, range) %>%
  rename(Section = group1,
         nIE = n,
         medianIE = median,
         sdIE = sd,
         rangeIE = range)

# Descriptive statistics for imports from export partner countries
descrEI <- baci92 %>% filter(impcountr == 0, import == 1) # select only relevant observations
descrEI <- as.data.frame(describeBy(descrEI$v, group = descrEI$hs2section, mat = TRUE, digits = 2))
descrEI <- descrEI %>%
  select(group1, n, median, sd, range) %>%
  rename(Section = group1,
         nEI = n,
         medianEI = median,
         sdEI = sd,
         rangeEI = range)

# Descriptive statistics for exports to export partner countries
descrEE <- baci92 %>% filter(impcountr == 0, import == 0) # select only relevant observations
descrEE <- as.data.frame(describeBy(descrEE$v, group = descrEE$hs2section, mat = TRUE, digits = 2))
descrEE <- descrEE %>%
  select(group1, n, median, sd, range) %>%
  rename(Section = group1,
         nEE = n,
         medianEE = median,
         sdEE = sd,
         rangeEE = range)

# Merge descriptive statistics for imports and exports from/to import partner countries
descrI <- descrII %>%
  left_join(descrIE, by = "Section")

# Merge descriptive statistics for imports and exports from/to export partner countries
descrE <- descrEI %>%
  left_join(descrEE, by = "Section")

# Obtain Latex code of tables
xtable(descrI, caption = "Descriptive Statistics for main import partners by HS section", digits = 1)
xtable(descrE, caption = "Descriptive Statistics for main export partners by HS section", digits = 1)

# Create treemap of aggregated trade volume (imports+exports) by section and WZ classification
palette.HCL.options <- list(hue_start=170, hue_end=360+170, luminance=40)
treemap(baci92, index = c("hs2section", "wz"), # define group and subgroup
                vSize = "v", # size of squares based on values of transactions
                type = "index",
                title = " ",
                palette.HCL.options = palette.HCL.options)



#***********************************************************************************************************************
### 4) Exercise 2 ######################################################################################################
#***********************************************************************************************************************
# Create list containing Germany's five largest import and export industries per year:
largest <- baci92 %>%
  group_by(t, import, wz) %>% # observations will be aggregated by year, import/export and industry
  summarise(v = sum(v)) %>%
  pivot_wider(names_from = "import", values_from = "v") %>% # pivot table to have column for import and for export
  rename(imp = "1",
         exp = "0") %>%
  replace_na(list(imp = 0, exp = 0)) %>% # if there were no imports/exports in a year by an industry assign 0
  mutate(netexp = exp-imp) %>% # compute net exports (as industries import and export at the same time)
  group_by(t) %>%
  mutate(rank = as.integer(rank(desc(netexp)))) %>% # provide score for size of net exports (large export, small import)
  filter(rank < 6 | rank > n()-5) %>% # filter out only first and last five industries per year
  select(t, rank, wz, netexp) %>%
  arrange(t, desc(netexp)) %>% # order by year and nat exports
  pivot_wider(names_from = "rank", values_from = c("wz", "netexp"))

# Otain Latex code of table
xtable(largest, caption = "Largest five import and export industries", digits = 0)



#***********************************************************************************************************************
### 5) Exercise 3 ******************************************************************************************************
#***********************************************************************************************************************
# As I found it more convenient to crate the panel in the beginning of the analysis, the code can be found in section 1)
# The created panel looks like the following extract:
panel <- baci92[1:10,]
xtable(panel, caption = "Panel Dataset", digits = 1)



#***********************************************************************************************************************
### 6) Exercise 4 ******************************************************************************************************
#***********************************************************************************************************************
# Aggregate transaction values by year, partner type (export or import) and whether transaction is export or import:
impexpts <- baci92 %>%
  group_by(t, impcountr, import) %>%
  summarise(v = sum(v))

# Plot the time series:
ggplot(impexpts, aes(x = t, y = v, color=as.character(impcountr))) + # color destinguishes type of partner
  geom_point() +
  geom_line(aes(linetype = as.character(import))) + #linetype destinguishes import/export
  scale_linetype_manual(name = NULL, values=c("longdash", "solid"), labels = c("Exports", "Imports")) +
  scale_color_discrete(name = NULL, labels = c("Main export partners", "Main import partners")) +
  scale_y_continuous("value in EUR", limits = c(0,500000000), labels = scales::comma) +
  scale_x_continuous("Year") +
  theme_minimal()

# Aggregate transaction values by year, partner type (export or import) and export or import for all countries:
impexpts2 <- baci92 %>%
  group_by(t, country, impcountr, import) %>%
  summarise(v = sum(v))

# Plot the time series
ggplot(impexpts2, aes(x = t, y = v, color=as.character(country))) +
  geom_point(aes(shape = as.character(impcountr))) +
  geom_line(aes(linetype = as.character(import))) +
  scale_linetype_manual(name = NULL, values=c("dotted", "solid"), labels = c("Exports", "Imports")) +
  scale_shape_discrete(name = NULL, labels = c("Main export partners", "Main import partners")) +
  scale_y_continuous("value in EUR", limits = c(0,80000000), labels = scales::comma) +
  scale_x_continuous("Year") +
  scale_color_discrete(name = "Country") +
  theme_minimal()



#***********************************************************************************************************************
### 7) Exercise 5 ******************************************************************************************************
#***********************************************************************************************************************
# Load Germany's GDP data
gdp <- read.csv(str_c(path, "GDP_data.csv"), sep = ",", skip = 4)

# Select and clean only relevant GDP data
gdp <- gdp %>%
  filter(Country.Code == "DEU") %>%
  select(Country.Name, X2005, X2006, X2007, X2008, X2009, X2010) %>%
  pivot_longer(!Country.Name, names_to = "t", values_to = "gdpger") %>%
  mutate(t = as.integer(str_remove(.$t, "X"))) %>%
  left_join(., eurusd, by = "t") %>% # add exchange rate to get values in EUR
  left_join(., cpi, by = "t") %>% # add CPI to adjust for inflation
  mutate(loggdp = log((gdpger/eurinusd)/base2005)) %>%
  select(-c(Country.Name, eurinusd, base2005))

# Merge to baci92 data frame
baci92est <- baci92 %>%
  left_join(., gdp, by = "t") %>%
  ungroup() %>%
  mutate(country = as.character(country),
         wz = as.character(wz),
         v = log(v))

# Run pooled OLS regressions
pooledIMP <- lm(loggdp ~ country + wz + v, data = baci92est[baci92est$import == 1,])
pooledEXP <- lm(loggdp ~ country + wz + v, data = baci92est[baci92est$import == 0,])

# Stargaze the output to latex
stargazer(list(pooledIMP, pooledEXP),
          type = "latex",
          align = TRUE,
          style = "all2",
          model.names = F,
          model.numbers = F,
          multicolumn = T,
          title = "Correlation trade volumes and Germany's log GDP",
          dep.var.caption = "log(GDP)",
          column.labels = c("Imports", "Exports"))



#***********************************************************************************************************************
### 8) Graphs in Appendix **********************************************************************************************
#***********************************************************************************************************************
# Histogram of value for imports and exports with main import partners
ggplot(baci92 %>% ungroup() %>% filter(impcountr == 1 & v < 1000), aes(x = v, fill = factor(import))) +
  geom_histogram(alpha = 0.6, binwidth = 0.5) +
  facet_zoom(x = v <= 60 & v >= 0, horizontal = TRUE)+
  scale_fill_discrete(name = NULL, labels = c("Exports", "Imports"))+
  scale_x_continuous(name = "value", labels = scales::comma) +
  theme_bw()

# Boxplots of trade volume (imports + exports):
boxplot(v ~ country, data = baci92[baci92$impcountr == 1,], las = 2, outline=FALSE) # boxplot import partners
boxplot(v ~ country, data = baci92[baci92$impcountr == 0,], las = 2, outline=FALSE) # boxplot export partners


# Show heterogeneity across years
plotmeans(v ~ t, data = baci92[baci92$import == 1,], # plot for imports
          text.n.label = "",
          col = "blue",
          type = "b")
par(new = TRUE)
plotmeans(v ~ t, data = baci92[baci92$import == 0,], # plots for exports
          text.n.label = "",
          xlab = "",
          ylab = "",
          col = "red",
          barcol = "red",
          xaxt = "n",
          yaxt = "n",
          type = "b")
