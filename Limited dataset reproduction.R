
# Please note, to preserve the anonymity of our participants, the published version of our dataset is limited.
# It does not contain any demographic variables, granular clinical details, timestamps, nor responses to specific
# items. The linear models fitted in this R script do not adjust for sex and age, and so will produce slightly 
# different results to those reported in the paper.

# We hope you find this dataset and code sufficient to approximately reproduce the key findings of our paper. 
# Please feel free to use it for secondary research (within the limits its licensing). Should you need a more
# granular version of the dataset, please contact the authors. 


## ---------- Load packages ---------- ##

options(scipen = 999)

if(!require(pacman)) install.packages("pacman")

p_install_version(
  c("dplyr", "lubridate", "ggplot2", "egg", "plyr", "lmerTest", "emmeans"), 
  c("1.1.4", "1.9.3", "3.5.1", "0.4.5", "1.8.9", "3.1-3", "1.10.6")
) 

p_load("dplyr", "lubridate", "ggplot2", "egg", "plyr", "lmerTest", "emmeans")


## ---------- Load data ---------- ##

nomination.level.data <- read.csv("nomination_level_data.csv")


## ---------- Descriptive statistics and initial exploration ---------- ##


# Check number of assessments completed, and number that were sent but not completed

nomination.level.data |>
  dplyr::group_by(testCode, status) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop")


## Demographics

# Number of participants by condition
nomination.level.data |> 
  group_by(studyId, condition) |>
  dplyr::summarise() |>
  pull(condition) |>
  table()

# Number of participants by treatment
nomination.level.data |> 
  group_by(studyId) |>
  filter(row_number() == 1) |>
  group_by(condition, treatment) |>
  dplyr::summarise(table(treatment))

# How many patients with CTS required follow-up
nomination.level.data |> 
  filter(treatment == "Standard carpal tunnel release") |> # only looking at those who have had a records review
  group_by(studyId, ctsFollowUpNeeded) |>
  dplyr::summarise() |>
  pull(ctsFollowUpNeeded) |>
  table() 

## Response rates

# PEMCAT - the CAT version of the PEM questionnaire 
nomination.level.data |>
  filter(testCode == "PEMCAT") |>
  pull(status) |>
  table() 


# PEMFL - the full-length version of the PEM questionnaire
nomination.level.data |>
  filter(testCode == "PEMFL") |>
  pull(status) |>
  table()


# UES - the User Engagement Scale 
nomination.level.data |>
  filter(testCode == "UES") |>
  pull(status) |>
  table()

# How many PEMCAT nominations were responded to per person?
responses.per.participant.table <- plyr::count(dplyr::filter(nomination.level.data, testCode == 'PEMCAT' & status == "Complete"), "studyId") |> 
  dplyr::arrange("participantID")  # I can use this to add to a table later where participants are rows

responses.per.participant.table |>
  pull(freq) |>
  quantile() # Median number of responses in 12 weeks


# Median and IQR time taken to complete PEMCAT (seconds)
dplyr::filter(nomination.level.data, testCode == 'PEMCAT' & status == "Complete") |> 
  pull("secondsTaken") |>
  quantile()

# Median and IQR time taken to complete PEMFL (seconds)
dplyr::filter(nomination.level.data, testCode == 'PEMFL' & status == "Complete") |> 
  pull("secondsTaken") |>
  quantile() 

# Plot distribution of response times
seconds.per.assessment <- nomination.level.data |>
  filter(status == "Complete" & testCode != "UES") |>
  pull("testCode", "secondsTaken")

seconds <- seconds.per.assessment |>
  names()

tests <- unname(seconds.per.assessment)

df <- data.frame(seconds, tests)

df$seconds <- as.numeric(df$seconds)

ggplot(df, aes(x = seconds, group = tests, fill = tests)) +
  geom_density(aes(fill = tests), alpha = 0.5) +
  scale_x_continuous(limits = c(0,300)) +
  theme_minimal() +
  ylab("Denisty") +
  xlab("Seconds between notification and response") +
  scale_fill_manual(values = c("navy", "darkorange"),
                    labels = c("CAT", "Full-length")) +
  theme(legend.title=element_blank())



# Median and IQR number of items completed
dplyr::filter(nomination.level.data, testCode == 'PEMCAT' & status == "Complete") |> 
  pull("itemsAdministeredNum") |>
  quantile()



## ---------- Linear modelling ---------- ##


nomination.level.data$postOpDay <- as.factor(nomination.level.data$postOpDay)


## Dupuytren's fasciectomy vs percutaneous needle fasciotomy

# Filter out relevant data
dups.data <- nomination.level.data|>
  filter(testCode == "PEMCAT" &
           status == "Complete" &
           condition == "Dupuytren's contracture" &
           (treatment == "Dupuytren's fasciectomy" | treatment == "Percutaneous needle fasciotomy"))


dups.mod <- lmerTest::lmer(theta ~ postOpDay*treatment + (1|studyId), data = dups.data) # Fit model
# Note, the model used in the paper also adjusted for age and sex, which we have not provided in this limited dataset
# The results here will differ slightly, as a result

dups.means <- emmeans(dups.mod, pairwise ~ treatment | postOpDay, infer = c(TRUE, TRUE), adjust = "tukey") # Estimate marginal means

# Look at contrasts
dups.means$contrast

# Create a data frame for plotting
dups.means.df <- dups.means$emmeans %>% 
  as.data.frame()

# Custom labels for the plot
my_custom_labels = c("0", "", "", "", "", "5",
                     "", "", "", "", "10",
                     "", "", "", "", "15",
                     "", "", "", "", "20",
                     "", "", "", "", "25",
                     "", "", "", "", "30",
                     "", "", "", "", "35",
                     "", "", "", "", "40",
                     "", "", "", "", "45",
                     "", "", "", "", "50",
                     "", "", "", "", "55",
                     "", "", "", "", "60",
                     "", "", "", "", "65",
                     "", "", "", "", "70",
                     "", "", "", "", "75",
                     "", "", "", "", "80",
                     "", "", "")

# Plot the marginal means
ggplot(dups.means.df, aes(x = postOpDay, y = emmean, colour = treatment, group = treatment, fill = treatment)) +
  geom_point(position = position_dodge(0.15), size = 1.5, alpha = 0.7) +
  geom_ribbon(aes(ymin = dups.means.df$asymp.LCL, ymax = dups.means.df$asymp.UCL), alpha =0.15, colour = NA) +
  scale_color_manual(values = c("dodgerblue3", "firebrick4"), labels = c("Fasciectomy", "PNF") ) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick4"), labels = c("Fasciectomy", "PNF") ) +
  theme_bw() +
  scale_x_discrete(labels = my_custom_labels, limits = (as.character(seq(0,83,1)))) +
  theme(legend.position = c(0.8, 0.85)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.2)) +
  xlab("Days") +
  ylab("Score") +
  theme(legend.title=element_blank())

# Look at adjusted p values
dups.contrasts <- dups.means$contrasts |>
  as.data.frame()

which(dups.contrasts$p.value < 0.05) -1 # On these post-op days, the adjusted p < 0.05
which(dups.contrasts$p.value < 0.01) -1 # On these post-op days, the adjusted p < 0.01


## Steroid injection vs trapeziectomy for thumb-base osteoarthritis

# Filter out relevant data
oa.data <- nomination.level.data|>
  filter(testCode == "PEMCAT" &
           status == "Complete" &
           condition == "Thumb-base osteoarthritis" &
           (treatment == "Steroid injection for thumb-base osteoarthritis" | treatment == "Trapeziectomy"))

oa.mod <- lmerTest::lmer(theta ~ postOpDay*treatment + (1|studyId), data = oa.data)# Fit model
# Note, the model used in the paper also adjusted for age and sex, which we have not provided in this limited dataset
# The results here will differ slightly, as a result

oa.means <- emmeans(oa.mod, pairwise ~ treatment | postOpDay, infer = c(TRUE, TRUE), adjust = "tukey") # Estimate marginal means

# Look at contrasts
oa.means$contrast

# Create a data frame for plotting
oa.means.df <- oa.means$emmeans %>% 
  as.data.frame()

ggplot(oa.means.df, aes(x = postOpDay, y = emmean, colour = treatment, group = treatment, fill = treatment)) +
  geom_point(position = position_dodge(0.15), size = 1.5, alpha = 0.7) +
  geom_ribbon(aes(ymin = oa.means.df$lower.CL, ymax = oa.means.df$upper.CL), alpha =0.15, colour = NA) +
  scale_color_manual(values = c("cyan4", "darkmagenta"), labels = c("Steroid injection", "Trapeziectomy") ) +
  scale_fill_manual(values = c("cyan4", "darkmagenta"), labels = c("Steroid injection", "Trapeziectomy") ) +
  theme_bw() +
  scale_x_discrete(labels = my_custom_labels, limits = (as.character(seq(0,83,1)))) +
  theme(legend.position = c(0.8, 0.85)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.2)) +
  xlab("Days") +
  ylab("Score") +
  theme(legend.title=element_blank())

# Create a data frame for plotting
oa.contrasts <- oa.means$contrasts |>
  as.data.frame()

which(oa.contrasts$p.value < 0.05) -1 # On these post-op days, the adjusted p < 0.05
which(oa.contrasts$p.value < 0.01) -1 # On these post-op days, the adjusted p < 0.01


## Steroid injection vs A1 pulley release for trigger finger 

# Filter out relevant data
trigger.data <- nomination.level.data|>
  filter(testCode == "PEMCAT" &
           status == "Complete" &
           condition == "Trigger finger" &
           (treatment == "Steroid injection for trigger finger" | treatment == "A1 pulley release"))

trigger.mod <- lmerTest::lmer(theta ~ postOpDay*treatment + (1|studyId), data = trigger.data) # Fit model
# Note, the model used in the paper also adjusted for age and sex, which we have not provided in this limited dataset
# The results here will differ slightly, as a result

trigger.means <- emmeans(trigger.mod, pairwise ~ treatment | postOpDay, infer = c(TRUE, TRUE), adjust = "tukey") # Estimate marginal means

# Look at contrasts
trigger.means$contrast

# Create a data frame for plotting
trigger.means.df <- trigger.means$emmeans %>% 
  as.data.frame()

ggplot(trigger.means.df, aes(x = postOpDay, y = emmean, colour = treatment, group = treatment, fill = treatment)) +
  geom_point(position = position_dodge(0.15), size = 1.5, alpha = 0.7) +
  geom_ribbon(aes(ymin = trigger.means.df$asymp.LCL, ymax = trigger.means.df$asymp.UCL), alpha =0.15, colour = NA) +
  scale_color_manual(values = c("darkorange", "navy"), labels = c("A1 pulley release", "Steroid injection") ) +
  scale_fill_manual(values = c("darkorange", "navy"), labels = c("A1 pulley release", "Steroid injection") ) +
  theme_bw() +
  scale_x_discrete(labels = my_custom_labels, limits = (as.character(seq(0,83,1)))) +
  theme(legend.position = c(0.8, 0.85)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.2)) +
  xlab("Days") +
  ylab("Score") +
  theme(legend.title=element_blank())

# Look at adjusted p values
trigger.contrasts <- trigger.means$contrasts |>
  as.data.frame()

which(trigger.contrasts$p.value < 0.05) -1 # On these post-op days, the adjusted p < 0.05
which(trigger.contrasts$p.value < 0.01) -1 # On these post-op days, the adjusted p < 0.01


## Standard carpal tunnel release vs minimally invasive carpal tunnel release

ctd.data <- nomination.level.data|>
  filter(testCode == "PEMCAT" &
           status == "Complete" &
           condition == "Carpal tunnel syndrome" &
           (treatment == "Standard carpal tunnel release" | treatment == "Percutaneous or endoscopic carpal tunnel release"))

ctd.mod <- lmerTest::lmer(theta ~ postOpDay*treatment + (1|studyId), data = ctd.data) # Fit model
# Note, the model used in the paper also adjusted for age and sex, which we have not provided in this limited dataset
# The results here will differ slightly, as a result

ctd.means <- emmeans(ctd.mod, pairwise ~ treatment | postOpDay, infer = c(TRUE, TRUE), adjust = "tukey", pbkrtest.limit = 11152, lmerTest.limit = 11152) # Estimate marginal means

# Look at contrasts
ctd.means$contrast

# Create a data frame for plotting
ctd.means.df <- ctd.means$emmeans %>% 
  as.data.frame()

ggplot(ctd.means.df, aes(x = postOpDay, y = emmean, colour = treatment, group = treatment, fill = treatment)) +
  geom_point(position = position_dodge(0.15), size = 1.5, alpha = 0.7) +
  geom_ribbon(aes(ymin = ctd.means.df$asymp.LCL, ymax = ctd.means.df$asymp.UCL), alpha =0.15, colour = NA) +
  scale_color_manual(values = c("darkgreen", "chocolate3"), labels = c("Minimally invasive CTD", "Standard CTD") ) +
  scale_fill_manual(values = c("darkgreen", "chocolate3"), labels = c("Minimally invasive CTD", "Standard CTD") ) +
  theme_bw() +
  scale_x_discrete(labels = my_custom_labels, limits = (as.character(seq(0,83,1)))) +
  theme(legend.position = c(0.8, 0.85)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.2)) +
  xlab("Days") +
  ylab("Score") +
  theme(legend.title=element_blank())

# Look at adjusted p values
ctd.contrasts <- ctd.means$contrasts |>
  as.data.frame()

which(ctd.contrasts$p.value < 0.05) -1 # On these post-op days, the adjusted p < 0.05
which(ctd.contrasts$p.value < 0.01) -1 # On these post-op days, the adjusted p < 0.01


## Standard carpal tunnel decompression - those discharged at follow-up vs those who needed further clinical input

ctd.data2 <- nomination.level.data|>
  filter(testCode == "PEMCAT" &
           status == "Complete" &
           treatment == "Standard carpal tunnel release")

ctd.mod2 <- lmerTest::lmer(theta ~ postOpDay*ctsFollowUpNeeded + (1|studyId), data = ctd.data2)  # Fit model
# Note, the model used in the paper also adjusted for age and sex, which we have not provided in this limited dataset
# The results here will differ slightly, as a result

ctd.means2 <- emmeans(ctd.mod2, pairwise ~ ctsFollowUpNeeded | postOpDay, infer = c(TRUE, TRUE), adjust = "tukey", pbkrtest.limit = 11152, lmerTest.limit = 11152) # Estimate marginal means

# Look at contrasts
ctd.means2$contrast

# Create a data frame for plotting
ctd.means.df2 <- ctd.means2$emmeans %>% 
  as.data.frame()

ggplot(ctd.means.df2, aes(x = postOpDay, y = emmean, colour = ctsFollowUpNeeded, group = ctsFollowUpNeeded, fill = ctsFollowUpNeeded)) +
  geom_point(position = position_dodge(0.15), size = 1.5, alpha = 0.7) +
  geom_ribbon(aes(ymin = ctd.means.df2$asymp.LCL, ymax = ctd.means.df2$asymp.UCL), alpha =0.15, colour = NA) +
  scale_color_manual(values = c("lightsteelblue4", "coral4"), labels = c("Discharged", "Required further clinical input") ) +
  scale_fill_manual(values = c("lightsteelblue4", "coral4"), labels = c("Discharged", "Required further clinical input") ) +
  theme_bw() +
  scale_x_discrete(labels = my_custom_labels, limits = (as.character(seq(0,83,1)))) +
  theme(legend.position = c(0.8, 0.85)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = 0.2)) +
  xlab("Days") +
  ylab("Score") +
  theme(legend.title=element_blank())

# Look at adjusted p values
ctd2.contrasts <- ctd.means2$contrasts |>
  as.data.frame()

which(ctd2.contrasts$p.value < 0.05) -1 # On these post-op days, the adjusted p < 0.05
which(ctd2.contrasts$p.value < 0.01) -1 # On these post-op days, the adjusted p < 0.01




