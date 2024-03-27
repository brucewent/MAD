
library(tidyverse)
library(openxlsx)
library(janitor)
library(gt)

poslist <- c("Cubmaster",
             "Assistant Cubmaster",
             "Webelos Leader",
             "Assistant Webelos Leader",
             "Den Leader",
             "Asst. Den Leader",
             "Tiger Den Leader",
             "Lion Guide",
             "Venturing Crew Advisor",
             "Venturing Crew Associate Advisor",
             "Explorer Post Advisor",
             "Post - Explorer Post Associate Advisor",
             "Explorer Post Associate Advisor",
             "Post Committee Chair",
             "Post Committee Member",
             "Scoutmaster",
             "Assistant Scoutmaster",
             "Unit Chaplain",
             "Unit College Scouter Reserve",
             "Unit Scouter Reserve",
             "Committee Chair",
             "Committee Member",
             "Chartered Organization Rep.",
             "District Commissioner",
             "Asst. District Commissioner",
             "Unit Commissioner",
             "District Chair",
             "District Vice-Chair",
             "District Member-at-Large",
             "Merit Badge Counselor",
             "New Member Coordinator",
             "Nova Counselor",
             "Supernova Mentor")

position <- read_csv('data/TrainedLeader_Mercer_Area_District 2024-03-20.csv',
                     skip=8,
                     col_names=TRUE,
                     col_types=list(MemberID=col_integer(),
                                    "Direct Contact Leader"=col_factor(),
                                    Trained=col_factor(),
                                    "Registration Expiration Date"=
                                      col_date(format = "%m/%d/%Y"))) |>
  mutate(posfact = fct(Position, levels = poslist)) |>
  select(-c("Position", "Service Area", "Sub-District")) |>
  rename(Gender_Accepted = "Gender Accepted",
         Charter_Org = "Chartered Org Name",
         First_Name = "First Name",
         Middle_Name = "Middle Name",
         Last_Name = "Last Name",
         Position = posfact,
         Zip_Code = "Zip Code",
         Member_ID = "MemberID",
         Direct_Contact = "Direct Contact Leader",
         Registration_Expiration = "Registration Expiration Date",
         Incomplete_Mandatory = "Incomplete Mandatory",
         Incomplete_Classroom = "Incomplete Classroom",
         Incomplete_Online = "Incomplete Online" ) |>
  mutate(Unit_Gender = case_when(!is.na(Gender_Accepted) ~ str_c(Unit, " ", Gender_Accepted),
                                 is.na(Gender_Accepted) ~ Unit)) |>
  select(-c(Unit, Gender_Accepted)) |>
  rename(Unit = Unit_Gender) |>
  arrange(Council, District, Unit, Position)

# Position factor still includes all values
nrow(filter(position,is.na(Position))) == 0

protection <- read_csv('data/YPT_Mercer_Area_District 2024-03-20.csv',
                    skip=7,
                    col_names=TRUE,
                    col_types=list(Member_ID=col_integer(),
                                   Status=col_factor(),
                                   Effective_Through=col_date(format="%m/%d/%Y"),
                                   Y01_Completed=col_date(format="%m/%d/%Y"),
                                   Y01_Expires=col_date(format="%m/%d/%Y"),
                                   Registration_Date=col_date(format="%m/%d/%Y"))) |>
  rename(District = "..District")

glimpse(position)
glimpse(protection)

leader <- protection |>
  filter(is.na(Y01_Completed)==FALSE) |>
  distinct(First_Name,
           Middle_Name,
           Last_Name,
           Member_ID,
           Status,
           Y01_Completed ,
           Y01_Expires,
           Street_Address,
           City,
           State,
           Zip,
           Email_Address,
           Phone_Number) |>
  rename(Y01_Status = Status) |>
  arrange(Last_Name, First_Name, Middle_Name, Member_ID)

# Member_ID still works as unique identifyer
nrow(leader) == nrow(distinct(leader,Member_ID))

training <- position |>
  select (Council,
          District,
          Unit,
          Charter_Org,
          Member_ID,
          Program,
          Position,
          Direct_Contact,
          Trained,
          Registration_Expiration,
          Incomplete_Mandatory,
          Incomplete_Classroom,
          Incomplete_Online) |>
  left_join(leader, by = "Member_ID") |>
  select(Council,
         District,
         Unit,
         Charter_Org,
         Program,
         Member_ID,
         First_Name,
         Middle_Name,
         Last_Name,
         Position,
         Direct_Contact,
         Trained,
         Registration_Expiration,
         Incomplete_Mandatory,
         Incomplete_Classroom,
         Incomplete_Online,
         Y01_Status,
         Y01_Completed,
         Y01_Expires,
         Street_Address,
         City,
         State,
         Zip,
         Email_Address,
         Phone_Number)

# Training record count still the same?
nrow(position) == nrow(training)

# Unit training statistics

unit_1 <- training |>
  filter(!is.na(Unit)) |>
  select(Council, District, Charter_Org, Unit) |>
  distinct()

unit_2 <- training |>
  filter(!is.na(Unit)) |>
  tabyl(Unit, Trained) |>
  mutate(Leaders=YES+NO, Train_pct=100*YES/(YES+NO)) |>
  rename(Trained=YES) |>
  select(Unit, Leaders, Trained, Train_pct)

unit_3 <- training |>
  filter(Direct_Contact == "YES") |>
  tabyl(Unit, Trained) |>
  mutate(DC_Leaders=YES+NO, DC_Train_pct=100*YES/(YES+NO)) |>
  rename(DC_Trained=YES) |>
  select(Unit, DC_Leaders, DC_Trained, DC_Train_pct)

unit_stats <- merge(unit_1, unit_2, by.x="Unit", by.y="Unit") |>
  merge(unit_3, by.x="Unit", by.y="Unit") |>
  select(Council, District, Charter_Org, Unit,
         Leaders, Trained, Train_pct,
         DC_Leaders, DC_Trained, DC_Train_pct)

wb <- createWorkbook()

addWorksheet(wb, "Units")
writeData(wb, "Units", unit_stats)
freezePane(wb, sheet = 1, firstRow = TRUE)
addFilter(wb, sheet = 1, row = 1, cols = 1:ncol(unit_stats))

addWorksheet(wb, "Training")
writeData(wb, "Training", training)
freezePane(wb, sheet = 2, firstRow = TRUE)
addFilter(wb, sheet = 2, row = 1, cols = 1:ncol(training))

addWorksheet(wb, "Leaders")
writeData(wb, "Leaders", leader)
freezePane(wb, sheet = 3, firstRow = TRUE)
addFilter(wb, sheet = 3, row = 1, cols = 1:ncol(leader))

saveWorkbook(wb, "training.xlsx", overwrite=TRUE)

# Format unit stats for HTML output

unit_stats |>
  select(Charter_Org, Unit, Leaders, Trained, Train_pct,
         DC_Leaders, DC_Trained, DC_Train_pct) |>
  arrange(by=Train_pct) |>
  gt()|>
  fmt_number(columns=c(Train_pct, DC_Train_pct),
             decimals = 1)

# The Tidyverse Cookbook - Transform Tables
# https://rstudio-education.github.io/tidyverse-cookbook/transform-tables.html

t_list <- training |>
  mutate (row_lab = paste(Unit, Charter_Org, sep="/"),
          Name=case_when(is.na(Middle_Name)==TRUE ~ paste(First_Name, Last_Name, sep=" "),
                         TRUE ~ paste(First_Name, Middle_Name, Last_Name, sep=" "))) |>
  filter (row_lab != "NA/NA")

t_head <- t_list |>
  select (row_lab) |>
  distinct()

row_lab <- t_head$row_lab

t_list <- t_list |>
  select(row_lab,
         Name,
         Member_ID,
         Position,
         Direct_Contact,
         Trained,
         Registration_Expiration,
         Incomplete_Mandatory,
         Incomplete_Classroom,
         Incomplete_Online) |>
  filter(Trained == "NO")

t_gt <- t_list |>
  gt(rowname_col = "row_lab") |>
  tab_row_group(label = row_lab,
                rows = Name, Member_ID)
t_gt
