
# trained.R
# See https://github.com/brucewent/MAD/

library(tidyverse)
library(openxlsx)
library(janitor)
library(gt)

# List of adult leadership positions in logical order

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
             "Pack Trainer",
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

# read the Trained Leaders Status data (CSV Details)

position <- read_csv('data/TrainedLeader_Mercer_Area_District 2024-12-17.csv',
                     skip=8,
                     col_names=TRUE,
                     col_types=list(MemberID=col_integer(),
                                    "Direct_Contact_Leader"=col_factor(),
                                    Trained=col_factor(),
                                    "Registration_Expiration_Date"=
                                      col_date(format = "%m/%d/%Y"))) |>
  mutate(posfact = fct(Position, levels = poslist)) |>
  select(-c("Position", "Service_Area", "Sub_District")) |>
  rename(Charter_Org = "Chartered_Org_Name",
         Position = posfact,
         Member_ID = "MemberID",
         Direct_Contact = "Direct_Contact_Leader",
         Registration_Expiration = "Registration_Expiration_Date" ) |>
  mutate(Unit_Gender = case_when(!is.na(Gender_Accepted) ~ str_c(Unit, " ", Gender_Accepted),
                                 is.na(Gender_Accepted) ~ Unit)) |>
  select(-c(Unit, Gender_Accepted)) |>
  rename(Unit = Unit_Gender) |>
  arrange(Council, District, Unit, Position)

# Check that position factor still includes all values found
nrow(filter(position,is.na(Position))) == 0

# Read the YPT Aging Report data (Export to CSV)

protection <- read_csv('data/YPT_Mercer_Area_District 2024-12-17.csv',
                    skip=7,
                    col_names=TRUE,
                    col_types=list(memberid=col_integer(),
                                   isyptcurrent2=col_factor(),
                                   yptexpirationdatec=col_date(format="%m/%d/%Y"),
                                   y01completiondatec=col_date(format="%m/%d/%Y"),
                                   y01expirationdatec=col_date(format="%m/%d/%Y"),
                                   registrationdatec=col_date(format="%m/%d/%Y"))) |>
  rename("District" = "..district",
         "Program" = "unittype",
         "Unit_Number" = "unitnumber",
         "Gender_Accepted" = "genderaccepted",
         "Chartered_Org" = "chartedorganization",
         "First_Name" = "firstname",
         "Middle_Name" = "middlename",
         "Last_Name" = "lastname",
         "Member_ID" = "memberid",
         "Position" = "positionname",
         "YPT_Current" = "isyptcurrent2",
         "Effective_Through" = "yptexpirationdatec",
         "Youth_Protection_Code" = "y01coursecode",
         "Y01_Completed" = "y01completiondatec",
         "Y01_Expires" = "y01expirationdatec",
         "Street_Address" = "streetaddress",
         "City" = "city",
         "State" = "statecode",
         "Zip" = "zip",
         "Email_Address" = "emailaddress",
         "Phone_Number" = "phonenumber",
         "Registration_Date" = "registrationdatec",
         "Expiry_Date" = "cregistrationexpirydate",
         "Online_Courses" = "stronlinecourses" )

# Create a list of distinct active leaders from the YPT data
# leader is the enhanced version of YPT

leader <- protection |>
  filter(is.na(Y01_Completed)==FALSE) |>
  distinct(First_Name,
           Middle_Name,
           Last_Name,
           Member_ID,
           YPT_Current,
           Y01_Completed ,
           Y01_Expires,
           Street_Address,
           City,
           State,
           Zip,
           Email_Address,
           Phone_Number) |>
#  rename(Y01_Status = Status) |>
  arrange(Last_Name, First_Name, Middle_Name, Member_ID)

# Check that member_ID still works as unique identifyer
nrow(leader) == nrow(distinct(leader,Member_ID))

# bring in identifying info from YPT to each position
# training is the enhanced version of position

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
         YPT_Current,
         Y01_Completed,
         Y01_Expires,
         Street_Address,
         City,
         State,
         Zip,
         Email_Address,
         Phone_Number)

# Check that training record count still the same?
nrow(position) == nrow(training)

# Summarize unit training statistics
# counts are distinct people as you can hold only
# one position in a given unit?

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

rm(unit_1, unit_2, unit_3)

# Create a spreadsheet with three sheets (Units, Training, Leaders)

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

# unit_stats |>
#   select(Charter_Org, Unit, Leaders, Trained, Train_pct,
#          DC_Leaders, DC_Trained, DC_Train_pct) |>
#   arrange(by=Train_pct) |>
#   gt()|>
#   fmt_number(columns=c(Train_pct, DC_Train_pct),
#              decimals = 1)

# Report training needs to unit

key3 <- mutate(training,
               posabbr = case_when(Position == "Cubmaster" ~ "CM",
                                   Position == "Venturing Crew Advisor" ~ "Advisor",
                                   Position == "Scoutmaster" ~ "SM",
                                   Position == "Committee Chair" ~ "CC",
                                   Position == "Chartered Organization Rep." ~ "COR")) |>
  mutate(k_email = str_c(Unit, " ", posabbr, " ", First_Name, " ", Last_Name, " <", Email_Address, ">,")) |>
  filter(!is.na(posabbr)) |>
  mutate(posabbr = case_when(posabbr == "CM" ~ "UL",
                             posabbr == "Advisor" ~ "UL",
                             posabbr == "SM" ~ "UL",
                             TRUE ~ posabbr))|>
  select(Unit,
         Charter_Org,
         posabbr,
         k_email)

k2 <- key3 |>
  group_by(Unit, Charter_Org) |>
  pivot_wider(names_from = posabbr, values_from = k_email)

k4 <- k2 |>
  mutate(km = case_when(Unit == "Pack 0044 F"  ~ str_c(CC,COR),            # No cubmaster
                        Unit == "Troop 0052 G" ~ str_c(CC,COR),            # no scoutmaster
                        Unit == "Pack 0193 F"  ~ str_c(UL,COR),            # no committee chair
                        Unit == "Troop 0850 B" ~ str_c(UL,CC,COR[1],COR[2]),  # two CORs
                        .default = str_c(UL,CC,COR)))

# write emails to CSV

# k4 |> write_csv("emails.csv")

# https://tidyr.tidyverse.org/articles/pivot.html#wider

str(key3)
str(k2)
str(k4)
