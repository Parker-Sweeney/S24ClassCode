office <- read.csv("office_employees.csv")

# See if there are any exact row matches
duplicated(office)

# Count of duplicated rows
sum(duplicated(office))

# See exact duplicated rows
office[duplicated(office),]

# Get rid of duplicates
office <- office[!duplicated(office),]
