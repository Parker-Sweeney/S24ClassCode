rm(list=ls())
### File Encoding ###

## Native Encoding ##
Sys.getenv() # List the current settings for 
# environment variables

# Return "LANG" and "LC_CTYPE" variables for 
# language encoding
Sys.getenv(c("LANG", "LC_CTYPE"))

# Can also examine "locale", which controls the default 
# formatting of text, dates, times, and currency
Sys.getlocale()

## Handling File Encoding ##
# Open file with UTF-8 encoding
authors<-read.csv("authors.txt", 
                  sep="|", # Pipe-delimited file
                  stringsAsFactors=FALSE,
                  encoding="UTF-8") # Specify file encoding
View(authors)
authors$Author[5] # Accents are rendered
authors$Birthplace[13] # Umlauts are rendered
authors$Biography[29] # Japanese characters are rendered

# Solution 1: Do nothing
# (i.e., write to file without UTF-8 encoding)
write.table(authors,"authors_ascii.txt",
            sep="|",row.names=FALSE,
            fileEncoding="ASCII")
warnings() # This will cause a warning "invalid char 
# string in output conversion"

# authors_ascii.txt should appear in working directory.
# Any characters following a special character are lost

# Solution 2: Strip non-ASCII characters
?gsub # Find and replace substrings/regex patterns
# (https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)
authors_stripped<-authors # Create copy
authors_stripped$Author<-gsub("[^[0-9a-zA-Z!@#$%&*:;\'\",.<>\\(\\)? /\\]",
                              " ",authors_stripped$Author)
authors_stripped$Birthplace<-gsub("[^[0-9a-zA-Z!@#$%&*:;\'\",.<>\\(\\)? /\\]",
                                  " ",authors_stripped$Birthplace)
authors_stripped$Biography<-gsub("[^[0-9a-zA-Z!@#$%&*:;\'\",.<>\\(\\)? /\\]",
                                 " ",authors_stripped$Biography)
# This pattern replaces anything that is not (^) a number, 
# English letter, punctuation or space
View(authors_stripped)
authors_stripped$Author[5] # Accents are removed
authors_stripped$Birthplace[13] # Umlauts are removed
authors_stripped$Biography[29] # Japanese characters are removed

# Write to file without UTF-8 encoding
write.table(authors_stripped,"authors_stripped.txt",
            sep="|",
            row.names=FALSE,
            fileEncoding="ASCII")
# File writes with no warnings, but is now missing data :(

# Solution 3: Transliteration
# NOTE: If the characters in the script below do not 
# show correctly, go to the file menu and use the option  
# to "Re-open with Encoding", and select "UTF-8" as the 
# encoding. This opens the script itself  with UTF-8 
# encoding (still need to handle UTF-8 characters in files).

# Create mappings of similar characters
# Example:
# Ã,Ã,Ã,Ã,Ã,Ã,Ã >> a
# Ã,Ã¾ >> b
# Ã >> c
# Ã,Ã,Ã,Ã >> e
# Ã,Ã,Ã,Ã >> i
# Ã >> n
# Ã,Ã,Ã,Ã,Ã,Ã,Ã° >> o
# Å  Ã >> s
# Ã,Ã,Ã >> u
# Ã,Ã¿,Ã½ >> y
# Å½ >> z

# Find and replace with gsub()
authors_sub<-authors # Create copy of authors data frame
# Handle accented characters with E
authors_sub$Author<-gsub("[ÃÃÃÃ]","e",
                         authors_sub$Author, 
                         ignore.case=TRUE) 
authors_sub$Author[5] # Ã© replaced with e
# Handle accented characters with O
authors_sub$Birthplace<-gsub("[ÃÃÃÃÃÃÃ°]","o",
                             authors_sub$Birthplace, 
                             ignore.case=TRUE)
# Handle accented characters with U
authors_sub$Birthplace<-gsub("[ÃÃÃ]","u",
                             authors_sub$Birthplace, 
                             ignore.case=TRUE)
authors_sub$Birthplace[13] # Ã¶ replaced with o, 
# Ã¼ replaced with u

# Handle accented characters with A
authors_sub$Biography<-gsub("[ÃÃÃÃÃÃÃ]","a",
                            authors_sub$Biography, 
                            ignore.case=TRUE)
# Handle accented characters with E
authors_sub$Biography<-gsub("[ÃÃÃÃ]","e",
                            authors_sub$Biography, 
                            ignore.case=TRUE)
# Handle accented characters with I
authors_sub$Biography<-gsub("[ÃÃÃÃ]","i",
                            authors_sub$Biography, 
                            ignore.case=TRUE)
# Handle accented characters with O
authors_sub$Biography<-gsub("[ÃÃÃÃÃÃÃ°]","o",
                            authors_sub$Biography, 
                            ignore.case=TRUE)
# Handle accented characters with U
authors_sub$Biography<-gsub("[ÃÃÃ]","u",
                            authors_sub$Biography, 
                            ignore.case=TRUE)
authors_sub$Biography[16] # Accented characters replaced
authors_sub$Biography[29] # Does not always work 

# Write to file without UTF-8 encoding
write.table(authors_sub,"authors_sub.txt",
            sep="|",
            row.names=FALSE,
            fileEncoding="ASCII")
# NOTE: Still some warnings. Would have to write a script 
# to iterate through all of the mappings to be thorough

# Solution 4: Convert encoding with iconv()
?iconv # Convert character encodings
authors_iconv<-authors # Create copy of authors data frame
authors_iconv$Author<-iconv(authors_iconv$Author, 
                            from="UTF-8", # Original encoding
                            to="ASCII//TRANSLIT", # New encoding
                            sub="??") # Substitute unknown
authors_iconv$Birthplace<-iconv(authors_iconv$Birthplace, 
                                from="UTF-8", 
                                to="ASCII//TRANSLIT", 
                                sub="??")
authors_iconv$Biography<-iconv(authors_iconv$Biography, 
                               from="UTF-8", 
                               to="ASCII//TRANSLIT", 
                               sub="??")

View(authors_iconv)
authors_iconv$Author[5] # Accents are replaced
authors_iconv$Birthplace[13] # Umlauts are replaced
authors_iconv$Biography[29] # Does not always work

# Write to file without UTF-8 encoding
write.table(authors_iconv,"authors_iconv.txt",
            sep="|",
            row.names=FALSE,
            fileEncoding="ASCII")
# File writes with no warnings, but replacements may 
# look weird (escape characters/apostrophes added)
# Transliteration with iconv() only works for Latin 
# characters (Japanese characters "??")
# iconv() may have slight differences between 
# Windows and Macs

# Solution 5: Use encoding="UTF-8" when reading files, 
# fileEncoding="UTF-8" when writing
write.table(authors,"authors_utf8.txt",
            sep="|",
            row.names=FALSE, 
            fileEncoding="UTF-8") # Specify output encoding

# NOTE: If you want to save this script with your notes, 
# use the option to "Save with Encoding" under the
# File menu. Then save the script as UTF-8.
