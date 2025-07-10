library(tabulapdf)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)

pth <- "data-raw/2025 RWSP Appendix 4-1 Reuse Estimates and Projections Public Draft FINAL.pdf"

data('facilities', package = 'tbeploads')

facilities <- facilities |>
  mutate(
    permit = case_when(
      permit %in% c('FL0028061LA', 'FL0028061SW') ~ 'FL0028061',
      T ~ permit
    )
  )

# table 4-1 -----------------------------------------------------------------------------------

datraw <- extract_tables(pth, pages = c(2, 3), method = "lattice", output = "tibble")

#####
# first page
dat1raw <- datraw[[1]]

# remove rows with totals
torm <- which(apply(dat1raw, 1, function(x) any(grepl("Total|otal", x, ignore.case = TRUE))))
dat1raw <- dat1raw[-torm[-1], ]

# remove columns with all NA values
torm <- which(apply(dat1raw, 2, function(x) all(is.na(x))))
dat1raw <- dat1raw[, -torm]

# rename columns
nms <- unlist(as.vector(dat1raw[1, ]))
names(nms) <- NULL
nms[is.na(nms)] <- paste0("temp_col_", which(is.na(nms)))

dat1raw <- dat1raw[2:nrow(dat1raw),]
names(dat1raw) <- nms

# convert all to character for bind
dat1raw <- dat1raw |>
  mutate(across(everything(), as.character))

# shift columns out of order
colshft1 <- which(!is.na(dat1raw[, 2, drop = T]))[1]
colshft2 <- which(grepl('^FLA', dat1raw[, 1, drop = T]))[1]

rows1 <- dat1raw[1:(colshft1 - 1), ]

rows2 <- dat1raw[colshft1:(colshft2 - 1), ]
rows2 <- rows2[, -ncol(dat1raw)]
rows2 <- bind_cols(
  tibble(empty2 = NA_character_),
  rows2
  )
names(rows2) <- nms
rows3 <- dat1raw[colshft2:nrow(dat1raw), ]
rows3 <- rows3[, -c((ncol(dat1raw) - 1), ncol(dat1raw))]
rows3 <- bind_cols(
  tibble(empty1 = NA_character_, empty2 = NA_character_),
  rows3
)
names(rows3) <- nms

# combine rows
dat1 <- bind_rows(rows1, rows2, rows3)

# remove empty columns
torm <- which(apply(dat1, 2, function(x) all(is.na(x))))
dat1 <- dat1[, -torm]

# final format page 1
dat1 <- dat1 |>
  mutate(
    temp_col_1 = case_when(
      `Facility Name` == 'Dade City' ~ 'Pasco',
      T ~ temp_col_1
    )
  ) |>
  fill(temp_col_1) |>
  mutate(across(-c(1:4), as.numeric))

#####
# second page
dat2raw <- datraw[[2]]

# remove rows with totals
torm <- which(apply(dat2raw, 1, function(x) any(grepl("Total|otal", x, ignore.case = TRUE))))
dat2raw <- dat2raw[-torm[-1], ]

# remove columns with all NA values
torm <- which(apply(dat2raw, 2, function(x) all(is.na(x))))
dat2raw <- dat2raw[, -torm]

# rename columns
nms <- unlist(as.vector(dat2raw[1, ]))
names(nms) <- NULL
nms[is.na(nms)] <- paste0("temp_col_", which(is.na(nms)))

dat2raw <- dat2raw[2:nrow(dat2raw),]
names(dat2raw) <- nms

# convert all to character for bind
dat2raw <- dat2raw |>
  mutate(across(everything(), as.character))

# shift columns out of order
colshft1 <- which(!is.na(dat2raw[, 2, drop = T]))[1]

rows1 <- dat2raw[1:(colshft1 - 1), ]

rows2 <- dat2raw[colshft1:nrow(dat2raw), ]
rows2 <- rows2[, -ncol(dat2raw)]
rows2 <- bind_cols(
  tibble(empty2 = NA_character_),
  rows2
)
names(rows2) <- nms

# combine rows
dat2 <- bind_rows(rows1, rows2)

# remove empty columns
torm <- which(apply(dat2, 2, function(x) all(is.na(x))))
dat2 <- dat2[, -torm]

# final format page 2
dat2 <- dat2 |>
  mutate(
    temp_col_1 = case_when(
      `Facility Name` == 'Pasco Land O Lakes' ~ 'Pasco',
      grepl('^Manatee County|^Palmetto$|^Bradenton$', `Facility Name`) ~ 'Manatee',
      T ~ temp_col_1
    )
  ) |>
  fill(temp_col_1) |>
  mutate(across(-c(1:4), function(x) ifelse(x == '-', NA_character_, x))) |>
  mutate(across(-c(1:4), as.numeric))

#####
# final table 4.1

tab41 <- bind_rows(dat1, dat2) |>
  mutate(IPR = NA_real_) |>
  rename(County = temp_col_1) |>
  pivot_longer(
    cols = -c(County, `Facility ID`, `Facility Name`, `Utility`),
    names_to = "Type",
    values_to = "FlowMGD"
  ) |>
  mutate(
    `Facility Name` = case_when(
      `Facility Name` == 'Mantee County - North' ~ 'Manatee County - North',
      T ~ `Facility Name`
    ),
    Grouping = case_when(
      Type %in% c('Permitted Capacity') ~ 'Facility Information',
      Type %in% c('Total WW Flow', 'Total Reuse', 'Supplemental Flow', 'Transfers In', 'Transfers Out', 'WW Disposal') ~ '2020 Total Flow - Wastewater, Reuse, Transfers and Disposal',
      Type %in% c('Res Irrig/ Landscape', 'Golf Courses', 'Ag Irrig', 'IPR', 'Industrial', 'Wetlands', 'Other Reuse') ~ '2020 Beneficial Reuse by Category',
      Type %in% c('Sprayfields', 'Recharge/ RIBS', 'ATP/Other') ~ '2020 Non-Beneficial Reuse by Category',
      Type %in% c('Surface Water', 'Deep Well Injection') ~ '2020 Disposal'
    )
  )

save(tab41, file = 'data/tab41.RData')
write.csv(tab41, 'data/tab41.csv', row.names = FALSE)

#####
# final table 4-1 for Tampa Bay

load(file = 'data/tab41.RData')

tab41tb <- tab41 |>
  filter(`Facility ID` %in% facilities$permit)

save(tab41tb, file = 'data/tab41tb.RData')
write.csv(tab41tb, 'data/tab41tb.csv', row.names = FALSE)

# table 4-2 -----------------------------------------------------------------------------------

datraw <- extract_tables(pth, pages = c(4:6), method = "stream", output = "tibble")

#####
# first page
dat1raw <- datraw[[1]]

# header
dat1rawhd <- dat1raw[1:2, ]
dat1raw <- dat1raw[-c(1:2), ]

# remove rows with regional totals
dat1raw <- dat1raw |>
  filter(!grepl('Total', `...3`))

# remove empty columns
torm <- which(apply(dat1raw, 2, function(x) all(is.na(x))))
dat1raw <- dat1raw[, -torm]

# rename all to x1, x2, etc.
names(dat1raw) <- paste0('x', seq_len(ncol(dat1raw)))

# fill county using totals
dat1raw <- dat1raw |>
  mutate(
    x1 = case_when(
      grepl('Total', x2) ~ 'total',
      T ~ x1
    )
  ) |>
  fill(x1, .direction = 'up') |>
  mutate(
    x1 = case_when(
      x1 == 'total' ~ NA_character_,
      x1 == '0 .23Highlands' ~ 'Highlands',
      T ~ x1
    )
  ) |>
  fill(x1, .direction = 'down') |>
  filter(!(grepl('Total', x2, ignore.case = TRUE) | is.na(x2)))

# correct numbers with space before decimal, remove commas
dat1raw <- dat1raw |>
  mutate(across(-c(1:2), function(x) gsub('\\s\\.', '.', x))) |>
  mutate(across(-c(1:2), function(x) gsub(',', '', x)))

# manually fix entries in x3 for these rows with more than four spaces before separate
# 4, 14, 19, 25, 29, 31, 32, 39
# dat1raw[c(4, 14, 19, 25, 29, 31, 32, 39), 3]
dat1raw[4, 3] <- '749 825 201 201'
dat1raw[14, 3] <- '4610 5634 559 566'
dat1raw[19, 3] <- '11862 20742 633 4213'
dat1raw[25, 3] <- '69780 86187 25 5130'
dat1raw[29, 3] <- '3018 3018 44 44'
dat1raw[31, 3] <- '6637 6793 279 279'
dat1raw[32, 3] <- '1902 1917 191 191'
dat1raw[39, 3] <- '6709 8177 142 142'

dat1raw <- dat1raw |>
  separate(x3, c('x3a', 'x3b', 'x3c', 'x3d'), sep = '\\s')

# manually fix entries in x4 for these rows with more than two spaces before separate
# dat1raw[4, 'x4']
dat1raw[4, 'x4'] <- '548 624'

dat1raw <- dat1raw |>
  separate(x4, c('x4a', 'x4b'), sep = '\\s')

# x10 to two columns
dat1raw <- dat1raw |>
  separate(x10, c('x10a', 'x10b'), sep = '\\s')

# x12 to two columns
dat1raw <- dat1raw |>
  separate(x12, c('x12a', 'x12b'), sep = '\\s')

# convert all 'N/A' and '-' to NA
dat1raw <- dat1raw |>
  mutate(across(everything(), function(x) ifelse(x %in% c('N/A', '-'), NA_character_, x)))

# remove remaining spaces, convert to numeric
dat1raw <- dat1raw |>
  mutate(across(-c(1:2), function(x) gsub('\\s', '', x))) |>
  mutate(across(-c(1:2), as.numeric))

names(dat1raw) <- paste0('x', seq_len(ncol(dat1raw)))

#####
# second page
dat2raw <- datraw[[2]]

# header
dat2rawhd <- dat2raw[1:2, ]
dat2raw <- dat2raw[-c(1:2), ]

# remove rows with regional totals
dat2raw <- dat2raw |>
  filter(!grepl('Total', `...3`))

# remove empty columns
torm <- which(apply(dat2raw, 2, function(x) all(is.na(x))))
dat2raw <- dat2raw[, -torm]

# rename all to x1, x2, etc.
names(dat2raw) <- paste0('x', seq_len(ncol(dat2raw)))

# fill county using totals
dat2raw <- dat2raw |>
  mutate(
    x1 = case_when(
      grepl('Total', x2) ~ 'total',
      T ~ x1
    )
  ) |>
  fill(x1, .direction = 'up') |>
  mutate(
    x1 = case_when(
      x1 == 'total' ~ NA_character_,
      x1 == '-Charlotte' ~ 'Charlotte',
      T ~ x1
    )
  ) |>
  fill(x1, .direction = 'down') |>
  filter(!(grepl('Total', x2, ignore.case = TRUE) | is.na(x2)))

# correct numbers with space before decimal, remove commas
dat2raw <- dat2raw |>
  mutate(across(-c(1:2), function(x) gsub('\\s\\.', '.', x))) |>
  mutate(across(-c(1:2), function(x) gsub(',', '', x)))

# x3 to four columns
dat2raw <- dat2raw |>
  separate(x3, c('x3a', 'x3b', 'x3c', 'x3d'), sep = '\\s')

# x4 to two columns
dat2raw <- dat2raw |>
  separate(x4, c('x4a', 'x4b'), sep = '\\s')

# x10 to two columns
dat2raw <- dat2raw |>
  separate(x10, c('x10a', 'x10b'), sep = '\\s')

# x12 to two columns
dat2raw <- dat2raw |>
  separate(x12, c('x12a', 'x12b'), sep = '\\s')

# convert all 'N/A' and '-' to NA
dat2raw <- dat2raw |>
  mutate(across(everything(), function(x) ifelse(x %in% c('N/A', '-'), NA_character_, x)))

# remove remaining spaces, convert to numeric
dat2raw <- dat2raw |>
  mutate(across(-c(1:2), function(x) gsub('\\s', '', x))) |>
  mutate(across(-c(1:2), as.numeric))

names(dat2raw) <- paste0('x', seq_len(ncol(dat2raw)))

#####
# third page
dat3raw <- datraw[[3]]

# header
dat3rawhd <- dat3raw[1:2, ]
dat3raw <- dat3raw[-c(1:2), ]

# rename all to x1, x2, etc.
names(dat3raw) <- paste0('x', seq_len(ncol(dat3raw)))

# fill county using totals
dat3raw <- dat3raw |>
  mutate(
    x1 = case_when(
      grepl('Total', x3) ~ 'total',
      T ~ x1
    )
  ) |>
  fill(x1, .direction = 'up') |>
  mutate(
    x1 = case_when(
      x1 == 'total' ~ NA_character_,
      x1 == '-Charlotte' ~ 'Charlotte',
      T ~ x1
    )
  ) |>
  fill(x1, .direction = 'down') |>
  filter(!(grepl('Total', x3, ignore.case = TRUE)))

# remove empty columns
torm <- which(apply(dat3raw, 2, function(x) all(is.na(x))))
dat3raw <- dat3raw[, -torm]

# rename
names(dat3raw) <- paste0('x', seq_len(ncol(dat3raw)))

# correct numbers with space before decimal, remove commas
dat3raw <- dat3raw |>
  mutate(across(-c(1:2), function(x) gsub('\\s\\.', '.', x))) |>
  mutate(across(-c(1:2), function(x) gsub(',', '', x)))

# x3 to two columns
dat3raw <- dat3raw |>
  separate(x3, c('x3a', 'x3b'), sep = '\\s')

# x4 to two columns
# manually fix entries in x4 for these rows with more than two spaces before separate
# dat3raw[2, 'x4']
dat3raw[2, 'x4'] <- '650 653'
dat3raw <- dat3raw |>
  separate(x4, c('x4a', 'x4b'), sep = '\\s')

# x5 to two columns
dat3raw <- dat3raw |>
  separate(x5, c('x5a', 'x5b'), sep = '\\s')

# x11 to two columns
dat3raw <- dat3raw |>
  separate(x11, c('x11a', 'x11b'), sep = '\\s')

# x13 to two columns
dat3raw <- dat3raw |>
  separate(x13, c('x13a', 'x13b'), sep = '\\s')

# convert all 'N/A' and '-' to NA
dat3raw <- dat3raw |>
  mutate(across(everything(), function(x) ifelse(x %in% c('N/A', 'n/A', '-'), NA_character_, x)))

# remove remaining spaces, convert to numeric
dat3raw <- dat3raw |>
  mutate(across(-c(1:2), function(x) gsub('\\s', '', x))) |>
  mutate(across(-c(1:2), as.numeric))

names(dat3raw) <- paste0('x', seq_len(ncol(dat3raw)))

#####
# final table 4.2

nms <- list(
    'Total Service Area Population' = c('2020', '2045'),
    'Septic Population' = c('2020', '2045'),
    'Adjusted Service Area Population' = c('2020', '2045', '2020-2045 Population Increase'),
    '2020 Totals' = c('Total WW Flows (MGD)', 'Total Reuse (MGD)', 'Supplemental Flows (MGD)', 'WW Disposal (MGD)'),
    '2045 Projected Additional' = c('WW Flow Projection (MGD)', 'Supplemental Flows (MGD)'),
    '2045 Projected Totals' = c('Total WW Flows (MGD)', 'Supplemental Flows (MGD)', 'Total Reuse (MGD)', 'Beneficial Reuse at 75% (MGD)')
  ) |>
  enframe() |>
  unnest('value') |>
  unite('name', name, value, sep = '_') |>
  pull(name)
nms <- c('County', 'Facility Name', nms)

tab42 <- bind_rows(dat1raw, dat2raw, dat3raw) |>
  setNames(nms) |>
  mutate(
    `Facility Name` = case_when(
      `Facility Name` == 'Pinellas North-Dunn' ~ 'Pinellas North - Dunn',
      `Facility Name` == 'Pinellas South-Southcross' ~ 'Pinellas South - Southcross',
      `Facility Name` == 'St. Petersburg Southwest' ~ 'St. Petersburg - Southwest',
      `Facility Name` == 'St. Petersburg Northwest' ~ 'St. Petersburg - Northwest',
      `Facility Name` == 'St. Petersburg Northeast' ~ 'St. Petersburg - Northeast',
      `Facility Name` == 'Manatee County Southeast' ~ 'Manatee County - Southeast',
      `Facility Name` == 'Mantee County North' ~ 'Manatee County - North',
      T ~ `Facility Name`
    )
  ) |>
  pivot_longer(
    cols = -c(County, `Facility Name`),
    names_to = "Grouping",
    values_to = "Value"
  ) |>
  separate(Grouping, c('Grouping', 'Entry'), sep = '_')

save(tab42, file = 'data/tab42.RData')
write.csv(tab42, 'data/tab42.csv', row.names = FALSE)

#####
# final table 4-2 for Tampa Bay

load(file = 'data/tab41tb.RData')
load(file = 'data/tab42.RData')

tab42tb <- tab42 |>
  filter(`Facility Name` %in% tab41tb$`Facility Name`)

save(tab42tb, file = 'data/tab42tb.RData')
write.csv(tab42tb, 'data/tab42tb.csv', row.names = FALSE)

# # comparison ----------------------------------------------------------------------------------
#
# load(file = 'data/tab41tb.RData')
# load(file = 'data/tab42tb.RData')
#
# # make sure facility names between the two are shared
# tab41nm <- unique(tab41tb$`Facility Name`)
# tab42nm <- unique(tab42tb$`Facility Name`)
# setdiff(tab41nm, tab42nm)
#
# # see which facilities are missing (these are likely closed)
# facsnmc <- facilities |>
#   filter(grepl('^PS\\s\\-\\sDomestic', source)) |>
#   select(entity, facname, permit, facid) |>
#   distinct()
#
# facstbl <- tab41tb |>
#   select(County, `Facility ID`, `Facility Name`, Utility) |>
#   distinct()
#
# tmp <- anti_join(facsnmc, facstbl, by = c('permit' = 'Facility ID'))

