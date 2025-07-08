library(tabulapdf)
library(dplyr)
library(tidyr)

pth <- "data-raw/2025 RWSP Appendix 4-1 Reuse Estimates and Projections Public Draft FINAL.pdf"


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

rows1 <- dat1raw[1:(colshft1 - 1), ]

rows2 <- dat2raw[colshft1:(colshft2 - 1), ]
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
  mutate(across(-c(1:4), as.numeric))

#####
# final table 4.1

tab41 <- bind_rows(dat1, dat2)

write.csv(tab41, 'data/table4-1.csv', row.names = FALSE)

# table 4-2 -----------------------------------------------------------------------------------


# Tampa Bay subset ----------------------------------------------------------------------------


