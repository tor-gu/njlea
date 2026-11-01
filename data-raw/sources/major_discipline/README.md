# NJ OAG Major Discipline: agency extract

`major_discipline_agencies.csv` holds the distinct (`County`, `Agency`, `Year`) rows of the NJ
Office of the Attorney General's Major Discipline data, written exactly as the source writes
them. It is the only part of the file njlea uses.

The source file names individual officers, so it is **not committed**; it is gitignored and
kept locally only.

- Source: NJ OAG Major Discipline, https://www.njoag.gov/majordiscipline/
- Box folder: https://njoag.box.com/s/v93zq4oxauyb3bazpbph077fzmb2roje
- File: `Major Discipline Data 2020-2025.xlsx` (Box file id `f_2281726465952`), saved as
  `Major_Discipline_Data_2020-2025.xlsx`
- Retrieved: 2026-10-31
- SHA-256 of the xlsx:
  `07ddea20d4b16a246b330084da4e681fefe90753a73b53bcb28ea4acfe3c71f2`
- The xlsx has one sheet, 3,005 rows and 14 columns, covering 2020-06-15 to 2025. The extract
  has 838 rows: 340 (County, Agency) spellings over the years 2020-2025.

To rebuild the extract from a fresh download:

```r
x <- readxl::read_excel("Major_Discipline_Data_2020-2025.xlsx", col_types = "text")
e <- unique(x[c("County", "Agency", "Year")])
e <- e[order(e$County, e$Agency, e$Year, method = "radix"), ]
write.csv(e, "major_discipline_agencies.csv", row.names = FALSE, fileEncoding = "UTF-8")
```
