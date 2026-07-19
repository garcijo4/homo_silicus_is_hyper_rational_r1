#!/usr/bin/env Rscript

# Cross-platform validation of every file listed in data/CHECKSUMS.md5.
# Uses only base R and accepts ADT_DATA_ROOT as an optional data override.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
} else {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
}

data_root <- Sys.getenv("ADT_DATA_ROOT", file.path(repo_root, "data"))
checksum_file <- file.path(data_root, "CHECKSUMS.md5")

if (!file.exists(checksum_file)) {
  stop("Checksum manifest not found: ", checksum_file)
}

lines <- readLines(checksum_file, warn = FALSE)
lines <- lines[nzchar(trimws(lines)) & !grepl("^\\s*#", lines)]
matches <- regexec("^([0-9a-fA-F]{32})\\s+\\*?(.+)$", lines)
parts <- regmatches(lines, matches)

if (!length(parts) || any(lengths(parts) != 3L)) {
  stop("Malformed line in ", checksum_file)
}

expected <- tolower(vapply(parts, `[[`, character(1), 2L))
relative <- vapply(parts, `[[`, character(1), 3L)
paths <- file.path(data_root, relative)

missing <- !file.exists(paths)
actual <- rep(NA_character_, length(paths))
actual[!missing] <- unname(tools::md5sum(paths[!missing]))
ok <- !missing & tolower(actual) == expected

result <- data.frame(
  file = relative,
  expected_md5 = expected,
  actual_md5 = actual,
  status = ifelse(missing, "MISSING", ifelse(ok, "OK", "MISMATCH")),
  check.names = FALSE
)
print(result, row.names = FALSE)

if (!all(ok)) {
  stop(sum(!ok), " checksum validation(s) failed.")
}

cat(sprintf("Verified %d data files: all MD5 checksums match.\n", length(paths)))
