resolveRiparianPolicy_sol6_1 <- function(policy, Jurisdiction) {
  fields <- c("small_stream", "large_stream", "small_lake", "large_lake")
  required <- c("jurisdiction", fields)
  if (!is.data.frame(policy) || !all(required %in% names(policy))) {
    stop(
      "riparianBufferPolicy must be a data.frame with columns: ",
      paste(required, collapse = ", ")
    )
  }
  policy <- as.data.frame(policy[, required], stringsAsFactors = FALSE)
  policy$jurisdiction <- trimws(as.character(policy$jurisdiction))
  policy$jurisdiction[tolower(policy$jurisdiction) == "default"] <- "default"
  policy$jurisdiction[policy$jurisdiction != "default"] <-
    toupper(policy$jurisdiction[policy$jurisdiction != "default"])
  if (
    anyNA(policy$jurisdiction) ||
      any(!nzchar(policy$jurisdiction)) ||
      anyDuplicated(policy$jurisdiction)
  ) {
    stop("Policy jurisdiction keys must be nonempty, non-missing and unique.")
  }
  if (!"default" %in% policy$jurisdiction) {
    stop("Policy must contain one default row.")
  }
  for (field in fields) {
    if (
      !is.numeric(policy[[field]]) ||
        any(!is.finite(policy[[field]])) ||
        any(policy[[field]] < 0)
    ) {
      stop(
        "Policy ",
        field,
        " must contain finite nonnegative numeric widths in metres."
      )
    }
  }
  # First available explicit key/name is used; raster IDs are not geographic boundaries.
  key <- intersect(
    c(
      "jurisdiction_code",
      "jurisdiction",
      "NAME_En",
      "NAME_EN",
      "name_en",
      "name",
      "NAME",
      "postal",
      "postal_code"
    ),
    names(Jurisdiction)
  )
  if (!length(key)) {
    stop(
      "Jurisdiction requires jurisdiction_code, jurisdiction, NAME_En or a recognized name field."
    )
  }
  original <- trimws(as.character(Jurisdiction[[key[1]]][, 1]))
  if (anyNA(original) || any(!nzchar(original))) {
    stop("Jurisdiction names/codes must not be missing or empty.")
  }
  mapping <- c(
    "Ontario" = "ON",
    "Quebec" = "QC",
    "Qu\u00e9bec" = "QC",
    "New Brunswick" = "NB",
    "Nova Scotia" = "NS",
    "Prince Edward Island" = "PE",
    "Newfoundland and Labrador" = "NL"
  )
  code <- toupper(original)
  mapped <- match(tolower(original), tolower(names(mapping)))
  code[!is.na(mapped)] <- unname(mapping[mapped[!is.na(mapped)]])
  code[tolower(original) == "default"] <- "default"
  fallback <- !code %in% policy$jurisdiction
  diagnostics <- unique(data.frame(
    jurisdiction = original[fallback],
    requested_code = code[fallback],
    policy_used = rep("default", sum(fallback)),
    stringsAsFactors = FALSE
  ))
  # Retain actual receiving-jurisdiction identities even when policies are identical.
  # Materialize explicit default rows so the engine matches jurisdiction_code directly.
  for (missingCode in unique(code[fallback])) {
    row <- policy[policy$jurisdiction == "default", , drop = FALSE]
    row$jurisdiction <- missingCode
    policy <- rbind(policy, row)
  }
  Jurisdiction$jurisdiction_code <- code
  if (nrow(diagnostics)) {
    message(
      "Default analytical policy used for: ",
      paste(diagnostics$jurisdiction, collapse = ", ")
    )
  }
  list(policy = policy, Jurisdiction = Jurisdiction, diagnostics = diagnostics)
}
