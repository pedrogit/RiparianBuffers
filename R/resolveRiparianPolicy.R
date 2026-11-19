#' Attach a policy key to each jurisdiction and fill in missing policies
#'
#' `policy` is a table with a `jurisdiction` column and four widths in metres
#' (small/large stream, small/large lake), including one "default" row.
#' `Jurisdiction` is a polygon vector with a recognised name or code column.
#' Province names (for example "Ontario", "Québec") are translated to codes
#' (ON, QC). Jurisdictions without a policy row get a copy of the default row
#' under their own code, and are listed in `diagnostics`. Returns list(policy,
#' Jurisdiction, diagnostics); the `Jurisdiction` copy gains `jurisdiction_code`.
resolveRiparianPolicy <- function(policy, Jurisdiction) {
  # Require one jurisdiction key and a width in metres for each hydrological class.
  fields <- c("small_stream", "large_stream", "small_lake", "large_lake")
  required <- c("jurisdiction", fields)
  if (!is.data.frame(policy) || !all(required %in% names(policy))) {
    stop(
      "riparianBufferPolicy must be a data.frame with columns: ",
      paste(required, collapse = ", ")
    )
  }
  # Work on a normalized local table; discard unrelated columns without changing inputs.
  policy <- as.data.frame(policy[, required], stringsAsFactors = FALSE)
  policy$jurisdiction <- trimws(as.character(policy$jurisdiction))
  policy$jurisdiction[tolower(policy$jurisdiction) == "default"] <- "default"
  policy$jurisdiction[policy$jurisdiction != "default"] <-
    toupper(policy$jurisdiction[policy$jurisdiction != "default"])
  # Duplicate or missing keys would make policy matching ambiguous.
  if (
    anyNA(policy$jurisdiction) ||
      any(!nzchar(policy$jurisdiction)) ||
      anyDuplicated(policy$jurisdiction)
  ) {
    stop("Policy jurisdiction keys must be nonempty, non-missing and unique.")
  }
  # Every unlisted jurisdiction needs an explicit analytical fallback row.
  if (!"default" %in% policy$jurisdiction) {
    stop("Policy must contain one default row.")
  }
  # Width zero is valid; negative, missing or infinite distances are not.
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
  # Preserve original jurisdiction labels for readable fallback diagnostics.
  original <- trimws(as.character(Jurisdiction[[key[1]]][, 1]))
  if (anyNA(original) || any(!nzchar(original))) {
    stop("Jurisdiction names/codes must not be missing or empty.")
  }
  # Translate recognized province names, including accented Quebec, to CSV keys.
  mapping <- c(
    "Ontario" = "ON",
    "Quebec" = "QC",
    "Qu\u00e9bec" = "QC",
    "New Brunswick" = "NB",
    "Nova Scotia" = "NS",
    "Prince Edward Island" = "PE",
    "Newfoundland and Labrador" = "NL"
  )
  # Explicit codes pass through; known names are replaced by their canonical codes.
  code <- toupper(original)
  mapped <- match(tolower(original), tolower(names(mapping)))
  code[!is.na(mapped)] <- unname(mapping[mapped[!is.na(mapped)]])
  code[tolower(original) == "default"] <- "default"
  # Record each unmatched identity before materializing its default widths.
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
  # Add resolved matching keys only to the local geometry returned to the caller.
  Jurisdiction$jurisdiction_code <- code
  # Make analytical defaults visible rather than silently treating them as legal policy.
  if (nrow(diagnostics)) {
    message(
      "Default analytical policy used for: ",
      paste(diagnostics$jurisdiction, collapse = ", ")
    )
  }
  # Return the expanded policy, keyed polygons and audit table together.
  list(policy = policy, Jurisdiction = Jurisdiction, diagnostics = diagnostics)
}
