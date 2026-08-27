# First sentence of a flattened Rd description, capped so a runaway
# paragraph can't blow up a tooltip. Returns list(text, clipped): `clipped`
# is TRUE when no sentence boundary was found within `max_chars`, i.e. the
# brief was cut off mid-sentence rather than at a natural end.
.first_sentence <- function(text, max_chars = 200) {
    if (is.null(text)) {
        return(list(text = NULL, clipped = FALSE))
    }
    text <- gsub("\\s+", " ", trimws(text))
    if (nchar(text) == 0) {
        return(list(text = NULL, clipped = FALSE))
    }

    m <- regexpr("[.!?](\\s|$)", text, perl = TRUE)
    has_boundary <- m != -1
    sentence <- if (has_boundary) substr(text, 1, m) else text
    sentence <- trimws(sentence)

    clipped <- !has_boundary && nchar(sentence) > max_chars
    if (nchar(sentence) > max_chars) {
        sentence <- paste0(trimws(substr(sentence, 1, max_chars)), "...")
    }
    list(text = sentence, clipped = clipped)
}

.html_escape <- function(text) {
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text <- gsub("\"", "&quot;", text, fixed = TRUE)
    text
}

.html_unescape <- function(text) {
    text <- gsub("&lt;", "<", text, fixed = TRUE)
    text <- gsub("&gt;", ">", text, fixed = TRUE)
    text <- gsub("&quot;", "\"", text, fixed = TRUE)
    text <- gsub("&#39;", "'", text, fixed = TRUE)
    text <- gsub("&amp;", "&", text, fixed = TRUE)
    text
}
