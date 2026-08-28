# First sentence of a flattened Rd description, capped so a runaway
# paragraph can't blow up a tooltip. Returns list(text, clipped): `clipped`
# is TRUE whenever the result had to be cut at `max_chars`, whether or not
# a sentence boundary was ever found -- a single sentence that runs past
# max_chars is just as mid-word-truncated as one with no boundary at all.
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

    clipped <- nchar(sentence) > max_chars
    if (clipped) {
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
