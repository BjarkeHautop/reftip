# First sentence of a flattened Rd description, capped so a runaway
# paragraph can't blow up a tooltip.
.first_sentence <- function(text, max_chars = 200) {
    if (is.null(text)) {
        return(NULL)
    }
    text <- gsub("\\s+", " ", trimws(text))
    if (nchar(text) == 0) {
        return(NULL)
    }

    m <- regexpr("[.!?](\\s|$)", text, perl = TRUE)
    sentence <- if (m == -1) text else substr(text, 1, m)
    sentence <- trimws(sentence)

    if (nchar(sentence) > max_chars) {
        sentence <- paste0(trimws(substr(sentence, 1, max_chars)), "...")
    }
    sentence
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
