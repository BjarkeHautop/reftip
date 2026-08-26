# First sentence of a flattened Rd description, whitespace-normalized and
# capped so a runaway paragraph can't blow up a tooltip. Simpler than
# DocumenterCodeBlocks's version (references.jl `_first_sentence`) --
# R docstrings don't have the `sort!`-style bang-function or `e.g.`-heavy
# abbreviation problem to the same degree -- but the same idea: cut at the
# first `.`/`!`/`?` followed by whitespace or end of string.
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
