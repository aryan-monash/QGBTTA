extract_text <- function(df) {
    df$Document <- as.character(NA)
    for (src in df$Source) {
        doc <- tryCatch({
            read_html(src) %>%
                html_nodes("p") %>%
                html_text()
        }, error = function(e) {
            cor_enc <- html_encoding_guess(src)$encoding[1]
            print(cor_enc)
            src_fixed <- repair_encoding(src, from = cor_enc)
            read_html(src_fixed) %>%
                html_nodes("p") %>%
                html_text()
        })
        df$Document[df$Source == src] <- paste(doc, collapse = " ")
    }
    return(df)
}
