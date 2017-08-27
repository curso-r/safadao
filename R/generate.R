sample_mod <- function(preds, diversity = 1) {
  preds <- log(preds) / diversity
  exp_preds <- exp(preds)
  preds <- exp_preds / sum(exp_preds)
  which.max(as.integer(stats::rmultinom(1, 1, preds)))
}

gerar_txt <- function(model, txt, diversity = 1.0, limit = 300, maxlen = 40) {
  chars <- sort(unique(txt))
  txt_index <- which(txt[-length(txt)] == '@')
  start_index <- sample(txt_index, size = 1) + 1L
  id_txt <- which(txt_index == start_index)
  cat(sprintf("Indice %d ---------------\n\n", id_txt))
  sentence <- txt[start_index:(start_index + maxlen - 1)]
  generated <- paste0(c(sentence, '|'), collapse = "")
  next_char <- ""
  total_chars <- 0
  while (next_char != '@' && total_chars < limit) {
    x <- sapply(chars, function(x) {as.integer(x == sentence)})
    dim(x) <- c(1, dim(x))
    next_index <- sample_mod(stats::predict(model, x), diversity)
    next_char <- chars[next_index]
    generated <- paste0(generated, next_char, collapse = "")
    sentence <- c(sentence[-1], next_char)
    total_chars <- total_chars + 1
  }
  s_final <- substr(generated, 1, nchar(generated) - 1)
  if (total_chars == limit) s_final <- paste0(s_final, '\n<truncated>')
  s_final
}

#' Generates music from Wesley Safadao
#'
#' Generates music from Wesley Safadao using keras model
#'
#' @param rand Random number
#'
#' @export
gen <- function(rand = runif(1)) {
  gerar_txt(modelo, txt)
}
