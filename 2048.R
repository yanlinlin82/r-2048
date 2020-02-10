suppressMessages(library(tidyverse))

n <- 4
m <- tibble(id = 1:(n*n),
            row = rep(1:n, each = n),
            col = rep(1:n, n),
            up = ifelse(id > n, id - 4, 0),
            down = ifelse(id <= n*(n-1), id + 4, 0),
            left = ifelse(id %% n != 1, id - 1, 0),
            right = ifelse(id %% n != 0, id + 1, 0),
            num = 1L,)

draw <- function() {
  print(m)
  g <- m %>%
    mutate(txt = ifelse(num >= 2, num, ""),
           txt = factor(txt, levels = c("", 2^(1:13)))) %>%
    ggplot(aes(x = col, y = -row, fill = txt, label = txt)) +
    geom_tile(color = "#999999", size = 3) +
    scale_fill_manual(values = c("#CCCCCC", rev(heat.colors(13))), drop = FALSE) +
    geom_text(size = 10, color = "#666666", fontface = "bold") +
    guides(size = FALSE, color = FALSE, fill = FALSE) +
    ggtitle("Press <Up>,<Down>,<Left>,<Right> to play, <ESC> to exit") +
    theme_void() +
    theme(plot.title = element_text(size = 12, hjust = .5, face = "bold", margin = margin(t = 20)))
  print(g, newpage = FALSE)
}

add <- function(n = 1) {
  cand <- which(m$num == 1)
  sel <- sample(seq_along(cand), n)
  m$num[cand[sel]] <<- 2
}

pull <- function(d) {
  message(d)
  while (TRUE) {
    a <- m %>% select(id, num) %>% rename(id_to = id) %>%
      inner_join(m %>% select(id, num, up, down, left, right),
                 by = c(id_to = d), suffix = c("_to", "")) %>%
      filter(num > 1, (num_to == 1 | num_to == num)) %>%
      arrange(ifelse(d %in% c("up", "left"), 1, -1) * id)
    print(a)
    if (nrow(a) == 0) break
    if (a$num_to[1] == 1) {
      m$num[a$id[1]] <<- 1
      m$num[a$id_to[1]] <<- a$num[1]
    } else {
      m$num[a$id[1]] <<- 1
      m$num[a$id_to[1]] <<- a$num[1] * 2
    }
    print(m)
  }
}

add(2); draw()
setGraphicsEventHandlers(onKeybd = function(x) { x })
while (any(m$num == 1)) {
  v <- getGraphicsEvent()
  if (is.null(v) || v == "\033") break
  if (v %in% c("Up", "Down", "Left", "Right")) pull(str_to_lower(v))
  draw(); Sys.sleep(.5); add(); draw()
}
