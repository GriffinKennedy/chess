library('rchess')
library(tidyverse)

Plot_My_Path <- function(pgn, piece, title = paste("Path of the", piece)){
  
  #input: All variables are strings. 
  #output: graph of the path of the desired piece.
  #instructions: have the "rchess" and "tidyverse" libraries running
  #note: This function only works for single pgn files. 
  
  #First, read in your pgn file
  pgn <- readLines(pgn, warn = FALSE)
  pgn <- paste(pgn, collapse = "\n")
  cat(pgn)
  chsspgn <- Chess$new()
  chsspgn$load_pgn(pgn)
  my_game_data <- chsspgn$history_detail() %>% arrange(number_move)
  
  #Next, create piecepath_df by joining the chessboard data and your pgn data 
  chessboard_df <- rchess:::.chessboarddata()
  piecepath_df <-  my_game_data %>%
    left_join(chessboard_df %>% 
                dplyr::rename(from = 'cell', 'x.from' = 'x', 'y.from' = 'y'),
              by = "from") %>%
    left_join(chessboard_df %>% 
                dplyr::rename('to' = 'cell', 'x.to' = 'x', 'y.to' = 'y') %>%
                select(-cc, -col, -row),
              by = "to") %>%
    
    #These mutated columns allow for the different aspects of geom_curve
    #check out http://rpubs.com/jbkunst/geom_curve for more info
    mutate(x_gt_y = abs(x.to - x.from) > abs(y.to - y.from),
           xy_sign = sign((x.to - x.from)*(y.to - y.from)) == 1,
           x_gt_y_equal_xy_sign = x_gt_y == xy_sign)
  
  #Last, plot the path of your piece
  piece_1 <- piece
  ggplot() +
    geom_tile(data = chessboard_df, aes(x, y, fill = cc)) +
    geom_curve(data = piecepath_df %>% filter(piece.x == piece_1,
                                              x_gt_y_equal_xy_sign),
               aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
               position = position_jitter(width = 0.075, height = 0.075),
               curvature = 0.50, angle = -45, alpha = 1.00, color = "white", size = .85) +
    geom_curve(data = piecepath_df %>% filter(piece.x == piece_1, 
                                              !x_gt_y_equal_xy_sign),
               aes(x = x.from, y = y.from, xend = x.to, yend = y.to),
               position = position_jitter(width = 0.075, height = 0.075),
               curvature = -0.50, angle = 45, alpha = 1.00, color = "white", size = .85) +
    ggtitle(title) +  
    scale_fill_manual(values =  c("gray70", "grey90")) + #colors of the chess board (black, white)
    coord_equal() +                                      #allows for the chess board to be symetric
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.ticks = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()) +
    scale_y_continuous(name = '',
                       breaks=c(0,1,2,3,4,5,6,7,8,9),
                       labels = c('', 1,2,3,4,5,6,7,8,''),
                       limits = c(0,9)) + 
    scale_x_continuous(name = '',
                       breaks=c(0,1,2,3,4,5,6,7,8,9),
                       labels = c('','A','B','C','D','E','F','G', 'H', ''),
                       limits = c(0,9))
}

pgn <- "~/Desktop/GitHub/Chess/edyubero_vs_brain-gainz_2017-09-21.pgn" 
Plot_My_Path(pgn, "White King")
