#READ PGN data in and create a table
#currently it is not working as the pgn isn't a single string
#it is multiple lines.

pgn <- read.table("~/Downloads/chess_com_games_2017-09-24.pgn", quote="", sep="\n", stringsAsFactors=FALSE)

# get column names
column_names <- sub("\\[(\\w+).+", "\\1", pgn[1:13,1]) #reads word after [ and before "
column_names[13] <- "PGN"


pgn.df <- data.frame(matrix(sub("\\[\\w+ \\\"(.+)\\\"\\]", "\\1", pgn[,1]),
                            byrow=TRUE, ncol=13)) #reads in string in quotes

names(pgn.df) <- column_names
View(pgn.df)
