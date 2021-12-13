# http://schw4rzr0tg0ld.s3-website.eu-central-1.amazonaws.com/blog/2016/12/ucl-b016.html

# champions league draw 19-20
library(data.table)  # for data handling
library(parallel)  # for parallelizing simulation
library(ggplot2)  # for plotting
library(gridExtra)  # for plotting
library(ggpubr)
library(knitr)  # for formatting

seed_teams <- c('Man City', 'Liverpool', 'Ajax', 'Real Madrid',
                'Bayern', 'Man Utd', 'Lille', 'Juventus')
seed_fa <- c('ENG', 'ENG', 'NED', 'ESP', 'GER', 'ENG', 'FRA', 'ITA')

unseed_teams <- c('Paris', 'Atletico', 'Sporting CP', 'Inter',
                  'Benfica', 'Villarreal', 'Salzburg', 'Chelsea')
unseed_fa <- c('FRA', 'ESP', 'POR', 'ITA', 'POR', 'ESP', 'AUS', 'ENG')

# get the data
teams <- data.table(
  team_f = seed_teams,
  team_f_id = 1L:8L,
  team_f_a = seed_fa,
  team_f_g = LETTERS[1:8],
  team_s = unseed_teams,
  team_s_id = 9L:16L,
  team_s_a = unseed_fa,
  team_s_g = LETTERS[1:8]
)

# cross-join tow data frames
CrossJoinDT <- function(x, y) {
  cj <- setkey(x[,c(k = 1, .SD)], k)[y[, c(k = 1, .SD)],
                                     allow.cartesian=TRUE][,k:=NULL]
  return(cj)
}

# check if values of one row in a data frame are unique
IsUniqDraw <- function(x) {
  isuniq <- apply(x, 1, function(l) {length(unique(l))}) == ncol(x)
  return(isuniq)
}

# possible matches only respecting rule (1)
matches <- CrossJoinDT(teams[, 1:4, with = FALSE], 
                       teams[, 5:8, with = FALSE])

# enforce rules (2) and (3)
matches <- matches[team_f_a != team_s_a & team_f_g != team_s_g, ]

# split by team first
matches_split <- split(matches[, list(team_f_id, team_s_id)], matches$team_f_id)

# create data frame of all possible draws
draws <- data.table(NULL)
for (m in matches_split) {
  draws <- CrossJoinDT(draws, m)
  draws <- draws[IsUniqDraw(draws), ]
}

# get into long format
draws_long <- matrix(t(as.matrix(draws)), ncol = 2, byrow = TRUE)

# fill wit names instead of ids
draws_names <- data.table(matrix(c(teams$team_f, teams$team_s)[draws_long], 
                                 nrow = nrow(draws_long)))
draws_names$draw_id <- sort(rep(seq_len(nrow(draws)), 8))
setnames(draws_names, c("f", "s", "draw_id"))


SimulateDraw <- function(draws_sim) {
  # initially available possibilities
  avail_draws <- draws_sim
  avail_s <- 9:16
  
  for (i in 1:7) {
    # choose allowed runner-up
    s1 = sample(avail_s, 1)
    # check possible opponents
    avail_f <- unique(avail_draws[s == s1, ]$f)
    if (length(avail_f) > 1) {
      f1 = sample(avail_f, 1)
    } else {
      f1 = avail_f
    }
    # calculate remaining draws
    remaining_draw_ids <- avail_draws[s == s1 & f == f1, ]$draw_id
    # update available runners-up/group first and draws
    avail_s <- setdiff(avail_s, s1)
    avail_draws <- avail_draws[draw_id %in% remaining_draw_ids, ]
  }
  return(avail_draws$draw_id[1])
}

# use ids in this case
draws_sim <- data.table(cbind(draws_long, sort(rep(seq_len(nrow(draws)), 8))))
setnames(draws_sim, c("f", "s", "draw_id"))

# theoretical result (Proof by exhaustion)
# summarizing result, method 1
draws_sim2 <- draws_sim
draws_sim2$s <- draws_sim2$s - 8

draw_matrix <- draw_matrix <- matrix(0, ncol = 8, nrow = 8)
n <- nrow(draws)

for(i in 1:n){
  draw_pairs <- subset(draws_sim2, draws_sim2$draw_id == i)
  r_idx <- draw_pairs$f
  c_idx <- draw_pairs$s
  draw_matrix[cbind(r_idx, c_idx)] <- draw_matrix[cbind(r_idx, c_idx)] + 1
}

draw_prob_theoretical <- draw_matrix/n

rownames(draw_prob_theoretical) <- seed_teams
colnames(draw_prob_theoretical) <- unseed_teams


# run N draws in parallel
# make cluster
cl <- makeCluster(detectCores() - 1)

# export libraries and data
clusterEvalQ(cl, library(data.table))
clusterExport(cl, c("draws_sim", "SimulateDraw"))

# replicate in parallel
set.seed(1337)
N <- 5 * 10^4
sim_draws <- parSapply(cl, 1:N, function(x) {
  SimulateDraw(draws_sim)
})

# stop the cluster and save
stopCluster(cl)

# test on Goodness-of-Fit where under the null hypothesis each draw is equally 
# likely (default of the chisq.test function)
chisq.test(table(sim_draws))

# compute the probability for liverpool
# liv_sim <- subset(draws_names, draws_names$f == 'Liverpool')
# liv_opp <- rep(NA, N)
# for(i in 1:N){
#   draw_id <- sim_draws[i]
#   liv_opp[i] <- liv_sim$s[liv_sim$draw_id == draw_id]
# }
# table(liv_opp)/N

# matrix mapping
# method 2 same time as method 1, but more space used
matrix_list <- list()

for(i in 1:N){
  draw_pairs <- subset(draws_sim2, draws_sim2$draw_id == sim_draws[i])
  draw_matrix <- matrix(0, ncol = 8, nrow = 8)
  r_idx <- draw_pairs$f
  c_idx <- draw_pairs$s
  draw_matrix[r_idx, c_idx] <- diag(8)
  matrix_list[[i]] <- draw_matrix
}

final_matrix <- Reduce('+', matrix_list)

draw_prob_sim <- as.data.frame(final_matrix/N)
rownames(draw_prob_sim) <- seed_teams
colnames(draw_prob_sim) <- unseed_teams

# ploting results
draw_df <- expand.grid(seed_teams,unseed_teams)

draw_df <- data.frame(draw_df, c(final_matrix/N))
colnames(draw_df) <- c('home', 'away', 'prob')

# order of winner need to fix
p1 <- ggplot(draw_df, aes(away, rev(home), fill = prob)) + scale_x_discrete(position = "top") + 
      geom_tile() + scale_fill_distiller(palette = 'OrRd', direction = 1) + 
      xlab('Runner-Up') + ylab('Winner') +  theme_minimal()

library(ggpubr)

p_theory <- ggtexttable(round(draw_prob_theoretical,3), 
            theme = ttheme(
              colnames.style = colnames_style(color = "white", fill = "#0E1E5B"),
              rownames.style = rownames_style(color = "white", fill = "#0E1E5B"),
              tbody.style = tbody_style(color = "white", face = "bold", fill = c("#3562A6", "#6594C0"))
            )
)

p_real <- ggtexttable(round(draw_prob_sim,3), 
            theme = ttheme(
              colnames.style = colnames_style(color = "white", fill = "#0E1E5B"),
              rownames.style = rownames_style(color = "white", fill = "#0E1E5B"),
              tbody.style = tbody_style(color = "white", face = "bold", fill = c("#3562A6", "#6594C0"))
            )
)

ggsave(filename = 'cl.png', plot = p_real, dpi = 300)

