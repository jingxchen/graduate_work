#-------------------------------------------------------------------------------
# Title: R functions for problem set 1 question 3
# Author: Jingxian(Derrick) Chen
# Date: Sep 20th, 2019
# Reference data: 
# https://github.com/jbhender/Stats506_F19/tree/master/problem_sets/data
#-------------------------------------------------------------------------------


# a)
# Functon description: to solve question 3a, translate input data to begin with
#                      time zero at the origin.
#
# Input: an n * 3 matrix representing the trajectory (x, y, t).
#
# Output: an n * 3 matrix representing the trajectory (x, y, t) beginning with
#         time zero at the origin.
translate <- function(trajectories_data) {
  translate_data <- trajectories_data
  translate_data[, 1] <- trajectories_data[, 1] - trajectories_data[1, 1]
  translate_data[, 2] <- trajectories_data[, 2] - trajectories_data[1, 2]
  translate_data[, 3] <- trajectories_data[, 3] - trajectories_data[1, 3]
  translate_data
}
#-------------------------------------------------------------------------------


# b)
# Function description: to compute the angle theta formed by the secant line 
#                       connecting the origin and the final position in the 
#                       trajectory.
#
# Input: the translated n * 3 matrix representing the trajectory (x, y, t).
#
# Output: a double digit representing the angle between [-pi, pi].
angle_compute <- function(trajectories_data) {
  x_final <- trajectories_data[nrow(trajectories_data), 1]
  y_final <- trajectories_data[nrow(trajectories_data), 2]
  angle <- as.double(atan2(y_final, x_final))
  angle
}
#-------------------------------------------------------------------------------


# c)
# Function description: to rotate the (x, y) coordinates of a trajectory so that
#                       the final point lies along the positive x-axis.
#
# Input: #1: the translated n * 3 matrix representing the trajectory (x, y, t).
#        #2: the angle computed by the angle_compute function.
#
# Output: an n * 3 matrix representing the trajectory (x, y, t) whose final point
#         lies along the positive x-axis. 
rotate <- function(trajectories_data, theta) {
# A represents the rotate matrix.
  new_data <- trajectories_data
  A <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  new_data[, 1:2] <- t(A %*% t(trajectories_data[, 1:2]))
  new_data
}
#-------------------------------------------------------------------------------


# d)
# Function description: to combine the functions above that normalizes an n * 3
#                       trajectory matrix to begin at the origin and end on the
#                       positive x-axis.
#
# Input: the origin n * 3 matrix representing the trajectory (x, y, t)
#
# Output: an n * 3 trajectory matrix to begin at the origin and end on the 
#         positive x-axis.
normalize <- function(trajectories_data) {
  translate_data <- translate(trajectories_data)
  theta <- angle_compute(translate_data)
  normalize_data <- rotate(translate_data, theta)
  normalize_data
}
#-------------------------------------------------------------------------------


# e)
# Function description: to compute the values of question e giving specific 
#                       subject_nr and count_trial.
#
# Input: the normalized trajectory matrix.
# 
# Output: a 1 * 4 vector consists of total (Euclidean) distance traveled, maximum
#         absolute deviation, average absolute deviation, absolute area.
compute <- function(trajectories_data) {
  data <- normalize(trajectories_data)
  tot_dist <- 0
  max_abs_dev <- 0
# temp_abs_dev is for computing the max_abs_dev.
  temp_abs_dev <- 0
  avg_abs_dev <- 0
  AUC <- 0
  t_final <- data[nrow(data), 3]
  x_final <- data[nrow(data), 1]
  y_final <- data[nrow(data), 2]
# d2 is for computing the temporary absolute deviation.
  d2 <- (y_final)^2 + (x_final)^2
  for (i in 1:nrow(data)) {
    temp_abs_dev <- abs(y_final * data[i, 1] - x_final * data[i, 2]) / sqrt(d2)
    avg_abs_dev <- avg_abs_dev + temp_abs_dev
    if (i != nrow(data)) {
# d1 is for computing the total distance.
      d1 <- (data[i, 1] - data[i + 1, 1])^2 + (data[i, 2] - data[i + 1, 2])^2
      tot_dist <- tot_dist + sqrt(d1)
      AUC <- AUC + diff(data[, 1])[i] * (abs(data[i + 1, 2]) + abs(data[i, 2])) / 2
    }
    if (temp_abs_dev >= max_abs_dev) {
      max_abs_dev <- temp_abs_dev
    }
  }
  avg_abs_dev <- avg_abs_dev / nrow(data)
  c("tot_dist" = as.double(tot_dist), "max_abs_dev" = as.double(max_abs_dev),
    "avg_abs_dev" = as.double(avg_abs_dev), "AUC" = as.double(AUC))
}

