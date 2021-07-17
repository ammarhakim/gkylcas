#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <time.h>

/**
 * Gets wall-clock time in secs/nanoseconds.
 * 
 * @return Time object.
 */
struct timespec gkyl_wall_clock(void);

/**
 * Difference between two timespec objects.
 *
 * @param tstart Start time
 * @param tend End time 
 * @return Time object representing difference
 */
struct timespec gkyl_time_diff(struct timespec tstart, struct timespec tend);

/**
 * Difference between timespec object and "now", returned in seconds.
 * 
 * @param tm Timespec
 * @return Time in seconds
 */
double gkyl_time_diff_now_sec(struct timespec tm);

/**
 * Compute in secs time stored in timespec object.
 *
 * @param tm Timespec object
 * @return Time in seconds
 */
double gkyl_time_sec(struct timespec tm);
