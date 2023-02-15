# Chase Anderson
# Biomedical Optics HW1
# rando.py

import random
import matplotlib.pyplot as plt
import numpy as np

def generate_random_numbers():
    return [random.uniform(0, 1) for _ in range(10000)]

def divide_into_intervals(numbers, num_intervals):
    intervals = []
    for i in range(num_intervals):
        start = i / num_intervals
        end = (i + 1) / num_intervals
        intervals.append((start, end))
    interval_counts = [0] * num_intervals
    for number in numbers:
        for i, (start, end) in enumerate(intervals):
            if start <= number < end:
                interval_counts[i] += 1
                break
    return intervals, interval_counts

def plot_bar_graph(intervals, interval_counts):
    plt.bar(list(range(len(intervals))), interval_counts)
    plt.xlabel("Interval")
    plt.ylabel("Frequency")
    plt.show()

def calculate_mean_and_stddev(interval_counts):
    mean = np.mean(interval_counts)
    stddev = np.std(interval_counts)
    return mean, stddev

def run_experiment(num_runs, num_intervals):
    for run in range(num_runs):
        numbers = generate_random_numbers()
        intervals, interval_counts = divide_into_intervals(numbers, num_intervals)
        mean, stddev = calculate_mean_and_stddev(interval_counts)
        print("Run", run + 1)
        print("Mean:", mean)
        print("Standard deviation:", stddev)
        print()
        plot_bar_graph(intervals, interval_counts)

run_experiment(5, 20)