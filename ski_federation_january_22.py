import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

# Create a plot to show my time compared to opponent's
# in the two-competitor scenario
mu, sigma = 100, 10 # parameters for distribution of the times of each run
races = 100000 # number of competitions to simulate
my_first_time = np.random.normal(mu, sigma, races)
my_second_time = np.random.normal(mu, sigma, races)
opponent_first_time = np.random.normal(mu, sigma, races)
opponent_second_time = np.random.normal(mu, sigma, races)

# Color-code plot points according to who won the first run
first_run_winner = np.where(my_first_time < opponent_first_time, 'r', 'b')

plt.style.use('seaborn')
fig, ax = plt.subplots()
ax.scatter(my_first_time + my_second_time, opponent_first_time + opponent_second_time, 
    s=1, c=first_run_winner, alpha=0.2)

# Add a line to divide my overall wins from opponent's
x = np.linspace(mu*1.6, mu*2.4, 100)
y = x
ax.plot(x, y, c=(0, 0, 0))

# Create legend
legend_elements = [Line2D([0], [0], marker='o', color='w', label="My first run was faster",
                          markerfacecolor='r', markersize=5),
                   Line2D([0], [0], marker='o', color='w', label="Opponent's first run was faster",
                          markerfacecolor='b', markersize=5)]
ax.legend(handles=legend_elements, loc = 'upper right')

# Axis titles
ax.set_title("Combined Times for Two Runs")
ax.set_xlabel("My combined time")
ax.set_ylabel("Opponent's combined time")

ax.text(mu*1.5, mu*2.5, "Points above the line \nindicate that I won")
ax.text(mu*2, mu*1.4, "Points below the line \nindicate that my opponent won")

plt.show()

# A function for the probability of winning overall, 
# given a win in the first run
def riddler_ski_challenge(competitors = 2, races = 1000000):
    """Determine how often the winner of the first run is the overall winner"""
    first_times_matrix = np.random.normal(100, 10, size = (races, competitors))
    second_times_matrix = np.random.normal(100, 10, size = (races, competitors))
    total_times_matrix = first_times_matrix + second_times_matrix
    first_times_min = np.argmin(first_times_matrix, axis = 1)
    total_times_min = np.argmin(total_times_matrix, axis = 1)
    same_winner = 0
    for pos in range(races - 1):
        if first_times_min[pos] == total_times_min[pos]:
            same_winner += 1
    return same_winner/races

#print(riddler_ski_challenge(competitors = 30))