---
author: Jared Weakly
title: Programming 2 Assignment CS 445
date: \today
geometry: margin=4cm
---

# Introduction
What I did was I wrote a Bayesian inference network to analyze emails and determine whether or not they were spam.
It was a surprisingly short assignment compared to the first one, which I was quite happy about.
The program works by grabbing the data as a single line, parsing it using the cassava CSV library, wrangling it into the right format, and then doing the straightforward computing on it.

# Results

| Accuracy | Precision | Recall |
|----------+-----------+--------|
| 80.83%   | 93.72%    | 68.89% |

| True Positive | True Negative | False Positive | False Negative |
|---------------+---------------+----------------+----------------|
| 850           | 1010          | 384            | 57             |

# Analysis
The attributes are most likely not nearly as independent as Naive Bayes requires; most spam has several telltale characteristics in a row, not just one or two. Most non-spam also has several tell-tale signs.
The fact that there were 384 false positives is also an indicator that the Bayesian reasoning isn't quite accurate enough to assume all independent factors.
That being said, the 80.83% accuracy isn't bad at all considering how low effort this sort of naive analysis is, so it's quite good for what it is. A little bit more work, little bit more analysis, and it could be a great first line of defense before mail is scanned by a more expensive process.
