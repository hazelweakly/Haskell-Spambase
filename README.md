# spambase

Turns out that Haskell code is way nicer when you don't have to plum state through state through state...
RIP in peace, MNIST, you were dead before you started--this one was far nicer to write.
Actually works, too.

57 attributes

- Last column: spam=1 not=0
* First 48 cols: 0-100% freq of word

* 48 cols: 0-100% freq of word <- word given in spambase.names
* 6 columns: 0-100% char frequency
* 1 col: ℝ avg len uninterrupted seq cap letters
* 1 col: ℝ max len uninterrupted seq cap letters
* 1 col: ℝ total num cap letters
* 1 col: {0,1} {not spam, spam}

1. Create code to source data into data structure.
2. Create probabilistic model
  - compute prior probability for each class (spam/not)
  - for each of 57 features, compute mean && std-dev
    - if std-dev == 0 => normalize to 0.0001
3. Run Naïve Bayes
  - use algz in the text thing
