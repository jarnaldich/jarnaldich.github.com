---
title: Incanter
date: 2000-01-01T00:00:00
tags: clojure, incanter, lisp, data
---

This will be a file on using incanter for data processing 



# Incanter


| Title                                               | Author          | Language |
|-----------------------------------------------------+-----------------+----------|
| Jaques le fataliste et son mâitre                   | Dennis Diderot  | French   |
|  _**Gravity's Rainbow**_                                   | **Thomas Pynchon**  | **English**  |
| The Life and Opinions of Tristram Shandy, Gentleman | Laurence Sterne | English  |



# $$\bowtie$$
$$ f(x)=\sum_{n=0}^\infty\frac{f^{(n)}(a)}{n!}(x-a)^n $$

## Use

```clojure
(use '(incanter core stats charts io))
```

## Stock datasets

```clojure
(def ds (get-dataset :iris))
```

## Create datasets

To define a dataset, the user must suply column names and data rows:

```clojure
(def ds (dataset [:a :b :c]
                 [[1 2 3]
                  [4 5 6]]))
```

The `to-dataset` method lets you create a DS from different objects.

```clojure
(def from-map (to-dataset [{:a 1, :b 2, :c 3},
                           {:a 4, :b 5, :c 6}]))

(def from-mat (to-dataset [[1 2 3] [4 5 6]]))
```

## Inspect

```clojure
(view ds)
```

## Save as CSV 

```clojure
(save ds)
```

## Select a whole column

```clojure
($ :Species iris)
```

## Select rows

```clojure
($where {"Species" "setosa"}
 (get-dataset :iris))

($where {"Petal.Width" {:lt 1.5}}
 (get-dataset :iris))

($where {"Petal.Width" {:gt 1.0, :lt 1.5}}
 (get-dataset :iris))

($where {"Petal.Width" {:gt 1.0, :lt 1.5}
 "Species" {:in #{"virginica" "setosa"}}}
 (get-dataset :iris))

($where (fn [row]
 (or (< (row "Petal.Width") 1.0)
 (> (row "Petal.Length") 5.0)))
 (get-dataset :iris))
```

