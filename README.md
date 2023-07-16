# WPF Tasks

This repository contains my solutions to tasks from the course "Introduction to
Functional Programming".

## Overview

WPF stands for "Wstęp do Programowania Funkcyjnego" in Polish, which translates
to "Introduction to Functional Programming". This repository includes my
solutions to the tasks from this course. All the solutions are written in OCaml.

## Task Descriptions

Here is a list of short task descriptions:

1. **Arithmetic**: Create a package for performing arithmetic operations on
approximate values in OCaml. The package includes constructors for creating
approximate values with specified precision, selectors for retrieving
information about the values, and modifiers for performing addition,
subtraction, multiplication, and division operations on the values.

2. **Leftist Trees**: Develop a mergeable priority queue using leftist trees.
Leftist trees are binary heaps that satisfy the leftist property in addition to
the heap property, enabling efficient operations for insertion, deletion of the
minimum element, and merging two queues. The task involves implementing the
queue operations, such as adding an element, deleting the minimum element, and
joining two queues, along with checking if a queue is empty.

3. **Tree Modification**: Modify the AVL tree-based library of single element
sets to support interval sets of integers. The modification involves ensuring
that the intervals are disjoint, merging adjacent intervals, and maintaining
logarithmic time complexity for operations.

4. **Origami**: Build an origami library to analyze the number of layers in a
folded sheet of paper at a specific point. The library should provide functions
to create rectangular and circular sheets, fold a sheet along a line, and fold a
sheet successively along multiple lines.

5. **Topological Sort**: Implement a topological sorting algorithm for directed
acyclic graphs (DAGs). The algorithm assigns unique numbers to the vertices of
the DAG, ensuring that for each edge, the source vertex has a lower number than
the target vertex.

6. **Pouring**: Design a procedure that determines the minimum number of actions
required to achieve a specific water distribution among a given set of glasses.
The procedure takes an array of pairs representing the capacities and desired
amounts of water in each glass and returns the minimum number of actions needed.
If the desired distribution is impossible, the procedure returns -1.

Please refer to the respective task directories for detailed solutions.
