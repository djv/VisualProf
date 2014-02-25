VisualProf
==========

http://www.reddit.com/r/haskell/comments/cr15z/visualprof_profiles_your_haskell_program_and/?sort=hot

visual-prof profiles your Haskell program and generates a html file containing its source code with parts of the code highlighted in different colors depending on the fraction of the running time that they take. visual-prof gives you an easy way to find places for optimization in your code.

Usage:
```
visual-prof -px A/B/C.hs run "arg1 arg2"
```
This will profile the C.hs file used by run.hs which contains the Main module of your project. Arguments to ./run are passed as shown (arg1, arg2,...). The parameters should be given in that order.

The simplest way to run it is:
```
visual-prof -px test.hs test
```
which will generate a profile for the file test.hs (which needs to have a main function)
