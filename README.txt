Vancouver Haskell unMeetup.

This project is intended to teach developers how to do a "real-world" Haskell project, by using Cabal and common Haskell libraries.

You may get started by implementing the whole project or just part of it, the tests that check the behaviour of the implementation are already there, so you can develop based checking if the test run successfuly on or not. 

1) 
  * [Advanced] To implement the whole project, you will need to checkout the branch *do_all_project*

    $ git checkout do_all_project

  * Edit file TicTacToe/src/Model/Matrix.hs
  * Edit file TicTacToe/src/Internal/Controller.hs


2) 
  * [Beginners] To implement the pure API (Model.Matrix module), you will need to checkout the branch *do_matrix*

    $ git checkout do_matrix

  * Edit file TicTacToe/src/Model/Matrix.hs


3) 
  * [Middle-Advanced] To implement the State Monad API (Internal.Controller module), you will need to checkout the branch *do_state*

    $ git checkout do_state

  * Edit file TicTacToe/src/Internal/Controller.hs


In order to compile and run the tests of the project, you will need to do the following

    $ cd TicTacToe/tests
    $ cabal configure
    $ cabal build

    # if you are doing (1)
    $ ./runTest 

    # if you are doing (2)
    $ ./runTest -t "Model.Matrix.Tests"

    # if you are doing (3)
    $ ./runTest -t "Controller.Tests"

In order to compile and run the project to check how it is working:

    $ cd TicTacToe
    $ cabal configure
    $ cabal build
    $ ./dist/build/TicTacToe/TicTacToe

Follow the instructions in the comments above each function in the source files to get a better understanding of what you need to do.

Good luck.
