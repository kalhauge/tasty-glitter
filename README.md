# Not all that G[l]itters is Gold.

The idea is simple. Instead of predicting the output of a test, you simply run
the test, and save the results. Now, you can check if the test have changed,
according to the version control system.

But, wait, there is more!
Golden tests are great for storing the output of files quickly, but we all make mistakes.
It's better to get the computer to check for common mistakes in the golden file before it is 
committed to the VCS.

## The `--changed-file` option.

It is 

## Related Packages

- [`hspec-golden`](https://hackage.haskell.org/package/hspec-golden)

- [`tasty-golden`](https://hackage.haskell.org/package/tasty-golden)
