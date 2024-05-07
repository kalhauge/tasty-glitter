# Not all that G[l]itters is Gold.

The idea is simple. Instead of predicting the output of a test, you simply run
the test, and save the results. Now, you can check if the test have changed,
according to the version control system. If you like it `git add` the file 
to indicate that it is good.

But, wait, there is more!
Golden tests are great for storing the output of files quickly, but we all make mistakes.
It's better to get the computer to check for common mistakes in the golden file before it is 
committed to the VCS.
To enable this, glitter test can run a sequence of test on changed files to see if they 
are conforming to requirements.
Since these checks are only run for changed files, they can take a long time.

## How it works. 

We maintain a list of glitter files, glitter files are files that should be of
special interest to glitter tests in the `TestTree`.
The goal of a glitter test is to see if file is in-fact gold.

We start with all changed files in the repository, and we can then make 
modifications to the repository and to the list.



## The `--changed-file` option.

Here you can change what constitute a changed file. 
Currently, there are two options: 

| Option      | Result                                                       |
|:---         |:---                                                          |
| `unstaged`  | Test all non-ignored files that are different from the stage |
| `all`       | Test all non-ignored files                                   |

We have plans for adding an option to specify changes according to a specific
commit, so that CI/CD's only have to check changes since last known working
commit.

## Design Decisions 

### Why are glitter test only available in IO mode.

## Related Packages

- [`hspec-golden`](https://hackage.haskell.org/package/hspec-golden)

- [`tasty-golden`](https://hackage.haskell.org/package/tasty-golden)

In Roman's [introduction to golden testing](https://ro-che.info/articles/2017-12-04-golden-tests), he claims that he prefers this kind of interaction. 

