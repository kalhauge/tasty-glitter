# Not all that G[l]itters is Gold.

The idea is simple. Instead of predicting the output of a test, you simply run
the test, and save the results. Now, you can check if the test has changed,
according to the version control system. If you like it `git add` the file 
to indicate that it is gold. This is in essence golden testing.

Golden tests are great to quickly identify regressions, but when you are in 
the explorative phase regressions will often happen. 
In this case it can take a long time to manually identify if the new golden file is 
in fact a golden, or just glitters. 
Glitter files are files produced by our tests, but we have not yet verified.

This library enables you to write glitter tests, that can help you get
confidence in your growing set of golden tests, while minimizing the friction
of writing test.

To emulate the normal golden test approach, we can write the following:
```haskell
goldenTest :: TestName -> FileName -> String -> TestTree
goldenTest testname filename content = 
    withPopulatedFile filename (\fp -> writeFile fp content) 
      $ testGroup testname [ assayShouldBeStaged ]
```

It essentially, it tries to populate a file using the content, then 
it tests if every changed file is staged using the `asssayShouldBeStaged`. 
Since we just overwrite the file, the only reason for it to have been changed
is if the `content` parameter has changed.

A sample glitter test might look like this:
```haskell
-- | Test if our compiler produce c-files that compile.
testTranspiling :: TestTree
testTranspiling =
  testGroup
    "transpiling"
    [ testGroup
      name
      [ -- Check that the file compiles with clang
        assayEach "should compile" \file -> do
          proc "clang" ["-o", "/dev/null", file]
            `shouldExitWith` ExitSuccess
      , -- Check that the file has not changed.
        assayShouldBeStaged
      ]
      -- Write the transpiled example to the file before all of test in the test group.
      & withPopulatedFile ("expected" </> "cfiles" </> name <.> "c") \fp -> do
        writeFile fp (transpile example)
    | (name, example) <- examples
    ]
 where
  examples =
    [ ("minimal", "int main(void) {}")
    , ("bigger", "int main(void) { return 0; }")
    ]
  transpile = id -- your interesting transpiler
```

## How it works. 

We maintain a list of glitter files, glitter files are files that should be of
special interest to glitter tests in the `TestTree`.
The goal of a glitter test is to see if file is in-fact gold.

We start with all changed files in the repository, and we can then make 
modifications to the repository and to the list.

To see a larger example look at [`test/src/Main.hs`](/test/src/Main.hs).

### Assaying

All glitter tests starts with the word [`assay`](https://en.wikipedia.org/wiki/Assay), 
which is to describe identifying the precesses of gold in a material.
They can take one or all of the glitter files and check if they are gold. 

The trivial glitter test is `assayShouldBeStaged` which simply checks that 
the file has been staged. This is useful for checking that all golden files 
have not changed.

### Changing the Glitter files

To change the glitter files to test, there are two options, either you can use
the `filterGliter` function, which removes files not interesting for the
glitter test, or you can modify the repository and investigate the modification.

We also provide the `withPopulatedFile` and `withPopulatedDirectory` which 
writes to a file or directory and check for changes.

## The `--changed-files` option.

Here you can change what constitute a changed file. Currently, there are two options: 

| Option      | Result                                                             |
|:---         |:---                                                                |
| `unstaged`  | Test all non-ignored files that are different from stage (default) |
| `all`       | Test all non-ignored files                                         |

We have plans for adding an option to specify changes according to a specific
commit, so that CI/CD's only have to check changes since last known working
commit.

## Future Work 

- Make all of this work even in non-git repositories (this is usefull when getting tests to work in 
  nix packages.)

## Related Packages

- [`hspec-golden`](https://hackage.haskell.org/package/hspec-golden)

- [`tasty-golden`](https://hackage.haskell.org/package/tasty-golden)

