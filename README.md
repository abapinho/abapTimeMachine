# abapTimeMachine

`abapTimeMachine` provides a simple way to look at past versions of classes, function groups, etc, as a whole (as opposed to the crappy native include-oriented approach). Alternatively, it also partially emulates [git-blame](https://www.git-scm.com/docs/git-blame) by showing "what revision and author last modified each line of a file" for any given version.

## Why

ABAP's versioning system is unbelievably bad. There is no native way to see how a whole class or a whole function group were at a given time in the past. Besides, it makes it very hard to find out which user was responsible for which changes. `abapTimeMachine` is here to solve both these problems.

## How

Please read the [Wiki](https://github.com/abapinho/abapTimeMachine/wiki) to learn how to install and use `ZTIMEMACHINE`.

## Requirements

* ABAP Version: 7.40 or higher
* [abapGit](https://abapgit.org)

## FAQ

For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/abapinho/abapTimeMachine/issues).

## Credits

* Most code by [Nuno Godinho](https://github.com/nununo)
* Diff algorithm and syntax highlighter parcially copied from [abapGit](https://abapgit.org)
