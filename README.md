# abapTimeMachine

A simple way to look at past versions of classes, function groups, etc, as a whole (as opposed to the crappy native include-oriented approach).

(abapTimeMachine used to be called abapBlame but it was renamed because the new Time Machine functionality seems to be so much more useful.)

(The README and the Wiki still need to be updated.)

## What

According to the [git-blame documentation](https://www.git-scm.com/docs/git-blame) it "show what revision and author last modified each line of a file".

`ZBLAME` is a simple to use ABAP program which, given a piece of code, implements the same functionality on top of SAP's ABAP code versioning.
It also allows you to see how that object was at a given time in the past.

## Why

ABAP's versioning system is unbelievably bad. Blame is just one of the dozens of things you should be able to do but, for some mysterious reason, are not. Enabling blame in ABAP code is the sole purpose of this simple but hopefully useful project.

## How

Please read the [Wiki](https://github.com/abapinho/abapBlame/wiki) to learn how to install and use `ZBLAME`.

## Requirements

* ABAP Version: 7.40 or higher
* [abapGit](https://abapgit.org)

## FAQ

For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/abapinho/abapblame/issues).

## Credits

* Most code by [Nuno Godinho](https://github.com/orgs/abapinho/people/nununo)
* Diff algorithm and syntax highlighter parcially copied from [abapGit](http://abapgit.org/)
