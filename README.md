(This is still in alpha stage.)

# abapBlame

A simple blame tool for ABAP code.

## What

According to the [git-blame documentation(https://www.git-scm.com/docs/git-blame) it "show what revision and author last modified each line of a file".

`ZBLAME` is a simple to use ABAP program which, given a piece of code, tries to do exactly this.

## Why

ABAP's versioning system is unbelievably bad. Blame is just one of the dozens of things you should be able to do but, for some mysterious reason, are not. Enabling blame in ABAP code is the sole purpose of this simple but hopefully useful project.

## Requirements

* ABAP Version: 7.40 or higher
* [abapGit](https://abapgit.org)

## FAQ

For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/abapinho/abapblame/issues).

## Credits

* Most code by [Nuno Godinho](https://github.com/orgs/abapinho/people/nununo)
* Diff algorithm and syntax highlighter parcially copied from [abapGit](http://abapgit.org/)
