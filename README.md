(This is still in alpha stage.)

# abapBlame

A simple blame tool for ABAP code.

## Why

ABAP's versioning system is unbelievably bad. Blame is just one of the dozens of things you should be able to do but, for some mysterious reason, are not. Enabling blame in ABAP code is the sole purpose of this simple but hopefully useful project.

## What

A simple programa called `ZBLAME`. Give it the name of a program and it will list its source code, blaming someone for every single line.

## Requirements

* ABAP Version: 740 or higher
* [abapGit](https://abapgit.org)

## FAQ

For questions/comments/bugs/feature requests/wishes please create an [issue](https://github.com/abapinho/abapblame/issues).

## Credits

* Most code by [Nuno Godinho](https://github.com/orgs/abapinho/people/nununo)
* Diff algorithm and syntax highlighter parcially copied from [abapGit](http://abapgit.org/)
