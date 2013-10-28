To contribute, you have a number of options:

1. Create a branch of this repository and edit `ss3sim-ms.md`. Let me (Sean)
   know afterwards by email or by starting an issue and I'll merge the changes
   in.

Steps: ([first set up Git and GitHub and clone the repository](https://github.com/seananderson/ss3sim/wiki/Working-with-Git-locally))

Then...

```git
git pull
```

```git
git checkout -b sean-ms-edits # pick an appropriate branch name
```

Do your editing here in any text editor you'd like.

```git
git add ss3sim-ms.md  # from within the /inst/ms/ folder
```

```git
git commit
```

Add your commit message now, save, and close the commit window.

```git
git push origin sean-ms-edits # replace the branch name with yours
```

Now, to get back to the `master` branch, if you want:

```git
git checkout master
```

Now, go to the GitHub ss3sim site and click the green button "Compare and pull request".


2. Download the file
   [`ss3sim-ms.pdf`](https://dl.dropboxusercontent.com/u/254940/ss3sim-ms.pdf)
   and add comments. Then email me the edited version.

3. Download the file
   [`ss3sim-ms.docx`](https://dl.dropboxusercontent.com/u/254940/ss3sim-ms.docx)
   and edit using track changes. Then email me the edited version.

We are developing the appendix (an R vignette) in the file
`ss3sim-vignette.Rnw` in the `vignettes` folder.

If you have references to add, please add them in BibTeX format to the file
`refs.bib` (or send them to me).

Note that the lines in `ss3sim-ms.md` are wrapped approximately on phrases to make Git
diffs and merging easier. Don't worry too much about this as you edit, just try
and keep the line length approximately 80 characters or less and don't make
line break changes unless you need to.
