# MacOS

Here comes a set of MacOS specific dotfiles and such.

## Homebrew

I use [Homebrew](https://brew.sh) to manage applications and libraries.

Bootstrap everything is easy as:
```bash
# installs homebrew
$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# installs all the casks and packages
$ brew bundle
```

From time to time, I should eventually update the `Brewfile` with:
```bash
# overwrites any existing brewfile!
$ brew bundle dump -f
```
