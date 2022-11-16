function create_abbr -d "Creates an abbreviation."
	set -l name $argv[1]
	set -l body $argv[2..-1]
	abbr -a $name $body
end

function setup_git_abbr -d "Sets up git abbreviations."
	create_abbr g     git

	create_abbr gcl   git clone --recurse-submodules
	create_abbr gr    git remote

	create_abbr gst   git status

	create_abbr gco   git checkout
	create_abbr gcb   git checkout -b

	create_abbr gb    git branch -vv
	create_abbr gba   git branch -vv -a

	create_abbr gd    git diff
	create_abbr gdc   git diff --cached

	create_abbr ga    git add
	create_abbr gaa   git add --all

	create_abbr grm   git rm
	create_abbr grmc  git rm --cached

	create_abbr gc    git commit -v -m
	create_abbr gc!   git commit -v --amend
	create_abbr gcn!  git commit -v --amend --no-edit
	create_abbr gca   git commit -v -a
	create_abbr gca!  git commit -v -a --amend
	create_abbr gcan! git commit -v -a --amend --no-edit

	create_abbr gp    git push
	create_abbr gp!   git push --force-with-lease

	create_abbr gf    git fetch --all --prune

	create_abbr gl    git pull -ff

	create_abbr gm    git merge

	# TODO(dmi): @bug Fails due to invalud substitution.
	# create_abbr glog  git log --pretty=format:'%C(yellow)%h %Cred%ad %Cblue%an%Cgreen%d %Creset%s' --date=short
end

if not set -q MY_ABBR_SET
	setup_git_abbr

	set -U MY_ABBR_SET true
end
