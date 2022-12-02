function create_abbr -d "Creates an abbreviation."
	set -l name $argv[1]
	set -l body $argv[2..-1]
	abbr -a $name $body
end

function setup_cargo_abbr -d "Sets up cargo abbreviations."
	create_abbr c    cargo

	create_abbr cb   cargo build
	create_abbr cbr  cargo build --release
	create_abbr cbrm cargo build --release --target=x86_64-unknown-linux-musl

	create_abbr cck  cargo check
	create_abbr ccl  cargo clean
	create_abbr cu   cargo update
	create_abbr cr   cargo run
	create_abbr ct   cargo test
end

function setup_git_abbr -d "Sets up git abbreviations."
	create_abbr g     git

	create_abbr gcl   git clone --recurse-submodules

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

	create_abbr glg   git log --pretty=format:'"%C(yellow)%h %Cred%ad %Cblue%an%Cgreen%d %Creset%s"' --date=short --graph
	create_abbr glog  git log --pretty=format:'"%C(yellow)%h %Cred%ad %Cblue%an%Cgreen%d %Creset%s"' --date=short
end

function setup_all_abbr -d "Sets up all the abbreviations."
	if not set -q MY_ABBR_SET
		setup_git_abbr
		setup_cargo_abbr
		set -U MY_ABBR_SET true
	end
end

function remove_all_abbr -d "Removes all the abbreviations."
	for a in (abbr --list)
		abbr -e $a
	end
end

function reset_all_abbr -d "Resets all the abbreviations."
	set -U -e MY_ABBR_SET
	remove_all_abbr
	setup_all_abbr
end

setup_all_abbr
