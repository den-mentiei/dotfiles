## Customization

# UTF-8 by default
[System.Console]::InputEncoding = [System.Text.Encoding]::GetEncoding("utf-8")
[System.Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding("utf-8")
$PSDefaultParameterValues["Out-File:Encoding"] = "utf8"

# https://technet.microsoft.com/en-us/magazine/hh241048.aspx
$MaximumHistoryCount = 10000;

## Utilities

function source {
	if (Test-Path $PROFILE) {
		Write-Verbose "Sourcing $PROFILE"
		. $PROFILE
	}
}

## Prompt

Invoke-Expression (&starship init powershell)

## Aliases

# Preventing conflict with builtins.
if ($PSVersionTable.PSVersion.Major -le 5) {
	function Remove-Alias ([string] $AliasName) {
		while (Test-Path Alias:$AliasName) {
			Remove-Item Alias:$AliasName -Force 2> $null
		}
	}
}
Remove-Alias gc -Force -ErrorAction SilentlyContinue
Remove-Alias gp -Force -ErrorAction SilentlyContinue
Remove-Alias gl -Force -ErrorAction SilentlyContinue

### Git

function g { git $args }

function gst { git status $args }

function gco { git checkout $args }
function gcb { git checkout -b $args }

function gd { git diff $args }
function gdc { git diff --cached $args }

function ga { git add $args }
function gaa { git add --all $args }

function gb { git branch -vv $args }
function gba { git branch -vv -a $args }

function gc { git commit -v $args }
function gc! { git commit -v --amend $args }
function gcan! { git commit -v --amend --no-edit $args }

function gp { git push $args}
function gp! { git push --force-with-lease $args }

function gf { git fetch --all --prune $args }
function gl { git pull -ff $args }

### Cargo

function c { cargo $args }
function ccl { cargo clean $args }
function cck { cargo check $args }
function cb { cargo build $args }
function cr { cargo run $args }
function cwc { cargo watch -x 'check' $args }
function cwt { cargo watch -x 'test' $args }
function cwb { cargo watch -x 'build' $args }

### Etc.

Remove-Alias ls -Force -ErrorAction SilentlyContinue
function la { ls -lah --color=auto $args }

### FZF history

# Requires `Install-Module PSFzf`.
Import-Module PSFzf
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+e' -PSReadlineChordReverseHistory 'Ctrl+r'
