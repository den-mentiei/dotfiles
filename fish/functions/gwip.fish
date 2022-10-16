# Allows to pause a branch development and switch to another one.
# It can be unwipped back later.
function gwip -d "Makes a WIP git commit."
	command git add -A
	command git rm (git ls-files --deleted) 2> /dev/null
	command git commit -m "WIP" --no-verify
end
